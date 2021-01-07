{-# LANGUAGE RecordWildCards #-}
module Learner.Internal where

import qualified Data.IntMap         as M
import           Data.List           (foldl')
import           Data.Maybe          (isNothing)

import           CHC
import           Data.CounterExample


-- | positivity class
-- @Nothing@ means undefined
type ClassP = Maybe Bool
type Degree = Double

-- | value instances of a predicate, annotated with degree
-- e.g. (f 0 1) will generate (unknown, [0, 1])
data Datapoint = Datapoint { degreeP :: Degree -- ^ degree of this point, generated from the origin position of the varvals
                           , classP  :: ClassP  -- ^ easy for reverse access
                           , vals    :: [VarVal]
                           }
  deriving (Eq, Show)

isClass :: Bool -> Datapoint -> Bool
isClass = (. classP) . (==) . Just

isMaybeClass :: Maybe Bool -> Datapoint -> Bool
isMaybeClass = (. classP) . (==)

isUnknown :: Datapoint -> Bool
isUnknown = isNothing . classP

setClass :: ClassP -> Datapoint -> Datapoint
setClass cl p = p { classP = cl }

-- | x \simeq b means x is unknown or equal to b
simeq :: Bool -> Datapoint -> Bool
simeq = (. classP) . (/=) . Just . not

getVarVal :: [Datapoint] -> [[VarVal]]
getVarVal = map vals

-- | a point with predicate attached
type FuncDatapoint = (FuncIx, Datapoint)

-- | Dataset, but with degrees computed from the clause each point comes from
data AnnotatedDataset = AnnotatedDataset { posA :: [[FuncDatapoint]]
                                         , negA :: [[FuncDatapoint]]
                                         , impA :: [[FuncDatapoint]] -- ^ lhs and rhs is collapsed after the degree annotation
                                         }
  deriving (Eq, Show)

-- | All datapoints for a predicate, separated by class
-- invariants:
--  * for p in trueC, classP p === Just True
--  * for p in falseC, classP p === Just False
--  * for p in unknownC classP p === Nothing
data ClassData = ClassData { trueC    :: [Datapoint]
                           , falseC   :: [Datapoint]
                           , unknownC :: [Datapoint]
                           }
  deriving (Eq, Show)

-- | change class of a point to fit classData
adjustClass :: ClassData -> ClassData
adjustClass ClassData{..} = ClassData { trueC = map (setClass (Just True)) trueC
                                      , falseC = map (setClass (Just False)) falseC
                                      , unknownC = map (setClass Nothing) unknownC
                                      }

-- | similar to @adjustClass@, but leave out unknownC
adjustClassKnown :: ClassData -> ClassData
adjustClassKnown ClassData {..} =  ClassData { trueC = map (setClass (Just True)) trueC
                                             , falseC = map (setClass (Just False)) falseC
                                             , unknownC = unknownC
                                             }

knownNum :: ClassData -> Int
knownNum ClassData{..} = length trueC + length falseC

allNum :: ClassData -> Int
allNum = length . allClassData

emptyTreeData :: ClassData
emptyTreeData = ClassData { trueC = []
                          , falseC = []
                          , unknownC = []
                          }

allClassData :: ClassData -> [Datapoint]
allClassData ClassData{..} = trueC ++ falseC ++ unknownC

-- | build ClassData from points according to each point's class
buildClassData :: [Datapoint] -> ClassData
buildClassData points = ClassData truePoints falsePoints unkPoints
  where
    (truePoints, falsePoints, unkPoints) = foldl' dispatch ([], [], []) points
    dispatch (ts, fs, us) p = case classP p of
                                Just True  -> (p:ts, fs, us)
                                Just False -> (ts, p:fs, us)
                                Nothing    -> (ts, fs, p:us)

isEmptyClassData :: ClassData -> Bool
isEmptyClassData ClassData{..} = null trueC && null falseC && null unknownC

-- assume all varval is unknown, annotate difference appearance with the contributed degree value
-- during the class assignment, all known varvals (@trucC@ and @falseC@) are changed to 1.0 and 0.0 resp.
-- duplicated varvals will be compressed, and their degree components will be summed
annotateDegree :: Dataset -> AnnotatedDataset
annotateDegree Dataset{..} = AnnotatedDataset { posA = map (distribute 1.0) pos
                                              , negA = map (distribute (-1.0)) neg
                                              , impA = map annotateImp imp
                                              }
  where
    distribute v funcDatas = let n = length funcDatas
                                 deg = if n == 0 then 0.0 else v / fromIntegral n
                              in map (annotate deg) funcDatas
    annotate deg (funcIx, varvals) = (funcIx, Datapoint deg Nothing varvals)
    annotateImp (lhs, rhs) = let n = 1 + length lhs + length rhs
                                 deg = 1.0 / fromIntegral n
                                 lhs' = map (annotate (-deg)) lhs
                                 rhs' = map (annotate deg) rhs
                              in lhs' ++ rhs'

assignClass :: FuncMap a -> AnnotatedDataset -> FuncMap ClassData
assignClass funcMap AnnotatedDataset{..} = dispatchDataset emptyFuncMap
  where
    dispatchDataset = acc atUnk unknowns . acc atPos singlePos . acc atNeg singleNeg

    emptyFuncMap = M.map (const emptyTreeData) funcMap

    (singlePos, posUnk) = pickoutSingle posA
    (singleNeg, negUnk) = pickoutSingle negA

    unknowns = filter notKnown $ posUnk ++ negUnk ++ concat impA

    notKnown (rho, p) = notIn singlePos && notIn singleNeg
      where
        notIn = not . any (\(rho', p') -> rho == rho' && vals p == vals p')

    acc :: (Datapoint -> ClassData -> ClassData) -> [FuncDatapoint] -> FuncMap ClassData -> FuncMap ClassData
    acc f = flip . foldl' . flip $ \(funcIx, datapoint) -> M.adjust (f datapoint) funcIx

    atPos datapoint t@ClassData{..} = t { trueC = setClass (Just True) datapoint:trueC}
    atNeg datapoint t@ClassData{..} = t { falseC = setClass (Just False) datapoint:falseC}
    atUnk datapoint t@ClassData{..} = t { unknownC = datapoint:unknownC}

    pickoutSingle = foldl' pick ([], [])
      where
        pick (singles, unks) [funcData] = (funcData:singles, unks)      -- singleton
        pick (singles, unks) funcDatas  = (singles, unks ++ funcDatas)  -- otherwise


updateUnkClass :: Bool -> FuncData -> FuncMap ClassData -> FuncMap ClassData
updateUnkClass b (rho, unkV) = M.update updateOne rho
  where
    updateOne ClassData{..} = Just ClassData { trueC = if b then Datapoint 1.0 (Just True) unkV:trueC else trueC
                                             , falseC = if not b then Datapoint 0.0 (Just False) unkV:falseC else falseC
                                             , unknownC = filter ((/= unkV)  . vals) unknownC
                                             }

isFuncMaybeClass :: FuncMap ClassData -> Maybe Bool -> FuncData -> Bool
isFuncMaybeClass allClassMap mb (rho, varvals) = let classData = allClassData $ allClassMap M.! rho
                                                     res = filter ((== varvals) . vals) classData
                                                  in if null res
                                                        then error $ "Query the class of data varvals " <> show varvals <> ", but not exist in classMap"
                                                        else isMaybeClass mb $ head res

simeqFunc :: FuncMap ClassData -> Bool -> FuncData -> Bool
simeqFunc allClassMap b = not . isFuncMaybeClass allClassMap (Just (not b))
