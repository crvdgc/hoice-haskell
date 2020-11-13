{-|
Module      : Learner.DecisionTree
Description : Synthesize the predicates with decision tree
Maintainer  : liuyuxi@kb.is.s.u-tokyo.ac.jp
Stability   : experimental
Portability : POSIX

From the original HoIce paper.
-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Learner.DecisionTree where

import           Debug.Logger

import qualified Data.IntMap            as M
import           Data.List              (elemIndex, foldl', maximumBy,
                                         partition)
import qualified Data.List.NonEmpty     as NE
import           Data.Maybe             (fromJust)
import qualified Data.Set               as S

import           CHC
import           Data.CounterExample
import           Language.Assertion.LIA
import           Learner.Internal

type Degree = Double
type Datapoint = (Degree, [VarVal]) -- degree is generated from the origin position of the varvals

type FuncDatapoint = (FuncIx, Datapoint)

data AnnotatedDataset = AnnotatedDataset { posA :: [[FuncDatapoint]]
                                         , negA :: [[FuncDatapoint]]
                                         , impA :: [[FuncDatapoint]] -- ^ lhs and rhs is collapsed after the degree annotation
                                         }
  deriving (Eq, Show)

data ClassData = ClassData { trueC    :: [Datapoint]
                           , falseC   :: [Datapoint]
                           , unknownC :: [Datapoint]
                           }
  deriving (Eq, Show)

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
    annotate deg (funcIx, varvals) = (funcIx, (deg, varvals))
    annotateImp (lhs, rhs) = let n = 1 + length lhs + length rhs
                                 deg = 1.0 / fromIntegral n
                                 lhs' = map (annotate (-deg)) lhs
                                 rhs' = map (annotate deg) rhs
                              in lhs' ++ rhs'

emptyTreeData :: ClassData
emptyTreeData = ClassData { trueC = []
                          , falseC = []
                          , unknownC = []
                          }

allClassData :: ClassData -> [Datapoint]
allClassData ClassData{..} = trueC ++ falseC ++ unknownC

assignClass :: FuncMap a -> AnnotatedDataset -> FuncMap ClassData
assignClass funcMap anno@AnnotatedDataset{..} = loggerShow assignClassLog "annotated dataset" anno $ dispatchDataset emptyFuncMap
  where
    assignClassLog = appendLabel "assignClass" learnerLog
    dispatchDataset = acc atUnk unknowns . acc atPos singlePos . acc atNeg singleNeg

    emptyFuncMap = M.map (const emptyTreeData) funcMap

    (singlePos, posUnk) = pickoutSingle posA
    (singleNeg, negUnk) = pickoutSingle negA

    unknowns = loggerShow assignClassLog "singlePos" singlePos $ loggerShow assignClassLog "singleNeg" singleNeg $ loggerShowId assignClassLog "after filter" $ filter notKnown $ loggerShowId assignClassLog "before filter" $ posUnk ++ negUnk ++ concat impA

    notKnown (_, (_, vs)) = notIn singlePos && notIn singleNeg
      where
        notIn = not . any ((== vs) . snd . snd)

    acc :: (Datapoint -> ClassData -> ClassData) -> [FuncDatapoint] -> FuncMap ClassData -> FuncMap ClassData
    acc f = flip . foldl' . flip $ \(funcIx, datapoint) -> M.adjust (f datapoint) funcIx

    atPos datapoint t@ClassData{..} = t { trueC = trueC ++ [datapoint] }
    atNeg datapoint t@ClassData{..} = t { falseC = falseC ++ [datapoint] }
    atUnk datapoint t@ClassData{..} = t { unknownC = unknownC ++ [datapoint] }

    pickoutSingle = foldl' pick ([], [])
      where
        pick (singles, unks) [funcData] = (funcData:singles, unks)      -- singleton
        pick (singles, unks) funcDatas  = (singles, unks ++ funcDatas)  -- otherwise

data LearnData = LearnData { classMap :: FuncMap ClassData
                           , dataset  :: Dataset
                           , quals    :: FuncMap [Qualifier]
                           }
  deriving (Eq, Show)


-- qualifiers indexing the free variables from 0
type Qualifier = LIA Bool BoundVarIx

mineAllQuals :: FuncMap Int -> FuncMap [[VarVal]] -> FuncMap [Qualifier]
mineAllQuals arityMap = M.mapWithKey $ mineQualsWith arityMap
  where
    mineQualsWith arityMap funcIx = mineQuals (arityMap M.! funcIx)

mineQuals :: Int -> [[VarVal]] -> [Qualifier]
mineQuals arity = let vars = [0..arity-1]
                   in concat . concatMap (mineAt vars)
  where
    mineAt vars varvals = zipWith ineql vars varvals ++ zipWith ineqlPair (pairs vars) (pairs varvals)
    ineql i x = [LIAAssert Le (LIAVar i) (LIAInt x), LIAAssert Ge (LIAVar i) (LIAInt x)]
    ineqlPair (i, j) (x, y) = [ LIAAssert Le (LIAArith Add (LIAVar i) (LIAVar j)) (LIAArith Add (LIAInt x) (LIAInt y))
                              , LIAAssert Ge (LIAArith Add (LIAVar i) (LIAVar j)) (LIAArith Add (LIAInt x) (LIAInt y))
                              , LIAAssert Le (LIAArith Sub (LIAVar i) (LIAVar j)) (LIAArith Sub (LIAInt x) (LIAInt y))
                              , LIAAssert Ge (LIAArith Sub (LIAVar i) (LIAVar j)) (LIAArith Sub (LIAInt x) (LIAInt y))
                              ]
    pairs []     = []
    pairs (x:xs) = map ((,) x) xs ++ pairs xs

knownNum :: ClassData -> Int
knownNum ClassData{..} = length trueC + length falseC

allNum :: ClassData -> Int
allNum = length . allClassData

entropy :: ClassData -> Double
entropy classData = let probTrue = heuristicTrue classData
                        probFalse = 1.0 - probTrue
                        log2 = logBase 2.0
                        epsilon = 0.001
                     in if probTrue < 0.0 || probFalse < 0.0
                           then error "negative probability"
                           else if probTrue < epsilon || probFalse < epsilon
                                  then 0.0
                                  else (-probTrue) * log2 probTrue - probFalse * log2 probFalse

heuristicTrue :: ClassData -> Double
heuristicTrue classData@ClassData{..} = if n == 0.0 then 0.0 else (truePr + unknownPr) / n
  where
    n = fromIntegral . length . allClassData $ classData
    truePr = fromIntegral . length $ trueC
    degreeOf = degree classData
    unknownPr = sum . map (\v -> 0.5 + atan (degreeOf v) / pi) $ getVarVal unknownC

degree :: ClassData -> [VarVal] -> Double
degree classData v = sum . map fst . filter ((== v) . snd) . allClassData $ classData

intmapAppend :: FuncIx -> [a] -> M.IntMap [a] -> M.IntMap [a]
intmapAppend rho xs = M.update (Just . (++ xs)) rho

initializeQuals :: FuncMap a -> CHC VarIx FuncIx -> FuncMap [Qualifier]
initializeQuals funcMap (CHC clss) = foldl' collectClause emptyQualifier clss
  where
    emptyQualifier = M.map (const []) funcMap

collectClause :: FuncMap [Qualifier] -> Clause VarIx FuncIx -> FuncMap [Qualifier]
collectClause funcMap Clause{..} = foldl' atFuncApp funcMap funcApps
  where
    funcApps = body ++ heads
    atFuncApp funcMap FuncApp{..} = let atomics = getBooleanAtomic (S.fromList args) phi
                                     in intmapAppend func atomics funcMap

getBooleanAtomic :: S.Set VarIx -> LIA Bool VarIx -> [Qualifier]
getBooleanAtomic args lia = if S.isSubsetOf (freeVarsLIA lia) args
                              then allBooleanAtomic lia
                              else concat . map (getBooleanAtomic args) $ subBooleanLIAs lia
  where
    allBooleanAtomic :: LIA Bool VarIx -> [Qualifier]
    allBooleanAtomic = \case
      LIABool _ -> []
      LIAAssert op t1 t2 -> [LIAAssert op t1 t2]
      LIANot t -> allBooleanAtomic t
      LIABoolEql t1 t2 -> [LIABoolEql t1 t2] ++ allBooleanAtomic t1 ++ allBooleanAtomic t2
      LIASeqLogic op ts -> [LIASeqLogic op ts] ++ concatMap allBooleanAtomic ts
    subBooleanLIAs :: LIA Bool VarIx -> [LIA Bool VarIx]
    subBooleanLIAs = \case
      LIABool _ -> []
      LIAAssert op t1 t2 -> []
      LIANot t -> [t]
      LIABoolEql t1 t2 -> [t1, t2]
      LIASeqLogic op ts -> NE.toList ts


splitData :: Qualifier -> ClassData -> (ClassData, ClassData)
splitData q classData@ClassData{..} = let (posTrueC, negTrueC) = splitVarvals q trueC
                                          (posFalseC, negFalseC) = splitVarvals q falseC
                                          (posUnknownC, negUnknownC) = splitVarvals q unknownC
                                       in ( ClassData posTrueC posFalseC posUnknownC
                                          , ClassData negTrueC negFalseC negUnknownC
                                          )
  where
    splitVarvals q = partition $ \(_, varvals) -> evaluateLIABool (varvals !!) q

informationGain :: Qualifier -> ClassData -> Double
informationGain q classData = let (classDataP, classDataN) = splitData q classData
                                  sizeP = fromIntegral $ allNum classDataP
                                  sizeN = fromIntegral $ allNum classDataN
                                  entropyP = entropy classDataP
                                  entropyN = entropy classDataN
                                  entropyD = entropy classData
                                  knownNumD = fromIntegral $ knownNum classData
                               in knownNumD * entropyD - (sizeP * entropyP + sizeN * entropyN)

selectQual :: [Qualifier] -> ClassData -> (Qualifier, Double)
selectQual quals classData = let qualGains = map (\q -> (q, informationGain q classData)) quals
                                 compareSnd x y = compare (snd x) (snd y)
                              in maximumBy compareSnd qualGains

deleteAll :: Eq a => a -> [a] -> [a]
deleteAll x = filter (/= x)

pickoutQual :: [Qualifier] -> ClassData -> Int -> [[VarVal]] -> (Qualifier, [Qualifier])
pickoutQual quals classData arity varvals = let pickLog = appendLabel "pickoutQual" learnerLog
                                                candidate = if null quals then mineQuals arity varvals else quals
                                                (bestQual, maxGain) = loggerShow pickLog "candidates" candidate $ selectQual candidate classData
                                             in if maxGain == 0 -- quals not empty, but cannot split
                                                  then let mined = logger pickLog "max gain is 0" $ error "out" $ mineQuals arity varvals
                                                           -- mined quals must be non-empty and have positive gain
                                                           (bestMined, maxGainMined) = pickoutQual mined classData arity varvals
                                                        in loggerShow (appendLabel "pickoutQual" learnerLog) "maxGainMined" maxGainMined (bestMined, deleteAll bestMined $ quals ++ mined)
                                                  else loggerShow (appendLabel "pickoutQual" learnerLog) "maxGain" maxGain (bestQual, deleteAll bestQual quals)

getVarVal :: [Datapoint] -> [[VarVal]]
getVarVal = map snd

learn :: CHC VarIx FuncIx -> FuncMap Int -> LearnData -> FuncMap ClassData -> (LearnData, FuncMap (LIA Bool VarIx))
learn chc arityMap = M.mapAccumWithKey (buildTree True rootLog)
  where
    buildTree :: Bool -> LogInfo -> LearnData -> FuncIx -> ClassData -> (LearnData, LIA Bool VarIx)
    buildTree showTree treeLog learnData rho classData
      | (loggerShow treeLog "rho" rho $ loggerShow treeLog "learnData" learnData $ null falseCV) && canBe True classData learnData rho = (unknownTo True learnData, LIABool True)
      | null trueCV && canBe False classData learnData rho = (unknownTo False learnData, LIABool False)
      | otherwise = let synLog = appendLabel ("synth for predicate #" <> show rho) treeLog
                        qualMap = quals learnData
                        qual = loggerShowId synLog "orginal quals" $ qualMap M.! rho
                        arity = arityMap M.! rho
                        (q, quals') = pickoutQual qual classData arity . getVarVal . allClassData $ classData
                        (posData, negData) = loggerShow synLog "best qual" q . loggerShowId synLog "split data" $ splitData q classData
                        learnDataQual = learnData { quals = M.update (const $ Just quals') rho qualMap }
                        (learnData', posLIA) = buildTree False (incLevel treeLog) (updateClass posData learnDataQual) rho posData
                        (learnData'', negLIA) = buildTree False (incLevel treeLog) (updateClass negData learnData') rho negData
                     in (learnData'', (if showTree then loggerShowId treeLog "tree" else id) $ flatOr (flatAnd q posLIA) (flatAnd (flatNot q) negLIA))
      where
        trueCV = getVarVal . trueC $ classData
        falseCV = getVarVal . falseC $ classData
        unknownCV = getVarVal . unknownC $ classData

        updateClass newClass learnData@LearnData{..} = learnData { classMap = M.update (const $ Just newClass) rho classMap }

        -- |check whether unknown data points can all be assgined to True or False
        -- - @canBe True@ implements @can_be_pos@
        -- - @canBe False@ implements @can_be_neg@
        canBe bool classData learnData@LearnData{..} rho = loggerShow canBeLog "rho, bool, classData" (rho, bool, classData) (loggerShowId canBeLog "consistentOther?" $ consistentOther otherPositivity) && (loggerShowId canBeLog "consistentImp?" $ all consistentImp (loggerShowId canBeLog "imp" $ imp dataset))
          where
            canBeLog = appendLabel "canBe" treeLog
            otherPositivity = if bool then neg dataset else pos dataset

            -- | among other known data points, is the other positivity constraint satisfied?
            consistentOther = all canDischarge . filter hasUnknown

            canDischarge = any (simeq $ not bool) . loggerShowId canBeLog "filtered others" . filter notSameUnknown
            notSameUnknown point@(funcIx', varvals') = rho /= funcIx' || not (isUnknown point)

            -- | among other known data points, is the implication constraint satisfied?
            consistentImp :: ([FuncData], [FuncData]) -> Bool
            consistentImp (lhs, rhs) = let antecedent = if bool then lhs else rhs
                                           succedent = if bool then rhs else lhs
                                        in if hasUnknown antecedent
                                              then loggerShow canBeLog "has unknown, (ants, sucs)" (antecedent, succedent) $ checkImp antecedent succedent
                                              else True
            hasUnknown = any $ \(rho', varvals') -> rho == rho' && elem varvals' unknownCV
            checkImp ants sucs = any (simeq bool) sucs || canDischarge ants

            simeq True  = not . isClass False
            simeq False = not . isClass True

            isClass cl (funcIx', varvals') = let cls = classMap M.! funcIx'
                                                 target = if cl then trueC cls else falseC cls
                                              in elem varvals' . getVarVal $ target
            isUnknown (funcIx', varvals') = let cls = classMap M.! funcIx'
                                             in elem varvals' . getVarVal . unknownC $ cls

        unknownTo bool learnData@LearnData{..} = learnData { classMap = M.update (allAssign bool) rho classMap }
        allAssign bool ClassData{..} = Just ClassData { trueC = if bool then trueC ++ unknownC else trueC
                                                      , falseC = if not bool then falseC ++ unknownC else falseC
                                                      , unknownC = []
                                                      }
    rootLog = appendLabel "buildTree" learnerLog
