{-|
Module      : Learner.DecisionTree
Description : Short description
Maintainer  : liuyuxi@kb.is.s.u-tokyo.ac.jp
Stability   : experimental
Portability : POSIX

From the original HoIce paper.
-}
{-# LANGUAGE RecordWildCards #-}
module Learner.DecisionTree where

import qualified Data.IntMap            as M
import           Data.List              (foldl', partition)
import qualified Data.List.NonEmpty     as NE

import           CHC
import           Data.CounterExample
import           Language.Assertion.LIA
import           Learner.Internal

data ClassData = ClassData { trueC    :: [[VarVal]]
                           , falseC   :: [[VarVal]]
                           , unknownC :: [[VarVal]]
                           }

emptyTreeData :: ClassData
emptyTreeData = ClassData { trueC = []
                          , falseC = []
                          , unknownC = []
                          }

assignClass :: FuncMap a -> Dataset -> FuncMap ClassData
assignClass funcMap Dataset{..} = dispatchDataset emptyFuncMap
  where
    dispatchDataset = acc atUnk unknowns . acc atPos singlePos . acc atNeg singleNeg

    emptyFuncMap = M.map (const emptyTreeData) funcMap

    (singlePos, posUnk) = pickoutSingle pos
    (singleNeg, negUnk) = pickoutSingle neg

    impUnk = concat . uncurry (++) . unzip $ imp
    unknowns = posUnk ++ negUnk ++ impUnk
    acc :: ([VarVal] -> ClassData -> ClassData) -> [FuncData] -> FuncMap ClassData -> FuncMap ClassData
    acc f = flip . foldl' . flip $ \(funcIx, varvals) -> M.adjust (f varvals) funcIx

    atNeg varvals t@ClassData{..} = t { trueC = trueC ++ [varvals] }
    atPos varvals t@ClassData{..} = t { falseC = falseC ++ [varvals] }
    atUnk varvals t@ClassData{..} = t { unknownC = unknownC ++ [varvals] }

    pickoutSingle = foldl' acc ([], [])
      where
        acc (singles, unks) [funcData] = (funcData:singles, unks)      -- singleton
        acc (singles, unks) funcDatas  = (singles, unks ++ funcDatas)  -- otherwise

type Qualifier = LIA Bool VarIx

data LearnData = LearnData { classMap :: FuncMap ClassData
                           , dataset  :: Dataset
                           , quals    :: [Qualifier]
                           }

-- | pick out elements of a list one by one
mapPickout :: [a] -> [(a, [a])]
mapPickout = pickOne []
  where
    pickOne _ [] = []
    pickOne before [x] = [(x, before)]
    pickOne before (x:xs) = let rest = tail xs
                                before' = before ++ [x]
                             in (x, before ++ rest) : pickOne before' xs

learn :: CHC VarIx FuncIx -> LearnData -> FuncMap ClassData -> (LearnData, FuncMap (LIA Bool VarIx))
learn chc = M.mapAccumWithKey buildTree
  where
    buildTree :: LearnData -> FuncIx -> ClassData -> (LearnData, LIA Bool VarIx)
    buildTree learnData rho classData
      | null (falseC classData) && canBe True classData learnData rho = (unknownTo True learnData, LIABool True)
      | null (trueC classData) && canBe False classData learnData rho = (unknownTo False learnData, LIABool False)
      | otherwise = let q = selectQual learnData
                        (posData, negData) = splitData q classData
                        (learnData', posLIA) = buildTree (updateClass posData learnData) rho posData
                        (learnData'', negLIA) = buildTree (updateClass negData learnData') rho negData
                     in (learnData'', LIASeqLogic Or $ NE.fromList [ LIASeqLogic And $ NE.fromList [q, posLIA]
                                                                   , LIASeqLogic And $ NE.fromList [LIANot q, negLIA]
                                                                   ])
      where
        emptyClass f = M.null . M.filter (not . null . f)
        updateClass newClass learnData@LearnData{..} = learnData { classMap = M.update (const $ Just newClass) rho classMap }
        -- |check whether unknown data points can all be assgined to True or False
        -- - @canBe True@ implements @can_be_pos@
        -- - @canBe False@ implements @can_be_neg@
        canBe bool classData learnData@LearnData{..} rho = all consistentOther otherPositivity && all consistentImp (imp dataset)
          where
            otherPositivity = if bool then neg dataset else pos dataset

            -- | @notClass bool@ implements @≃ (not bool)@ or \(\simeq \neg \texttt{bool}\)
            notClass bool (_, varvals'') = let cl = if bool then trueC classData else falseC classData
                                            in notElem varvals'' cl
            -- | among other known data points, is the other positivity constraint satisfied?
            consistentOther = all hasOther . mapPickout
            hasOther ((funcIx, _), others) = any (notClass bool) . filter notSameUnknown $ others
            notSameUnknown (funcIx', varvals') = funcIx' /= rho || notElem varvals' (unknownC classData)

            -- | among other known data points, is the implication constraint satisfied?
            consistentImp :: ([FuncData], [FuncData]) -> Bool
            consistentImp (lhs, rhs) = let antecedent = if bool then lhs else rhs
                                           succedent = if bool then rhs else lhs
                                        in if hasUnknown antecedent
                                              then checkImp antecedent succedent
                                              else True
            hasUnknown = any $ \(rho', varvals') -> rho == rho' && elem varvals' (unknownC classData)
            checkImp ants sucs = any (isClass $ not bool) sucs || (any (isClass bool) . filter notSameUnknown $ ants)
            isClass cl (funcIx', varvals') = let cls = classMap M.! funcIx'
                                                 target = if cl then trueC cls else falseC cls
                                              in elem varvals' target

        unknownTo bool learnData@LearnData{..} = learnData { classMap = M.update (allAssign bool) rho classMap }
        allAssign bool ClassData{..} = Just ClassData { trueC = if bool then trueC ++ unknownC else trueC
                                                      , falseC = if not bool then falseC ++ unknownC else falseC
                                                      , unknownC = []
                                                      }
        selectQual = head . quals
        splitData q classData@ClassData{..} = let (posTrueC, negTrueC) = splitVarvals q trueC
                                                  (posFalseC, negFalseC) = splitVarvals q falseC
                                                  (posUnknownC, negUnknownC) = splitVarvals q unknownC
                                               in ( ClassData { trueC = posTrueC, falseC = posFalseC, unknownC = posUnknownC }
                                                  , ClassData { trueC = negTrueC, falseC = negFalseC, unknownC = negUnknownC }
                                                  )
          where
            splitVarvals q = partition $ \varvals -> evaluateLIABool (varvals !!) q
