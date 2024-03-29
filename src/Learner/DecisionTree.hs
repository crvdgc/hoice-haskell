{-|
Module      : Learner.DecisionTree
Description : Synthesize the predicates with decision tree
Maintainer  : liuyuxi@kb.is.s.u-tokyo.ac.jp
Stability   : experimental
Portability : POSIX

From the original HoIce paper.
-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Learner.DecisionTree where

import           Debug.Logger

import qualified Data.IntMap            as M
import           Data.List              (elemIndex, foldl', maximumBy,
                                         partition)
import qualified Data.List.NonEmpty     as NE
import           Data.Maybe             (fromJust)
import qualified Data.Set               as S
import qualified Data.Text              as T

import           CHC
import           Data.CounterExample
import           Language.Assertion.LIA
import           Learner.Internal
import           Learner.Propagate

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
    pairs (x:xs) = map (x, ) xs ++ pairs xs

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
degree classData v = sum . map degreeP . filter ((== v) . vals) . allClassData $ classData

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
                                        boundedAtomics = map (fmap (fromJust . flip elemIndex args)) atomics
                                     in intmapAppend func boundedAtomics funcMap

getBooleanAtomic :: S.Set VarIx -> LIA Bool VarIx -> [Qualifier]
getBooleanAtomic args lia = if S.isSubsetOf (freeVarsLIA lia) args
                              then allBooleanAtomic lia
                              else concatMap (getBooleanAtomic args) $ subBooleanLIAs lia
  where
    allBooleanAtomic :: LIA Bool VarIx -> [Qualifier]
    allBooleanAtomic = \case
      LIABool _ -> []
      LIAAssert op t1 t2 -> [LIAAssert op t1 t2]
      LIANot t -> allBooleanAtomic t
      LIABoolEql t1 t2 -> [LIABoolEql t1 t2] ++ allBooleanAtomic t1 ++ allBooleanAtomic t2
      LIASeqLogic op ts -> LIASeqLogic op ts : concatMap allBooleanAtomic ts
    subBooleanLIAs :: LIA Bool VarIx -> [LIA Bool VarIx]
    subBooleanLIAs = \case
      LIABool _ -> []
      LIAAssert {} -> []
      LIANot t -> [t]
      LIABoolEql t1 t2 -> [t1, t2]
      LIASeqLogic _ ts -> NE.toList ts


splitData :: Qualifier -> ClassData -> (ClassData, ClassData)
splitData q ClassData{..} = let (posTrueC, negTrueC) = splitVarvals q trueC
                                (posFalseC, negFalseC) = splitVarvals q falseC
                                (posUnknownC, negUnknownC) = splitVarvals q unknownC
                             in ( ClassData posTrueC posFalseC posUnknownC
                                , ClassData negTrueC negFalseC negUnknownC
                                )
  where
    splitVarvals q = partition $ \p -> evaluateLIABool (vals p !!) q

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
                              in loggerShow (appendLabel "selectQual" learnerLog) "gains" qualGains $ maximumBy compareSnd qualGains

deleteAll :: Eq a => a -> [a] -> [a]
deleteAll x = filter (/= x)

pickoutQual :: [Qualifier] -> ClassData -> Int -> [[VarVal]] -> Maybe (Qualifier, [Qualifier])
pickoutQual quals classData arity varvals = if loggerShow pickLog "classData to split" classData $ null quals
                                               then logger pickLog "start mining because quals empty" pickoutMine
                                               else let (bestQual, maxGain) = loggerShow pickLog "picking from quals" quals $ selectQual quals classData
                                                     in if hasEmpty bestQual
                                                           then logger pickLog "start mining because best from quals cannot split" $ loggerShow pickLog "bestQual" bestQual pickoutMine
                                                           else let res = (bestQual, deleteAll bestQual quals)
                                                                 in loggerShow pickLog "maxGain" maxGain $ loggerShow pickLog "bestQual" bestQual $ Just res
  where
    pickLog = appendLabel "pickoutQual" learnerLog
    pickoutMine = let mined = loggerShow pickLog "mining from" varvals $ mineQuals arity varvals
                      (bestMined, maxGainMined) = selectQual mined classData
                   in if hasEmpty bestMined
                         then logger pickLog "even best mined cannot split" . loggerShow pickLog "mined" mined . loggerShow pickLog "bestMined" bestMined $ Nothing
                         else let res = (bestMined, deleteAll bestMined $ mined ++ quals)
                               in loggerShow pickLog "maxGainMined" maxGainMined . loggerShow pickLog "bestMined" bestMined $ Just res

    hasEmpty qual = let (classP, classN) = splitData qual classData
                     in isEmptyClassData classP || isEmptyClassData classN

-- |check whether unknown data points can all be assgined to True or False
-- - @canBe True@ implements @can_be_pos@
-- - @canBe False@ implements @can_be_neg@
canBe :: LogInfo -> Bool -> ClassData -> LearnData -> FuncIx -> Bool
canBe treeLog bool classData LearnData{..} rho = canBeLogger $ loggerShowId canBeLog "consistentOther?" (consistentOther otherPositivity) && loggerShowId canBeLog "consistentImp?" (all consistentImp (loggerShowId canBeLog "imp" $ imp dataset))
  where
    unknownCV = getVarVal . unknownC $ classData
    canBeLog = appendLabel ("canBe " <> if bool then "pos" else "neg") treeLog
    canBeLogger = loggerShow canBeLog "rho, classData" (rho, bool, classData)
    otherPositivity = if bool then neg dataset else pos dataset

    -- | among other known data points, is the other positivity constraint satisfied?
    consistentOther = all canDischarge . filter hasUnknown

    canDischarge = any (simeqFunc (not bool) classMap) . loggerShowId canBeLog "filtered others" . filter notSameUnknown
    notSameUnknown (funcIx', varvals') = rho /= funcIx' || varvals' `notElem` unknownCV

    -- | among other known data points, is the implication constraint satisfied?
    consistentImp :: ([FuncData], [FuncData]) -> Bool
    consistentImp (lhs, rhs) = let antecedent = if bool then lhs else rhs  -- at least one has a different positivity
                                   succedent = if bool then rhs else lhs   -- at least one has the same positivity
                                in not (hasUnknown antecedent) || loggerShow canBeLog "has unknown, (ants, sucs)" (antecedent, succedent) (checkImp antecedent succedent)
    hasUnknown = any $ \(rho', varvals') -> rho == rho' && elem varvals' unknownCV
    checkImp ants sucs = any (simeqFunc bool classMap) sucs || canDischarge ants

type TreeNode = (Maybe LearnData, LIA Bool VarIx)

failNode = (Nothing, LIABool True)

learn :: FuncMap Int -> LearnData -> (Maybe LearnData, FuncMap (LIA Bool VarIx))
learn arityMap learnData = M.mapAccumWithKey dispatchTree (Just learnData) arityMap
  where
    -- | @classMap@ in learnData represents the global class assginment, while @classData@ is the local data to be classified
    dispatchTree :: Maybe LearnData -> FuncIx -> a -> TreeNode
    dispatchTree Nothing _ _ = failNode
    dispatchTree (Just learnData) funcIx _ = buildTree rootLog learnData funcIx (classMap learnData M.! funcIx)

    -- The return type:
    --   * Left (LearnData, LIA Bool VarIx)
    --       Fail at the current node or in subtrees
    --       Therefore use SAT to classify all data points, and get the synth result @LIA Bool VarIx@
    --       Forward the result directly to the root, abort other trees
    --   * Right TreeNode
    --       Either successfully use HoIce method,
    --       or use SAT to classify, this will try only two status (no backtracking)
    --          * The current status
    --              We assume all previous classification is possible to satisfy all constraints.
    --              Therefore, SAT only tries to assign unknown points.
    --              This could fail because there might be contradiction.
    --          * The initial status
    --              In this case, we rebuild @classMap@ from @allDataset@, only keep points classified with certainty (singleton, or from simplification)
    --              Then SAT assign all other points.
    --              If this also fails, then it's not possible to satisfy the constraints.
    buildTree :: LogInfo -> LearnData -> FuncIx -> ClassData -> TreeNode
    buildTree treeLog learnData rho classData
      | treeLogger $ null falseCV && canBe treeLog True classData learnData rho = loggerShowId treeLog "leaf true" $ unknownTo True unks learnData
      | null trueCV && canBe treeLog False classData learnData rho = loggerShowId treeLog "leaf false" $ unknownTo False unks learnData
      | otherwise = case maybeQual of
                      -- @Nothing@ means contradiction, use SAT solver to rebuild tree
                      Nothing -> failNode
                      Just (q, quals') -> let (posData, negData) = loggerShow synLog "best qual" q $ splitLogger $ splitData q classData
                                              learnDataQual = learnData { quals = M.update (const $ Just quals') rho qualMap }
                                              (maybeLearnData', posLIA) = buildTree (incLevel treeLog) learnDataQual rho posData
                                           in case maybeLearnData' of
                                                Nothing -> failNode
                                                Just learnData' -> let learnDataQual' = learnData' { quals = M.update (const $ Just quals') rho qualMap }
                                                                       (maybeLearnData'', negLIA) = buildTree (incLevel treeLog) learnDataQual' rho negData
                                                                    in case maybeLearnData'' of
                                                                         Nothing -> failNode
                                                                         Just learnData'' -> let lia = loggerShowId treeLog "tree" $ flatOr (flatAnd q posLIA) (flatAnd (flatNot q) negLIA)
                                                                                              in (Just learnData'', lia)
      where
        synLog = appendLabel ("synth for predicate #" <> T.pack (show rho)) treeLog
        pickLog = appendLabel "pickoutQual" learnerLog
        qualLogger = loggerShow pickLog "rho" rho . loggerShow pickLog "learnData" learnData  . loggerShow pickLog "classData" classData
        splitLogger = loggerShow synLog "before split" classData . loggerShowId synLog "after split"
        treeLogger = loggerShow treeLog "rho" rho $ loggerShow treeLog "learnData" learnData  $ loggerShow treeLog "classData" classData

        qualMap = quals learnData
        qual = loggerShowId synLog "orginal quals" $ qualMap M.! rho
        arity = arityMap M.! rho
        maybeQual = qualLogger $ pickoutQual qual classData arity . getVarVal . allClassData $ classData

        trueCV = getVarVal . trueC $ classData
        falseCV = getVarVal . falseC $ classData
        unks = unknownC classData

        unknownTo bool unks LearnData{..} = let unkPoints = map (rho,) $ getVarVal unks
                                             in case propagate (allAssign classMap unkPoints) dataset of
                                                  Nothing -> failNode
                                                  Just (classMap', dataset') -> let learnData' = LearnData classMap' dataset' quals
                                                                                 in (Just learnData', LIABool bool)
          where
            allAssign :: FuncMap ClassData -> [FuncData] -> FuncMap ClassData
            allAssign = foldl' (flip $ updateUnkClass bool)

    rootLog = appendLabel "buildTree" learnerLog

