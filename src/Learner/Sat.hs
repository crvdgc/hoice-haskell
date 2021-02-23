{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Learner.Sat where

import qualified Data.IntMap         as M
import           Data.List           (foldl')
import qualified Data.Set            as S

import           SAT.Mios            (CNFDescription (..), solveSAT)

import           Data.CounterExample

import           Debug.Logger

-- | @Dataset@ is a set of constraints for unknown points (should be propagated first)
-- it will return a classification of all points if it satisfies the constraints
satSolve :: Dataset -> IO (Maybe KnownPair)
satSolve dataset = let (pointSet, index) = indexDataset $ loggerShowId satLog "sat gets" dataset
                       clauses = datasetToClauses pointSet dataset

                       numVar = S.size pointSet
                       numClauses = length clauses

                       desc = CNFDescription numVar numClauses "file"
                    in do
                      asg <- solveSAT desc clauses
                      if null asg
                         then pure Nothing
                         else pure . Just $ loggerShowId satLog "sat classifies" $ collectToKnownPair asg index

indexDataset :: Dataset -> (S.Set FuncData, M.IntMap FuncData)
indexDataset dataset = (pointSet, index)
  where
    points = allFuncData dataset
    pointSet = S.fromList points
    index = M.fromAscList . zip [1..] . S.toAscList $ pointSet

datasetToClauses :: S.Set FuncData -> Dataset -> [[Int]]
datasetToClauses pointSet Dataset{..} = clausesPos <> clausesNeg <> clausesImp
  where
    findIndex = (1 +) . (`S.findIndex` pointSet)
    toPos = fmap findIndex
    toNeg = fmap (negate . findIndex)
    clausesPos = fmap toPos pos
    clausesNeg = fmap toNeg neg
    clausesImp = fmap (\(lhs, rhs) -> toNeg lhs <> toPos rhs) imp

collectToKnownPair :: [Int] -> M.IntMap FuncData -> KnownPair
collectToKnownPair asg index = foldl' acc ([], []) asg
  where
    acc :: KnownPair -> Int -> KnownPair
    acc (accPos, accNeg) curAsg = if curAsg > 0
                                     then (index M.! curAsg:accPos, accNeg)
                                     else (accPos, index M.! negate curAsg:accNeg)

satLog = appendLabel "sat" hoiceLog
