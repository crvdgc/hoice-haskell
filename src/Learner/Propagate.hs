{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Learner.Propagate where

import           Data.List           (foldl')

import           Debug.Logger

import           CHC
import           Data.CounterExample
import           Learner.Internal


-- | With a set of known points and constraints, classify unknown points and simplify the constraints
propagate :: FuncMap ClassData -> Dataset -> Maybe (FuncMap ClassData, Dataset)
propagate classMap dataset = do
  dataset' <- loggerShowId propagateLog "after removeKnown" $ removeKnown classMap dataset
  ((pointsPos, pointsNeg), dataset'') <- simplifyWithChanged dataset'
  let classMap' = foldl' (flip $ updateUnkClass True) classMap pointsPos
  let classMap'' = foldl' (flip $ updateUnkClass False) classMap' pointsNeg
  pure (classMap'', dataset'')

-- | Remove known classification from constraints
--   @Nothing@ means contradiction
--   Newly discovered classification can be obtained from simplifying the result constraint
removeKnown :: FuncMap ClassData -> Dataset -> Maybe Dataset
removeKnown classMap Dataset{..} = do
  pos' <- loggerShowId propagateLog "pos" $ filter (not . null) <$> mapM (removePoints True) pos
  neg' <- loggerShowId propagateLog "neg" $ filter (not . null) <$> mapM (removePoints False) neg
  imp' <- loggerShowId propagateLog "imp" $ removeImp imp
  pure $ Dataset pos' neg' imp'

  where
    classOf = getFuncMaybeClass classMap

    removePoints :: Bool -> [FuncData] -> Maybe [FuncData]
    removePoints _ [] = Just []
    removePoints good (p:ps) = if classOf p == Just good
                                  then removePoints good ps          -- known, no need to keep
                                  else (p:) <$> removePoints good ps -- unknown, or could be discharge

    removeImp :: [([FuncData], [FuncData])] -> Maybe [([FuncData], [FuncData])]
    removeImp = mapM $ \(lhs, rhs) -> do
      lhs' <- loggerShowId propagateLog "lhs" $ removePoints True lhs
      rhs' <- loggerShowId propagateLog "rhs" $ removePoints False rhs
      pure (lhs', rhs')

propagateLog = appendLabel "propagate" hoiceLog


