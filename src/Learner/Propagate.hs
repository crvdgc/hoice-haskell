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
  pure . loggerShowId propagateLog "after propagtion" $ (classMap'', dataset'')

-- | Remove known classification from constraints
--   @Nothing@ means contradiction
--   Newly discovered classification can be obtained from simplifying the result constraint
removeKnown :: FuncMap ClassData -> Dataset -> Maybe Dataset
removeKnown classMap Dataset{..} = do
  pos' <- loggerShowId propagateLog "pos" $ filter (not . null) <$> mapM (atLeastOneIs True) pos
  neg' <- loggerShowId propagateLog "neg" $ filter (not . null) <$> mapM (atLeastOneIs False) neg
  imp' <- loggerShowId propagateLog "imp" $ removeImp imp
  pure $ Dataset pos' neg' imp'

  where
    classOf = getFuncMaybeClass classMap

    -- succeed if @good@, check next if @not good@, keep if unknown, fail on empty
    atLeastOneIs :: Bool -> [FuncData] -> Maybe [FuncData]
    atLeastOneIs _ [] = Nothing                                             -- fail on empty
    atLeastOneIs good (p:ps) = case classOf p of
                                 Just known -> if known == good
                                                  then Just []              -- known, can discharge, whole constraint satisfied
                                                  else atLeastOneIs good ps -- known, can't discharge, check next
                                 Nothing -> (p:) <$> atLeastOneIs good ps          -- unknown, need to be kept in the constraint

    -- check next if @good@, fail if @not good@, keep if unknown, succeed on empty
    everyOneIs :: Bool -> [FuncData] -> Maybe [FuncData]
    everyOneIs _ [] = Just []                                               -- succeed on empty
    everyOneIs good (p:ps) = case classOf p of
                               Just known -> if known == good
                                                then everyOneIs good ps     -- known, can't discharge, check next
                                                else Nothing                -- known, impossible to discharge, fail
                               Nothing -> (p:) <$> everyOneIs good ps

    removeImp :: [([FuncData], [FuncData])] -> Maybe [([FuncData], [FuncData])]
    removeImp = mapM $ \(lhs, rhs) -> do
      lhs' <- loggerShowId propagateLog "lhs" $ everyOneIs True lhs
      rhs' <- loggerShowId propagateLog "rhs" $ atLeastOneIs True rhs
      pure (lhs', rhs')

propagateLog = appendLabel "propagate" hoiceLog

