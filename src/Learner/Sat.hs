{-# LANGUAGE RecordWildCards #-}
module Learner.Sat where

import           SAT.Mios            (CNFDescription (..), solveSAT)

import           CHC
import           Data.CounterExample
import           Learner.Internal

satClassify :: FuncMap ClassData -> Dataset -> IO (Maybe (FuncMap ClassData))
satClassify classMap Dataset{..} = undefined


