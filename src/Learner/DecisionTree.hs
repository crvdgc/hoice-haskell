{-# LANGUAGE RecordWildCards #-}
module Learner.DecisionTree where

import           Data.CounterExample
import           Learner.Internal

data TreeData = TreeData { quals :: ()
                         , posT  :: [[VarVal]]
                         , negT  :: [[VarVal]]
                         , unkT  :: [[VarVal]]
                         }

selectFunc :: Dataset -> FuncIx -> TreeData
selectFunc Dataset{..} f = TreeData { quals = ()
                                    , posT = singlePos
                                    , negT = singleNeg
                                    , unkT = unkowns }
  where
    (singlePos, posUnk) = pickout isSingleton pos
    (singleNeg, negUnk) = pickout isSingleton neg
    (_, impUnk) = pickout (const False) . combine $ imp
    unkowns = posUnk ++ negUnk ++ impUnk
    isSingleton [_] = True
    isSingleton _   = False
    combine xys = uncurry (++) . unzip
    pickout = undefined

