{-# LANGUAGE RecordWildCards #-}
module Learner.DecisionTree where

import           Data.List           (foldl')

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
                                    , unkT = unknowns }
  where
    (singlePos, posUnk) = pickoutSingle pos
    (singleNeg, negUnk) = pickoutSingle neg
    impUnk = pickoutAll . combine $ imp
    unknowns = posUnk ++ negUnk ++ impUnk
    isSingleton [_] = True
    isSingleton _   = False
    combine = concat . uncurry (++) . unzip
    pickoutAll = map snd  . filter ((== f) . fst)
    pickoutSingle = foldl' acc ([], [])
      where
        acc (singles, unks) [(f, varvals)] = (varvals:singles, unks)           -- singleton
        acc (singles, unks) funDatas = (singles, pickoutAll funDatas ++ unks)  -- otherwise

