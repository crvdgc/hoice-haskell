module Data.CounterExample where

import           Data.List (union)

import           CHC       (FuncIx, VarVal)

type FuncData = (FuncIx, [VarVal])

data Dataset = Dataset { pos :: [[FuncData]]               -- ^ at least one is @true@
                       , neg :: [[FuncData]]               -- ^ at least one is @false@
                       , imp :: [([FuncData], [FuncData])] -- ^ if lhs all @true@, rhs has at least one @true@
                       }
  deriving (Eq, Show)

emptyDataset :: Dataset
emptyDataset = Dataset { pos = []
                       , neg = []
                       , imp = []
                       }

instance Semigroup Dataset where
  d <> d' = Dataset (pos d `union` pos d') (neg d `union` neg d') (imp d `union` imp d')

instance Monoid Dataset where
  mempty = emptyDataset
