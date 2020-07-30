module Data.CounterExample where

import qualified Data.IntMap as M

type FuncIx = Int
type VarVal = Int

type FuncData = M.IntMap [VarVal]

data Dataset = Dataset { pos :: [[FuncData]]               -- ^ at least one is @true@
                       , neg :: [[FuncData]]               -- ^ at least one is @false@
                       , imp :: [([FuncData], [FuncData])] -- ^ if lhs all @true@, rhs has at least one @true@
                       }


