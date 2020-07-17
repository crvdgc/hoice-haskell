{-# LANGUAGE RecordWildCards #-}
module Data.CounterExample where

type FuncIx = Int
type VarVal = Int

type FuncData = (FuncIx, [VarVal])

data Dataset = Dataset { pos :: [[FuncData]]               -- ^ at least one is @true@
                       , neg :: [[FuncData]]               -- ^ at least one is @false@
                       , imp :: [([FuncData], [FuncData])] -- ^ if lhs all @true@, rhs has at least one @true@
                       }

data TreeData = TreeData { quals :: ()
                         , posT  :: [[VarVal]]
                         , negT  :: [[VarVal]]
                         , unkT  :: [[VarVal]]
                         }

selectFunc :: Dataset -> FuncIx -> TreeData
selectFunc Dataset{..} f = undefined

