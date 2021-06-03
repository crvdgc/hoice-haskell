{-# LANGUAGE RecordWildCards #-}
module CHC.Preproc.Simplify where

import           CHC

simplifyCHC :: CHC FuncIx VarIx -> CHC FuncIx VarIx
simplifyCHC (CHC clss) = CHC . filter keep $ clss
  where
    keep Clause{..}
      | null heads || null body = False
      | otherwise = True
