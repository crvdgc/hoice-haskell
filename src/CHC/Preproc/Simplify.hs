{-# LANGUAGE RecordWildCards #-}
module CHC.Preproc.Simplify where

import CHC

simplifyCHC :: CHC FuncIx VarIx -> CHC FuncIx VarIx
simplifyCHC (CHC clss) = CHC filtered
  where
    filtered = [ cls | cls <- clss, keep cls ]
    keep Clause{..}
      | null heads || null body = False
      | otherwise = True
