{-# LANGUAGE RecordWildCards #-}
module CHC.Preproc.Simplify where

import           CHC
import           Language.Assertion.LIA

simplifyCHC :: CHC FuncIx VarIx -> CHC FuncIx VarIx
-- simplifyCHC (CHC clss) = CHC . filter keep $ clss
--   where
--     keep Clause{..}
--       | null heads = False
--       | null body = phi /= LIABool False
--       | otherwise = True
simplifyCHC = id
