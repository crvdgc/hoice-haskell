module CHC.Preproc where

import           CHC

data IndexedCHC v f = IndexedCHC
  { funcArity :: FuncMap Int
  , chc       :: CHC v f
  }

