module CHC.Preproc where

import           CHC

data IndexedCHC v f = IndexedCHC
  { funcArity :: FuncMap Int
  , chc       :: CHC v f
  }

-- applyStrategies :: (Eq v, Eq f) => [IndexedCHC v f -> IndexedCHC v f] -> IndexedCHC v f -> [IndexedCHC v f]
-- applyStrategies = Data.List.iterate

