{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
module Language.Assertion where

import qualified Data.Set             as S
import           Language.SMT2.Syntax

class AST node res var | node -> res, node -> var where
  freeVars :: Ord var => node -> S.Set var
  interprete :: (var -> res) -> node -> res

class AssertionLanguage lan where
  fromTerm :: forall res var. Term -> Maybe (lan res var)

