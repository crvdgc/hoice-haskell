{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeFamilies              #-}
module Language.Assertion where

import qualified Data.Set             as S
import qualified Data.Text            as T
import           Language.SMT2.Syntax

class (Ord var) => AST node res var | node -> res, node -> var where
  freeVars :: Ord var => node -> S.Set var
  interprete :: (var -> res) -> node -> res
  evaluate :: Monoid var => node -> res

data family ConcreteRes a

class AssertLanType a

class AssertLan lan where
  fromTerm :: AssertLanType res => Term -> Maybe (lan res T.Text)

