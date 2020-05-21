{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Assertion.LIA where

import qualified Data.List.NonEmpty   as NE
import qualified Data.Set             as S
import qualified Data.Text.Read       as TR
import           Language.Assertion
import           Language.SMT2.Syntax

data ArithOp = Add | Sub | Mul

data AssertOp = Lt | Le | Eql | Ge | Gt

data SeqLogicOp = And | Or

data LIA res var where
  LIAVar  :: var -> LIA Int var
  LIAInt  :: Int -> LIA Int var
  LIABool :: Bool -> LIA Bool var
  LIAArith :: ArithOp -> LIA Int var -> LIA Int var -> LIA Int var
  LIAAssert :: AssertOp -> LIA Int var -> LIA Int var -> LIA Bool var
  LIANot  :: LIA Bool var -> LIA Bool var
  LIASeqLogic :: SeqLogicOp -> NE.NonEmpty (LIA Bool var) -> LIA Bool var

instance Functor (LIA res) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f ast = case ast of
                 LIAVar v           -> LIAVar (f v)
                 LIAInt n           -> LIAInt n
                 LIABool b          -> LIABool b
                 LIAArith op t1 t2  -> LIAArith op (f <$> t1) (f <$> t2)
                 LIAAssert op t1 t2 -> LIAAssert op (f <$> t1) (f <$> t2)
                 LIANot t           -> LIANot (f <$> t)
                 LIASeqLogic op ts  -> LIASeqLogic op $ NE.map (f <$>) ts

instance AST (LIA res var) res var where
  -- freeVars :: lan -> S.Set var
  freeVars ast = case ast of
                   LIAVar v          -> S.singleton v
                   LIAInt _          -> S.empty
                   LIABool _         -> S.empty
                   LIAArith _ t1 t2  -> freeVars t1 `S.union` freeVars t2
                   LIAAssert _ t1 t2 -> freeVars t1 `S.union` freeVars t2
                   LIANot t          -> freeVars t
                   LIASeqLogic _ ts  -> S.unions . NE.map freeVars $ ts

  -- interprete :: (var -> res) -> lan -> res
  interprete = undefined

