{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Language.Assertion.LIA where

import           Control.Monad
import qualified Data.List.NonEmpty   as NE
import qualified Data.Set             as S
import qualified Data.Text            as T
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

instance (Ord var) => AST (LIA res var) res var where
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
  evaluate = undefined


parseTermLIA :: Term -> Maybe (Either (LIA Int T.Text) (LIA Bool T.Text))
parseTermLIA t = case t of
  TermSpecConstant (SCNumeral n) ->  case TR.decimal n of
                                       Left _           -> Nothing
                                       Right (n', rest) -> Just . Left . LIAInt $ n'
  TermQualIdentifier (Unqualified (IdSymbol b)) -> case b of
                                                     "true"  -> Just . Right . LIABool $ True
                                                     "false" -> Just . Right . LIABool $ False
                                                     v -> Just . Left . LIAVar $ v
  TermQualIdentifier (Qualified (IdSymbol b) (SortSymbol (IdSymbol s))) -> case s of
                                                                             "Bool" -> case b of
                                                                                         "true" -> Just . Right . LIABool $ True
                                                                                         "false" -> Just . Right . LIABool $ False
                                                                                         _ -> Nothing
                                                                             "Int" -> Just . Left . LIAVar $ b
  TermApplication f ts -> case f of
    Unqualified (IdSymbol s) -> parseNode s ts Nothing
    Qualified (IdSymbol s) (SortSymbol (IdSymbol srt)) -> parseNode s ts (Just srt)
  where
    parseNode s ts msrt
      | s `elem` ["+", "-", "*"] = if length ts == 2 && msrt /= Just "Bool"
                                     then parseArithm s (NE.head ts) (NE.last ts)
                                     else Nothing
      | s `elem` ["<", "<=", "=", ">=", ">"] = if length ts == 2 && msrt /= Just "Int"
                                                 then parseAssert s (NE.head ts) (NE.last ts)
                                                 else Nothing
      | s == "not" = if length ts == 1 && msrt /= Just "Int"
                        then parseNot (NE.head ts)
                        else Nothing
      | s `elem` ["and", "or"] = if msrt /= Just "Int"
                                    then parseSeq s ts
                                    else Nothing
      | otherwise = Nothing
    parseArithm s t1 t2 = do
      v1 <- parseInt t1
      v2 <- parseInt t2
      Just . Left $ case s of
                      "+" -> LIAArith Add v1 v2
                      "-" -> LIAArith Sub v1 v2
                      "*" -> LIAArith Mul v1 v2
    parseAssert s t1 t2 = do
      v1 <- parseInt t1
      v2 <- parseInt t2
      Just . Right $ case s of
                       "<"  -> LIAAssert Lt v1 v2
                       "<=" -> LIAAssert Le v1 v2
                       "="  -> LIAAssert Eql v1 v2
                       ">=" -> LIAAssert Ge v1 v2
                       ">"  -> LIAAssert Gt v1 v2
    parseNot t = do
      v <- parseBool t
      Just . Right . LIANot $ v
    parseSeq s ts = do
      vs <- forM ts $ \t -> parseBool t
      Just . Right $ case s of
                       "and" -> LIASeqLogic And vs
                       "or"  -> LIASeqLogic Or vs
    parseInt t = do
      v <- parseTermLIA t
      case v of
        Left v' -> Just v'
        Right _ -> Nothing
    parseBool t = do
      v <- parseTermLIA t
      case v of
        Left _   -> Nothing
        Right v' -> Just v'

