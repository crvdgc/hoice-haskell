{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Assertion.LIA where

import           Control.Monad
import           Control.Monad.State
import qualified Data.IntMap          as M
import qualified Data.List.NonEmpty   as NE
import qualified Data.Set             as S
import qualified Data.Text            as T
import qualified Data.Text.Read       as TR
import           Language.SMT2.Syntax

data ArithOp = Add | Sub | Mul
  deriving Eq

instance Show ArithOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"

data AssertOp = Lt | Le | Eql | Ge | Gt
  deriving Eq

instance Show AssertOp where
  show Lt  = "<"
  show Le  = "<="
  show Eql = "="
  show Ge  = ">="
  show Gt  = ">"

data SeqLogicOp = And | Or
  deriving Eq

instance Show SeqLogicOp where
  show And = "and"
  show Or  = "or"

data LIA res var where
  LIAVar  :: var -> LIA Int var
  LIAInt  :: Int -> LIA Int var
  LIABool :: Bool -> LIA Bool var
  LIAArith :: ArithOp -> LIA Int var -> LIA Int var -> LIA Int var
  LIAAssert :: AssertOp -> LIA Int var -> LIA Int var -> LIA Bool var
  LIANot  :: LIA Bool var -> LIA Bool var
  LIASeqLogic :: SeqLogicOp -> NE.NonEmpty (LIA Bool var) -> LIA Bool var

instance Eq var => Eq (LIA res var) where
  LIAVar v1 == LIAVar v2 = v1 == v2
  LIAInt n1 == LIAInt n2 = n1 == n2
  LIABool b1 == LIABool b2 = b1 == b2
  LIAArith op1 t1 t2 == LIAArith op2 t3 t4 = op1 == op2 && t1 == t3 && t2 == t4
  LIAAssert op1 t1 t2 == LIAAssert op2 t3 t4 = op1 == op2 && t1 == t3 && t2 == t4
  LIANot t1 == LIANot t2 = t1 == t2
  LIASeqLogic op1 ts1 == LIASeqLogic op2 ts2 = op1 == op2 && ts1 == ts2
  _ == _ = False

instance Show var => Show (LIA res var) where
  show node = case node of
                LIAVar v           -> "$" <> show v
                LIAInt n           -> show n
                LIABool b          -> if b then "true" else "false"
                LIAArith op t1 t2  -> wrap [show op, show t1, show t2]
                LIAAssert op t1 t2 -> wrap [show op, show t1, show t2]
                LIANot t           -> wrap ["not", show t]
                LIASeqLogic op ts  -> wrap . (show op:) . map show . NE.toList $ ts
    where wrap xs = unwords $ ("(":xs) ++ [")"]


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


freeVarsLIA :: Ord var => LIA res var -> S.Set var
freeVarsLIA ast = case ast of
                    LIAVar v          -> S.singleton v
                    LIAInt _          -> S.empty
                    LIABool _         -> S.empty
                    LIAArith _ t1 t2  -> freeVarsLIA t1 `S.union` freeVarsLIA t2
                    LIAAssert _ t1 t2 -> freeVarsLIA t1 `S.union` freeVarsLIA t2
                    LIANot t          -> freeVarsLIA t
                    LIASeqLogic _ ts  -> S.unions . NE.map freeVarsLIA $ ts



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

evaluateLIAInt :: (var -> Int) -> LIA Int var -> Int
evaluateLIAInt rho ast = case ast of
                           LIAVar v -> rho v
                           LIAInt n -> n
                           LIAArith op t1 t2 -> let v1 = evaluateLIAInt rho t1
                                                    v2 = evaluateLIAInt rho t2
                                                 in case op of
                                                      Add -> v1 + v2
                                                      Sub -> v1 - v2
                                                      Mul -> v1 * v2

evaluateLIABool :: (var -> Int) -> LIA Bool var -> Bool
evaluateLIABool rho ast = case ast of
                            LIABool b -> b
                            LIAAssert op t1 t2 -> let v1 = evaluateLIAInt rho t1
                                                      v2 = evaluateLIAInt rho t2
                                                   in case op of
                                                        Lt  -> v1 < v2
                                                        Le  -> v1 <= v2
                                                        Eql -> v1 == v2
                                                        Ge  -> v1 >= v2
                                                        Gt  -> v1 > v2
                            LIANot t -> let v = evaluateLIABool rho t
                                         in not v
                            LIASeqLogic op ts -> let vs = NE.map (evaluateLIABool rho) ts
                                                  in case op of
                                                       And -> and vs
                                                       Or  -> or vs

indexVarLIA :: (Ord var) => LIA res var -> (LIA res Int, M.IntMap var)
indexVarLIA lia = (indexed, ixs)
  where
    fvs = freeVarsLIA lia
    indexed = fmap (`S.findIndex` fvs) lia
    ixs = M.fromAscList . zip [0..] . S.toAscList $ fvs


