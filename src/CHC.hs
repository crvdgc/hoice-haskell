{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module CHC where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.IntMap
import qualified Data.IntSet          as S
import qualified Data.List.NonEmpty   as NE
import           Data.STRef
import qualified Data.Text            as T
import qualified Data.Text.Read       as TR
import           Language.SMT2.Syntax

type Arr a = Array Int a

type Var = Int -- index of variables
type VarMap a = IntMap a
type VarArr e = Array Var e
type VarVal  = Int

type Pred = Int -- uninterpreted preds
type PredMap a = IntMap a
type PredArr e = Array Pred e
type PredVal = [VarVal] -> Bool -- sort must match

-- | a CHC system, parameterized by an assertion language
type Pi = Arr CHC

-- | an application of an uninterpreted pred
data PredApp = PredApp { pred   :: Pred
                       , params :: Arr Var
                       }

-- | a single CHC clause
-- forall vars. heads <- body /\ phi
data CHC = CHC { vars  :: Arr Var
               , heads :: Arr PredApp -- disjunction of preds
               , body  :: Arr PredApp -- conjunction of preds
               , phi   :: LIA Bool  -- formula of assertion language
               }

data LIA res where
  LIAVar  :: Var -> LIA Int
  LIAInt  :: Int -> LIA Int
  LIABool :: Bool -> LIA Bool
  LIAAdd  :: LIA Int -> LIA Int -> LIA Int
  LIASub  :: LIA Int -> LIA Int -> LIA Int
  LIAMul  :: LIA Int -> LIA Int -> LIA Int
  LIALt   :: LIA Int -> LIA Int -> LIA Bool
  LIALe   :: LIA Int -> LIA Int -> LIA Bool
  LIAEq   :: LIA Int -> LIA Int -> LIA Bool
  LIAGe   :: LIA Int -> LIA Int -> LIA Bool
  LIAGt   :: LIA Int -> LIA Int -> LIA Bool
  LIANot  :: LIA Bool -> LIA Bool
  LIAAnd  :: NE.NonEmpty (LIA Bool) -> LIA Bool
  LIAOr   :: NE.NonEmpty (LIA Bool) -> LIA Bool

type VarSet = S.IntSet

varSetToList :: VarSet -> [Var]
varSetToList = S.toList

freeVars :: LIA a -> VarSet
freeVars lia = case lia of
  (LIAVar v)     -> S.singleton v
  (LIAInt _)     -> S.empty
  (LIABool _)    -> S.empty
  (LIAAdd t1 t2) -> f t1 t2
  (LIASub t1 t2) -> f t1 t2
  (LIAMul t1 t2) -> f t1 t2
  (LIALt t1 t2)  -> f t1 t2
  (LIALe t1 t2)  -> f t1 t2
  (LIAEq t1 t2)  -> f t1 t2
  (LIAGe t1 t2)  -> f t1 t2
  (LIAGt t1 t2)  -> f t1 t2
  (LIANot t)     -> freeVars t
  (LIAAnd ts)    -> g ts
  (LIAOr ts)     -> g ts
  where
    f t1 t2 = freeVars t1 `S.union` freeVars t2
    g = S.unions . NE.map freeVars

-- | since return type is specific to the assertion language
-- cannot use typeclass
parseTermLIA :: Term -> Maybe (Either (LIA Int) (LIA Bool))
parseTermLIA t = case t of
  TermSpecConstant (SCNumeral n) ->  case TR.decimal n of
                                       Left _           -> Nothing
                                       Right (n', rest) -> Just . Left . LIAInt $ n'
  TermQualIdentifier (Unqualified (IdSymbol b)) -> case b of
                                                     "true"  -> Just . Right . LIABool $ True
                                                     "false" -> Just . Right . LIABool $ False

  TermApplication f ts -> case f of
    Unqualified (IdSymbol s) -> parseNode s ts Nothing
    Qualified (IdSymbol s) (SortSymbol (IdSymbol srt)) -> parseNode s ts (Just srt)
  where
    err s = error $ "unexpected usage for LIA predicate: " <> T.unpack s
    parseNode s ts msrt
      | s `elem` ["+", "-", "*"] = if length ts == 2 && msrt /= Just "Bool"
                                     then parseArithm s (NE.head ts) (NE.last ts)
                                     else err s
      | s `elem` ["<", "<=", "=", ">=", ">"] = if length ts == 2 && msrt /= Just "Int"
                                                 then parseAssert s (NE.head ts) (NE.last ts)
                                                 else err s
      | s == "not" = if length ts == 1 && msrt /= Just "Int"
                        then parseNot (NE.head ts)
                        else err s
      | s `elem` ["and", "or"] = if msrt /= Just "Int"
                                    then parseSeq s ts
                                    else err s
      | otherwise = Nothing
    parseArithm s t1 t2 = do
      v1 <- parseInt t1
      v2 <- parseInt t2
      Just . Left $ case s of
                      "+" -> LIAAdd v1 v2
                      "-" -> LIASub v1 v2
                      "*" -> LIAMul v1 v2
    parseAssert s t1 t2 = do
      v1 <- parseInt t1
      v2 <- parseInt t2
      Just . Right $ case s of
                       "<"  -> LIALt v1 v2
                       "<=" -> LIALe v1 v2
                       "="  -> LIAEq v1 v2
                       ">=" -> LIAGe v1 v2
                       ">"  -> LIAGt v1 v2
    parseNot t = do
      v <- parseBool t
      Just . Right . LIANot $ v
    parseSeq s ts = do
      vs <- forM ts $ \t -> parseBool t
      Just . Right $ case s of
                       "and" -> LIAAnd vs
                       "or"  -> LIAOr vs
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

maxPredsPerBody = 150 :: Int
maxPredsPerHeads = 20 :: Int
maxParamsPerPred = 20 :: Int

maxPredsPerCHC = maxPredsPerBody + maxPredsPerHeads
maxVarsPerCHC = maxPredsPerCHC * maxParamsPerPred

type NameMap = Arr T.Text

-- | parse a script to a graph, only accept the following commands:
--
-- * @(set-info _)@   -> ignore
-- * @(set-logic _)@  -> ignore, since we expect HORN with disjunction on the head
-- * @(declare-fun _symbol (_sort*) _sort)@
-- * @(assert (forall/exists (_sortedVar+) (=> _term _term))@
--      where @_sortedVar@ is @(_symbol _sort)@
-- * @(check-sat)@    -> ignore
-- * @(get-model)@    -> ignore
-- * @(exit)@         -> ignore
--
-- all other commands are not expected, but when present, they are ignored as well
--
-- Reference: https://github.com/sosy-lab/sv-benchmarks/blob/master/clauses/README.txt
-- The following directories contain benchmarks in SMT-LIB2.
-- The benchmarks are annotated with (set-logic HORN) to indicate
-- that the formulas belong to a quantified Horn fragment.
-- The asserted formulas are of the form:
--
-- horn ::=
--   |   (forall (quantified-variables) body)
--   |   (not (exists (quantified-variables) co-body))
--
-- body ::=
--   |   (=> co-body body)
--   |   (or literal*)
--   |   literal
--
-- co-body ::=
--   |   (and literal*)
--   |   literal
--
-- literal ::=
--   |   formula over interpreted relations (such as =, <=, >, ...)
--   |   (negated) uninterpreted predicate with arguments
--
-- A body has at most one uninterpreted relation with positive polarity,
-- and a co-body uses only uninterpreted relations with positive polarity.
--
-- Note: however, this hoice solver accepts more than one uninterpreted
-- relations with positive polarity in body.
parsePi :: Script -> Pi
parsePi cmds = undefined
  where
    n = length cmds
    maxVars = n * maxVarsPerCHC
    maxPreds = n * maxPredsPerCHC


