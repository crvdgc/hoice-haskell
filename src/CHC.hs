{-# LANGUAGE GADTs #-}
module CHC where

import           Data.IntMap.Strict   (IntMap, empty)
import qualified Data.Text            as T
import qualified Data.Text.Read       as TR
import           Language.SMT2.Parser (parseFileMsg, script)
import           Language.SMT2.Syntax
--
-- | index, or "name" of variables and preds
type Var  = Int
type Pred = Int -- uninterpreted preds

type VarVal  = Int
type PredVal = [VarVal] -> Bool -- sort must match

-- | indexed by name of vars and preds
type VarMap a  = IntMap a
type PredMap a = IntMap a

type VarValMap  = VarMap VarVal
type PredValMap = PredMap PredVal

-- | a CHC system, parameterized by an assertion language @a@
type Pi al = [CHC al]

-- | an application of an uninterpreted pred
data PredApp = PredApp { pred   :: Pred
                       , params :: [Var]
                       }

-- | a single CHC clause
-- forall vars. head <- body /\ phi
data CHC al = CHC { vars :: [Var]
                  , head :: [PredApp] -- disjunction of preds
                  , body :: [PredApp] -- conjunction of preds
                  , phi  :: al Bool   -- formula of assertion language @a@
                  }

data NameMap = NameMap { varName  :: VarMap  T.Text
                       , predName :: PredMap T.Text
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
  LIANeg  :: LIA Bool -> LIA Bool
  LIAAnd  :: LIA Bool -> LIA Bool -> LIA Bool
  LIAOr   :: LIA Bool -> LIA Bool -> LIA Bool


class AssertionLanguage al where
  -- | a term in the assertion language
  -- only have variables and interpreted preds
  parseTerm :: Term -> Maybe (al res)

instance AssertionLanguage LIA where
  parseTerm t = case t of
    TermSpecConstant (SCNumeral n) -> Just $ LIAInt (TR.decimal n)

{-
instance AssertionLanguage (LIA a) where
  parseApp (TermApplication (Unqualified (IdSymbol s)) terms) = if length terms /= 2
                                                                 then Nothing
                                                                 else case s of
                                                                        "+"  -> Just (LIAInt Add)
                                                                        "-"  -> Just (LIAInt Sub)
                                                                        "*"  -> Just (LIAInt Mul)
                                                                        "<"  -> Just (LIABool Lt)
                                                                        "<=" -> Just (LIABool Le)
                                                                        "="  -> Just (LIABool Equ)
                                                                        ">=" -> Just (LIABool Ge)
                                                                        ">"  -> Just (LIABool Gt)
                                                                        _    -> Nothing
  parseApp (TermApplication (Qualified (IdSymbol s) srt) terms) = if length terms /= 2
                                                                     then Nothing
                                                                     else case srt of
                                                                            SortSymbol (IdSymbol "Int")  -> case s of
                                                                                                             "+" -> Just (LIAInt Add)
                                                                                                             "-" -> Just (LIAInt Sub)
                                                                                                             "*" -> Just (LIAInt Mul)
                                                                                                             _   -> Nothing
                                                                            SortSymbol (IdSymbol "Bool") -> case s of
                                                                                                              "<"  -> Just (LIABool Lt)
                                                                                                              "<=" -> Just (LIABool Le)
                                                                                                              "="  -> Just (LIABool Equ)
                                                                                                              ">=" -> Just (LIABool Ge)
                                                                                                              ">"  -> Just (LIABool Gt)
                                                                                                              _    -> Nothing
                                                                            _ -> Nothing
  parseApp _ = Nothing

-}

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
parseScript :: T.Text -> Either T.Text Script
parseScript t = filter keep <$> parseFileMsg script t
  where
    keep cmd = case cmd of
                 DeclareFun {} -> True
                 Assert     {} -> True
                 _             -> False
