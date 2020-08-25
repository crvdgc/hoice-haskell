{-# LANGUAGE GADTs #-}
module Teacher where

import           Control.Exception      (catch)
import           Control.Monad
import           Data.Functor           (($>))
import qualified Data.IntMap            as M
import qualified Data.List.NonEmpty     as NE
import qualified Data.Text              as T
import qualified Data.Traversable       as Tr

import           Language.Assertion.LIA
import           Language.SMT2.Parser   (parseFileMsg, script)
import           Language.SMT2.Syntax
import           Z3.Monad

import           CHC
import           Data.CounterExample

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


-- | use exception to check if the given declarations and assertions are well-sorted
checkSort :: Z3 a -> IO ()
checkSort z3 = catch (evalZ3 z3 $> ()) handler
  where
    handler :: Z3Error -> IO ()
    handler = putStrLn . ("Z3 error when checking sort: " <>) . show

-- | LIA formula with Z3 variable to a Z3 formula
mkLIA :: MonadZ3 z3 => LIA res AST -> z3 AST
mkLIA node = case node of
               LIAVar v  -> pure v
               LIAInt n  -> mkInteger (fromIntegral n)
               LIABool b -> mkBool b
               LIAArith op t1 t2 -> let vs = Tr.sequence [mkLIA t1, mkLIA t2]
                                     in case op of
                                          Add -> mkAdd =<< vs
                                          Sub -> mkSub =<< vs
                                          Mul -> mkMul =<< vs
               LIAAssert op t1 t2 -> let v1 = mkLIA t1
                                         v2 = mkLIA t2
                                      in join $ case op of
                                                  Lt  -> liftM2 mkLt v1 v2
                                                  Le  -> liftM2 mkLe v1 v2
                                                  Eql -> liftM2 mkEq v2 v2
                                                  Ge  -> liftM2 mkGe v1 v2
                                                  Gt  -> liftM2 mkGt v1 v2
               LIANot t -> let v = mkLIA t
                            in mkNot =<< v
               LIASeqLogic op ts -> let vs = Tr.sequence . map mkLIA . NE.toList $ ts
                                     in case op of
                                          And -> mkAnd =<< vs
                                          Or  -> mkOr =<< vs

-- | new int constant variable
newConst :: (MonadZ3 z3) => String -> z3 AST
newConst s = join $ mkConst <$> mkStringSymbol s <*> mkIntSort

-- | LIA formula to Z3 constant formula, and the variable map
liaToZ3Const :: (Ord var, Show var, MonadZ3 z3) => LIA res var -> z3 (AST, M.IntMap AST)
liaToZ3Const lia = do
  vars <- mapM (newConst . ("$" <>) . show) ixs
  let deindexed = fmap (vars M.!) indexed
  liftM2 (,) (mkLIA deindexed) (pure vars)
  where
    (indexed, ixs) = indexVarLIA lia

-- | indexed CHC formula to Z3 constant formula, and the variable map
chcToZ3 :: (MonadZ3 z3) => CHC Int (LIA Bool Int) -> z3 (AST, M.IntMap AST)
chcToZ3 chc = let impls = chcToImpls chc
               in if null impls
                    then liftM2 (,) mkTrue (pure M.empty)
                    else mkCHC impls
  where
    (CHC indexed, ixs) = indexCHCVars chc
    mkCHC neImpls = do
      vars <- mapM (newConst . ("$" <>) . show) ixs
      let deindexed = map (clauseToImpl . intoClauseVar (vars M.!)) indexed
      clss <- mapM (\(a, b) -> join $ liftM2 mkImplies (mkLIA a) (mkLIA b)) deindexed
      liftM2 (,) (mkAnd clss) (pure vars)

-- | check a CHC and return a counter example
--falsify :: (MonadZ3 z3) => z3
