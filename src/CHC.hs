{-# LANGUAGE RecordWildCards #-}
module CHC where

import           Control.Monad
import qualified Data.IntMap            as M
import qualified Data.Set               as S
import qualified Data.Text              as T
import qualified Language.Assertion.LIA as L
import           Language.SMT2.Syntax

data FuncApp v f = FuncApp { func :: f        -- ^ function
                           , args :: S.Set v  -- ^ arguments
                           }
  deriving (Eq, Show)

instance Functor (FuncApp v) where
  fmap f funcApp@FuncApp{..} = funcApp { func = f func }

data Clause v f = Clause { vars  :: S.Set v        -- ^ @forall@ qualified variables
                         , body  :: [FuncApp v f]  -- ^ uninterpreted preds
                         , phi   :: L.LIA Bool v   -- ^ constraints
                         , heads :: [FuncApp v f]  -- ^ uninterpreted preds
                         }
  deriving (Eq, Show)

instance Functor (Clause v) where
  fmap f cls@Clause{..} = cls { body = (fmap . fmap) f body, heads = (fmap . fmap) f heads }

newtype CHC v f = CHC [Clause v f]

dispatchPreds :: ([FuncApp v f] -> a) -> ([FuncApp v f] -> b) -> (a -> b -> c) -> Clause v f -> c
dispatchPreds fHeads fBody g cls = g (fHeads $ heads cls) (fBody $ body cls)

bothPreds :: ([FuncApp v f] -> a) -> (a -> a -> b) -> Clause v f -> b
bothPreds = join dispatchPreds

setToMap :: S.Set a -> M.IntMap a
setToMap = M.fromAscList . zip [0..] . S.toAscList

funcs :: (Ord f) => Clause v f -> S.Set f
funcs = bothPreds getFuncs S.union
  where
    getFuncs = S.fromList . fmap func

allVars :: (Ord v) => Clause v f -> S.Set v
allVars cls = S.unions . fmap (\f -> f cls) $ [vars, bothPreds getVars S.union, L.freeVarsLIA . phi]
  where
    getVars = S.unions . fmap args

indexCHCFunc :: (Ord f) => CHC v f -> (CHC v Int, M.IntMap f)
indexCHCFunc (CHC clss) = (CHC indexed, ixs)
  where
    allFuncs = S.unions . fmap funcs $ clss
    indexed = (fmap . fmap) (`S.findIndex` allFuncs) clss
    ixs = setToMap allFuncs


indexClauseVars :: (Ord v) => Clause v f -> (Clause Int f, M.IntMap v)
indexClauseVars cls@Clause{..} = (indexed, ixs)
  where
    avs = allVars cls
    ixs = setToMap avs
    find = flip S.findIndex avs
    findVars funcApp@FuncApp{..} = funcApp { args = S.map find args }
    indexed = Clause { vars  = S.map find vars
                     , body  = findVars <$> body
                     , phi   = find <$> phi
                     , heads = findVars <$> heads
                     }

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

