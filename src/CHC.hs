{-# LANGUAGE RecordWildCards #-}
module CHC where

import           Control.Monad
import           Data.Bifunctor
import qualified Data.IntMap            as M
import           Data.List              (foldl')
import qualified Data.List.NonEmpty     as NE
import qualified Data.Set               as S
import qualified Data.Text              as T

import           Language.Assertion.LIA
import           Language.SMT2.Syntax

data FuncApp v f = FuncApp { func :: f        -- ^ function
                           , args :: [v]      -- ^ arguments
                           }
  deriving (Eq, Show)

instance Bifunctor FuncApp where
  -- first :: (v1 -> v2) -> FuncApp v1 f -> FuncApp v2 f
  first f funcApp@FuncApp{..} = funcApp { args = map f args }
  -- second :: (f1 -> f2) -> FuncApp v f1 -> FuncApp v f2
  second g funcApp@FuncApp{..} = funcApp { func = g func }


data Clause v f = Clause { vars  :: S.Set v        -- ^ @forall@ qualified variables
                         , body  :: [FuncApp v f]  -- ^ uninterpreted preds
                         , phi   :: LIA Bool v   -- ^ constraints
                         , heads :: [FuncApp v f]  -- ^ uninterpreted preds
                         }
  deriving (Eq, Show)

instance Functor (Clause v) where
  fmap f cls@Clause{..} = cls { body = (fmap . second) f body, heads = (fmap . second) f heads }

fmapClauseVar :: (Ord v1, Ord v2) => (v1 -> v2) -> Clause v1 f -> Clause v2 f
fmapClauseVar g Clause{..} = Clause { vars = S.map g vars
                                    , body = first g <$> body
                                    , phi = g <$> phi
                                    , heads = first g <$> heads
                                    }

intoClauseVar :: (Ord v1, Ord v2, Functor f) => (v1 -> v2) -> Clause v1 (f v1) -> Clause v2 (f v2)
intoClauseVar g Clause{..} = Clause { vars = S.map g vars
                                    , body = bimap g (fmap g) <$> body
                                    , phi = g <$> phi
                                    , heads = bimap g (fmap g) <$> heads
                                    }

newtype CHC v f = CHC [Clause v f]

instance Functor (CHC v) where
  fmap f (CHC clss) = CHC (map (f <$>) clss)

fmapCHCVar :: (Ord v1, Ord v2) => (v1 -> v2) -> CHC v1 f -> CHC v2 f
fmapCHCVar f (CHC clss) = CHC (map (fmapClauseVar f) clss)

fmapCHCVarM :: (Ord v1, Ord v2, Monad m) => (v1 -> m v2) -> CHC v1 f -> m (CHC v2 f)
fmapCHCVarM f (CHC clss) = undefined

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
allVars cls = S.unions . fmap (\f -> f cls) $ [vars, bothPreds getVars S.union, freeVarsLIA . phi]
  where
    getVars = S.unions . fmap (S.fromList . args)

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
    findVars funcApp@FuncApp{..} = funcApp { args = find <$> args }
    indexed = Clause { vars  = S.map find vars
                     , body  = findVars <$> body
                     , phi   = find <$> phi
                     , heads = findVars <$> heads
                     }

indexCHCVars :: (Ord v) => CHC v f -> (CHC Int f, M.IntMap v)
indexCHCVars (CHC clss) = let indexedClss = indexClauseVars <$> clss
                              (collected, indices, _) = foldl' acc ([], M.empty, 0) indexedClss
                           in (CHC collected, indices)
  where
    inc x = (+ x)
    incKey x = M.fromAscList . fmap (\(k, v) -> (k + x, v)) . M.toAscList
    acc (clss, ixss, offset) (cls, ixs) = (fmapClauseVar (inc offset) cls:clss, incKey offset ixs `M.union` ixss, offset + M.size ixs)


clauseToImpl :: Clause v (LIA Bool v) -> LIAImpl v
clauseToImpl = dispatchPreds (collect And) (collect Or) (,)
  where
    collect op = seqLogicFromList op . fmap func

chcToImpls :: CHC v (LIA Bool v) -> [LIAImpl v]
chcToImpls (CHC clss) = clauseToImpl <$> clss

