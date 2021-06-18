{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module CHC where

import           Control.Monad
import           Data.Bifunctor
import qualified Data.IntMap            as M
import qualified Data.List.NonEmpty     as NE
import qualified Data.Set               as S
import qualified Data.Text              as T

import           Language.Assertion.LIA

type FuncIx = Int
type FuncMap = M.IntMap

type VarIx = Int
type VarMap = M.IntMap
type VarVal = Int

type BoundVarIx = Int

class ToSMT a where
  toSMT :: a -> T.Text

data FuncApp v f = FuncApp { func :: f        -- ^ function
                           , args :: [v]      -- ^ arguments
                           }
  deriving (Eq, Show)

instance (ToSMT v, ToSMT f) => ToSMT (FuncApp v f) where
  toSMT FuncApp{..} =
       "("
    <> "|" <> toSMT func <> "| "
    <> (T.unwords . map toSMT $ args)
    <> ")"

instance ToSMT String where
  toSMT = T.pack

instance ToSMT T.Text where
  toSMT = id

instance ToSMT VarIx where
  toSMT v = "v_" <> tShow v

tShow :: Show a => a -> T.Text
tShow = T.pack . show

wrap :: [T.Text] -> T.Text
wrap xs = "(" <> T.unwords xs <> ")"

instance (ToSMT v) => ToSMT (LIA Bool v) where
  toSMT = \case
    LIABool b          -> T.pack $ if b then "true" else "false"
    LIAAssert op t1 t2 -> wrap [tShow op, toSMT t1, toSMT t2]
    LIANot t           -> wrap ["not", toSMT t]
    LIABoolEql t1 t2   -> wrap ["=", toSMT t1, toSMT t2]
    LIASeqLogic op ts  -> wrap . (tShow op:) . map toSMT . NE.toList $ ts

instance (ToSMT v) => ToSMT (LIA Int v) where
  toSMT = \case
    LIAVar v           -> toSMT v
    LIAInt n           -> tShow n
    LIAArith op t1 t2  -> wrap [tShow op, toSMT t1, toSMT t2]

instance Bifunctor FuncApp where
  -- first :: (v1 -> v2) -> FuncApp v1 f -> FuncApp v2 f
  first f funcApp@FuncApp{..} = funcApp { args = map f args }
  -- second :: (f1 -> f2) -> FuncApp v f1 -> FuncApp v f2
  second g funcApp@FuncApp{..} = funcApp { func = g func }


data Clause v f = Clause { vars  :: S.Set v        -- ^ @forall@ qualified variables
                         , body  :: [FuncApp v f]  -- ^ uninterpreted preds
                         , phi   :: LIA Bool v     -- ^ constraints
                         , heads :: [FuncApp v f]  -- ^ uninterpreted preds
                         }
  deriving (Eq, Show)

instance (Ord v, ToSMT v, ToSMT f) => ToSMT (Clause v f) where
  toSMT cls
    | null (vars cls') =
       "(assert "
    <> "\n(=>\n(and true\n"
    <> bodySMT
    <> ")\n(or false\n"
    <> headSMT
    <> "\n)))"
    | otherwise =
       "(assert (forall ("
    <> varList
    <> ")\n(=>\n(and true\n"
    <> bodySMT
    <> ")\n(or false\n"
    <> headSMT
    <> "\n))))"
    where
      (cls', _) = indexClauseVars cls
      varList = T.intercalate " " . map (\v -> "(" <> toSMT v <> " Int)") . S.toList $ vars cls'
      bodySMT = funcAppsToSMT (body cls') <> "\n" <> toSMT (phi cls')
      headSMT = funcAppsToSMT (heads cls')

      funcAppsToSMT = T.intercalate "\n" . map toSMT

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
  deriving (Eq, Show)

instance (Ord v, ToSMT v, ToSMT f) => ToSMT (CHC v f) where
  toSMT (CHC clss) = T.intercalate "\n" . map toSMT $ clss

instance Functor (CHC v) where
  fmap f (CHC clss) = CHC (map (f <$>) clss)

fmapCHCVar :: (Ord v1, Ord v2) => (v1 -> v2) -> CHC v1 f -> CHC v2 f
fmapCHCVar f (CHC clss) = CHC (map (fmapClauseVar f) clss)

dispatchPreds :: ([FuncApp v f] -> a) -> ([FuncApp v f] -> b) -> (a -> b -> c) -> Clause v f -> c
dispatchPreds fBody fHeads g cls = g (fBody $ body cls) (fHeads $ heads cls)

dispatchPredsAndPhi :: ([FuncApp v f] -> a) -> ([FuncApp v f] -> b) -> (a -> b -> LIA Bool v -> c) -> Clause v f -> c
dispatchPredsAndPhi fBody fHeads g cls = g (fBody $ body cls) (fHeads $ heads cls) (phi cls)

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

updateClauseVars :: Ord v => Clause v f -> Clause v f
updateClauseVars clause = let vars' = allVars clause
                           in clause { vars = vars' }

indexCHCFunc :: (Ord f) => CHC v f -> (CHC v FuncIx, FuncMap f)
indexCHCFunc (CHC clss) = (CHC indexed, ixs)
  where
    allFuncs = S.unions . fmap funcs $ clss
    indexed = (fmap . fmap) (`S.findIndex` allFuncs) clss
    ixs = setToMap allFuncs

indexClauseVars :: (Ord v) => Clause v f -> (Clause VarIx f, VarMap v)
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

indexCHCVars :: (Ord v) => CHC v f -> [(Clause VarIx f, VarMap v)]
indexCHCVars (CHC clss) = indexClauseVars <$> clss

substituteVar :: Clause v (LIA Bool BoundVarIx) -> Clause v (LIA Bool v)
substituteVar Clause{..} = Clause { vars = vars
                                  , body = map subsFuncApp body
                                  , phi = phi
                                  , heads = map subsFuncApp heads
                                  }
  where
    subsFuncApp FuncApp{..} = FuncApp { func = fmap (args !!) func
                                      , args = args
                                      }

clauseToImpl :: Clause v (LIA Bool v) -> LIAImpl v
clauseToImpl = dispatchPredsAndPhi (collect And) (collect Or) toImpl
  where
    collect op = seqLogicFromList op . fmap func
    toImpl bodyLIA headsLIA phi = (flatAnd bodyLIA phi, headsLIA)

chcToImpls :: CHC v (LIA Bool v) -> [LIAImpl v]
chcToImpls (CHC clss) = clauseToImpl <$> clss

chcArityMap :: CHC v FuncIx -> FuncMap a -> FuncMap Int
chcArityMap (CHC clauses) = M.mapWithKey (funcParamNum clauses) . M.map (const ())
  where
    funcParamNum [] _ () = 0
    funcParamNum (cls:clss) rho () = case clauseFuncParamNum rho cls of
                                       Just n  -> n
                                       Nothing -> funcParamNum clss rho ()
    clauseFuncParamNum rho = bothPreds (findMatch rho) eitherJust
    findMatch _ [] = Nothing
    findMatch rho (funcApp:rest) = if func funcApp == rho
                                      then Just . length . args $ funcApp
                                      else findMatch rho rest
    eitherJust (Just a) _       = Just a
    eitherJust Nothing (Just b) = Just b
    eitherJust Nothing Nothing  = Nothing
