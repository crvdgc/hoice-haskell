{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Teacher where

import           Control.Exception      (catch)
import           Control.Monad
import           Data.Functor           (($>))
import qualified Data.IntMap            as M
import           Data.List              (foldl')
import qualified Data.List.NonEmpty     as NE
import qualified Data.Traversable       as Tr

import           Language.Assertion.LIA
import           Language.SMT2.Syntax
import           Z3.Monad

import           CHC
import           Data.CounterExample

-- | use exception to check if the given declarations and assertions are well-sorted
checkSort :: Z3 a -> IO ()
checkSort z3 = catch (evalZ3 z3 $> ()) handler
  where
    handler :: Z3Error -> IO ()
    handler = putStrLn . ("Z3 error when checking sort: " <>) . show

-- | LIA formula with Z3 variable to a Z3 formula
mkLIA :: MonadZ3 z3 => LIA res AST -> z3 AST
mkLIA = \case
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

mkClause :: (MonadZ3 z3) => Clause VarIx (LIA Bool VarIx) -> z3 (AST, VarMap AST)
mkClause cls = do
      let (indexed, ixs) = indexClauseVars cls
      -- new constants
      vars <- mapM (newConst . ("$" <>) . show) ixs
      -- replace vars with new consts, change to implications
      let (bodyLIA, headsLIA) = clauseToImpl . intoClauseVar (vars M.!) $ indexed
      -- implications to ast
      cls <- join $ liftM2 mkImplies (mkLIA bodyLIA) (mkLIA headsLIA)
      -- return clause ast and varmap
      pure (cls, vars)


-- | indexed CHC formula to Z3 constant formula, and the variable map
chcToZ3 :: (MonadZ3 z3) => CHC VarIx (LIA Bool VarIx) -> z3 [(AST, VarMap AST)]
chcToZ3 (CHC clss) = mapM mkClause clss

-- | check a CHC, if falsifiable, return variable values
falsify :: (MonadZ3 z3) => z3 (AST, VarMap AST) -> z3 (Result, Maybe (VarMap VarVal))
falsify liaVarmap = do
  (lia, varmap) <- liaVarmap
  assert =<< mkNot lia
  withModel $ \m ->
    fmap (M.mapMaybe $ fmap fromIntegral) . sequence . M.map (evalInt m) $ varmap

-- | transform a double indexed CHC and variable values to a training dataset
buildDataset :: CHC VarIx FuncIx -> FuncMap (LIA Bool VarIx) -> VarMap VarVal -> Dataset
buildDataset (CHC clss) funcMap varMap = foldl' addClause emptyDataset clss
  where
    addClause dataset@Dataset{..} Clause{..}
      | basically True body = dataset { pos = pos ++ [toFuncDataList heads] }
      | basically False heads = dataset { neg = neg ++ [toFuncDataList body] }
      | otherwise = dataset { imp = imp ++ [(toFuncDataList body, toFuncDataList heads)]}
    basically _ [] = True
    basically b [funcApp] = case funcMap M.! func funcApp of
                              LIABool b' -> b == b'
                              _          -> False
    basically _ _ = False
    toFuncDataList = map $ liftM2 (,) func (map (varMap M.!) . args)

buildDatasetClause :: Clause VarIx FuncIx -> VarMap VarVal -> Dataset
buildDatasetClause cls@Clause{..} varMap
      | null body = emptyDataset { pos = [toFuncDataList heads] }
      | null heads = emptyDataset { neg = [toFuncDataList body] }
      | otherwise = emptyDataset { imp = [(toFuncDataList body, toFuncDataList heads)]}
  where
    toFuncDataList = map $ liftM2 (,) func (map (varMap M.!) . args)
