{-# LANGUAGE GADTs #-}
module Solver where

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
--
parseScript :: T.Text -> Either T.Text Script
parseScript t = filter keep <$> parseFileMsg script t
  where
    keep cmd = case cmd of
                 DeclareFun {} -> True
                 Assert     {} -> True
                 _             -> False
--
--
-- data Response  = CounterEx [Int]
--                | Satisfied Model
--
-- | use exception to check if the given declarations and assertions are well-sorted
checkSort :: Z3 a -> IO ()
checkSort z3 = catch (evalZ3 z3 $> ()) handler
  where
    handler :: Z3Error -> IO ()
    handler = putStrLn . ("Z3 error when checking sort: " <>) . show


liaToZ3 :: (Ord var, Show var, MonadZ3 z3) => LIA res var -> z3 AST
liaToZ3 lia = do
  vars <- mapM (\ix -> mkFreshIntVar $ "$" <> show ix) ixs
  let deindexed = fmap (vars M.!) indexed
  mkLIA deindexed
  where
    (indexed, ixs) = indexVarLIA lia
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

