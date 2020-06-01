{-# LANGUAGE GADTs #-}
module Solver where

-- import           Control.Exception    (catch)
-- import           Control.Monad        (foldM)
-- import           Control.Monad.ST
-- import           Data.Functor         (($>))
import qualified Data.IntMap            as M
-- import           Data.STRef
import qualified Data.Text              as T
import           Language.Assertion.LIA
-- import qualified Data.Traversable     as Tr
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



--
-- liaToZ3 :: MonadZ3 z3 => LIA a -> z3 AST
-- liaToZ3 f = case f of
--   LIAVar v     -> mkFreshIntVar $ "x" <> show v
--   LIAInt n     -> mkInteger (fromIntegral n)
--   LIABool b    -> mkBool b
--   LIAAdd t1 t2 -> mkAdd =<< z3seq [t1, t2]
--   LIASub t1 t2 -> mkSub =<< z3seq [t1, t2]
--   LIAMul t1 t2 -> mkMul =<< z3seq [t1, t2]
--   where
--     z3seq ts = Tr.sequence . fmap liaToZ3 $ ts
--
--
--
-- candidateToZ3 :: MonadZ3 z3 => LIA Bool -> z3 Bool
-- candidateToZ3 c = do
--   vs <- mapM newIntVar fvs
--   undefined
--   where
--     fvs = varSetToList . freeVars $ c
--     newIntVar name = mkFreshIntVar $ "X" ++ show name
