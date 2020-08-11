{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Exception      (try)
import           Control.Monad
import qualified Data.IntMap            as M
import qualified Data.List.NonEmpty     as NE
import           Data.Maybe             (catMaybes, fromJust)
import qualified Data.Text              as T
import           Test.HUnit             hiding (assert)

import           Z3.Monad

import           Language.SMT2.Parser   (parseFileMsg, term)
import           Language.SMT2.Syntax

import           Data.CounterExample
import           Language.Assertion.LIA
import           Teacher

sortTestScript :: Z3 ()
sortTestScript = do
  a <- mkFreshBoolVar "a"
  b <- mkFreshIntVar "b"
  bS <- mkBoolSort
  iS <- mkIntSort
  f <- mkFreshFuncDecl "f" [bS, iS] bS
  -- mkApp f [a, b]
  mkApp f [b, a]
  pure ()

z3ConstTest :: MonadZ3 z3 => z3 (Maybe Integer)
z3ConstTest = do
  _x <- mkStringSymbol "x"
  _int <- mkIntSort
  x <- mkConst _x _int
  _0 <- mkInteger 0
  _1 <- mkInteger 1
  lia <- mkNot =<< mkEq x _1
  -- ptn <- mkPattern [lia]
  -- app_x <- toApp x
  -- assert =<< mkExistsConst [ptn] [app_x] lia
  assert lia
  fmap snd $ withModel $ \m ->
    fromJust <$> evalInt m x

z3CELoop :: (Ord var, Show var, MonadZ3 z3) => [Integer] -> LIA Bool var -> z3 (Maybe [Integer])
z3CELoop examples lia = do
  (node, varmap) <- liaToZ3Const lia
  let vars = map snd . M.toList $ varmap
  negs <- mapM mkInteger examples
  ineqls <- forM vars $ \var ->
    forM negs $ \neg ->
      mkNot =<< mkEq neg var
  synthesized <- mkAnd (node:concat ineqls)
  assert synthesized
  fmap snd $ withModel $ \m ->
    catMaybes <$> mapM (evalInt m) vars


runConst :: IO ()
runConst = evalZ3 z3ConstTest >>= \case
  Just n -> putStrLn $ "Counter example: " ++ show n
  Nothing -> putStrLn "Satisfied"

assertFalse msg = assertBool msg False

testLIA = LIAAssert Le (LIAArith Add (LIAVar "x") (LIAInt 1)) (LIAInt 3)

testCHC :: LIA Bool String -> LIA Bool String
testCHC lia = LIASeqLogic And (NE.fromList [lia, testLIA])

run :: [Integer] -> IO ()
run examples = evalZ3 (z3CELoop examples testLIA) >>= \case
    Nothing  -> putStrLn "Satisfied"
    Just sol -> let ces = sol ++ examples
                 in if length ces > 100
                       then pure ()
                       else putStr "Counter examples: " >> print ces >> putStrLn "" >> run ces

simpleLIA :: Test
simpleLIA = TestList [ "true" @> Right (LIABool True)
                     ]
  where
    infixl 6 @>
    s @> l = TestCase $ case parseFileMsg term s of
                          Left s -> assertFalse $ "parsing term failed for " <> T.unpack s
                          Right t -> case parseTermLIA t of
                                       Nothing -> assertFalse $ "term to LIA failed for " <> show t
                                       Just l' -> assertEqual "lia" l l'


liaTest :: Test
liaTest = TestList [simpleLIA]

-- main :: IO ()
-- main = do
--   result <- try (evalZ3 sortTestScript) :: IO (Either Z3Error ())
--   let b = case result of
--             Left _  -> False
--             Right _ -> True
--   print b
--
-- main :: IO ()
-- main = do
--   counts <- runTestTT liaTest
--   print counts

-- main :: IO ()
-- main = run []

main :: IO ()
main = run []

