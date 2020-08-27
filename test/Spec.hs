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
import           Learner.DecisionTree
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

testLIA' = LIAAssert Le (LIAVar "x") (LIAInt 2)

-- | x + 1 <= 3 => x <= 2
testClause1 = LIASeqLogic Or (NE.fromList [LIANot testLIA,  testLIA'])

-- | true => y < 0
testClause2 = LIAAssert Lt (LIAVar "y") (LIAInt 0)

testCHC = LIANot $ LIASeqLogic And (NE.fromList [testClause1, testClause2])

run :: [Integer] -> IO ()
run examples = evalZ3 (z3CELoop examples testCHC) >>= \case
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

cls1 = Clause { vars = S.fromList ["x"]
              , body = [FuncApp { func = "p", args = ["x"] }]
              , phi = LIAAssert Gt (LIAVar "x") (LIAInt 1)
              , heads = [FuncApp { func = "p", args = ["x"]}]
              }

chc1 = CHC [cls1]

chcLoop :: CHC T.Text T.Text -> IO (Either T.Text (CHC VarIx (LIA Bool VarIx)))
chcLoop chc = let (chc', funcName) = indexCHCFunc chc
                  (chc'', varName) = indexCHCVars chc'
                  initSynth = fmapCHCVar (const $ LIABool True) chc''
               in ceLoop chc'' initSynth funcName
  where
    ceLoop chc synthesized funcName = do
      let z3Script = falsify . chcToZ3 $ synthesized
      evalZ3 z3Script >>= \case
        (Sat, Just counterExample) -> let dataset = buildDataset chc synthesized counterExample
                                          quals = []
                                          classMap = assignClass funcName dataset
                                          learnData = LearnData { classMap = classMap
                                                                , dataset = dataset
                                                                , quals = quals
                                                                }
                                          (_, synthesized') = learn chc learnData classMap
                                       in Right $ ceLoop chc synthesized' funcName
        (Unsat, _) -> Left "Satisfied"
        (Undef, _) -> Left "Unknown"


main :: IO ()
main = run []

