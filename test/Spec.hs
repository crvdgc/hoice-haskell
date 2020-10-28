{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
import           Test.Tasty
import           Test.Tasty.HUnit

import           Debug.Trace            (trace, traceShow, traceShowId)

import           Control.Monad
import qualified Data.IntMap            as M
import           Data.Maybe             (catMaybes, isNothing)
import qualified Data.Text              as T

import           Z3.Monad

import           Language.SMT2.Parser   (parseFileMsg, term)
import           Language.SMT2.Syntax   hiding (Sat, Unsat)

import           CHC
import           Data.CounterExample
import           Language.Assertion.LIA
import           Learner.DecisionTree
import           Parser
import           Teacher

tests :: TestTree
tests = testGroup "Tests" [unitTests]

smtFiles :: [String]
smtFiles = [ "test/files/simple-disjunction-no-synth.smt2"
           , "test/files/sum.smt2"
           , "test/files/simple-synth.smt2"
           , "test/files/simple-fib.smt2"
           ]
unitTests :: TestTree
unitTests = testGroup "SMT2 tests" [ testCase "Simple disjunction no synth" $ testHoice "test/files/simple-disjunction-no-synth.smt2"
                                   , testCase "Sum" $ testHoice "test/files/sum.smt2"
                                   , testCase "Simple synth" $ testHoice "test/files/simple-synth.smt2"
                                   ]

synthesize :: T.Text -> IO ()
synthesize srpt = case parseScript srpt of
                    Left msg  -> print $ "Parse error: " <> msg
                    Right chc -> trace ("chc: " <> show chc) synthesizeCHC chc

synthesizeCHC :: CHC T.Text T.Text -> IO ()
synthesizeCHC chc = let (chc', funcNames) = indexCHCFunc chc
                        clsVars = indexCHCVars chc'
                        chc'' = CHC $ map fst clsVars
                     in do
                       res <- ceSynthCHC chc'' funcNames
                       case res of
                         Nothing      -> print $ "Synthesize error"
                         Just funcMap -> print $ "Satisfied, model:\n" <> show funcMap

type CEResult = Maybe (FuncMap (LIA Bool VarIx))

ceSynthCHC :: CHC VarIx FuncIx -> FuncMap a -> IO CEResult
ceSynthCHC chc funcMap = let initialSynth = M.map (const $ LIABool True) funcMap
                          in atTeacher 10 chc initialSynth emptyDataset

ceExtractDatasetCHC :: FuncMap (LIA Bool VarIx) -> CHC VarIx FuncIx -> IO (Maybe Dataset)
ceExtractDatasetCHC funcMap (CHC clss) = do
  datasets <- mapM (ceExtractDatasetClause funcMap) clss
  if all isNothing datasets
     then pure Nothing -- all not falsifiable
     else pure . Just . traceShowId . mconcat . catMaybes $ datasets

ceExtractDatasetClause :: FuncMap (LIA Bool VarIx) -> Clause VarIx FuncIx -> IO (Maybe Dataset)
ceExtractDatasetClause funcMap cls = let synthesized = fmap (funcMap M.!) cls in do
  (res, maybeVarMap) <- evalZ3 . falsify . mkClause $ synthesized
  case res of
    Unsat -> pure Nothing -- not falsifiable
    Undef -> error "Solver error"
    Sat -> case maybeVarMap of -- satisfiable, extract counter examples from model
             Nothing     -> error "No counter examples"
             Just varMap -> pure . Just $ buildDatasetClause cls varMap


atTeacher :: Int -> CHC VarIx FuncIx -> FuncMap (LIA Bool VarIx) -> Dataset -> IO CEResult
atTeacher n chc funcMap knownDataset = if n == 0 then pure Nothing else let synthesized = fmap (funcMap M.!) chc in do
  maybeDataset <- ceExtractDatasetCHC funcMap chc
  case maybeDataset of
    Nothing -> pure $ Just funcMap
    Just dataset -> let arityMap = chcArityMap chc funcMap
                        initialQuals = initializeQuals funcMap chc
                        learnClass = trace ("initial quals: " <> show initialQuals) $ assignClass funcMap $ annotateDegree dataset
                        allDataset = dataset <> knownDataset
                        learnData = LearnData learnClass allDataset initialQuals
                        (_, funcMap') = learn chc arityMap learnData learnClass
                     in atTeacher (n-1) chc funcMap' allDataset

testHoice :: String -> IO ()
testHoice file = do
  s <- readFile file
  synthesize . T.pack $ s

main :: IO ()
main = mapM_ reportFile smtFiles
  where
    reportFile f = do
      putStrLn f
      testHoice f
      putStrLn ""
