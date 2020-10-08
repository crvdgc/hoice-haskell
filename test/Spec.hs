{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Monad
import qualified Data.IntMap            as M
import qualified Data.Text              as T

import           Debug.Trace            (traceShow)

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

unitTests :: TestTree
unitTests = testGroup "SMT2 tests" [ testCase "Simple disjunction no synth" $ testHoice "test/files/simple-disjunction-no-synth.smt2"
                                   , testCase "Sum" $ testHoice "test/files/sum.smt2"
                                   , testCase "Simple synth" $ testHoice "test/files/simple-synth.smt2"
                                   ]

synthesize :: T.Text -> IO ()
synthesize srpt = case parseScript srpt of
                    Left msg  -> print $ "Parse error: " <> msg
                    Right chc -> traceShow chc $ synthesizeCHC chc

synthesizeCHC :: CHC T.Text T.Text -> IO ()
synthesizeCHC chc = let (chc', funcNames) = indexCHCFunc chc
                        (chc'', varNames) = indexCHCVars chc'
                        funcMapEmpty = M.map (const ()) funcNames
                     in do
                       res <- counterExampleSynth chc'' funcMapEmpty
                       case res of
                         Nothing      -> print $ "Synthesize error"
                         Just funcMap -> print $ "Satisfied"

type CEResult = Maybe (FuncMap (LIA Bool VarIx))

counterExampleSynth :: CHC VarIx FuncIx -> FuncMap () -> IO CEResult
counterExampleSynth chc funcMapEmpty = let initialSynth = M.map (const $ LIABool True) funcMapEmpty
                                        in traceShow (initialSynth :: M.IntMap (LIA Bool T.Text)) $ atTeacher chc initialSynth

atTeacher :: CHC VarIx FuncIx -> FuncMap (LIA Bool VarIx) -> IO CEResult
atTeacher chc funcMap = let synthesized = fmap (funcMap M.!) chc in do
  traceShow synthesized $ pure ()
  (res, maybeVarMap) <- evalZ3 . falsify . chcToZ3 $ synthesized
  traceShow res $ pure ()
  traceShow maybeVarMap $ pure ()
  case res of
    -- unfalsifiable, successfully synthesized predicates
    Unsat -> pure $ Just funcMap
    -- solver error
    Undef -> pure $ Nothing
    -- falsifiable, get counter examples
    Sat -> case maybeVarMap of
             Nothing -> pure $ Nothing
             Just varMap -> let learnDataset = buildDataset chc funcMap varMap
                                paramNumMap = chcParamNumMap chc funcMap
                                initialQuals = initializeQuals funcMap chc
                                learnClass = assignClass funcMap $ annotateDegree learnDataset
                                learnData = LearnData learnClass learnDataset initialQuals
                                () = traceShow learnData ()
                                (_, funcMap') = learn chc paramNumMap learnData learnClass
                             in atTeacher chc funcMap'

testHoice :: String -> IO ()
testHoice file = do
  s <- readFile file
  synthesize . T.pack $ s

main :: IO ()
main = defaultMain tests

