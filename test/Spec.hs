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
import           Hoice                  (hoice)
import           Language.Assertion.LIA
import           Learner.DecisionTree
import           Parser
import           Teacher

tests :: TestTree
tests = testGroup "Tests" [unitTests]

smtFiles :: [String]
-- smtFiles = [ "test/files/simple-disjunction-no-synth.smt2"
--            , "test/files/sum.smt2"
--            , "test/files/simple-synth.smt2"
--            , "test/files/simple-fib.smt2"
--            ]
smtFiles = ["test/files/simple-fib.smt2"]

unitTests :: TestTree
unitTests = testGroup "SMT2 tests" [ testCase "Simple disjunction no synth" $ hoice "test/files/simple-disjunction-no-synth.smt2"
                                   , testCase "Sum" $ hoice "test/files/sum.smt2"
                                   , testCase "Simple synth" $ hoice "test/files/simple-synth.smt2"
                                   ]

main :: IO ()
main = mapM_ reportFile smtFiles
  where
    reportFile f = do
      putStrLn f
      hoice f
      putStrLn ""
