{-# LANGUAGE OverloadedStrings #-}
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
import           Hoice                  (hoice, printCheckHoice)
import           Language.Assertion.LIA
import           Learner.DecisionTree
import           Parser
import           Teacher

smtFiles :: [String]
smtFiles = [ "debug.smt2"-- "test/files/debug-repeat.smt2"
           --   "test/files/simple-disjunction-no-synth.smt2"
           --, "test/files/sum.smt2"
           --, "test/files/simple-synth.smt2"
           --, "test/files/debug-fib.smt2"
           --, "test/files/100-greater-than-0.smt2"
           ]

sumTest :: IO ()
sumTest = do
  (res, maybeVarMap) <- evalZ3 script
  case res of
    Sat   -> print "Sat" >> print maybeVarMap
    Unsat -> print "Unsat"
    Undef -> print "Undef"
  where
    script = do
      intSort <- mkIntSort
      k1 <- newConst "k1"
      a1 <- newConst "a1"
      _0 <- mkInt 0 =<< mkIntSort
      let kCl = mkNot =<< mkEq _0 k1
      -- body <- mkAnd =<< sequence [join $ liftM2 mkEq kCl (mkLe a1 _0), kCl]
      body <- mkAnd =<< sequence [join $ liftM2 mkIff kCl (mkLe a1 _0), kCl]
      -- body <- mkAnd =<< sequence [mkNot =<< join (liftM2 mkXor kCl (mkLe a1 _0)), kCl]
      -- body <- mkAnd =<< sequence [mkBoolEql kCl (mkLe a1 _0), kCl]
      -- head <- mkOr =<< sequence [mkFalse]
      head <- mkFalse
      -- assert =<< mkImplies body head
      assert =<< mkNot =<< mkImplies body head
      withModel $ \m ->
        mapEval evalInt m [k1, a1]
    newConst name = join $ mkConst <$> mkStringSymbol name <*> mkIntSort
    mkBoolEql :: (MonadZ3 z3) => z3 AST -> z3 AST -> z3 AST
    mkBoolEql mx my = mkOr =<< sequence [ mkAnd =<< sequence [mx, my]
                                        , mkAnd =<< sequence [ mkNot =<< mx
                                                             , mkNot =<< my
                                                             ]
                                        ]

main :: IO ()
main = hoiceMain
  where
    hoiceMain = mapM_ reportFile smtFiles
      where
        reportFile f = do
          putStrLn f
          hoice f
          putStrLn ""
