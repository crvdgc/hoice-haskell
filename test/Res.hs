module Res where

import qualified Data.Set               as S
import           Z3.Monad

import           Language.SMT2.Parser   (parseFileMsg, term)
import           Language.SMT2.Syntax   hiding (Sat, Unsat)

import           CHC
import           Data.CounterExample
import           Language.Assertion.LIA
import           Learner.DecisionTree
import           Parser
import           Teacher

cls1 = Clause { vars = S.empty
              , body = []
              , phi = LIABool True
              , heads = []
              }

chc1 = CHC [ cls1 ]

cls2 = Clause { vars = S.empty
              , body = [ FuncApp (LIABool True) [] ]
              , phi = LIABool True
              , heads = [ FuncApp (LIABool True) []]
              }

chc2 = CHC [ cls2 ]
