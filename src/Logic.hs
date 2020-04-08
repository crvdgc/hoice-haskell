module Logic where

import           Control.Exception    (catch)
import           Control.Monad        (foldM)
import           Data.Functor         (($>))
import           Data.IntMap.Strict   (IntMap, empty)
import qualified Data.Text            as T
import           Language.SMT2.Parser (parseFileMsg, script)
import           Language.SMT2.Syntax
import           Z3.Monad


type VarIx    = Int
type FunIx    = Int

type VarMap a = IntMap a
type FunMap a = IntMap a

type VarVal   = Int
type Var      = Maybe VarVal

type StateVal = VarMap VarVal
type State    = VarMap Var

type FunVal   = StateVal -> Bool        -- must be total
type Fun      = State    -> Maybe Bool

data NameMap  = NameMap { varsName :: VarMap T.Text
                        , funsName :: FunMap T.Text
                        }

-- | A node for an indexed bipartie multigraph.
data Node a b = Node { node  :: a
                     , edges :: [b]
                     }

type FunNode  = Node Fun VarIx -- has which vars as parameters
type VarNode  = Node Var FunIx -- is a parameter to which funs

data Graph    = Graph { vars :: VarMap VarNode
                      , funs :: FunMap FunNode
                      }

data Response = CounterEx StateVal
              | Satisfied Model

-- | parse a script to a graph, only accept the following commands:
--
-- * @(set-info _)@   -> ignore
-- * @(set-logic _)@  -> ignore, since we expect HORN with disjunction on the head
-- * @(declare-fun _symbol (_sort*) _sort)@
-- * @(assert (forall/exists (_sortedVar+) (=> _term _term))@
--      where @_sortedVar@ is @(_symbol _sort)@
-- * @(check-sat)@    -> ignore
-- * @(get-model)@    -> ignore
-- * @(exit)@         -> ignore
--
-- all other commands are not expected, but when present, they are ignored as well
--
-- Reference: https://github.com/sosy-lab/sv-benchmarks/blob/master/clauses/README.txt
-- The following directories contain benchmarks in SMT-LIB2.
-- The benchmarks are annotated with (set-logic HORN) to indicate
-- that the formulas belong to a quantified Horn fragment.
-- The asserted formulas are of the form:
--
-- horn ::=
--   |   (forall (quantified-variables) body)
--   |   (not (exists (quantified-variables) co-body))
--
-- body ::=
--   |   (=> co-body body)
--   |   (or literal*)
--   |   literal
--
-- co-body ::=
--   |   (and literal*)
--   |   literal
--
-- literal ::=
--   |   formula over interpreted relations (such as =, <=, >, ...)
--   |   (negated) uninterpreted predicate with arguments
--
-- A body has at most one uninterpreted relation with positive polarity,
-- and a co-body uses only uninterpreted relations with positive polarity.
--
-- Note: however, this hoice solver accepts more than one uninterpreted
-- relations with positive polarity in body.
parseScript :: T.Text -> Either T.Text Script
parseScript t = filter keep <$> parseFileMsg script t
  where
    keep cmd = case cmd of
                 DeclareFun {} -> True
                 Assert     {} -> True
                 _             -> False


processCmd :: Script -> Either T.Text (NameMap, Graph)
processCmd = foldM accumCmd initial
      where
        initial = ( NameMap { varsName = empty, funsName = empty }
                  , Graph   { vars     = empty, funs     = empty }
                  )
        accumCmd (names, graph) cmd = case cmd of
                                        DeclareFun sym srts srt -> undefined
                                        Assert t                -> undefined

scriptToZ3 :: Script -> Z3 Response
scriptToZ3 = undefined

-- | use exception to check if the given declarations and assertions are well-sorted
checkSort :: Z3 a -> IO ()
checkSort z3 = catch (evalZ3 z3 $> ()) handler
  where
    handler :: Z3Error -> IO ()
    handler = putStrLn . ("Z3 error when checking sort: " <>) . show

