{-# LANGUAGE OverloadedStrings #-}
module Logic where

import           Data.Array
import qualified Data.Text  as T
import           Z3.Monad   (Z3)

type VarIx    = Int
type VarVal   = Int
type Var      = Maybe VarVal

type StateVal = Array VarIx VarVal
type State    = Array VarIx (Maybe VarVal)

type FunIx    = Int
type FunVal   = StateVal -> Bool        -- must be total
type Fun      = State    -> Maybe Bool


-- | collection of names
type NameMap ix = Array ix T.Text

type VarNameMap = NameMap VarIx
type FunNameMap = NameMap FunIx

-- | A node for an indexed bipartie multigraph.
data Node a b = Node { node  :: a
                     , edges :: [b]
                     }

type FunNode  = Node Fun VarIx -- has which vars as parameters
type VarNode  = Node Var FunIx -- is a parameter to which funs

data Graph    = Graph { funs :: Array FunIx FunNode
                      , vars :: Array VarIx VarNode
                      }

type Model    = Array FunIx FunVal

data Response = CounterEx StateVal
              | Satisfied T.Text
              | Z3Error   T.Text

-- | parse a script to a graph
parseGraph :: String -> Graph
parseGraph = undefined

graphToZ3 :: Model -> Z3 Response
graphToZ3 = undefined

