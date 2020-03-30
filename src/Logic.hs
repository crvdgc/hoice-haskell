{-# LANGUAGE OverloadedStrings #-}
module Logic where

import           Data.Array
import qualified Data.Text  as T

-- | A node for a parameterized bipartie multigraph.
--
-- Two parts of the graph have different type of nodes, @a@ and @b@.
data Node a b = Node { node  :: a
                     , edges :: [b]
                     }

data Entry c = Entry { name    :: T.Text
                     , content :: c
                     }

type VarIx  = Int
type VarVal = Maybe Int

type State  = Array VarIx VarVal

type FunVal = State -> Maybe Bool

type Var = Entry VarVal
type Fun = Entry FunVal

type FunNode = Node Fun


