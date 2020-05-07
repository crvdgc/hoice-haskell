module Logic where

import           Control.Exception    (catch)
import           Control.Monad        (foldM)
import           Data.Functor         (($>))
import           Language.SMT2.Syntax
import           Z3.Monad


{-
processCmd :: AssertionLanguage a => Script -> Either T.Text (NameMap, Graph, Pi a)
processCmd = foldM accumCmd initial
      where
        initial = ( NameMap { varsName = empty, funsName = empty }
                  , Graph   { vars     = empty, funs     = empty }
                  , [] :: Pi a
                  )
        accumCmd acc cmd = case cmd of
                             Assert (TermForall vs (TermApplication (Unqualified (IdSymbol "=>") ts))) -> register acc vs ts
                             _  -> acc
        register acc@(names, graph, chcs) vs ts = undefined

data Response  = CounterEx StateVal
               | Satisfied Model

scriptToZ3 :: Script -> Z3 Response
scriptToZ3 = undefined

-- | use exception to check if the given declarations and assertions are well-sorted
checkSort :: Z3 a -> IO ()
checkSort z3 = catch (evalZ3 z3 $> ()) handler
  where
    handler :: Z3Error -> IO ()
    handler = putStrLn . ("Z3 error when checking sort: " <>) . show

-}
