module Logic where

import           Control.Exception    (catch)
import           Control.Monad        (foldM)
import           Data.Functor         (($>))
import qualified Data.Text            as T
import           Language.SMT2.Parser (parseFileMsg, script)
import           Language.SMT2.Syntax
import           Z3.Monad

parseScript :: T.Text -> Either T.Text Script
parseScript t = filter keep <$> parseFileMsg script t
  where
    keep cmd = case cmd of
                 DeclareFun {} -> True
                 Assert     {} -> True
                 _             -> False


data Response  = CounterEx [Int]
               | Satisfied Model

-- | use exception to check if the given declarations and assertions are well-sorted
checkSort :: Z3 a -> IO ()
checkSort z3 = catch (evalZ3 z3 $> ()) handler
  where
    handler :: Z3Error -> IO ()
    handler = putStrLn . ("Z3 error when checking sort: " <>) . show

