{-# LANGUAGE OverloadedStrings #-}
import           Control.Exception      (try)
import qualified Data.Text              as T
import           Language.Assertion.LIA
import           Language.SMT2.Parser   (parseFileMsg, term)
import           Language.SMT2.Syntax
import           Test.HUnit
import           Z3.Monad

sortTestScript :: Z3 ()
sortTestScript = do
  a <- mkFreshBoolVar "a"
  b <- mkFreshIntVar "b"
  bS <- mkBoolSort
  iS <- mkIntSort
  f <- mkFreshFuncDecl "f" [bS, iS] bS
  -- mkApp f [a, b]
  mkApp f [b, a]
  pure ()

assertFalse msg = assertBool msg False

simpleLIA :: Test
simpleLIA = TestList [ "true" @> Right (LIABool True)
                     ]
  where
    infixl 6 @>
    s @> l = TestCase $ case parseFileMsg term s of
                          Left s -> assertFalse $ "parsing term failed for " <> T.unpack s
                          Right t -> case parseTermLIA t of
                                       Nothing -> assertFalse $ "term to LIA failed for " <> show t
                                       Just l' -> assertEqual "lia" l l'


liaTest :: Test
liaTest = TestList [simpleLIA]

-- main :: IO ()
-- main = do
--   result <- try (evalZ3 sortTestScript) :: IO (Either Z3Error ())
--   let b = case result of
--             Left _  -> False
--             Right _ -> True
--   print b
--
main :: IO ()
main = do
  counts <- runTestTT liaTest
  print counts


