import           Control.Exception (try)
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

main :: IO ()
main = do
  result <- try (evalZ3 sortTestScript) :: IO (Either Z3Error ())
  let b = case result of
            Left _  -> False
            Right _ -> True
  print b

