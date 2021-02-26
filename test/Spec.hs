{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import           Z3.Monad            hiding (assert, simplify)
import qualified Z3.Monad            as ZM

import           Data.CounterExample
-- import           Hoice            (hoice)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "simplify"
  [ testCase "one pos discharge" $
    simplifyFrom posDataset ([p1], []) @?= Just (Dataset [[p1]] [] [])
  , testCase "one pos determine" $
    simplifyFrom negDataset ([p1], []) @?= Just (Dataset [[p1]] [[p2]] [])
  , testCase "one pos round" $
    simplifyKnownPair negDataset ([], []) ([p1], []) @?= Just (Dataset [] [] [], ([p1], []), ([], [p2]))
  , testCase "one pos imp split" $
    simplifyFrom impDataset ([p1], []) @?= Just (Dataset [[p1], [p2]] [] [])
  , testCase "one pos simplifies a body of imp" $
    simplify impDataset2 @?= Just (Dataset [[p1]] [] [([p2], [p3])])
  , testCase "many pos simplify a body of imp" $
    simplify impDataset3 @?= Just (Dataset [[p2], [p1]] [] [([p3], [p4])])
  , testCase "test of mochi/apply.smt2" $
    simplify applyDataset @?= Nothing
  ]
    where
      p1 = (0, [1])
      p2 = (1, [0, 0])
      p3 = (2, [1])
      p4 = (2, [4])
      posDataset = Dataset [[p1, p2]] [] []
      negDataset = Dataset [] [[p1, p2]] []
      impDataset = Dataset [] [] [([p1], [p2])]
      impDataset2 = Dataset [[p1]] [] [([p1, p2], [p3])]
      impDataset3 = Dataset [[p1], [p2]] [] [([p1, p3], [p4])]
      applyDataset =   Dataset
                              { pos =
                                    [ [ ( 6, [ 0 ] ) ] ]
                                , neg =
                                      [ [ ( 4, [ 0, 1 ] ) ] ]
                                , imp =
                                      [
                                            ( [ ( 6, [ 0 ] ), ( 3, [ 1, 0 ] ) ], [ ( 6, [ 1 ] ) ] )
                                          ,
                                            ( [ ( 2, [ 0 ] ), ( 1, [ 1, 0 ] ) ], [ ( 3, [ 1, 0 ] ) ] )
                                          ,
                                            (
                                                    [ ( 6, [ 0 ] ), ( 5, [ 1, 0, 0 ] ), ( 0, [ 0 ] ) ]
                                                  ,
                                                    [ ( 1, [ 1, 0 ] ) ] )
                                          ,
                                            ( [ ( 4, [ 0, 0 ] ) ], [ ( 5, [ 1, 0, 0 ] ) ] )
                                          ,
                                            ( [ ( 2, [ - 1 ] ) ], [ ( 0, [ - 1 ] ) ] )
                                          ,
                                            (
                                                    [ ( 6, [ 0 ] ), ( 5, [ 0, 0, 0 ] ), ( 0, [ 0 ] ) ]
                                                  ,
                                                    [ ( 1, [ 0, 0 ] ) ] )
                                          ,
                                            ( [ ( 2, [ 1 ] ) ], [ ( 0, [ 1 ] ) ] )
                                          ,
                                            ( [ ( 6, [ 0 ] ), ( 0, [ - 1 ] ) ], [ ( 4, [ - 1, 0 ] ) ] )
                                          ,
                                            ( [ ( 2, [ 0 ] ), ( 1, [ 0, 0 ] ) ], [ ( 3, [ 0, 0 ] ) ] )
                                          ,
                                            ( [ ( 6, [ 0 ] ), ( 0, [ 1 ] ) ], [ ( 4, [ 1, 0 ] ) ] )
                                          ,
                                            ( [ ( 6, [ 0 ] ), ( 3, [ 0, 0 ] ) ], [ ( 6, [ 1 ] ) ] )
                                          ,
                                            ( [ ( 6, [ 1 ] ), ( 0, [ 0 ] ) ], [ ( 4, [ 0, 1 ] ) ] )
                                          ,
                                            ( [ ( 6, [ 0 ] ) ], [ ( 2, [ 0 ] ) ] )
                                          ,
                                            ( [ ( 2, [ 0 ] ) ], [ ( 0, [ 0 ] ) ] )
                                          ,
                                            ( [ ( 6, [ 0 ] ), ( 0, [ 0 ] ) ], [ ( 4, [ 0, 0 ] ) ] ) ] }


smtFiles :: [String]
smtFiles = [ -- "debug.smt2"
            "test/files/inductive4.smt2"
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
      ZM.assert =<< mkNot =<< mkImplies body head
      withModel $ \m ->
        mapEval evalInt m [k1, a1]
    newConst name = join $ mkConst <$> mkStringSymbol name <*> mkIntSort
    mkBoolEql :: (MonadZ3 z3) => z3 AST -> z3 AST -> z3 AST
    mkBoolEql mx my = mkOr =<< sequence [ mkAnd =<< sequence [mx, my]
                                        , mkAnd =<< sequence [ mkNot =<< mx
                                                             , mkNot =<< my
                                                             ]
                                        ]

-- main :: IO ()
-- main = hoiceMain
--   where
--     hoiceMain = mapM_ reportFile smtFiles
--       where
--         reportFile f = do
--           putStrLn f
--           hoice f
--           putStrLn ""

