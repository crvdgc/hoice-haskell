module Main where

import           System.Environment

import           Hoice


main :: IO ()
main = getArgs >>= mapM_ reportFile
  where
    reportFile f = do
      putStrLn f
      hoice f
      putStrLn ""
