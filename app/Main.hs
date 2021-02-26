module Main where

import           System.Environment

import           Hoice

import           Options.Applicative

data Arg = Arg
  { inputFile    :: String
  , log          :: Bool
  , verboseLevel :: Int
  }

argParser :: Parser Arg
argParser = Arg
             <$> strOption
                 ( long "input file"
                <> metavar "STRING"
                <> help "Input smt2 file" )
             <*> switch
                 ( long "log"
                <> short 'l'
                <> help "Whether to log intermediate data" )
             <*> option auto
                 ( long "log verbose level"
                <> help "What is the verbose level when logging"
                <> showDefault
                <> value 0
                <> metavar "INT" )

main :: IO ()
main = getArgs >>= mapM_ reportFile
  where
    reportFile f = do
      putStrLn f
      hoice f
      putStrLn ""
