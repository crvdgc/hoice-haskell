{-# LANGUAGE RecordWildCards #-}
module Main where

import           Data.Semigroup      ((<>))
import           Hoice
import           Options.Applicative
import           System.IO.Unsafe    (unsafePerformIO)

data Arg = Arg
  { inputFile    :: String,
    log          :: Bool,
    verboseLevel :: Int
  }
  deriving (Show)

argParser :: Parser Arg
argParser =
  Arg
    <$> argument
      str
      ( metavar "INPUT"
          <> help "input CHC in smt2 format"
      )
    <*> switch
      ( long "log"
          <> short 'l'
          <> help "whether to log intermediate data"
      )
    <*> option
      auto
      ( long "log verbose level"
          <> help "verbose level of logging, 0 will show all log. only valid with `-l`"
          <> showDefault
          <> value 0
      )

main :: IO ()
main = runHoice =<< execParser opts
  where
    opts =
      info
        (argParser <**> helper)
        ( fullDesc
            <> progDesc "A solver based on HoIce, but for CHC with disjunction on heads"
            <> header "hoice-ex - a solver for extended CHC"
        )

runHoice :: Arg -> IO ()
runHoice Arg{..} = hoice inputFile

