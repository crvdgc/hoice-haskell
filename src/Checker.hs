{-|
Module      : Checker
Description : Checking the result of HoIce with Z3, by literally replacing the result
Maintainer  : liuyuxi@kb.is.s.u-tokyo.ac.jp
Stability   : experimental
Portability : POSIX
-}

module Checker where

import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Process

z3Check :: FilePath -> IO (Maybe String)
z3Check inputFile = do
  (exitCode, stdoutMsg, stderrMsg) <- readProcessWithExitCode "z3" [inputFile] ""
  pure $ if exitCode /= ExitSuccess
     then Just $ "Exit Code:" <> show exitCode <> "\nMsg: " <> stdoutMsg <> "\n" <> stderrMsg
     else Nothing
