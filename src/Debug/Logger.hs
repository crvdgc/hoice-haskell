module Debug.Logger where

import           Data.Bifunctor
import           Debug.Trace    (trace)

type LogInfo = (Int, String)

logger :: LogInfo -> String -> a -> a
logger (level, label) message = trace (  replicate 20 '>' <> "\n"
                                      <> replicate (2 * level) ' '
                                      <> "\ESC[31m[" <> label <> "]\ESC[0m: "
                                      <> message
                                      <> "\n" <> replicate 20 '<' <> "\n"
                                      )

loggerShow :: (Show a) => LogInfo -> String -> a -> a
loggerShow info message a = logger info (message <> ": " <> show a) a

genLogger :: (Show a) => LogInfo -> (String -> a -> a, String -> a -> a)
genLogger info = (logger info, loggerShow info)

incLevel :: LogInfo -> LogInfo
incLevel = first (+1)

setLabel :: String -> LogInfo -> LogInfo
setLabel label = second (const label)

overLabel :: (String -> String) -> LogInfo -> LogInfo
overLabel = second

appendLabel :: String -> LogInfo -> LogInfo
appendLabel label = bimap (+1) (<> " :: " <> label)

hoiceLogInfo = (0, "hoice")
learnerLogInfo = appendLabel "learner" hoiceLogInfo
teacherLogInfo = appendLabel "teacher" hoiceLogInfo
