module Debug.Logger where

import           Data.Bifunctor
import           Debug.Trace    (trace)

type LogInfo = (Int, String)

logAction :: String -> a -> a
logAction x = trace $! x
-- logAction _ = id

logger :: LogInfo -> String -> a -> a
logger (level, label) message = logAction (  replicate (2 * level) '>'
                                          <> "\ESC[31m[" <> label <> "]\ESC[0m: "
                                          <> message
                                          )

loggerShow :: (Show a) => LogInfo -> String -> a -> b -> b
loggerShow info message a = logger info ("$" <> message <> "=" <> show a)

loggerShowId :: (Show a) => LogInfo -> String -> a -> a
loggerShowId info message a = logger info (message <> ": " <> show a) a

incLevel :: LogInfo -> LogInfo
incLevel = first (+1)

setLabel :: String -> LogInfo -> LogInfo
setLabel label = second (const label)

overLabel :: (String -> String) -> LogInfo -> LogInfo
overLabel = second

appendLabel :: String -> LogInfo -> LogInfo
appendLabel label = bimap (+1) (<> " :: " <> label)

hoiceLog = (0, "hoice")
learnerLog = appendLabel "learner" hoiceLog
teacherLog = appendLabel "teacher" hoiceLog
