{-# LANGUAGE OverloadedStrings #-}
module Debug.Logger where

import           Data.Bifunctor
import qualified Data.Text      as T
import           Debug.Trace    (trace)

type LogInfo = (Int, T.Text)

--selection = [ "hoice :: learner :: pickoutQual"
--            , "hoice :: learner :: buildTree"
--            ]
selection = []

selectLog :: [T.Text] -> T.Text -> T.Text -> a -> a
selectLog selected label message = if any (`T.isPrefixOf` label) selected
                                      then trace $! T.unpack message
                                      else id

logger :: LogInfo -> T.Text -> a -> a
logger (level, label) message = selectLog selection label (  T.replicate (2 * level) ">"
                                                          <> "\ESC[31m[" <> label <> "]\ESC[0m: "
                                                          <> message
                                                          )

loggerShow :: (Show a) => LogInfo -> T.Text -> a -> b -> b
loggerShow info message a = logger info ("$" <> message <> "=" <> T.pack (show a))

loggerShowId :: (Show a) => LogInfo -> T.Text -> a -> a
loggerShowId info message a = logger info (message <> ": " <> T.pack (show a)) a

incLevel :: LogInfo -> LogInfo
incLevel = first (+1)

setLabel :: T.Text -> LogInfo -> LogInfo
setLabel label = second (const label)

overLabel :: (T.Text -> T.Text) -> LogInfo -> LogInfo
overLabel = second

appendLabel :: T.Text -> LogInfo -> LogInfo
appendLabel label = bimap (+1) (<> " :: " <> label)

hoiceLog = (0, "hoice")
learnerLog = appendLabel "learner" hoiceLog
teacherLog = appendLabel "teacher" hoiceLog
