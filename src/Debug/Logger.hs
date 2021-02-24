{-# LANGUAGE OverloadedStrings #-}
module Debug.Logger where

import           Data.Bifunctor
import qualified Data.Text          as T
import           Data.Text.Lazy     (toStrict)
import           Debug.Trace        (trace)
import           Text.Pretty.Simple (OutputOptions (..),
                                     defaultOutputOptionsDarkBg, pShowOpt)

type LogInfo = (Int, T.Text)

selection = [ -- "hoice :: learner :: pickoutQual",
              -- "hoice :: learner"
              -- "hoice :: atTeacher"
            -- , "hoice :: learner :: buildTree"
              "hoice"
            ]
exclusion = [ "hoice :: learner :: pickoutQual"
            , "hoice :: learner :: canBe"
            ]
--selection = []

myPshow :: Show a => a -> T.Text
myPshow = toStrict . pShowOpt defaultOutputOptionsDarkBg { outputOptionsIndentAmount = 2, outputOptionsCompact = True, outputOptionsCompactParens = True }

selectLog :: [T.Text] -> [T.Text] -> T.Text -> T.Text -> a -> a
selectLog selected excluded label message = if any (`T.isPrefixOf` label) selected && not (any (`T.isPrefixOf` label) excluded)
                                              then trace $! T.unpack message
                                              else id

logger :: LogInfo -> T.Text -> a -> a
logger (level, label) message = selectLog selection exclusion label (  T.replicate (2 * level) ">"
                                                                    <> T.pack (show level) <> " "
                                                                    <> "\ESC[31m[" <> label <> "]\ESC[0m: "
                                                                    <> message
                                                                    )

loggerShow :: (Show a) => LogInfo -> T.Text -> a -> b -> b
loggerShow info message a = logger info ("$" <> message <> "=" <> myPshow a)

loggerShowId :: (Show a) => LogInfo -> T.Text -> a -> a
loggerShowId info message a = logger info (message <> ": " <> myPshow a) a

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
