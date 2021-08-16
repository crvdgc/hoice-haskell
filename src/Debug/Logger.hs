{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Debug.Logger where

import           Data.Bifunctor
import qualified Data.Text          as T
import           Data.Text.Lazy     (toStrict)
import           Debug.Trace        (trace)
import           Text.Pretty.Simple (OutputOptions (..),
                                     defaultOutputOptionsDarkBg, pShowOpt)

type LogInfo = (Int, T.Text)

data LogOption = LogOption { inclusion      :: [T.Text]
                           , exclusion      :: [T.Text]
                           , verbosityLimit :: Maybe Int
                           }

defaultLogOption = LogOption { inclusion = ["hoice"]
                             , exclusion = [ "hoice :: learner :: pickoutQual"
                                           , "hoice :: learner :: canBe"
                                           ]
                             , verbosityLimit = Nothing
                             }

myPshow :: Show a => a -> T.Text
myPshow = toStrict . pShowOpt defaultOutputOptionsDarkBg
  { outputOptionsIndentAmount  = 2
  , outputOptionsCompact       = True
  , outputOptionsCompactParens = True
  }

selectLog :: LogOption -> Int -> T.Text -> T.Text -> a -> a
selectLog LogOption{..} level label message = if any (`T.isPrefixOf` label) inclusion && not (any (`T.isPrefixOf` label) exclusion) && verbose
                                                then trace $! T.unpack message
                                                else id
  where
    verbose = case verbosityLimit of
                Nothing    -> True
                Just limit -> level <= limit

loggerWith :: LogOption -> LogInfo -> T.Text -> a -> a
-- loggerWith _ _ _ = id
loggerWith logOption (level, label) message = selectLog logOption level label (  T.replicate (2 * level) ">"
                                                                              <> T.pack (show level) <> " "
                                                                              <> "\ESC[31m[" <> label <> "]\ESC[0m: "
                                                                              <> message
                                                                              )
logger :: LogInfo -> T.Text -> a -> a
logger = loggerWith defaultLogOption { inclusion = [ "hoice :: raf :: rafRes"
                                                   , "hoice :: far :: farRes"
                                                   , "hoice :: resol :: resolRes"
                                                   , "hoice :: resol :: removePred"
                                                   , "hoice :: stat"
                                                   -- , "hoice :: atTeacher"
                                                   ]
                                     }

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

statLog = appendLabel "stat" hoiceLog
