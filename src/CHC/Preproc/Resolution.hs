{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module CHC.Preproc.Resolution where

import           CHC
import           Language.Assertion.LIA

import qualified Data.IntMap            as M
import           Data.List              (foldl')
import qualified Data.Set               as S

import           Debug.Logger

type ClsIndex = Int
type PredIndex = (ClsIndex, ClsIndex)  -- index of appearance in head and body
type PredMap = FuncMap PredIndex

resolute :: CHC FuncIx VarIx -> CHC FuncIx VarIx
resolute (CHC clss) = CHC $ go clss
  where
    go cs =
      let predMap = foldl' findPred M.empty $ zip [0..] cs
          candidates = M.toList . M.mapMaybe filterResolution $ predMap
       in if null candidates
             then logger resolutionLog "no more candidates for resolution"
               cs
             else loggerShow resolutionLog "find one candidate for resolution" (head candidates) $
               go $ removePred cs (head candidates)

data PredState
  = OnceAt ClsIndex
  | NotSeen
  | MoreThanOnce
  deriving (Eq, Show)

findPred
  :: FuncMap (PredState, PredState)
  -> (ClsIndex, Clause VarIx FuncIx)
  -> FuncMap (PredState, PredState)
findPred indexMap (k, Clause{..}) =
    insertFuncApps True heads
  . insertFuncApps False body
  $ indexMap
  where
    insertFunc  inHead = flip $ M.alter (updateIndex inHead)
    insertFuncs inHead vs idxMap = foldl' (insertFunc inHead) idxMap vs

    insertFuncApps inHead = insertFuncs inHead . map func

    updateIndex True = \case
      Nothing            -> Just (OnceAt k, NotSeen)
      Just (NotSeen, st) -> Just (OnceAt k, st)
      Just (_, st)       -> Just (MoreThanOnce, st)
    updateIndex False = \case
      Nothing            -> Just (NotSeen, OnceAt k)
      Just (st, NotSeen) -> Just (st, OnceAt k)
      Just (st, _)       -> Just (st, MoreThanOnce)

filterResolution :: (PredState, PredState) -> Maybe PredIndex
filterResolution = \case
  (OnceAt k1, OnceAt k2) ->
    if k1 /= k2
       then Just (k1, k2)
       else Nothing
  _ -> Nothing


removePred :: [Clause VarIx FuncIx] -> (FuncIx, PredIndex) -> [Clause VarIx FuncIx]
removePred clss (rho, (k1, k2)) = combined:rest
  where
    (cls1, cls2, rest) = extractTwo k1 k2 clss

    (funcApp1, restHead) = extractRho $ heads cls1 ++ heads cls2
    (funcApp2, restBody) = extractRho $ body cls1 ++ body cls2

    resoluteConstraint =
      flatAndSeq $ zipWith equalVars (args funcApp1) (args funcApp2)

    equalVars v1 v2 = LIAAssert Eql (LIAVar v1) (LIAVar v2)

    combined = Clause
      { vars = S.empty
      , heads = restHead
      , body  = restBody
      , phi   = flatAndSeq [phi cls1, phi cls2, resoluteConstraint]
      }

    extractRho xs =
      let (before, after) = span ((== rho) . func) xs
       in (head after, before ++ tail after)


-- TODO: More efficient implementation
extractTwo :: Int -> Int -> [a] -> (a, a, [a])
extractTwo k1 k2 xs = (xs !! k1, xs !! k2, filtered)
  where
    filtered = map snd . filter (\(k, _) -> k /= k1 && k /= k2) $ zip [0..] xs

resolutionLog :: LogInfo
resolutionLog = appendLabel "resol" hoiceLog
