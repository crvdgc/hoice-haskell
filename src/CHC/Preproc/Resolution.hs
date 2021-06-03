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
               go $ case head candidates of
                 (k, Left idx)    -> trivialRemovePred cs k idx
                 (k, Right idxes) -> removePred cs k idxes

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

filterResolution :: (PredState, PredState) -> Maybe (Either ClsIndex PredIndex)
filterResolution = \case
  (OnceAt k1, OnceAt k2) ->
    if k1 /= k2
       then Just . Right $ (k1, k2)
       else Nothing
  (OnceAt k, NotSeen) -> Just . Left $ k
  (NotSeen, OnceAt k) -> Just . Left $ k
  _ -> Nothing


trivialRemovePred :: [Clause VarIx FuncIx] -> FuncIx -> ClsIndex -> [Clause VarIx FuncIx]
trivialRemovePred clss rho k = updateClauseVars removed : rest
  where
    (cls, rest) = extractOne k clss
    removeRho = filter ((/= rho) . func)
    removed = Clause
      { vars = S.empty
      , heads = removeRho $ heads cls
      , body = removeRho $ body cls
      , phi = phi cls
      }


removePred :: [Clause VarIx FuncIx] -> FuncIx -> PredIndex -> [Clause VarIx FuncIx]
removePred clss rho (k1, k2) = updateClauseVars combined : rest
  where
    (cls1, cls2, rest) = extractTwo k1 k2 clss

    (funcApp1, restHead) = extractRho rho $ heads cls1 ++ heads cls2
    (funcApp2, restBody) = extractRho rho $ body cls1 ++ body cls2

    resoluteConstraint =
      flatAndSeq $ zipWith equalVars (args funcApp1) (args funcApp2)

    equalVars v1 v2 = LIAAssert Eql (LIAVar v1) (LIAVar v2)

    combined = Clause
      { vars = S.empty
      , heads = restHead
      , body  = restBody
      , phi   = flatAndSeq [phi cls1, phi cls2, resoluteConstraint]
      }

extractWith :: Eq b => (a -> b) -> b -> [a] -> (a, [a])
extractWith f b as =
  let (before, after) = span ((== b) . f) as
   in (head after, before ++ tail after)

extractRho :: FuncIx -> [FuncApp VarIx FuncIx] -> (FuncApp VarIx FuncIx, [FuncApp VarIx FuncIx])
extractRho = extractWith func

extractOne :: Int -> [a] -> (a, [a])
extractOne k xs = (xs !! k, filtered)
  where
    filtered = map snd . filter (\(k', _) -> k /= k') $ zip [0..] xs

-- TODO: More efficient implementation
extractTwo :: Int -> Int -> [a] -> (a, a, [a])
extractTwo k1 k2 xs = (xs !! k1, xs !! k2, filtered)
  where
    filtered = map snd . filter (\(k, _) -> k /= k1 && k /= k2) $ zip [0..] xs

resolutionLog :: LogInfo
resolutionLog = appendLabel "resol" hoiceLog
