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
type PredPair = (ClsIndex, ClsIndex)  -- index of appearance in head and body
type PredMap = FuncMap PredPair

resolute :: CHC FuncIx VarIx -> CHC FuncIx VarIx
resolute (CHC clss) =
  let clss' = go clss
   in loggerShow (appendLabel "resolRes" resolutionLog) "# of preds removed " (length clss - length clss') $
     CHC clss'
  where
    go cs =
      let predMap = foldl' findPred M.empty $ zip [0..] cs
          candidates = M.toList . M.mapMaybe filterResolution $ predMap
       in if null candidates
             then logger resolutionLog "no more candidates for resolution"
               cs
             else loggerShow resolutionLog "find one candidate for resolution" (head candidates) $
               go $ case head candidates of
                 (k, Left idxs)   -> trivialRemovePred cs k idxs
                 (k, Right idxes) -> removePred cs k idxes

findPred
  :: FuncMap (S.Set ClsIndex, S.Set ClsIndex)
  -> (ClsIndex, Clause VarIx FuncIx)
  -> FuncMap (S.Set ClsIndex, S.Set ClsIndex)
findPred indexMap (k, Clause{..}) =
    insertFuncApps True heads
  . insertFuncApps False body
  $ indexMap
  where
    insertFunc  inHead = flip $ M.alter (updateIndex inHead)
    insertFuncs inHead vs idxMap = foldl' (insertFunc inHead) idxMap vs

    insertFuncApps inHead = insertFuncs inHead . map func

    updateIndex True = \case
      Nothing           -> Just (S.singleton k, S.empty)
      Just (hIxs, bIxs) -> Just (S.insert k hIxs, bIxs)
    updateIndex False = \case
      Nothing           -> Just (S.empty, S.singleton k)
      Just (hIxs, bIxs) -> Just (hIxs, S.insert k bIxs)

filterResolution :: (S.Set ClsIndex, S.Set ClsIndex) -> Maybe (Either (S.Set ClsIndex) PredPair)
filterResolution (hIxs, bIxs) =
  case (S.size hIxs, S.size bIxs) of
    (0, 0) -> Nothing  -- impossible
    (0, _) -> Just . Left $ bIxs
    (_, 0) -> Just . Left $ hIxs
    (1, 1) -> Just . Right $ (fromSingleton hIxs, fromSingleton bIxs)
    _ -> Nothing
  where
    fromSingleton = head . S.toList

-- | Simply remove all appearance of @rho@ in clauses with index @idxs@
trivialRemovePred :: [Clause VarIx FuncIx] -> FuncIx -> S.Set ClsIndex -> [Clause VarIx FuncIx]
trivialRemovePred clss rho idxs = zipWith removeFunc [0..] clss
  where
    removeFunc i cls
      | i `S.member` idxs = removeFuncCls cls
      | otherwise         = cls
    removeRho = filter ((/= rho) . func)
    removeFuncCls cls = updateClauseVars $ Clause
      { vars = S.empty
      , heads = removeRho $ heads cls
      , body = removeRho $ body cls
      , phi = phi cls
      }

removePred :: [Clause VarIx FuncIx] -> FuncIx -> PredPair -> [Clause VarIx FuncIx]
removePred clss rho (k1, k2) = updateClauseVars combined : rest
  where
    (cls1, cls2, rest) = extractTwo k1 k2 clss

    (funcApp1, restHead) = extractRho rho $ heads cls1 ++ heads cls2
    (funcApp2, restBody) = extractRho rho $ body  cls1 ++ body  cls2

    resoluteConstraint =
      flatAndSeq $ zipWith equalVars (args funcApp1) (args funcApp2)

    equalVars v1 v2 = LIAAssert Eql (LIAVar v1) (LIAVar v2)

    combined = Clause
      { vars  = S.empty
      , heads = restHead
      , body  = restBody
      , phi   = flatAndSeq [phi cls1, phi cls2, resoluteConstraint]
      }

extractWith :: Eq b => (a -> b) -> b -> [a] -> (a, [a])
extractWith f b as =
  let (before, after) = span ((/= b) . f) as
   in if null after
         then error "During resolution, cannot extract because not present"
         else (head after, before ++ tail after)

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
