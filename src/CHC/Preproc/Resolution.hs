{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module CHC.Preproc.Resolution where

import           CHC
import           Language.Assertion.LIA

import qualified Data.IntMap            as M
import           Data.List              (foldl')
import Data.Maybe (fromJust)
import qualified Data.Set               as S

import           Debug.Logger

type ClsIndex = Int
type PredPair = (S.Set ClsIndex, S.Set ClsIndex)  -- index of appearance in head and body
type PredMap = FuncMap PredPair

resolute :: CHC FuncIx VarIx -> CHC FuncIx VarIx
resolute (CHC clss) =
  let clss' = go clss
   in loggerShow resolResLog "# of clauses removed from resolution" (length clss - length clss') $
     CHC clss'
  where
    resolResLog = appendLabel "resolRes" resolutionLog
    go cs =
      let predMap = foldl' findPred M.empty $ zip [0..] cs
          candidates = M.toList . M.mapMaybe filterResolution $ predMap
       in if null candidates
             then logger resolutionLog "no more candidates for resolution"
               cs
             else loggerShow resolResLog "find one candidate for resolution" (head candidates) $
               go $ case head candidates of
                 (k, Left _)   -> map (`trivialRemovePred` k) cs
                 (k, Right idxes) -> removePred cs k idxes

findPred
  :: FuncMap (Maybe (S.Set ClsIndex, S.Set ClsIndex))
  -> (ClsIndex, Clause VarIx FuncIx)
  -> FuncMap (Maybe (S.Set ClsIndex, S.Set ClsIndex))
findPred indexMap (k, Clause{..}) =
    insertFuncApps True heads
  . insertFuncApps False body
  $ indexMap
  where
    insertFunc  inHead = flip $ M.alter (updateIndex inHead)
    insertFuncs inHead vs idxMap = foldl' (insertFunc inHead) idxMap vs

    insertFuncApps inHead = insertFuncs inHead . map func

    updateIndex True = \case
      Nothing                  -> Just . Just $ (S.singleton k, S.empty)
      Just Nothing             -> Just Nothing
      Just (Just (hIxs, bIxs)) -> Just $ if k `S.member` hIxs || k `S.member` bIxs  -- same predicate appears more than once
                                            then Nothing
                                            else Just (S.insert k hIxs, bIxs)
    updateIndex False = \case
      Nothing                  -> Just . Just $ (S.empty, S.singleton k)
      Just Nothing             -> Just Nothing
      Just (Just (hIxs, bIxs)) -> Just $ if k `S.member` hIxs || k `S.member` bIxs  -- same predicate appears more than once
                                            then Nothing
                                            else Just (hIxs, S.insert k bIxs)

filterResolution :: Maybe (S.Set ClsIndex, S.Set ClsIndex) -> Maybe (Either (S.Set ClsIndex) PredPair)
filterResolution Nothing = Nothing
filterResolution (Just (hIxs, bIxs)) =
  case (S.size hIxs, S.size bIxs) of
    (0, 0) -> Nothing  -- impossible
    (0, _) -> Just . Left $ bIxs
    (_, 0) -> Just . Left $ hIxs
    _ -> Just . Right $ (hIxs, bIxs)

-- | Simply remove the at most one appearance of @rho@
trivialRemovePred :: Clause VarIx FuncIx -> FuncIx -> Clause VarIx FuncIx
trivialRemovePred cls rho = removeFuncCls
  where
    removeRho = filter ((/= rho) . func)
    removeFuncCls = updateClauseVars $ Clause
      { vars = S.empty
      , heads = removeRho $ heads cls
      , body = removeRho $ body cls
      , phi = phi cls
      }

removePred :: [Clause VarIx FuncIx] -> FuncIx -> PredPair -> [Clause VarIx FuncIx]
removePred clss rho (k1, k2) = loggerShowId removePredLog "combined" combined : rest
  where
    removePredLog = appendLabel "removePred" resolutionLog
    (appeared, rest) = extractTwoIxSets k1 k2 clss
    combined =
      if null appeared
         then error "no clauses"
         else let (cls1, clss') = (head appeared, tail appeared)
                  cls1' = trivialRemovePred cls1 rho
                  initialVars = fromJust . fst $ extractVarsFrom rho (heads cls1 <> body cls1)
               in foldl' (combineTwo initialVars) cls1' clss'
    combineTwo rhoVars acc cls =
      let offset = S.size . vars $ acc
          cls' = extractVarsForPred cls rho rhoVars offset
       in updateClauseVars $ Clause
            { vars = S.empty
            , heads = heads acc <> heads cls'
            , body = body acc <> body cls'
            , phi = flatAnd (phi acc) (phi cls')
            }

extractVarsFrom :: FuncIx -> [FuncApp VarIx FuncIx] -> (Maybe [VarIx], [FuncApp VarIx FuncIx])
extractVarsFrom rho funcApps =
  let (rhoApp, rest) = extractWithMaybe func rho funcApps
   in (args <$> rhoApp, rest)

extractVarsForPred ::
     Clause VarIx FuncIx
  -> FuncIx    -- ^ predicate p
  -> [VarIx]  -- ^ vars of p are mapped to the same binding
  -> Int      -- ^ all other vars are shifted by this offset
  -> Clause VarIx FuncIx  -- ^ the clause with p application removed, and variables shifted
extractVarsForPred Clause{..} rho pvs offset = cls'
  where
    (pvsHead, heads') = extractVarsFrom rho heads
    (pvsBody, body') = extractVarsFrom rho body
    pvs' = case (pvsHead, pvsBody) of
             (Just vs, _) -> vs
             (Nothing, Just vs) -> vs
             (Nothing, Nothing) -> error "no predicate application where at least one is expected"

    cls' = fmapClauseVar shiftVars . updateClauseVars $ Clause
      { vars = S.empty
      , heads = heads'
      , body = body'
      , phi = phi
      }

    mapRhoArg u = go pvs' pvs
      where
        go [] [] = Nothing
        go (v':vs') (v:vs) = if u == v'
                          then Just v
                          else go vs' vs
        go _ _ = error "application has different number of arguments"

    shiftVars u = case mapRhoArg u of
                    Just v -> v
                    Nothing -> u + offset

extractWithMaybe :: Eq b => (a -> b) -> b -> [a] -> (Maybe a, [a])
extractWithMaybe f b as =
  let (before, after) = span ((/= b) . f) as
   in if null after
         then (Nothing, before)
         else (Just $ head after, before ++ tail after)

extractWith :: Eq b => (a -> b) -> b -> [a] -> (a, [a])
extractWith f b as =
  let (before, after) = span ((/= b) . f) as
   in if null after
         then error "During resolution, cannot extract because not present"
         else (head after, before ++ tail after)

extractOne :: Int -> [a] -> (a, [a])
extractOne k xs = (xs !! k, filtered)
  where
    filtered = map snd . filter (\(k', _) -> k /= k') $ zip [0..] xs

-- TODO: More efficient implementation
extractTwo :: Int -> Int -> [a] -> (a, a, [a])
extractTwo k1 k2 xs = (xs !! k1, xs !! k2, filtered)
  where
    filtered = map snd . filter (\(k, _) -> k /= k1 && k /= k2) $ zip [0..] xs

extractTwoIxSets :: S.Set Int -> S.Set Int -> [a] -> ([a], [a])
extractTwoIxSets s1 s2 = go ([], []) . zip [0..]
  where
    go acc [] = acc
    go (appear, notAppear) ((k, x):xs) =
      if k `S.member` s1 || k `S.member` s2
         then go (x:appear, notAppear) xs
         else go (appear, x:notAppear) xs

resolutionLog :: LogInfo
resolutionLog = appendLabel "resol" hoiceLog
