{-# LANGUAGE RecordWildCards #-}
module Data.CounterExample where

import           Control.Monad (foldM)
import           Data.List     (foldl', union)

import           CHC           (FuncIx, VarVal)

type FuncData = (FuncIx, [VarVal])

data Dataset = Dataset { pos :: [[FuncData]]               -- ^ at least one is @true@
                       , neg :: [[FuncData]]               -- ^ at least one is @false@
                       , imp :: [([FuncData], [FuncData])] -- ^ if lhs all @true@, rhs has at least one @true@
                       }
  deriving (Eq, Show)

emptyDataset :: Dataset
emptyDataset = Dataset { pos = []
                       , neg = []
                       , imp = []
                       }

instance Semigroup Dataset where
  d <> d' = Dataset (pos d `union` pos d') (neg d `union` neg d') (imp d `union` imp d')

instance Monoid Dataset where
  mempty = emptyDataset


splitSingle :: [[a]] -> ([a], [[a]])
splitSingle = foldl' acc ([], [])
  where
    acc (singles, rest) [a] = (a:singles, rest)
    acc (singles, rest) as  = (singles, as:rest)

-- Points known to be @True@ and @False@
type KnownPair = ([FuncData], [FuncData])

-- @KnownPair@ returns new known pairs because the current @FuncData@ is classified
-- simplifyPoint :: Bool -> Dataset -> FuncData -> Maybe (Dataset, KnownPair)
-- simplifyPoint b Dataset{..} point = if hasContradiction
--                                   then Nothing
--                                   else Just (Dataset pos' neg' restImp, (filterSingle impPos, filterSingle impNeg))
--   where
--     hasContradiction = let inspected = if b then neg' else pos'
--                         in any hasPoint inspected
--     restPos = if b then discharge pos else pos
--     restNeg = if b then neg else discharge neg
--     pos' = restPos <> impPos
--     neg' = restNeg <> impNeg
--     (impPos, impNeg, restImp) = foldl' acc ([], [], []) imp
--     filterSingle = concat . filter ((== 1) . length)
--
--     acc (accPos, accNeg, accImp) curImp@(ants, sucs) = if b
--                                                   then case (hasPoint ants, hasPoint sucs) of
--                                                          (False, False) -> (accPos, accNeg, curImp:accImp)
--                                                          (True, False) -> (sucs:accPos, accNeg, accImp)
--                                                          (_, True) -> (accPos, accNeg, accImp)
--                                                   else case (hasPoint ants, hasPoint sucs) of
--                                                          (False, False) -> (accPos, accNeg, curImp:accImp)
--                                                          (False, True) -> (accPos, ants:accNeg, accImp)
--                                                          (True, _) -> (accPos, accNeg, accImp)
--     hasPoint = (point `notElem`)
--     discharge = filter hasPoint

-- | @Nothing@ if found contradiction
-- else returns a simplified dataset and new known pairs
simplifyKnownPair :: Dataset -> KnownPair -> KnownPair -> Maybe (Dataset, KnownPair, KnownPair)
simplifyKnownPair dataset (assumpPos, assumpNeg) (newPos, newNeg) = do
  impDataset <- maybeImpRes
  pos' <- dischargeAndCheck True
  neg' <- dischargeAndCheck False
  let resPos = pos impDataset ++ pos'
  let resNeg = neg impDataset ++ neg'
  let (newPos', resPos') = splitSingle resPos
  let (newNeg', resNeg') = splitSingle resNeg
  pure (Dataset resPos' resNeg' (imp impDataset), (knownPos, knownNeg), (newPos', newNeg'))

  where
    knownPos = assumpPos ++ newPos
    knownNeg = assumpNeg ++ newNeg
    -- filter through the pos/neg constraints
    dischargeAndCheck :: Bool -> Maybe [[FuncData]]
    dischargeAndCheck b = let target = if b then pos dataset else neg dataset
                              dischargeIfExistIn = if b then knownPos else knownNeg
                              contradictIfAllIn = if b then knownNeg else knownPos
                              -- remove a point if its positivity is in contradiction to the constraint
                              removeContradict = filter (`notElem` contradictIfAllIn)
                              atPoint :: [[FuncData]] -> [FuncData] -> Maybe [[FuncData]]
                              atPoint acc points
                                | any (`elem` dischargeIfExistIn) points = Just acc
                                | null removed = Nothing
                                | otherwise = Just (removed:acc)
                                where
                                  removed = removeContradict points
                           in foldM atPoint [] target

    -- break implication constraints
    maybeImpRes = foldM atImp emptyDataset (imp dataset)
    atImp :: Dataset -> ([FuncData], [FuncData]) -> Maybe Dataset
    atImp acc (lhs, rhs)
      | any (`elem` knownNeg) lhs || any (`elem` knownPos) rhs = Just acc
      | null lhs' && null rhs' = Nothing                -- True => False
      | null lhs' = Just (acc { pos = rhs':pos acc })   -- True => rhs'
      | null rhs' = Just (acc { neg = lhs':neg acc })   -- lhs' => False
      | otherwise = Just (acc { imp = (lhs', rhs'):imp acc })
      where
        lhs' = filter (`notElem` knownPos) lhs
        rhs' = filter (`notElem` knownNeg) rhs



-- | closure application of simplifyPair
simplifyRound :: Dataset -> KnownPair -> KnownPair -> Maybe Dataset
simplifyRound dataset assump new@(newPos, newNeg)
  | null newPos && null newNeg = Just $ addAssump assump dataset
  | otherwise = simplifyKnownPair dataset assump new >>= uncurry3 simplifyRound
  where
    uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
    uncurry3 f (a, b, c) = f a b c

    addAssump :: KnownPair -> Dataset -> Dataset
    addAssump (assumpPos, assumpNeg) Dataset{..} = Dataset (pos ++ toSingleton assumpPos) (neg ++ toSingleton assumpNeg) imp

    toSingleton = map (:[])

simplifyFrom :: Dataset -> KnownPair -> Maybe Dataset
simplifyFrom dataset = simplifyRound dataset ([], [])

-- | bootstrap simplification from dataset
simplify :: Dataset -> Maybe Dataset
simplify Dataset{..} = let (singlePos, pos') = splitSingle pos
                           (singleNeg, neg') = splitSingle neg
                        in simplifyRound (Dataset pos' neg' imp) ([], []) (singlePos, singleNeg)
