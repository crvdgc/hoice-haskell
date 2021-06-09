module CHC.Preproc where

import           Data.Maybe (fromJust)

-- | try all functions, terminate if none of them change @x@
convergeAll :: Eq a => [a -> a] -> a -> a
convergeAll fs x =
  case dropWhile (== x) . map ($ x) $ fs of
    []     -> x
    (x':_) -> convergeAll fs x'


-- | may diverge
converge :: Eq a => (a -> a) -> a -> a
converge f x = fromJust . convergeSeq $ iterate f x

convergeSeq :: Eq a => [a] -> Maybe a
convergeSeq [] = Nothing
convergeSeq xs =
  case dropWhile (uncurry (/=)) $ zip xs (tail xs) of
    []        -> Nothing
    ((x,_):_) -> Just x

