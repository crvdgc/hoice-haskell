module CHC.Preproc where

converge
  :: Eq a
  => (a -> a)  -- ^ iterated
  -> a -> a
converge iterated e =
  let e' = iterated e
   in if e == e'
        then e
        else converge iterated e'


