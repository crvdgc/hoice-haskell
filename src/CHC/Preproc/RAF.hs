{- Redundant Argument Filtering -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module CHC.Preproc.RAF where

import           CHC
import           CHC.Preproc
import           Language.Assertion.LIA

import           Control.Monad          ((<=<))
import qualified Data.IntMap            as M
import qualified Data.IntSet            as IS
import           Data.List              (foldl')
import           Data.Maybe             (isNothing)
import qualified Data.Set               as S
import           Teacher                (mkLIA)

import           Z3.Monad

-- -------
-- Top level
-- -------

type Arg = (FuncIx, Int)
type Erasure = [Arg]

raf :: IndexedCHC VarIx FuncIx -> IO (CHC VarIx FuncIx)
raf IndexedCHC{..} = do
   safe <- converge (filterSafeCHC chc) top
   pure $ eraseCHC safe chc
  where
    top = concatMap expandArity . M.toList $ funcArity
    expandArity (rho, arity) = map (rho,) [1..arity]

converge
  :: (Erasure -> IO Erasure)  -- ^ iterated
  -> Erasure                  -- ^ initial
  -> IO Erasure
converge = undefined

-- -------
-- Erasure application
--
-- Naming convention:
-- @eraseSomething@ performs an entire erasure to something
-- @eraseSomething_@ removes a signle @Arg@ in something
-- -------

eraseCHC :: Erasure -> CHC VarIx FuncIx -> CHC VarIx FuncIx
eraseCHC erasure (CHC clss) = CHC $ map (eraseClause erasure) clss

eraseClause :: Erasure -> Clause VarIx FuncIx -> Clause VarIx FuncIx
eraseClause erasure Clause{..} =
  let erased = erasedVars erasure heads `S.union` erasedVars erasure body
      cls' = Clause { vars  = vars
                    , body  = eraseFuncApps erasure body
                    , phi   = eraseConstraint erased phi
                    , heads = eraseFuncApps erasure heads
                    }
   in cls' { vars = allVars cls'}

erasedVars :: Erasure -> [FuncApp VarIx FuncIx] -> S.Set VarIx
erasedVars erasure funcApps = S.fromList . concatMap (`argVars` funcApps) $ erasure

-- | Apply an entire erasure to funcApps
eraseFuncApps :: Erasure -> [FuncApp v FuncIx] -> [FuncApp v FuncIx]
eraseFuncApps erasure funcApps = foldl' (flip eraseArgFuncApps) funcApps erasure

-- | Erase one (rho, v) from funcApps
eraseArgFuncApps :: Arg -> [FuncApp v FuncIx] -> [FuncApp v FuncIx]
eraseArgFuncApps (rho, k) = map eraseOne
  where
    eraseOne funcApp@FuncApp{..}
      | func == rho = FuncApp rho . map snd . filter ((== k) . fst) . zip [1..] $ args
      | otherwise   = funcApp

-- | after a var is erased, the hole will propagate, and select suitable unit value along the way
type EraseLIA res var = Maybe (LIA res var)

eraseConstraint :: S.Set VarIx -> LIA Bool VarIx -> LIA Bool VarIx
eraseConstraint vs = undefined

-- -------
-- Safety judgement
-- -------

filterSafeCHC :: CHC v FuncIx -> [Arg] -> IO [Arg]
filterSafeCHC (CHC clss) erasure = undefined

-- | Judge if a single argument is safe to erase
safeEraseArg :: (Ord v) => [Arg] -> Arg -> Clause v FuncIx -> IO Bool
safeEraseArg erasure arg Clause{..} = undefined

-- | Get all variables for argument (rho, k) from funcApps
argVars :: (Eq v) => Arg -> [FuncApp v FuncIx] -> [v]
argVars (rho, k) = map ((!! k) . args) . filter ((== rho) . func)

-- | check if any variables for argument (rho, k) appears 0 or at most once in funcApps
-- appears False: whether any
appears :: (Ord v) => Bool -> Arg -> [FuncApp v FuncIx] -> Bool
appears allowOnce = (checkAll . ) . argVars
  where
    checkAll = isNothing . fst . foldl' hasMet (Just S.empty, allowOnce)
    hasMet (Nothing, _) _ = (Nothing, False)
    hasMet (Just metVars, redeem) v =
      if v `S.member` metVars
        then if redeem
                then (Just metVars, False)
                else (Nothing, False)
        else (Just $ S.insert v metVars, redeem)


-- -------
-- Syntactical substitution
--
-- We start from the normal form where all arguments of predicates are variables.
--
-- Otherwise, introduce a fresh variable and add the formula equals the fresh variable to the constraint.
--
-- When checking if variable y appears only once in the body,
-- if y only appears in the constraint and once in the argument
-- for variables x1, x2, ..., xm to be removed (in H, but not in H|E),
-- let C be the constraint, z1, z2, ..., zn are variables in C but will not be removed,
-- Then y can be removed iff
-- forall z1 z2 ... zn. exists y x1 x2 ... xm. C
--
-- -------

mkSubstitute
  :: MonadZ3 z3
  => LIA Bool VarIx  -- ^ constraint
  -> S.Set VarIx     -- ^ exists variables
  -> z3 Result
mkSubstitute constr existsVs =
  let vs = freeVarsLIA constr
      forallVs = vs S.\\ existsVs
      evs = S.toList existsVs
      fvs = S.toList forallVs
   in do
    evMap <- zip evs <$> mapM (mkFreshIntVar . show) evs
    fvMap <- zip fvs <$> mapM (mkFreshIntVar . show) fvs
    varMap <- sequence $
      M.fromSet (mkFreshIntVar . show) (IS.fromAscList . S.toAscList $ vs)
    constrZ3 <- mkLIA $ (varMap M.!) <$> constr
    evApp <- mapM (toApp . snd) evMap
    fvApp <- mapM (toApp . snd) fvMap
    assert =<< mkForallConst [] fvApp =<< mkExistsConst [] evApp constrZ3
    check

canSubstitute
  :: LIA Bool VarIx
  -> S.Set VarIx
  -> IO Bool
canSubstitute constr existsVs = do
  res <- evalZ3 $ mkSubstitute constr existsVs
  pure $ case res of
    Sat -> True
    _   -> False

