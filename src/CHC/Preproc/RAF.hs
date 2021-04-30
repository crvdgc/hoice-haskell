{- Redundant Argument Filtering -}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE MultiWayIf     #-}
module CHC.Preproc.RAF where

import           CHC
import           CHC.Preproc
import           Language.Assertion.LIA

import           Control.Applicative    ((<|>))
import           Control.Monad          (when)
import           Data.Bifunctor         (bimap)
import           Data.Function          (on)
import qualified Data.IntMap            as M
import           Data.List              (elemIndex, foldl', minimumBy,
                                         partition, sort)
import qualified Data.List.NonEmpty     as NE
import           Data.Maybe             (fromJust, fromMaybe, isJust, isNothing,
                                         mapMaybe)
import qualified Data.Set               as S

import           Debug.Logger

-- -------
-- Top level
-- -------

type Arg = (FuncIx, Int)
type Erasure = [Arg]

raf :: IndexedCHC VarIx FuncIx -> CHC VarIx FuncIx
raf IndexedCHC{..} =
  let safe = loggerShowId rafLogger "safe" $ converge (filterSafeCHC chc) top
   in eraseCHC (reverse . sort $ safe) chc
  where
    top = loggerShowId rafLogger "top" $ concatMap expandArity . M.toList $ funcArity
    expandArity (rho, arity) = map (rho,) [0..arity-1]

converge
  :: (Erasure -> Erasure)  -- ^ iterated
  -> Erasure               -- ^ initial
  -> Erasure
converge iterated e = go e
  where
    go e = let e' = iterated e
            in if S.fromList e == S.fromList e'
                  then e
                  else go e'

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
                    , phi   = phi
                    , heads = eraseFuncApps erasure heads
                    }
   in cls' { vars = allVars cls'}

erasedVars :: (Show v, Ord v) => Erasure -> [FuncApp v FuncIx] -> S.Set v
erasedVars erasure funcApps = S.fromList . concatMap (`argVars` funcApps) $ erasure

-- | Apply an entire erasure to funcApps
eraseFuncApps :: Erasure -> [FuncApp v FuncIx] -> [FuncApp v FuncIx]
eraseFuncApps erasure = fmap eraseOne
  where
    eraseOne FuncApp{..} = FuncApp
      { func = func
      , args = [ arg | (arg, i) <- zip args [0..], (func, i) `notElem` erasure ]
      }

-- -------
-- Safety judgement
-- -------

filterSafeCHC :: (Show v, Ord v) => CHC v FuncIx -> [Arg] -> [Arg]
filterSafeCHC (CHC clss) erasure = loggerShow rafLogger "erasure before filter" erasure $ go [] erasure
  where
    go acc [] = acc
    go acc erasure@(arg:args) =
      if all (safeEraseArg (acc ++ erasure) arg) clss
         then go (arg:acc) args
         else go acc args

-- | Judge if a single argument is safe to erase
safeEraseArg :: (Show v, Ord v) => [Arg] -> Arg -> Clause v FuncIx -> Bool
safeEraseArg erasure arg cls@Clause{..} =
  logShowInput
    ( loggerShowId seLogger "1. at most once in body funcApps" (appears 1 arg body)
    && loggerShowId seLogger "2. all body vars safe to erase" (all safeEraseVar $ loggerShowId seLogger "body vars" (argVars arg body))
    && loggerShowId seLogger "3. not in heads after erase" (appears 0 arg (eraseFuncApps erasure heads))
    )
  where
    seLogger = appendLabel "safeEraseArg" rafLogger
    logShowInput =
      loggerShow seLogger "erasure" erasure $
      loggerShow seLogger "arg" arg $
      loggerShow seLogger "clause" cls
    xs = erasedVars erasure heads
    phiFreeVars = freeVarsLIA phi
    safeEraseVar y =
      let (eqls, phi') = loggerShowId seLogger "extracted eqls, rest" $ extractEquations (y `S.insert` xs) phi
       in not (y `S.member` phiFreeVars)
          ||  (  not (y `S.member` freeVarsLIA phi')
              && guassianCanSub (S.toList xs) y eqls
              )


-- | Get all variables for argument (rho, k) from funcApps
argVars :: (Show v, Eq v) => Arg -> [FuncApp v FuncIx] -> [v]
argVars (rho, k) funcApps = mapMaybe (\FuncApp{..} -> getArg k args) . filter ((== rho) . func) $
  loggerShowId (appendLabel "argVars" rafLogger) "funcApps" funcApps
    where
      getArg k args
        | length args <= k = Nothing
        | otherwise        = Just (args !! k)

-- | check if any variables for argument (rho, k) appears at most n times in funcApps
appears :: (Show v, Eq v) => Int -> Arg -> [FuncApp v FuncIx] -> Bool
appears n = (checkAll . ) . argVars
  where
    apLogger = appendLabel "appears" rafLogger
    checkAll = all (< n) . appearTimes
    appearTimes [] = []
    appearTimes (v:vs) =
      let (allV, noV) = partition (== v) vs
       in length allV : appearTimes noV


-- -------
-- Syntactical substitution
--
-- We start from the normal form where all arguments of predicates are variables.
--
-- Otherwise, introduce a fresh variable and add the formula equals the fresh variable to the constraint.
--
-- An argument's removal is safe if the corresponding variable y appears at most only once in the body,
-- and y only appears in top level equality constraints (e1, e2, ..., en) and once in the argument
-- for variables x1, x2, ..., xm to be removed (in H, but not in H|E),
-- Then y's removal is safe if
--
-- there exists numbers r1, r2, ..., rn \in [1..m] (can repeat) and linear functions s1, s2, ..., sn, such that
--
-- e1 <=> x_{r1} = s1(y, x1, x2, ..., xm)
-- e2 <=> x_{r2} = s2(y, x1, x2, ..., xm)
-- ...
-- en <=> x_{rn} = sn(y, x1, x2, ..., xm)
--
-- Proof: we remove each equation from the constraint and substitute x_{rj} in the head with sj(y, x1, x2, ..., xm).
-- Then y no longer appears in the constraint, while the satisfiability remains the same.
-- -------

-- | recursively collect all equation relations.
-- Only allow look deeper into @and@ levels
extractEquations
  :: Ord v
  => S.Set v                    -- ^ variables to be removed and to be checked
  -> LIA Bool v
  -> ([LIA Bool v], LIA Bool v) -- ^ equations and rest
extractEquations allowed = \case
     e@(LIAAssert Eql _ _) ->
       if freeVarsLIA e `S.isSubsetOf` allowed
         then ([e], LIABool True)
         else ([], e)
     LIASeqLogic And es ->
       foldr1 collect . map (extractEquations allowed) . NE.toList $ es
         where
           collect (es, rest) = bimap (es ++) (flatAnd rest)
     e -> ([], e)


-- | Check if with equations e1...en can be removed
guassianCanSub
  :: Eq v
  => [v]  -- ^ xs
  -> v    -- ^ y
  -> [LIA Bool v]
  -> Bool
guassianCanSub xs y eqls
  | y `elem` xs = True
  | otherwise   = guassianSolve $ loggerShowId rafLogger "matrix" $ map (eqlToVec (y:xs)) eqls  -- normalize to coefficients

-- | map an equality assertion to a vector
-- first column is always constants
-- second column is coefficients of y
-- rest is xs
eqlToVec :: Eq v => [v] -> LIA Bool v -> [Int]
eqlToVec vars (LIAAssert Eql eql1 eql2) = exprToVec eql1 `vecSub` exprToVec eql2
  where
    exprToVec = \case
      LIAVar v           -> 0:[ if v == var then 1 else 0 | var <- vars]
      LIAInt n           -> n:map (const 0) vars
      LIAArith Add e1 e2 -> exprToVec e1 `vecAdd` exprToVec e2
      LIAArith Sub e1 e2 -> exprToVec e1 `vecSub` exprToVec e2
      LIAArith Mul e1 e2 -> evaluateLIAInt (const 0) e1 `scalarMul` exprToVec e2


vecSub :: [Int] -> [Int] -> [Int]
vecSub = zipWith (-)

vecAdd :: [Int] -> [Int] -> [Int]
vecAdd = zipWith (+)

scalarMul :: Int -> [Int] -> [Int]
scalarMul 1 = id
scalarMul k = map (*k)

scalarDiv :: Int -> [Int] -> Maybe [Int]
scalarDiv 1 = Just
scalarDiv (-1) = Just . scalarMul (-1)
scalarDiv k = traverse $ \x ->
  if x `mod` k == 0
     then Just $ x `div` k
     else Nothing

vecGCD :: [Int] -> Maybe Int
vecGCD [] = Nothing
vecGCD [x] = Just x
vecGCD (x:xs) = gcd x <$> vecGCD xs

getCol :: Int -> [[Int]] -> [Int]
getCol c = map (!! c)

extractCol :: Int -> [Int] -> (Int, [Int])
extractCol j xs =
  let (before, after) = splitAt j xs
   in (head after, before ++ tail after)


-- | Solving process
--
-- 1. Divide by GCD for each vector
-- 2. Find x_j such that the coefficient a_{ij} == 1 or -1 in vector v_i
--   - Remove row i (since this equation can be removed by substitute x_{ij}
--   - For all the other row k, row_k' <- row_k + a_{kj} / a_{ij} (perform substitution)
--   - Remove column j (since there's only one use of x_j)
-- 3. If there are no more x columns
--   - If all coefficients are 0 (for y and constants), then succeed
--   - else fail (since there are concrete constraints, like 1 + y = 0
-- 4. Repeat until all vectors are removed, then succeed
guassianSolve :: [[Int]] -> Bool
guassianSolve [] = True  -- no equations to substitute
guassianSolve vs =
  let xColN = length (head vs) - 2
   in if
     | xColN < 0  -> error "Vector too short, missing the constant column or the y column"
     | xColN == 0 -> all (== [0, 0]) vs    -- no cols, check if all zero
     | otherwise  ->
       let cleaned = clean vs              -- remove zero, try divide by GCD
        in case findCoeffOne cleaned of
             Nothing -> False              -- if cannot find one, fail
             Just (row, j, rest) -> null rest || guassianSolve (replaceWith row j rest)
  where
    clean = map tryDiv . filter (not . all (== 0))
    tryDiv v = fromMaybe v $ do
      vGCD <- vecGCD v
      scalarDiv vGCD v

    findCoeffOne vs = go [] vs
      where
        go _ [] = Nothing
        go acc (v:vs) = do
          let (before, after) = span (\x -> x /= 1 && x /= -1) (drop 2 v)
          if null after  -- no coeff 1 or -1
            then go (v:acc) vs
            else
              let j = length before
                  row = before ++ tail after  -- col j of row is removed
                  rest = acc ++ vs            -- while rest vectors are not
               in pure $ if head after == 1
                     then (row, j, rest)
                     else (scalarMul (-1) row, j, rest)

    replaceWith row j = map $ \xs ->
      let (x, v) = extractCol j xs
       in v `vecSub` scalarMul x row


rafLogger :: LogInfo
rafLogger = appendLabel "raf" hoiceLog
