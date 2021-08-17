{- Redundant Argument Filtering -}

{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module CHC.Preproc.RAF
  (rafFar)
  where

import           CHC
import           CHC.Preproc            (converge, convergeAll)
import           Language.Assertion.LIA

import           Data.Bifunctor         (bimap, second)
import qualified Data.IntMap            as M
import qualified Data.List.NonEmpty     as NE
import           Data.Maybe             (fromMaybe)
import qualified Data.Set               as S

import           Debug.Logger

-- -------
-- Top level
-- -------

type Arg = (FuncIx, Int)
type ErasureSet = S.Set Arg

rafFar :: FuncMap a -> CHC VarIx FuncIx -> CHC VarIx FuncIx
rafFar funcMap = convergeAll [fromTop funcMap raf, fromTop funcMap far]

fromTop
  :: FuncMap a
  -> (ErasureSet -> CHC VarIx FuncIx -> CHC VarIx FuncIx)
  -> CHC VarIx FuncIx
  -> CHC VarIx FuncIx
fromTop funcMap preprocessor chc = preprocessor top chc
  where
    fromTopLogger = appendLabel "fromTop" rafLogger
    funcArity = loggerShow fromTopLogger "chc" chc $ chcArityMap chc funcMap
    top = loggerShowId fromTopLogger "top" $ S.fromList . concatMap expandArity . M.toList $ funcArity
    expandArity (rho, arity) = map (rho,) [0..arity-1]

raf :: ErasureSet -> CHC VarIx FuncIx -> CHC VarIx FuncIx
raf eset chc =
  let safe = loggerShowId rafLogger "raf safe" $
               converge (filterSafeCHC safeEraseArgRAF chc) eset
   in loggerShow (appendLabel "rafRes" rafLogger) "# arguments removed " (length safe) $ eraseCHC safe chc

far :: ErasureSet -> CHC VarIx FuncIx -> CHC VarIx FuncIx
far eset chc =
  let safe = loggerShowId farLogger "far safe" $
               converge (filterSafeCHC safeEraseArgFAR chc) eset
   in loggerShow (appendLabel "farRes" farLogger) "# arguments removed " (length safe) $ eraseCHC safe chc


-- -------
-- Erasure application
--
-- Naming convention:
-- @eraseSomething@ performs an entire erasure to something
-- @eraseSomething_@ removes a signle @Arg@ in something
-- -------

eraseCHC :: ErasureSet -> CHC VarIx FuncIx -> CHC VarIx FuncIx
eraseCHC eset (CHC clss) = CHC $ map (eraseClause eset) clss

eraseClause :: ErasureSet -> Clause VarIx FuncIx -> Clause VarIx FuncIx
eraseClause eset Clause{..} =
  let cls' = Clause { vars  = vars
                    , body  = eraseFuncApps eset body
                    , phi   = phi
                    , heads = eraseFuncApps eset heads
                    }
   in cls' { vars = allVars cls'}

erasedVars :: (Show v, Ord v) => ErasureSet -> [FuncApp v FuncIx] -> S.Set v
erasedVars eset funcApps = S.fromList . concatMap (`argVars` funcApps) $ eset

-- | Apply an entire erasure to funcApps
eraseFuncApps :: ErasureSet -> [FuncApp v FuncIx] -> [FuncApp v FuncIx]
eraseFuncApps eset = fmap eraseOne
  where
    eraseOne FuncApp{..} = FuncApp
      { func = func
      , args = [ arg | (arg, i) <- zip args [0..], not $ (func, i) `S.member` eset ]
      }

-- -------
-- Safety judgement
-- -------

filterSafeCHC
  :: (Show v, Ord v)
  => (ErasureSet -> Arg -> Clause v FuncIx -> Bool)
  -> CHC v FuncIx
  -> ErasureSet
  -> ErasureSet
filterSafeCHC safetyP (CHC clss) eset = loggerShow rafLogger "erasure before filter" eset $
  S.fromList . go [] . S.toList $ eset
  where
    go acc [] = acc
    go acc curErasure@(arg:args) =
      if all (safetyP (S.fromList $ acc ++ curErasure) arg) clss
         then go (arg:acc) args
         else go acc args


-- | Judge if a single argument is safe to erase, with RAF algorithm
safeEraseArgRAF :: (Show v, Ord v) => ErasureSet -> Arg -> Clause v FuncIx -> Bool
safeEraseArgRAF eset arg cls@Clause{..} =
  logShowInput
    ( loggerShowId seLogger "1. at most once in body funcApps" (appears 1 arg body body)
    && loggerShowId seLogger "2. all body vars safe to erase" (all safeEraseVar $ loggerShowId seLogger "body vars" (argVars arg body))
    && loggerShowId seLogger "3. not in heads after erase" (arg `S.member` eset || appears 0 (shiftArg eset arg) body (eraseFuncApps eset heads))
    )
  where
    seLogger = appendLabel "safeEraseArgRAF" rafLogger
    logShowInput =
      loggerShow seLogger "eset" eset $
      loggerShow seLogger "arg" arg $
      loggerShow seLogger "clause" cls
    xs = erasedVars eset heads
    phiFreeVars = freeVarsLIA phi
    safeEraseVar y =
      let (eqls, phi') = loggerShowId seLogger "extracted eqls, rest" $ extractEquations (y `S.insert` xs) phi
       in not (y `S.member` phiFreeVars)
          ||  (  not (y `S.member` freeVarsLIA phi')
              && guassianCanSub (S.toList xs) y eqls
              )


safeEraseArgFAR :: ErasureSet -> Arg -> Clause VarIx FuncIx -> Bool
safeEraseArgFAR eset arg cls@Clause{..} =
  logShowInput
    ( loggerShowId seLogger "1. at most once in head funcApps" (appears 1 arg heads heads)
    && loggerShowId seLogger "2. not in the constraint" (not $ fst arg `S.member` phiFreeVars)
    && loggerShowId seLogger "3. not in body funcApps after erase" (arg `S.member` eset || appears 0 (shiftArg eset arg) heads (eraseFuncApps eset body))
    )
  where
    seLogger = appendLabel "safeEraseArgFAR" farLogger
    logShowInput =
      loggerShow seLogger "eset" eset $
      loggerShow seLogger "arg" arg $
      loggerShow seLogger "clause" cls
    phiFreeVars = freeVarsLIA phi

-- | Get all variables for argument (rho, k) from funcApps
argVars :: (Show v, Eq v) => Arg -> [FuncApp v FuncIx] -> [v]
argVars (rho, k) funcApps = map (\FuncApp{..} -> args !! k) . filter ((== rho) . func) $
  loggerShow argVarLog "arg" (rho, k) $ loggerShowId argVarLog "funcApps" funcApps
    where argVarLog = appendLabel "argVars" rafLogger

-- | check if any variable for argument (rho, k) from source appears at most n times in target
appears :: (Show v, Ord v) => Int -> Arg -> [FuncApp v FuncIx] -> [FuncApp v FuncIx] -> Bool
appears n arg src target = S.foldr (&&) True appearTimes
  where
    srcVs = S.fromList $ argVars arg src
    targetVs = argVars arg target
    appearTimes = S.map countTimes srcVs
    countTimes v = (<= n) . length . filter (== v) $ targetVs


-- | shift argument if some args before it get erased
-- @arg@ must not be in @ErasureSet@ (it wouldn't make sense either)
shiftArg :: ErasureSet -> Arg -> Arg
shiftArg eset arg = second (\x -> x - nBefore) arg
  where
    nBefore = S.size . S.filter (isBefore arg) $ eset
    isBefore (f, k) (f', k') = f' == f && k' < k

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

type Equation v = (LIA Int v, LIA Int v)

-- | recursively collect all equation relations.
-- Only allow look deeper into @and@ levels
extractEquations
  :: Ord v
  => S.Set v                    -- ^ variables to be removed and to be checked
  -> LIA Bool v
  -> ([Equation v], LIA Bool v) -- ^ equations and rest
extractEquations allowed = \case
     e@(LIAAssert Eql e1 e2) ->
       if freeVarsLIA e `S.isSubsetOf` allowed
         then ([(e1, e2)], LIABool True)
         else ([], e)
     LIASeqLogic And es ->
       foldr1 collect . map (extractEquations allowed) . NE.toList $ es
         where
           collect (equations, rest) = bimap (equations ++) (flatAnd rest)
     e -> ([], e)


-- | Check if with equations e1...en can be removed
guassianCanSub
  :: Eq v
  => [v]  -- ^ xs
  -> v    -- ^ y
  -> [Equation v]
  -> Bool
guassianCanSub xs y eqls
  | y `elem` xs = True
  | otherwise   = guassianSolve $ loggerShowId rafLogger "matrix" $ map (eqlToVec (y:xs)) eqls  -- normalize to coefficients

-- | map an equality assertion to a vector
-- first column is always constants
-- second column is coefficients of y
-- rest is xs
eqlToVec :: Eq v => [v] -> Equation v -> [Int]
eqlToVec vars (eql1, eql2) = exprToVec eql1 `vecSub` exprToVec eql2
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
vecGCD []     = Nothing
vecGCD [x]    = Just x
vecGCD (x:xs) = gcd x <$> vecGCD xs

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
guassianSolve vars =
  let xColN = loggerShow guassianLog "vars" vars $ length (head vars) - 2
   in if
     | xColN < 0  -> error "Vector too short, missing the constant column or the y column"
     | xColN == 0 -> all (== [0, 0]) vars    -- no cols, check if all zero
     | otherwise  ->
       let cleaned = clean vars              -- remove zero, try divide by GCD
        in case findCoeffOne cleaned of
             Nothing -> False              -- if cannot find one, fail
             Just (row, j, rest) -> null rest || guassianSolve (replaceWith row j rest)
  where
    guassianLog = appendLabel "guassian" rafLogger
    clean = map tryDiv . filter (not . all (== 0))
    tryDiv v = fromMaybe v $ do
      vGCD <- vecGCD v
      scalarDiv vGCD v

    findCoeffOne = go []
      where
        go _ [] = Nothing
        go acc (v:vs) =
          let (before, after) = span (\x -> x /= 1 && x /= -1) (drop 2 v)
           in if null after  -- no coeff 1 or -1
                then go (v:acc) vs
                else
                  let j = length before
                      row = take 2 v ++ before ++ tail after  -- col j of row is removed
                      rest = acc ++ vs            -- while rest vectors are not
                   in Just $ if head after == 1
                         then (row, j, rest)
                         else (scalarMul (-1) row, j, rest)

    replaceWith row j = map $ \xs ->
      let (x, v) = extractCol j xs
       in v `vecSub` scalarMul x row


rafLogger :: LogInfo
farLogger :: LogInfo
rafLogger = appendLabel "raf" hoiceLog
farLogger = appendLabel "far" hoiceLog
