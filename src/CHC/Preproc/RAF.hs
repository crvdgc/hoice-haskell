{- Redundant Argument Filtering -}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module CHC.Preproc.RAF where

import           CHC
import           CHC.Preproc
import           Language.Assertion.LIA

import           Control.Applicative    ((<|>))
import           Data.Bifunctor         (bimap)
import           Data.Function          (on)
import qualified Data.IntMap            as M
import           Data.List              (elemIndex, foldl', minimumBy, sort)
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
            in if e == e'
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
filterSafeCHC (CHC clss) erasure = filter allSafe $ loggerShowId rafLogger "erasure before filter" erasure
  where
    allSafe arg = all (safeEraseArg erasure arg) clss

-- | Judge if a single argument is safe to erase
safeEraseArg :: (Show v, Ord v) => [Arg] -> Arg -> Clause v FuncIx -> Bool
safeEraseArg erasure arg cls@Clause{..} =
  logShowInput
    ( appears True arg body
    && all safeEraseVar (argVars arg body)
    && not (appears False arg (eraseFuncApps erasure heads))
    )
  where
    seLogger = appendLabel "safeEraseArg" rafLogger
    logShowInput =
      loggerShow seLogger "erasure" erasure $
      loggerShow seLogger "arg" arg $
      loggerShow seLogger "clause" cls
    xs = erasedVars erasure heads
    safeEraseVar y =
      let (eqls, phi') = extractEquations (y `S.insert` xs) phi
       in not (y `S.member` freeVarsLIA phi)
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

-- | check if any variables for argument (rho, k) appears 0 or at most once in funcApps
-- appears False: whether any
appears :: (Show v, Ord v) => Bool -> Arg -> [FuncApp v FuncIx] -> Bool
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
-- An argument's removal is safe if the corresponding variable y appears at most only once in the body,
-- and y only appears in top level equality constraints (e1, e2, ..., ek) and once in the argument
-- for variables x1, x2, ..., xm to be removed (in H, but not in H|E),
-- Then y's removal is safe if
--
-- there exists linear functions s1, s2, ..., sm, such that
--
-- e1, e2, ..., ek <=>
-- x1=s1(y, x1, x2, ..., xm)
-- x2=s2(y, x1, x2, ..., xm)
-- ...
-- xm=sm(y, x1, x2, ..., xm)
--
-- This can in turn be solved by first normalizing each ei as a coefficient vector
-- and then performing Gaussian elimination restricted to linear equations
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

-- | Check if with equations e1...ek, all xs can be substituted by xs and y
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
-- second column is y
-- rest is xs
eqlToVec :: Eq v => [v] -> LIA Bool v -> [Int]
eqlToVec vars (LIAAssert Eql eql1 eql2) = exprToVec eql1 `vecSub` exprToVec eql2
  where
    exprToVec = \case
      LIAVar v -> 0:[ if v == var then 1 else 0 | var <- vars]
      LIAInt n -> n:map (const 0) vars
      LIAArith Add e1 e2 -> exprToVec e1 `vecAdd` exprToVec e2
      LIAArith Sub e1 e2 -> exprToVec e1 `vecSub` exprToVec e2
      LIAArith Mul e1 e2 -> evaluateLIAInt (const 0) e1 `scalarMul` exprToVec e2


vecSub :: [Int] -> [Int] -> [Int]
vecSub = zipWith (-)

vecAdd :: [Int] -> [Int] -> [Int]
vecAdd = zipWith (+)

scalarMul :: Int -> [Int] -> [Int]
scalarMul k = map (*k)

scalarDiv :: Int -> [Int] -> Maybe [Int]
scalarDiv 1 = Just
scalarDiv (-1) = Just . scalarMul (-1)
scalarDiv k = traverse $ \x ->
  if x `mod` k == 0
     then Just $ x `div` k
     else Nothing

-- | normalize to the smallest non-zero coefficient, input must be non-empty
normalize :: [Int] -> [Int]
normalize xs
  | all (== 0) xs = xs  -- x can be trivially satisfied
  | otherwise     = fromMaybe xs $ scalarDiv (smallestNonZeroAbs xs) xs

-- | smallest non-zero absolute, input must not be empty or all 0
smallestNonZeroAbs :: [Int] -> Int
smallestNonZeroAbs = minimumBy (compare `on` abs) . filter (/= 0)

vecMulSub :: Int -> [Int] -> [Int] -> [Int]
vecMulSub = (vecSub .) . scalarMul

getCol :: Int -> [[Int]] -> [Int]
getCol c = map (!! c)

-- | Input matrix: first column is constant, second column is y, rest is xs
guassianSolve :: [[Int]] -> Bool
guassianSolve [] = False  -- no equations to substitute y to head
guassianSolve xss@(xs:_) = isJust . foldl' checkCol (Just xss) $ [2..length xs-1]
  where
    checkCol Nothing _     = Nothing  -- fail in the middle
    checkCol (Just []) _   = Just []  -- no more xs to check
    checkCol (Just xss) c  =
      let xss' = map normalize xss
          col = getCol c xss'
       in if | all (== 0) col              -> Just xss'
             | smallestNonZeroAbs col == 1 -> eliminateRest xss' col
             | otherwise                   -> Nothing

    eliminateRest xss col = findOne col >>= \(isNeg, rowNum) ->
      let col' = if isNeg then scalarMul (-1) col else col
          (before, (_, row):after) = splitAt rowNum $ zip col' xss
          rest = before ++ after
          eliminator = flip vecMulSub row
       in Just $ map (uncurry eliminator) rest

    findOne col =  ((True, ) <$> elemIndex 1 col)
               <|> ((False,) <$> elemIndex (-1) col)

rafLogger = appendLabel "raf" hoiceLog
