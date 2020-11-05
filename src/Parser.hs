{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser where

import           Control.Applicative    ((<|>))
import           Data.Either            (partitionEithers)
import           Data.Foldable          (foldl')
import qualified Data.List.NonEmpty     as NE
import qualified Data.Set               as S
import qualified Data.Text              as T
import           Data.Text.Read         (decimal)

import           Language.SMT2.Parser   (parseFileMsg, script)
import           Language.SMT2.Syntax

import           CHC
import           Language.Assertion.LIA

-- | parse a script to a graph, only accept the following commands:
--
-- * @(set-info _)@   -> ignore
-- * @(set-logic _)@  -> ignore, since we expect HORN with disjunction on the head
-- * @(declare-fun _symbol (_sort*) _sort)@ -> ignore, we check sort beforehand
-- * @(assert (forall/exists (_sortedVar+) (=> _term _term))@
--      where @_sortedVar@ is @(_symbol _sort)@
-- * @(check-sat)@    -> ignore
-- * @(get-model)@    -> ignore
-- * @(exit)@         -> ignore
--
-- all other commands are not expected, but when present, they are ignored as well
--
-- Reference: https://github.com/sosy-lab/sv-benchmarks/blob/master/clauses/README.txt
-- The following directories contain benchmarks in SMT-LIB2.
-- The benchmarks are annotated with (set-logic HORN) to indicate
-- that the formulas belong to a quantified Horn fragment.
-- The asserted formulas are of the form:
--
-- horn ::=
--   |   (forall (quantified-variables) body)
--   |   (not (exists (quantified-variables) co-body))
--   |   body              -- generalization, with no quantified vars
--
-- body ::=
--   |   (=> co-body body)
--   |   (or literal*)     -- extension
--   |   literal
--
-- co-body ::=
--   |   (and literal*)
--   |   literal
--
-- literal ::=
--   |   formula over interpreted relations (such as =, <=, >, ...)
--   |   (negated) uninterpreted predicate with arguments
--
-- A body has at most one uninterpreted relation with positive polarity,
-- and a co-body uses only uninterpreted relations with positive polarity.
--
-- Note: however, this hoice solver accepts more than one uninterpreted
-- relations with positive polarity in body.
parseScript :: T.Text -> Either T.Text (CHC T.Text T.Text)
parseScript t = do
  scr <- parseFileMsg script t
  let cmds = filter keep scr
  cls <- sequence $ map (parseRes . parseHorn) cmds
  pure (CHC cls)
  where
    keep Assert {} = True
    keep _         = False

parseRes :: [Clause T.Text T.Text] -> Either T.Text (Clause T.Text T.Text)
parseRes []    = Left "Parsing error"
parseRes (x:_) = Right x

-- | partial function from Assertion to a CHC clause
-- horn ::=
--   |   (forall (quantified-variables) body)
--   |   (not (exists (quantified-variables) co-body))
--   |   body              -- generalization, with no quantified vars
parseHorn :: Command -> [Clause T.Text T.Text]
parseHorn (Assert t) =  parseForall t
                    <|> parseNotExist t
                    <|> noQuantifiedVars t
  where
    noQuantifiedVars t = do
      (body, phi, heads) <- parseBody t
      pure $ Clause S.empty body phi heads

parseForall :: Term -> [Clause T.Text T.Text]
parseForall (TermForall srtVars t) = do
  vars <- parseSortedVars srtVars
  (body, phi, heads) <- parseBody t
  pure $ Clause vars body phi heads
parseForall _ = []

-- | (not (exists (quantified-variables) co-body)) === (forall (quantified-variables) (=> co-body true))
parseNotExist :: Term -> [Clause T.Text T.Text]
parseNotExist (TermApplication (Unqualified (IdSymbol "not"))
                               ((TermExists srtVars t) NE.:| empty)) = do
  vars <- parseSortedVars srtVars
  (body, phi) <- parseCobody t
  pure $ Clause vars body phi []
parseNotExist _ = []

parseSortedVars :: NE.NonEmpty SortedVar -> [S.Set T.Text]
parseSortedVars srtVars = if null vars then [] else [S.fromList vars]
  where
    vars = [ var | (SortedVar var (SortSymbol (IdSymbol "Int"))) <- NE.toList srtVars]

type BodyType = ([FuncApp T.Text T.Text], LIA Bool T.Text, [FuncApp T.Text T.Text])
type CobodyType = ([FuncApp T.Text T.Text], LIA Bool T.Text)

-- | accumulate all co-bodies into the clause's body
-- body ::=
--   |   (=> co-body body)
--   |   (or literal*)     -- extension
--   |   literal
parseBody :: Term -> [BodyType]
parseBody t =  parseImp t
           <|> parseOr t
           <|> literalToBody <$> (parseLiteral t)

-- | parse cobody
-- co-body ::=
--   |   (and literal*)
--   |   literal
parseCobody :: Term -> [CobodyType]
parseCobody t =  parseAnd t
             <|> literalToCobody <$> parseLiteral t

parseOr :: Term -> [BodyType]
parseOr (TermApplication (Unqualified (IdSymbol "or")) ts) = if null literals
                                                                then []
                                                                else [([], phi, funcApps)]
  where
    literals = parseLiterals ts
    (phis, funcApps) = partitionEithers literals
    phi = foldl' flatOr (LIABool False) phis
parseOr _ = []

parseAnd :: Term -> [CobodyType]
parseAnd (TermApplication (Unqualified (IdSymbol "and")) ts) = if null literals
                                                                 then []
                                                                 else [(funcApps, phi)]
  where
    literals = parseLiterals ts
    (phis, funcApps) = partitionEithers literals
    phi = foldl' flatAnd (LIABool True) phis
parseAnd _ = []

parseImp :: Term -> [BodyType]
parseImp (TermApplication (Unqualified (IdSymbol "=>"))
                          (cobodyTerm NE.:| [bodyTerm])) = do
  (body, phi) <- parseCobody cobodyTerm
  (body', phi', heads') <- parseBody bodyTerm
  pure $ (body ++ body', flatAnd phi (flatNot phi'), heads')
parseImp _ = []

type LiteralType = Either (LIA Bool T.Text) (FuncApp T.Text T.Text)

-- | parse a literal
-- literal ::=
--   |   formula over interpreted relations (such as =, <=, >, ...)
--   |   (negated) uninterpreted predicate with arguments
parseLiteral :: Term -> [LiteralType]
parseLiteral t =  Left <$> parseLIABool t
              <|> Right <$> parsePred t

parseLIABool :: Term -> [LIA Bool T.Text]
parseLIABool t = case parseTermLIA t of
                   Just (Right lia) -> [lia]
                   _                -> []

parsePred :: Term -> [FuncApp T.Text T.Text]
parsePred (TermApplication (Unqualified (IdSymbol f)) ts) = if null args
                                                               then []
                                                               else [FuncApp f args]
  where
    args = failOnEmpty $ parseArg <$> NE.toList ts
    parseArg :: Term -> [T.Text]
    parseArg (TermQualIdentifier (Unqualified (IdSymbol arg))) = [arg]
    parseArg _                                                 = []
parsePred _ = []

failOnEmpty :: [[a]] -> [a]
failOnEmpty xs = if any null xs
                    then []
                    else map head xs

parseLiterals :: NE.NonEmpty Term -> [LiteralType]
parseLiterals ts = failOnEmpty $ parseLiteral <$> NE.toList ts

literalToBody :: Either (LIA Bool T.Text) (FuncApp T.Text T.Text) -> BodyType
literalToBody (Left phi)      = ([], phi, [])
literalToBody (Right funcApp) = ([], LIABool False, [funcApp])

literalToCobody :: Either (LIA Bool T.Text) (FuncApp T.Text T.Text) -> CobodyType
literalToCobody (Left phi)      = ([], phi)
literalToCobody (Right funcApp) = ([funcApp], LIABool True)
