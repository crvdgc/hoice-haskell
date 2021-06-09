{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Parser where

import           Control.Applicative    ((<|>))
import           Data.Bifunctor         (bimap)
import           Data.Foldable          (foldl')
import qualified Data.List.NonEmpty     as NE
import qualified Data.Set               as S
import qualified Data.Text              as T
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
  cls <- mapM (parseRes . parseHorn) cmds
  pure (CHC cls)
  where
    keep Assert {} = True
    keep _         = False

-- | Either a variable with name @T.Text@ or a LIA expression
type ArgType = Either T.Text (LIA Int T.Text)

-- | Change LIA arguments into variables with additional constraints
-- @(f 0 x) -> (g y) ~> (f c x) /\ (= c 0) -> (g y)@
-- @(f (+ 1 x)) -> (g y) ~> (f l) /\ (= l (+ 1 x)) -> (g y)@
normalizeArgs :: FuncApp ArgType T.Text -> (FuncApp T.Text T.Text, LIA Bool T.Text)
normalizeArgs FuncApp{..} = (varFuncApp, normalizedLIA)
  where
    (vars, liaExprs) = foldl' addFresh ([], []) (zip args [1..])
    addFresh (varAcc, liaAcc) = \case
      (Left arg, _)  -> (varAcc ++ [arg], liaAcc)
      (Right lia, f) ->
        let fresh = T.pack ("normalVar_" ++ show f)
         in (varAcc ++ [fresh], LIAAssert Eql (LIAVar fresh) lia:liaAcc)
    varFuncApp = FuncApp func vars
    normalizedLIA = flatAndSeq liaExprs


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
      pure $ clauseFromUnnormBody (body, phi, heads)
parseHorn _ = []

-- (ts, lia, t's) === (and ts lia) => t's
type BodyType = ([FuncApp ArgType T.Text], LIA Bool T.Text, [FuncApp ArgType T.Text])

-- (ts, lia) === (and ts lia)
type CobodyType = ([FuncApp ArgType T.Text], LIA Bool T.Text)

clauseFromUnnormBody :: BodyType -> Clause T.Text T.Text
clauseFromUnnormBody (body, phi, heads) = let (body', bodyPhis) = unzip . map normalizeArgs $ body
                                              (heads', headsPhis) = unzip . map normalizeArgs $ heads
                                              phi' = flatAndSeq . (phi:) $ bodyPhis ++ headsPhis
                                           in updateClauseVars $ Clause S.empty body' phi' heads'

clauseFromUnnormCobody :: CobodyType -> Clause T.Text T.Text
clauseFromUnnormCobody (cobody, phi) = let (cobody', cobodyPhis) = unzip . map normalizeArgs $ cobody
                                           phi' = flatAndSeq (phi:cobodyPhis)
                                        in updateClauseVars $ Clause S.empty cobody' phi' []

parseForall :: Term -> [Clause T.Text T.Text]
parseForall (TermForall srtVars t) = do
  _ <- parseSortedVars srtVars
  (body, phi, heads) <- parseBody t
  pure $ clauseFromUnnormBody (body, phi, heads)
parseForall _ = []

-- | (not (exists (quantified-variables) co-body)) === (forall (quantified-variables) (=> co-body false))
parseNotExist :: Term -> [Clause T.Text T.Text]
parseNotExist (TermApplication (Unqualified (IdSymbol "not"))
                               (TermExists srtVars t NE.:| [])) = do
  _ <- parseSortedVars srtVars
  (cobody, phi) <- parseCobody t
  pure $ clauseFromUnnormCobody (cobody, phi)
parseNotExist _ = []

parseSortedVars :: NE.NonEmpty SortedVar -> [S.Set T.Text]
parseSortedVars srtVars = [S.fromList vars | not (null vars)]
  where
    vars = [ var | (SortedVar var (SortSymbol (IdSymbol "Int"))) <- NE.toList srtVars]

-- | accumulate all co-bodies into the clause's body
-- body ::=
--   |   (=> co-body body)
--   |   (or literal*)     -- extension
--   |   literal
parseBody :: Term -> [BodyType]
parseBody t =  parseImp t
           <|> parseOr t
           <|> literalToBody <$> parseLiteral t

-- | parse cobody
-- co-body ::=
--   |   (and literal*)
--   |   literal
parseCobody :: Term -> [CobodyType]
parseCobody t =  parseAnd t
             <|> literalToCobody <$> parseLiteral t

parseOr :: Term -> [BodyType]
parseOr (TermApplication (Unqualified (IdSymbol "or")) ts) = [([], phi, funcApps) | not (null parsed)]
  where
    parsed = failOnEmpty . fmap parseOrLiteral . NE.toList $ ts
    parseOrLiteral t =  parseOr t
                    <|> literalToBody <$> parseLiteral t
    phi = flatOrSeq . fmap (\(_, phi, _) -> phi) $ parsed
    funcApps = concatMap (\(_, _, funcApps') -> funcApps') parsed
parseOr _ = []

parseAnd :: Term -> [CobodyType]
parseAnd (TermApplication (Unqualified (IdSymbol "and")) ts) = [(funcApps, phi) | not (null parsed)]
  where
    parsed = failOnEmpty . fmap parseAndLiteral . NE.toList $ ts
    parseAndLiteral t = parseAnd t
                     <|> literalToCobody <$> parseLiteral t
    phi = flatAndSeq . fmap snd $ parsed
    funcApps = concatMap fst parsed
parseAnd _ = []

parseImp :: Term -> [BodyType]
parseImp (TermApplication (Unqualified (IdSymbol "=>"))
                          (cobodyTerm NE.:| [bodyTerm])) = do
  (body, phi) <- parseCobody cobodyTerm
  (body', phi', heads') <- parseBody bodyTerm
  pure (body ++ body', flatAnd phi (flatNot phi'), heads')
parseImp _ = []

type LiteralType = Either (LIA Bool T.Text) (FuncApp ArgType T.Text)

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

parseLIAInt :: Term -> [LIA Int T.Text]
parseLIAInt t = case parseTermLIA t of
                   Just (Left lia) -> [lia]
                   _               -> []

parsePred :: Term -> [FuncApp ArgType T.Text]
parsePred (TermApplication (Unqualified (IdSymbol f)) ts) = [FuncApp f args | not (null args)]
  where
    args = failOnEmpty $ parseArg <$> NE.toList ts
    parseArg :: Term -> [ArgType]
    parseArg (TermQualIdentifier (Unqualified (IdSymbol arg))) = [Left arg]
    parseArg t = let res = parseLIAInt t
                  in [Right . head $ res | not (null res)]
parsePred (TermQualIdentifier (Unqualified (IdSymbol f))) = [FuncApp f []]
parsePred _ = []

failOnEmpty :: [[a]] -> [a]
failOnEmpty xs = if any null xs
                    then []
                    else map head xs

parseLiterals :: NE.NonEmpty Term -> [LiteralType]
parseLiterals ts = failOnEmpty $ parseLiteral <$> NE.toList ts

parseLiteralOrs :: NE.NonEmpty Term -> ([LiteralType], [BodyType])
parseLiteralOrs ts = bimap failOnEmpty failOnEmpty literalOrs
  where
    literalOrs = foldl' accLiteralOr ([], []) $ NE.toList ts
    accLiteralOr (ls, bs) t = let l = parseLiteral t
                               in if null l
                                     then let b = parseOr t
                                           in (ls, bs ++ [b])
                                     else (ls ++ [l], bs)

literalToBody :: Either (LIA Bool T.Text) (FuncApp ArgType T.Text) -> BodyType
literalToBody (Left phi)      = ([], phi, [])
literalToBody (Right funcApp) = ([], LIABool False, [funcApp])

literalToCobody :: Either (LIA Bool T.Text) (FuncApp ArgType T.Text) -> CobodyType
literalToCobody (Left phi)      = ([], phi)
literalToCobody (Right funcApp) = ([funcApp], LIABool True)
