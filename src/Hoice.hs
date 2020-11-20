{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Hoice where

import           Control.Monad
import           Data.Either            (partitionEithers)
import qualified Data.IntMap            as M
import           Data.Maybe             (catMaybes, isNothing)
import qualified Data.Text              as T
import           System.Exit            (exitFailure)
import           System.IO

import           Debug.Logger

import           Z3.Monad

import           Language.SMT2.Parser   (parseFileMsg, term)
import           Language.SMT2.Syntax   hiding (Sat, Unsat)

import           CHC
import           Data.CounterExample
import           Language.Assertion.LIA
import           Learner.DecisionTree
import           Parser
import           Teacher

type NamedFunc = FuncMap (T.Text, Int, LIA Bool VarIx)
type SynthResult = Either T.Text NamedFunc

synthesize :: T.Text -> IO SynthResult
synthesize srpt = case parseScript srpt of
                    Left msg  -> pure . Left $ "Parse error: " <> msg
                    Right chc -> synthesizeCHC chc

synthesizeCHC :: CHC T.Text T.Text -> IO SynthResult
synthesizeCHC chc = let (chc', funcNames) = indexCHCFunc chc
                        clsVars = loggerShow hoiceLog "funcNames" funcNames $ indexCHCVars chc'
                        chc'' = CHC $ map fst clsVars -- discard varnames
                        arityMap = chcArityMap chc'' funcNames
                        initialSynth = M.map (const $ LIABool False) funcNames
                     in deindexNameArity funcNames arityMap <$> atTeacher chc'' arityMap initialSynth emptyDataset

type CEResult = Either T.Text (FuncMap (LIA Bool VarIx))

deindexNameArity :: FuncMap T.Text -> FuncMap Int -> CEResult -> SynthResult
deindexNameArity funcNames arityMap = fmap (M.mapWithKey findNameArity)
  where
    findNameArity rho = (funcNames M.! rho, arityMap M.! rho,)

ceExtractDatasetCHC :: FuncMap (LIA Bool BoundVarIx) -> CHC VarIx FuncIx -> IO (Either T.Text (Maybe Dataset))
ceExtractDatasetCHC funcMap (CHC clss) = do
  datasets <- mapM (ceExtractDatasetClause funcMap) clss
  let (ls, rs) = partitionEithers datasets
  pure $ if null ls
    then if all isNothing rs
           then Right Nothing -- all not falsifiable
           else Right $ Just . mconcat . catMaybes $ rs
    else Left $ "Solver errors: " <> T.unwords ls

ceExtractDatasetClause :: FuncMap (LIA Bool BoundVarIx) -> Clause VarIx FuncIx -> IO (Either T.Text (Maybe Dataset))
ceExtractDatasetClause funcMap cls = let synthesized = (loggerShowId hoiceLog "to teacher" $ substituteVar $ fmap (funcMap M.!) cls) in do
  (res, maybeVarMap) <- evalZ3 . falsify . mkClause $ synthesized
  pure $ case res of
    Unsat -> logger hoiceLog "negation unsat (not falsifiable)" $ Right Nothing -- not falsifiable
    Undef -> Left $ "Solver error for clause: " <> T.pack (show cls)
    Sat -> case maybeVarMap of -- satisfiable, extract counter examples from model
             Nothing     -> Left "No counter examples"
             Just varMap -> Right . Just $ loggerShowId hoiceLog "counterexamples" $ buildDatasetClause cls varMap


atTeacher :: CHC VarIx FuncIx -> FuncMap Int -> FuncMap (LIA Bool BoundVarIx) -> Dataset -> IO CEResult
atTeacher chc arityMap funcMap knownDataset = do
  eitherDataset <- ceExtractDatasetCHC funcMap chc
  case eitherDataset of
    Left msg -> pure . Left $ msg
    Right Nothing -> pure . Right $ funcMap
    Right (Just dataset) -> let initialQuals = initializeQuals funcMap chc
                                allDataset = dataset <> knownDataset
                                learnClass = assignClass funcMap $ annotateDegree allDataset
                                learnData = loggerShowId atTeacherLog "LearnData" $ LearnData learnClass allDataset initialQuals
                                (_, funcMap') = learn chc arityMap learnData learnClass
                             in loggerShow atTeacherLog "learner returns" funcMap' $ atTeacher chc arityMap funcMap' allDataset
  where
    atTeacherLog = appendLabel "atTeacher" hoiceLog

produceCheckFile :: T.Text -> FuncMap (T.Text, Int, LIA Bool VarIx) -> T.Text
produceCheckFile inputSMT synthRes = T.unlines . addHouseKeeping . addDefinition synthRes . removeDeclaration . T.lines $ inputSMT
  where
    removeDeclaration = filter (T.isPrefixOf "(assert")
    addHouseKeeping cmds = ["(set-logic LIA)"] ++ cmds ++ ["(check-sat)", "(exit)"]
    addDefinition = (++) . map toDefinition . M.elems
    toDefinition (funcName, arity, lia) = T.concat ["(define-fun |", funcName, "| ", toSort arity, T.pack . show $ lia]
    toSort arity = T.concat [ "("
                            , T.unwords . map (\i -> "($" <> T.pack (show i) <> " Int)") $ [0..arity-1]
                            , " Bool)"
                            ]

withResult :: (FuncMap (T.Text, Int, LIA Bool VarIx) -> IO ()) -> SynthResult -> IO ()
withResult f = \case
  Left msg -> print ("Synth error: " <> msg) >> exitFailure
  Right namedFunc -> print "Satisfied, result:\n" >> f namedFunc

checkHoice :: T.Text -> SynthResult -> IO ()
checkHoice inputSMT = withResult (print . produceCheckFile inputSMT)

reportHoice :: SynthResult -> IO ()
reportHoice = withResult print

hoice :: FilePath -> IO ()
hoice file = readFile file >>= synthesize . T.pack >>= reportHoice

printCheckHoice :: FilePath -> IO ()
printCheckHoice file = do
  smtStr <- readFile file
  let smt = T.pack smtStr
  synthesize smt >>= checkHoice smt
