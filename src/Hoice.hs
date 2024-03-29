{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Hoice where

import Control.Monad (when)
import           CHC
import           CHC.Preproc.RAF        (rafFar)
import           CHC.Preproc.Resolution (resolute)
import           Data.CounterExample
import           Data.Either            (partitionEithers)
import qualified Data.IntMap            as M
import           Data.Maybe             (catMaybes, isNothing)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Debug.Logger
import           Language.Assertion.LIA
import           Learner.DecisionTree
import           Learner.Internal
import           Learner.Propagate      (propagate)
import           Learner.Sat            (satSolve)
import           Parser
import           System.Exit            (exitFailure)
import           Teacher
import           Z3.Monad               hiding (simplify)

import System.TimeIt (timeItNamed)

type NamedFunc = FuncMap (T.Text, Int, LIA Bool VarIx)

type SynthResult = Either T.Text NamedFunc

synthesize :: T.Text -> IO (Either T.Text (NamedFunc, CHC T.Text T.Text))
synthesize srpt = case parseScript srpt of
  Left msg  -> pure . Left $ "Parse error: " <> msg
  Right chc -> fmap (, chc) <$> uncurry synthesizeCHC (indexCHCNames chc)

indexCHCNames :: CHC T.Text T.Text -> (CHC VarIx FuncIx, FuncMap T.Text)
indexCHCNames chc =
  let (chc', funcNames) = indexCHCFunc chc
      synthLog = appendLabel "synth" hoiceLog
      clsVars = loggerShow synthLog "funcNames" funcNames $ indexCHCVars chc'
      chc'' = CHC $ map fst clsVars -- discard varnames
   in (chc'', funcNames)

synthesizeCHC :: CHC VarIx FuncIx -> FuncMap T.Text -> IO SynthResult
synthesizeCHC chc@(CHC clss) funcNames =
  let arityMap = loggerShow statLog "# of preds" (M.size funcNames) $ loggerShow statLog "# of clauses" (length clss) $ chcArityMap chc funcNames
      initialSynth = loggerShow statLog "# of args" (sum arityMap) $ M.map (const $ LIABool False) funcNames
   in deindexNameArity funcNames arityMap <$> atTeacher chc arityMap initialSynth emptyDataset

statCHC :: CHC VarIx FuncIx -> FuncMap a -> IO ()
statCHC chc@(CHC clss) funcNames =
  let arityMap = chcArityMap chc funcNames
   in do
     putStrLn $ "# of clauses " ++ show (length clss)
     putStrLn $ "# of preds " ++ show (M.size funcNames) 
     putStrLn $ "# of args " ++ show (sum arityMap)

produceCheckingFile :: (Ord v, ToSMT v, ToSMT f) => NamedFunc -> CHC v f -> T.Text
produceCheckingFile resMap chc = T.intercalate "\n" [logic, definitions, assertions, checkSAT]
  where
    logic = "(set-logic LIA)"
    definitions = T.intercalate "\n" . map (funcToDef . snd) $ M.toList resMap
    assertions = toSMT chc
    checkSAT = "(check-sat)\n(exit)\n"

    funcToDef :: (T.Text, Int, LIA Bool VarIx) -> T.Text
    funcToDef (name, arity, bodyLIA) = "(define-fun |" <> name <> "| (" <> argList <> ") Bool\n" <> T.pack (show bodyLIA) <> "\n)"
      where
        argList = T.pack $ concatMap (\k -> "(v_" <> show k <> " Int) ") [0..arity-1]

type CEResult = Either T.Text (FuncMap (LIA Bool VarIx))

deindexNameArity :: FuncMap T.Text -> FuncMap Int -> CEResult -> SynthResult
deindexNameArity funcNames arityMap = fmap (M.mapWithKey findNameArity)
  where
    findNameArity rho = (funcNames M.! rho,arityMap M.! rho,)

ceExtractDatasetCHC :: FuncMap (LIA Bool BoundVarIx) -> CHC VarIx FuncIx -> IO (Either T.Text (Maybe Dataset))
ceExtractDatasetCHC funcMap (CHC clss) = do
  datasets <- mapM (ceExtractDatasetClause funcMap) clss
  let (ls, rs) = partitionEithers datasets
  pure $
    if null ls
      then
        if all isNothing rs
          then Right Nothing -- all not falsifiable
          else Right $ Just . mconcat . catMaybes $ rs
      else Left $ "Solver errors: " <> T.unwords ls

ceExtractDatasetClause :: FuncMap (LIA Bool BoundVarIx) -> Clause VarIx FuncIx -> IO (Either T.Text (Maybe Dataset))
ceExtractDatasetClause funcMap cls =
  let synthesized = (loggerShowId hoiceLog "to teacher" $ substituteVar $ fmap (funcMap M.!) cls)
   in do
        (res, maybeVarMap) <- evalZ3 . falsify . mkClause $ synthesized
        pure $ case res of
          Unsat -> logger hoiceLog "negation unsat (not falsifiable)" $ Right Nothing -- not falsifiable
          Undef -> Left $ "Solver error for clause: " <> T.pack (show cls)
          Sat -> case maybeVarMap of -- satisfiable, extract counter examples from model
            Nothing -> logger hoiceLog "No counter examples" $ Left "No counter examples"
            Just varMap -> loggerShowId hoiceLog "counterexamples" $ Right . Just $ buildDatasetClause cls varMap

-- | invoke learn, but bootstrap the @learnData@ from @Dataset@
atLearner :: FuncMap [Qualifier] -> FuncMap Int -> Dataset -> (Maybe LearnData, FuncMap (LIA Bool VarIx))
atLearner initialQuals arityMap learnDataset =
  let atLearnerLog = appendLabel "atLearner" hoiceLog
      learnClass = loggerShowId atLearnerLog "initial class" $ assignClass arityMap (annotateDegree learnDataset)
   in case propagate learnClass learnDataset of
        -- propagation failed
        Nothing -> logger atLearnerLog "Found contradiction when propagating, to SAT solving" (Nothing, M.empty)
        -- propagation succeeded, get propagated classification and dataset
        Just (learnClass', learnDataset') ->
          let learnData = loggerShowId atLearnerLog "LearnData" $ LearnData learnClass' learnDataset' initialQuals
           in -- use the propagated learnData for decision tree learning
              learn arityMap learnData

atTeacher :: CHC VarIx FuncIx -> FuncMap Int -> FuncMap (LIA Bool BoundVarIx) -> Dataset -> IO CEResult
atTeacher chc arityMap = iceRound
  where
    initialQuals = initializeQuals arityMap chc

    atTeacherLog = appendLabel "atTeacher" hoiceLog

    satRound :: Dataset -> IO CEResult
    satRound allDataset = case loggerShow atTeacherLog "sat round dataset" allDataset $ simplify allDataset of
      Nothing -> pure . Left $ "Found contradiction when simplifying the original teacher dataset in SAT, program unsafe"
      -- "free" simplification succeeded, use SAT solver to classify
      Just simplifiedDataset ->
        satSolve simplifiedDataset >>= \case
          Nothing -> pure . Left $ "Found contradiction when classifying the original teacher dataset in SAT, program unsafe"
          Just posNegPair -> case simplifyFrom simplifiedDataset posNegPair of
            Nothing -> pure . Left $ "Found contradiction with the returned classification in SAT, program unsafe"
            Just satDataset -> case atLearner initialQuals arityMap satDataset of
              -- decision tree successfully uses the SAT classification
              (Just _, funcMap) -> loggerShow atTeacherLog "learner use SAT classification, returns" funcMap $ iceRound funcMap allDataset
              _ -> pure . Left $ "Decision tree cannot learn from SAT classification, should never happen"

    iceRound :: FuncMap (LIA Bool BoundVarIx) -> Dataset -> IO CEResult
    iceRound funcMap knownDataset = do
      eitherDataset <- ceExtractDatasetCHC funcMap chc
      case eitherDataset of
        Left msg -> pure . Left $ msg
        Right Nothing -> pure . Right $ funcMap
        Right (Just newDataset) ->
          let allDataset = loggerShowId atTeacherLog "dataset before simplify" (newDataset <> knownDataset)
           in case atLearner initialQuals arityMap allDataset of
                -- decision tree succeeded, enter next round with result predicate candidates @funcMap@
                (Just _, funcMap') -> loggerShow atTeacherLog "learner returns" funcMap' $ iceRound funcMap' allDataset
                -- decision tree failed, discard current classification, use SAT
                _ -> satRound allDataset

withResult :: (NamedFunc -> IO ()) -> SynthResult -> IO ()
withResult f = \case
  Left msg -> T.putStrLn ("Synth error: " <> msg) >> exitFailure
  Right namedFunc -> T.putStrLn "Satisfied, result:" >> f namedFunc

withResultCHC :: (NamedFunc -> CHC T.Text T.Text -> IO ()) -> Either T.Text (NamedFunc, CHC T.Text T.Text) -> IO ()
withResultCHC f = \case
  Left msg -> T.putStrLn ("Synth error: " <> msg) >> exitFailure
  Right (namedFunc, chc) -> T.putStrLn "Satisfied, result: " >> f namedFunc chc

reportHoice :: SynthResult -> IO ()
reportHoice = withResult (T.putStrLn . myPshow)

reportCheckFile :: Either T.Text (NamedFunc, CHC T.Text T.Text) -> IO ()
reportCheckFile = withResultCHC $ \namedFunc chc -> do
  let checkFile = produceCheckingFile namedFunc chc
  T.putStrLn checkFile

hoice :: Bool -> FilePath -> IO ()
-- hoice file = readFile file >>= synthesize . T.pack >>= reportAndCheck
hoice produceCheck file = readFile file >>= timeItNamed "solve time" . synthesize . T.pack >>= report
  where
    report (Left err) = T.putStrLn err
    report res@(Right (funcDef, _)) = do
      reportHoice (Right funcDef)
      when produceCheck $
        reportCheckFile res

runPreproc :: Bool -> Bool -> Bool -> Bool -> FilePath -> IO ()
runPreproc raf resol produceCheck statMode file = print file >> readFile file >>= reportPreproc . T.pack
  where
    reportPreproc :: T.Text -> IO ()
    reportPreproc script =
      case parseScript script of
        Left msg  -> print $ "Parse error: " <> msg
        Right chc ->
            let (chc', funcNames) = indexCHCFunc chc
                clsVars = indexCHCVars chc'
                chc'' = CHC $ map fst clsVars -- discard varnames
                chcWrite = if statMode
                             then T.writeFile "/dev/null"
                             else T.putStrLn
             in do
               preproced <- timeItNamed "preproc time" . pure $
                   (if raf then rafFar funcNames else id)
                 . (if resol then resolute else id)
                 $ chc''
               r <- timeItNamed "solve time" $ synthesizeCHC preproced funcNames
               reportHoice r
               when produceCheck $
                 reportCheckFile $ (, chc) <$> r

runStat :: FilePath -> IO ()
runStat file = print file >> readFile file >>= reportStat . T.pack
  where
    reportStat :: T.Text -> IO ()
    reportStat script =
      case parseScript script of
        Left msg  -> print $ "Parse error: " <> msg
        Right chc -> let (chc', funcNames) = indexCHCFunc chc
                         clsVars = indexCHCVars chc'
                         chc'' = CHC $ map fst clsVars -- discard varnames
                      in statCHC chc'' funcNames
