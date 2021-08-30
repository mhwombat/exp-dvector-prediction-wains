------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Prediction.Experiment
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- A data mining agent, designed for the Créatúr framework.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.DVector.Prediction.Experiment
  (
    PatternWain,
    randomPatternWain,
    run,
    startRound,
    finishRound,
    schemaQuality,
    printStats,
    versionInfo,
    idealPopControlDeltaE -- exported for testing only
  ) where

import           ALife.Creatur
    (agentId, isAlive, programVersion)
import           ALife.Creatur.Persistent
    (getPS, putPS)
import           ALife.Creatur.Task
    (checkPopSize, requestShutdown)
import qualified ALife.Creatur.Wain                                    as W
import qualified ALife.Creatur.Wain.Brain                              as B
import           ALife.Creatur.Wain.Checkpoint
    (enforceAll)
import qualified ALife.Creatur.Wain.Classifier                         as Cl
import           ALife.Creatur.Wain.DVector.Pattern                    (Pattern)
import           ALife.Creatur.Wain.DVector.Prediction.Action
    (Action, postdict, predict)
import           ALife.Creatur.Wain.DVector.Prediction.DataSource
    (endOfData, nextVector)
import           ALife.Creatur.Wain.DVector.Prediction.Muser
    (DMuser, makeMuser)
import           ALife.Creatur.Wain.DVector.Prediction.ResponseTweaker
    (ResponseTweaker (..))
import qualified ALife.Creatur.Wain.DVector.Prediction.Universe        as U
import           ALife.Creatur.Wain.DVector.Tweaker
    (PatternTweaker (..))
import qualified ALife.Creatur.Wain.DVector.Wain                       as UW
import           ALife.Creatur.Wain.GeneticSOM
    (LearningParamRanges (..), numModels, randomLearningParams, schemaQuality,
    tweaker)
import           ALife.Creatur.Wain.PersistentStatistics
    (clearStats, readStats, updateStats)
import qualified ALife.Creatur.Wain.Predictor                          as P
import           ALife.Creatur.Wain.Pretty                             (pretty)
import           ALife.Creatur.Wain.Raw                                (raw)
import           ALife.Creatur.Wain.Response
    (Response, action, labels, outcomes)
import qualified ALife.Creatur.Wain.Statistics                         as Stats
import           ALife.Creatur.Gene.Numeric.UnitInterval
    (UIDouble, uiToDouble)
import           ALife.Creatur.Gene.Numeric.Util
    (unitInterval)
import           ALife.Creatur.Gene.Numeric.Weights
    (makeWeights, weightAt)
import           Control.Conditional                                   (whenM)
import           Control.Lens                                          hiding
    (universe)
import           Control.Monad
    (unless, when)
import           Control.Monad.IO.Class                                (liftIO)
import           Control.Monad.Random
    (Rand, RandomGen, evalRandIO, getRandom, getRandomR, getRandomRs,
    getRandoms)
import           Control.Monad.State.Lazy
    (StateT, execStateT, get, put)
import           Data.List                                            (intercalate)
import           Data.Version
    (showVersion)
import           Data.Word                                             (Word64)
import           Paths_exp_dvector_prediction_wains                    (version)
import           System.Directory
    (createDirectoryIfMissing)
import           System.FilePath
    (dropFileName)

versionInfo :: String
versionInfo
  = "exp-dvector-prediction-wains-" ++ showVersion version
      ++ ", compiled with " ++ UW.packageVersion
      ++ ", " ++ W.packageVersion
      ++ ", " ++ ALife.Creatur.programVersion

type PatternWain
  = W.Wain [Double] PatternTweaker ResponseTweaker DMuser Action

randomPatternWain
  :: RandomGen r
    => String -> U.Universe PatternWain -> Word64 -> Word64
      -> Rand r PatternWain
randomPatternWain wName u classifierSize predictorSize = do
  let k = view U.uVectorLength u
  let fcp = LearningParamRanges
              { _r0Range = view U.uClassifierR0Range u,
                _rfRange = view U.uClassifierRfRange u,
                _tfRange = view U.uClassifierTfRange u }
  fc <- randomLearningParams fcp
  ws <- (makeWeights . take (2*k)) <$> getRandoms
  let c = Cl.buildClassifier fc classifierSize (PatternTweaker ws)
  let fdp = LearningParamRanges
              { _r0Range = view U.uPredictorR0Range u,
                _rfRange = view U.uPredictorRfRange u,
                _tfRange = view U.uPredictorTfRange u }
  fd <- randomLearningParams fdp
  rtw <- (ResponseTweaker . makeWeights . take 2) <$> getRandoms
  let p = P.buildPredictor fd predictorSize rtw
  hw <- (makeWeights . take 4) <$> getRandomRs unitInterval
  t <- getRandom
  s <- getRandomR (view U.uStrictnessRange u)
  dp <- getRandomR $ view U.uDepthRange u
  wp <- getRandomR $ view U.uWidthRange u
  dos <- take 4 <$> getRandomRs (view U.uDefaultOutcomeRange u)
  let (Right mr) = makeMuser dos dp wp
  ios <- take 4 <$> getRandomRs (view U.uImprintOutcomeRange u)
  rds <- take 4 <$> getRandomRs (view U.uReinforcementDeltasRange u)
  let (Right wBrain) = B.makeBrain c mr p hw t s ios rds
  wDevotion <- getRandomR . view U.uDevotionRange $ u
  wAgeOfMaturity <- getRandomR . view U.uMaturityRange $ u
  let wPassionDelta = 0
  let wBoredomDelta = 0
  let wAppearance = replicate k 0
  return $ W.buildWainAndGenerateGenome wName wAppearance wBrain
    wDevotion wAgeOfMaturity wPassionDelta wBoredomDelta

data Summary = Summary
  {
    _rPopSize               :: Int,
    _rVectorNovelty         :: UIDouble,
    _rVectorAdjustedNovelty :: Int,
    _rPredDeltaE            :: Double,
    _rMetabolismDeltaE      :: Double,
    _rPopControlDeltaE      :: Double,
    _rMatingDeltaE          :: Double,
    _rOldAgeDeltaE          :: Double,
    _rOtherMatingDeltaE     :: Double,
    _rNetDeltaE             :: Double,
    _rChildNetDeltaE        :: Double,
    _rPredictedValue        :: Double,
    _rActualValue           :: Double,
    _rValuePredictionErr    :: Double,
    _rRewardPredictionErr   :: Double,
    _rBirthCount            :: Int,
    _rWeanCount             :: Int,
    _rFlirtCount            :: Int,
    _rMateCount             :: Int,
    _rDeathCount            :: Int
  }
makeLenses ''Summary

initSummary :: Int -> Summary
initSummary p = Summary
  {
    _rPopSize = p,
    _rVectorNovelty = 0,
    _rVectorAdjustedNovelty = 0,
    _rPredDeltaE = 0,
    _rMetabolismDeltaE = 0,
    _rPopControlDeltaE = 0,
    _rMatingDeltaE = 0,
    _rOldAgeDeltaE = 0,
    _rOtherMatingDeltaE = 0,
    _rNetDeltaE = 0,
    _rChildNetDeltaE = 0,
    _rPredictedValue = 0,
    _rActualValue = 0,
    _rValuePredictionErr = 0,
    _rRewardPredictionErr = 0,
    _rBirthCount = 0,
    _rWeanCount = 0,
    _rFlirtCount = 0,
    _rMateCount = 0,
    _rDeathCount = 0
  }

summaryStats :: Summary -> [Stats.Statistic]
summaryStats r =
  [
    Stats.dStat "pop. size" (view rPopSize r),
    Stats.dStat "DO novelty" (view rVectorNovelty r),
    Stats.iStat "DO novelty (adj.)"
      (view rVectorAdjustedNovelty r),
    Stats.dStat "adult prediction Δe" (view rPredDeltaE r),
    Stats.dStat "adult metabolism Δe" (view rMetabolismDeltaE r),
    Stats.dStat "adult pop. control Δe" (view rPopControlDeltaE r),
    Stats.dStat "adult mating Δe" (view rMatingDeltaE r),
    Stats.dStat "adult old age Δe" (view rOldAgeDeltaE r),
    Stats.dStat "other adult mating Δe" (view rOtherMatingDeltaE r),
    Stats.dStat "adult net Δe" (view rNetDeltaE r),
    Stats.dStat "child net Δe" (view rChildNetDeltaE r),
    Stats.dStat "predicted value" (view rPredictedValue r),
    Stats.dStat "actual value" (view rActualValue r),
    Stats.dStat "value pred. err" (view rValuePredictionErr r),
    Stats.dStat "reward pred err" (view rRewardPredictionErr r),
    Stats.iStat "bore" (view rBirthCount r),
    Stats.iStat "weaned" (view rWeanCount r),
    Stats.iStat "flirted" (view rFlirtCount r),
    Stats.iStat "mated" (view rMateCount r),
    Stats.iStat "died" (view rDeathCount r)
  ]

data Experiment = Experiment
  {
    _subject   :: PatternWain,
    _other     :: PatternWain,
    _weanlings :: [PatternWain],
    _universe  :: U.Universe PatternWain,
    _summary   :: Summary
  }
makeLenses ''Experiment

startRound :: StateT (U.Universe PatternWain) IO ()
startRound = do
  whenM (zoom U.uDataSource endOfData) $ requestShutdown "end of data"
  updateVector
  evaluateErrors

updateVector :: StateT (U.Universe PatternWain) IO ()
updateVector = do
  xsOld <- zoom U.uCurrVector getPS
  xs <- zoom U.uDataSource nextVector
  let deltas = zipWith (\x xOld -> abs (x - xOld)) xs xsOld
    -- xs is shorter because it doesn't include any deltas, so the
    -- result will be the same length as xs, and won't include any
    -- deltas of previous deltas.
  zoom U.uCurrVector $ putPS (xs ++ deltas)
  U.writeToLog $ "Current data: " ++ show xs
  U.writeToLog $ "Deltas: " ++ show deltas

evaluateErrors :: StateT (U.Universe PatternWain) IO ()
evaluateErrors = do
  xs <- zoom U.uCurrVector getPS
  ps <- zoom U.uNewPredictions getPS
  zoom U.uPrevPredictions $ putPS ps
  zoom U.uNewPredictions $ putPS []
  when (not . null $ ps) $ do
    let actual = head xs
    let predictions = map thirdOfThree ps
    let errors = map (\e -> abs (actual - e)) predictions
    let popPrediction = mean predictions
    let popError = abs (actual - popPrediction)
    let meanError = mean errors
    let minError = minimum errors
    zoom U.uMeanError $ putPS meanError
    zoom U.uMinError $ putPS minError
    U.writeToLog $ "actual=" ++ show actual
      ++ " pop. prediction=" ++ show popPrediction
      ++ " pop. error=" ++ show popError
    U.writeToLog $ "meanError=" ++ show meanError
      ++ " minError=" ++ show minError
      ++ " meanError-popError=" ++ show (meanError - popError)

finishRound :: StateT (U.Universe PatternWain) IO ()
finishRound = do
  v <- use U.uCurrVector
  assign U.uPrevVector v
  calculateMeanMetabMetric
  f <- use U.uStatsFile
  xss <- readStats f
  let yss = Stats.summarise xss
  printStats yss
  let zs = concat yss
  adjustPopControlDeltaE zs
  cs <- use U.uCheckpoints
  enforceAll zs cs
  clearStats f
  (a, b) <- use U.uAllowedPopulationRange
  checkPopSize (a, b)

calculateMeanMetabMetric :: StateT (U.Universe PatternWain) IO ()
calculateMeanMetabMetric = do
  ks <- zoom U.uCurrMetabMetrics getPS
  let k = if null ks
            then 1
            else mean (map snd ks)
  zoom U.uPrevMetabMetrics $ putPS ks
  zoom U.uPrevMeanMetabMetric $ putPS k
  zoom U.uCurrMetabMetrics $ putPS []

writeToLog :: String -> StateT Experiment IO ()
writeToLog = zoom universe . U.writeToLog

run :: [PatternWain]
      -> StateT (U.Universe PatternWain) IO [PatternWain]
run (me:otherWain:xs) = do
  when (null xs) $ U.writeToLog "WARNING: Last two wains  standing!"
  p <- U.popSize
  u <- get
  let e = Experiment { _subject = me,
                       _other = otherWain,
                       _weanlings = [],
                       _universe = u,
                       _summary = initSummary p}
  e' <- liftIO $ execStateT run' e
  put (view universe e')
  let modifiedAgents
        = view subject e' : view other e' : view weanlings e'
  U.writeToLog $
    "Modified agents: " ++ show (map agentId modifiedAgents)
  reportAnyDeaths modifiedAgents
  return modifiedAgents
run _ = error "too few wains"

run' :: StateT Experiment IO ()
run' = do
  (e0, ec0) <- totalEnergy
  a <- use subject
  writeToLog $ "---------- start of " ++ agentId a ++ "'s turn ----------"
  writeToLog $ "At beginning of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty (customStats a)
  -- mapM_ writeToLog . map ("before: " ++) $ report a
  -- writeToLog $ "..... start of reward ....."
  rewardPrediction
  -- writeToLog $ "..... end of reward ....."
  -- writeToLog $ "..... start of metabolism ....."
  runMetabolism
  -- writeToLog $ "..... end of metabolism ....."
  autoPopControl <- use (universe . U.uPopControl)
  when autoPopControl applyPopControl
  subject %= W.incAge
  -- writeToLog $ "..... start of flirt option ....."
  maybeFlirt
  -- writeToLog $ "..... end of flirt option ....."
  -- writeToLog $ "..... start of prediction ....."
  makePrediction
  -- writeToLog $ "..... end of prediction ....."
  a' <- use subject
  -- assign (summary.rNetDeltaE) (energy a' - energy a)
  unless (isAlive a') $ assign (summary.rDeathCount) 1
  summary %= fillInSummary
  (ef, ecf) <- totalEnergy
  balanceEnergyEquation e0 ec0 ef ecf
  updateChildren
  measureMetabolism
  killIfTooOld
  agentStats <- ((customStats a' ++) . summaryStats) <$> use summary
  writeToLog $ "At end of turn, " ++ agentId a
    ++ "'s summary: " ++ pretty agentStats
  rsf <- use (universe . U.uRawStatsFile)
  zoom universe $ writeRawStats (agentId a) rsf agentStats
  sf <- use (universe . U.uStatsFile)
  zoom universe $ updateStats agentStats sf
  -- mapM_ writeToLog . map ("after: " ++) $ report a
  writeToLog $ "---------- end of " ++ agentId a ++ "'s turn ----------"

customStats :: PatternWain -> [Stats.Statistic]
customStats w = Stats.stats w
  ++ [
       Stats.dStat "predictorActionWeight" . uiToDouble $
         ws `weightAt` 0,
       Stats.dStat "predictorScenarioWeight" . uiToDouble $
         ws `weightAt` 1
     ]
  where (ResponseTweaker ws) = view (W.brain . B.predictor . tweaker) w

fillInSummary :: Summary -> Summary
fillInSummary s = s
  {
    _rNetDeltaE = _rPredDeltaE s
         + _rMetabolismDeltaE s
         + _rPopControlDeltaE s
         + _rMatingDeltaE s
         + _rOldAgeDeltaE s
         + _rOtherMatingDeltaE s,
    _rChildNetDeltaE = 0
         -- include energy given to wains when they are born
         - _rMatingDeltaE s
         - _rOtherMatingDeltaE s
  }

balanceEnergyEquation
  :: Double -> Double -> Double -> Double -> StateT Experiment IO ()
balanceEnergyEquation e0 ec0 ef ecf = do
  netDeltaE1 <- use (summary . rNetDeltaE)
  let netDeltaE2 = ef - e0
  let err = abs (netDeltaE1 - netDeltaE2)
  when (err > 0.000001) $ do
    writeToLog $ "WARNING: Adult energy equation doesn't balance"
    writeToLog $ "e0=" ++ show e0 ++ ", ef=" ++ show ef
      ++ ", netDeltaE2=" ++ show netDeltaE2
      ++ ", netDeltaE1=" ++ show netDeltaE1
      ++ ", err=" ++ show err
  childNetDeltaE1 <- use (summary . rChildNetDeltaE)
  let childNetDeltaE2 = ecf - ec0
  let childErr = abs (childNetDeltaE1 - childNetDeltaE2)
  when (childErr > 0.000001) $ do
    writeToLog $ "WARNING: Child energy equation doesn't balance"
    writeToLog $ "ec0=" ++ show ec0 ++ ", ecf=" ++ show ecf
      ++ ", childNetDeltaE2=" ++ show childNetDeltaE2
      ++ ", childNetDeltaE1=" ++ show childNetDeltaE1
      ++ ", childErr=" ++ show childErr

measureMetabolism :: StateT Experiment IO ()
measureMetabolism = do
  a <- use subject
  ccf <- use (universe . U.uChildCostFactor)
  let x = metabMetric 1 a
                 + sum (map (metabMetric ccf) (view W.litter a))
  xs <- zoom (universe . U.uCurrMetabMetrics) getPS
  zoom (universe . U.uCurrMetabMetrics) $ putPS ((agentId a, x):xs)

metabMetric :: Double -> PatternWain -> Double
metabMetric scale w = scale * n
  where n = fromIntegral . numModels . view (W.brain . B.classifier) $ w

runMetabolism :: StateT Experiment IO ()
runMetabolism = do
  a <- use subject
  xs <- zoom (universe . U.uPrevMetabMetrics) getPS
  case lookup (agentId a) xs of
    Nothing ->
      zoom universe . U.writeToLog
        $ "First turn for " ++ agentId a ++ " no metabolism cost"
    Just n -> do
      bmc <- use (universe . U.uBaseMetabolismDeltaE)
      cpcm <- use (universe . U.uAdjustableMetabolismDeltaE)
      meanSize <- zoom (universe . U.uPrevMeanMetabMetric) getPS
      let deltaE = bmc + cpcm * n / meanSize
      adjustWainEnergy subject deltaE rMetabolismDeltaE "metabolism"

rewardPrediction :: StateT Experiment IO ()
rewardPrediction = do
  a <- use subject
  ps <- zoom (universe . U.uPrevPredictions) getPS
  case lookup3 (agentId a) ps of
    Nothing ->
      zoom universe . U.writeToLog
        $ "First turn for " ++ agentId a ++ " no prediction reward"
    Just (r, predicted) -> do
      actual <- head <$> zoom (universe . U.uCurrVector) getPS
      let e = abs (actual - predicted)
      meanError <- zoom (universe . U.uMeanError) getPS
      minError <- zoom (universe . U.uMinError) getPS
      meanDeltaE <- use (universe . U.uMeanAccuracyDeltaE)
      maxDeltaE <- use (universe . U.uMaxAccuracyDeltaE)
      let de = if (abs(meanError - minError) < 1e-5)
                 then meanDeltaE
                 else (meanDeltaE - maxDeltaE) * (e - minError)
                        / (meanError - minError) + maxDeltaE
      let deltaE = max 0 de
      adjustWainEnergy subject deltaE rPredDeltaE "prediction"
      zoom universe . U.writeToLog $
        agentId a ++ " predicted=" ++ show predicted
        ++ ", actual=" ++ show actual
        ++ ", error e=" ++ show e
        ++ ", meanDeltaE=" ++ show meanDeltaE
        ++ ", maxDeltaE=" ++ show maxDeltaE
        ++ ", meanError =" ++ show meanError
        ++ ", minError=" ++ show minError
        ++ ", deltaE=" ++ show deltaE
      assign (summary . rPredictedValue) predicted
      assign (summary . rActualValue) actual
      assign (summary . rValuePredictionErr) e
      letSubjectReflect a r

chooseAction3
  :: PatternWain -> Pattern
    -> StateT (U.Universe PatternWain) IO
        (W.DecisionReport Pattern Action, Response Action, PatternWain)
chooseAction3 w vs = do
  whenM (use U.uShowClassifierModels) $ do
    U.writeToLog "--- begin classifier models"
    mapM_ U.writeToLog $ W.prettyClassifierModels w
    U.writeToLog "--- end classifier models"
  whenM (use U.uShowPredictorModels) $ do
    U.writeToLog "--- begin predictor models"
    mapM_ U.writeToLog $ W.prettyPredictorModels w
    U.writeToLog "--- end predictor models"
  let (decisionReport, r, w') = W.chooseAction [vs] w
  let dObjNoveltyAdj = head $ W.adjNovelties decisionReport
  U.writeToLog $ "To " ++ agentId w
    ++ ", the vector has novelty " ++ show dObjNoveltyAdj
  whenM (use U.uShowClassificationReport) $ do
    U.writeToLog "--- begin classifier report"
    mapM_ U.writeToLog $ W.prettyClassificationReport w decisionReport
    U.writeToLog "--- end classifier report"
  whenM (use U.uShowScenarioReport) $ do
    U.writeToLog "--- begin scenario report"
    mapM_ U.writeToLog $ W.prettyScenarioReport w decisionReport
    U.writeToLog "--- end scenario report"
  whenM (use U.uShowPredictionReport) $ do
    U.writeToLog "--- begin predictor report"
    mapM_ U.writeToLog $ W.prettyPredictionReport w decisionReport
    U.writeToLog "--- end predictor report"
  whenM (use U.uShowActionReport) $ do
    U.writeToLog "--- begin action report"
    mapM_ U.writeToLog $ W.prettyActionReport w decisionReport
    U.writeToLog "--- end action report"
  U.writeToLog $ agentId w ++ " chooses to " ++ pretty (view action r)
    ++ " predicting the outcomes " ++ pretty (view outcomes r)
  return (decisionReport, r, w')


lookup3 :: Eq a => a -> [(a, b, c)] -> Maybe (b, c)
lookup3 k ((a, b, c):xs) | a == k    = Just (b, c)
                         | otherwise = lookup3 k xs
lookup3 _ [] = Nothing

thirdOfThree :: (a, b, c) -> c
thirdOfThree (_, _, c) = c

makePrediction :: StateT Experiment IO ()
makePrediction = do
  a <- use subject
  dObj <- zoom (universe . U.uCurrVector) getPS
  (decisionReport, r, a')
    <- zoom universe $ chooseAction3 a dObj
  assign (summary.rVectorNovelty) (head $ W.novelties decisionReport)
  assign (summary.rVectorAdjustedNovelty) (head $ W.adjNovelties decisionReport)
  assign subject a'
  ps <- zoom (universe . U.uNewPredictions) getPS
  when (null dObj) $
    zoom universe . U.writeToLog $ "WARNING: dObj is null"
  let x = head dObj
  let xPredicted = predict (view action r) x
  zoom universe . U.writeToLog $
    agentId a ++ " predicts " ++ show xPredicted
  let ps' = (agentId a, r, xPredicted) : ps
  zoom (universe . U.uNewPredictions) $ putPS ps'

--
-- Utility functions
--

applyPopControl :: StateT Experiment IO ()
applyPopControl = do
  deltaE <- zoom (universe . U.uPopControlDeltaE) getPS
  adjustWainEnergy subject deltaE rPopControlDeltaE "pop control"

maybeFlirt :: StateT Experiment IO ()
maybeFlirt = do
  frequency <- use (universe . U.uFlirtingFrequency)
  x <- getRandom
  when (x < frequency) flirt

flirt :: StateT Experiment IO ()
flirt = do
  a <- use subject
  b <- use other
  babyName <- zoom universe U.genName
  (a':b':_, msgs, aMatingDeltaE, bMatingDeltaE)
    <- liftIO . evalRandIO $ W.mate a b babyName
  if null msgs
    then do
      writeToLog $ agentId a ++ " and " ++ agentId b ++ " mated"
      writeToLog $ "Contribution to child: " ++
        agentId a ++ "'s share is " ++ show aMatingDeltaE ++ " " ++
        agentId b ++ "'s share is " ++ show bMatingDeltaE
      assign subject a'
      assign other b'
      recordBirths
      (summary . rMatingDeltaE) += aMatingDeltaE
      (summary . rOtherMatingDeltaE) += bMatingDeltaE
      (summary . rMateCount) += 1
    else mapM_ writeToLog msgs

recordBirths :: StateT Experiment IO ()
recordBirths = do
  a <- use subject
  (summary.rBirthCount) += length (view W.litter a)

updateChildren :: StateT Experiment IO ()
updateChildren = do
  (a:matureChildren) <- W.weanMatureChildren <$> use subject
  assign subject a
  (a':deadChildren) <- W.pruneDeadChildren <$> use subject
  assign subject a'
  assign weanlings (matureChildren ++ deadChildren)
  (summary.rWeanCount) += length matureChildren

killIfTooOld :: StateT Experiment IO ()
killIfTooOld = do
  a <- view W.age <$> use subject
  maxAge <- use (universe . U.uMaxAge)
  when (fromIntegral a > maxAge) $
    adjustWainEnergy subject (-100) rOldAgeDeltaE "old age"

adjustPopControlDeltaE
  :: [Stats.Statistic] -> StateT (U.Universe PatternWain) IO ()
adjustPopControlDeltaE xs =
  unless (null xs) $ do
    let (Just average) = Stats.lookup "avg. energy" xs
    pop <- U.popSize
    desiredPop <- use U.uInitialPopulationSize
    let c = idealPopControlDeltaE average pop desiredPop
    U.writeToLog $ "Current avg. energy = " ++ show average
    U.writeToLog $ "Adjusted pop. control Δe = " ++ show c
    zoom U.uPopControlDeltaE $ putPS c

-- TODO: Make hard-coded values configurable
idealPopControlDeltaE :: Double -> Int -> Int -> Double
idealPopControlDeltaE averageEnergy pop desiredPop
  | averageEnergy > 0.7 = 0.7 - averageEnergy
  | otherwise = averageEnergy*(p0 - p)/p
  where p0 = fromIntegral desiredPop
        p = fromIntegral pop

totalEnergy :: StateT Experiment IO (Double, Double)
totalEnergy = do
  a <- fmap uiToDouble $ view W.energy <$> use subject
  b <- fmap uiToDouble $ view W.energy <$> use other
  d <- W.childEnergy <$> use subject
  e <- W.childEnergy <$> use other
  return (a + b, d + e)

printStats
  :: [[Stats.Statistic]] -> StateT (U.Universe PatternWain) IO ()
printStats = mapM_ f
  where f xs = U.writeToLog $
                 "Summary - " ++ intercalate "," (map pretty xs)

adjustWainEnergy
  :: Lens Experiment Experiment PatternWain PatternWain -> Double
    -> Lens Summary Summary Double Double -> String
    -> StateT Experiment IO ()
adjustWainEnergy
    wainSelector deltaE statLens reason = do
  w <- use wainSelector
  let (w', used) = W.adjustEnergy deltaE w
  writeToLog $ "Adjusting energy of " ++ agentId w
    ++ " because " ++ reason ++ ": " ++ show (view W.energy w)
    ++ " " ++ show deltaE
    ++ " -> " ++ show (view W.energy w')
    ++ " used=" ++ show used ++ " leftover=" ++ show (deltaE - used)
  (summary . statLens) += used
  assign wainSelector w'

letSubjectReflect
  :: PatternWain -> Response Action -> StateT Experiment IO ()
letSubjectReflect wBefore r = do
  w <- use subject
  writeToLog $ "The action taken was " ++ pretty (view action r)
  -- Learn the outcome of the action taken
  let (rReflect, w') = W.reflect r wBefore w
  whenM (use (universe . U.uShowReflectionReport)) $ do
    writeToLog "--- begin reflection report"
    mapM_ writeToLog $ W.prettyReflectionReport w rReflect
    writeToLog "--- end reflection report"
  -- Learn the correct response
  v1 <- zoom (universe . U.uPrevVector) getPS
  let x1 = head v1
  x2 <- head <$> zoom (universe . U.uCurrVector) getPS
  let a = postdict x1 x2
  writeToLog $ "The correct action would have been " ++ pretty a
  let (rImprint, w'') = W.imprintResponse (view labels r) a w'
  whenM (use (universe . U.uShowImprintReport)) $ do
    writeToLog "--- begin imprint report"
    mapM_ writeToLog $ W.prettyResponseImprintReport w rImprint
    writeToLog "--- end imprint report"
  -- Update the stats
  assign subject w''
  assign (summary . rRewardPredictionErr) $ W.happinessError rReflect

writeRawStats
  :: String -> FilePath -> [Stats.Statistic]
    -> StateT (U.Universe PatternWain) IO ()
writeRawStats n f xs = do
  liftIO $ createDirectoryIfMissing True (dropFileName f)
  t <- U.currentTime
  liftIO . appendFile f $
    "time=" ++ show t ++ ",agent=" ++ n ++ ',':raw xs ++ "\n"

reportAnyDeaths
  :: [PatternWain] -> StateT (U.Universe PatternWain) IO ()
reportAnyDeaths ws = mapM_ f ws
  where f w = when (not . isAlive $ w) $
                U.writeToLog
                  (agentId w ++ " dead at age " ++ show (view W.age w))

mean :: (Eq a, Fractional a, Foldable t) => t a -> a
mean xs
  | count == 0 = error "no data"
  | otherwise = total / count
  where (total, count) = foldr f (0, 0) xs
        f x (y, n) = (y+x, n+1)
