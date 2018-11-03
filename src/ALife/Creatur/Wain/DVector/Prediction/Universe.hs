------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Prediction.Universe
-- Copyright   :  (c) Amy de Buitléir 2012-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Universe for image mining agents
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module ALife.Creatur.Wain.DVector.Prediction.Universe
  (
    -- * Constructors
    Universe(..),
    loadUniverse,
    U.Agent,
    -- * Lenses
    uExperimentName,
    uClock,
    uLogger,
    uDB,
    uNamer,
    uChecklist,
    uStatsFile,
    uRawStatsFile,
    uDataSource,
    uShowClassifierModels,
    uShowPredictorModels,
    uShowClassificationReport,
    uShowScenarioReport,
    uShowPredictionReport,
    uShowActionReport,
    uShowReflectionReport,
    uShowImprintReport,
    uSleepBetweenTasks,
    uVectorLength,
    uClassifierSizeRange,
    uPredictorSizeRange,
    uDevotionRange,
    uMaturityRange,
    uMaxAge,
    uInitialPopulationSize,
    uInitialEnergy,
    uAllowedPopulationRange,
    uPopControl,
    uMeanAccuracyDeltaE,
    uMaxAccuracyDeltaE,
    uBaseMetabolismDeltaE,
    uAdjustableMetabolismDeltaE,
    uChildCostFactor,
    uFlirtingFrequency,
    uPopControlDeltaE,
    uClassifierR0Range,
    uClassifierRfRange,
    uClassifierTfRange,
    uPredictorR0Range,
    uPredictorRfRange,
    uPredictorTfRange,
    uDefaultOutcomeRange,
    uStrictnessRange,
    uImprintOutcomeRange,
    uReinforcementDeltasRange,
    uDepthRange,
    uWidthRange,
    uCheckpoints,
    uCurrVector,
    uPrevVector,
    uNewPredictions,
    uPrevPredictions,
    uMeanError,
    uMinError,
    uPrevMetabMetrics,
    uCurrMetabMetrics,
    uPrevMeanMetabMetric,
    -- * Other
    U.agentIds,
    U.currentTime,
    U.genName,
    U.getAgent,
    U.popSize,
    U.store,
    U.writeToLog
  ) where

import qualified ALife.Creatur as A
import qualified ALife.Creatur.Namer as N
import qualified ALife.Creatur.Checklist as CL
import qualified ALife.Creatur.Counter as K
import qualified ALife.Creatur.Database as D
import qualified ALife.Creatur.Database.CachedFileSystem as CFS
import qualified ALife.Creatur.Logger.SimpleLogger as SL
import ALife.Creatur (AgentId)
import ALife.Creatur.Persistent (Persistent, mkPersistent)
import qualified ALife.Creatur.Universe as U
import qualified ALife.Creatur.Wain.Checkpoint as CP
import ALife.Creatur.Wain.DVector.Prediction.Action (Action)
import ALife.Creatur.Wain.DVector.Prediction.DataSource (DataSource,
  mkDataSource)
import ALife.Creatur.Wain.Response (Response)
import ALife.Creatur.Wain.PlusMinusOne (PM1Double)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Control.Exception (SomeException, try)
import Control.Lens hiding (Setting)
import Data.AppSettings (Setting(..), GetSetting(..),
  FileLocation(Path), readSettings)
import Data.Word (Word8, Word16, Word64)
import System.Directory (makeRelativeToCurrentDirectory)

data Universe a = Universe
  {
    _uExperimentName :: String,
    _uClock :: K.PersistentCounter,
    _uLogger :: SL.SimpleLogger,
    _uDB :: CFS.CachedFSDatabase a,
    _uNamer :: N.SimpleNamer,
    _uChecklist :: CL.PersistentChecklist,
    _uStatsFile :: FilePath,
    _uRawStatsFile :: FilePath,
    _uDataSource :: DataSource,
    _uShowClassifierModels :: Bool,
    _uShowPredictorModels :: Bool,
    _uShowClassificationReport :: Bool,
    _uShowScenarioReport :: Bool,
    _uShowPredictionReport :: Bool,
    _uShowActionReport :: Bool,
    _uShowReflectionReport :: Bool,
    _uShowImprintReport :: Bool,
    _uSleepBetweenTasks :: Int,
    _uVectorLength :: Int,
    _uClassifierSizeRange :: (Word64, Word64),
    _uPredictorSizeRange :: (Word64, Word64),
    _uDevotionRange :: (UIDouble, UIDouble),
    _uMaturityRange :: (Word16, Word16),
    _uMaxAge :: Int,
    _uInitialPopulationSize :: Int,
    _uInitialEnergy :: Double,
    _uAllowedPopulationRange :: (Int, Int),
    _uPopControl :: Bool,
    _uMeanAccuracyDeltaE :: Double,
    _uMaxAccuracyDeltaE :: Double,
    _uBaseMetabolismDeltaE :: Double,
    _uAdjustableMetabolismDeltaE :: Double,
    _uChildCostFactor :: Double,
    _uFlirtingFrequency :: UIDouble,
    _uPopControlDeltaE :: Persistent Double,
    _uClassifierR0Range :: (UIDouble, UIDouble),
    _uClassifierRfRange :: (UIDouble, UIDouble),
    _uClassifierTfRange :: (Word64, Word64),
    _uPredictorR0Range :: (UIDouble, UIDouble),
    _uPredictorRfRange :: (UIDouble, UIDouble),
    _uPredictorTfRange :: (Word64, Word64),
    _uDefaultOutcomeRange :: (PM1Double, PM1Double),
    _uStrictnessRange :: (Word64, Word64),
    _uImprintOutcomeRange :: (PM1Double, PM1Double),
    _uReinforcementDeltasRange :: (PM1Double, PM1Double),
    _uDepthRange :: (Word8, Word8),
    _uWidthRange :: (Double, Double),
    _uCheckpoints :: [CP.Checkpoint],
    _uCurrVector :: Persistent [Double],
    _uPrevVector :: Persistent [Double],
    _uPrevPredictions :: Persistent [(AgentId, Response Action, Double)],
    _uMeanError :: Persistent Double,
    _uMinError :: Persistent Double,
    _uNewPredictions :: Persistent [(AgentId, Response Action, Double)],
    _uPrevMetabMetrics :: Persistent [(AgentId, Double)],
    _uCurrMetabMetrics :: Persistent [(AgentId, Double)],
    _uPrevMeanMetabMetric :: Persistent Double
  } deriving Show
makeLenses ''Universe

instance (A.Agent a, D.SizedRecord a) => U.Universe (Universe a) where
  type Agent (Universe a) = a
  type Clock (Universe a) = K.PersistentCounter
  clock = _uClock
  setClock u c = u { _uClock=c }
  type Logger (Universe a) = SL.SimpleLogger
  logger = _uLogger
  setLogger u l = u { _uLogger=l }
  type AgentDB (Universe a) = CFS.CachedFSDatabase a
  agentDB = _uDB
  setAgentDB u d = u { _uDB=d }
  type Namer (Universe a) = N.SimpleNamer
  agentNamer = _uNamer
  setNamer u n = u { _uNamer=n }
  type Checklist (Universe a) = CL.PersistentChecklist
  checklist = _uChecklist
  setChecklist u cl = u { _uChecklist=cl }

requiredSetting :: String -> Setting a
requiredSetting key
  = Setting key (error $ key ++ " not defined in configuration")

cExperimentName :: Setting String
cExperimentName = requiredSetting "experimentName"

cWorkingDir :: Setting FilePath
cWorkingDir = requiredSetting "workingDir"

cDataFile :: Setting FilePath
cDataFile = requiredSetting "dataFile"

cCacheSize :: Setting Int
cCacheSize = requiredSetting "cacheSize"

cShowClassifierModels :: Setting Bool
cShowClassifierModels = requiredSetting "showClassifierModels"

cShowPredictorModels :: Setting Bool
cShowPredictorModels = requiredSetting "showPredictorModels"

cShowClassificationReport :: Setting Bool
cShowClassificationReport = requiredSetting "showClassificationReport"

cShowScenarioReport :: Setting Bool
cShowScenarioReport = requiredSetting "showScenarioReport"

cShowPredictionReport :: Setting Bool
cShowPredictionReport = requiredSetting "showPredictionReport"

cShowActionReport :: Setting Bool
cShowActionReport = requiredSetting "showActionReport"

cShowReflectionReport :: Setting Bool
cShowReflectionReport = requiredSetting "showReflectionReport"

cShowImprintReport :: Setting Bool
cShowImprintReport = requiredSetting "showImprintReport"

cSleepBetweenTasks :: Setting Int
cSleepBetweenTasks = requiredSetting "sleepTimeBetweenTasks"

cVectorLength :: Setting Int
cVectorLength = requiredSetting "vectorLength"

cClassifierSizeRange :: Setting (Word64, Word64)
cClassifierSizeRange
  = requiredSetting "classifierSizeRange"

cPredictorSizeRange :: Setting (Word64, Word64)
cPredictorSizeRange
  = requiredSetting "predictorSizeRange"

cDevotionRange :: Setting (UIDouble, UIDouble)
cDevotionRange = requiredSetting "devotionRange"

cMaturityRange :: Setting (Word16, Word16)
cMaturityRange = requiredSetting "maturityRange"

cMaxAge :: Setting Int
cMaxAge = requiredSetting "maxAge"

cInitialPopulationSize :: Setting Int
cInitialPopulationSize = requiredSetting "initialPopSize"

cInitialEnergy :: Setting Double
cInitialEnergy = requiredSetting "initialEnergy"

cAllowedPopulationRange :: Setting (Double, Double)
cAllowedPopulationRange = requiredSetting "allowedPopRange"

cPopControl :: Setting Bool
cPopControl = requiredSetting "popControl"

cMeanAccuracyDeltaE :: Setting Double
cMeanAccuracyDeltaE = requiredSetting "meanAccuracyDeltaE"

cMaxAccuracyDeltaE :: Setting Double
cMaxAccuracyDeltaE = requiredSetting "maxAccuracyDeltaE"

cBaseMetabolismDeltaE :: Setting Double
cBaseMetabolismDeltaE = requiredSetting "baseMetabDeltaE"

cAdjustableMetabolismDeltaE :: Setting Double
cAdjustableMetabolismDeltaE
  = requiredSetting "adjustableMetabolismDeltaE"

cChildCostFactor :: Setting Double
cChildCostFactor = requiredSetting "childCostFactor"

cFlirtingFrequency :: Setting UIDouble
cFlirtingFrequency = requiredSetting "flirtingFrequency"

cClassifierR0Range :: Setting (UIDouble, UIDouble)
cClassifierR0Range = requiredSetting "classifierR0Range"

cClassifierRfRange :: Setting (UIDouble, UIDouble)
cClassifierRfRange = requiredSetting "classifierRfRange"

cClassifierTfRange :: Setting (Word64, Word64)
cClassifierTfRange = requiredSetting "classifierTfRange"

cPredictorR0Range :: Setting (UIDouble, UIDouble)
cPredictorR0Range = requiredSetting "predictorR0Range"

cPredictorRfRange :: Setting (UIDouble, UIDouble)
cPredictorRfRange = requiredSetting "predictorRfRange"

cPredictorTfRange :: Setting (Word64, Word64)
cPredictorTfRange = requiredSetting "predictorTfRange"

cDefaultOutcomeRange :: Setting (PM1Double, PM1Double)
cDefaultOutcomeRange = requiredSetting "defaultOutcomeRange"

cStrictnessRange :: Setting (Word64, Word64)
cStrictnessRange = requiredSetting "strictnessRange"

cImprintOutcomeRange :: Setting (PM1Double, PM1Double)
cImprintOutcomeRange = requiredSetting "imprintOutcomeRange"

cReinforcementDeltasRange :: Setting (PM1Double, PM1Double)
cReinforcementDeltasRange = requiredSetting "reinforcementDeltasRange"

cDepthRange :: Setting (Word8, Word8)
cDepthRange = requiredSetting "depthRange"

cWidthRange :: Setting (Double, Double)
cWidthRange = requiredSetting "widthRange"

cCheckpoints :: Setting [CP.Checkpoint]
cCheckpoints = requiredSetting "checkpoints"

loadUniverse :: IO (Universe a)
loadUniverse = do
  configFile <- Path <$> makeRelativeToCurrentDirectory "wain.config"
  readResult <- try $ readSettings configFile
  case readResult of
    Right (_, GetSetting getSetting) ->
      return $ config2Universe getSetting
    Left (x :: SomeException) ->
      error $ "Error reading the config file: " ++ show x

config2Universe :: (forall a. Read a => Setting a -> a) -> Universe b
config2Universe getSetting =
  Universe
    {
      _uExperimentName = en,
      _uClock = K.mkPersistentCounter (workDir ++ "/clock"),
      _uLogger = SL.mkSimpleLogger (workDir ++ "/log/" ++ en ++ ".log"),
      _uDB = CFS.mkCachedFSDatabase (workDir ++ "/db")
               (getSetting cCacheSize),
      _uNamer = N.mkSimpleNamer (en ++ "_") (workDir ++ "/namer"),
      _uChecklist = CL.mkPersistentChecklist (workDir ++ "/todo"),
      _uStatsFile = workDir ++ "/statsFile",
      _uRawStatsFile = workDir ++ "/rawStatsFile",
      _uDataSource = mkDataSource dataFile readCounterFile,
      _uShowClassifierModels = getSetting cShowClassifierModels,
      _uShowPredictorModels = getSetting cShowPredictorModels,
      _uShowClassificationReport = getSetting cShowClassificationReport,
      _uShowScenarioReport = getSetting cShowScenarioReport,
      _uShowPredictionReport = getSetting cShowPredictionReport,
      _uShowActionReport = getSetting cShowActionReport,
      _uShowReflectionReport = getSetting cShowReflectionReport,
      _uShowImprintReport = getSetting cShowImprintReport,
      _uSleepBetweenTasks = getSetting cSleepBetweenTasks,
      _uVectorLength = n,
      _uClassifierSizeRange = getSetting cClassifierSizeRange,
      _uPredictorSizeRange = getSetting cPredictorSizeRange,
      _uDevotionRange = getSetting cDevotionRange,
      _uMaturityRange = getSetting cMaturityRange,
      _uMaxAge = getSetting cMaxAge,
      _uInitialPopulationSize = p0,
      _uInitialEnergy = e0,
      _uAllowedPopulationRange = (a', b'),
      _uPopControl = getSetting cPopControl,
      _uMeanAccuracyDeltaE = getSetting cMeanAccuracyDeltaE,
      _uMaxAccuracyDeltaE = getSetting cMaxAccuracyDeltaE,
      _uBaseMetabolismDeltaE = getSetting cBaseMetabolismDeltaE,
      _uAdjustableMetabolismDeltaE
        = getSetting cAdjustableMetabolismDeltaE,
      _uChildCostFactor = getSetting cChildCostFactor,
      _uFlirtingFrequency = getSetting cFlirtingFrequency,
      _uPopControlDeltaE
        = mkPersistent 0 (workDir ++ "/popControlDeltaE"),
      _uClassifierR0Range = getSetting cClassifierR0Range,
      _uClassifierRfRange = getSetting cClassifierRfRange,
      _uClassifierTfRange = getSetting cClassifierTfRange,
      _uPredictorR0Range = getSetting cPredictorR0Range,
      _uPredictorRfRange = getSetting cPredictorRfRange,
      _uPredictorTfRange = getSetting cPredictorTfRange,
      _uDefaultOutcomeRange = getSetting cDefaultOutcomeRange,
      _uStrictnessRange = getSetting cStrictnessRange,
      _uImprintOutcomeRange = getSetting cImprintOutcomeRange,
      _uReinforcementDeltasRange = getSetting cReinforcementDeltasRange,
      _uDepthRange = getSetting cDepthRange,
      _uWidthRange = getSetting cWidthRange,
      _uCheckpoints = getSetting cCheckpoints,
      _uCurrVector = mkPersistent zeroes (workDir ++ "/currVector"),
      _uPrevVector = mkPersistent zeroes (workDir ++ "/prevVector"),
      _uNewPredictions = mkPersistent [] (workDir ++ "/newPredictions"),
      _uPrevPredictions
        = mkPersistent [] (workDir ++ "/prevPredictions"),
      _uMeanError = mkPersistent 0 (workDir ++ "/meanError"),
      _uMinError = mkPersistent 0 (workDir ++ "/minError"),
      _uPrevMetabMetrics
        = mkPersistent [] (workDir ++ "/prevMetabMetrics"),
      _uCurrMetabMetrics
        = mkPersistent [] (workDir ++ "/currMetabMetrics"),
      _uPrevMeanMetabMetric
        = mkPersistent 1 (workDir ++ "/prevMeanMetabMetric")
    }
  where en = getSetting cExperimentName
        workDir = getSetting cWorkingDir
        e0 = getSetting cInitialEnergy
        p0 = getSetting cInitialPopulationSize
        (a, b) = getSetting cAllowedPopulationRange
        a' = round (fromIntegral p0 * a)
        b' = round (fromIntegral p0 * b)
        n = getSetting cVectorLength
        zeroes = replicate n 0
        dataFile = getSetting cDataFile
        readCounterFile = workDir ++ "/readCounter"
