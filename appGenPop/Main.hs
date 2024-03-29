------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ???
--
------------------------------------------------------------------------
module Main where

{-# LANGUAGE TypeFamilies #-}

import ALife.Creatur (agentId)
import ALife.Creatur.Wain.DVector.Prediction.Experiment (PatternWain,
  randomPatternWain, printStats)
import ALife.Creatur.Wain (adjustEnergy)
import ALife.Creatur.Wain.Pretty (pretty)
import ALife.Creatur.Wain.PersistentStatistics (clearStats)
import ALife.Creatur.Wain.Statistics (Statistic, stats, summarise)
import ALife.Creatur.Wain.DVector.Prediction.Universe (Universe(..),
  writeToLog, store, loadUniverse, uClassifierSizeRange,
  uPredictorSizeRange, uInitialPopulationSize, uInitialEnergy,
  uStatsFile)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random (evalRandIO)
import Control.Monad.Random.Class (getRandomR)
import Control.Monad.State.Lazy (StateT, evalStateT, get)

introduceRandomAgent
  :: String -> StateT (Universe PatternWain) IO [Statistic]
introduceRandomAgent name = do
  u <- get
  classifierSize
    <- liftIO . evalRandIO . getRandomR . view uClassifierSizeRange $ u
  predictorSize
    <- liftIO . evalRandIO . getRandomR . view uPredictorSizeRange $ u
  agent
    <- liftIO . evalRandIO $
        randomPatternWain name u classifierSize predictorSize
  let e = view uInitialEnergy u
  let (agent', _) = adjustEnergy e agent
  writeToLog $ "GeneratePopulation: Created " ++ agentId agent'
  writeToLog $ "GeneratePopulation: Stats " ++ pretty (stats agent')
  store agent'
  return (stats agent')

introduceRandomAgents
  :: [String] -> StateT (Universe PatternWain) IO ()
introduceRandomAgents ns = do
  xs <- mapM introduceRandomAgent ns
  let yss = summarise xs
  printStats yss
  statsFile <- use uStatsFile
  clearStats statsFile

main :: IO ()
main = do
  u <- loadUniverse
  let ns = map (("Founder" ++) . show) [1..(view uInitialPopulationSize u)]
  print ns
  evalStateT (introduceRandomAgents ns) u
