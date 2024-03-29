------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2017-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- The daemon that runs the Créatúr experiment.
--
------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import ALife.Creatur.Daemon (Job(..), launchInteractive)
import ALife.Creatur.Task (runInteractingAgents, simpleJob)
import ALife.Creatur.Wain.DVector.Prediction.Experiment (PatternWain,
  run, startRound, finishRound, versionInfo)
import ALife.Creatur.Wain.DVector.Prediction.Universe (Universe(..),
  writeToLog, loadUniverse, uSleepBetweenTasks)
import Control.Concurrent (MVar, newMVar, readMVar, swapMVar)
import Control.Lens
import Control.Monad (unless)
import Control.Monad.State (execStateT)
import System.IO.Unsafe (unsafePerformIO)

shutdownMessagePrinted :: MVar Bool
{-# NOINLINE shutdownMessagePrinted #-}
shutdownMessagePrinted = unsafePerformIO (newMVar False)

startupHandler
  :: String -> Universe PatternWain -> IO (Universe PatternWain)
startupHandler programName
  = execStateT (writeToLog $ "Starting " ++ programName)

shutdownHandler :: String -> Universe PatternWain -> IO ()
shutdownHandler programName u = do
  -- Only print the message once
  handled <- readMVar shutdownMessagePrinted
  unless handled $ do
    _ <- execStateT (writeToLog $ "Shutdown requested for "
                      ++ programName) u
    _ <- swapMVar shutdownMessagePrinted True
    return ()

main :: IO ()
main = do
  u <- loadUniverse
  let program = run
  let message = versionInfo ++ ", configuration=" ++ show u
  let j = simpleJob
        { task=runInteractingAgents program startRound finishRound,
          onStartup=startupHandler message,
          onShutdown=shutdownHandler message,
          sleepTime=view uSleepBetweenTasks u }
  launchInteractive j u
