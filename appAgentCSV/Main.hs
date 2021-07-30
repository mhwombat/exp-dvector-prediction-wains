------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Do a full analysis of a wain and generate a report.
--
------------------------------------------------------------------------
module Main where

import ALife.Creatur.Wain.DVector.Prediction.Experiment
import ALife.Creatur.Wain.AgentCSV (fetchObjects, agentToCSV)
import System.Environment

main :: IO ()
main = do
  (f:_) <- getArgs
  ws <- fetchObjects f :: IO [PatternWain]
  mapM_ agentToCSV ws
