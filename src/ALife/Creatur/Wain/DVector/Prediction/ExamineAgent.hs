------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Prediction.ExamineAgent
-- Copyright   :  (c) Amy de Buitléir 2017
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
import ALife.Creatur.Wain.DVector.ExamineAgent (fetchWains, examine)
import System.Environment

main :: IO ()
main = do
  (f:_) <- getArgs
  ws <- fetchWains f :: IO [PatternWain]
  mapM_ examine ws
