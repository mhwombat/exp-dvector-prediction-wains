------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Amy de Buitléir 2017
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Runs the QuickCheck tests.
--
------------------------------------------------------------------------
module Main where

import ALife.Creatur.Wain.DVector.Prediction.ActionQC (test)
import ALife.Creatur.Wain.DVector.Prediction.ExperimentQC (test)

import Test.Framework as TF (defaultMain, Test)

tests :: [TF.Test]
tests = 
  [
    -- In increasing order of complexity
    ALife.Creatur.Wain.DVector.Prediction.ActionQC.test,
    ALife.Creatur.Wain.DVector.Prediction.ExperimentQC.test
  ]

main :: IO ()
main = defaultMain tests
