------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2017 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Runs the QuickCheck tests.
--
------------------------------------------------------------------------
import           ALife.Creatur.Wain.DVector.Prediction.ActionQC
    (test)
import           ALife.Creatur.Wain.DVector.Prediction.ExperimentQC
    (test)

import           Test.Framework                                     as TF
    (Test, defaultMain)

tests :: [TF.Test]
tests =
  [
    -- In increasing order of complexity
    ALife.Creatur.Wain.DVector.Prediction.ActionQC.test,
    ALife.Creatur.Wain.DVector.Prediction.ExperimentQC.test
  ]

main :: IO ()
main = defaultMain tests
