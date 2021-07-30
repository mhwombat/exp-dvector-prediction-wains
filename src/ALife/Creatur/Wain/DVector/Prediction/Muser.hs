------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Prediction.Muser
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- The muser takes classification information (labels and
-- probabilities) about the objects in a wain's field of "view",
-- and determines the most likely scenarios that the wain could be
-- facing.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.DVector.Prediction.Muser
  (
    DMuser,
    depth,
    width,
    makeMuser,
    generateResponses,
    defaultOutcomes
  ) where

import           ALife.Creatur.Wain.DVector.Prediction.MuserInternal
