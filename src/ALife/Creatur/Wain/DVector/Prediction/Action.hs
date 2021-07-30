------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Prediction.Action
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ?????
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.DVector.Prediction.Action
  (
    Action,
    mkAction,
    actionToDouble,
    predict,
    postdict,
    actionDiff,
    makeActionSimilar,
    expandActionList
  ) where

import           ALife.Creatur.Wain.DVector.Prediction.ActionInternal
