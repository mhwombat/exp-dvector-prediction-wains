------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Prediction.Object
-- Copyright   :  (c) Amy de Buitléir 2017
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for working with objects that could be either wains or
-- vectors.
--
------------------------------------------------------------------------
{-# LANGUAGE Rank2Types #-}
module ALife.Creatur.Wain.Object
  (
    Object(..),
    isImage,
    objectId,
    objectNum,
    objectAppearance,
    addIfWain,
    objectToWain
  ) where

import ALife.Creatur (agentId)
import qualified ALife.Creatur.Wain as W
import ALife.Creatur.Wain.Image (Image)
import ALife.Creatur.Wain.ImageTweaker (ImageTweaker(..))
import Control.Lens

data Object a = IObject [Double] String
              | AObject (W.Wain [Double] ImageTweaker a)

isImage :: Object a -> Bool
isImage (IObject _ _) = True
isImage (AObject _) = False

objectId :: Object a -> String
objectId (IObject _ s) = "Image " ++ s
objectId (AObject a) = agentId a

objectNum :: Object a -> Int
objectNum (IObject _ _) = error "empty vector"
objectNum (IObject _ s) = read [head s]
objectNum (AObject _) = 10

objectAppearance :: Object a -> Image
objectAppearance (IObject img _) = img
objectAppearance (AObject a) = view W.appearance a

-- objectEnergy :: Object a -> Double
-- objectEnergy (IObject _ _) = 0
-- objectEnergy (AObject a) = view W.energy a

-- objectChildEnergy :: Object a -> Double
-- objectChildEnergy (IObject _ _) = 0
-- objectChildEnergy (AObject a) = W.childEnergy a

addIfWain
  :: Object a -> [W.Wain [Double] ImageTweaker a]
    -> [W.Wain Image ImageTweaker a]
addIfWain (IObject _ _) xs = xs
addIfWain (AObject a) xs = a:xs

objectToWain :: Object a -> W.Wain Image ImageTweaker a
objectToWain (IObject _ _) = error "image, not wain"
objectToWain (AObject a) = a
