------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Prediction.ActionInternal
-- Copyright   :  (c) 2012-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ?????
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ALife.Creatur.Wain.DVector.Prediction.ActionInternal where

import           ALife.Creatur.Genetics.BRGCWord8
    (Genetic)
import           ALife.Creatur.Genetics.Diploid
    (Diploid)
import           ALife.Creatur.Wain.DVector.Double
    (diff, makeSimilar, sanitise)
import           ALife.Creatur.Wain.GeneticSOM
    (Difference)
import           ALife.Creatur.Wain.Pretty
    (Pretty)
import           ALife.Creatur.Wain.Statistics
    (Statistical (..), dStat, popStdDev)
import           ALife.Creatur.Wain.UnitInterval
    (UIDouble, doubleToUI)
import           Control.DeepSeq
    (NFData)
import           Data.List
    (nub, sort)
import           Data.Serialize
    (Serialize)
import           GHC.Generics
    (Generic)
import           System.Random
    (Random, random, randomR)

data Action = Add Double
  deriving (Show, Read, Eq, Ord, Generic, NFData)

mkAction :: Double -> Action
mkAction = Add . sanitise

actionToDouble :: Action -> Double
actionToDouble (Add x) = x

instance Serialize Action
instance Genetic Action
instance Diploid Action
instance Pretty Action

instance Random Action where
  randomR (Add x, Add y) g = (Add z, g')
    where (z, g') = randomR (x, y) g
  random g = (Add z, g')
    where (z, g') = random g

instance Statistical Action where
  stats a = [dStat "action" . actionToDouble $ a]

predict :: Action -> Double -> Double
predict (Add z) x = sanitise $ x + z

postdict :: Double -> Double -> Action
postdict x1 x2 = Add z
  where z = sanitise (x2 - x1)

actionDiff :: Action -> Action -> Difference
actionDiff (Add x) (Add y) = doubleToUI $ diff x y

makeActionSimilar :: Action -> UIDouble -> Action -> Action
makeActionSimilar (Add x) r (Add y)
  = Add $ makeSimilar x r y

expandActionList :: Double -> [Action] -> [Action]
expandActionList f as = map mkAction $ nub (es ++ ms ++ xs)
  where xs = sort $ map actionToDouble as :: [Double]
        ms = midpoints xs :: [Double]
        es = extendBoundaries f xs :: [Double]

-- Don't need to sanitise values here, mkAction will do it
extendBoundaries :: Double -> [Double] -> [Double]
extendBoundaries _ [] = [1]
extendBoundaries f (x:[])
  = [max (-1) (x - f*(abs x)), min 1 (x + f*(abs x))]
extendBoundaries f xs
  = [max (-1) (head xs - f*sd), min 1 (last xs + f*sd)]
  where sd = popStdDev xs

-- Don't need to sanitise values here, mkAction will do it
midpoints :: [Double] -> [Double]
midpoints (x:y:xs) = ((x + y)/2) : midpoints xs
midpoints _        = []
