------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Prediction.Action
-- Copyright   :  (c) Amy de Buitléir 2017
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- ?????
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
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

import ALife.Creatur.Genetics.BRGCWord8 (Genetic)
import ALife.Creatur.Genetics.Diploid (Diploid)
import ALife.Creatur.Wain.DVector.Double (sanitise, diff, makeSimilar)
import ALife.Creatur.Wain.Pretty (Pretty)
import ALife.Creatur.Wain.GeneticSOM (Difference)
import ALife.Creatur.Wain.Statistics (popStdDev)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Data.List (sort, nub)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.Random (Random, random, randomR)

data Action = Add Double
  deriving (Show, Read, Eq, Ord, Generic)

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

predict :: Action -> Double -> Double
predict (Add z) x = sanitise $ x + z

postdict :: Double -> Double -> Action
postdict x1 x2 = Add z
  where z = sanitise (x2 - x1)

actionDiff :: Action -> Action -> Difference
actionDiff (Add x) (Add y) = diff x y

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
extendBoundaries f (x:[]) = [x/f, f*x]
extendBoundaries f xs = [head xs - f*sd, last xs + f*sd]
  where sd = popStdDev xs

-- Don't need to sanitise values here, mkAction will do it
midpoints :: [Double] -> [Double]
midpoints (x:y:xs) = ((x + y)/2) : midpoints xs
midpoints _ = []
