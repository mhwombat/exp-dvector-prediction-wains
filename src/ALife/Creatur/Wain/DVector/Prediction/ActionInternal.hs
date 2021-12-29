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
{-# LANGUAGE TypeFamilies   #-}
module ALife.Creatur.Wain.DVector.Prediction.ActionInternal where

import qualified ALife.Creatur.Gene.Numeric.PlusMinusOne as PM1
import qualified ALife.Creatur.Gene.Numeric.UnitInterval as UI
import           ALife.Creatur.Gene.Numeric.Weights      (Weights, weightedSum)
import qualified ALife.Creatur.Genetics.BRGCWord8        as G
import           ALife.Creatur.Genetics.Diploid          (Diploid)
import           ALife.Creatur.Wain.LearningParams       (LearningParams,
                                                          toLearningFunction)
import           ALife.Creatur.Wain.Pretty               (Pretty)
import           ALife.Creatur.Wain.Report               (Report, report)
import           ALife.Creatur.Wain.Response             (Response (..), action,
                                                          labelSimilarity,
                                                          labels, outcomes)
import           ALife.Creatur.Wain.Statistics           (Statistical (..),
                                                          dStat, popStdDev)
import           Control.DeepSeq                         (NFData)
import qualified Data.Datamining.Clustering.SGM4         as SOM
import qualified Data.Datamining.Pattern.List            as L
import qualified Data.Datamining.Pattern.Numeric         as N
import           Data.List                               (nub, sort)
import           Data.Serialize                          (Serialize)
import           Data.Word                               (Word32)
import           GHC.Generics                            (Generic)
import           System.Random                           (Random, random,
                                                          randomR)
import           Test.QuickCheck                         (Arbitrary, arbitrary)

newtype Action = Add Double
  deriving (Show, Read, Eq, Ord, Pretty, Serialize, G.Genetic, Diploid,
            Generic, NFData)

mkAction :: Double -> Action
mkAction = Add . sanitise

sanitise :: Double -> Double
sanitise = max N.minDouble . min N.maxDouble

actionToDouble :: Action -> Double
actionToDouble (Add x) = x

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

actionDiff :: Action -> Action -> UI.UIDouble
actionDiff (Add x) (Add y) = UI.narrow $ N.diff x y

makeActionSimilar :: Action -> UI.UIDouble -> Action -> Action
makeActionSimilar (Add x) r (Add y)
  = Add $ N.makeSimilar x (UI.wide r) y

expandActionList :: Double -> [Action] -> [Action]
expandActionList f as = map mkAction $ nub (es ++ ms ++ xs)
  where xs = sort $ map actionToDouble as :: [Double]
        ms = midpoints xs :: [Double]
        es = extendBoundaries f xs :: [Double]

-- Don't need to sanitise values here, mkAction will do it
extendBoundaries :: Double -> [Double] -> [Double]
extendBoundaries _ [] = [1]
extendBoundaries f [x]
  = [max (-1) (x - f * abs x), min 1 (x + f * abs x)]
extendBoundaries f xs
  = [max (-1) (head xs - f*sd), min 1 (last xs + f*sd)]
  where sd = popStdDev xs

-- Don't need to sanitise values here, mkAction will do it
midpoints :: [Double] -> [Double]
midpoints (x:y:xs) = ((x + y)/2) : midpoints xs
midpoints _        = []


data ResponseAdjuster = ResponseAdjuster LearningParams Weights
  deriving (Show, Read, Eq, Pretty, Serialize, G.Genetic, Diploid,
            Generic, NFData)

instance SOM.Adjuster ResponseAdjuster where
  type TimeType ResponseAdjuster = Word32
  type MetricType ResponseAdjuster = UI.UIDouble
  type PatternType ResponseAdjuster = Response Action
  learningRate (ResponseAdjuster l _) = toLearningFunction l
  difference (ResponseAdjuster _ ws) x y = weightedSum ws [d1, d2]
    where d1 = actionDiff (action x) (action y)
          d2 = 1 - labelSimilarity (labels x) (labels y)
  makeSimilar _ target r x = Response s a o
    where s = labels x -- never change this
          a = makeActionSimilar (action target) r (action x)
          o = L.makeSimilar PM1.makeSimilar (outcomes target) r (outcomes x)

instance Statistical ResponseAdjuster where
  stats (ResponseAdjuster l _) = stats l

instance Report ResponseAdjuster where
  report (ResponseAdjuster l _) = report l

instance Arbitrary ResponseAdjuster where
  arbitrary = ResponseAdjuster <$> arbitrary <*> arbitrary
