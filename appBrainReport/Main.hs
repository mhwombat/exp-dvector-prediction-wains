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
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           ALife.Creatur                                    (agentId)
import qualified ALife.Creatur.Gene.Numeric.PlusMinusOne          as PM1
import           ALife.Creatur.Wain                               (Wain, brain)
import           ALife.Creatur.Wain.Brain                         (classifier,
                                                                   predictor)
import           ALife.Creatur.Wain.DVector.Pattern               ()
import           ALife.Creatur.Wain.DVector.Prediction.Experiment
import           ALife.Creatur.Wain.ExamineAgent                  (fetchObjects)
import           ALife.Creatur.Wain.GeneticSOM                    (Label)
import           ALife.Creatur.Wain.Pretty                        (Pretty (..))
import           ALife.Creatur.Wain.Response                      (Response,
                                                                   action,
                                                                   labels,
                                                                   outcomes)
import qualified Data.Datamining.Clustering.SGM4                  as SOM
import           Data.List                                        (intercalate)
import qualified Data.Map.Strict                                  as M
import           System.Environment
import           Text.Printf                                      (printf)

main :: IO ()
main = do
  (f:_) <- getArgs
  ws <- fetchObjects f :: IO [PatternWain]
  mapM_ report ws

report
  :: (SOM.Adjuster ct, SOM.Adjuster pt, Pretty p, Pretty a, Pretty ct, Pretty pt)
    => Wain ct pt p a m -> IO ()
report a = do
  putStrLn $ "Classifier tweaker: " ++ pretty (SOM.adjuster . classifier . brain $ a)
  putStrLn $ "Predictor counts: " ++ pretty (SOM.counterMap . predictor . brain $ a)
  mapM_ putStrLn $ describePredictorModels a

describePredictorModels
  :: (Pretty p, Pretty a)
    => Wain ct pt p a m -> [String]
describePredictorModels w = map f rs
  where rs = M.toList . SOM.modelMap .predictor . brain $ w
        f (l, r) = agentId w ++ "'s predictor model "
                     ++ pretty l ++ ": " ++ describePredictorModel r cMap
        cMap = SOM.modelMap . classifier . brain $ w

describePredictorModel
  :: (Pretty p, Pretty a)
    => Response a -> M.Map Label p -> String
describePredictorModel r cMap =
  intercalate "," (map (\l -> pretty (forceLookup l cMap)) ls) ++ '|':pretty a ++ '|':format os
    where ls = labels r
          a = action r
          os = outcomes r
          format xs =  intercalate "|" . map (printf "%.3f" .  PM1.wide) $ xs

forceLookup :: Ord p => p -> M.Map p a -> a
forceLookup k m = v
  where (Just v) = M.lookup k m
