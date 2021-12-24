------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Prediction.MuserQC
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.DVector.Prediction.MuserQC
  (
    test,
    equivMuser,
    sizedArbMuser
  ) where

import qualified ALife.Creatur.Gene.Numeric.PlusMinusOne        as PM1
import           ALife.Creatur.Gene.Test
import           ALife.Creatur.Wain.DVector.Prediction.Action   (Action)
import           ALife.Creatur.Wain.DVector.Prediction.ActionQC ()
import           ALife.Creatur.Wain.DVector.Prediction.Muser
import           ALife.Creatur.Wain.GeneticSOM                  (Label)
import           ALife.Creatur.Wain.Probability                 (Probability)
import           Control.DeepSeq                                (deepseq)
import           Control.Lens                                   (view)
import qualified Numeric.ApproxEq                               as N
import           Test.Framework                                 (Test,
                                                                 testGroup)
import           Test.Framework.Providers.QuickCheck2           (testProperty)
import           Test.QuickCheck

sizedArbMuser :: Int -> Gen DMuser
sizedArbMuser n = do
  o <- vectorOf 4 arbitrary
  d <- choose (1, min 3 (fromIntegral n + 1))
  w <- choose (1, 100)
  let (Right m) = makeMuser o d w
  return m

instance Arbitrary DMuser where
  arbitrary = sized sizedArbMuser

-- TODO: Make genetic encoding of PM1Double more accurate
equivMuser :: DMuser -> DMuser -> Bool
equivMuser x y
  = and (zipWith (N.within 500)
           (map PM1.wide $ view defaultOutcomes x)
           (map PM1.wide $ view defaultOutcomes y))
      && N.within 2 (fromIntegral $ view depth x)
           (fromIntegral $ view depth y)
      && N.within 2 (view width x) (view width y)

prop_generateResponses_never_causes_error
  :: DMuser
    -> [Action] -> [([Label], Probability)]
      -> Bool
prop_generateResponses_never_causes_error m as sps
  = deepseq (generateResponses m as sps) True

test :: Test
test = testGroup "ALife.Creatur.Wain.MuserQC"
  [
    testProperty "prop_serialize_round_trippable - DMuser"
      (prop_serialize_round_trippable :: DMuser -> Bool),
    testProperty "prop_genetic_round_trippable - DMuser"
      (prop_genetic_round_trippable equivMuser :: DMuser -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - DMuser"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> DMuser -> Bool),
    testProperty "prop_diploid_identity - DMuser"
      (prop_diploid_identity (==) :: DMuser -> Bool),
    testProperty "prop_show_read_round_trippable - DMuser"
      (prop_show_read_round_trippable (==) :: DMuser -> Bool),
    testProperty "prop_diploid_expressable - DMuser"
      (prop_diploid_expressable :: DMuser -> DMuser -> Bool),
    testProperty "prop_diploid_readable - DMuser"
      (prop_diploid_readable :: DMuser -> DMuser -> Bool),
    testProperty "prop_generateResponses_never_causes_error"
      prop_generateResponses_never_causes_error
  ]
