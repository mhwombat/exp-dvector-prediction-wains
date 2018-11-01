------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Prediction.ResponseTweakerQC
-- Copyright   :  (c) Amy de Buitléir 2013-2018
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
module ALife.Creatur.Wain.DVector.Prediction.ResponseTweakerQC
  (
    test
  ) where

import ALife.Creatur.Wain.DVector.Double (maxDouble, minDouble)
import ALife.Creatur.Wain.DVector.Prediction.ResponseTweaker
import ALife.Creatur.Wain.DVector.Prediction.Action (Action, mkAction)
import ALife.Creatur.Wain.DVector.Prediction.ActionQC ()
import ALife.Creatur.Wain.Response (Response(..))
import ALife.Creatur.Wain.TestUtils (sizedArbResponse,
  sizedArbWeights, prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary (Response Action) where
  arbitrary = sized (sizedArbResponse arbitrary)

instance Arbitrary ResponseTweaker where
  arbitrary = do
    ws <- sizedArbWeights 2
    return $ ResponseTweaker ws

prop_responseDiff_is_reflexive
  :: ResponseTweaker -> Response Action -> Response Action -> Property
prop_responseDiff_is_reflexive rt a b
  = property $ diff rt a b == diff rt b a

prop_responseDiff_can_be_zero :: ResponseTweaker -> Response Action -> Property
prop_responseDiff_can_be_zero rt a
  = property $ diff rt a a == 0

prop_responseDiff_can_be_one :: ResponseTweaker -> Int -> Property
prop_responseDiff_can_be_one rt n
  = property $ diff rt r1 r2 == 1
  where ls1 = replicate n 0
        ls2 = replicate n 1
        a1 = mkAction maxDouble
        a2 = mkAction minDouble
        os1 = replicate 4 (-1)
        os2 = replicate 4 1
        r1 = Response ls1 a1 os1
        r2 = Response ls2 a2 os2

test :: Test
test = testGroup "ALife.Creatur.Wain.DVector.Prediction.ResponseTweakerQC"
  [
    testProperty "prop_serialize_round_trippable - Action"
      (prop_serialize_round_trippable :: Action -> Property),
    testProperty "prop_genetic_round_trippable - Action"
      (prop_genetic_round_trippable (==) :: Action -> Property),
    testProperty "prop_diploid_identity - Action"
      (prop_diploid_identity (==) :: Action -> Property),
    testProperty "prop_responseDiff_is_reflexive"
      prop_responseDiff_is_reflexive,
    testProperty "prop_responseDiff_can_be_zero"
      prop_responseDiff_can_be_zero,
    testProperty "prop_responseDiff_can_be_one"
      prop_responseDiff_can_be_one
  ]
