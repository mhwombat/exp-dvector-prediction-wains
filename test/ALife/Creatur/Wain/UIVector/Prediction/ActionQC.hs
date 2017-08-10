------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Prediction.ActionQC
-- Copyright   :  (c) Amy de Buitléir 2013-2016
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
module ALife.Creatur.Wain.DVector.Prediction.ActionQC
  (
    test
  ) where

import ALife.Creatur.Wain.TestUtils (prop_serialize_round_trippable,
  prop_genetic_round_trippable, prop_diploid_identity)
import ALife.Creatur.Wain.DVector.Prediction.Action
-- import ALife.Creatur.Wain.DVector.Prediction.TestUtils
--   (prop_serialize_round_trippable, prop_genetic_round_trippable,
--     prop_diploid_identity)
import ALife.Creatur.Wain.UnitInterval (UIDouble)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance Arbitrary Action where
  arbitrary = elements [minBound .. maxBound]

prop_action_diff_is_reflexive :: Action -> Action -> Property
prop_action_diff_is_reflexive a b
  = property $ actionDiff a b == actionDiff b a

prop_action_diff_can_be_zero :: Action -> Property
prop_action_diff_can_be_zero a = property $ actionDiff a a == 0

prop_action_diff_can_be_one :: Property
prop_action_diff_can_be_one
  = property $ actionDiff minBound maxBound == 1

prop_predict_consistent_with_postdict :: UIDouble -> Action -> Property
prop_predict_consistent_with_postdict x1 a =
  x2 > 0 && x2 < 1 ==> a' == a
  where x2 = predict a x1
        a' = postdict x1 x2

test :: Test
test = testGroup "ALife.Creatur.Wain.DVector.Prediction.ActionQC"
  [
    testProperty "prop_serialize_round_trippable - Action"
      (prop_serialize_round_trippable :: Action -> Property),
    testProperty "prop_genetic_round_trippable - Action"
      (prop_genetic_round_trippable (==) :: Action -> Property),
    testProperty "prop_diploid_identity - Action"
      (prop_diploid_identity (==) :: Action -> Property),
    testProperty "prop_action_diff_is_reflexive"
      prop_action_diff_is_reflexive,
    testProperty "prop_action_diff_can_be_zero"
      prop_action_diff_can_be_zero,
    testProperty "prop_action_diff_can_be_one"
      prop_action_diff_can_be_one,
    testProperty "prop_predict_consistent_with_postdict"
      prop_predict_consistent_with_postdict
  ]
