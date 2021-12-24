------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Prediction.ActionQC
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.DVector.Prediction.ActionQC
  (
    test
  ) where

import           ALife.Creatur.Gene.Test                      (prop_diploid_expressable,
                                                               prop_diploid_identity,
                                                               prop_diploid_readable,
                                                               prop_genetic_round_trippable,
                                                               prop_makeSimilar_works,
                                                               prop_serialize_round_trippable,
                                                               prop_show_read_round_trippable)
import           ALife.Creatur.Wain.DVector.Prediction.Action
import qualified Data.Datamining.Pattern.Numeric              as N
import           Test.Framework                               (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2         (testProperty)
import           Test.QuickCheck

instance Arbitrary Action where
  arbitrary = fmap mkAction arbitrary

prop_action_diff_is_reflexive :: Action -> Action -> Bool
prop_action_diff_is_reflexive a b
  = actionDiff a b == actionDiff b a

prop_action_diff_can_be_zero :: Action -> Bool
prop_action_diff_can_be_zero a = actionDiff a a == 0

prop_action_diff_can_be_one :: Property
prop_action_diff_can_be_one = property
  $ actionDiff (mkAction N.minDouble) (mkAction N.maxDouble) == 1

prop_predict_consistent_with_postdict :: Double -> Action -> Property
prop_predict_consistent_with_postdict x1 a =
  x2 > 0 && x2 < 1 ==> equiv a' a
  where x2 = predict a x1
        a' = postdict x1 x2
        equiv y z = actionToDouble y - actionToDouble z < aTad
        aTad = 1e-10

test :: Test
test = testGroup "ALife.Creatur.Wain.DVector.Prediction.ActionQC"
  [
    testProperty "prop_serialize_round_trippable - Action"
      (prop_serialize_round_trippable :: Action -> Bool),
    testProperty "prop_genetic_round_trippable - Action"
      (prop_genetic_round_trippable (==) :: Action -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - Action"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> Action -> Bool),
    testProperty "prop_diploid_identity - Action"
      (prop_diploid_identity (==) :: Action -> Bool),
    testProperty "prop_show_read_round_trippable - Action"
      (prop_show_read_round_trippable (==) :: Action -> Bool),
    testProperty "prop_diploid_expressable - Action"
      (prop_diploid_expressable :: Action -> Action -> Bool),
    testProperty "prop_diploid_readable - Action"
      (prop_diploid_readable :: Action -> Action -> Bool),
    testProperty "prop_makeSimilar_works"
      (prop_makeSimilar_works actionDiff makeActionSimilar),
    testProperty "prop_action_diff_is_reflexive"
      prop_action_diff_is_reflexive,
    testProperty "prop_action_diff_can_be_zero"
      prop_action_diff_can_be_zero,
    testProperty "prop_action_diff_can_be_one"
      prop_action_diff_can_be_one,
    testProperty "prop_predict_consistent_with_postdict"
      prop_predict_consistent_with_postdict
  ]
