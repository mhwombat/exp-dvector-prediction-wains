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
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ALife.Creatur.Wain.DVector.Prediction.ActionQC
  (
    test
  ) where

import qualified ALife.Creatur.Gene.AdjusterTest                      as AT
import qualified ALife.Creatur.Gene.Numeric.PlusMinusOne              as PM1
import qualified ALife.Creatur.Gene.Numeric.UnitInterval              as UI
import           ALife.Creatur.Gene.Numeric.WeightsInternal           (sizedArbWeights)
import qualified ALife.Creatur.Gene.Test                              as GT
import           ALife.Creatur.Wain.DVector.Prediction.ActionInternal
import           ALife.Creatur.Wain.Response                          (Response,
                                                                       action,
                                                                       outcomes)
import           ALife.Creatur.Wain.ResponseInternal                  (arbResponse,
                                                                       sizedArbResponse)
import qualified Data.Datamining.Pattern.List                         as L
import qualified Data.Datamining.Pattern.Numeric                      as N
import           Test.Framework                                       (Test,
                                                                       testGroup)
import           Test.Framework.Providers.QuickCheck2                 (testProperty)
import           Test.QuickCheck.Counterexamples                      hiding
                                                                      (labels)

instance Arbitrary Action where
  arbitrary = fmap mkAction arbitrary

instance Arbitrary (Response Action) where
  arbitrary = sized (sizedArbResponse arbitrary)

instance Arbitrary ResponseAdjuster where
  arbitrary = ResponseAdjuster <$> arbitrary <*> sizedArbWeights 2

prop_predict_consistent_with_postdict :: Double -> Action -> Property
prop_predict_consistent_with_postdict x1 a =
  x2 > 0 && x2 < 1 ==> eq a' a
  where x2 = predict a x1
        a' = postdict x1 x2
        eq y z = actionToDouble y - actionToDouble z < aTad
        aTad = 1e-10

data TwoResponsesSameLength = TwoResponsesSameLength (Response Action) (Response Action)
  deriving Show

sizedTwoResponsesSameLength :: Int -> Gen TwoResponsesSameLength
sizedTwoResponsesSameLength n = do
  nObjects <- choose (0, n)
  let nConditions = n - nObjects
  x <- arbResponse nObjects nConditions arbitrary
  y <- arbResponse nObjects nConditions arbitrary
  return $ TwoResponsesSameLength x y

instance Arbitrary TwoResponsesSameLength where
  arbitrary = sized sizedTwoResponsesSameLength

equiv :: Response Action -> Response Action -> Bool
equiv x y = dMean <= 1e-10
    where d1 = UI.wide $ actionDiff (action x) (action y)
          d2 = L.diff N.realFloatDiff (map PM1.wide $ outcomes x)
                                      (map PM1.wide $ outcomes y)
          dMean = abs $ (d1 + d2) / 2

prop_makeSimilar_improves_similarity :: ResponseAdjuster -> TwoResponsesSameLength -> UI.Double -> Bool
prop_makeSimilar_improves_similarity a (TwoResponsesSameLength x y) r
  = AT.prop_makeSimilar_improves_similarity a x r y

prop_zero_adjustment_makes_no_change :: ResponseAdjuster -> TwoResponsesSameLength -> Bool
prop_zero_adjustment_makes_no_change a (TwoResponsesSameLength x y)
  = AT.prop_zero_adjustment_makes_no_change equiv a x y

prop_full_adjustment_gives_perfect_match :: ResponseAdjuster -> TwoResponsesSameLength -> Bool
prop_full_adjustment_gives_perfect_match a (TwoResponsesSameLength x y)
  = AT.prop_full_adjustment_gives_perfect_match equiv a x y

test :: Test
test = testGroup "ALife.Creatur.Wain.DVector.Prediction.ActionQC"
  [
    testProperty "prop_serialize_round_trippable - Action"
      (GT.prop_serialize_round_trippable :: Action -> Bool),
    testProperty "prop_genetic_round_trippable - Action"
      (GT.prop_genetic_round_trippable (==) :: Action -> Bool),
    -- testProperty "prop_genetic_round_trippable2 - Action"
    --   (GT.prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> Action -> Bool),
    testProperty "prop_diploid_identity - Action"
      (GT.prop_diploid_identity (==) :: Action -> Bool),
    testProperty "prop_show_read_round_trippable - Action"
      (GT.prop_show_read_round_trippable (==) :: Action -> Bool),
    testProperty "prop_diploid_expressable - Action"
      (GT.prop_diploid_expressable :: Action -> Action -> Bool),
    testProperty "prop_diploid_readable - Action"
      (GT.prop_diploid_readable :: Action -> Action -> Bool),

    testProperty "prop_predict_consistent_with_postdict"
      prop_predict_consistent_with_postdict,

    testProperty "prop_diff_can_be_0 - ResponseAdjuster"
      (AT.prop_diff_can_be_0 :: ResponseAdjuster -> Response Action -> Bool),
    -- testProperty "prop_diff_can_be_1 - ResponseAdjuster"
    --   (AT.prop_diff_can_be_1 :: ResponseAdjuster -> Response Action -> Bool),
    testProperty "prop_diff_is_symmetric - ResponseAdjuster"
      (AT.prop_diff_is_symmetric :: ResponseAdjuster -> Response Action -> Response Action -> Bool),
    testProperty "prop_makeSimilar_improves_similarity - ResponseAdjuster"
      -- (AT.prop_makeSimilar_improves_similarity :: ResponseAdjuster -> Response Action -> UI.Double -> Response Action -> Bool),
      prop_makeSimilar_improves_similarity,
    testProperty "prop_zero_adjustment_makes_no_change - ResponseAdjuster"
      -- (AT.prop_zero_adjustment_makes_no_change equiv :: ResponseAdjuster -> Response Action -> Response Action -> Bool),
      prop_zero_adjustment_makes_no_change,
    testProperty "prop_full_adjustment_gives_perfect_match - ResponseAdjuster"
      -- (AT.prop_full_adjustment_gives_perfect_match equiv :: ResponseAdjuster -> Response Action -> Response Action -> Bool)
      prop_full_adjustment_gives_perfect_match
  ]
