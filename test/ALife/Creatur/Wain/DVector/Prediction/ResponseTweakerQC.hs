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

import           ALife.Creatur.Wain.DVector.Double
    (maxDouble, minDouble)
import           ALife.Creatur.Wain.DVector.Prediction.Action
    (Action, mkAction)
import           ALife.Creatur.Wain.DVector.Prediction.ActionQC
    ()
import           ALife.Creatur.Wain.DVector.Prediction.ResponseTweaker
import           ALife.Creatur.Wain.Response
    (Response (..))
import           ALife.Creatur.Wain.TestUtils
    ( prop_diploid_expressable
    , prop_diploid_identity
    , prop_diploid_readable
    , prop_genetic_round_trippable
    , prop_makeSimilar_works
    , prop_serialize_round_trippable
    , prop_show_read_round_trippable
    , sizedArbResponse
    , sizedArbWeights
    )
import           ALife.Creatur.Wain.UnitInterval
    (UIDouble, uiToDouble)
import           ALife.Creatur.Wain.Weights
    (Weights, toUIDoubles)
import qualified Numeric.ApproxEq                                      as N
import           Test.Framework
    (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2
    (testProperty)
import           Test.QuickCheck

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

prop_makeResponseSimilarWorks
  :: ResponseTweaker -> Response Action -> UIDouble -> Response Action
  -> Property
prop_makeResponseSimilarWorks t = prop_makeSimilar_works (diff t) (adjust t)

-- TODO: Move to TestUtils
equivWeights :: Weights -> Weights -> Bool
equivWeights x y = and $ zipWith f (map uiToDouble $ toUIDoubles x)
                                   (map uiToDouble $ toUIDoubles y)
  where f a b = N.within 2 a b

equiv :: ResponseTweaker -> ResponseTweaker -> Bool
equiv (ResponseTweaker xs) (ResponseTweaker ys) = equivWeights xs ys

test :: Test
test = testGroup "ALife.Creatur.Wain.DVector.Prediction.ResponseTweakerQC"
  [
    testProperty "prop_serialize_round_trippable - ResponseTweaker"
      (prop_serialize_round_trippable :: ResponseTweaker -> Property),
    testProperty "prop_genetic_round_trippable - ResponseTweaker"
      (prop_genetic_round_trippable equiv :: ResponseTweaker -> Property),
    -- testProperty "prop_genetic_round_trippable2 - ResponseTweaker"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> ResponseTweaker -> Property),
    testProperty "prop_diploid_identity - ResponseTweaker"
      (prop_diploid_identity equiv :: ResponseTweaker -> Property),
    testProperty "prop_show_read_round_trippable - ResponseTweaker"
      (prop_show_read_round_trippable (==) :: ResponseTweaker -> Property),
    testProperty "prop_diploid_expressable - ResponseTweaker"
      (prop_diploid_expressable :: ResponseTweaker -> ResponseTweaker -> Property),
    testProperty "prop_diploid_readable - ResponseTweaker"
      (prop_diploid_readable :: ResponseTweaker -> ResponseTweaker -> Property),
    testProperty "prop_responseDiff_is_reflexive"
      prop_responseDiff_is_reflexive,
    testProperty "prop_responseDiff_can_be_zero"
      prop_responseDiff_can_be_zero,
    testProperty "prop_responseDiff_can_be_one"
      prop_responseDiff_can_be_one,
    testProperty "prop_makeResponseSimilarWorks"
      prop_makeResponseSimilarWorks
  ]
