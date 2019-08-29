------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Prediction.MuserQC
-- Copyright   :  (c) Amy de Buitléir 2013-2018
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- QuickCheck tests.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ALife.Creatur.Wain.DVector.Prediction.MuserQC
  (
    test,
    equivMuser,
    sizedArbMuser
  ) where

import           ALife.Creatur.Wain.DVector.Prediction.Action
    (Action)
import           ALife.Creatur.Wain.DVector.Prediction.ActionQC
    ()
import           ALife.Creatur.Wain.DVector.Prediction.Muser
import           ALife.Creatur.Wain.GeneticSOM
    (Label)
import           ALife.Creatur.Wain.PlusMinusOne
    (pm1ToDouble)
import           ALife.Creatur.Wain.Probability
    (Probability)
import           ALife.Creatur.Wain.TestUtils
import           Control.DeepSeq
    (deepseq)
import           Control.Lens
    (view)
import qualified Numeric.ApproxEq                               as N
import           Test.Framework
    (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2
    (testProperty)
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
  = (and $ zipWith (N.within 500)
           (map pm1ToDouble $ view defaultOutcomes x)
           (map pm1ToDouble $ view defaultOutcomes y))
      && (N.within 2 (fromIntegral $ view depth x)
           (fromIntegral $ view depth y))
      && (N.within 2 (view width x) (view width y))

prop_generateResponses_never_causes_error
  :: DMuser
    -> [Action] -> [([Label], Probability)]
      -> Property
prop_generateResponses_never_causes_error m as sps
  = property $ deepseq (generateResponses m as sps) True

test :: Test
test = testGroup "ALife.Creatur.Wain.MuserQC"
  [
    testProperty "prop_serialize_round_trippable - DMuser"
      (prop_serialize_round_trippable :: DMuser -> Property),
    testProperty "prop_genetic_round_trippable - DMuser"
      (prop_genetic_round_trippable equivMuser :: DMuser -> Property),
    -- testProperty "prop_genetic_round_trippable2 - DMuser"
    --   (prop_genetic_round_trippable2
    --    :: Int -> [Word8] -> DMuser -> Property),
    testProperty "prop_diploid_identity - DMuser"
      (prop_diploid_identity (==) :: DMuser -> Property),
    testProperty "prop_show_read_round_trippable - DMuser"
      (prop_show_read_round_trippable (==) :: DMuser -> Property),
    testProperty "prop_diploid_expressable - DMuser"
      (prop_diploid_expressable :: DMuser -> DMuser -> Property),
    testProperty "prop_diploid_readable - DMuser"
      (prop_diploid_readable :: DMuser -> DMuser -> Property),
    testProperty "prop_generateResponses_never_causes_error"
      prop_generateResponses_never_causes_error
  ]
