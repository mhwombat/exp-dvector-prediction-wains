------------------------------------------------------------------------
-- |
-- Module      :  ALife.Creatur.Wain.DVector.Prediction.MuserInternal
-- Copyright   :  (c) 2013-2021 Amy de Buitléir
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- The muser takes classification information (labels and
-- probabilities) about the objects in a wain's field of "view",
-- and determines the most likely scenarios that the wain could be
-- facing.
--
------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module ALife.Creatur.Wain.DVector.Prediction.MuserInternal
  (
    DMuser,
    depth,
    width,
    makeMuser,
    generateResponses,
    defaultOutcomes
  ) where

import qualified ALife.Creatur.Gene.Numeric.PlusMinusOne      as PM1
import qualified ALife.Creatur.Gene.Numeric.UnitInterval      as UI
import qualified ALife.Creatur.Genetics.BRGCWord8             as G
import           ALife.Creatur.Genetics.Diploid               (Diploid)
import           ALife.Creatur.Wain.DVector.Prediction.Action (Action,
                                                               expandActionList)
import           ALife.Creatur.Wain.GeneticSOM                (Label)
import qualified ALife.Creatur.Wain.Muser                     as M
import           ALife.Creatur.Wain.Pretty                    (Pretty)
import           ALife.Creatur.Wain.Response                  (Response (..))
import           ALife.Creatur.Wain.Statistics                (Statistical,
                                                               dStat, iStat,
                                                               stats)
import           Control.DeepSeq                              (NFData)
import           Control.Lens
import           Data.List                                    (sortBy)
import           Data.Ord                                     (comparing)
import           Data.Serialize                               (Serialize)
import           Data.Word                                    (Word8)
import           GHC.Generics                                 (Generic)

-- | Object responsible for generating potential responses for
--   consideration.
data DMuser = DMuser
  {
    -- | If a wain has no model for a response it's considering, it
    --   will use these values as a prediction.
    --   Positive values make the wain optimistic and more likely to
    --   take risks. A negative value makes the wain pessimistic and
    --   risk-averse.
    _defaultOutcomes :: [PM1.Double],
    -- | Number of possible scenarios a wain will evaluate before
    --   choosing an action.
    _depth           :: Word8,
    -- | Controls how quickly a wain will explore the prediction space
    _width           :: Double
  } deriving ( Eq, Show, Read, Generic, Ord, Serialize, Diploid, NFData )
makeLenses ''DMuser

instance Pretty DMuser

instance Statistical DMuser where
  stats (DMuser (eo:po:lso:_) d w) = [iStat "depth" d,
         dStat "width" w,
         dStat "default energy outcome" . PM1.wide $ eo,
         dStat "default passion outcome" . PM1.wide $ po,
         dStat "default litterSize outcome" . PM1.wide $ lso]
  stats _ = error "default outcome list is too short"

instance G.Genetic DMuser where
  put (DMuser o d w) = G.put o >> G.put d >> G.put w
  get = do
    o <- G.get
    d <- G.get
    w <- G.get
    -- Use the safe constructor!
    case makeMuser <$> o <*> d <*> w of
      Left msgs -> return $ Left msgs
      Right b   -> return b

instance M.Muser DMuser where
  type Action DMuser = Action
  generateResponses = generateResponses
  defaultOutcomes = view defaultOutcomes

-- | Constructor
makeMuser :: [PM1.Double] -> Word8 -> Double -> Either [String] DMuser
makeMuser os d w
 | d == 0         = Left ["zero depth"]
 | length os < 3 = Left ["default outcome list is too short"]
 | otherwise     = Right $ DMuser os d w

generateResponses
  :: DMuser
    -> [Action] -> [([Label], UI.Double)]
      -> [(Response Action, UI.Double)]
generateResponses m as sps = concatMap (generateResponses' m sps') as'
  where sps' = bestHypotheses m sps
        as' = expandActionList (_width m) as

-- | Internal method
generateResponses'
  :: DMuser -> [([Label], UI.Double)] -> Action
    -> [(Response Action, UI.Double)]
generateResponses' m sps a = map (generateResponse m a) sps

-- | Internal method
generateResponse
  :: DMuser -> Action -> ([Label], UI.Double)
    -> (Response Action, UI.Double)
generateResponse m a (ls, p) = (Response ls a os, p)
  where os = _defaultOutcomes m

-- | Given the wain's current condition, and a list of scenarios
--   paired with the probability each scenario is true, selects the
--   most likely scenarios.
bestHypotheses
  :: DMuser -> [([Label], UI.Double)] -> [([Label], UI.Double)]
bestHypotheses m
  = take (fromIntegral . _depth $ m) . sortBy (flip (comparing snd))
