{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}

module Sibyl.Models.HoltWinters where

import qualified Data.Vector.Unboxed as U
import Sibyl.TimeSeries (TimeSeries, Period)
import Sibyl.Model
  ( Model(..)
  , ModelFamily(..)
  , Fitted
  , Prediction(..)
  , Summary(..)
  , FitError(..)
  , TrainingSummary(..)
  , ErrorMeasures(..)
  )

data SeasonalVariant
  = Additive
  | Multiplicative
  deriving (Show, Eq)

data HoltWintersSettings = HoltWintersSettings
  { hwVariant  :: SeasonalVariant
  , hwPeriod   :: Period
  , hwAlpha    :: Maybe Double  -- level smoothing;   Nothing = optimize
  , hwBeta     :: Maybe Double  -- trend smoothing;   Nothing = optimize
  , hwGamma    :: Maybe Double  -- seasonal smoothing; Nothing = optimize
  , hwCILevel  :: Double        -- e.g. 0.95
  } deriving (Show, Eq)

defaultHoltWintersSettings :: HoltWintersSettings
defaultHoltWintersSettings = HoltWintersSettings
  { hwVariant  = Additive
  , hwPeriod   = 12
  , hwAlpha    = Nothing
  , hwBeta     = Nothing
  , hwGamma    = Nothing
  , hwCILevel  = 0.95
  }

data instance Fitted 'HoltWinters idx = FittedHoltWinters
  { hwSettings    :: HoltWintersSettings
  , hwSeries      :: TimeSeries idx Double
  , hwFittedVals  :: U.Vector Double
  , hwResiduals   :: U.Vector Double
  , hwLevel       :: U.Vector Double   -- L(t) for each t
  , hwTrend       :: U.Vector Double   -- T(t) for each t
  , hwSeasonal    :: U.Vector Double   -- S(t) for each t; length = n
  , hwFinalAlpha  :: Double            -- alpha actually used (optimized or supplied)
  , hwFinalBeta   :: Double
  , hwFinalGamma  :: Double
  , hwSigma2      :: Double
  }

instance Model 'HoltWinters where
  type Settings 'HoltWinters = HoltWintersSettings
  type Future   'HoltWinters = ()

  fit          = fitHoltWinters
  predict      = predictHoltWinters
  residuals    = hwResiduals
  fitted       = hwFittedVals
  modelSummary = hwModelSummary

fitHoltWinters
  :: U.Unbox idx
  => HoltWintersSettings
  -> TimeSeries idx Double
  -> Either FitError (Fitted 'HoltWinters idx)
fitHoltWinters = undefined

predictHoltWinters
  :: (Ord idx, Enum idx, U.Unbox idx)
  => Int
  -> ()
  -> Fitted 'HoltWinters idx
  -> Either FitError (Prediction idx)
predictHoltWinters = undefined

hwModelSummary
  :: U.Unbox idx
  => Fitted 'HoltWinters idx
  -> Summary idx
hwModelSummary = undefined