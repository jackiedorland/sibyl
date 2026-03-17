{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Sibyl.Models.SARIMAX where

import qualified Data.Vector.Unboxed as U
import Sibyl.Safe.TimeSeries (TimeSeries)
import Sibyl.Model (Model(..), Forecastable(..), ForecastableWith(..), FitError(..), IC, RegressorMatrix)
import Sibyl.Forecast (Forecast)

type PDQ  = (Int, Int, Int)
type PDQM = (Int, Int, Int, Int)

data EstimationMethod = CSS | ML | CSSML deriving (Show, Eq)

-- ARIMA

data ARIMASettings = ARIMASettings
    { arimaP          :: Int
    , arimaD          :: Int
    , arimaQ          :: Int
    , arimaIC         :: IC
    , arimaAllowMean  :: Bool
    , arimaAllowDrift :: Bool
    , arimaMethod     :: EstimationMethod
    , arimaStepwise   :: Bool
    , arimaLambda     :: Maybe Double
    } deriving (Show, Eq)

defaultARIMASettings :: ARIMASettings
defaultARIMASettings = undefined

data ARIMA t = ARIMA
    { arimaSettings  :: ARIMASettings
    , arimaCoeffs    :: U.Vector Double  -- AR then MA coefficients
    , arimaCoeffSEs  :: U.Vector Double  -- standard errors; empty if CSS
    , arimaResids    :: U.Vector Double
    , arimaFitted    :: U.Vector Double
    , arimaSigma2    :: Double
    , arimaLogLik    :: Maybe Double     -- Nothing if CSS
    , arimaConverged :: Maybe Bool       -- Nothing if CSS
    , arimaSeries    :: TimeSeries t Double
    }

fitARIMA :: U.Unbox t => PDQ -> TimeSeries t Double -> Either FitError (ARIMA t)
fitARIMA = undefined

fitARIMAWith :: U.Unbox t => ARIMASettings -> TimeSeries t Double -> Either FitError (ARIMA t)
fitARIMAWith = undefined

forecastARIMA :: (Ord t, Enum t, U.Unbox t) => Int -> ARIMA t -> Forecast t
forecastARIMA = undefined

instance (Ord t, Enum t, U.Unbox t) => Model ARIMA t where
    residuals    = arimaResids
    fitted       = arimaFitted
    modelSummary = undefined

instance (Ord t, Enum t, U.Unbox t) => Forecastable ARIMA t where
    forecast = forecastARIMA

-- ARIMAX

data ARIMAX t = ARIMAX
    { arimaxSettings  :: ARIMASettings
    , arimaxCoeffs    :: U.Vector Double  -- AR, MA, then regressor coefficients
    , arimaxCoeffSEs  :: U.Vector Double
    , arimaxResids    :: U.Vector Double
    , arimaxFitted    :: U.Vector Double
    , arimaxSigma2    :: Double
    , arimaxLogLik    :: Maybe Double
    , arimaxConverged :: Maybe Bool
    , arimaxSeries    :: TimeSeries t Double
    , arimaxTrainRegs :: RegressorMatrix  -- regressors used during fitting
    }

fitARIMAX :: U.Unbox t => PDQ -> TimeSeries t Double -> RegressorMatrix -> Either FitError (ARIMAX t)
fitARIMAX = undefined

fitARIMAXWith :: U.Unbox t => ARIMASettings -> TimeSeries t Double -> RegressorMatrix -> Either FitError (ARIMAX t)
fitARIMAXWith = undefined

instance (Ord t, Enum t, U.Unbox t) => Model ARIMAX t where
    residuals    = arimaxResids
    fitted       = arimaxFitted
    modelSummary = undefined

instance (Ord t, Enum t, U.Unbox t) => ForecastableWith RegressorMatrix ARIMAX t where
    forecastWith = undefined

-- SARIMA

data SARIMASettings = SARIMASettings
    { sarimaP          :: Int
    , sarimaD          :: Int
    , sarimaQ          :: Int
    , sarimaBigP       :: Int
    , sarimaBigD       :: Int
    , sarimaBigQ       :: Int
    , sarimaPeriod     :: Int
    , sarimaIC         :: IC
    , sarimaAllowMean  :: Bool
    , sarimaAllowDrift :: Bool
    , sarimaMethod     :: EstimationMethod
    , sarimaStepwise   :: Bool
    , sarimaLambda     :: Maybe Double
    } deriving (Show, Eq)

defaultSARIMASettings :: SARIMASettings
defaultSARIMASettings = undefined

data SARIMA t = SARIMA
    { sarimaSettings  :: SARIMASettings
    , sarimaCoeffs    :: U.Vector Double  -- AR, MA, SAR, SMA coefficients
    , sarimaCoeffSEs  :: U.Vector Double
    , sarimaResids    :: U.Vector Double
    , sarimaFitted    :: U.Vector Double
    , sarimaSigma2    :: Double
    , sarimaLogLik    :: Maybe Double
    , sarimaConverged :: Maybe Bool
    , sarimaSeries    :: TimeSeries t Double
    }

fitSARIMA :: U.Unbox t => PDQ -> PDQM -> TimeSeries t Double -> Either FitError (SARIMA t)
fitSARIMA = undefined

fitSARIMAWith :: U.Unbox t => SARIMASettings -> TimeSeries t Double -> Either FitError (SARIMA t)
fitSARIMAWith = undefined

forecastSARIMA :: (Ord t, Enum t, U.Unbox t) => Int -> SARIMA t -> Forecast t
forecastSARIMA = undefined

instance (Ord t, Enum t, U.Unbox t) => Model SARIMA t where
    residuals    = sarimaResids
    fitted       = sarimaFitted
    modelSummary = undefined

instance (Ord t, Enum t, U.Unbox t) => Forecastable SARIMA t where
    forecast = forecastSARIMA

-- SARIMAX

data SARIMAX t = SARIMAX
    { sarimaxSettings  :: SARIMASettings
    , sarimaxCoeffs    :: U.Vector Double  -- AR, MA, SAR, SMA, regressor coefficients
    , sarimaxCoeffSEs  :: U.Vector Double
    , sarimaxResids    :: U.Vector Double
    , sarimaxFitted    :: U.Vector Double
    , sarimaxSigma2    :: Double
    , sarimaxLogLik    :: Maybe Double
    , sarimaxConverged :: Maybe Bool
    , sarimaxSeries    :: TimeSeries t Double
    , sarimaxTrainRegs :: RegressorMatrix
    }

fitSARIMAX :: U.Unbox t => PDQ -> PDQM -> TimeSeries t Double -> RegressorMatrix -> Either FitError (SARIMAX t)
fitSARIMAX = undefined

fitSARIMAXWith :: U.Unbox t => SARIMASettings -> TimeSeries t Double -> RegressorMatrix -> Either FitError (SARIMAX t)
fitSARIMAXWith = undefined

instance (Ord t, Enum t, U.Unbox t) => Model SARIMAX t where
    residuals    = sarimaxResids
    fitted       = sarimaxFitted
    modelSummary = undefined

instance (Ord t, Enum t, U.Unbox t) => ForecastableWith RegressorMatrix SARIMAX t where
    forecastWith = undefined
