{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}

module Sibyl.Models.SARIMAX where

import qualified Data.Vector.Unboxed as U
import Sibyl.Safe.TimeSeries (TimeSeries)
import Sibyl.Model (ModelFamily(..), Model(..), FitError(..), IC, RegressorMatrix)
import Sibyl.Prediction (Prediction)

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
    , arimaCILevel    :: Double
    , arimaLambda     :: Maybe Double
    } deriving (Show, Eq)

defaultARIMASettings :: ARIMASettings
defaultARIMASettings = undefined

data instance Fitted 'ARIMA idx = FittedARIMA
    { arimaSettings   :: ARIMASettings
    , arimaCoeffs     :: U.Vector Double
    , arimaCoeffSEs   :: U.Vector Double
    , arimaResids     :: U.Vector Double
    , arimaFittedVals :: U.Vector Double
    , arimaSigma2     :: Double
    , arimaLogLik     :: Maybe Double
    , arimaConverged  :: Maybe Bool
    , arimaSeries     :: TimeSeries idx Double
    }

instance Model 'ARIMA where
    type Settings 'ARIMA = ARIMASettings
    type Future   'ARIMA = ()

    fit       = fitARIMA
    predict   = predictARIMA
    summarize = arimaSummary
    residuals = arimaResids
    fitted    = arimaFittedVals

fitARIMA :: U.Unbox idx => ARIMASettings -> TimeSeries idx Double -> Either FitError (Fitted 'ARIMA idx)
fitARIMA = undefined

predictARIMA :: U.Unbox idx => Int -> () -> Fitted 'ARIMA idx -> Either FitError (Prediction idx)
predictARIMA = undefined

arimaSummary :: U.Unbox idx => Fitted 'ARIMA idx -> Summary idx
arimaSummary = undefined

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
    , sarimaCILevel    :: Double
    , sarimaLambda     :: Maybe Double
    } deriving (Show, Eq)

defaultSARIMASettings :: SARIMASettings
defaultSARIMASettings = undefined

data instance Fitted 'SARIMA idx = FittedSARIMA
    { sarimaSettings   :: SARIMASettings
    , sarimaCoeffs     :: U.Vector Double
    , sarimaCoeffSEs   :: U.Vector Double
    , sarimaResids     :: U.Vector Double
    , sarimaFittedVals :: U.Vector Double
    , sarimaSigma2     :: Double
    , sarimaLogLik     :: Maybe Double
    , sarimaConverged  :: Maybe Bool
    , sarimaSeries     :: TimeSeries idx Double
    }

instance Model 'SARIMA where
    type Settings 'SARIMA = SARIMASettings
    type Future   'SARIMA = ()

    fit       = fitSARIMA
    predict   = predictSARIMA
    summarize = sarimaSummary
    residuals = sarimaResids
    fitted    = sarimaFittedVals

fitSARIMA :: U.Unbox idx => SARIMASettings -> TimeSeries idx Double -> Either FitError (Fitted 'SARIMA idx)
fitSARIMA = undefined

predictSARIMA :: U.Unbox idx => Int -> () -> Fitted 'SARIMA idx -> Either FitError (Prediction idx)
predictSARIMA = undefined

sarimaSummary :: U.Unbox idx => Fitted 'SARIMA idx -> Summary idx
sarimaSummary = undefined

-- SARIMAX

data SARIMAXSettings = SARIMAXSettings
    { sarimaxP          :: Int
    , sarimaxD          :: Int
    , sarimaxQ          :: Int
    , sarimaxBigP       :: Int
    , sarimaxBigD       :: Int
    , sarimaxBigQ       :: Int
    , sarimaxPeriod     :: Int
    , sarimaxIC         :: IC
    , sarimaxAllowMean  :: Bool
    , sarimaxAllowDrift :: Bool
    , sarimaxMethod     :: EstimationMethod
    , sarimaxStepwise   :: Bool
    , sarimaxCILevel    :: Double
    , sarimaxLambda     :: Maybe Double
    , sarimaxTrainRegs  :: RegressorMatrix
    } deriving (Show, Eq)

defaultSARIMAXSettings :: SARIMAXSettings
defaultSARIMAXSettings = undefined

data instance Fitted 'SARIMAX idx = FittedSARIMAX
    { sarimaxSettings   :: SARIMAXSettings
    , sarimaxCoeffs     :: U.Vector Double
    , sarimaxCoeffSEs   :: U.Vector Double
    , sarimaxResids     :: U.Vector Double
    , sarimaxFittedVals :: U.Vector Double
    , sarimaxSigma2     :: Double
    , sarimaxLogLik     :: Maybe Double
    , sarimaxConverged  :: Maybe Bool
    , sarimaxSeries     :: TimeSeries idx Double
    }

instance Model 'SARIMAX where
    type Settings 'SARIMAX = SARIMAXSettings
    type Future   'SARIMAX = RegressorMatrix

    fit       = fitSARIMAX
    predict   = predictSARIMAX
    summarize = sarimaxSummary
    residuals = sarimaxResids
    fitted    = sarimaxFittedVals

fitSARIMAX :: U.Unbox idx => SARIMAXSettings -> TimeSeries idx Double -> Either FitError (Fitted 'SARIMAX idx)
fitSARIMAX = undefined

predictSARIMAX :: U.Unbox idx => Int -> RegressorMatrix -> Fitted 'SARIMAX idx -> Either FitError (Prediction idx)
predictSARIMAX = undefined

sarimaxSummary :: U.Unbox idx => Fitted 'SARIMAX idx -> Summary idx
sarimaxSummary = undefined