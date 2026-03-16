{-# LANGUAGE MultiParamTypeClasses #-}

module Sibyl.Model where

import qualified Data.Vector.Unboxed as U
import Sibyl.Forecast (Forecast)

data TrainingSummary t = TrainingSummary
    { dataStart  :: t
    , dataEnd    :: t
    , nObs       :: Int    
    , sigma2     :: Double 
    , naiveScale :: Double 
    } deriving (Show, Eq)

data InformationCriteria = InformationCriteria
    { aic  :: Double -- AIC
    , aicc :: Double -- AICc
    , bic  :: Double -- BIC
    , hqic :: Double -- future: Hannan-Quinn, maybe
    } deriving (Show, Eq)

data ModelSummary t = ModelSummary
    { name         :: String                        -- e.g. ARIMA(1,1,1)
    , coefficients :: [(String, Double, Double)]    -- e.g. [("ar1", 0.42, 0.09), ("ma1", -0.31, 0.11)]
    , criteria     :: Maybe InformationCriteria
    , logLik       :: Maybe Double
    , converged    :: Maybe Bool                   
    , training     :: TrainingSummary t
    }

data FitError
    = InvalidModelSpec String
    | InsufficientData String
    | NumericalFailure String
    deriving (Show, Eq)

class SibylModel m t where 
    forecast     :: Int -> m t -> Forecast t
    residuals    :: m t -> U.Vector Double
    fitted       :: m t -> U.Vector Double
    summarize    :: m t -> IO ()
    modelSummary :: m t -> ModelSummary t


