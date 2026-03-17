{-# LANGUAGE MultiParamTypeClasses #-}

module Sibyl.Model where

import qualified Data.Vector.Unboxed as U
import Sibyl.Forecast (Forecast)

data IC = AIC | AICc | BIC deriving (Show, Eq)

-- this kinda sucks, maybe hmatrix is better? not sure
data RegressorMatrix = RegressorMatrix
    { regressorNames :: [String]
    , regressorData  :: U.Vector Double
    , regressorRows  :: Int
    , regressorCols  :: Int
    } deriving (Show, Eq)

class Model m t where
    residuals    :: m t -> U.Vector Double
    fitted       :: m t -> U.Vector Double
    modelSummary :: m t -> ModelSummary t
    summarize    :: m t -> IO ()
    summarize    =  print . modelSummary

class Model m t => Forecastable m t where
    forecast :: Int -> m t -> Forecast t

class Model m t => ForecastableWith r m t where
    forecastWith :: Int -> r -> m t -> Forecast t

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
    , hqic :: Double -- Hannan-Quinn (future use)
    } deriving (Show, Eq)

data ErrorMeasures = ErrorMeasures
    { emMe   :: Double
    , emRmse :: Double
    , emMae  :: Double
    , emMape :: Double
    , emMase :: Double
    } deriving (Show, Eq)

data ModelSummary t = ModelSummary
    { name         :: String                      -- e.g. "ARIMA(1,1,1)"
    , coefficients :: [(String, Double, Double)]  -- (name, estimate, std error)
    , criteria     :: Maybe InformationCriteria
    , logLik       :: Maybe Double
    , converged    :: Maybe Bool
    , errors       :: Maybe ErrorMeasures
    , training     :: TrainingSummary t
    }

instance Show (ModelSummary t) where
    show ms = unlines
        [ name ms
        , replicate (length (name ms)) '='
        , ""
        , pad "Coefficients:"
        , unlines [ "  " ++ pad coef ++ show val ++ "  (se: " ++ show stdErr ++ ")"
                  | (coef, val, stdErr) <- coefficients ms ]
        , pad "Log-likelihood:" ++ maybe "N/A" show (logLik ms)
        , pad "Converged:"      ++ maybe "N/A" show (converged ms)
        , ""
        , case criteria ms of
            Nothing -> ""
            Just ic -> unlines
                [ "Information criteria:"
                , "  " ++ pad "AIC:"  ++ show (aic  ic)
                , "  " ++ pad "AICc:" ++ show (aicc ic)
                , "  " ++ pad "BIC:"  ++ show (bic  ic)
                ]
        , ""
        , case errors ms of
            Nothing -> "Error measures: N/A"
            Just e  -> unlines
                [ "Error measures:"
                , "  " ++ pad "ME:"   ++ show (emMe   e)
                , "  " ++ pad "RMSE:" ++ show (emRmse e)
                , "  " ++ pad "MAE:"  ++ show (emMae  e)
                , "  " ++ pad "MAPE:" ++ show (emMape e)
                , "  " ++ pad "MASE:" ++ show (emMase e)
                ]
        ]
      where
        pad s = s ++ replicate (20 - length s) ' '

data FitError
    = InvalidModelSpec String
    | InsufficientData String
    | NumericalFailure  String
    deriving (Show, Eq)