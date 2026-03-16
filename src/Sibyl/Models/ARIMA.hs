module Sibyl.Models.ARIMA where

import qualified Data.Vector.Unboxed as U
import Sibyl.Safe.TimeSeries (TimeSeries)
import Sibyl.Model (SibylModel(..), ModelSummary, FitError, TrainingSummary)

data IC = AIC | AICc | BIC deriving (Show, Eq)

data EstimationMethod = CSS | ML | CSSML deriving (Show, Eq)

data ARIMAOrder
    = Manual { p :: Int, d :: Int, q :: Int
             , bigP :: Int, bigD :: Int, bigQ :: Int }
    | Auto   { maxP :: Int, maxQ :: Int, maxD :: Int
             , maxBigP :: Int, maxBigQ :: Int, maxBigD :: Int
             , maxOrder :: Int }

data ARIMASettings = ARIMASettings
    { order      :: ARIMAOrder
    , period     :: Maybe Int
    , ic         :: IC
    , allowMean  :: Bool
    , allowDrift :: Bool
    , method     :: EstimationMethod  -- CSS | ML | CSSML
    , stepwise   :: Bool
    , lambda     :: Maybe Double
    }

defaultARIMA :: ARIMASettings
defaultARIMA = undefined

data ARIMA t = ARIMA 
    {   coefficients     :: U.Vector Double
    ,   residuals        :: U.Vector Double
    ,   fitted           :: TimeSeries t Double
    ,   settings         :: ARIMASettings
    ,   trainingSummary  :: TrainingSummary t -- training data
    }