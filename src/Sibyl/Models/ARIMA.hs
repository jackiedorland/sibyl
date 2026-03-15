module Sibyl.Models.ARIMA where

import qualified Data.Vector.Unboxed as U
import Sibyl.Safe.TimeSeries (TimeSeries)
import Sibyl.Model (SibylModel(..), ModelSummary, FitError, TrainingSummary)

data ARIMASettings = ARIMASettings 
    { maxP :: Int
    , maxQ :: Int
    , maxD :: Int -- THIS IS INCOMPLETE; FOR PLANNING ONLY   
    }

defaultARIMA :: ARIMASettings
defaultARIMA = undefined

data ARIMA t = ARIMA 
    {   coefficients     :: U.Vector Double
    ,   residiuals       :: U.Vector Double
    ,   fitted           :: TimeSeries t Double
    ,   settings         :: ARIMASettings
    ,   trainingSummary  :: TrainingSummary -- training data
    }