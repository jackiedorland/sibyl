module Sibyl.Models.ARIMA where

import qualified Data.Vector.Unboxed as U
import Sibyl.Safe.TimeSeries (TimeSeries)
import Sibyl.Model (SibylModel(..), ModelSummary, FitError)

data ARIMASettings = ARIMASettings 
    { maxP :: Int
    , maxQ :: Int
    , maxD :: Int -- incomplete, for planning only
    }

data TrainingSummary t = TrainingSummary
    { length :: Int
    , start  :: t
    , end    :: t
    }

defaultARIMA :: ARIMASettings
defaultARIMA = undefined

data ARIMA t = ARIMA 
    {   coefficients     :: U.Vector Double
    ,   residiuals       :: U.Vector Double
    ,   fitted           :: TimeSeries t Double
    ,   settings         :: ARIMASettings
    ,   trainingSummary  :: TrainingSummary t -- training data
    }