module Sibyl.Safe
  ( module Sibyl.TimeSeries
  , module Sibyl.Model
  , module Sibyl.Forecast
  , fitARIMA
  , fitETS
  , fitNaive
  ) where

import Sibyl.TimeSeries hiding (name)
import Sibyl.Model
import Sibyl.Forecast
import qualified Sibyl.Models.ARIMA as ARIMA

fitARIMA :: ARIMA.ARIMASettings -> TimeSeries t Double -> IO (Either FitError (ARIMA.ARIMA t))
fitARIMA = undefined

fitETS :: a
fitETS = undefined

fitNaive :: a
fitNaive = undefined
