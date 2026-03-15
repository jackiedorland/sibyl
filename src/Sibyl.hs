module Sibyl
  ( module Sibyl.TimeSeries
  , module Sibyl.Model
  , module Sibyl.Forecast
  , fitARIMA
  , fitETS
  , fitNaive
  , TS
  , FC
  ) where

import Sibyl.TimeSeries hiding (name)
import Sibyl.Model
import Sibyl.Forecast
import qualified Sibyl.Models.ARIMA as ARIMA

type TS t y = TimeSeries t y
type FC t = Forecast t

fitARIMA :: ARIMA.ARIMASettings -> TimeSeries t Double -> IO (ARIMA.ARIMA t)
fitARIMA = undefined 

fitETS :: a
fitETS = undefined

fitNaive :: a
fitNaive = undefined
