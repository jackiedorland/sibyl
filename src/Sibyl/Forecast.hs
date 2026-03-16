module Sibyl.Forecast where

import qualified Data.Vector.Unboxed as U
import Sibyl.Safe.TimeSeries (TimeSeries)

data ForecastResult t = ForecastResult
  { point     :: TimeSeries t Double -- h point forecasts
  , lower     :: TimeSeries t Double -- lower bound at frLevel
  , upper     :: TimeSeries t Double -- upper bound at frLevel
  , level     :: Double              -- e.g. 0.95
  , residuals :: U.Vector Double     -- in-sample residuals from training
  , actuals   :: U.Vector Double     -- 
  }

type Forecast t = ForecastResult t