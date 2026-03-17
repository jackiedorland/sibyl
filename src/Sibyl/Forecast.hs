module Sibyl.Forecast where

import qualified Data.Vector.Unboxed as U
import Sibyl.Safe.TimeSeries (TimeSeries)

data Forecast t = Forecast
  { point     :: TimeSeries t Double -- h point forecasts
  , lower     :: TimeSeries t Double -- lower bound at level
  , upper     :: TimeSeries t Double -- upper bound at level
  , ciLevel   :: Double              -- e.g. 0.95
  , residuals :: U.Vector Double     -- in-sample residuals from training
  , actuals   :: U.Vector Double     -- actuals from training
  }

data ForecastError
    = InvalidHorizon    String   -- e.g. h <= 0
    | InsufficientHistory String -- series too short for requested horizon
    | RegressorMismatch String   -- column count or name mismatch (X variants)
    deriving (Show, Eq)