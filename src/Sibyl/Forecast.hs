module Sibyl.Forecast where

class Model a where
  forecast  :: (U.Unbox t) => a -> Int -> Double -> ForecastResult t
  summarize :: a -> Summary
  residuals :: a -> U.Vector Double

data ForecastResult t = ForecastResult
  { point     :: TimeSeries t Double   -- h point forecasts
  , lower     :: TimeSeries t Double   -- lower bound at frLevel
  , upper     :: TimeSeries t Double   -- upper bound at frLevel
  , level     :: Double                -- e.g. 0.95
  , residuals :: U.Vector Double       -- in-sample residuals from training
  }

data Summary = Summary
  { model   :: Text
  , coeffs  :: [(Text, Double, Double)]  -- name, estimate, std error
  , sigma2  :: Double
  , aic     :: Double
  , bic     :: Double
  , LogLik  :: Double
  }