module Sibyl.Smoothing
  ( sma
  , sesManual
  , ses
  , SmoothingError(..)
  ) where

import qualified Data.Vector.Unboxed as U
import Sibyl.TimeSeries
  ( TimeSeries
  , TimeSeriesError(InvalidQuantity)
  , index
  , mkTimeSeries
  , observations
  , rolling
  )
import Statistics.Sample as Sm

data SmoothingError
  = WindowTooSmall   -- ^ k <= 0
  | WindowTooLarge   -- ^ k > number of observations
  | InvalidAlpha     -- ^ alpha not strictly in (0, 1)
  | InsufficientData -- ^ fewer than 2 observations
  | InvalidTimeSeries TimeSeriesError
  deriving (Show, Eq)

-- * Simple Moving Average

-- | Trailing simple moving average with window size @k@.
-- Output length is @n - k + 1@; the index aligns to the last observation in each window.
sma :: U.Unbox t => Int -> TimeSeries t Double -> Either SmoothingError (TimeSeries t Double)
sma k ts = case rolling k Sm.mean ts of
  Left InvalidQuantity -> Left (if k <= 0 then WindowTooSmall else WindowTooLarge)
  Left e               -> Left (InvalidTimeSeries e)
  Right result         -> Right result

-- * Single Exponential Smoothing

-- | Single exponential smoothing with a manually supplied @alpha@ in @(0, 1)@.
-- Returns the level sequence aligned to the input index.
sesManual :: (Ord t, U.Unbox t) => Double -> TimeSeries t Double -> Either SmoothingError (TimeSeries t Double)
sesManual alpha ts
  | isNaN alpha || isInfinite alpha = Left InvalidAlpha
  | alpha <= 0 || alpha >= 1 = Left InvalidAlpha
  | n < 2                    = Left InsufficientData
  | otherwise                = case mkTimeSeries (index ts) levels of
      Left err     -> Left (InvalidTimeSeries err)
      Right result -> Right result
  where
    obs    = observations ts
    n      = U.length obs
    levels = U.scanl' step (U.head obs) (U.tail obs)
    step l y = alpha * y + (1 - alpha) * l

-- | Single exponential smoothing with @alpha@ chosen automatically by minimising
-- one-step-ahead squared errors. Use 'sesManual' to supply @alpha@ directly.
ses :: (Ord t, U.Unbox t) => TimeSeries t Double -> Either SmoothingError (TimeSeries t Double)
ses ts
  | n < 2     = Left InsufficientData
  | otherwise = sesManual alphaOpt ts
  where
    obs      = observations ts
    n        = U.length obs

    alphaOpt = goldenSectionMinimize sse 1e-4 (1 - 1e-4) 1e-8 200

    sse :: Double -> Double
    sse alpha = U.sum $ U.zipWith squaredError forecasts actuals
      where
        levels    = U.scanl' (\l y -> alpha * y + (1 - alpha) * l) (U.head obs) (U.tail obs)
        forecasts = U.take (n - 1) levels
        actuals   = U.drop 1 obs
        squaredError f a = (a - f) ^ (2 :: Int)

-- | Bounded one-dimensional minimization. Golden-section search is sufficient
-- for the SES objective and keeps smoothing free of a system GSL dependency.
goldenSectionMinimize
  :: (Double -> Double)
  -> Double
  -> Double
  -> Double
  -> Int
  -> Double
goldenSectionMinimize objective lower upper tolerance maxIterations =
  go lower upper leftProbe rightProbe (objective leftProbe) (objective rightProbe) maxIterations
  where
    goldenRatio = (sqrt 5 - 1) / 2
    leftProbe = upper - goldenRatio * (upper - lower)
    rightProbe = lower + goldenRatio * (upper - lower)

    go lo hi x1 x2 f1 f2 iterations
      | iterations <= 0 || hi - lo <= tolerance = (lo + hi) / 2
      | f1 <= f2 =
          let nextX1 = hi - goldenRatio * (x2 - lo)
          in go lo x2 nextX1 x1 (objective nextX1) f1 (iterations - 1)
      | otherwise =
          let nextX2 = x1 + goldenRatio * (hi - x1)
          in go x1 hi x2 nextX2 f2 (objective nextX2) (iterations - 1)
