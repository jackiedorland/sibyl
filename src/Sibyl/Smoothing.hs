module Sibyl.Smoothing
  ( sma
  , sesManual
  , ses
  , SmoothingError(..)
  ) where

import qualified Data.Vector.Unboxed as U
import Numeric.GSL.Minimization (minimizeV, MinimizeMethod(..))
import Numeric.LinearAlgebra (Vector, atIndex, fromList)
import Sibyl.TimeSeries (TimeSeries(..), observations, TimeSeriesError (InvalidQuantity), rolling)
import Statistics.Sample as Sm

data SmoothingError
  = WindowTooSmall   -- ^ k <= 0
  | WindowTooLarge   -- ^ k > number of observations
  | InvalidAlpha     -- ^ alpha not strictly in (0, 1)
  | InsufficientData -- ^ fewer than 2 observations
  deriving (Show, Eq)

-- * Simple Moving Average

-- | Trailing simple moving average with window size @k@.
-- Output length is @n - k + 1@; the index aligns to the last observation in each window.
sma :: U.Unbox t => Int -> TimeSeries t Double -> Either SmoothingError (TimeSeries t Double)
sma k ts = case rolling k Sm.mean ts of
  Left InvalidQuantity -> Left (if k <= 0 then WindowTooSmall else WindowTooLarge)
  Left e               -> error ("sma: unexpected error " ++ show e)
  Right result         -> Right result

-- * Single Exponential Smoothing

-- | Single exponential smoothing with a manually supplied @alpha@ in @(0, 1)@.
-- Returns the level sequence aligned to the input index.
sesManual :: U.Unbox t => Double -> TimeSeries t Double -> Either SmoothingError (TimeSeries t Double)
sesManual alpha ts
  | alpha <= 0 || alpha >= 1 = Left InvalidAlpha
  | n < 2                    = Left InsufficientData
  | otherwise                = Right ts { observations = levels }
  where
    obs    = observations ts
    n      = U.length obs
    levels = U.scanl' step (U.head obs) (U.tail obs)
    step l y = alpha * y + (1 - alpha) * l

-- | Single exponential smoothing with @alpha@ chosen automatically by minimising
-- one-step-ahead squared errors. Use 'sesManual' to supply @alpha@ directly.
ses :: U.Unbox t => TimeSeries t Double -> Either SmoothingError (TimeSeries t Double)
ses ts
  | n < 2     = Left InsufficientData
  | otherwise = sesManual alphaOpt ts
  where
    obs      = observations ts
    n        = U.length obs

    (rawSol, _path) = minimizeV NMSimplex2 tolerance maxIters simplexStep sse initialGuess
    tolerance   = 1e-8  :: Double
    maxIters    = 200   :: Int
    simplexStep = fromList [0.3]
    initialGuess = fromList [0.5]

    -- clamp to (0,1) b/c we dont want it to degen
    alphaOpt = max 1e-4 (min (1 - 1e-4) (rawSol `atIndex` 0))

    sse :: Vector Double -> Double
    sse v = U.sum $ U.zipWith squaredError forecasts actuals
      where
        alpha     = v `atIndex` 0
        levels    = U.scanl' (\l y -> alpha * y + (1 - alpha) * l) (U.head obs) (U.tail obs)
        forecasts = U.take (n - 1) levels
        actuals   = U.drop 1 obs
        squaredError f a = (a - f) ^ (2 :: Int)
