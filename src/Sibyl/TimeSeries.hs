module Sibyl.TimeSeries where

import qualified Data.Vector.Unboxed as U
import Prelude hiding (drop)
import Sibyl.Internal.Util

-- * Error Types

-- | Error type for TimeSeries invariants and TimeSeries transformations (i.e. lag, diff, etc...)
data TimeSeriesError
  = LengthMismatch
  | NonMonotonicIndex
  | EmptySeries
  | IndexMismatch
  -- Transformation errors
  | InvalidLag
  | InvalidLead
  | InvalidSlice
  | InvalidQuantity
  | InsufficientObservations
  deriving (Show, Eq)

-- * Unboxed Time Series

-- | Core univariate time series type internally represented by unboxed vectors.
--
-- Invariants (enforced by 'mkTimeSeries'):
--
-- * 'index' and 'observations' have equal length
-- * index must be strictly increasing (i.e. 1, 2, 3 and not 1, 1, 3)
-- * series is non-empty
--
-- Prefer using 'mkTimeSeries' instead of record literals... you'll thank yourself later!
--
-- 'index' is the time axis and may be any ordered type (e.g. Int, Day, POSIX-like values),
-- not only consecutive integers.
--
-- This type does not have a 'Functor' instance because mapping over unboxed vectors
-- requires additional 'U.Unbox' constraints on output types.
data TimeSeries t y = TimeSeries
  { index        :: !(U.Vector t)   -- ^ Strictly increasing index of any ordered type.
  , observations :: !(U.Vector y)   -- ^ Values (observations) aligned 1:1 with 'index'
  } deriving (Eq, Show)

type Period = Int

-- ** Construction

-- | Produces a `TimeSeries` from its inputs and enforces invariants.
--
-- * 'index' and 'observations' have equal length
-- * index must be strictly increasing (i.e. 1, 2, 3 and not 1, 1, 3)
-- * series is non-empty
--
-- Requires `U.Unbox` constraints on index and observation element types.
mkTimeSeries :: (Ord t, U.Unbox t, U.Unbox y) => U.Vector t -> U.Vector y -> Either TimeSeriesError (TimeSeries t y)
mkTimeSeries idx values
  | ilen /= vlen                    = Left LengthMismatch
  | U.null idx                      = Left EmptySeries
  | not (strictlyIncreasing idx)    = Left NonMonotonicIndex
  | otherwise                       = Right (TimeSeries idx values)
  where
    ilen = U.length idx
    vlen = U.length values

-- | Splits a 'SeasonalSeries' into full-season chunks. Trailing incomplete season is dropped.
seasonSlices :: (U.Unbox t, U.Unbox y) => Int -> TimeSeries t y -> [TimeSeries t y]
seasonSlices m ss =
  [ TimeSeries (U.slice (i * m) m idx) (U.slice (i * m) m obs)
  | i <- [0 .. fullSeasons - 1] ]
  where
    idx         = index ss
    obs         = observations ss
    fullSeasons = U.length obs `div` m

-- ** Sample Data

-- | Provides a sample `TimeSeries` for testing purposes.
sampleTimeSeries :: TimeSeries Int Double
sampleTimeSeries =
  TimeSeries
    { index = U.fromList [1 .. 8]
    , observations = U.fromList [101.0, 103.0, 102.5, 104.0, 106.0, 105.5, 107.0, 108.0]
    }

-- | Synonym for `sampleTimeSeries`
defaultTimeSeries :: TimeSeries Int Double
defaultTimeSeries = sampleTimeSeries

-- ** Summary

-- | Returns integer length of a `TimeSeries`
tsLength :: (U.Unbox t) => TimeSeries t y -> Int
tsLength = U.length . index

-- | Returns the start time of a `TimeSeries`
tsStart :: (U.Unbox t) => TimeSeries t y -> t
tsStart = U.head . index

-- | Returns the end time of a `TimeSeries`
tsEnd :: (U.Unbox t) => TimeSeries t y -> t
tsEnd = U.last . index

-- ** Transformations

-- | Maps a function @f :: U.Vector y -> b@ over a `TimeSeries`.
--
-- __Example:__ using `Statistics.Sample` functions on a `TimeSeries`:
--
-- >>> mapObservations (Statistics.Sample.mean) (defaultTimeSeries)
-- 104.625
--
mapObservations :: (U.Vector y -> b) -> TimeSeries t y -> b
mapObservations f = f . observations

-- | Maps observations with access to the aligned time index.
mapWithIndex :: (U.Unbox t, U.Unbox y, U.Unbox b) => (t -> y -> b) -> TimeSeries t y -> TimeSeries t b
mapWithIndex f ts =
  ts { observations = U.zipWith f (index ts) (observations ts) }

-- ** Combination And Slicing

-- | Combines two time series point-wise with a binary function.
zipWithSeries :: (Eq t, U.Unbox a, U.Unbox b, U.Unbox c, U.Unbox t) => (a -> b -> c) -> TimeSeries t a -> TimeSeries t b -> Either TimeSeriesError (TimeSeries t c)
zipWithSeries f tsA tsB
  | lenA /= lenB     = Left LengthMismatch
  | indexA /= indexB = Left IndexMismatch
  | otherwise        = Right TimeSeries { index = indexA, observations = zipped }
  where
    lenA = tsLength tsA
    lenB = tsLength tsB
    indexA = index tsA
    indexB = index tsB
    obsA = observations tsA
    obsB = observations tsB
    zipped = U.zipWith f obsA obsB

-- | Returns a subseries bounded by start and end index values.
slice :: (Ord t, U.Unbox t, U.Unbox y) => t -> t -> TimeSeries t y -> Either TimeSeriesError (TimeSeries t y)
slice start end ts
  | start > end      = Left InvalidSlice
  | U.null newIndex  = Left EmptySeries
  | otherwise        = Right ts { index = newIndex, observations = newObs }
  where
    timeIndex = index ts
    obs = observations ts
    drops = U.length (U.takeWhile (< start) timeIndex)
    remaining = U.length (U.takeWhile (<= end) (U.drop drops timeIndex))
    newIndex = U.slice drops remaining timeIndex
    newObs = U.slice drops remaining obs

-- | Keeps the last @k@ observations.
takeLast :: (U.Unbox t, U.Unbox y) => Int -> TimeSeries t y -> Either TimeSeriesError (TimeSeries t y)
takeLast k ts
  | k < 0            = Left InvalidQuantity
  | k > n            = Left InvalidQuantity
  | U.null newIndex  = Left EmptySeries
  | otherwise        = Right ts { index = newIndex, observations = newObs }
  where
    timeIndex = index ts
    obs = observations ts
    n = tsLength ts
    newIndex = U.drop (n - k) timeIndex
    newObs = U.drop (n - k) obs

-- | Keeps the first @k@ observations.
takeFirst :: (U.Unbox t, U.Unbox y) => Int -> TimeSeries t y -> Either TimeSeriesError (TimeSeries t y)
takeFirst k ts
  | k < 0            = Left InvalidQuantity
  | k > n            = Left InvalidQuantity
  | U.null newIndex  = Left EmptySeries
  | otherwise        = Right ts { index = newIndex, observations = newObs }
  where
    timeIndex = index ts
    obs = observations ts
    n = tsLength ts
    newIndex = U.take k timeIndex
    newObs = U.take k obs

-- | Drops first @k@ observations.
drop :: (U.Unbox t, U.Unbox y) => Int -> TimeSeries t y -> Either TimeSeriesError (TimeSeries t y)
drop k ts
  | k < 0            = Left InvalidQuantity
  | k > n            = Left InvalidQuantity
  | U.null newIndex  = Left EmptySeries
  | otherwise        = Right ts { index = newIndex, observations = newObs }
  where
    timeIndex = index ts
    obs = observations ts
    n = tsLength ts
    newIndex = U.drop k timeIndex
    newObs = U.drop k obs

-- ** Transformations

-- | Shifts observations in a `TimeSeries` back by k.
-- Output length is @n-k@; @k < 0@ or @k >= n@ results in `InvalidLag`
lag :: (U.Unbox t, U.Unbox y) => Int -> TimeSeries t y -> Either TimeSeriesError (TimeSeries t y)
lag k ts
  | U.null timeIndex          = Left EmptySeries
  | k < 0                     = Left InvalidLag
  | k >= n                    = Left InvalidLag
  | otherwise                 = Right ts
      { index = U.drop k timeIndex
      , observations = U.take (n - k) obs
      }
  where
    timeIndex = index ts
    obs = observations ts
    n = U.length timeIndex

-- | Shifts observations in a `TimeSeries` forward by k.
-- Output length is @n-k@; @k < 0@ or @k >= n@ results in `InvalidLead`
lead :: (U.Unbox t, U.Unbox y) => Int -> TimeSeries t y -> Either TimeSeriesError (TimeSeries t y)
lead k ts
  | U.null timeIndex          = Left EmptySeries
  | k < 0                     = Left InvalidLead
  | k >= n                    = Left InvalidLead
  | otherwise                 = Right ts
      { index = U.take (n - k) timeIndex
      , observations = U.drop k obs
      }
  where
    timeIndex = index ts
    obs = observations ts
    n = U.length timeIndex

-- | Differences consecutive observations in a `TimeSeries`.
-- This function will drop the first value in the `TimeSeries` index.
diff :: (Num y, U.Unbox t, U.Unbox y) => TimeSeries t y -> Either TimeSeriesError (TimeSeries t y)
diff ts
  | U.null timeIndex         = Left EmptySeries
  | n == 1                   = Left InsufficientObservations
  | otherwise                = Right ts
      { index = U.drop 1 timeIndex
      , observations = U.zipWith (-) (U.drop 1 (observations ts)) (observations ts) -- y' = y_t - y_(t-1)
      }
  where
    timeIndex = index ts
    n = U.length timeIndex
