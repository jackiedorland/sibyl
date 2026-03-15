module Sibyl.TimeSeries
  ( -- * Types
    TimeSeries(..)
  , AnnotatedSeries(..)
  , TimeSeriesError(..)
    -- * Construction
  , mkTimeSeries
  , annotateSeries
  , defaultTimeSeries
  , sampleTimeSeries
    -- * Accessors
  , tsLength
  , tsStart
  , tsEnd
    -- * Combination and Slicing
  , zipWithSeries
  , slice
  , takeLast
  , takeFirst
    -- * Transformations
  , mapObservations
  , mapWithIndex
  , lag
  , lead
  , diff
  ) where

import qualified Data.Vector.Unboxed as U
import Sibyl.Safe.TimeSeries
  ( AnnotatedSeries(..)
  , TimeSeries(..)
  , TimeSeriesError(..)
  , annotateSeries
  , defaultTimeSeries
  , mapObservations
  , mapWithIndex
  , sampleTimeSeries
  , tsEnd
  , tsLength
  , tsStart
  )
import qualified Sibyl.Safe.TimeSeries as Safe
import Sibyl.Internal.Util

-- | Produces a `TimeSeries` from its inputs and enforces invariants.
--
-- * 'index' and 'observations' have equal length
-- * index must be strictly increasing (i.e. 1, 2, 3 and not 1, 1, 3)
-- * series is non-empty
--
-- Requires `U.Unbox` constraints on index and observation element types.
mkTimeSeries :: (Ord t, U.Unbox t, U.Unbox y) => U.Vector t -> U.Vector y -> TimeSeries t y
mkTimeSeries idx values = unsafeFromEither "mkTimeSeries" (Safe.mkTimeSeries idx values)


-- | Combines two time series point-wise with a binary function.
zipWithSeries :: (Eq t, U.Unbox a, U.Unbox b, U.Unbox c, U.Unbox t) => (a -> b -> c) -> TimeSeries t a -> TimeSeries t b -> TimeSeries t c
zipWithSeries f tsA tsB = unsafeFromEither "zipWithSeries" (Safe.zipWithSeries f tsA tsB)

-- | Returns a subseries (`TimeSeries`) from start to end.
slice :: (Ord t, U.Unbox t, U.Unbox y) => t -> t -> TimeSeries t y -> TimeSeries t y
slice start end ts = unsafeFromEither "slice" (Safe.slice start end ts)

-- | Keeps the last @k@ observations of a `TimeSeries`.
takeLast :: (U.Unbox t, U.Unbox y) => Int -> TimeSeries t y -> TimeSeries t y
takeLast k ts = unsafeFromEither "takeLast" (Safe.takeLast k ts)

-- | Keeps the first @k@ observations of a `TimeSeries`.
takeFirst :: (U.Unbox t, U.Unbox y) => Int -> TimeSeries t y -> TimeSeries t y
takeFirst k ts = unsafeFromEither "takeFirst" (Safe.takeFirst k ts)


-- | Shifts observations in a `TimeSeries` back by k.
-- Output length is @n-k@; @k < 0@ or @k >= n@ results in `InvalidLag`
lag :: (U.Unbox t, U.Unbox y) => TimeSeries t y -> Int -> TimeSeries t y
lag ts k = unsafeFromEither "lag" (Safe.lag ts k)

-- | Shifts observations in a `TimeSeries` forward by k.
-- Output length is @n-k@; @k < 0@ or @k >= n@ results in `InvalidLead`
lead :: (U.Unbox t, U.Unbox y) => TimeSeries t y -> Int -> TimeSeries t y
lead ts k = unsafeFromEither "lead" (Safe.lead ts k)

-- | Differences consecutive observations in a `TimeSeries`.
-- This function will drop the first value in the `TimeSeries` index.
-- @n == 1@ results in InsufficientObservations
diff :: (Num y, U.Unbox t, U.Unbox y) => TimeSeries t y -> TimeSeries t y
diff ts = unsafeFromEither "diff" (Safe.diff ts)
