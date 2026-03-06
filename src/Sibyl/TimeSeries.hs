{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Sibyl.TimeSeries where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as B
import Sibyl.Internal.Util

-- * Error Types

-- | Error type for TimeSeries invariants and TimeSeries transformations (i.e. lag, diff, etc...)
data TimeSeriesError 
  = LengthMismatch 
  | NonMonotonicIndex 
  | EmptySeries 
  -- Transformation errors
  | InvalidLag 
  | InvalidLead
  deriving (Show, Eq)

-- * Unboxed Time Series

-- | Core univariate time series type internally represented by unboxed vectors.
--
-- Invariants (enforced by 'mkTimeSeries'):
--
-- * 'tsIndex' and 'tsObservations' have equal length
-- * index must be strictly increasing (i.e. 1, 2, 3 and not 1, 1, 3)
-- * series is non-empty
--
-- Prefer using 'mkTimeSeries' instead of record literals... you'll thank yourself later!
--
-- 'tsIndex' is the time axis and may be any ordered type (e.g. Int, Day, POSIX-like values),
-- not only consecutive integers.
--
-- This type does not have a 'Functor' instance because mapping over unboxed vectors
-- requires additional 'U.Unbox' constraints on output types.
data TimeSeries t y = TimeSeries
  { tsIndex        :: !(U.Vector t)   -- ^ Strictly increasing index of any ordered type.
  , tsObservations :: !(U.Vector y)   -- ^ Values (observations) aligned 1:1 with tsIndex
  , tsName         :: Maybe Text      -- ^ Optional metaname for the series
  , tsDescription  :: Maybe Text      -- ^ Optional description for the series
  } deriving (Eq, Show)

-- ** Construction

-- | Produces a `TimeSeries` from its inputs and enforces invariants. 
-- 
-- * 'tsIndex' and 'tsObservations' have equal length
-- * index must be strictly increasing (i.e. 1, 2, 3 and not 1, 1, 3)
-- * series is non-empty
-- 
-- Requires `U.Unbox` constraints on index and observation element types.
mkTimeSeries :: (Ord t, U.Unbox t, U.Unbox y) => U.Vector t -> U.Vector y -> Maybe Text -> Maybe Text -> Either TimeSeriesError (TimeSeries t y)
mkTimeSeries index values name desc 
  | ilen /= vlen                      = Left LengthMismatch
  | U.null index                      = Left EmptySeries
  | not (strictlyIncreasing index)    = Left NonMonotonicIndex 
  | otherwise                         = Right (TimeSeries index values name desc)
  where 
    ilen = U.length index
    vlen = U.length values

-- ** Sample Data

-- | Provides a sample `TimeSeries` for testing purposes.
sampleTimeSeries :: TimeSeries Int Double
sampleTimeSeries =
  TimeSeries
    { tsIndex = U.fromList [1 .. 8]
    , tsObservations = U.fromList [101.0, 103.0, 102.5, 104.0, 106.0, 105.5, 107.0, 108.0]
    , tsName = Just "sample"
    , tsDescription = Just "sample TimeSeries"
    }

-- | Synonym for `sampleTimeSeries`
defaultTimeSeries :: TimeSeries Int Double
defaultTimeSeries = sampleTimeSeries

-- ** Summary

-- | Returns integer length of a `TimeSeries`
tsLength :: (U.Unbox t) => TimeSeries t y -> Int
tsLength = U.length . tsIndex

-- | Returns the start time of a `TimeSeries`
tsStart :: (U.Unbox t) => TimeSeries t y -> t
tsStart = U.head . tsIndex 

-- | Returns the end time of a `TimeSeries`
tsEnd :: (U.Unbox t) => TimeSeries t y -> t
tsEnd = U.last . tsIndex

-- ** Transformations

-- | Maps a function /f :: U.Vector y -> b/ over a `TimeSeries`.
-- 
-- __Example:__ using `Statistics.Sample` functions on a `TimeSeries`:
-- 
-- >>> mapObservations (Statistics.Sample.mean) (defaultTimeSeries)
-- 104.625
-- 
mapObservations :: (U.Vector y -> b) -> TimeSeries t y -> b
mapObservations f = f . tsObservations

-- | Maps observations with access to the aligned time index.
mapWithIndex :: (U.Unbox t, U.Unbox y, U.Unbox b) => (t -> y -> b) -> TimeSeries t y -> TimeSeries t b
mapWithIndex f ts =
  ts { tsObservations = U.zipWith f (tsIndex ts) (tsObservations ts) }

-- ** Combination And Slicing

-- | Combines two time series point-wise with a binary function.
zipWithSeries :: (y -> b -> c) -> TimeSeries t y -> TimeSeries t b -> Either TimeSeriesError (TimeSeries t c)
zipWithSeries = undefined

-- | Returns a subseries bounded by start and end index values.
slice :: t -> t -> TimeSeries t y -> TimeSeries t y
slice = undefined

-- | Keeps the last @n@ observations.
takeLast :: Int -> TimeSeries t y -> TimeSeries t y
takeLast = undefined

-- | Keeps the first @n@ observations.
takeFirst :: Int -> TimeSeries t y -> TimeSeries t y
takeFirst = undefined

-- ** Transformations

-- | Shifts observations in a `TimeSeries` back by k.
-- Output length is @n-k@; @k < 0@ or @k >= n@ results in `InvalidLag`
lag :: (U.Unbox t, U.Unbox y) => TimeSeries t y -> Int -> Either TimeSeriesError (TimeSeries t y)
lag ts k
  | U.null timeIndex          = Left EmptySeries
  | k < 0                     = Left InvalidLag
  | k >= n                    = Left InvalidLag
  | otherwise                 = Right ts
      { tsIndex = U.drop k timeIndex
      , tsObservations = U.take (n - k) obs
      }
  where 
    timeIndex = tsIndex ts
    obs = tsObservations ts
    n = U.length timeIndex

-- | Shifts observations in a `TimeSeries` forward by k.
-- Output length is @n-k@; @k < 0@ or @k >= n@ results in `InvalidLead`
lead :: (U.Unbox t, U.Unbox y) => TimeSeries t y -> Int -> Either TimeSeriesError (TimeSeries t y)
lead ts k
  | U.null timeIndex          = Left EmptySeries
  | k < 0                     = Left InvalidLead
  | k >= n                    = Left InvalidLead
  | otherwise                 = Right ts
      { tsIndex = U.take (n - k) timeIndex
      , tsObservations = U.drop k obs
      }
  where 
    timeIndex = tsIndex ts
    obs = tsObservations ts
    n = U.length timeIndex

-- * Boxed Time Series

-- | Core univariate time series type internally represented by boxed vectors.
--
-- Invariants (enforced by 'mkBTimeSeries'):
--
-- * 'btsIndex' and 'btsObservations' have equal length
-- * index must be strictly increasing (i.e. 1, 2, 3 and not 1, 0.5, 5)
-- * series is non-empty
--
-- Prefer using 'mkBTimeSeries' instead of record literals... you'll thank yourself later!
--
-- 'btsIndex' is the time axis and may be any ordered type (e.g. Int, Day, POSIX-like values),
-- not only consecutive integers.
--
-- This type derives Functor, Foldable and Traversable at the cost of raw performance. 
-- Boxed vectors are a little more flexible but less memory/cache efficient. 
data BTimeSeries t y = BTimeSeries
  { btsIndex        :: !(B.Vector t)   -- ^ Strictly increasing index of any ordered type.
  , btsObservations :: !(B.Vector y)   -- ^ Values (observations) aligned 1:1 with btsIndex
  , btsName         :: Maybe Text      -- ^ Optional metaname for the series
  , btsDescription  :: Maybe Text      -- ^ Optional description for the series
  } deriving (Eq, Show, Functor, Foldable, Traversable)

-- ** Construction

-- | Produces a `BTimeSeries` from its inputs and enforces invariants. 
-- 
-- * 'btsIndex' and 'btsObservations' have equal length
-- * index must be strictly increasing (i.e. 1, 2, 3 and not 1, 1, 3)
-- * series is non-empty
mkBTimeSeries :: (Ord t) => B.Vector t -> B.Vector y -> Maybe Text -> Maybe Text -> Either TimeSeriesError (BTimeSeries t y)
mkBTimeSeries index values name desc 
  | ilen /= vlen                       = Left LengthMismatch
  | B.null index                       = Left EmptySeries
  | not (bStrictlyIncreasing index)    = Left NonMonotonicIndex
  | otherwise                          = Right (BTimeSeries index values name desc)
  where 
    ilen = B.length index
    vlen = B.length values

-- ** Sample Data

-- | Provides a sample `BTimeSeries` for testing purposes.
sampleBTimeSeries :: BTimeSeries Int Double
sampleBTimeSeries =
  BTimeSeries
    { btsIndex = B.fromList [1 .. 8]
    , btsObservations = B.fromList [101.0, 103.0, 102.5, 104.0, 106.0, 105.5, 107.0, 108.0]
    , btsName = Just "sample"
    , btsDescription = Just "sample BTimeSeries"
    }

-- | Synonym for `sampleBTimeSeries`
defaultBTimeSeries :: BTimeSeries Int Double
defaultBTimeSeries = sampleBTimeSeries

-- ** Combination And Slicing

-- | Combines two time series point-wise with a binary function.
zipWithBSeries :: (y -> b -> c) -> TimeSeries t y -> TimeSeries t b -> Either TimeSeriesError (TimeSeries t c)
zipWithBSeries = undefined

-- | Returns a subseries bounded by start and end index values.
sliceB :: t -> t -> TimeSeries t y -> TimeSeries t y
sliceB = undefined

-- | Keeps the last @n@ observations.
takeLastB :: Int -> TimeSeries t y -> TimeSeries t y
takeLastB = undefined

-- | Keeps the first @n@ observations.
takeFirstB :: Int -> TimeSeries t y -> TimeSeries t y
takeFirstB = undefined

-- ** Transformations

-- | Shifts observations in a `BTimeSeries` back by k.
-- Output length is @n-k@; @k < 0@ or @k >= n@ results in `InvalidLag`
lagB :: BTimeSeries t y -> Int -> Either TimeSeriesError (BTimeSeries t y)
lagB ts k
  | B.null timeIndex          = Left EmptySeries
  | k < 0                     = Left InvalidLag
  | k >= n                    = Left InvalidLag
  | otherwise                 = Right ts
      { btsIndex = B.drop k timeIndex
      , btsObservations = B.take (n - k) obs
      }
  where 
    timeIndex = btsIndex ts
    obs = btsObservations ts
    n = B.length timeIndex

-- | Shifts observations in a `BTimeSeries` forward by k.
-- Output length is @n-k@; @k < 0@ or @k >= n@ results in `InvalidLead`
leadB :: BTimeSeries t y -> Int -> Either TimeSeriesError (BTimeSeries t y)
leadB bts k
  | B.null timeIndex          = Left EmptySeries
  | k < 0                     = Left InvalidLead
  | k >= n                    = Left InvalidLead
  | otherwise                 = Right bts
      { btsIndex = B.take (n - k) timeIndex
      , btsObservations = B.drop k obs
      }
  where 
    timeIndex = btsIndex bts
    obs = btsObservations bts
    n = B.length timeIndex

-- ** Summary

-- | Returns integer length of a `BTimeSeries` 
btsLength :: BTimeSeries t y -> Int
btsLength = B.length . btsIndex

-- | Returns the start time of a `BTimeSeries` 
btsStart :: BTimeSeries t y -> t
btsStart = B.head . btsIndex

-- | Returns the end time of a `BTimeSeries`
btsEnd :: BTimeSeries t y -> t
btsEnd = B.last . btsIndex
