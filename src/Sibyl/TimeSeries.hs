{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Sibyl.TimeSeries where

import qualified Data.Vector.Unboxed as U
import qualified DataFrame as D
import DataFrame (DataFrame, DataFrameException)
import DataFrame.Internal.Column (Columnable)
import Data.Bifunctor (first)
import Prelude hiding (drop)
import Sibyl.Internal.Util
import qualified Data.Text as T

-- * Error Types

-- | Error type for TimeSeries invariants and transformations (i.e. lag, diff, etc.)
data TimeSeriesError
  = LengthMismatch
  | NonMonotonicIndex
  | EmptySeries
  | IndexMismatch
  | NullIndex                -- ^ One or more index values are missing
  -- Transformation errors
  | InvalidLag
  | InvalidLead
  | InvalidSlice
  | InvalidQuantity
  | InsufficientObservations
  deriving (Show, Eq)

-- | Error type for densification failures.
data DensifyError
  = EmptyAfterDrop           -- ^ 'DropMissing' would produce an empty series
  deriving (Show, Eq)

data ConversionError
  = ColumnNotFound T.Text
  | ColumnTypeMismatch T.Text
  | InvalidSeries TimeSeriesError
  | DataFrameError DataFrameException

-- * Fill Strategy

-- | Strategy for filling missing observations when converting 'TimeSeries' to 'TimeSeries''.
data FillStrategy
  = ForwardFill           -- ^ Propagate last valid observation forward
  | BackwardFill          -- ^ Propagate next valid observation backward
  | LinearInterpolation   -- ^ Linearly interpolate between adjacent valid observations
  | DropMissing           -- ^ Drop all missing observations (shrinks the series)
  deriving (Show, Eq)

-- * Nullable Time Series

-- | Univariate time series with a per-observation validity mask.
--
-- Invariants (enforced by 'mkTimeSeries'):
--
-- * 'index', 'observations', and 'validityMask' have equal length
-- * 'index' is non-empty
-- * 'index' is strictly increasing
--
-- 'validityMask': @True@ = observation is present; @False@ = observation was never recorded.
-- NaN in 'observations' is semantically distinct from @False@ in 'validityMask':
-- NaN means a computation was undefined (e.g. 0\/0); @False@ means the datum was never collected.
data TimeSeries t y = TimeSeries
  { index        :: !(U.Vector t)      -- ^ Strictly increasing index of any ordered type
  , observations :: !(U.Vector y)      -- ^ Values aligned 1:1 with 'index'
  , validityMask :: !(U.Vector Bool)   -- ^ True = present, False = missing
  } deriving (Eq, Show)

-- * Strict (Dense) Time Series

-- | Dense time series: a 'TimeSeries' with the invariant that every entry in
-- 'validityMask' is @True@.
--
-- Construct via 'mkTimeSeries'' or 'densify'. 
newtype TimeSeries' t y = TimeSeries' { unTimeSeries' :: TimeSeries t y }
  deriving (Eq, Show)

type Period = Int

-- * Series Typeclass

-- | Operations common to both 'TimeSeries' and 'TimeSeries''.
--
-- 'TimeSeries'' instances unwrap, apply the 'TimeSeries' logic, and rewrap.
-- None of these operations introduce new gaps, so 'TimeSeries'' is closed under them.
class Series s where
  diff
    :: (Num y, U.Unbox t, U.Unbox y)
    => s t y -> Either TimeSeriesError (s t y)
  lag
    :: (U.Unbox t, U.Unbox y)
    => Int -> s t y -> Either TimeSeriesError (s t y)
  lead
    :: (U.Unbox t, U.Unbox y)
    => Int -> s t y -> Either TimeSeriesError (s t y)
  slice
    :: (Ord t, U.Unbox t, U.Unbox y)
    => t -> t -> s t y -> Either TimeSeriesError (s t y)
  takeFirst
    :: (U.Unbox t, U.Unbox y)
    => Int -> s t y -> Either TimeSeriesError (s t y)
  takeLast
    :: (U.Unbox t, U.Unbox y)
    => Int -> s t y -> Either TimeSeriesError (s t y)
  drop
    :: (U.Unbox t, U.Unbox y)
    => Int -> s t y -> Either TimeSeriesError (s t y)
  zipWithSeries
    :: (Eq t, U.Unbox t, U.Unbox a, U.Unbox b, U.Unbox c)
    => (a -> b -> c) -> s t a -> s t b -> Either TimeSeriesError (s t c)
  -- | Maps a function over the observation vector.
  mapObservations
    :: (U.Vector y -> b)
    -> s t y -> b
  -- | Maps observations with access to the aligned time index.
  -- Validity is preserved: mapping does not change whether an observation was recorded.
  mapWithIndex
    :: (U.Unbox t, U.Unbox y, U.Unbox b)
    => (t -> y -> b) -> s t y -> s t b

-- ** TimeSeries instance

instance Series TimeSeries where

  -- | Differences consecutive observations. Drops the first index entry.
  diff ts
    | U.null timeIndex = Left EmptySeries
    | n == 1           = Left InsufficientObservations
    | otherwise        = Right ts
        { index        = U.drop 1 timeIndex
        , observations = U.zipWith (-) (U.drop 1 (observations ts)) (observations ts)
        , validityMask  = U.drop 1 (validityMask ts)
        }
    where
      timeIndex = index ts
      n         = U.length timeIndex

  -- | Shifts observations back by @k@. Output length is @n-k@.
  lag k ts
    | U.null timeIndex = Left EmptySeries
    | k < 0            = Left InvalidLag
    | k >= n           = Left InvalidLag
    | otherwise        = Right ts
        { index        = U.drop k timeIndex
        , observations = U.take (n - k) (observations ts)
        , validityMask  = U.take (n - k) (validityMask ts)
        }
    where
      timeIndex = index ts
      n         = U.length timeIndex

  -- | Shifts observations forward by @k@. Output length is @n-k@.
  lead k ts
    | U.null timeIndex = Left EmptySeries
    | k < 0            = Left InvalidLead
    | k >= n           = Left InvalidLead
    | otherwise        = Right ts
        { index        = U.take (n - k) timeIndex
        , observations = U.drop k (observations ts)
        , validityMask  = U.drop k (validityMask ts)
        }
    where
      timeIndex = index ts
      n         = U.length timeIndex

  -- | Returns a subseries bounded by start and end index values (inclusive).
  slice start end ts
    | start > end     = Left InvalidSlice
    | U.null newIndex = Left EmptySeries
    | otherwise       = Right ts
        { index        = newIndex
        , observations = U.slice drops remaining (observations ts)
        , validityMask  = U.slice drops remaining (validityMask ts)
        }
    where
      timeIndex = index ts
      drops     = U.length (U.takeWhile (< start) timeIndex)
      remaining = U.length (U.takeWhile (<= end) (U.drop drops timeIndex))
      newIndex  = U.slice drops remaining timeIndex

  -- | Keeps the first @k@ observations.
  takeFirst k ts
    | k < 0           = Left InvalidQuantity
    | k > n           = Left InvalidQuantity
    | U.null newIndex = Left EmptySeries
    | otherwise       = Right ts
        { index        = newIndex
        , observations = U.take k (observations ts)
        , validityMask  = U.take k (validityMask ts)
        }
    where
      timeIndex = index ts
      n         = tsLength ts
      newIndex  = U.take k timeIndex

  -- | Keeps the last @k@ observations.
  takeLast k ts
    | k < 0           = Left InvalidQuantity
    | k > n           = Left InvalidQuantity
    | U.null newIndex = Left EmptySeries
    | otherwise       = Right ts
        { index        = newIndex
        , observations = U.drop (n - k) (observations ts)
        , validityMask  = U.drop (n - k) (validityMask ts)
        }
    where
      timeIndex = index ts
      n         = tsLength ts
      newIndex  = U.drop (n - k) timeIndex

  -- | Drops the first @k@ observations.
  drop k ts
    | k < 0           = Left InvalidQuantity
    | k > n           = Left InvalidQuantity
    | U.null newIndex = Left EmptySeries
    | otherwise       = Right ts
        { index        = newIndex
        , observations = U.drop k (observations ts)
        , validityMask  = U.drop k (validityMask ts)
        }
    where
      timeIndex = index ts
      n         = tsLength ts
      newIndex  = U.drop k timeIndex

  -- | Combines two series point-wise. Indices must match exactly.
  zipWithSeries f tsA tsB
    | lenA /= lenB     = Left LengthMismatch
    | indexA /= indexB = Left IndexMismatch
    | otherwise        = Right TimeSeries
        { index        = indexA
        , observations = U.zipWith f (observations tsA) (observations tsB)
        , validityMask  = undefined -- combine validity masks
        }
    where
      lenA   = tsLength tsA
      lenB   = tsLength tsB
      indexA = index tsA
      indexB = index tsB

  mapObservations f ts = f (observations ts)

  mapWithIndex f ts =
    ts { observations = U.zipWith f (index ts) (observations ts) }

-- ** TimeSeries' instance

instance Series TimeSeries' where
  diff (TimeSeries' ts)             = fmap TimeSeries' (diff ts)
  lag k (TimeSeries' ts)            = fmap TimeSeries' (lag k ts)
  lead k (TimeSeries' ts)           = fmap TimeSeries' (lead k ts)
  slice s e (TimeSeries' ts)        = fmap TimeSeries' (slice s e ts)
  takeFirst k (TimeSeries' ts)      = fmap TimeSeries' (takeFirst k ts)
  takeLast k (TimeSeries' ts)       = fmap TimeSeries' (takeLast k ts)
  drop k (TimeSeries' ts)           = fmap TimeSeries' (drop k ts)
  zipWithSeries f (TimeSeries' a) (TimeSeries' b) = fmap TimeSeries' (zipWithSeries f a b)
  mapObservations f (TimeSeries' ts) = f (observations ts)
  mapWithIndex f (TimeSeries' ts)    = TimeSeries' (ts { observations = U.zipWith f (index ts) (observations ts) })

-- * Construction

-- | Constructs a 'TimeSeries', enforcing all invariants.
--
-- The validity mask is optional: 'Nothing' defaults to all @True@ (fully present).
-- The mask is derived automatically when constructing from a 'DataFrame' via 'fromDataFrame';
-- users should not need to construct 'U.Vector Bool' directly.
--
-- Rejects if: lengths differ, index is empty, index is not strictly increasing,
-- or any index value is null ('NullIndex').
mkTimeSeries
  :: (Ord t, U.Unbox t, U.Unbox y)
  => U.Vector t
  -> U.Vector y
  -> Maybe (U.Vector Bool)
  -> Either TimeSeriesError (TimeSeries t y)
mkTimeSeries idx values mValidity
  | ilen /= vlen                 = Left LengthMismatch
  | U.null idx                   = Left EmptySeries
  | not (strictlyIncreasing idx) = Left NonMonotonicIndex
  | otherwise                    = Right (TimeSeries idx values validity)
  where
    ilen     = U.length idx
    vlen     = U.length values
    validity = maybe (U.replicate ilen True) id mValidity

-- | Constructs a dense 'TimeSeries'', additionally rejecting if any validity entry is @False@.
mkTimeSeries'
  :: (Ord t, U.Unbox t, U.Unbox y)
  => U.Vector t
  -> U.Vector y
  -> Maybe (U.Vector Bool)
  -> Either TimeSeriesError (TimeSeries' t y)
mkTimeSeries' = undefined

-- * Densification

-- | Explicitly converts a nullable 'TimeSeries' to a dense 'TimeSeries'' using a FillStrategy.
densify
  :: (U.Unbox t, Fractional y, U.Unbox y)
  => FillStrategy
  -> TimeSeries t y
  -> Either DensifyError (TimeSeries' t y)
densify = undefined

-- * Season Slices

-- | Splits a series into full-season chunks. Trailing incomplete season is dropped.
seasonSlices :: (U.Unbox t, U.Unbox y) => Int -> TimeSeries t y -> [TimeSeries t y]
seasonSlices m ss =
  [ TimeSeries
      (U.slice (i * m) m idx)
      (U.slice (i * m) m obs)
      (U.slice (i * m) m val)
  | i <- [0 .. fullSeasons - 1] ]
  where
    idx         = index ss
    obs         = observations ss
    val         = validityMask ss
    fullSeasons = U.length obs `div` m

-- * Sample Data

-- | Provides a sample 'TimeSeries' for testing purposes.
sampleTimeSeries :: TimeSeries Int Double
sampleTimeSeries = TimeSeries
  { index        = U.fromList [1 .. 8]
  , observations = U.fromList [101.0, 103.0, 102.5, 104.0, 106.0, 105.5, 107.0, 108.0]
  , validityMask  = U.replicate 8 True
  }

-- | Synonym for 'sampleTimeSeries'.
defaultTimeSeries :: TimeSeries Int Double
defaultTimeSeries = sampleTimeSeries

-- * Summary

-- | Returns the integer length of a 'TimeSeries'.
tsLength :: (U.Unbox t) => TimeSeries t y -> Int
tsLength = U.length . index

-- | Returns the start time of a 'TimeSeries'.
tsStart :: (U.Unbox t) => TimeSeries t y -> t
tsStart = U.head . index

-- | Returns the end time of a 'TimeSeries'.
tsEnd :: (U.Unbox t) => TimeSeries t y -> t
tsEnd = U.last . index

-- * DataFrame Interop

toFromDFExcept :: Either DataFrameException a -> Either ConversionError a
toFromDFExcept = first DataFrameError

-- | Converts two DataFrame columns to a 'TimeSeries', deriving the validity mask from
-- NULL values: @(t, NULL)@ → @False@; @(t, y)@ → @True@ (even if @y@ is NaN).
-- @(NULL, y)@ is rejected; @(NULL, NULL)@ rows are dropped before construction.
fromDataFrame
  :: forall t. (Columnable t, U.Unbox t)
  => T.Text -> T.Text -> DataFrame -> Either ConversionError (TimeSeries t Double)
fromDataFrame = undefined

toDataFrame
  :: (Columnable t, Columnable y, U.Unbox t, U.Unbox y)
  => TimeSeries t y -> DataFrame
toDataFrame ts = D.fromNamedColumns
  [ (T.pack "index",        D.fromUnboxedVector (index ts))
  , (T.pack "observations", D.fromUnboxedVector (observations ts))
  ]
