{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Sibyl.TimeSeries
  ( TimeSeries
  , Period
  , TimeSeriesError(..)
  , ConversionError(..)
  , mkTimeSeries
  , index
  , observations
  , sampleTimeSeries
  , defaultTimeSeries
  , tsLength
  , tsStart
  , tsEnd
  , mapObservations
  , mapWithIndex
  , zipWithSeries
  , slice
  , takeLast
  , takeFirst
  , drop
  , lag
  , lead
  , diff
  , diffN
  , diffSeasonal
  , diffSeasonalN
  , rolling
  , rollingMean
  , rollingVariance
  , rollingStdDev
  , rollingSum
  , rollingMin
  , rollingMax
  , rollingMedian
  , rollingCorr
  , fromDataFrame
  , toDataFrame
  ) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import DataFrame (DataFrame, DataFrameException)
import qualified DataFrame as D
import DataFrame.Internal.Column (Columnable)
import Prelude hiding (drop)
import Sibyl.Internal.Util (strictlyIncreasing)
import qualified Statistics.Sample as Sm

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
  | UndefinedCorrelation
  deriving (Show, Eq)

data ConversionError
  = ColumnNotFound T.Text
  | ColumnTypeMismatch T.Text
  | InvalidSeries TimeSeriesError
  | DataFrameError DataFrameException
  deriving (Show)

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
  { timeSeriesIndex        :: !(U.Vector t)
  , timeSeriesObservations :: !(U.Vector y)
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

-- | Returns the time index. The returned immutable vector cannot be used to
-- mutate the series or violate its construction invariants.
index :: TimeSeries t y -> U.Vector t
index = timeSeriesIndex

-- | Returns observations aligned one-to-one with 'index'.
observations :: TimeSeries t y -> U.Vector y
observations = timeSeriesObservations

-- ** Sample Data

-- | Provides a sample `TimeSeries` for testing purposes.
sampleTimeSeries :: TimeSeries Int Double
sampleTimeSeries =
  TimeSeries
    (U.fromList [1 .. 8])
    (U.fromList [101.0, 103.0, 102.5, 104.0, 106.0, 105.5, 107.0, 108.0])

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
  TimeSeries (index ts) (U.zipWith f (index ts) (observations ts))

-- ** Combination And Slicing

-- | Combines two time series point-wise with a binary function.
zipWithSeries :: (Eq t, U.Unbox a, U.Unbox b, U.Unbox c, U.Unbox t) => (a -> b -> c) -> TimeSeries t a -> TimeSeries t b -> Either TimeSeriesError (TimeSeries t c)
zipWithSeries f tsA tsB
  | lenA /= lenB     = Left LengthMismatch
  | indexA /= indexB = Left IndexMismatch
  | otherwise        = Right (TimeSeries indexA zipped)
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
  | otherwise        = Right (TimeSeries newIndex newObs)
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
  | otherwise        = Right (TimeSeries newIndex newObs)
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
  | otherwise        = Right (TimeSeries newIndex newObs)
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
  | otherwise        = Right (TimeSeries newIndex newObs)
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
  | otherwise                 = Right (TimeSeries
      (U.drop k timeIndex)
      (U.take (n - k) obs))
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
  | otherwise                 = Right (TimeSeries
      (U.take (n - k) timeIndex)
      (U.drop k obs))
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
  | otherwise                = Right (TimeSeries
      (U.drop 1 timeIndex)
      (U.zipWith (-) (U.drop 1 (observations ts)) (observations ts))) -- y' = y_t - y_(t-1)
  where
    timeIndex = index ts
    n = U.length timeIndex

-- | Applies 'diff' @k@ times, dropping the first @k@ values from the index.
-- @k < 0@ returns 'InvalidQuantity'; @k == 0@ returns the series unchanged.
diffN :: (Num y, U.Unbox t, U.Unbox y) => Int -> TimeSeries t y -> Either TimeSeriesError (TimeSeries t y)
diffN k ts
  | k < 0     = Left InvalidQuantity
  | k == 0    = Right ts
  | otherwise = foldM (\acc _ -> diff acc) ts [1..k]

-- | Seasonal differencing with period @m@: computes @y_t - y_{t-m}@.
-- Output length is @n - m@; the first @m@ index entries are dropped.
-- Requires @m >= 1@ and at least @m + 1@ observations.
diffSeasonal :: (Num y, U.Unbox t, U.Unbox y) => Int -> TimeSeries t y -> Either TimeSeriesError (TimeSeries t y)
diffSeasonal m ts
  | m < 1     = Left InvalidQuantity
  | n <= m    = Left InsufficientObservations
  | otherwise = Right (TimeSeries
      (U.drop m timeIndex)
      (U.zipWith (-) (U.drop m obs) (U.take (n - m) obs)))
  where
    timeIndex = index ts
    obs       = observations ts
    n         = U.length timeIndex

-- | Applies 'diffSeasonal' @k@ times with the same period @m@.
-- @k < 0@ returns 'InvalidQuantity'; @k == 0@ returns the series unchanged.
diffSeasonalN :: (Num y, U.Unbox t, U.Unbox y) => Int -> Int -> TimeSeries t y -> Either TimeSeriesError (TimeSeries t y)
diffSeasonalN k m ts
  | k < 0     = Left InvalidQuantity
  | k == 0    = Right ts
  | otherwise = foldM (\acc _ -> diffSeasonal m acc) ts [1..k]

-- | Applies @f@ to each trailing window of size @k@, producing a series of length @n - k + 1@.
-- The output index aligns to the last observation in each window.
rolling :: (U.Unbox t, U.Unbox y, U.Unbox b)
        => Int
        -> (U.Vector y -> b)
        -> TimeSeries t y
        -> Either TimeSeriesError (TimeSeries t b)
rolling k f ts
  | k <= 0    = Left InvalidQuantity
  | k > n     = Left InvalidQuantity
  | otherwise = Right (TimeSeries
      (U.drop (k - 1) (index ts))
      (U.generate (n - k + 1) (f . window)))
  where
    obs    = observations ts
    n      = U.length obs
    window i = U.slice i k obs

-- | Rolling arithmetic mean over a trailing window of size @k@.
rollingMean :: U.Unbox t => Int -> TimeSeries t Double -> Either TimeSeriesError (TimeSeries t Double)
rollingMean k = rolling k Sm.mean

-- | Rolling sample variance over a trailing window of size @k@.
rollingVariance :: U.Unbox t => Int -> TimeSeries t Double -> Either TimeSeriesError (TimeSeries t Double)
rollingVariance k
  | k < 2     = const (Left InvalidQuantity)
  | otherwise = rolling k Sm.varianceUnbiased

-- | Rolling sample standard deviation over a trailing window of size @k@.
rollingStdDev :: U.Unbox t => Int -> TimeSeries t Double -> Either TimeSeriesError (TimeSeries t Double)
rollingStdDev k
  | k < 2     = const (Left InvalidQuantity)
  | otherwise = rolling k Sm.stdDev

-- | Rolling sum over a trailing window of size @k@.
rollingSum :: (U.Unbox t, Num y, U.Unbox y) => Int -> TimeSeries t y -> Either TimeSeriesError (TimeSeries t y)
rollingSum k = rolling k U.sum

-- | Rolling minimum over a trailing window of size @k@.
rollingMin :: (U.Unbox t, Ord y, U.Unbox y) => Int -> TimeSeries t y -> Either TimeSeriesError (TimeSeries t y)
rollingMin k = rolling k U.minimum

-- | Rolling maximum over a trailing window of size @k@.
rollingMax :: (U.Unbox t, Ord y, U.Unbox y) => Int -> TimeSeries t y -> Either TimeSeriesError (TimeSeries t y)
rollingMax k = rolling k U.maximum

-- | Rolling median over a trailing window of size @k@.
-- Unlike the aggregations above, this sorts each window in O(k log k) per step.
rollingMedian :: U.Unbox t => Int -> TimeSeries t Double -> Either TimeSeriesError (TimeSeries t Double)
rollingMedian k = rolling k median
  where
    median values =
      let sorted = sort (U.toList values)
          n = length sorted
          midpoint = n `div` 2
      in if odd n
           then sorted !! midpoint
           else (sorted !! (midpoint - 1) + sorted !! midpoint) / 2

-- | Rolling Pearson correlation between two series over a trailing window of size @k@.
-- Both series must have the same index.
rollingCorr :: (Eq t, U.Unbox t) => Int -> TimeSeries t Double -> TimeSeries t Double -> Either TimeSeriesError (TimeSeries t Double)
rollingCorr k tsA tsB
  | index tsA /= index tsB = Left IndexMismatch
  | k < 2                  = Left InvalidQuantity
  | k > n                  = Left InvalidQuantity
  | otherwise = do
      correlations <- traverse correlationAt [0 .. n - k]
      Right (TimeSeries (U.drop (k - 1) (index tsA)) (U.fromList correlations))
  where
    n = tsLength tsA
    obsA = observations tsA
    obsB = observations tsB

    correlationAt offset =
      let windowA = U.slice offset k obsA
          windowB = U.slice offset k obsB
          meanA = Sm.mean windowA
          meanB = Sm.mean windowB
          centeredA = U.map (subtract meanA) windowA
          centeredB = U.map (subtract meanB) windowB
          numerator = U.sum (U.zipWith (*) centeredA centeredB)
          denominator = sqrt (U.sum (U.map (^ (2 :: Int)) centeredA)
                            * U.sum (U.map (^ (2 :: Int)) centeredB))
      in if denominator == 0
           then Left UndefinedCorrelation
           else Right (numerator / denominator)

toFromDFExcept :: Either DataFrameException a -> Either ConversionError a
toFromDFExcept = first DataFrameError

fromDataFrame :: forall t. (Columnable t, U.Unbox t)
              => T.Text -> T.Text -> DataFrame -> Either ConversionError (TimeSeries t Double)
fromDataFrame colA colB df = do
  indexV <- toFromDFExcept $ D.columnAsUnboxedVector (D.col @t colA) df
  obsV   <- toFromDFExcept $ D.columnAsDoubleVector  (D.col @Double colB) df -- TODO: change to castWith when DataFrame compiles on my stack version (weird pragma B.S.)
  first InvalidSeries $ mkTimeSeries indexV obsV

toDataFrame   :: (Columnable t, Columnable y, U.Unbox t, U.Unbox y)
              => TimeSeries t y -> DataFrame
toDataFrame ts = D.fromNamedColumns
  [ (T.pack "index",        D.fromUnboxedVector (index ts))
  , (T.pack "observations", D.fromUnboxedVector (observations ts))
  ]
