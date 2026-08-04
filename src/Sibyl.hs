{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Sibyl
  ( module Sibyl.TimeSeries
  , module Sibyl.Model
  , mkTimeSeries
  , zipWithSeries
  , slice
  , takeLast
  , takeFirst
  , drop
  , lag
  , lead
  , diff
  , fit
  , predict
  , predictWith
  , mae, rmse, mape, mase
  , AccuracyError(..)
  , NaiveMethod(..)
  , NaiveSettings(..)
  , defaultNaiveSettings
  , fitNaive
  , fitNaiveWith
  , forecastNaive
  , TS
  ) where

import Sibyl.TimeSeries hiding (mkTimeSeries, zipWithSeries, slice, takeLast, takeFirst, drop, lag, lead, diff)
import qualified Sibyl.TimeSeries as TS
import Sibyl.Model hiding (fit, predict)
import qualified Sibyl.Model as Model
import qualified Sibyl.Accuracy as Accuracy
import Sibyl.Accuracy (mae, rmse, AccuracyError(..))
import Sibyl.Internal.Util (unsafeFromEither)
import Sibyl.Models.Naive (NaiveMethod(..), NaiveSettings(..), defaultNaiveSettings)
import qualified Sibyl.Models.Naive as Naive
import qualified Data.Vector.Unboxed as U
import Prelude hiding (drop)

type TS t y = TimeSeries t y

-- | Fits any model from its settings and a training series. Throws on error.
-- Import "Sibyl.Model" directly to use the checked 'Either' version.
fit
  :: (Model mdl, U.Unbox idx)
  => Settings mdl
  -> TimeSeries idx Double
  -> Fitted mdl idx
fit settings trainingSeries =
  unsafeFromEither "fit" (Model.fit settings trainingSeries)

-- | Predicts from a model that does not need future regressors. Throws on
-- error. This is the notebook forecasting operation.
predict
  :: (Model mdl, Future mdl ~ (), Ord idx, Enum idx, U.Unbox idx)
  => Int
  -> Fitted mdl idx
  -> Prediction idx
predict horizon fittedModel =
  unsafeFromEither "predict" (Model.predict horizon () fittedModel)

-- | Predicts from a model with explicit future inputs, such as future
-- regressors. Throws on error.
predictWith
  :: (Model mdl, Ord idx, Enum idx, U.Unbox idx)
  => Int
  -> Future mdl
  -> Fitted mdl idx
  -> Prediction idx
predictWith horizon future fittedModel =
  unsafeFromEither "predictWith" (Model.predict horizon future fittedModel)

-- | Produces a `TimeSeries` from its inputs and enforces invariants. Throws on error.
mkTimeSeries :: (Ord t, U.Unbox t, U.Unbox y) => U.Vector t -> U.Vector y -> TimeSeries t y
mkTimeSeries idx values = unsafeFromEither "mkTimeSeries" (TS.mkTimeSeries idx values)

-- | Combines two time series point-wise with a binary function. Throws on error.
zipWithSeries :: (Eq t, U.Unbox a, U.Unbox b, U.Unbox c, U.Unbox t) => (a -> b -> c) -> TimeSeries t a -> TimeSeries t b -> TimeSeries t c
zipWithSeries f tsA tsB = unsafeFromEither "zipWithSeries" (TS.zipWithSeries f tsA tsB)

-- | Returns a subseries bounded by start and end index values. Throws on error.
slice :: (Ord t, U.Unbox t, U.Unbox y) => t -> t -> TimeSeries t y -> TimeSeries t y
slice start end ts = unsafeFromEither "slice" (TS.slice start end ts)

-- | Keeps the last @k@ observations. Throws on error.
takeLast :: (U.Unbox t, U.Unbox y) => Int -> TimeSeries t y -> TimeSeries t y
takeLast k ts = unsafeFromEither "takeLast" (TS.takeLast k ts)

-- | Keeps the first @k@ observations. Throws on error.
takeFirst :: (U.Unbox t, U.Unbox y) => Int -> TimeSeries t y -> TimeSeries t y
takeFirst k ts = unsafeFromEither "takeFirst" (TS.takeFirst k ts)

-- | Shifts observations in a `TimeSeries` back by k. Throws on error.
lag :: (U.Unbox t, U.Unbox y) => Int -> TimeSeries t y -> TimeSeries t y
lag k ts = unsafeFromEither "lag" (TS.lag k ts)

-- | Shifts observations in a `TimeSeries` forward by k. Throws on error.
lead :: (U.Unbox t, U.Unbox y) => Int -> TimeSeries t y -> TimeSeries t y
lead k ts = unsafeFromEither "lead" (TS.lead k ts)

-- | Differences consecutive observations in a `TimeSeries`. Throws on error.
diff :: (Num y, U.Unbox t, U.Unbox y) => TimeSeries t y -> TimeSeries t y
diff ts = unsafeFromEither "diff" (TS.diff ts)

-- | Drops first @k@ observations. Throws on error.
drop :: (U.Unbox t, U.Unbox y) => Int -> TimeSeries t y -> TimeSeries t y
drop k ts = unsafeFromEither "drop" (TS.drop k ts)

mape :: U.Vector Double -> U.Vector Double -> Double
mape resids acts = unsafeFromEither "mape" (Accuracy.mape resids acts)

mase :: U.Vector Double -> Double -> Double
mase resids scale = unsafeFromEither "mase" (Accuracy.mase resids scale)

-- | Fits the default naive model. Throws on validation failure.
-- Import "Sibyl.Models.Naive" directly for the checked 'Either' API.
fitNaive :: U.Unbox idx => TimeSeries idx Double -> Fitted 'Naive idx
fitNaive ts = unsafeFromEither "fitNaive" (Naive.fitNaive ts)

-- | Fits a configured naive model. Throws on validation failure.
fitNaiveWith :: U.Unbox idx => NaiveSettings -> TimeSeries idx Double -> Fitted 'Naive idx
fitNaiveWith settings ts =
  unsafeFromEither "fitNaiveWith" (Naive.fitNaiveWith settings ts)

-- | Forecasts from a fitted naive model. Throws on validation failure.
forecastNaive
  :: (Ord idx, Enum idx, U.Unbox idx)
  => Int
  -> Fitted 'Naive idx
  -> Prediction idx
forecastNaive horizon fittedModel =
  predict horizon fittedModel
