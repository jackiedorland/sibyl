module Sibyl
  ( module Sibyl.TimeSeries
  , module Sibyl.Model
  , mae, rmse, mape, mase
  , AccuracyError(..)
  , TS
  ) where

import Sibyl.TimeSeries hiding (name)
import Sibyl.Model
import qualified Sibyl.Accuracy as Accuracy
import Sibyl.Accuracy (mae, rmse, AccuracyError(..))
import Sibyl.Internal.Util (unsafeFromEither)
import qualified Data.Vector.Unboxed as U

type TS t y = TimeSeries t y

mape :: U.Vector Double -> U.Vector Double -> Double
mape resids acts = unsafeFromEither "mape" (Accuracy.mape resids acts)

mase :: U.Vector Double -> Double -> Double
mase resids scale = unsafeFromEither "mase" (Accuracy.mase resids scale)
