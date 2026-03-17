module Sibyl
  ( module Sibyl.TimeSeries
  , module Sibyl.Model
  , module Sibyl.Forecast
  , mae, rmse, mape, mase, maseFromModel
  , AccuracyError(..)
  , TS
  , FC
  ) where

import Sibyl.TimeSeries hiding (name)
import Sibyl.Model
import Sibyl.Forecast
import qualified Sibyl.Accuracy as Accuracy
import Sibyl.Accuracy (mae, rmse, maseFromModel, AccuracyError(..))
import Sibyl.Internal.Util (unsafeFromEither)
import qualified Data.Vector.Unboxed as U

type TS t y = TimeSeries t y
type FC t = Forecast t

mape :: U.Vector Double -> U.Vector Double -> Double
mape resids acts = unsafeFromEither "mape" (Accuracy.mape resids acts)

mase :: U.Vector Double -> Double -> Double
mase resids naiveScale = unsafeFromEither "mase" (Accuracy.mase resids naiveScale)

