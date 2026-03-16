module Sibyl
  ( module Sibyl.TimeSeries
  , module Sibyl.Model
  , module Sibyl.Forecast
  , mae, rmse, mape, mase
  , AccuracyError(..)
  , fitARIMA
  , fitETS
  , fitNaive
  , TS
  , FC
  ) where

import Sibyl.TimeSeries hiding (name)
import Sibyl.Model
import Sibyl.Forecast
import qualified Sibyl.Accuracy as Accuracy
import Sibyl.Accuracy (mae, rmse, AccuracyError(..))
import Sibyl.Internal.Util (unsafeFromEither)
import qualified Data.Vector.Unboxed as U
import qualified Sibyl.Models.ARIMA as ARIMA

type TS t y = TimeSeries t y
type FC t = Forecast t

mape :: Forecast t -> Double
mape = unsafeFromEither "mape" . Accuracy.mape

mase :: U.Unbox t => Forecast t -> SeasonalSeries t Double -> Double
mase fc ss = unsafeFromEither "mase" (Accuracy.mase fc ss)



fitARIMA :: ARIMA.ARIMASettings -> TimeSeries t Double -> IO (ARIMA.ARIMA t)
fitARIMA = undefined 

fitETS :: a
fitETS = undefined

fitNaive :: a
fitNaive = undefined
