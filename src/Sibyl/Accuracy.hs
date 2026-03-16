module Sibyl.Accuracy
  ( mae, rmse, mape, mase
  , AccuracyError(..)
  ) where

import Sibyl.Forecast
import Sibyl.Safe.TimeSeries (SeasonalSeries(..), TimeSeries(..), tsLength)
import qualified Data.Vector.Unboxed as U
import Statistics.Sample as Sm

data AccuracyError 
    = ZeroInActuals
    | ConstantTraining
    deriving (Show, Eq)

mae :: Forecast t -> Double
mae fc = Sm.mean $ U.map abs (residuals fc)

rmse :: Forecast t -> Double
rmse fc = sqrt $ Sm.mean $ U.map (^2) (residuals fc)

mape :: Forecast t -> Either AccuracyError Double
mape fc 
    | U.any (== 0) (actuals fc)    = Left ZeroInActuals
    | otherwise                    = Right $ Sm.mean $ U.zipWith (\r a -> abs r / abs a) (residuals fc) (actuals fc)

mase :: U.Unbox t => Forecast t -> SeasonalSeries t Double -> Either AccuracyError Double
mase fc ss
    | naiveMae == 0  = Left ConstantTraining
    | otherwise      = Right $ mae fc / (naiveMae / fromIntegral (n - m))
    where
        rawSeries = series ss
        n = tsLength rawSeries
        obs = observations rawSeries
        naiveMae = U.sum $ U.zipWith (\yt ytm -> abs (yt - ytm)) (U.drop m obs) obs
        m = period ss