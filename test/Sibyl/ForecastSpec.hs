module Sibyl.ForecastSpec (spec) where

import qualified Data.Vector.Unboxed as U
import Sibyl.Forecast (Forecast(..), Forecast, point, lower, upper, ciLevel, residuals, actuals)
import qualified Sibyl.TimeSeries as TS
import Test.Hspec

spec :: Spec
spec = do
  describe "forecast basics" $ do
    it "builds forecastresult with aligned fields" $ do
      let idx = U.fromList [1 :: Int, 2, 3]
          pointTs = TS.mkTimeSeries idx (U.fromList [10.0 :: Double, 11.0, 12.0])
          lowerTs = TS.mkTimeSeries idx (U.fromList [9.0 :: Double, 10.0, 11.0])
          upperTs = TS.mkTimeSeries idx (U.fromList [11.0 :: Double, 12.0, 13.0])
          fr = Forecast
            { point = pointTs
            , lower = lowerTs
            , upper = upperTs
            , ciLevel = 0.95
            , residuals = U.fromList [0.1, -0.2, 0.05]
            , actuals   = U.fromList []
            }
      TS.tsLength (point fr) `shouldBe` 3
      ciLevel fr `shouldBe` 0.95

    it "forecast alias still works" $ do
      let idx = U.fromList [1 :: Int]
          one = TS.mkTimeSeries idx (U.fromList [10.0 :: Double])
          asForecast :: Forecast Int
          asForecast = Forecast one one one 0.8 (U.fromList [0.0]) (U.fromList [])
      ciLevel asForecast `shouldBe` 0.8
