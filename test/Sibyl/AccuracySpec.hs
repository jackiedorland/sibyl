module Sibyl.AccuracySpec (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed as U
import Sibyl.Accuracy
import Sibyl.Forecast (Forecast(..), ForecastResult, point, lower, upper, ciLevel, residuals, actuals)
import Sibyl.TimeSeries (sampleTimeSeries)
import Sibyl.Safe.TimeSeries (SeasonalSeries(..), mkSeasonalSeries)

mkFc :: U.Vector Double -> U.Vector Double -> Forecast Int
mkFc res acts = Forecast
  { point     = sampleTimeSeries
  , lower     = sampleTimeSeries
  , upper     = sampleTimeSeries
  , ciLevel   = 0.95
  , residuals = res
  , actuals   = acts
  }

mkSS :: [Int] -> [Double] -> Int -> SeasonalSeries Int Double
mkSS idx obs m = case mkSeasonalSeries (U.fromList idx) (U.fromList obs) m of
  Right ss -> ss
  Left e   -> error (show e)

spec :: Spec
spec = describe "accuracy metrics" $ do

  describe "mae" $ do
    it "computes mean absolute error from residuals" $
      mae (mkFc (U.fromList [1, -2, 3, -4]) (U.fromList [])) `shouldBe` 2.5

  describe "rmse" $ do
    it "computes root mean squared error from residuals" $
      rmse (mkFc (U.fromList [1, 1, 1, 1]) (U.fromList [])) `shouldBe` 1.0
    it "is larger than mae when errors vary" $
      rmse (mkFc (U.fromList [1, 3]) (U.fromList [])) > mae (mkFc (U.fromList [1, 3]) (U.fromList []))
        `shouldBe` True

  describe "mape" $ do
    it "computes mean absolute percentage error" $
      mape (mkFc (U.fromList [10, 20]) (U.fromList [100, 200])) `shouldBe` Right 10.0
    it "returns ZeroInActuals when any actual is 0" $
      mape (mkFc (U.fromList [1]) (U.fromList [0])) `shouldBe` Left ZeroInActuals

  describe "mase" $ do
    it "computes mase of 1.0 when forecast matches naive baseline" $ do
      let ss = mkSS [1..8] [10,20,30,40,12,22,32,42] 4
          fc = mkFc (U.fromList [2, 2, 2, 2]) (U.fromList [])
      mase fc ss `shouldBe` Right 1.0

    it "returns ConstantTraining when training series has no variation" $ do
      let ss = mkSS [1..8] [5,5,5,5,5,5,5,5] 4
          fc = mkFc (U.fromList [1]) (U.fromList [])
      mase fc ss `shouldBe` Left ConstantTraining
