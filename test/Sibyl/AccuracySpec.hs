module Sibyl.AccuracySpec (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed as U
import Sibyl.Accuracy
import Sibyl.Forecast
import Sibyl.TimeSeries (sampleTimeSeries)
import Sibyl.Safe.TimeSeries (SeasonalSeries(..), mkSeasonalSeries)

-- Minimal ForecastResult fixture — point/lower/upper are placeholders
mkFc :: U.Vector Double -> U.Vector Double -> Forecast Int
mkFc res acts = ForecastResult
  { point     = sampleTimeSeries
  , lower     = sampleTimeSeries
  , upper     = sampleTimeSeries
  , level     = 0.95
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
      mape (mkFc (U.fromList [10, 20]) (U.fromList [100, 200])) `shouldBe` Right 0.1
    it "returns ZeroInActuals when any actual is 0" $
      mape (mkFc (U.fromList [1]) (U.fromList [0])) `shouldBe` Left ZeroInActuals

  describe "mase" $ do
    it "computes mase of 1.0 when forecast matches naive baseline" $ do
      -- training: [10,20,30,40,12,22,32,42], period 4
      -- naive diffs: |12-10|=2, |22-20|=2, |32-30|=2, |42-40|=2 → naiveDenom = 8/4 = 2.0
      -- forecast residuals: [2,2,2,2] → mae = 2.0 → MASE = 2.0/2.0 = 1.0
      let ss = mkSS [1..8] [10,20,30,40,12,22,32,42] 4
          fc = mkFc (U.fromList [2, 2, 2, 2]) (U.fromList [])
      mase fc ss `shouldBe` Right 1.0

    it "returns ConstantTraining when training series has no variation" $ do
      let ss = mkSS [1..8] [5,5,5,5,5,5,5,5] 4
          fc = mkFc (U.fromList [1]) (U.fromList [])
      mase fc ss `shouldBe` Left ConstantTraining
