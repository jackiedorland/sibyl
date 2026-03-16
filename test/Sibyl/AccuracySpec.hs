module Sibyl.AccuracySpec (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed as U
import Sibyl.Accuracy

spec :: Spec
spec = describe "accuracy metrics" $ do

  describe "mae" $ do
    it "computes mean absolute error from residuals" $
      mae (U.fromList [1, -2, 3, -4]) `shouldBe` 2.5

  describe "rmse" $ do
    it "computes root mean squared error from residuals" $
      rmse (U.fromList [1, 1, 1, 1]) `shouldBe` 1.0
    it "is larger than mae when errors vary" $
      rmse (U.fromList [1, 3]) > mae (U.fromList [1, 3]) `shouldBe` True

  describe "mape" $ do
    it "computes mean absolute percentage error" $
      mape (U.fromList [10, 20]) (U.fromList [100, 200]) `shouldBe` Right 10.0
    it "returns ZeroInActuals when any actual is 0" $
      mape (U.fromList [1]) (U.fromList [0]) `shouldBe` Left ZeroInActuals

  describe "mase" $ do
    -- series [10,20,30,40,12,22,32,42] period=4:
    -- seasonal naive scale = (|12-10|+|22-20|+|32-30|+|42-40|) / (8-4) = 8/4 = 2.0
    it "computes mase of 1.0 when forecast errors match naive baseline" $
      mase (U.fromList [2, 2, 2, 2]) 2.0 `shouldBe` Right 1.0

    it "returns ConstantTraining when naive scale is zero" $
      mase (U.fromList [1]) 0.0 `shouldBe` Left ConstantTraining
