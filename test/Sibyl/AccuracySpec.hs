module Sibyl.AccuracySpec (spec) where

import Test.Hspec

spec :: Spec
spec =
  describe "accuracy roadmap" $ do
    it "mae" $
      pendingWith "add mae and check it against a hand computed fixture"

    it "rmse" $
      pendingWith "add rmse and confirm sqrt mean squared error behavior with deterministic data"

    it "mape" $
      pendingWith "add mape and lock down what happens around zeros with valid and invalid inputs"

    it "mase" $
      pendingWith "add mase with seasonal naive denominator and cover seasonal plus non seasonal fixtures"
