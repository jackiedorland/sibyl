module Sibyl.PlottingSpec (spec) where

import Test.Hspec

spec :: Spec
spec =
  describe "plotting roadmap" $ do
    it "time series plotting payload" $
      pendingWith "define plotting payload contract for x y labels and intervals then test payload shape"

    it "forecast ribbon plotting payload" $
      pendingWith "define forecast interval rendering contract and test lower upper consistency"
