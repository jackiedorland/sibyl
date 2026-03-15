module Sibyl.SmoothingSpec (spec) where

import Test.Hspec

spec :: Spec
spec =
  describe "smoothing roadmap" $ do
    it "simple moving average" $
      pendingWith "implement sma with window size k and cover edge cases for k less than or equal to zero and k bigger than n"

    it "single exponential smoothing" $
      pendingWith "implement ses level recursion and test against a known textbook sequence"

    it "holt trend method" $
      pendingWith "implement holt additive trend and verify one step forecasts against fixture values"

    it "holt winters seasonal method" $
      pendingWith "implement additive and multiplicative variants and validate with periodic synthetic data"
