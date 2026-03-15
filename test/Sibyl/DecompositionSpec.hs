module Sibyl.DecompositionSpec (spec) where

import Test.Hspec

spec :: Spec
spec =
  describe "decomposition roadmap" $ do
    it "classical additive decomposition" $
      pendingWith "implement trend seasonal remainder split and test reconstruction identity"

    it "classical multiplicative decomposition" $
      pendingWith "implement trend seasonal remainder product split and test reconstruction with tolerance"

    it "stl decomposition" $
      pendingWith "implement robust stl and verify trend smoothness and seasonal periodicity on synthetic data"
