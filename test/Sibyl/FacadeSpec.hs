module Sibyl.FacadeSpec (spec) where

import qualified Sibyl as S
import qualified Sibyl.TimeSeries as TS
import Test.Hspec

spec :: Spec
spec = do
  describe "facade contracts" $ do
    it "prod style direct module import works" $ do
      TS.tsLength TS.sampleTimeSeries `shouldBe` 8

    it "fit entrypoints are still stubs" $ do
      pendingWith "wire fitARIMA fitETS fitNaive after internals are real then swap this for behavior tests"
