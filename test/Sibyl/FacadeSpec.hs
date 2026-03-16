module Sibyl.FacadeSpec (spec) where

import qualified Sibyl as S
import qualified Sibyl.TimeSeries as TS
import Test.Hspec

spec :: Spec
spec = do
  describe "facade contracts" $ do
    it "unsafe exports ts alias" $ do
      let _value :: S.TS Int Double
          _value = S.sampleTimeSeries
      True `shouldBe` True

    it "unsafe exports fc alias" $ do
      let _forecastList :: [S.FC Int]
          _forecastList = []
      length _forecastList `shouldBe` 0

    it "prod style direct module import works" $ do
      TS.tsLength TS.sampleTimeSeries `shouldBe` 8

    it "fit entrypoints are still stubs" $ do
      pendingWith "wire fitARIMA fitETS fitNaive after internals are real then swap this for behavior tests"
