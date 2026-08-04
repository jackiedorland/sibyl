module Sibyl.FacadeSpec (spec) where

import qualified Sibyl as S
import qualified Sibyl.TimeSeries as TS
import qualified Data.Vector.Unboxed as U
import Test.Hspec

spec :: Spec
spec = do
  describe "facade contracts" $ do
    it "prod style direct module import works" $ do
      TS.tsLength TS.sampleTimeSeries `shouldBe` 8

    it "supports the complete interactive naive forecasting loop" $ do
      let fitted = S.fit S.defaultNaiveSettings TS.sampleTimeSeries
          prediction = S.predict 3 fitted
      TS.index (S.predPoint prediction) `shouldBe` U.fromList [9, 10, 11]
      TS.observations (S.predPoint prediction) `shouldBe` U.replicate 3 108.0

    it "keeps model-specific convenience aliases" $ do
      let fitted = S.fitNaive TS.sampleTimeSeries
          prediction = S.forecastNaive 2 fitted
      TS.index (S.predPoint prediction) `shouldBe` U.fromList [9, 10]
