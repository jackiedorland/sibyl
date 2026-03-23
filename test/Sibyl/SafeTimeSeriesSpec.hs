module Sibyl.SafeTimeSeriesSpec (spec) where

import qualified Data.Vector.Unboxed as U
import qualified Sibyl.TimeSeries as SafeTS
import Test.Hspec

spec :: Spec
spec = do
  describe "safe timeseries" $ do
    it "builds a valid series" $ do
      let idx = U.fromList [1 :: Int, 2, 3]
          obs = U.fromList [10.0 :: Double, 11.0, 12.0]
      SafeTS.mkTimeSeries idx obs `shouldBe`
        Right
          SafeTS.TimeSeries
            { SafeTS.index = idx
            , SafeTS.observations = obs
            }

    it "gives left for non monotonic index" $ do
      let idx = U.fromList [1 :: Int, 1, 2]
          obs = U.fromList [10.0 :: Double, 11.0, 12.0]
      SafeTS.mkTimeSeries idx obs `shouldBe` Left SafeTS.NonMonotonicIndex

    it "gives left for length mismatch" $ do
      let idx = U.fromList [1 :: Int, 2, 3]
          obs = U.fromList [10.0 :: Double, 11.0]
      SafeTS.mkTimeSeries idx obs `shouldBe` Left SafeTS.LengthMismatch

    it "zipwithseries checks index alignment" $ do
      let mk = SafeTS.mkTimeSeries
          a = mk (U.fromList [1 :: Int, 2, 3]) (U.fromList [1.0 :: Double, 2.0, 3.0])
          b = mk (U.fromList [10 :: Int, 11, 12]) (U.fromList [4.0 :: Double, 5.0, 6.0])
      case (a, b) of
        (Right tsA, Right tsB) ->
          SafeTS.zipWithSeries (+) tsA tsB `shouldBe` Left SafeTS.IndexMismatch
        _ -> expectationFailure "fixture setup failed"
