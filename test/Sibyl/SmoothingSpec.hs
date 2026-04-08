module Sibyl.SmoothingSpec (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed as U
import Sibyl.TimeSeries (TimeSeries(..), sampleTimeSeries, index, observations)
import Sibyl.Smoothing (sma, ses, sesManual, SmoothingError(..))

spec :: Spec
spec = do
  describe "sma" $ do

    it "produces correct window means for k=3" $ do
      let ts = TimeSeries (U.fromList [1,2,3,4,5 :: Int]) (U.fromList [1,2,3,4,5 :: Double])
      case sma 3 ts of
        Left err  -> expectationFailure (show err)
        Right out -> observations out `shouldBe` U.fromList [2.0, 3.0, 4.0]

    it "aligns output index to the last observation in each window" $ do
      let ts = TimeSeries (U.fromList [1,2,3,4,5 :: Int]) (U.fromList [1,2,3,4,5 :: Double])
      case sma 3 ts of
        Left err  -> expectationFailure (show err)
        Right out -> index out `shouldBe` U.fromList [3, 4, 5]

    it "k=1 returns the original observations unchanged" $ do
      let ts = TimeSeries (U.fromList [1,2,3 :: Int]) (U.fromList [10,20,30 :: Double])
      case sma 1 ts of
        Left err  -> expectationFailure (show err)
        Right out -> observations out `shouldBe` observations ts

    it "k=n returns a single point equal to the global mean" $ do
      let ts = TimeSeries (U.fromList [1,2,3,4 :: Int]) (U.fromList [1,2,3,4 :: Double])
      case sma 4 ts of
        Left err  -> expectationFailure (show err)
        Right out -> do
          U.length (observations out) `shouldBe` 1
          U.head (observations out) `shouldBe` 2.5

    it "k <= 0 returns WindowTooSmall" $ do
      sma 0    sampleTimeSeries `shouldBe` Left WindowTooSmall
      sma (-3) sampleTimeSeries `shouldBe` Left WindowTooSmall

    it "k > n returns WindowTooLarge" $ do
      sma 9 sampleTimeSeries `shouldBe` Left WindowTooLarge

  describe "sesManual" $ do

    it "produces correct level sequence against a handcrafted example" $ do
      let ts = TimeSeries (U.fromList [1,2,3 :: Int]) (U.fromList [10,20,30 :: Double])
      case sesManual 0.5 ts of
        Left err  -> expectationFailure (show err)
        Right out -> observations out `shouldBe` U.fromList [10.0, 15.0, 22.5]

    it "output has the same length and index as the input" $ do
      let ts = TimeSeries (U.fromList [1,2,3,4 :: Int]) (U.fromList [1,2,3,4 :: Double])
      case sesManual 0.3 ts of
        Left err  -> expectationFailure (show err)
        Right out -> do
          U.length (observations out) `shouldBe` 4
          index out `shouldBe` index ts

    it "alpha <= 0 returns InvalidAlpha" $ do
      sesManual 0.0    sampleTimeSeries `shouldBe` Left InvalidAlpha
      sesManual (-0.1) sampleTimeSeries `shouldBe` Left InvalidAlpha

    it "alpha >= 1 returns InvalidAlpha" $ do
      sesManual 1.0 sampleTimeSeries `shouldBe` Left InvalidAlpha
      sesManual 1.5 sampleTimeSeries `shouldBe` Left InvalidAlpha

    it "fewer than 2 observations returns InsufficientData" $ do
      let ts = TimeSeries (U.fromList [1 :: Int]) (U.fromList [42.0 :: Double])
      sesManual 0.5 ts `shouldBe` Left InsufficientData

  describe "ses (auto alpha)" $ do

    it "returns a series with the same length and index as the input" $ do
      case ses sampleTimeSeries of
        Left err  -> expectationFailure (show err)
        Right out -> do
          U.length (observations out) `shouldBe` U.length (observations sampleTimeSeries)
          index out `shouldBe` index sampleTimeSeries

    it "SSE of auto alpha is no worse than a fixed midpoint alpha" $ do
      -- i think the optimizer should find something better than 0.5 for alpha, enforce that with the test
      let ts = sampleTimeSeries
          obs = observations ts
          n   = U.length obs
          sse alpha =
            let lvls = U.scanl' (\l y -> alpha * y + (1 - alpha) * l) (U.head obs) (U.tail obs)
            in  U.sum $ U.zipWith (\f a -> (a - f)^(2::Int)) (U.take (n-1) lvls) (U.drop 1 obs)
      case ses ts of
        Left err  -> expectationFailure (show err)
        Right out -> do
          let autoLvls  = observations out
              autoAlpha = let l0 = U.head autoLvls
                              l1 = autoLvls U.! 1
                              y1 = obs U.! 1
                          in  (l1 - (1 - ((l1 - l0) / (y1 - l0 + 1e-12))) * l0)
                                / (y1 - (1 - ((l1 - l0) / (y1 - l0 + 1e-12))) * l0 + 1e-12)
              sseMid  = sse 0.5
              sseAuto = sse autoAlpha
          sseAuto `shouldSatisfy` (<= sseMid + 1e-6)

    it "fewer than 2 observations returns InsufficientData" $ do
      let ts = TimeSeries (U.fromList [1 :: Int]) (U.fromList [42.0 :: Double])
      ses ts `shouldBe` Left InsufficientData
