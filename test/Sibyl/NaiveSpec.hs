module Sibyl.NaiveSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector.Unboxed as U
import Sibyl.Models.Naive
import qualified Sibyl.Safe.TimeSeries as SafeTS
import qualified Sibyl.TimeSeries as TS
import Sibyl.Forecast (Forecast(..), point, lower, upper, ciLevel, residuals, actuals)

mkLast :: [Int] -> [Double] -> Naive Int
mkLast idx obs = Naive defaultNaiveSettings (TS.mkTimeSeries (U.fromList idx) (U.fromList obs))

mkMean :: [Int] -> [Double] -> Naive Int
mkMean idx obs = Naive (NaiveSettings Mean Nothing 0.95) (TS.mkTimeSeries (U.fromList idx) (U.fromList obs))

-- increasing list 
genIncreasing :: Int -> Gen [Int]
genIncreasing n = do
  start <- arbitrary
  diffs <- vectorOf (n - 1) (choose (1, 10))
  return $ scanl (+) start diffs

-- arbitrary Doubles
genObs :: Int -> Gen [Double]
genObs = flip vectorOf arbitrary

spec :: Spec
spec = describe "naive model" $ do

  describe "naiveForecastLast (unit)" $ do

    it "point forecast is last observation repeated h times" $ do
      let fc = naiveForecastLast 3 (mkLast [1,2,3,4,5] [10,20,30,40,50])
      U.toList (SafeTS.observations (point fc)) `shouldBe` [50, 50, 50]

    it "output length equals h" $ do
      let fc = naiveForecastLast 5 (mkLast [1..10] (map fromIntegral [1..10 :: Int]))
      TS.tsLength (point fc) `shouldBe` 5

    it "level equals ciLevel from settings" $ do
      let fc = naiveForecastLast 2 (mkLast [1,2,3] [1,2,3])
      ciLevel fc `shouldBe` 0.95

    it "residuals length is n-1" $ do
      let fc = naiveForecastLast 3 (mkLast [1..8] (map fromIntegral [1..8 :: Int]))
      U.length (residuals fc) `shouldBe` 7

    it "actuals length matches residuals length" $ do
      let fc = naiveForecastLast 3 (mkLast [1..8] (map fromIntegral [1..8 :: Int]))
      U.length (actuals fc) `shouldBe` U.length (residuals fc)

    it "lower < point at every horizon" $ do
      let fc    = naiveForecastLast 4 (mkLast [1..5] [2,4,6,8,10])
          pts   = U.toList (SafeTS.observations (point fc))
          lows  = U.toList (SafeTS.observations (lower fc))
      all (uncurry (>)) (zip pts lows) `shouldBe` True

    it "upper > point at every horizon" $ do
      let fc    = naiveForecastLast 4 (mkLast [1..5] [2,4,6,8,10])
          pts   = U.toList (SafeTS.observations (point fc))
          highs = U.toList (SafeTS.observations (upper fc))
      all (uncurry (<)) (zip pts highs) `shouldBe` True

    it "CI is symmetric: upper - point == point - lower" $ do
      let fc    = naiveForecastLast 4 (mkLast [1..5] [2,4,6,8,10])
          pts   = U.toList (SafeTS.observations (point fc))
          lows  = U.toList (SafeTS.observations (lower fc))
          highs = U.toList (SafeTS.observations (upper fc))
          diffs = zipWith3 (\l p u -> abs ((u - p) - (p - l))) lows pts highs
      all (< 1e-10) diffs `shouldBe` True

    it "CI half-width strictly increases with horizon (z*sigma*sqrt(k) growth)" $ do
      let fc    = naiveForecastLast 5 (mkLast [1..5] [2,4,6,8,10])
          pts   = U.toList (SafeTS.observations (point fc))
          highs = U.toList (SafeTS.observations (upper fc))
          widths = zipWith (-) highs pts
      all (uncurry (<)) (zip widths (tail widths)) `shouldBe` True

    it "future index is strictly increasing and starts after training end" $ do
      let fc    = naiveForecastLast 3 (mkLast [1,2,3] [10,20,30])
          fidx  = U.toList (SafeTS.index (point fc))
      fidx `shouldBe` [4, 5, 6]

  describe "naiveForecastLast (properties)" $ do

    it "point values always equal last observation" $
      property $ forAll (choose (2, 20)) $ \n ->
        forAll (genIncreasing n) $ \idx ->
        forAll (genObs n) $ \obs ->
        forAll (choose (1, 10)) $ \h ->
          let fc  = naiveForecastLast h (mkLast idx obs)
              pts = U.toList (SafeTS.observations (point fc))
          in all (== last obs) pts

    it "output length always equals h" $
      property $ forAll (choose (2, 20)) $ \n ->
        forAll (genIncreasing n) $ \idx ->
        forAll (genObs n) $ \obs ->
        forAll (choose (1, 10)) $ \h ->
          TS.tsLength (point (naiveForecastLast h (mkLast idx obs))) == h

    it "actuals length always equals residuals length" $
      property $ forAll (choose (2, 20)) $ \n ->
        forAll (genIncreasing n) $ \idx ->
        forAll (genObs n) $ \obs ->
        forAll (choose (1, 10)) $ \h ->
          let fc = naiveForecastLast h (mkLast idx obs)
          in U.length (actuals fc) == U.length (residuals fc)

    it "CI is always symmetric" $
      property $ forAll (choose (2, 20)) $ \n ->
        forAll (genIncreasing n) $ \idx ->
        forAll (genObs n) $ \obs ->
        forAll (choose (1, 10)) $ \h ->
          let fc    = naiveForecastLast h (mkLast idx obs)
              pts   = U.toList (SafeTS.observations (point fc))
              lows  = U.toList (SafeTS.observations (lower fc))
              highs = U.toList (SafeTS.observations (upper fc))
              diffs = zipWith3 (\l p u -> abs ((u - p) - (p - l))) lows pts highs
          in all (< 1e-10) diffs

  describe "naiveForecastMean (unit)" $ do

    it "point forecast is series mean repeated h times" $ do
      let fc  = naiveForecastMean 3 (mkMean [1,2,3,4] [2,4,6,8])
          pts = U.toList (SafeTS.observations (point fc))
      pts `shouldBe` [5.0, 5.0, 5.0]

    it "output length equals h" $ do
      let fc = naiveForecastMean 4 (mkMean [1..6] (map fromIntegral [1..6 :: Int]))
      TS.tsLength (point fc) `shouldBe` 4

    it "residuals length is n" $ do
      -- Mean residual exists for every observation
      let fc = naiveForecastMean 2 (mkMean [1..6] (map fromIntegral [1..6 :: Int]))
      U.length (residuals fc) `shouldBe` 6

    it "actuals length equals n" $ do
      let fc = naiveForecastMean 2 (mkMean [1..6] (map fromIntegral [1..6 :: Int]))
      U.length (actuals fc) `shouldBe` 6

    it "CI half-width is constant across all horizons" $ do
      -- Mean CI does not grow with h: hw = z*sigma*sqrt(1 + 1/n)
      let fc    = naiveForecastMean 5 (mkMean [1..5] [2,4,6,8,10])
          pts   = U.toList (SafeTS.observations (point fc))
          highs = U.toList (SafeTS.observations (upper fc))
          widths = zipWith (-) highs pts
      length (filter (/= head widths) widths) `shouldBe` 0

    it "lower < point < upper" $ do
      let fc    = naiveForecastMean 3 (mkMean [1..5] [2,4,6,8,10])
          pts   = U.toList (SafeTS.observations (point fc))
          lows  = U.toList (SafeTS.observations (lower fc))
          highs = U.toList (SafeTS.observations (upper fc))
      all (uncurry (>)) (zip pts lows) `shouldBe` True
      all (uncurry (<)) (zip pts highs) `shouldBe` True

  describe "naiveForecastMean (properties)" $ do

    it "point values always equal series mean" $
      property $ forAll (choose (2, 20)) $ \n ->
        forAll (genIncreasing n) $ \idx ->
        forAll (genObs n) $ \obs ->
        forAll (choose (1, 10)) $ \h ->
          let fc   = naiveForecastMean h (mkMean idx obs)
              pts  = U.toList (SafeTS.observations (point fc))
              mean = sum obs / fromIntegral (length obs)
          in all (\p -> abs (p - mean) < 1e-10) pts

    it "CI half-width is always constant" $
      property $ forAll (choose (2, 20)) $ \n ->
        forAll (genIncreasing n) $ \idx ->
        forAll (genObs n) $ \obs ->
        forAll (choose (2, 10)) $ \h ->
          let fc     = naiveForecastMean h (mkMean idx obs)
              pts    = U.toList (SafeTS.observations (point fc))
              highs  = U.toList (SafeTS.observations (upper fc))
              widths = zipWith (-) highs pts
          in all (\w -> abs (w - head widths) < 1e-10) (tail widths)
