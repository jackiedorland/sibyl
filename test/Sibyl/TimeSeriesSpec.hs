module Sibyl.TimeSeriesSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck
import qualified Data.Vector.Unboxed as U
import Sibyl
import qualified Sibyl.TimeSeries as TS

-- sorry i know this isn't very Haskelly but I prefer it - jackie
(???) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation 
a ??? b = a `shouldBe` b
infix 1 ???

sampleTs :: TimeSeries Int Double
sampleTs = sampleTimeSeries

spec :: Spec
spec = do
  describe "mkTimeSeries" $ do
    it "rejects length mismatch" $ do
      let index = U.fromList [1 :: Int, 2]
          values = U.fromList [10.0 :: Double]
      evaluate (mkTimeSeries index values) `shouldThrow` errorCall "mkTimeSeries: LengthMismatch"

    it "rejects empty series" $ do
      let index = U.empty :: U.Vector Int
          values = U.empty :: U.Vector Double
      evaluate (mkTimeSeries index values) `shouldThrow` errorCall "mkTimeSeries: EmptySeries"

  describe "basic invariants" $ do
    it "sampleTimeSeries has aligned index/observation lengths" $ do
      U.length (index sampleTs) ??? U.length (observations sampleTs)

    it "length equals input length for valid strictly increasing index" $
      property $ \xs ->
        let indexList = uniqueSorted (map (abs :: Int -> Int) xs)
            valuesList = map fromIntegral indexList :: [Double]
            index = U.fromList indexList
            values = U.fromList valuesList
         in not (null indexList) ==>
              tsLength (mkTimeSeries index values) === length indexList

  describe "lag tests" $ do 
    it "lag by one period" $ do
      let lagged = lag 1 sampleTs
      lagged ??? sampleTs
        { observations = U.take (tsLength sampleTs - 1) (observations sampleTs)
        , index = U.drop 1 (index sampleTs)
        }
    
    it "rejects -1 lag" $ do
      evaluate (lag (-1) sampleTs) `shouldThrow` errorCall "lag: InvalidLag"

    it "rejects n lag" $ do
      evaluate (lag (tsLength sampleTs) sampleTs) `shouldThrow` errorCall "lag: InvalidLag"

  describe "lead tests" $ do
    it "lead by one period" $ do
      let led = lead 1 sampleTs
      led ??? sampleTs
        { observations = U.drop 1 (observations sampleTs)
        , index = U.take (tsLength sampleTs - 1) (index sampleTs)
        }

    it "rejects -1 lead" $ do
      evaluate (lead (-1) sampleTs) `shouldThrow` errorCall "lead: InvalidLead"

    it "rejects n lead" $ do
      evaluate (lead (tsLength sampleTs) sampleTs) `shouldThrow` errorCall "lead: InvalidLead"

  describe "diff tests" $ do
    it "diff by one period" $ do
      let differenced = diff sampleTs
      index differenced ??? U.fromList [2, 3, 4, 5, 6, 7, 8]
      observations differenced ??? U.fromList [2.0, -0.5, 1.5, 2.0, -0.5, 1.5, 1.0]

    it "rejects singleton series" $ do
      let singletonTs = mkTimeSeries (U.fromList [1 :: Int]) (U.fromList [101.0 :: Double])
      evaluate (diff singletonTs) `shouldThrow` errorCall "diff: InsufficientObservations"

  describe "length, start and end for TimeSeries" $ do
    it "tsLength" $ do
      tsLength sampleTs ??? 8

    it "tsStart" $ do
      tsStart sampleTs ??? 1

    it "tsEnd" $ do
      tsEnd sampleTs ??? 8

  describe "takeFirst/takeLast/slice" $ do
    it "slice returns correct subrange" $ do
      let sts = slice 3 5 sampleTs
      observations sts ??? U.fromList [102.5, 104.0, 106.0]
      index sts ??? U.fromList [3, 4, 5]

    it "slice rejects start > end" $ do
      evaluate (slice 5 3 sampleTs) `shouldThrow` errorCall "slice: InvalidSlice"

    it "slice rejects empty result" $ do
      evaluate (slice 100 200 sampleTs) `shouldThrow` errorCall "slice: EmptySeries"

    it "takeFirst returns first k elements" $ do
      let sts = takeFirst 3 sampleTs
      observations sts ??? U.fromList [101.0, 103.0, 102.5]
      index sts ??? U.fromList [1, 2, 3]

    it "takeFirst rejects negative k" $ do
      evaluate (takeFirst (-1) sampleTs) `shouldThrow` errorCall "takeFirst: InvalidQuantity"

    it "takeFirst rejects k > length" $ do
      evaluate (takeFirst 9 sampleTs) `shouldThrow` errorCall "takeFirst: InvalidQuantity"

    it "takeLast returns last k elements" $ do
      let sts = takeLast 3 sampleTs
      observations sts ??? U.fromList [105.5, 107.0, 108.0]
      index sts ??? U.fromList [6, 7, 8]

    it "takeLast rejects negative k" $ do
      evaluate (takeLast (-1) sampleTs) `shouldThrow` errorCall "takeLast: InvalidQuantity"

    it "takeLast rejects k > length" $ do
      evaluate (takeLast 9 sampleTs) `shouldThrow` errorCall "takeLast: InvalidQuantity"

  describe "zipWithSeries" $ do
    it "adds two identical series element-wise" $ do
      let zipped = zipWithSeries (+) sampleTs sampleTs
      index zipped ??? index sampleTs
      observations zipped ??? U.map (* 2) (observations sampleTs)

    it "subtracts a series from itself gives all zeroes" $ do
      let zipped = zipWithSeries (-) sampleTs sampleTs
      observations zipped ??? U.replicate (tsLength sampleTs) 0.0

    it "preserves the index from the input series" $ do
      let zipped = zipWithSeries (*) sampleTs sampleTs
      index zipped ??? index sampleTs

    it "rejects series with different lengths" $ do
      let shorter = takeFirst 4 sampleTs
      evaluate (zipWithSeries (+) sampleTs shorter) `shouldThrow` errorCall "zipWithSeries: LengthMismatch"

    it "rejects series with same length but different indices" $ do
      let shiftedTs = sampleTs { index = U.fromList [10..17 :: Int] }
      evaluate (zipWithSeries (+) sampleTs shiftedTs) `shouldThrow` errorCall "zipWithSeries: IndexMismatch"

  describe "diffN" $ do
    it "diffN 0 returns the series unchanged" $ do
      TS.diffN 0 sampleTs `shouldBe` Right sampleTs

    it "diffN 1 equals diff" $ do
      TS.diffN 1 sampleTs `shouldBe` Right (diff sampleTs)

    it "diffN 2 equals diff applied twice" $ do
      TS.diffN 2 sampleTs `shouldBe` Right (diff (diff sampleTs))

    it "diffN negative returns InvalidQuantity" $ do
      TS.diffN (-1) sampleTs `shouldBe` Left TS.InvalidQuantity

    it "diffN k too large returns InsufficientObservations" $ do
      -- sampleTs has 8 obs; diff 8 times leaves nothing
      TS.diffN 8 sampleTs `shouldBe` Left TS.InsufficientObservations

  describe "diffSeasonal" $ do
    it "computes y_t - y_{t-m} for period 2" $ do
      -- obs: [0,1,2,3,4], period 2 -> [2-0, 3-1, 4-2] = [2,2,2]
      let ts = mkTimeSeries (U.fromList [1..5 :: Int]) (U.fromList [0,1,2,3,4 :: Double])
      case TS.diffSeasonal 2 ts of
        Left err  -> expectationFailure (show err)
        Right out -> do
          observations out ??? U.fromList [2.0, 2.0, 2.0]
          index out        ??? U.fromList [3, 4, 5]

    it "removes a constant seasonal pattern" $ do
      -- period-4 constant season: [1,2,3,4,1,2,3,4] -> all zeros
      let ts = mkTimeSeries (U.fromList [1..8 :: Int]) (U.fromList [1,2,3,4,1,2,3,4 :: Double])
      case TS.diffSeasonal 4 ts of
        Left err  -> expectationFailure (show err)
        Right out -> observations out ??? U.replicate 4 0.0

    it "output length is n - m" $ do
      case TS.diffSeasonal 3 sampleTs of
        Left err  -> expectationFailure (show err)
        Right out -> tsLength out ??? tsLength sampleTs - 3

    it "rejects m < 1" $ do
      TS.diffSeasonal 0 sampleTs `shouldBe` Left TS.InvalidQuantity

    it "rejects n <= m" $ do
      -- sampleTs has 8 obs; m=8 means no output
      TS.diffSeasonal 8 sampleTs `shouldBe` Left TS.InsufficientObservations

  describe "diffSeasonalN" $ do
    it "diffSeasonalN 0 returns the series unchanged" $ do
      TS.diffSeasonalN 0 4 sampleTs `shouldBe` Right sampleTs

    it "diffSeasonalN 1 equals diffSeasonal" $ do
      TS.diffSeasonalN 1 3 sampleTs `shouldBe` TS.diffSeasonal 3 sampleTs

    it "diffSeasonalN 2 equals diffSeasonal applied twice" $ do
      let step1 = TS.diffSeasonal 2 sampleTs
      TS.diffSeasonalN 2 2 sampleTs `shouldBe` (step1 >>= TS.diffSeasonal 2)

    it "rejects negative k" $ do
      TS.diffSeasonalN (-1) 4 sampleTs `shouldBe` Left TS.InvalidQuantity

uniqueSorted :: [Int] -> [Int]
uniqueSorted = dedup . quicksort
  where
    dedup [] = []
    dedup [x] = [x]
    dedup (x:y:rest)
      | x == y    = dedup (y:rest)
      | otherwise = x : dedup (y:rest)

    quicksort [] = []
    quicksort (p:ys) =
      quicksort [z | z <- ys, z <= p] ++ [p] ++ quicksort [z | z <- ys, z > p]