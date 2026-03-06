module Sibyl.TimeSeriesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Foldable (toList)
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U
import Sibyl.TimeSeries

-- sorry i know this isn't very Haskelly but I prefer it - jackie
(???) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation 
a ??? b = a `shouldBe` b
infix 1 ???

sampleTs :: TimeSeries Int Double
sampleTs = sampleTimeSeries

sampleBts :: BTimeSeries Int Double
sampleBts = sampleBTimeSeries

spec :: Spec
spec = do
  describe "mkTimeSeries" $ do
    it "rejects length mismatch" $ do
      let index = U.fromList [1 :: Int, 2]
          values = U.fromList [10.0 :: Double]
      mkTimeSeries index values Nothing Nothing ??? Left LengthMismatch

    it "rejects empty series" $ do
      let index = U.empty :: U.Vector Int
          values = U.empty :: U.Vector Double
      mkTimeSeries index values Nothing Nothing ??? Left EmptySeries

  describe "basic invariants" $ do
    it "sampleTimeSeries has aligned index/observation lengths" $ do
      U.length (tsIndex sampleTs) ??? U.length (tsObservations sampleTs)

    it "length equals input length for valid strictly increasing index" $
      property $ \xs ->
        let indexList = uniqueSorted (map (abs :: Int -> Int) xs)
            valuesList = map fromIntegral indexList :: [Double]
            index = U.fromList indexList
            values = U.fromList valuesList
         in not (null indexList) ==>
              case mkTimeSeries index values Nothing Nothing of
                Left _ -> property False
                Right ts -> tsLength ts === length indexList

  describe "mkBTimeSeries" $ do
    it "rejects length mismatch" $ do
      let index = B.fromList [1 :: Int, 2]
          values = B.fromList [10.0 :: Double]
      mkBTimeSeries index values Nothing Nothing ??? Left LengthMismatch

    it "rejects empty series" $ do
      let index = B.empty :: B.Vector Int
          values = B.empty :: B.Vector Double
      mkBTimeSeries index values Nothing Nothing ??? Left EmptySeries

    it "accepts a valid series shape from the sample time series" $ do
      mkBTimeSeries (btsIndex sampleBts) (btsObservations sampleBts) (btsName sampleBts) (btsDescription sampleBts) ??? Right sampleBts

  describe "BTimeSeries Functor" $ do
    it "maps only observations" $ do
      let mapped = fmap (+1) sampleBts
       in do
            btsIndex mapped ??? btsIndex sampleBts
            btsObservations mapped ??? B.map (+ 1) (btsObservations sampleBts)

    it "fmap id = id" $ do
      fmap id sampleBts ??? sampleBts

  describe "BTimeSeries Foldable" $ do
    it "foldr sums observations" $ do
      foldr (+) 0 sampleBts ??? 837.0

    it "toList returns observation values" $ do
      toList sampleBts ??? [101.0, 103.0, 102.5, 104.0, 106.0, 105.5, 107.0, 108.0 :: Double]

  describe "lag tests" $ do 
    it "lag by one period" $ do
      case lag sampleTs 1 of
        Left e -> expectationFailure ("unexpected lag error: " ++ show e)
        Right lagged ->
          lagged ??? sampleTs
            { tsObservations = U.take (tsLength sampleTs - 1) (tsObservations sampleTs)
            , tsIndex = U.drop 1 (tsIndex sampleTs)
            }
    
    it "rejects -1 lag" $ do
      lag sampleTs (-1) ??? Left InvalidLag

    it "rejects n lag" $ do
      lag sampleTs (tsLength sampleTs) ??? Left InvalidLag

  describe "lagB tests" $ do
    it "lagB by one period" $ do
      case lagB sampleBts 1 of
        Left e -> expectationFailure ("unexpected lagB error: " ++ show e)
        Right lagged ->
          lagged ??? sampleBts
            { btsObservations = B.take (btsLength sampleBts - 1) (btsObservations sampleBts)
            , btsIndex = B.drop 1 (btsIndex sampleBts)
            }

    it "rejects -1 lagB" $ do
      lagB sampleBts (-1) ??? Left InvalidLag

    it "rejects n lagB" $ do
      lagB sampleBts (btsLength sampleBts) ??? Left InvalidLag

  describe "lead tests" $ do
    it "lead by one period" $ do
      case lead sampleTs 1 of
        Left e -> expectationFailure ("unexpected lead error: " ++ show e)
        Right led ->
          led ??? sampleTs
            { tsObservations = U.drop 1 (tsObservations sampleTs)
            , tsIndex = U.take (tsLength sampleTs - 1) (tsIndex sampleTs)
            }

    it "rejects -1 lead" $ do
      lead sampleTs (-1) ??? Left InvalidLead

    it "rejects n lead" $ do
      lead sampleTs (tsLength sampleTs) ??? Left InvalidLead

  describe "leadB tests" $ do
    it "leadB by one period" $ do
      case leadB sampleBts 1 of
        Left e -> expectationFailure ("unexpected leadB error: " ++ show e)
        Right led ->
          led ??? sampleBts
            { btsObservations = B.drop 1 (btsObservations sampleBts)
            , btsIndex = B.take (btsLength sampleBts - 1) (btsIndex sampleBts)
            }

    it "rejects -1 leadB" $ do
      leadB sampleBts (-1) ??? Left InvalidLead

    it "rejects n leadB" $ do
      leadB sampleBts (btsLength sampleBts) ??? Left InvalidLead
    
  describe "length, start and end for (B)TimeSeries" $ do
    it "tsLength" $ do
      tsLength sampleTs ??? 8

    it "tsStart" $ do
      tsStart sampleTs ??? 1

    it "tsEnd" $ do
      tsEnd sampleTs ??? 8

    it "btsLength" $ do
      btsLength sampleBts ??? 8

    it "btsStart" $ do
      btsStart sampleBts ??? 1

    it "btsEnd" $ do
      btsEnd sampleBts ??? 8


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