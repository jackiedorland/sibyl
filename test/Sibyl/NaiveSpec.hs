{-# LANGUAGE DataKinds #-}

module Sibyl.NaiveSpec (spec) where

import qualified Data.Vector.Unboxed as U
import Sibyl.Accuracy (AccuracyError(ConstantTraining))
import Sibyl.Model
  ( ErrorMeasures(emMase)
  , FitError(..)
  , Fitted
  , ModelFamily(Naive)
  , Prediction(..)
  , Summary(summaryErrors)
  , modelSummary
  )
import Sibyl.Models.Naive
  ( NaiveMethod(..)
  , NaiveSettings(..)
  , defaultNaiveSettings
  , fitNaive
  , fitNaiveWith
  , predictNaive
  )
import Sibyl.TimeSeries
  ( TimeSeries
  , TimeSeriesError(InsufficientObservations)
  , index
  , mkTimeSeries
  , observations
  , sampleTimeSeries
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "fitNaiveWith validation" $ do
    it "rejects confidence levels outside the open unit interval" $ do
      fitNaiveWith (defaultNaiveSettings { naiveCiLevel = 0 }) sampleTimeSeries
        `shouldFailWith` isInvalidConfidenceLevel
      fitNaiveWith (defaultNaiveSettings { naiveCiLevel = 1 }) sampleTimeSeries
        `shouldFailWith` isInvalidConfidenceLevel
      fitNaiveWith (defaultNaiveSettings { naiveCiLevel = 0 / 0 }) sampleTimeSeries
        `shouldFailWith` isInvalidConfidenceLevel

    it "rejects non-finite training observations" $ do
      let ts = fixture [1, 2] [1, 1 / 0]
      fitNaive ts `shouldFailWith` isInvalidTrainingData

    it "requires a valid period and two complete seasons" $ do
      fitNaiveWith (NaiveSettings Seasonal Nothing 0.95) sampleTimeSeries
        `shouldFailWith` isInvalidModelSpec
      fitNaiveWith (NaiveSettings Seasonal (Just 1) 0.95) sampleTimeSeries
        `shouldFailWith` isInvalidModelSpec
      fitNaiveWith (NaiveSettings Seasonal (Just 5) 0.95) sampleTimeSeries
        `shouldFailWith` isInsufficientData

  describe "predictNaive" $ do
    it "rejects non-positive horizons through the checked API" $ do
      let fittedModel = fitted sampleTimeSeries
      predictNaive 0 fittedModel `shouldFailWith` isInvalidHorizon
      predictNaive (-1) fittedModel `shouldFailWith` isInvalidHorizon

    it "returns point forecasts, intervals, and future indexes" $ do
      case predictNaive 3 (fitted sampleTimeSeries) of
        Left err -> expectationFailure (show err)
        Right prediction -> do
          index (predPoint prediction) `shouldBe` U.fromList [9, 10, 11]
          observations (predPoint prediction) `shouldBe` U.replicate 3 108.0
          U.and (U.zipWith (<=) (observations (predLower prediction))
                                (observations (predPoint prediction))) `shouldBe` True
          U.and (U.zipWith (<=) (observations (predPoint prediction))
                                (observations (predUpper prediction))) `shouldBe` True
          predCILevel prediction `shouldBe` 0.95

    it "returns a checked error when a bounded Enum index cannot advance" $ do
      let ts = fixture [maxBound - 1, maxBound] [1, 2]
      predictNaive 1 (fitted ts) `shouldBeLeft`
        InvalidForecastIndex InsufficientObservations

    it "cycles the last full season for seasonal naive forecasts" $ do
      let ts = fixture [1..8] [1,2,3,4,1,2,3,4]
          settings = NaiveSettings Seasonal (Just 4) 0.9
      case fitNaiveWith settings ts >>= predictNaive 6 of
        Left err -> expectationFailure (show err)
        Right prediction ->
          observations (predPoint prediction) `shouldBe` U.fromList [1,2,3,4,1,2]

  describe "naive summaries" $ do
    it "retains an unavailable MASE reason instead of emitting NaN" $ do
      let constantSeries = fixture [1..4] [5,5,5,5]
          errors = summaryErrors (modelSummary (fitted constantSeries))
      fmap emMase errors `shouldBe` Just (Left ConstantTraining)

fitted :: TimeSeries Int Double -> Fitted 'Naive Int
fitted ts = case fitNaive ts of
  Left err -> error ("invalid fitted-model fixture: " ++ show err)
  Right model -> model

fixture :: [Int] -> [Double] -> TimeSeries Int Double
fixture idx values = case mkTimeSeries (U.fromList idx) (U.fromList values) of
  Left err -> error ("invalid time-series fixture: " ++ show err)
  Right ts -> ts

isInvalidConfidenceLevel :: Either FitError a -> Bool
isInvalidConfidenceLevel (Left (InvalidConfidenceLevel _)) = True
isInvalidConfidenceLevel _ = False

isInvalidTrainingData :: Either FitError a -> Bool
isInvalidTrainingData (Left (InvalidTrainingData _)) = True
isInvalidTrainingData _ = False

isInvalidModelSpec :: Either FitError a -> Bool
isInvalidModelSpec (Left (InvalidModelSpec _)) = True
isInvalidModelSpec _ = False

isInsufficientData :: Either FitError a -> Bool
isInsufficientData (Left (InsufficientData _)) = True
isInsufficientData _ = False

isInvalidHorizon :: Either FitError a -> Bool
isInvalidHorizon (Left (InvalidForecastHorizon _)) = True
isInvalidHorizon _ = False

shouldBeLeft :: (Eq e, Show e) => Either e a -> e -> Expectation
shouldBeLeft result expected = case result of
  Left actual -> actual `shouldBe` expected
  Right _ -> expectationFailure "expected Left, got Right"

shouldFailWith :: Either FitError a -> (Either FitError a -> Bool) -> Expectation
shouldFailWith result predicate = case result of
  Left err
    | predicate (Left err) -> pure ()
    | otherwise -> expectationFailure ("unexpected error: " ++ show err)
  Right _ -> expectationFailure "expected validation failure, got Right"
