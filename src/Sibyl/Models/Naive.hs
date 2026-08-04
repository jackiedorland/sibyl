{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}

module Sibyl.Models.Naive
  ( NaiveMethod(..)
  , NaiveSettings(..)
  , defaultNaiveSettings
  , fitNaive
  , fitNaiveWith
  , predictNaive
  ) where

import Sibyl.Model
  ( Model(..), ModelFamily(..), Fitted
  , Prediction(..), Summary(..), FitError(..)
  , TrainingSummary(..), ErrorMeasures(..)
  )
import Sibyl.TimeSeries
  ( Period
  , TimeSeries
  , TimeSeriesError(InsufficientObservations)
  , mkTimeSeries
  , observations
  , tsEnd
  , tsLength
  , tsStart
  )

import qualified Sibyl.Accuracy as Accuracy
import qualified Data.Vector.Unboxed as U
import qualified Statistics.Sample as Sm
import Statistics.Distribution (quantile)
import Statistics.Distribution.Normal (standard)
import Statistics.Distribution.StudentT (studentT)

data NaiveMethod
    = Last
    | Mean
    | Drift
    | Seasonal
    deriving (Show, Eq)

data NaiveSettings = NaiveSettings
    { naiveMethod  :: NaiveMethod
    , period       :: Maybe Period
    , naiveCiLevel :: Double
    } deriving (Show, Eq)

defaultNaiveSettings :: NaiveSettings
defaultNaiveSettings = NaiveSettings Last Nothing 0.95

data instance Fitted 'Naive idx = FittedNaive
    { naiveSettings :: NaiveSettings
    , naiveSeries   :: TimeSeries idx Double
    }

fitNaive :: U.Unbox idx => TimeSeries idx Double -> Either FitError (Fitted 'Naive idx)
fitNaive = fitNaiveWith defaultNaiveSettings

fitNaiveWith :: U.Unbox idx => NaiveSettings -> TimeSeries idx Double -> Either FitError (Fitted 'Naive idx)
fitNaiveWith cfg ts
    | not (validCILevel (naiveCiLevel cfg)) = Left (InvalidConfidenceLevel (naiveCiLevel cfg))
    | U.any (not . isFinite) (observations ts) = Left (InvalidTrainingData "observations must all be finite")
    | n < 2     = Left (InsufficientData "Need at least 2 observations for naive forecast")
    | naiveMethod cfg == Seasonal = case period cfg of
        Nothing -> Left (InvalidModelSpec "Seasonal method requires a period (not Nothing)")
        Just m
            | m < 2     -> Left (InvalidModelSpec "Period must be >= 2")
            | n < 2*m   -> Left (InsufficientData "Need at least 2 full seasons")
            | otherwise -> Right (FittedNaive cfg ts)
    | otherwise = Right (FittedNaive cfg ts)
    where
        n = tsLength ts

        isFinite value = not (isNaN value || isInfinite value)

instance Model 'Naive where
    type Settings 'Naive = NaiveSettings
    type Future   'Naive = ()

    fit          = fitNaiveWith
    predict      = naivePredict
    modelSummary = naiveModelSummary
    residuals    = naiveResiduals
    fitted       = naiveFitted

-- | Forecast from a fitted naive model using the index's next 'Enum' values.
predictNaive :: (Ord idx, Enum idx, U.Unbox idx) => Int -> Fitted 'Naive idx -> Either FitError (Prediction idx)
predictNaive h = naivePredict h ()

naivePredict :: (Ord idx, Enum idx, U.Unbox idx) => Int -> () -> Fitted 'Naive idx -> Either FitError (Prediction idx)
naivePredict h _ fn
    | h <= 0    = Left (InvalidForecastHorizon h)
    | otherwise = case naiveMethod (naiveSettings fn) of
        Last     -> naivePredictLast     h fn
        Mean     -> naivePredictMean     h fn
        Drift    -> naivePredictDrift    h fn
        Seasonal -> naivePredictSeasonal h fn

naivePredictLast :: (Ord idx, Enum idx, U.Unbox idx) => Int -> Fitted 'Naive idx -> Either FitError (Prediction idx)
naivePredictLast h fn = do
    futures <- futureIndices h innerSeries
    makePrediction futures pointVals halfWidths (naiveCiLevel nsettings) resids (U.drop 1 obs)
    where
        innerSeries = naiveSeries fn
        nsettings   = naiveSettings fn
        obs         = observations innerSeries
        resids      = naiveInSampleResiduals fn
        sigma       = sqrt $ Sm.mean $ U.map (^ (2 :: Int)) resids
        z           = quantile standard ((1 + naiveCiLevel nsettings) / 2)
        pointVals   = U.replicate h (U.last obs)
        halfWidths  = U.generate h (\k -> z * sigma * sqrt (fromIntegral (k+1)))

naivePredictMean :: (Ord idx, Enum idx, U.Unbox idx) => Int -> Fitted 'Naive idx -> Either FitError (Prediction idx)
naivePredictMean h fn = do
    futures <- futureIndices h innerSeries
    makePrediction futures pointVals halfWidths (naiveCiLevel nsettings) resids obs
    where
        innerSeries = naiveSeries fn
        nsettings   = naiveSettings fn
        obs         = observations innerSeries
        resids      = naiveInSampleResiduals fn
        n           = U.length obs
        sigma       = sqrt $ Sm.varianceUnbiased resids
        z           = quantile (studentT (fromIntegral (n - 1))) ((1 + naiveCiLevel nsettings) / 2)
        pointVals   = U.replicate h (Sm.mean obs)
        halfWidths  = U.replicate h (z * sigma * sqrt (1 + 1 / fromIntegral n))

naivePredictDrift :: (Ord idx, Enum idx, U.Unbox idx) => Int -> Fitted 'Naive idx -> Either FitError (Prediction idx)
naivePredictDrift h fn = do
    futures <- futureIndices h innerSeries
    makePrediction futures pointVals halfWidths (naiveCiLevel nsettings) resids (U.drop 1 obs)
    where
        innerSeries = naiveSeries fn
        nsettings   = naiveSettings fn
        obs         = observations innerSeries
        resids      = naiveInSampleResiduals fn
        n           = U.length obs
        slope       = (U.last obs - U.head obs) / fromIntegral (n - 1)
        sigma       = sqrt $ Sm.mean $ U.map (^ (2 :: Int)) resids
        z           = quantile standard ((1 + naiveCiLevel nsettings) / 2)
        pointVals   = U.generate h (\k -> U.last obs + fromIntegral (k+1) * slope)
        halfWidths  = U.generate h (\k -> let k' = fromIntegral (k+1)
                                           in z * sigma * sqrt (k' * (1 + k' / fromIntegral n)))

naivePredictSeasonal :: (Ord idx, Enum idx, U.Unbox idx) => Int -> Fitted 'Naive idx -> Either FitError (Prediction idx)
naivePredictSeasonal h fn = do
    futures <- futureIndices h innerSeries
    makePrediction futures pointVals halfWidths (naiveCiLevel cfg) resids (U.drop m obs)
    where
        innerSeries = naiveSeries fn
        cfg         = naiveSettings fn
        obs         = observations innerSeries
        resids      = naiveInSampleResiduals fn
        n           = U.length obs
        m           = seasonalPeriod fn
        sigma       = sqrt $ Sm.mean $ U.map (^ (2 :: Int)) resids
        z           = quantile standard ((1 + naiveCiLevel cfg) / 2)
        pointVals   = U.generate h (\k -> obs U.! (n - m + (k `mod` m)))
        halfWidths  = U.generate h (\k -> z * sigma * sqrt (fromIntegral (k `div` m + 1)))

naiveInSampleResiduals :: Fitted 'Naive idx -> U.Vector Double
naiveInSampleResiduals fn = case naiveMethod (naiveSettings fn) of
        Last     -> U.zipWith (-) (U.drop 1 obs) (U.take (n-1) obs)
        Mean     -> U.map (subtract $ Sm.mean obs) obs
        Drift    -> U.zipWith (\next cur -> next - (cur + slope)) (U.drop 1 obs) (U.take (n-1) obs)
        Seasonal -> U.zipWith (-) (U.drop m obs) (U.take (n-m) obs)
    where
        obs   = observations $ naiveSeries fn
        slope = (U.last obs - U.head obs) / fromIntegral (n-1)
        n     = U.length obs
        m     = seasonalPeriod fn

naiveResiduals :: Fitted 'Naive idx -> U.Vector Double
naiveResiduals = naiveInSampleResiduals

naiveFitted :: Fitted 'Naive idx -> U.Vector Double
naiveFitted fn = U.zipWith (-) acts (naiveInSampleResiduals fn)
  where
    obs  = observations (naiveSeries fn)
    m    = seasonalPeriod fn
    acts = case naiveMethod (naiveSettings fn) of
      Last     -> U.drop 1 obs
      Mean     -> obs
      Drift    -> U.drop 1 obs
      Seasonal -> U.drop m obs

naiveModelSummary :: U.Unbox idx => Fitted 'Naive idx -> Summary idx
naiveModelSummary fn = Summary
    { summaryName      = "Naive (" ++ show method ++ ")"
    , summaryCoeffs    = []
    , summaryCriteria  = Nothing
    , summaryLogLik    = Nothing
    , summaryConverged = Nothing
    , summaryErrors    = Just ErrorMeasures
                        { emMe   = Sm.mean resids
                        , emRmse = Accuracy.rmse resids
                        , emMae  = Accuracy.mae  resids
                        , emMape = Accuracy.mape resids acts
                        , emMase = Accuracy.mase resids naiveScaleVal
                        }
    , summaryTraining  = TrainingSummary
                        { dataStart  = tsStart innerSeries
                        , dataEnd    = tsEnd innerSeries
                        , nObs       = n
                        , sigma2     = Sm.mean (U.map (^ (2 :: Int)) resids)
                        , naiveScale = naiveScaleVal
                        }
    }
    where
        method       = naiveMethod (naiveSettings fn)
        innerSeries  = naiveSeries fn
        obs          = observations innerSeries
        n            = U.length obs
        m            = seasonalPeriod fn
        resids       = naiveResiduals fn
        naiveScaleVal = Accuracy.mae $ U.zipWith (-) (U.drop m obs) (U.take (n-m) obs)
        acts         = case method of
            Last     -> U.drop 1 obs
            Mean     -> obs
            Drift    -> U.drop 1 obs
            Seasonal -> U.drop m obs

validCILevel :: Double -> Bool
validCILevel level =
    not (isNaN level || isInfinite level) && level > 0 && level < 1

seasonalPeriod :: Fitted 'Naive idx -> Period
seasonalPeriod fn = case period (naiveSettings fn) of
    Just m  -> m
    Nothing -> 1

futureIndices
    :: (Ord idx, Enum idx, U.Unbox idx)
    => Int
    -> TimeSeries idx Double
    -> Either FitError (U.Vector idx)
futureIndices h ts =
    let candidates = take h (drop 1 (enumFrom (tsEnd ts)))
        future = U.fromList candidates
    in if U.length future /= h
         then Left (InvalidForecastIndex InsufficientObservations)
         else case mkTimeSeries future (U.replicate h ()) of
           Left err -> Left (InvalidForecastIndex err)
           Right _  -> Right future

makePrediction
    :: (Ord idx, U.Unbox idx)
    => U.Vector idx
    -> U.Vector Double
    -> U.Vector Double
    -> Double
    -> U.Vector Double
    -> U.Vector Double
    -> Either FitError (Prediction idx)
makePrediction futures pointVals halfWidths ciLevel resids actuals = do
    point <- checkedSeries pointVals
    lower <- checkedSeries (U.zipWith (-) pointVals halfWidths)
    upper <- checkedSeries (U.zipWith (+) pointVals halfWidths)
    Right Prediction
        { predPoint = point
        , predLower = lower
        , predUpper = upper
        , predCILevel = ciLevel
        , predResiduals = resids
        , predActuals = actuals
        }
  where
    checkedSeries values = case mkTimeSeries futures values of
        Left err -> Left (InvalidForecastIndex err)
        Right ts -> Right ts
