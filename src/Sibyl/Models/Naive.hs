{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}

module Sibyl.Models.Naive
  ( NaiveMethod(..)
  , NaiveSettings(..)
  , defaultNaiveSettings
  , fitNaive
  , fitNaiveWith
  ) where

import Sibyl.Model
  ( Model(..), ModelFamily(..), Fitted
  , Prediction(..), Summary(..), FitError(..)
  , TrainingSummary(..), ErrorMeasures(..)
  )
import Sibyl.TimeSeries (TimeSeries, Period, observations, tsEnd, tsStart, tsLength)
import Sibyl (mkTimeSeries)

import Data.Maybe (fromJust, fromMaybe)
import Data.Either (fromRight)
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
    }

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

instance Model 'Naive where
    type Settings 'Naive = NaiveSettings
    type Future   'Naive = ()

    fit          = fitNaiveWith
    predict      = naivePredict
    modelSummary = naiveModelSummary
    residuals    = naiveResiduals
    fitted       = naiveFitted

naivePredict :: (Ord idx, Enum idx, U.Unbox idx) => Int -> () -> Fitted 'Naive idx -> Either FitError (Prediction idx)
naivePredict h _ fn = case naiveMethod (naiveSettings fn) of
    Last     -> naivePredictLast     h fn
    Mean     -> naivePredictMean     h fn
    Drift    -> naivePredictDrift    h fn
    Seasonal -> naivePredictSeasonal h fn

naivePredictLast :: (Ord idx, Enum idx, U.Unbox idx) => Int -> Fitted 'Naive idx -> Either FitError (Prediction idx)
naivePredictLast h fn = Right Prediction
    { predPoint     = mkTimeSeries futures pointVals
    , predLower     = mkTimeSeries futures (U.zipWith (-) pointVals halfWidths)
    , predUpper     = mkTimeSeries futures (U.zipWith (+) pointVals halfWidths)
    , predCILevel   = naiveCiLevel nsettings
    , predResiduals = resids
    , predActuals   = U.drop 1 obs
    }
    where
        innerSeries = naiveSeries fn
        nsettings   = naiveSettings fn
        obs         = observations innerSeries
        resids      = naiveInSampleResiduals fn
        lastIdx     = tsEnd innerSeries
        n           = U.length obs
        sigma       = sqrt $ Sm.mean $ U.map (^2) resids
        z           = quantile standard ((1 + naiveCiLevel nsettings) / 2)
        futures     = U.fromList $ take h $ tail $ iterate succ lastIdx
        pointVals   = U.replicate h (U.last obs)
        halfWidths  = U.generate h (\k -> z * sigma * sqrt (fromIntegral (k+1)))

naivePredictMean :: (Ord idx, Enum idx, U.Unbox idx) => Int -> Fitted 'Naive idx -> Either FitError (Prediction idx)
naivePredictMean h fn = Right Prediction
    { predPoint     = mkTimeSeries futures pointVals
    , predLower     = mkTimeSeries futures (U.zipWith (-) pointVals halfWidths)
    , predUpper     = mkTimeSeries futures (U.zipWith (+) pointVals halfWidths)
    , predCILevel   = naiveCiLevel nsettings
    , predResiduals = resids
    , predActuals   = obs
    }
    where
        innerSeries = naiveSeries fn
        nsettings   = naiveSettings fn
        obs         = observations innerSeries
        resids      = naiveInSampleResiduals fn
        lastIdx     = tsEnd innerSeries
        n           = U.length obs
        sigma       = sqrt $ Sm.varianceUnbiased resids
        z           = quantile (studentT (fromIntegral (n - 1))) ((1 + naiveCiLevel nsettings) / 2)
        futures     = U.fromList $ take h $ tail $ iterate succ lastIdx
        pointVals   = U.replicate h (Sm.mean obs)
        halfWidths  = U.replicate h (z * sigma * sqrt (1 + 1 / fromIntegral n))

naivePredictDrift :: (Ord idx, Enum idx, U.Unbox idx) => Int -> Fitted 'Naive idx -> Either FitError (Prediction idx)
naivePredictDrift h fn = Right Prediction
    { predPoint     = mkTimeSeries futures pointVals
    , predLower     = mkTimeSeries futures (U.zipWith (-) pointVals halfWidths)
    , predUpper     = mkTimeSeries futures (U.zipWith (+) pointVals halfWidths)
    , predCILevel   = naiveCiLevel nsettings
    , predResiduals = resids
    , predActuals   = U.drop 1 obs
    }
    where
        innerSeries = naiveSeries fn
        nsettings   = naiveSettings fn
        obs         = observations innerSeries
        resids      = naiveInSampleResiduals fn
        lastIdx     = tsEnd innerSeries
        n           = U.length obs
        slope       = (U.last obs - U.head obs) / fromIntegral (n - 1)
        sigma       = sqrt $ Sm.mean $ U.map (^2) resids
        z           = quantile standard ((1 + naiveCiLevel nsettings) / 2)
        futures     = U.fromList $ take h $ tail $ iterate succ lastIdx
        pointVals   = U.generate h (\k -> U.last obs + fromIntegral (k+1) * slope)
        halfWidths  = U.generate h (\k -> let k' = fromIntegral (k+1)
                                           in z * sigma * sqrt (k' * (1 + k' / fromIntegral n)))

naivePredictSeasonal :: (Ord idx, Enum idx, U.Unbox idx) => Int -> Fitted 'Naive idx -> Either FitError (Prediction idx)
naivePredictSeasonal h fn = Right Prediction
    { predPoint     = mkTimeSeries futures pointVals
    , predLower     = mkTimeSeries futures (U.zipWith (-) pointVals halfWidths)
    , predUpper     = mkTimeSeries futures (U.zipWith (+) pointVals halfWidths)
    , predCILevel   = naiveCiLevel cfg
    , predResiduals = resids
    , predActuals   = U.drop m obs
    }
    where
        innerSeries = naiveSeries fn
        cfg         = naiveSettings fn
        obs         = observations innerSeries
        resids      = naiveInSampleResiduals fn
        lastIdx     = tsEnd innerSeries
        n           = U.length obs
        m           = fromJust $ period cfg
        sigma       = sqrt $ Sm.mean $ U.map (^2) resids
        z           = quantile standard ((1 + naiveCiLevel cfg) / 2)
        futures     = U.fromList $ take h $ tail $ iterate succ lastIdx
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
        m     = fromJust $ period (naiveSettings fn)

naiveResiduals :: Fitted 'Naive idx -> U.Vector Double
naiveResiduals = naiveInSampleResiduals

naiveFitted :: Fitted 'Naive idx -> U.Vector Double
naiveFitted fn = U.zipWith (-) acts (naiveInSampleResiduals fn)
  where
    obs  = observations (naiveSeries fn)
    n    = U.length obs
    m    = fromJust $ period (naiveSettings fn)
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
                        , emMape = fromRight (0/0) $ Accuracy.mape resids acts
                        , emMase = fromRight (0/0) $ Accuracy.mase resids naiveScaleVal
                        }
    , summaryTraining  = TrainingSummary
                        { dataStart  = tsStart innerSeries
                        , dataEnd    = tsEnd innerSeries
                        , nObs       = n
                        , sigma2     = Sm.varianceUnbiased resids
                        , naiveScale = naiveScaleVal
                        }
    }
    where
        method       = naiveMethod (naiveSettings fn)
        innerSeries  = naiveSeries fn
        obs          = observations innerSeries
        n            = U.length obs
        m            = fromMaybe 1 (period (naiveSettings fn))
        resids       = naiveResiduals fn
        naiveScaleVal = Accuracy.mae $ U.zipWith (-) (U.drop m obs) (U.take (n-m) obs)
        acts         = case method of
            Last     -> U.drop 1 obs
            Mean     -> obs
            Drift    -> U.drop 1 obs
            Seasonal -> U.drop m obs
