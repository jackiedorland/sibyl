{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Sibyl.Models.Naive where

import Sibyl.Model
import Sibyl.Forecast (Forecast(..), point, lower, upper, actuals)
import Sibyl.Safe.TimeSeries (TimeSeries, Period, observations, tsEnd)
import Sibyl.TimeSeries (mkTimeSeries)

import Data.Maybe (fromJust)
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
    { naiveMethod :: NaiveMethod
    , period :: Maybe Period
    , naiveCiLevel :: Double
    }

defaultNaiveSettings :: NaiveSettings
defaultNaiveSettings = NaiveSettings Last Nothing 0.95

data Naive t = Naive 
    { settings    :: NaiveSettings
    , naiveSeries :: TimeSeries t Double
    }

fitNaive :: U.Unbox t => NaiveSettings -> TimeSeries t Double -> Either FitError (Naive t)
fitNaive = undefined

instance (Ord t, Enum t, U.Unbox t) => SibylModel Naive t where
    forecast     = naiveForecast
    residuals    = naiveResiduals
    fitted       = naiveFitted
    summarize    = naiveSummarize
    modelSummary = naiveModelSummary

naiveForecast :: (Ord t, Enum t, U.Unbox t) => Int -> Naive t -> Forecast t
naiveForecast h naive = case naiveMethod (settings naive) of
        Last     -> naiveForecastLast     h naive
        Mean     -> naiveForecastMean     h naive
        Drift    -> naiveForecastDrift    h naive
        Seasonal -> naiveForecastSeasonal h naive

naiveForecastLast :: (Ord t, Enum t, U.Unbox t) => Int -> Naive t -> Forecast t
naiveForecastLast h naive = Forecast 
    { point = mkTimeSeries futures pointVals
    , lower = mkTimeSeries futures (U.zipWith (-) pointVals halfWidths)
    , upper = mkTimeSeries futures (U.zipWith (+) pointVals halfWidths)
    , ciLevel = naiveCiLevel nsettings
    , residuals = resids
    , actuals = U.drop 1 obs
    }
    where
        -- type setup
        innerSeries = naiveSeries naive
        nsettings   = settings naive
        obs         = observations innerSeries  -- training observations
        n           = U.length obs
        resids      = naiveInSampleResiduals naive
        -- math operations
        sigma      = sqrt $ Sm.mean $ U.map (^2) resids -- residual std dev
        z          = quantile standard ((1 + naiveCiLevel nsettings) / 2) -- normal quantile for CI
        lastIdx    = tsEnd innerSeries
        futures    = U.fromList $ take h $ tail $ iterate succ lastIdx -- future time index
        pointVals  = U.replicate h (U.last obs) -- ŷ_{T+k} = y_T
        halfWidths = U.generate h (\k -> z * sigma * sqrt (fromIntegral (k+1))) -- hw_k = z*σ*√k

naiveForecastMean :: (Ord t, Enum t, U.Unbox t) => Int -> Naive t -> Forecast t
naiveForecastMean h naive = Forecast 
    { point = mkTimeSeries futures pointVals
    , lower = mkTimeSeries futures (U.zipWith (-) pointVals halfWidths)
    , upper = mkTimeSeries futures (U.zipWith (+) pointVals halfWidths)
    , ciLevel = naiveCiLevel nsettings
    , residuals = resids
    , actuals = obs
    }
    where
        -- type setup
        innerSeries = naiveSeries naive
        nsettings   = settings naive
        obs         = observations innerSeries
        n           = U.length obs
        resids      = naiveInSampleResiduals naive
        -- math operations
        sigma = sqrt $ Sm.varianceUnbiased resids  -- Sm.variance uses n-1!
        z           = quantile (studentT (fromIntegral (n - 1))) ((1 + naiveCiLevel nsettings) / 2) 
        lastIdx     = tsEnd innerSeries
        futures     = U.fromList $ take h $ tail $ iterate succ lastIdx 
        pointVals   = U.replicate h (Sm.mean obs)
        halfWidths  = U.replicate h (z * sigma * sqrt (1 + 1 / fromIntegral n))

naiveForecastDrift :: (Ord t, Enum t, U.Unbox t) => Int -> Naive t -> Forecast t
naiveForecastDrift h naive = undefined

naiveForecastSeasonal :: (Ord t, Enum t, U.Unbox t) => Int -> Naive t -> Forecast t
naiveForecastSeasonal h naive = undefined

naiveInSampleResiduals :: (U.Unbox t) => Naive t -> U.Vector Double
naiveInSampleResiduals naive = case (naiveMethod (settings naive)) of
        Last     -> U.zipWith (-) (U.drop 1 obs) (U.take (n-1) obs)
        Mean     -> U.map (subtract $ Sm.mean(obs)) obs
        Drift    -> U.zipWith (\next cur -> next - (cur + slope)) (U.drop 1 obs) (U.take (n-1) obs)
        Seasonal -> U.zipWith (-) (U.drop m obs) (U.take (n-m) obs)
    where 
        obs = observations $ naiveSeries naive
        slope = (U.last obs - U.head obs) / fromIntegral (n-1)
        n = U.length obs
        m = fromJust $ period (settings naive)

naiveResiduals :: (U.Unbox t) => Naive t -> U.Vector Double
naiveResiduals n = naiveInSampleResiduals n

naiveFitted :: Naive t -> U.Vector Double
naiveFitted = undefined

naiveSummarize :: Naive t -> IO ()
naiveSummarize = undefined

naiveModelSummary :: Naive t -> ModelSummary
naiveModelSummary = undefined