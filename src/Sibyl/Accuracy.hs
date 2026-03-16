module Sibyl.Accuracy
  ( mae, rmse, mape, mase, maseFromModel
  , AccuracyError(..)
  ) where

import qualified Data.Vector.Unboxed as U
import Statistics.Sample as Sm
import Sibyl.Model (SibylModel(..), naiveScale, training, modelSummary)

data AccuracyError
    = ZeroInActuals
    | ConstantTraining
    deriving (Show, Eq)

mae :: U.Vector Double -> Double
mae resids = Sm.mean $ U.map abs resids

rmse :: U.Vector Double -> Double
rmse resids = sqrt $ Sm.mean $ U.map (^2) resids

-- | actuals = fitted + residuals; pass both to avoid recomputing
mape :: U.Vector Double -> U.Vector Double -> Either AccuracyError Double
mape resids acts
    | U.any (== 0) acts = Left ZeroInActuals
    | otherwise         = Right $ (*100) $ Sm.mean $ U.zipWith (\r a -> abs r / abs a) resids acts

mase :: U.Vector Double -> Double -> Either AccuracyError Double
mase resids scale
    | scale == 0 = Left ConstantTraining
    | otherwise  = Right $ mae resids / scale

maseFromModel :: (SibylModel m t, U.Unbox t) => m t -> Either AccuracyError Double
maseFromModel m = mase (residuals m) (naiveScale (training (modelSummary m)))