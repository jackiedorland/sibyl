{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}

module Sibyl.Model
  ( ModelFamily(..)
  , Model(..)
  , Fitted
  , Prediction(..)
  , Summary(..)
  , FitError(..)
  , IC(..)
  , RegressorMatrix(..)
  , TrainingSummary(..)
  , InformationCriteria(..)
  , ErrorMeasures(..)
  ) where

import Data.Kind (Type)
import qualified Data.Vector.Unboxed as U
import Sibyl.TimeSeries (TimeSeries)
import qualified Numeric.LinearAlgebra as LinAlg

data ModelFamily
  = ARIMA
  | SARIMA
  | SARIMAX
  | Naive
  | ETS
  | Theta
  | TBATS
  | NNETAR

data family Fitted (mdl :: ModelFamily) idx

class Model (mdl :: ModelFamily) where
  type Settings mdl :: Type
  type Future   mdl :: Type

  fit          :: U.Unbox idx => Settings mdl -> TimeSeries idx Double -> Either FitError (Fitted mdl idx)
  predict      :: (Ord idx, Enum idx, U.Unbox idx) => Int -> Future mdl -> Fitted mdl idx -> Either FitError (Prediction idx)
  modelSummary :: U.Unbox idx => Fitted mdl idx -> Summary idx
  summarize    :: U.Unbox idx => Fitted mdl idx -> IO ()
  summarize    =  print . modelSummary
  residuals    :: Fitted mdl idx -> U.Vector Double
  fitted       :: Fitted mdl idx -> U.Vector Double

data IC = AIC | AICc | BIC deriving (Show, Eq)

data RegressorMatrix = RegressorMatrix
  { regressorNames :: [String]
  , regressorData  :: LinAlg.Matrix Double
  } deriving (Show, Eq)

data Prediction idx = Prediction
  { predPoint     :: TimeSeries idx Double
  , predLower     :: TimeSeries idx Double
  , predUpper     :: TimeSeries idx Double
  , predCILevel   :: Double
  , predResiduals :: U.Vector Double
  , predActuals   :: U.Vector Double
  }

data FitError
  = InvalidModelSpec String
  | InsufficientData String
  | NumericalFailure String
  deriving (Show, Eq)

data TrainingSummary idx = TrainingSummary
  { dataStart  :: idx
  , dataEnd    :: idx
  , nObs       :: Int
  , sigma2     :: Double
  , naiveScale :: Double
  } deriving (Show, Eq)

data InformationCriteria = InformationCriteria
  { aic  :: Double
  , aicc :: Double
  , bic  :: Double
  , hqic :: Double
  } deriving (Show, Eq)

data ErrorMeasures = ErrorMeasures
  { emMe   :: Double
  , emRmse :: Double
  , emMae  :: Double
  , emMape :: Double
  , emMase :: Double
  } deriving (Show, Eq)

data Summary idx = Summary
  { summaryName      :: String
  , summaryCoeffs    :: [(String, Double, Double)]
  , summaryCriteria  :: Maybe InformationCriteria
  , summaryLogLik    :: Maybe Double
  , summaryConverged :: Maybe Bool
  , summaryErrors    :: Maybe ErrorMeasures
  , summaryTraining  :: TrainingSummary idx
  }

instance Show (Summary idx) where
  show s = unlines
    [ summaryName s
    , replicate (length (summaryName s)) '='
    , ""
    , pad "Coefficients:"
    , unlines [ "  " ++ pad coef ++ show val ++ "  (se: " ++ show se ++ ")"
              | (coef, val, se) <- summaryCoeffs s ]
    , pad "Log-likelihood:" ++ maybe "N/A" show (summaryLogLik s)
    , pad "Converged:"      ++ maybe "N/A" show (summaryConverged s)
    , ""
    , case summaryCriteria s of
        Nothing -> ""
        Just ic -> unlines
          [ "Information criteria:"
          , "  " ++ pad "AIC:"  ++ show (aic  ic)
          , "  " ++ pad "AICc:" ++ show (aicc ic)
          , "  " ++ pad "BIC:"  ++ show (bic  ic)
          ]
    , ""
    , case summaryErrors s of
        Nothing -> "Error measures: N/A"
        Just e  -> unlines
          [ "Error measures:"
          , "  " ++ pad "ME:"   ++ show (emMe   e)
          , "  " ++ pad "RMSE:" ++ show (emRmse e)
          , "  " ++ pad "MAE:"  ++ show (emMae  e)
          , "  " ++ pad "MAPE:" ++ show (emMape e)
          , "  " ++ pad "MASE:" ++ show (emMase e)
          ]
    ]
    where
      pad str = str ++ replicate (20 - length str) ' '