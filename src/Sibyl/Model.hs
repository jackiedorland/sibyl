module Sibyl.Model where

data TrainingSummary t = TrainingSummary
    { length :: Int
    , start  :: t
    , end    :: t
    }

data InformationCriteria = 
    { aic  :: Double -- AIC
      aicc :: Double -- AICc
      bic  :: Double -- BIC
      hqic :: Double -- future: Hannan-Quinn?
    } deriving (Show, Eq)

data ModelSummary = ModelSummary 
    {    name         :: String -- i.e. ARIMA(1,1,1)
    ,    coefficients :: [(String, Double, Double)] -- like [("ar1", 0.42, 0.09), ("ma1", -0.31, 0.11)] 
    ,    criteria     :: Maybe InformationCriteria
    ,    sigma2       :: Double 
    ,    logLik       :: Maybe Double
    ,    nObs         :: Int
    ,    converged    :: Maybe Bool -- Nothing here means that convergence doesn't make any sense, like the naive model
    }

class SibylModel m t where 
    forecast     :: Int -> m t -> Forecast t
    residuals    :: m t -> U.Vector Double
    fitted       :: 


