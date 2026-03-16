module Sibyl.ModelSpec (spec) where

import qualified Sibyl.Model as M
import Test.Hspec

spec :: Spec
spec = do
  describe "model core types" $ do
    it "builds informationcriteria" $ do
      let ic = M.InformationCriteria { M.aic = 10.1, M.aicc = 10.5, M.bic = 11.0, M.hqic = 10.9 }
      (M.aic ic, M.bic ic) `shouldBe` (10.1, 11.0)

    it "fiterror has eq and show" $ do
      show (M.NumericalFailure "lapack") `shouldContain` "lapack"
      M.InvalidModelSpec "x" `shouldBe` M.InvalidModelSpec "x"

    it "modelsummary payload shape is usable" $ do
      let ts = M.TrainingSummary { M.dataStart = (1 :: Int), M.dataEnd = 100, M.nObs = 100, M.sigma2 = 1.2, M.naiveScale = 0.5 }
          ms = M.ModelSummary
              { M.name         = "ARIMA(1,1,1)"
              , M.coefficients = [("ar1", 0.4, 0.1)]
              , M.criteria     = Nothing
              , M.logLik       = Nothing
              , M.converged    = Just True
              , M.errors       = Nothing
              , M.training     = ts
              }
      M.name ms `shouldBe` "ARIMA(1,1,1)"
      M.nObs (M.training ms) `shouldBe` 100
