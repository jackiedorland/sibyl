module Main (main) where

import Test.Hspec
import qualified Sibyl.TimeSeriesSpec as TimeSeriesSpec

main :: IO ()
main = hspec $ do
  describe "Sibyl.TimeSeries" TimeSeriesSpec.spec