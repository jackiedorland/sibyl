module Main (main) where

import Test.Hspec
import qualified Sibyl.AccuracySpec as AccuracySpec
import qualified Sibyl.FacadeSpec as FacadeSpec
import qualified Sibyl.NaiveSpec as NaiveSpec
import qualified Sibyl.SafeTimeSeriesSpec as SafeTimeSeriesSpec
import qualified Sibyl.SmoothingSpec as SmoothingSpec
import qualified Sibyl.TimeSeriesSpec as TimeSeriesSpec

main :: IO ()
main = hspec $ do
  describe "facade" FacadeSpec.spec
  describe "timeseries" TimeSeriesSpec.spec
  describe "safe timeseries" SafeTimeSeriesSpec.spec
  describe "accuracy" AccuracySpec.spec
  describe "smoothing" SmoothingSpec.spec
  describe "naive" NaiveSpec.spec
