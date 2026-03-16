module Main (main) where

import Test.Hspec
import qualified Sibyl.AccuracySpec as AccuracySpec
import qualified Sibyl.NaiveSpec as NaiveSpec
import qualified Sibyl.DecompositionSpec as DecompositionSpec
import qualified Sibyl.FacadeSpec as FacadeSpec
import qualified Sibyl.ForecastSpec as ForecastSpec
import qualified Sibyl.ModelSpec as ModelSpec
import qualified Sibyl.PlottingSpec as PlottingSpec
import qualified Sibyl.SafeTimeSeriesSpec as SafeTimeSeriesSpec
import qualified Sibyl.SmoothingSpec as SmoothingSpec
import qualified Sibyl.TimeSeriesSpec as TimeSeriesSpec

main :: IO ()
main = hspec $ do
  describe "facade" FacadeSpec.spec
  describe "timeseries" TimeSeriesSpec.spec
  describe "safe timeseries" SafeTimeSeriesSpec.spec
  describe "forecast" ForecastSpec.spec
  describe "model" ModelSpec.spec
  describe "accuracy" AccuracySpec.spec
  describe "naive" NaiveSpec.spec
  describe "smoothing" SmoothingSpec.spec
  describe "decomposition" DecompositionSpec.spec
  describe "plotting" PlottingSpec.spec