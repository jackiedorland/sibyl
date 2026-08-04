<pre style="background:none; border:none; line-height:1.1;">
          ███  █████                ████ 
         ░░░  ░░███                ░░███ 
  █████  ████  ░███████  █████ ████ ░███ 
 ███░░  ░░███  ░███░░███░░███ ░███  ░███ 
░░█████  ░███  ░███ ░███ ░███ ░███  ░███ 
 ░░░░███ ░███  ░███ ░███ ░███ ░███  ░███ 
 ██████  █████ ████████  ░░███████  █████               meow!
░░░░░░  ░░░░░ ░░░░░░░░    ░░░░░███ ░░░░░           ╱|、
                          ███ ░███               (˚ˎ 。7  
                         ░░██████                 |、˜〵 
                          ░░░░░░                  じしˍ,)ノ 
</pre>

> *Tell us what the future holds, so we may know that you are gods.*

## Notebook-friendly, dataframe-oriented forecasting for Haskell

Sibyl is designed first for interactive statistical exploration. Import one module, fit a model, predict a horizon, and inspect the result:

```haskell
import Sibyl

model = fit defaultNaiveSettings sampleTimeSeries
forecast = predict 12 model
```

The notebook facade deliberately reads like R or Python while the underlying direct modules retain checked `Either` results for applications. Sibyl is narrower than a general statistics toolkit: the goal is a coherent R-`forecast`-style workflow that starts and ends with Haskell `DataFrame`s.

The current `0.0.0.1` surface is a stabilized foundation, not the full vision. Naive forecasting is the only release-ready model family today. ARIMA, Holt-Winters, decomposition, plotting, and dataframe-native grouped workflows remain under development and are not exposed as working features.

## What works now

- Opaque `TimeSeries index value` values with checked construction (non-empty, equal-length vectors and a strictly increasing index).
- Safe slicing, lag/lead, ordinary and seasonal differencing, and rolling aggregations.
- Rolling mean, variance, standard deviation, sum, minimum, maximum, median, and Pearson correlation.
- Simple moving average and manually or automatically tuned single exponential smoothing.
- Last-value, mean, drift, and seasonal naive forecasts with prediction intervals.
- Fitted values, residuals, model summaries, and MAE/RMSE/MAPE/MASE.
- Basic conversion of one index column and one numeric value column to/from `DataFrame`.
- Generic notebook operations like `fit`, `predict`, `predictWith`, `summarize`, `fitted`, and `residuals`

## Quick start

### Notebook and REPL use

`import Sibyl` is the primary exploration interface.

```haskell
import qualified Data.Vector.Unboxed as U
import Sibyl

sales = mkTimeSeries
  (U.fromList [1..8 :: Int])
  (U.fromList [10, 20, 30, 40, 12, 22, 32, 42 :: Double])

settings = defaultNaiveSettings
  { naiveMethod = Seasonal
  , period = Just 4
  , naiveCiLevel = 0.95
  }

model = fit settings sales
forecast = predict 6 model

forecastValues = observations (predPoint forecast)
lowerBounds = observations (predLower forecast)
upperBounds = observations (predUpper forecast)

-- In IO / an IHaskell cell:
-- summarize model
```

The central vocabulary is model-independent:

```haskell
fit         settings series
predict     horizon model
predictWith horizon futureData model
summarize   model
fitted      model
residuals   model
```

`fit` infers the model family from the settings value. `predict` is for models that need no future inputs. `predictWith` supports future regressors or other model-specific future data without complicating the common case.

Model-specific helpers such as `fitNaive` and `forecastNaive` remain as optional conveniences, if you prefer them.

### Safe use

Import direct modules when you need explicit error handling. They expose the same implementation through `Either`:

```haskell
import Sibyl.Model (Prediction(..))
import Sibyl.Models.Naive (fitNaive, predictNaive)
import Sibyl.TimeSeries (index, observations, sampleTimeSeries)

main :: IO ()
main = case fitNaive sampleTimeSeries >>= predictNaive 3 of
  Left err -> print err
  Right prediction -> do
    print (index (predPoint prediction))
    print (observations (predPoint prediction))
```

## DataFrame status

Basic interop exists today:

```haskell
fromDataFrame :: Text -> Text -> DataFrame -> Either ConversionError (TimeSeries index Double)
toDataFrame   :: TimeSeries index value -> DataFrame
```

but this is first priority for the future.

## Modules

- `Sibyl` is the primary notebook/REPL facade with generic `fit` and `predict` operations.
- `Sibyl.TimeSeries` has checked series construction and transformations.
- `Sibyl.Smoothing` is moving-average and single-exponential smoothing.
- `Sibyl.Accuracy` has forecasting accuracy measures.
- `Sibyl.Model` contains the common fitted-model, prediction, summary, and error types.
- `Sibyl.Models.Naive` has the "safe" naive model fitting and prediction.

## Near-term roadmap

1. DataFrame 1.1 migration and first-class single/grouped frame workflows.
2. Forecast frequency/calendar policies that replace implicit `Enum` index extension.
3. ACF/PACF, Ljung-Box, and reference-value diagnostic tests.
4. Optimized Holt/Holt-Winters with intervals.
5. ARIMA/SARIMA/SARIMAX and deterministic automatic order selection.
6. Rolling-origin cross-validation and accuracy by horizon.

## Building

Sibyl supports GHC 9.6.7 and 9.10.3.

```bash
cabal build all --enable-tests
cabal test all
```

## License

BSD-3-Clause. See [LICENSE](LICENSE).
