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

## A performant, R-style time series analysis library for Haskell.

##### BE AWARE: Sibyl is in extremely early development. Many features are unimplemented or defined as `undefined`.

### Why Sibyl?

R lets you do analysis fast. It also lets you shoot yourself in the foot fast. Sibyl is a statistical library made for people who agree that statisticians and developers deserve better!

Sibyl is split into two layers, one for developers, and one for statisticians in notebooks like [Jupyter](https://jupyter.org) or [Sabela](https://github.com/DataHaskell/sabela). `import Sibyl` gives you the notebook-friendly layer: functions that throw on failure rather than returning `Either`. For production pipelines,  you can import individual modules like `Sibyl.Safe.TimeSeries` or `Sibyl.Accuracy` directly to get full error handling. Both layers can be used together in the same file.

So, what will it look like (eventually)?

##### For statisticians:
```haskell
import qualified DataFrame as D
import Sibyl

main :: IO ()
main = do
  -- you can also do this in GHCi or the DataFrame REPL!
  series   <- fromDataFrame (D.readCsv "./dataset.csv")
  model    <- fitARIMA defaultARIMASettings series
  summarize model
  autoplot $ forecast 12 model
```

##### For developers and those who need safe error handling:
```haskell
import qualified DataFrame as D
import qualified Data.Vector.Unboxed as U
import qualified Sibyl.Safe.TimeSeries as TS
import qualified Sibyl.Models.ARIMA as ARIMA
import qualified Sibyl.Model as M

main :: IO ()
main = do
  raw <- D.readCsv "./dataset.csv"

  -- an example, with error handling:
  case TS.mkTimeSeries (extractIndex raw) (extractValues raw) of
    Left tsErr -> do
      logError ("failed! " ++ show tsErr)
      fail ":("

    Right series -> do
      fitResult <- ARIMA.fitARIMA ARIMA.defaultARIMASettings series
      case fitResult of
        Left fitErr -> logError ("fit failure! " ++ show fitErr)
        Right model -> do
          let fc = M.forecast 12 model
              summary = M.modelSummary model
          writeJson "./artifacts/model-summary.json" summary
          writeJson "./artifacts/forecast.json" fc

-- writeJson, extractIndex, extractValues, etc...
```

### Core Infrastructure
- [x] Unsafe timeseries for tools (`Sibyl.TimeSeries`)
- [x] Safe timeseries for production pipelines (`Sibyl.Safe.TimeSeries`)
- [x] Constructors with invariant enforcement

### Transformations
- [x] Lag and lead on `TimeSeries`
- [x] Series differencing 
- [ ] Rolling / sliding window operations
- [ ] Seasonal adjustments

### Decomposition
- [ ] Classical decomposition (additive / multiplicative)
- [ ] STL decomposition
- [ ] Trend-cycle estimation

### Smoothing
- [ ] Simple moving averages
- [ ] Exponential smoothing (SES, Holt, Holt-Winters)
- [ ] Kernel smoothing

### Regression
- [ ] Linear regression with time series features
- [ ] Dynamic regression (regression + ARIMA errors)
- [ ] Fourier terms for seasonality

### ARIMA
- [ ] AR, MA, ARMA
- [ ] ARIMA
- [ ] Seasonal ARIMA (SARIMA)
- [ ] Automatic model selection (AIC/BIC-based)

### ETS
- [ ] State space models
- [ ] Automatic ETS selection

### Evaluation & Diagnostics
- [x] Forecast accuracy metrics (MAE, RMSE, MAPE, MASE)
- [ ] Bias & coverage
- [ ] Residual diagnostics (ACF, Ljung-Box)
- [ ] Cross-validation (time series CV / rolling origin)
- [ ] Information criteria (AIC, AICc, BIC)

### Future
- [ ] Multivariate / VAR models
- [ ] Hierarchical / grouped forecasting