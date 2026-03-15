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

Sibyl is in extremely early development.

What's it look like?
```haskell
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import qualified Sibyl.Unsafe as S

main :: IO ()
main = do
       ts <- TS.fromDataFrame $ D.readCsv "./dataset.csv" -- DataFrame -> TimeSeries
       summarize $ runARIMA ts 
       -- or:
       plot $ runARIMA ts 
```

### Core Infrastructure
- [x] Unsafe timeseries for tools (`Sibyl.TimeSeries`)
- [x] Safe timeseries for production pipelines (`Sibyl.Safe.TimeSeries`)
- [x] Constructors with invariant enforcement (`error`)

### Transformations
- [x] Lag and lead
- [ ] Differencing and integration
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
- [ ] Forecast accuracy metrics (MAE, RMSE, MAPE, MASE)
- [ ] Residual diagnostics (ACF, Ljung-Box)
- [ ] Cross-validation (time series CV / rolling origin)
- [ ] Information criteria (AIC, AICc, BIC)

### Future
- [ ] Multivariate / VAR models
- [ ] Hierarchical / grouped forecasting