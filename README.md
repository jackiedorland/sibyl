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
## A performant, R-style time series analysis library for Haskell.

Sibyl is in extremely early development.

### Core Infrastructure
- [x] Unboxed and boxed time series types
- [x] Safe constructors with invariant enforcement
- [x] Explicit, pattern-matchable error types

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