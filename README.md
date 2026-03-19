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

### BE AWARE: Sibyl is in extremely early development. Many features are unimplemented or `= undefined`. 

### Why Sibyl?

Haskell has strong numerical libraries (`statistics`, `hmatrix`, `vector`) but nothing equivalent to R's `forecast` or Python's `statsmodels`. Anyone doing time series work in Haskell today has to either call out to R or Python via FFI, or implement everything themselves. Sibyl is designed to change that.

Sibyl sits as a domain-specific analysis layer on top of [`dataframe`](https://hackage.haskell.org/package/dataframe): load tabular data with `dataframe`, pull a column into a `TimeSeries`, fit a model, and get results back as a `DataFrame` for export or plotting. The workflow mirrors how R's ecosystem is structured: `dataframe` is to `dplyr` what Sibyl is to `forecast`.

### Design Philosophy

Sibyl is built around three principles:

**Be easy to use.** Sibyl keeps similar names to R/Python functions (`predict`, `summarize`, `fit`) and implements similar algorithms. Statisticians familiar with R should feel right at home. `import Sibyl` gives you a notebook-friendly facade that throws on failure rather than returning `Either`, just like R. For production pipelines, import individual modules like `Sibyl.Safe.TimeSeries` or `Sibyl.Models.ARIMA` directly for full error handling.

**Fail loudly and correctly.** R tends to fail in hard-to-debug, silent ways -- NA propagation being the most infamous example. Sibyl enforces validation at construction time via smart constructors (`mkTimeSeries`), so bad data doesn't make it far enough to cause silent downstream failures.

**Allow for extensibility.** Core types and functions are exposed so you can bring your own model. The `Model` typeclass is indexed by a `ModelFamily` kind (via `DataKinds`), with associated type families for `Settings` and `Future` inputs. Self-contained models set `Future` to `()`; models with exogenous regressors like SARIMAX set it to `RegressorMatrix`. Adding a new model means adding a constructor to `ModelFamily`, a `data instance` for the fitted model, and a `Model` instance -- and it integrates with the existing framework and DataFrame exports automatically.

---

### What's Implemented

**Core Infrastructure**
- [x] Unsafe timeseries for interactive use (`Sibyl.TimeSeries`)
- [x] Safe timeseries for production pipelines (`Sibyl.Safe.TimeSeries`)
- [x] Smart constructors with invariant enforcement (strictly increasing index, equal lengths, non-empty)

**Transformations**
- [x] Lag and lead
- [x] First-order differencing
- [x] Slicing, `takeFirst`, `takeLast`, `zipWithSeries`
- [ ] Higher-order and seasonal differencing
- [ ] Rolling / sliding window operations

**Forecasting & Models**
- [x] Naive forecasting (Last, Mean, Drift, Seasonal) with prediction intervals
- [x] Forecast accuracy metrics (MAE, RMSE, MAPE, MASE)
- [ ] Exponential smoothing (SES, Holt, Holt-Winters, damped trend)
- [ ] ARIMA with automatic model selection (Hyndman-Khandakar)
- [ ] Seasonal ARIMA (SARIMA)

**Diagnostics & Decomposition**
- [ ] ACF and PACF
- [ ] Ljung-Box test
- [ ] Classical decomposition (additive / multiplicative)
- [ ] STL decomposition

**Infrastructure**
- [ ] DataFrame interop (`fromDataFrame`, `toDataFrame`)
- [ ] Information criteria (AIC, AICc, BIC)
- [ ] Time series cross-validation / rolling origin

---

### Roadmap

Sibyl is being developed as a new library for the dataHaskell ecosystem. The goal by the end of the Q3 2026 is a complete automatic ARIMA pipeline, with exponential smoothing, classical decomposition, and diagnostics along the way.

The implementation follows the same mathematical foundations as R's `forecast` package, with Hyndman-Khandakar automatic model selection as the primary milestone. Natural next steps include SARIMA, ETS state-space models, and hierarchical forecasting.

Progress is tracked on [GitHub Projects](https://github.com/jackiedorland/sibyl/projects).

---

### Building

Sibyl targets GHC 9.8 via Stackage LTS 24.33.

```bash
stack build
stack test
```

---

### License

BSD-3-Clause. See [LICENSE](LICENSE).