# Sibyl Model Representation

**Author:** Jackie Dorland  
**Date:** March 2026  
**Status:** Draft for review

---

## Summary

This document describes the chosen design for how Sibyl represents statistical models. After evaluating four approaches ranging from full DataKinds with type-level model orders to a simple record-of-capabilities pattern, the chosen design uses a closed `ModelFamily` kind with associated type families and a data family for fitted models. Model orders (p, d, q, etc.) live at the value level instead of the type level.

The design prioritizes:

1. **Uniform interface**: `fit`, `predict`, `summarize`, `residuals`, and `fitted` all live in one typeclass, enabling generic combinators like cross-validation and model comparison.
2. **Clean autofit ergonomics**: since orders are value-level, `autofit` returns a concrete type with no existential wrappers.
3. **Two-layer API**: the safe layer (`Sibyl.Model`) returns `Either FitError`, and the unsafe facade (`Sibyl`) throws on failure for notebook convenience.
4. **Readability**: type variables are named `mdl` (model family) and `idx` (time index) instead of the conventional but ambiguous `f` and `t`.

The main tradeoff is a **closed kind**: new model families (e.g. Prophet) cannot be added without modifying Sibyl's source. This is accepted because Sibyl is in early development and the set of planned model families is known. If third-party extensibility becomes a priority later, migrating to open type tags is straightforward; otherwise, a new model family could simply be a PR or fork to Sibyl itself instead of an extending package. (This has the side effect of inducing people to contribute to Sibyl when they want to use our tooling for their models, which is good?)

##### Note: TimeSeries are represented internally by two unboxed vectors (TimeSeries idx Double), with invariants enforced using smart constructors `mkTimeSeries`

---
 
## Approaches Considered
 
Four approaches were evaluated, ranging from the most type-level machinery to the simplest possible implementation. The chosen design (Approach 2, with refinements) is a compromise between type safety, ergonomics, and contributor accessibility.
 
### Approach 1: Full DataKinds with type-level orders
 
Model orders are encoded as type-level naturals using `GHC.TypeLits`:
 
```haskell
data Model = ARIMA Nat Nat Nat | SARIMA Nat Nat Nat Nat Nat Nat Nat
 
data Fitted (m :: Model) = Fitted { fittedParams :: Params m, fittedState :: State m }
 
class TimeSeriesModel (m :: Model) where
  fit      :: Spec m -> Input m -> Either FitError (Fitted m)
  forecast :: Fitted m -> Int -> Either FitError (Output m)
```
 
A user would write `Fitted ('ARIMA 1 1 1)` and get compile-time guarantees that the orders are known.
 
**Pros:**
 
- Strongest compile-time safety. Orders are part of the type, so two models with different orders have provably different types.
- The type signature `Fitted ('ARIMA 1 1 1)` is self-documenting.
 
**Cons:**
 
- Autofit is painful. Hyndman-Khandakar picks orders at runtime, so the return type isn't known at compile time. This forces an existential wrapper (`SomeARIMA`) that hides the very information the types were meant to expose.
- Closed kind: `Model` is a closed data kind. Third-party packages cannot add new model families without modifying Sibyl.
- The orders (p, d, q) are almost always small non-negative integers. A one-line runtime check in `fit` gives equivalent safety in practice for much less complexity.
- GHC error messages involving promoted `Nat`-kinded constructors are notoriously hard to read.
 
**Rejected because:** autofit is the primary user workflow for notebooks, and the existential wrapper negates the main benefit of type-level orders... not a fan of it.
 
### Approach 2: DataKinds for model family, value-level orders (chosen, with refinements)
 
The model *family* (ARIMA vs ETS vs Naive) lives at the type level as a closed kind. The *orders* (p, d, q) are plain `Int` values inside the fitted model.
 
```haskell
data ModelFamily = ARIMA | SARIMA | Naive | ETS | ...
 
class Model (mdl :: ModelFamily) where
  type Settings   mdl   :: Type
  type Future     mdl   :: Type
  fit     :: Settings mdl -> TimeSeries idx Double -> Either FitError (Fitted mdl idx)
  predict :: Int -> Future mdl -> Fitted mdl idx -> Either FitError (Prediction idx)
 
data family Fitted (mdl :: ModelFamily) idx
```
 
**Pros:**
 
- `fit` and `predict` live in the typeclass, enabling generic combinators (cross-validation, model comparison, fit-and-predict pipelines).
- Autofit returns a concrete type (`Fitted 'ARIMA idx`) with no existential wrapper.
- The data family means `Fitted 'ARIMA idx` is the canonical type name; no aliases or indirection.
- Type signatures are readable: `Model mdl => Fitted mdl idx -> Summary idx`.
 
**Cons:**
 
- Adding a new model family requires editing Sibyl's source.
- Models with future regressors (SARIMAX) require `Future mdl` to be `()` for self-contained models, leading to `predict h () model` in the safe layer. The unsafe facade hides this.
- Contributors need to understand associated type families and data families to add a new model.
- GHC error messages for type family mismatches are less clear than plain typeclass errors.
 
**Chosen because:** it gives the uniform typeclass interface needed for generic combinators, autofit is clean, and the closed-kind tradeoff is acceptable for a library in early development.
 
### Approach 2b: Open type tags (variant of Approach 2)
 
Same structure as Approach 2, but replaces the closed kind with open empty types as tags:
 
```haskell
data ARIMATag   -- empty type, just a label
data NaiveTag
data ETSTag
 
class Model tag where
  type Settings   tag   :: Type
  type Fitted     tag idx :: Type
  type Future     tag   :: Type
  fit     :: Settings tag -> TimeSeries idx Double -> Either FitError (Fitted tag idx)
  predict :: Int -> Future tag -> Fitted tag idx -> Either FitError (Prediction idx)
```
 
Third-party packages define a new empty type and write a `Model` instance. No changes to Sibyl needed.
 
**Pros:**
 
- Fully open. Anyone can add a model family from an external package.
- Same generic combinator benefits as Approach 2.
 
**Cons:**
 
- Users see both `ARIMATag` (the tag) and `ARIMAFitted` (the data type) and have to understand the relationship. I think this is personally obtuse and ugly.
- The tag type carries no information; it exists purely as a typeclass index. This is weird Haskell.
- Error messages reference the tag type, which is meaningless on its own.
- Slightly more boilerplate per model than the closed-kind version.
 
**Rejected because:** the tag indirection adds complexity without enough benefit during early development. If third-party extensibility becomes critical, migrating from Approach 2 to 2b is straightforward (see "Future Migration" below).
 
### Approach 3: Simple typeclass, no DataKinds
 
Each model is its own regular Haskell type. A typeclass provides the shared interface for forecasting and diagnostics. `fit` is not in the typeclass because each model takes different arguments.
 
```haskell
class TSModel m where
  forecast  :: Int -> m -> Prediction
  summarize :: m -> Summary
  residuals :: m -> U.Vector Double
  fitted    :: m -> U.Vector Double
 
data ARIMA idx = ARIMA { ... }
instance TSModel (ARIMA idx) where ...
 
fitARIMA :: ARIMASettings -> TimeSeries idx Double -> Either FitError (ARIMA idx)
```
 
Generic combinators that need `fit` take the fit function as an argument:
 
```haskell
timeSeriesCV :: (config -> TimeSeries idx Double -> Either FitError model)
             -> (Int -> model -> Either FitError (Prediction idx))
             -> config -> TimeSeries idx Double -> Int -> Int
             -> [(Prediction idx, U.Vector Double)]
```
 
**Pros:**
 
- Simplest implementation. No DataKinds, no type families, no data families.
- Fully open. Anyone can define a type and write a `TSModel` instance.
- GHC error messages are straightforward.
- No extensions required in user code.
- Contributors only need to understand basic typeclasses.
 
**Cons:**
 
- `fit` cannot be generic. Each model has `fitARIMA`, `fitNaive`, `fitETS`, etc. Generic combinators that involve fitting must take the fit function as an explicit argument, which is more verbose.
- Cross-validation, model comparison, and fit-and-predict pipelines are per-model or require passing functions around.
 
**Rejected because:** generic combinators (especially cross-validation and model comparison) are important to Sibyl's design goals, and `fit` being outside the typeclass makes them significantly more verbose.
 
### Approach 4: Record of capabilities, no typeclass
 
No typeclass at all. A fitted model is a record that bundles everything a consumer needs:
 
```haskell
data FittedModel idx = FittedModel
  { fmPredict   :: Int -> Either FitError (Prediction idx)
  , fmSummarize :: Summary idx
  , fmResiduals :: U.Vector Double
  , fmFitted    :: U.Vector Double
  , fmName      :: String
  }
 
fitARIMA :: ARIMASettings -> TimeSeries idx Double -> Either FitError (FittedModel idx)
fitNaive :: NaiveSettings -> TimeSeries idx Double -> Either FitError (FittedModel idx)
```
 
**Pros:**
 
- Simplest possible design. No typeclasses, no extensions, no type families.
- Cross-family autofit is trivial: every model returns `FittedModel idx`, so the winner can be returned directly.
- Maximum openness: anyone writes a `fitMyModel :: ... -> FittedModel idx` and they're done.
 
**Cons:**
 
- Lose the ability to recover the original model type. Once it's a `FittedModel`, you can't get the ARIMA coefficients back without downcasting.
- No type-level distinction between models. A function can't require "specifically a fitted ARIMA."
- Can't write `Model mdl =>` constrained functions; everything takes `FittedModel idx` directly.
- Feels unprincipled for a Haskell library. Equivalent to manually passing a table
 
**Rejected because:** losing access to model-specific internals (coefficients, convergence status, log-likelihood) after fitting is unacceptable for a statistical library where inspecting the fitted model is a core workflow.
 
---

## The Closed Kind

```haskell
data ModelFamily
  = ARIMA
  | SARIMA
  | SARIMAX
  | Naive
  | ETS
  | Theta
  | TBATS
  | NNETAR
```

Each constructor is promoted to a type via `DataKinds`. In type signatures, promoted constructors appear with a tick: `'ARIMA`, `'Naive`, etc.

### Why closed?

An open kind (using empty tag types like `data ARIMATag`) would allow third-party packages to define new model families without modifying Sibyl. However, it introduces a layer of indirection: users see both `ARIMATag` (the tag) and some fitted type, and have to understand the relationship between them. For a library in early development with a known set of planned models, the closed kind is simpler. The migration path to open tags is well-understood and doesn't require changing user-facing type signatures (only internal plumbing).

### Why not type-level orders?

An earlier design encoded model orders at the type level:

```haskell
data Model = ARIMA Nat Nat Nat | ...
-- so you'd write: Fitted ('ARIMA 1 1 1) idx
```

This was rejected because Hyndman-Khandakar (automatic ARIMA model selection) chooses orders at runtime. Type-level orders would require an existential wrapper around the autofit return type, hiding the very information the types were meant to expose. Since most users will reach for `autofit` rather than manually specifying orders, the type-level encoding would be invisible in the primary workflow.

Orders are validated at the value level inside `fit`, following the same smart-constructor philosophy as `mkTimeSeries`.

---

## The Model Typeclass

```haskell
class Model (mdl :: ModelFamily) where
  type Settings mdl   :: Type
  type Future   mdl   :: Type

  fit       :: U.Unbox idx
            => Settings mdl
            -> TimeSeries idx Double
            -> Either FitError (Fitted mdl idx)

  predict   :: U.Unbox idx
            => Int
            -> Future mdl
            -> Fitted mdl idx
            -> Either FitError (Prediction idx)

  summarize :: U.Unbox idx
            => Fitted mdl idx
            -> Summary idx

  residuals :: Fitted mdl idx -> U.Vector Double
  fitted    :: Fitted mdl idx -> U.Vector Double
```

### Associated Types

| Type Family    | Role                                                                                                            | Example for `'ARIMA` |
| -------------- | --------------------------------------------------------------------------------------------------------------- | -------------------- |
| `Settings mdl` | What the user passes to `fit`. Contains orders, estimation method, CI level, etc.                               | `ARIMASettings`      |
| `Future mdl`   | What `predict` needs beyond the fitted model. `()` for self-contained models, `Regressors` for X variants.      | `()`                 |

### Why `fit` takes `TimeSeries idx Double` directly

Every model on the roadmap (ARIMA, SARIMA, ETS, Naive, Theta, TBATS, NNETAR) takes a univariate time series as training data. The X variants (ARIMAX, SARIMAX) additionally need regressors, which are bundled into their `Settings` type. This keeps `fit` uniform across all models without needing a type family for training input.

### The `Future` type and the `()` papercut

For models without exogenous regressors, `Future mdl` is `()`. This means the safe-layer call looks like:

```haskell
M.predict 12 () model
```

The `()` is hidden by the unsafe facade (see below). In the safe layer, it's mildly ugly but explicit about the fact that no future data is needed.

---

## The Data Family

```haskell
data family Fitted (mdl :: ModelFamily) idx
```

Each model defines a `data instance`:

```haskell
data instance Fitted 'ARIMA idx = FittedARIMA
  { arimaSettings   :: ARIMASettings
  , arimaCoeffs     :: U.Vector Double
  , arimaCoeffSEs   :: U.Vector Double
  , arimaResids     :: U.Vector Double
  , arimaFittedVals :: U.Vector Double
  , arimaSigma2     :: Double
  , arimaLogLik     :: Maybe Double
  , arimaConverged  :: Maybe Bool
  , arimaSeries     :: TimeSeries idx Double
  }
```

### Why a data family instead of a type family?

With a type family, you'd need two names for the same thing:

```haskell
data ARIMAFitted idx = ...              -- the real type
type instance Fitted 'ARIMA idx = ARIMAFitted idx  -- the alias
```

A data family means `Fitted 'ARIMA idx` IS the type. There is no `ARIMAFitted`. The constructor (`FittedARIMA`) is only needed for pattern matching. User-facing type signatures are always `Fitted 'ARIMA idx`, which is simple and readable. It's saying "This is a fitted ARIMA model with this time index type."

### Parameters are internal

Model-specific fields (coefficients, log-likelihood, convergence status) are accessed through the data instance's record fields directly. There is no `Parameters` type family. The rationale: `summarize` already provides a generic view of coefficients via `Summary`, and anything more specific requires knowing the model type anyway, so this can be up to the model.

---

## Shared Output Types

### Prediction

```haskell
data Prediction idx = Prediction
  { predPoint     :: TimeSeries idx Double
  , predLower     :: TimeSeries idx Double
  , predUpper     :: TimeSeries idx Double
  , predCILevel   :: Double
  , predResiduals :: U.Vector Double
  , predActuals   :: U.Vector Double
  } deriving (Show, Eq)
```

`Prediction` is a plain concrete type, not a type family. Every model produces the same shape of output. The CI level is carried on the `Prediction` so the output is self-describing (important after `autofit`, where the user didn't directly set the CI level).

This is one point I am unconfident about - I think possibly other things could go here, but I'd need someone with a good eye for stats to look at it.

### Summary

```haskell
data Summary idx = Summary
  { summaryName      :: String
  , summaryCoeffs    :: [(String, Double, Double)]
  , summaryCriteria  :: Maybe InformationCriteria
  , summaryLogLik    :: Maybe Double
  , summaryConverged :: Maybe Bool
  , summaryErrors    :: Maybe ErrorMeasures
  , summaryTraining  :: TrainingSummary idx
  } deriving (Show)
```

---

## The Unsafe Facade

The `Sibyl` module re-exports wrapped versions of the typeclass methods:

```haskell
-- for models without future regressors (ARIMA, Naive, ETS, ...):
predict :: (Model mdl, Future mdl ~ ())
        => Int -> Fitted mdl idx -> Prediction idx -- this is pretty, it says Time Horizon -> Fitted Model -> Prediction, which is intuitive and simple.
predict h mdl = unsafeFromEither "predict" (Model.predict h () mdl)

-- for models with future regressors (SARIMAX, ARIMAX, ...):
predictWith :: Model mdl
            => Int -> Future mdl -> Fitted mdl idx -> Prediction idx
predictWith h future mdl = unsafeFromEither "predict" (Model.predict h future mdl)
```

This follows the same pattern as `Sibyl.TimeSeries` wrapping `Sibyl.Safe.TimeSeries`: same function names, `Either` removed, errors thrown on failure.

---

## ARIMA Instance, for example

```haskell
data ARIMASettings = ARIMASettings
  { arimaP          :: Int
  , arimaD          :: Int
  , arimaQ          :: Int
  , arimaIC         :: IC
  , arimaMethod     :: EstimationMethod
  , arimaAllowMean  :: Bool
  , arimaAllowDrift :: Bool
  , arimaCILevel    :: Double
  , arimaLambda     :: Maybe Double
  } deriving (Show, Eq)

data instance Fitted 'ARIMA idx = FittedARIMA
  { arimaSettings   :: ARIMASettings
  , arimaCoeffs     :: U.Vector Double
  , arimaCoeffSEs   :: U.Vector Double
  , arimaResids     :: U.Vector Double
  , arimaFittedVals :: U.Vector Double
  , arimaSigma2     :: Double
  , arimaLogLik     :: Maybe Double
  , arimaConverged  :: Maybe Bool
  , arimaSeries     :: TimeSeries idx Double
  }

instance Model 'ARIMA where
  type Settings 'ARIMA = ARIMASettings
  type Future   'ARIMA = ()

  fit       = fitARIMA
  predict   = predictARIMA
  summarize = arimaSummary
  residuals = arimaResids
  fitted    = arimaFittedVals
```

---

## Autofit

```haskell
autofit :: U.Unbox idx
        => Bounds
        -> TimeSeries idx Double
        -> Either FitError (Fitted 'ARIMA idx)
```

`autofit` implements Hyndman-Khandakar: determines differencing order via unit root tests, searches over candidate (p, q) orders, fits each with CSS, compares AICc, and refits the winner with MLE. The return type is `Fitted 'ARIMA idx` with no existential wrapper, because orders are value-level fields inside `FittedARIMA`.

---

## Generic Combinators

The uniform typeclass interface enables model-agnostic functions:

```haskell
-- time series cross-validation (rolling origin)
timeSeriesCV :: (Model mdl, Future mdl ~ (), U.Unbox idx)
             => Settings mdl
             -> TimeSeries idx Double
             -> Int    -- forecast horizon
             -> Int    -- minimum training size
             -> [(Prediction idx, U.Vector Double)]

-- residual diagnostics
diagnose :: Model mdl => Fitted mdl idx -> DiagnosticReport

-- fit-and-predict in one step
fitAndPredict :: (Model mdl, Future mdl ~ (), U.Unbox idx)
              => Settings mdl
              -> TimeSeries idx Double
              -> Int
              -> Either FitError (Prediction idx)
fitAndPredict cfg series h = do
  mdl <- fit cfg series
  predict h () mdl

-- compare multiple configurations within a family
bestByIC :: (Model mdl, Future mdl ~ (), U.Unbox idx)
         => [Settings mdl]
         -> TimeSeries idx Double
         -> Either FitError (Fitted mdl idx)
```

Without the typeclass, each of these would need to be duplicated per model family, which kinda sucks.

---

## User Experience

### Notebook (unsafe facade)

```haskell
import Sibyl

main :: IO ()
main = do
  series <- fromDataFrame <$> D.readCsv "./sales.csv"

  -- Manual fit
  let model = fit (ARIMASettings 1 1 1 AICc CSSML True False 0.95 Nothing) series
  summarize $ predict 12 model

  -- Autofit
  let best = autofit defaultSearchBounds series
  summarize $ predict 12 best
```

### Production (safe layer)

```haskell
import qualified Sibyl.Model as M

main :: IO ()
main = do
  case M.fit settings series of
    Left err    -> logError (show err)
    Right model -> do
      case M.predict 12 () model of
        Left err         -> logError (show err)
        Right prediction -> writePrediction prediction
```

---

## Required GHC Extensions (User Code)

Users writing type signatures with `Fitted 'ARIMA idx` need:

```haskell
{-# LANGUAGE DataKinds #-}
```

Users writing generic `Model mdl =>` functions additionally need:

```haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
```

Users who only call `fit`, `predict`, and `summarize` on concrete models and let GHC infer the types may not need any extensions at all.

---

## Tradeoffs

### Closed kind

New model families require adding a constructor to `ModelFamily` in Sibyl's source. This is acceptable during early development. If third-party extensibility becomes a priority, the migration path is:

1. Replace `data ModelFamily = ...` with open empty-type tags (`data ARIMATag`).
2. Change `class Model (mdl :: ModelFamily)` to `class Model mdl`.
3. Each `data instance Fitted 'ARIMA idx` becomes `data instance Fitted ARIMATag idx`.
4. User-facing code changes are minimal (tick marks disappear, tag names may change).

### Deriving

Each `data instance` derives independently. You cannot write a blanket `deriving Show` for all `Fitted` instances; each one must derive (or manually implement) its own. This is minor but important nonetheless

### Cross-family autofit (future)

If Sibyl ever wants a single `autofitBest` that tries ARIMA, ETS, and Theta and returns the winner, it would need an existential wrapper:

```haskell
data SomeFitted idx where
  MkSomeFitted :: Model mdl => Fitted mdl idx -> SomeFitted idx
```

This kinda sucks. The primary use case (autofit within ARIMA) doesn't need it; I would prefer a different solution but couldn't come up with one.
