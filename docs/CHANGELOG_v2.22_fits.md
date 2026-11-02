### v2.22 fits engine (draft; the supervisor folds this into CHANGELOG.md)

- New fitting engine in `Lysis_Curve_App_v28.R` (block `v2.22 fits engine`, after the v2.18 engine): pure
  functions, base R only, documented in `docs/FITS_V222.md` and tested by `tests/test_v222_fits.R`.
- `fit_lysis_models()`: post-peak (or whole-curve) one-phase decay (`SSasymp` starts) and decreasing
  four-parameter logistic (`SSfpl` starts); k, half-life, t50 of lysis, plateau, peak OD, top, bottom, xmid, scale,
  each with SE and 95 % profile interval (Wald fallback), R2, adjusted R2, RMSE, AIC, AICc, BIC, residual df.
- `fit_ci()` (profile with Wald fallback), `fit_quality()` (R2, adjusted R2, RMSE, Sy.x, AIC, AICc, BIC, runs test,
  Shapiro-Wilk), `akaike_weights()`.
- `fit_bands()`: delta-method confidence and prediction bands for any `nls` fit; `fit_growth_object()` rebuilds the
  `nls` object behind a v2.18 growth-model row so its bands can be drawn (ln scale, or OD with `backtransform = exp`).
- `fit_exponential_window()`: log-linear regression of the exponential phase, automatic window (highest R2 over
  contiguous windows of at least `min_points`) or a chosen window; mu and doubling time with confidence intervals.
- `compare_fits()`: Prism-style extra-sum-of-squares F test (one curve for all samples vs separate curves, or only mu /
  lag / A separate) for the logistic, Gompertz and Baranyi growth models, with delta AICc and a per-sample parameter table.
- `fit_dose_response()`: four-parameter logistic on log10(MOI), EC50 with interval, Hill slope, optional fixed plateaus,
  overlay curve with confidence band.
- `fit_weights()` (none, 1/y, 1/y2, 1/sd2) accepted by the lysis and dose-response fits; `fit_residual_frame()` for
  residual plots; `FIT_MODEL_LABELS_V222` and `fit_parameter_definitions()` for the UI.
- No change to existing metrics, fits, defaults or the UI; no new package dependencies.
