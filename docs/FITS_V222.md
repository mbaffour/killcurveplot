# Prism-grade fits (v2.22 fits engine)

Pure functions in `Lysis_Curve_App_v28.R` (block `v2.22 fits engine`, directly after the v2.18
engine), tested by `tests/test_v222_fits.R`. Everything is base R (`stats::nls`, `SSasymp`,
`SSfpl`, `confint`, `vcov`); no extra packages. They extend, and do not change, the growth-model
fits of `FITS_AND_EXTRACTION.md`.

**Conventions (same as the v2.18 engine).** Time is `t`, optical density `od`. A fit that cannot be
made (too few points, flat or all-NA data, no convergence) never raises an error: it returns a row of
`NA` values with the reason in `note`. Every `nls` call is wrapped in `tryCatch`. Parameter
intervals are 95 % profile-likelihood intervals (`confint`); when the profile fails or is not finite
the Wald interval `est +/- qt(1 - (1 - level) / 2, df) * se` is used instead (the `ci_method`
column and `fit_ci()$method` say which). Quantities derived from the parameters (half-life, `t50`,
fitted `od0` of the logistic, Hill slope) use the delta method (Wald) or the transformed limits of
the parameter they come from.

## Function index

| Function | Purpose |
| --- | --- |
| `fit_lysis_models(t, od, models, from, weights, level)` | post-peak (or whole-curve) one-phase decay and decreasing four-parameter logistic |
| `predict_lysis_fit(res, model, t)` | fitted OD of a `fit_lysis_models()` result |
| `fit_ci(fit, level, method)` | parameter intervals (profile with Wald fallback, or Wald) |
| `fit_quality(fit, n)` | R2, adjusted R2, RMSE, Sy.x, AIC, AICc, BIC, runs test, Shapiro-Wilk |
| `akaike_weights(aicc)` | Akaike weights |
| `fit_bands(fit, newdata_t, level, type, backtransform)` | delta-method confidence / prediction bands |
| `fit_growth_object(fit_row, t, od)` | rebuild the `nls` object of a v2.18 growth-model row (for bands, residuals) |
| `fit_exponential_window(t, od, window, min_points, auto, level)` | log-linear regression of the exponential phase |
| `compare_fits(df, model, param, by, until, log_transform, alpha)` | extra-sum-of-squares F test between samples |
| `fit_dose_response(moi, value, log_x, fix_bottom, fix_top, weights, level, n_curve)` | four-parameter logistic on log10 MOI, EC50 |
| `fit_weights(od, scheme, sd)` | weights for `nls(weights = )` |
| `fit_residual_frame(fit, t)` | residuals for a residual plot |
| `FIT_MODEL_LABELS_V222`, `fit_parameter_definitions()` | labels and the parameter glossary |

## 1. `fit_lysis_models`

Fits the part of a curve after the peak (`from = "peak"`, the default: the points from the maximum
of `od` to the end, `t_ref` = time of the maximum, `tp = t - t_ref`) or the whole curve
(`from = "start"`, `t_ref` = first time). Models (`models = c("one_phase_decay", "fpl_decay")`):

    one_phase_decay:  od = plateau + (od0 - plateau) * exp(-k * tp)
    fpl_decay:        od = top + (bottom - top) / (1 + exp((xmid - tp) / scal)),   scal > 0, top > bottom

`one_phase_decay` is Prism's "One phase decay": starting values come from `SSasymp`
(`Asym + (R0 - Asym) exp(-exp(lrc) tp)`, `k = exp(lrc)`) plus a grid of manual starts, the final fit
is in the (plateau, od0, k) parameterisation so that `k` has its own standard error and profile
interval. `fpl_decay` is the four-parameter logistic of `SSfpl` oriented as a decrease in time.

Output, one row per model: `model, converged, n, t_ref`, then for each of `k, half_life,
t50_lysis, plateau, od0, top, bottom, xmid, scal` the estimate with `_se`, `_lo`, `_hi`, then
`r2, adj_r2, rmse, sigma, aic, aicc, bic, df_resid, ci_method, note`. The `nls` objects are in
`attr(result, "fits")` (a list per model: `fit`, `t_ref`, `data`).

| Quantity | Definition |
| --- | --- |
| `k` | decay rate constant, per time unit (one-phase only) |
| `half_life` | `ln 2 / k` (CI: `ln 2 /` the k limits, reversed) |
| `t50_lysis` | **absolute** time at which the fitted OD is half-way between `od0` (the peak of the fitted curve) and the plateau. One-phase: `t_ref + half_life`. Logistic: `t_ref + xmid - scal * ln((bottom - h) / (h - top))`, `h = (od0 + bottom) / 2` |
| `plateau` | the level the OD decays to (`= bottom` for the logistic) |
| `od0` | fitted OD at `tp = 0` (the peak of the *fitted* curve). For the logistic it is derived: `top + (bottom - top) / (1 + exp(xmid / scal))` |
| `top` | logistic left asymptote (OD as `tp -> -infinity`); it can lie above the observed peak, so use `od0` for "the peak" |

Caveats. A one-phase decay assumes the curve levels off at a plateau; when it has not (the OD is
still falling at the end), `plateau` runs away to negative values and `k` becomes small: the note says
"fitted plateau is below 0". The logistic needs a genuinely sigmoidal drop; an exponential-like
decay is not identifiable with four parameters and returns `converged = FALSE` ("did not converge"):
use the one-phase model there. The minimum is 4 points for one-phase decay and 5 for the logistic, but
`aicc` needs n - k - 1 > 0 with k = parameters + 1, i.e. 6 and 7 points.

## 2. `fit_ci`

`fit_ci(fit, level = 0.95, method = "profile")` returns `data.frame(term, estimate, se, lo, hi,
method)`. `"profile"` calls `confint(fit)`; terms for which that fails come back as Wald, and the
`method` column says per row which was used. `"wald"` is `est +/- qt(1 - (1 - level) / 2, df) * se`
with `df = df.residual(fit)`, `se` from `vcov(fit)`. With no residual df (n = p) the `se`, `lo` and `hi`
are `NA`. A `NULL` fit gives one `NA` row. Profile intervals are asymmetric and are the better choice
for the few-point curves that are typical here; Wald intervals assume the likelihood is quadratic.

## 3. `fit_quality`, `akaike_weights`

`fit_quality(fit, n = NULL)` (one row): `n`, `k`, `r2`, `adj_r2`, `rmse`, `sigma`, `aic`, `aicc`, `bic`,
`df_resid`, `runs_test_p`, `shapiro_p`.

    r2      = 1 - RSS / SST                   (weighted sums when the fit has weights)
    adj_r2  = 1 - (1 - r2)(n - 1) / (n - p)   p = fitted parameters
    rmse    = sqrt(RSS / n)         sigma = Sy.x = sqrt(RSS / df)
    k       = p + 1                           (the residual variance counts as a parameter, as in AIC)
    aicc    = AIC + 2k(k + 1) / (n - k - 1)   NA when n - k - 1 <= 0
    AIC     = n (ln 2 pi + 1 + ln(RSS / n)) + 2k   (Gaussian, the same as stats::AIC)

`n` overrides the point count used in AICc. `runs_test_p` is the Wald-Wolfowitz runs test on the signs
of the residuals in data order, normal approximation (`mu = 2 n+ n- / n + 1`, `var = (mu - 1)(mu - 2) /
(n - 1)`, two-sided p, no continuity correction; zeros dropped; `NA` when all residuals have one sign).
It asks whether the curve is *systematically* off the data (Prism: "Runs test: is the scatter of
points around the curve random?"); a small p means the model is wrong in shape. With fewer than about
10 points it has almost no power and the normal approximation is crude. `shapiro_p` is the Shapiro-Wilk
test of the residuals (`NA` for n < 3 or n > 5000).

`akaike_weights(aicc)`: `w_i = exp(-delta_i / 2) / sum_j exp(-delta_j / 2)`, `delta = AICc - min(AICc)`;
`NA` entries get `NA` and are left out of the sum. Weights are the relative support for each model *in
the set compared*; do not read them as probabilities that a model is true. With n = 5-7 points per curve
use AICc, not AIC, and treat |delta| < 2 as a tie.

## 4. `fit_bands`

`fit_bands(fit, newdata_t, level = 0.95, type = "confidence", backtransform = NULL)` returns
`data.frame(t, fit, lo, hi)` for any `nls` fit. With `g(t)` the gradient of the curve with respect to the
parameters (central finite differences) and `V = vcov(fit)`:

    confidence band:  fit +/- qt(1 - (1 - level) / 2, df) * sqrt(g' V g)
    prediction band:  fit +/- qt(...) * sqrt(g' V g + sigma^2)       sigma^2 = RSS / df

A weighted fit's prediction band treats the new observation as having weight 1. `lo`/`hi` are `NA` when
`vcov` is unavailable (no residual df, singular fit).

**Scale.** The band is on the scale the model was fitted on. For `fit_lysis_models()` and
`fit_dose_response()` that is OD (or the response). For the v2.18 growth models it is
y = ln(od / od[1]) against tt = t - t[1] (`fit_growth_models` returns rows, not objects, so
`fit_growth_object(row, t, od)` rebuilds the `nls` object, preparing the data exactly as the engine did).
`backtransform = exp` (or `TRUE`) returns OD units: `OD0 * exp(.)` for a ln fit (`OD0 + .` for a fit on
OD - OD0), identical to `predict_growth_model(row, t, row$y0)` for the curve; the limits are transformed
the same way. The `newdata_t` argument is always the absolute time axis of the data (the offsets `t0`
and `t_ref` are handled through attributes on the fit).

## 5. `fit_exponential_window`

`lm(log(od) ~ t)` on a window: `t_start, t_end, n, mu, mu_se, mu_lo, mu_hi, doubling_time,
doubling_time_lo, doubling_time_hi, r2, intercept, note`. `mu` is the slope of ln od (per time unit);
`doubling_time = ln 2 / mu` with `doubling_time_lo = ln 2 / mu_hi` and `doubling_time_hi = ln 2 / mu_lo`
(`NA` when the slope interval reaches 0). The slope interval is `mu +/- qt(0.975, n - 2) se`.

* `window = c(from, to)` fits exactly that window (`NA` = open end; at least 3 points).
* `window = NULL, auto = TRUE` (default) tries every contiguous window of at least `min_points`
  (default 4) points and keeps the one with the highest R2 among those with a positive slope; ties
  (within 1e-10) go to the longer window, then the earlier one. Windows are evaluated with prefix sums
  so this is cheap even for long curves.
* `auto = FALSE` uses all points.

Caveat: R2 favours short windows (any 4 nearly collinear points have R2 close to 1), so the "best" window
is often the minimal one. Raise `min_points` (6-8 for a dense plate-reader curve), or choose the window
by eye from the log plot and pass it as `window`. The window is data-driven, so its CI is optimistic: the
selection itself is not accounted for.

## 6. `compare_fits` (Prism "Compare: do the best-fit values differ between data sets?")

`compare_fits(df, model = "logistic", param = "all", by = "sample")`; `df` has `t`, `od` and the grouping
column (`sample`), replicate points stacked. Two models are fitted by `nls` to *all* the points at once:

* **null** ("one curve for all data sets"): one set of `A`, `mu`, `lag` shared by every sample;
* **alternative** ("separate curves"): `param = "all"` makes all three sample-specific; `param = "mu"`
  (`"lag"`, `"A"`) makes only that parameter sample-specific and shares the other two, which is Prism's
  "compare only selected parameters" option.

The growth models are the v2.18 ones (`logistic`, `gompertz`, `baranyi`; Zwietering 1990, Baranyi and Roberts
1994) on the ln scale: `y = ln(od) = y0_s + f(tt) - f(0)`, `tt = t - t0_s`. The sample baselines `y0_s`
(ln OD0) are fitted for every sample in *both* models, because the first reading is noisy and fixing it
would let baseline noise alone reject the null. Points up to the peak of each sample's mean curve are used
(`until = "peak"`, as in `fit_growth_models`).

    F = ((SS_null - SS_alt) / (df_null - df_alt)) / (SS_alt / df_alt)      p = P(F(df_null - df_alt, df_alt) > F)
    df = N - (number of fitted parameters, baselines included)

`test` also has `delta_aicc = AICc(alt) - AICc(null)` (negative favours separate curves), `preferred`
("alternative" when p < `alpha`, else "null", Prism's rule) and `preferred_aicc`. `params` is the alternative
fit per sample (`od0`, `A`, `mu`, `lag` with standard errors, `n_points`, `shared`); a shared parameter has the
same value in every row. `null_params` and the `nls` objects (`fits`) are returned too. A failure
(one sample, fewer than 4 points in a sample, no convergence, no residual df) returns `NA` in `test` and a
`note`. The start values come from `fit_growth_models` on each sample's mean curve, with a second start
expanded from the null solution; if the alternative lands above the null SS, the note says so.

Caveats for n = 5-7 points per curve. With 3 replicates each of two samples and about 6 time points each
the alternative model has 12 parameters for 36 points; with single curves it has 6-8 parameters for 10-14
points, leaving very few residual degrees of freedom, and the F test then has little power and relies on
the usual `nls` approximations, which are poor for the lag parameter (flat likelihood when no clear lag
exists). Prefer `param = "mu"` (one extra parameter) to `"all"` when the question is a growth-rate
difference, report `df_alt`, and treat p near the threshold with caution. The F test assumes independent
Gaussian errors of equal variance on the ln scale; replicate points of one well that are correlated over time
(drift) inflate the evidence. Several `param` comparisons on the same data are multiple tests: correct for
them.

## 7. `fit_dose_response`

Four-parameter logistic of the response against `log10(moi)` (`log_x = TRUE`; rows with `moi <= 0` are
dropped and noted) or against `moi`:

    y = A + (B - A) / (1 + exp((xmid - x) / scal)),    scal > 0
    EC50 = 10^xmid      hill = sign(B - A) / (scal ln 10)

`A` is the asymptote at low x and `B` at high x, so a response that *falls* with MOI has `A > B` and a
negative Hill slope, exactly Prism's "log(agonist) vs response -- variable slope" (HillSlope on log10 x).
`top` is reported as the larger asymptote and `bottom` as the smaller. `fix_bottom` / `fix_top` hold a
plateau constant (the direction of the curve is taken from the data, so `fix_bottom = 0` always fixes the
lower plateau): the fit then has fewer parameters and, with the plateaus constrained, a much better
determined EC50 from few points. `weights` are passed to `nls`. With `log_x = FALSE` the EC50 is `xmid`
itself and `hill = sign / scal` (a natural-log slope).

Result row: `ec50, ec50_lo, ec50_hi` (`10^` the xmid limits), `hill` (with `_lo`, `_hi`), `top`, `bottom`
(each with `_se`, `_lo`, `_hi`; `NA` for a fixed plateau), `xmid` (with `_se`, `_lo`, `_hi`), `scal`,
`scal_se`, `r2`, `adj_r2`, `aicc`, `df_resid`, `n`, `converged`, `ci_method`, `note`;
`attr(result, "curve")` = `data.frame(moi, fit, lo, hi)` over the MOI range (log-spaced for `log_x`; the
`lo`/`hi` are the delta-method confidence band) and `attr(result, "nls")` the fit. Fewer points than free
parameters (4 with free plateaus), flat data, or no convergence return `converged = FALSE`; an EC50 outside the
tested MOI range is flagged in `note` (extrapolation).

Caveats. With 4 parameters and 5-7 concentrations the intervals are very wide or unavailable (`df_resid` 1-3;
the note says so); fix the plateaus where the experiment defines them (0 and 100 %) and replicate the MOIs.
Profile limits can be asymmetric on the MOI scale because they are exponentiated.

## 8. Weights and residuals

`fit_weights(od, scheme)`: `"none"` (1), `"1/y"`, `"1/y2"` (relative weighting, Prism's "1/Y^2"; stabilises
variance that grows with OD) and `"1/sd2"` (inverse variance from a supplied `sd`, e.g. the SD of the
replicates). An undefined weight (od <= 0, sd = 0 or `NA`) is `NA`; `fit_lysis_models` and
`fit_dose_response` drop such points and say so. Weighted fits minimise `sum(w * r^2)` and `r2`, `sigma`, AIC
use the weighted sums, so AICc values are comparable only between fits with the *same* weights (and on the
same points).

`fit_residual_frame(fit, t)`: `t, fitted, residual` (observed - fitted, original units), `std_residual =
sqrt(w) * residual / sigma` with `sigma = sqrt(RSS / df)`. Plot it against `t`; a pattern (runs of one
sign) is what `runs_test_p` tests.

## 9. Labels and definitions

`FIT_MODEL_LABELS_V222` (named character vector of display names for the new models) and
`fit_parameter_definitions()` (`data.frame(parameter, definition, unit)` for every parameter above) feed the
UI tables and footnotes.

## 10. Prism equivalents

| Prism | Here |
| --- | --- |
| Nonlinear regression, "One phase decay" | `fit_lysis_models(models = "one_phase_decay")` |
| Four-parameter logistic ("log(inhibitor) vs response -- variable slope") | `fit_dose_response()`; `fit_lysis_models("fpl_decay")` in time |
| "Compare: do the best-fit values of selected parameters differ between data sets?" (extra sum-of-squares F test) | `compare_fits()` |
| "Compare: which model is correct?" (AICc, Akaike weights) | `fit_quality()$aicc`, `akaike_weights()` |
| Confidence / prediction bands | `fit_bands()` |
| "Runs test", "Normality of residuals", Sy.x, adjusted R2 | `fit_quality()` |
| Weighting by 1/Y, 1/Y^2, 1/SD^2 | `fit_weights()` |
| Residual plot | `fit_residual_frame()` |
| Asymmetrical (profile) confidence intervals | `fit_ci(method = "profile")` |

Differences from Prism: the runs test uses the normal approximation (Prism uses exact tables for small
n); the profile interval is R's `confint.nls`; AICc counts the residual variance as a parameter (Prism
does the same); the F test for `compare_fits` fits a free baseline per sample (nuisance parameter, see
section 6) and uses the ln scale of the v2.18 growth models.
