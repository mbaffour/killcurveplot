# Growth-model fits, phenotype matrix, all-metrics statistics and extraction (v2.18 engine)

These are pure functions in `Lysis_Curve_App_v28.R` (block "v2.18 engine"), tested by
`tests/test_v218_engine.R`. They do not change any existing metric; the curve metrics in
`METRICS_REFERENCE.md` are still the rolling-window engine, and the fits below are an
additional, model-based view of the growth phase.

## 1. Growth-model fits (`fit_growth_models`)

Three classic sigmoidal models are fitted by non-linear least squares (`nls`) to one curve.

**What is fitted.** With `log_transform = TRUE` (default) the response is the change from the
first reading on the natural-log scale,

    y(t) = ln( od(t) / od(t0) ),      tt = t - t0

(with `log_transform = FALSE`, `y = od(t) - od(t0)`; then `A` is in OD units and `mu_max` is a
slope in OD per time unit rather than a specific rate). The model function is `f(tt) - f(0)`,
so a curve that equals `c * exp(f(tt))` is recovered exactly whatever its starting OD, and
`od(t) = od(t0) * exp(f(tt) - f(0))`. Time is measured from the first fitted point, so `lag`
is a duration since that point.

**Parameters** (reported in every row): `A` asymptote of `f` (ln-OD units: the total
log-increase), `mu_max` maximum specific growth rate (per time unit, on ln OD), `lag`
(lambda, time units). `y0` is the baseline OD (the first fitted reading) used to
back-transform.

**Modified logistic** (Zwietering et al. 1990):

    f(tt) = A / ( 1 + exp( 4 mu_max / A * (lag - tt) + 2 ) )

**Modified Gompertz** (Zwietering et al. 1990):

    f(tt) = A * exp( -exp( mu_max * e / A * (lag - tt) + 1 ) )

**Baranyi-Roberts** (Baranyi and Roberts 1994), with the lag-adjustment function and
`h0 = mu_max * lag`, initial value 0 and `ymax = A`:

    A(tt) = tt + (1 / mu_max) * ln( exp(-mu_max tt) + exp(-h0) - exp(-mu_max tt - h0) )
    f(tt) = mu_max * A(tt) - ln( 1 + ( exp(mu_max * A(tt)) - 1 ) / exp(A) )

`mu_max` is the model parameter. For the logistic and Gompertz it equals the steepest slope
of the fitted curve; for Baranyi it is the exponential-phase rate, which can be well above
the steepest slope the data actually show when the curve bends early, so the three models do
not give the same `mu_max` on the same data. Compare models on AIC, not on
`mu_max`, and read the rolling-window `mu_max` of the metrics table as the model-free
reference.

**Starting values.** `mu_max`: the largest centered rolling slope of y (`rolling_slope`,
window 5, the same regression as the engine's `mu_max`); `lag`: the engine's rule (first
time that slope reaches 10 % of it) and, as a second start, the tangent intercept; `A`:
`max(y)` and 1.5 x `max(y)`. `nls` is run with `algorithm = "port"` (bounds: 0.001-5 x
`max(y)` for `A`, 0.001-20 x the starting rate for `mu_max`, 0 to the time span for `lag`)
and falls back to the default Gauss-Newton algorithm when `port` fails; the lowest-RSS
converged start wins. The function never raises an error: a model that fails returns
`converged = FALSE`, `NA` parameters and the reason in `message`. A converged fit can still
carry a `message` (for example "only 5 points: parameters are poorly determined", or
"asymptote at its upper bound (no plateau reached in the fitted points)", which means the
fitted points are still on the exponential phase and `A` is not identified).

**Statistics per row.** `rss`, `aic`, `bic`, `r2` (on the fitted scale, y), and
`se_A / se_mu_max / se_lag` (asymptotic standard errors from `nls`, `NA` when
`summary()` cannot compute them). AIC and BIC use the Gaussian likelihood with k = 4
(three parameters plus the residual variance):
`AIC = n (ln 2 pi + 1 + ln(RSS / n)) + 2k`, `BIC = same + k ln n`.
Extra columns `t0`, `log_transform` and `until` are kept so `predict_growth_model()` can
rebuild the curve from a row.

**Why fit "up to the peak" (`until = "peak"`, default).** A lysis curve is growth followed
by a collapse. The logistic, Gompertz and Baranyi models describe growth to a plateau; fitted
through the collapse they would compromise between the rise and the fall, giving a low
asymptote, a biased `mu_max` and a poor R2 for a curve whose growth phase was fine. With
`until = "peak"` only the points up to and including the maximum of `od` are used, which is
the growth the host achieved before lysis. The cost is that a phage curve that peaks early
leaves few points (the model needs at least 4; fewer than 6 are flagged as poorly
determined). `until = "all"` fits every point, appropriate for curves without lysis. For a
curve that regrows past its first peak (resistance), the global maximum is at the end, so
`"peak"` includes the lysis; trim the curve with the time windows below first.

**Choosing a model (`best_model`).** For each sample (and replicate, when the table has a
`replicate` column) the converged model with the lowest AIC is returned. The models have the
same number of parameters, so this is the lowest RSS; a difference of about 2 AIC units or
less is not meaningful, and on 4-6 points the choice is weak. A group in which nothing
converged keeps one row with `model = NA`.

`predict_growth_model(fit_row, t, od0)` returns the fitted OD at times `t` from baseline
`od0` (use the row's `y0` to draw the fit over the data); `NA` for a model that did not
converge. `fit_growth_models_by_sample(pd)` takes the mean-curve frame
(`time, variable, mean_value`) and `fit_growth_models_by_replicate(rep_pd)` the replicate
frame (`time, variable, replicate, value`); both prepend `sample` (and `replicate`).

## 2. Phenotype matrix (`phenotype_matrix`)

A samples x metrics matrix for a heatmap. With `metrics = NULL`, every numeric column of the
metrics table except `sample`, `n_timepoints`, `t_start`, `t_end`, `replicate` (and
`n_replicates`). A replicate-level table is averaged per sample. `scale = "z"`: each column
is centred on its mean and divided by its SD across samples; `"range"`: min to 0, max to 1;
`"none"`: unchanged. A metric that is all `NA`, or has the same value in every sample (z and
range are undefined), is not returned and is listed in `dropped` (an `NA` for one sample
stays `NA`). Columns are labelled with `metric_label()`; `raw` is the same matrix unscaled.
The z-score is across the samples you chose, so adding or removing a sample changes every
cell of that column.

## 3. All-metrics statistics (`compare_all_metrics`)

`compare_replicate_metric()` (Welch t or Mann-Whitney, Holm within the metric, Hedges' g) run
for every metric and stacked into one long table. `p_adj_holm` and `stars` are exactly the
per-metric values of the Analysis tab. `p_adj_bh_across` is the Benjamini-Hochberg adjustment
over **all** tests in the table, every metric x comparison, to control the false-discovery
rate when scanning dozens of metrics.

**Exploratory.** The metrics are strongly correlated (many are functions of the same peak or
slope), BH assumes independent or positively dependent tests, and with three replicates
per group the tests have little power. Treat `p_adj_bh_across` as a way to rank and
screen metrics for follow-up, not as a confirmatory result; report a pre-specified metric
with its per-metric Holm p.

## 4. Extraction

All functions use the replicate frame (`time, variable, replicate, value`); non-finite values
are ignored.

* `od_at_times(rep_pd, times, method)`: the value of each replicate curve at the requested
  times. `"interpolate"`: linear interpolation between the bracketing points; `"nearest"`:
  the reading at the closest sampled time. A time outside a curve's own range (before its
  first or after its last point) is `NA`, never extrapolated.
* `summarise_od_at_times(x)`: per `time_requested` and `sample`: `mean`, `sd`, `n`
  (replicates with a value), `sem = sd / sqrt(n)`, `ci95 = t(0.975, n - 1) x sem` (half-width).
  `sd`, `sem` and `ci95` are `NA` with fewer than 2 values.
* `time_to_od(rep_pd, threshold, direction, after)`: per sample and replicate, the first
  time the curve crosses `threshold`. `"up"`: from below to at-or-above; `"down"`: from
  at-or-above to below. The crossing time is linearly interpolated between the two
  bracketing points. A curve that starts on the far side has no crossing until it comes
  back, so it is not given its first time. `after = "peak"` searches only from the curve's
  maximum onward (the post-peak decline, the usual "time to fall to X OD"); a number
  starts the search at the first point at or after that time. `NA` when there is no
  crossing. Results have `sample`, `replicate`, `time`, `threshold`, `direction`.
* `summarise_time_to_od(x)`: per sample: `mean`, `sd`, `n` (replicates that crossed), `sem`,
  `ci95`, and `n_curves` (replicates examined).
* `apply_sample_time_windows(df, windows)`: `windows` is a named list `sample -> c(min, max)`
  (a `NULL` or `NA` bound is no limit); each named sample keeps only rows with
  `min <= time <= max`; samples without an entry are untouched. It works on the mean frame and
  the replicate frame (columns `variable` and `time`), so a trimmed curve can feed the fits and
  the metrics.

## References

* Zwietering MH, Jongenburger I, Rombouts FM, van 't Riet K (1990) Modeling of the bacterial
  growth curve. Appl Environ Microbiol 56:1875-1881.
* Baranyi J, Roberts TA (1994) A dynamic approach to predicting bacterial growth in food.
  Int J Food Microbiol 23:277-294.
* Benjamini Y, Hochberg Y (1995) Controlling the false discovery rate. J R Stat Soc B 57:289-300.
