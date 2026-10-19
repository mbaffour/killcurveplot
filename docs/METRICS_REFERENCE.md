# Metrics Reference — Lysis Curve Plotter (killcurveplot)

All metrics are computed from each sample's **mean curve as plotted**
(after time filtering / point exclusion). The growth/lysis engine is a
verified port of the analyzer used for the N4 lysis paper
([mbaffour/lysis-curve-app](https://github.com/mbaffour/lysis-curve-app));
`tests/test_metrics_parity.R` proves the numbers match it exactly.

Notation: `t_i` time points, `y_i` mean value, `n` points.

## Summary statistics (per sample × time)

mean, SD, `n` (**non-missing replicate values only**), `SEM = SD/√n`,
`CI95 half-width = t(0.975, n−1) × SEM`.

### Peak-detection smoothing (optional, OFF by default)

Every lysis-related metric below (`lysis_time`, `lysis_rate`, `od_drop`,
`residual_od`, `recovery_slope`, `lysis_efficiency`, `recovery_ratio`,
`t_half_max_decline`) is anchored on `max_idx`, the index of the detected
peak. At the app's own demo noise level (~4% multiplicative), which raw
point counts as "the peak" can move by a whole time point between
otherwise-identical noise draws, and every metric above moves with it. The
**Peak-detection smoothing window** control (sidebar, default 1 = off)
locates the peak on a centered moving average (`moving_average()`, window
in points) of the curve instead of the raw curve; every *reported* OD value
still comes from the raw data at that index — smoothing only decides WHERE
the peak is, never WHAT its value is. Off by default so existing exports
are bit-for-bit unchanged unless a user opts in.

### Peak mode (optional, "global" by default)

A curve that lyses and then regrows above its first peak (resistant mutants)
has its global maximum at the end, so `lysis_time`, `od_drop`,
`t_half_max_decline` and the other peak-anchored metrics are NA, and the
landmarks figure marks the wrong peak. With **Peak used for lysis metrics =
First peak before a > 5 % decline** (`peak_mode = "first_local"`), `max_idx`
is the first index `i` such that `peak_vec[i]` is the running maximum, a later
point falls below 95 % of `peak_vec[i]`, and the curve has not risen above
`peak_vec[i]` before that decline (without this last condition every point on
the way up would qualify whenever the post-lysis trough is below the
inoculum). If there is no such point the global maximum is used. Every
downstream metric (lysis, `od_drop`, residual, recovery, `t_half_max_decline`)
then uses that index unchanged, and `max_od` / `time_max_od` report it.
`peak_vec` is the raw or smoothed curve exactly as for the smoothing option.

## Curve metrics (per sample)

| Metric | Definition |
|---|---|
| `n_timepoints`, `t_start`, `t_end` | Size and span of the (filtered) curve |
| `initial_od`, `final_od` | First / last mean value |
| `max_od`, `time_max_od` | Peak and its time |
| `global_min_od`, `time_global_min_od` | The curve's global minimum and its time — usually the inoculum, near `t_start`. **Not** the post-lysis floor; see `residual_od` for that. |
| `auc` | Trapezoidal area under the mean curve |
| `mu_max` | Largest centered rolling-window regression slope of ln(mean) vs time (window = smoothing setting, default 5 points; slopes must exceed 1e-10 so flat curves report NA). Any window touching a non-positive OD reading is excluded rather than clamped — see `n_nonpositive_od` below. |
| `n_nonpositive_od` | Count of time points with OD ≤ 0 (e.g. a blank-subtraction artefact). Windows touching one of these contribute no `mu_max` slope; `mu_max`/`doubling_time`/`lag_phase` may be NA or based on fewer windows when this is > 0. |
| `doubling_time` | `ln(2)/mu_max`; NA without growth |
| `lag_phase` | First time the rolling ln-slope reaches 10% of `mu_max` |
| `stat_phase_dur` | Time spanned by points ≥ 95% of the peak |
| `lysis_time` | First post-peak time with decline > 5% of peak; NA if never |
| `lysis_rate` | Regression slope from peak to post-peak trough |
| `od_drop` | Peak − post-peak minimum |
| `residual_od` | Post-peak minimum |
| `recovery_slope` | Positive regression slope from trough to end; NA if none |
| `lysis_efficiency` | `od_drop / max_od` |
| `recovery_ratio` | `final_od / max_od` |
| `fold_change_final_vs_initial` | `final_od / initial_od` (NA if initial 0) |
| `pct_drop_from_max` | `(max_od − final_od)/max_od × 100` |
| `t_half_max_decline` | First time after the peak below 50% of the peak, linearly interpolated; NA if never |
| `steepest_decline_rate/interval` | Most negative consecutive-point slope, and where |

## Per-replicate metrics (optional, additive — "Per-Replicate Metrics, mean ± SD (CSV)")

Everything above is computed **once per sample, from the already-averaged mean
curve** — a single number with no replicate-to-replicate uncertainty. The
Per-Replicate Metrics export instead runs the exact same metrics engine
(`compute_one_curve_metrics()`) **once per individual replicate curve**, then
reports `<metric>_mean` and `<metric>_sd` across replicates per sample, plus
`n_replicates`.

**These are not the same number.** Most of these metrics are nonlinear
functions of the curve (`max_od` picks whichever point is the argmax;
`mu_max` picks the largest of many regression slopes; `lysis_time` is a
threshold crossing), so the mean of the per-replicate metrics is generally
**not** equal to the metric of the mean curve — e.g. two replicates that each
individually peak at OD 0.5, at different times, can average to a mean curve
whose peak is only 0.35. Both numbers are kept, separately labelled, rather
than one being presented as if it were the other. A metric that is `NA` for
some replicates (e.g. `mu_max` on a replicate that never grows) is excluded
from that replicate's contribution to the mean/SD, not treated as zero; the
SD is `NA` (not 0) when fewer than two replicates have a defined value for
that metric — the same reasoning as KC-1 for summary statistics.

Replicate identity: for wide files it is the replicate column's position
(matching the same `rv$col_map` used for the REPLICATE FIX); for long files
with no explicit replicate-ID column, it is the row's occurrence order within
each (sample, time) group, which assumes same-replicate rows are entered in a
consistent relative order across time points.

## Infection metrics (choose a reference sample)

| Metric | Definition |
|---|---|
| `relative_growth` | `auc / auc_ref` |
| `infection_strength` | `1 − auc/auc_ref` over the full run (≡ PhageScore/100 — **not** the Storms virulence index, which uses a bounded integration window) |
| `relative_mu_max`, `relative_max_od` | Ratios vs the reference |
| `lysis_onset_delta` | `lysis_time − lag_phase` |

Metrics that don't apply are `NA`, never fabricated. Two deliberate
corrections vs the original reference implementation are documented in the
source: per-sample ratios (the reference recycled the first sample's values
via a length-1 `ifelse()`, fixed upstream 2026-08-19) and the 1e-10 μmax
epsilon.

## Tests

```bash
Rscript tests/test_v28.R              # integration checks (shiny::testServer)
Rscript tests/test_metrics_parity.R   # parity checks vs lysis-curve-app
```

The parity suite's reference-comparison checks need a clone of
`mbaffour/lysis-curve-app` as a sibling directory (or `REF_APP=<path to its
.R file>`) whose `calculate_growth_metrics()`/`calculate_infection_metrics()`
still match the shape this suite's `grab()`-based extraction expects. All
other checks (rolling_slope vs `zoo`, infection-metrics self-consistency,
and the hand-computed spot checks) always run. If the reference is missing,
or present but its function signature has drifted, the suite now **fails**
by default (KC-5 — it used to silently exit 0) with a message naming the
cause; set `ALLOW_SKIP=1` to downgrade that to a visibly-printed skip
instead.
