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
First peak before a > 10 % decline** (`peak_mode = "first_local"`), `max_idx`
is the first index `i` such that `peak_vec[i]` is the running maximum, a later
point falls below 90 % of `peak_vec[i]`, and the curve has not risen above
`peak_vec[i]` before that decline (without this last condition every point on
the way up would qualify whenever the post-lysis trough is below the
inoculum). If there is no such point the global maximum is used. Every
downstream metric (lysis, `od_drop`, residual, recovery, `t_half_max_decline`)
then uses that index unchanged, and `max_od` / `time_max_od` report it.
`peak_vec` is the raw or smoothed curve exactly as for the smoothing option.
The threshold is 10 %, not 5 %: replicate noise of about 4 % on a plateau
produces single-point dips of more than 5 %, which at 5 % made the Uninfected
replicates report a "lysis onset" late in the run. Combine the mode with a
3-point peak-detection smoothing window (the gallery does for the resistance
scenario).

## Curve metrics (per sample)

| Metric | Definition |
|---|---|
| `n_timepoints`, `t_start`, `t_end` | Size and span of the (filtered) curve |
| `initial_od`, `final_od` | First / last mean value |
| `max_od`, `time_max_od` | Peak and its time |
| `global_min_od`, `time_global_min_od` | The curve's global minimum and its time — usually the inoculum, near `t_start`. **Not** the post-lysis floor; see `residual_od` for that. |
| `auc` | Trapezoidal area under the mean curve |
| `mu_max` | Largest centered rolling-window regression slope of ln(mean) vs time (window = smoothing setting, default 5 points; slopes must exceed 1e-10 so flat curves report NA). Any window touching a non-positive OD reading is excluded rather than clamped — see `n_nonpositive_od` below. It is the maximum anywhere on the curve, so on a regrowth curve it can come from the regrowth phase rather than the first growth (the resistant demo curve shows μmax at about 180 min); the landmarks figure draws it where it was found. |
| `n_nonpositive_od` | Count of time points with OD ≤ 0 (e.g. a blank-subtraction artefact). Windows touching one of these contribute no `mu_max` slope; `mu_max`/`doubling_time`/`lag_phase` may be NA or based on fewer windows when this is > 0. |
| `doubling_time` | `ln(2)/mu_max`; NA without growth |
| `lag_phase` | First time the rolling ln-slope reaches 10% of `mu_max` |
| `stat_phase_dur` | Duration of the **longest contiguous run** of points ≥ 95% of the peak (see *Lysis detection rules* below); NA when the longest run is a single point |
| `lysis_time` | Lysis onset: the **linearly interpolated** time at which the mean falls the lysis threshold (default 5% of the peak, adjustable with **Lysis onset: decline from peak that counts as lysis (%)**, `lysis_threshold`) below the peak, called only when the decline is **noise-aware and sustained** (see *Lysis detection rules* below); NA if never. The threshold also gates `lysis_rate`, `od_drop`, `residual_od`, `lysis_efficiency` and `recovery_slope`, which are NA when no point declines past it |
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
| `first_peak_od`, `t_first_peak` | The **first prominent peak** (Blazanin et al., PNAS 2025): the earliest local maximum of the 3-point centered moving mean (raw value kept at the two edge points) whose topographic prominence — the drop to the shallower of its two saddles — is at least 5 % of that smoothed curve's range. `first_peak_od` is the raw OD at that point. With no prominent peak (or fewer than 3 points) both fall back to the **global** maximum, whatever the peak mode. Unlike `max_od` it is not displaced by regrowth that overtakes the first peak. The 5 %-of-range floor has no noise term, so on noisy curves a small early bump can qualify (the companion audit's LC-18, deliberately unchanged because the definition is published) |
| `extinction_time` | First time the mean falls **below the 0.05 OD floor**, linearly interpolated between the last point at or above the floor and the first below it. A curve that *starts* below the floor reports its first time point; NA if it never falls below. The floor is the companion app's default (argument `extinction_threshold`, not exposed in the UI): note that a blanked inoculum near 0.05 OD can sit just under it and so score an extinction at t = start, which is a property of the rule, not of the curve |
| `centroid_x`, `centroid_y` | Centroid of the area under the mean curve, exact: each consecutive pair of points is a trapezoid with a closed-form centroid and the curve centroid is their area-weighted mean. NA when the total area is not positive |

### Lysis detection rules (ported from the companion app, audit findings LC-3 / LC-9 / LC-17)

Three accuracy fixes, ported from `calculate_growth_metrics()` in
`lysis-curve-app` (`AUDIT-2026-09.md`). Output column names are unchanged.

* **Noise-aware, sustained onset (LC-3).** Lysis is *detected* only when the
  decline below the peak exceeds `max(lysis_threshold × peak, 3 × σ)` for
  `LYSIS_SUSTAIN_PTS` (= 2) consecutive points. σ = `curve_noise_sd(od)` is a
  robust point-to-point noise SD from the second differences,
  `median(|y_i − 2y_{i+1} + y_{i+2}|) × 1.4826 / √6` (the median keeps genuine
  curvature — the growth shoulder, the lysis kink — from inflating it; 0 with
  fewer than 5 points). A single point 5 % below a noisy peak is noise, not
  lysis: the old one-point 5 % rule scored a clean 4 %-CV uninfected control as
  lysing in 39 % of simulated runs, and the demo Uninfected replicates now
  correctly report NA. Consequences: a curve whose peak has fewer than two points
  after it can never report lysis, and `lysis_time`, `lysis_rate`, `od_drop`,
  `residual_od`, `lysis_efficiency`, `recovery_slope` are all NA together when
  lysis is not detected.
* **Interpolated `lysis_time` (LC-9).** Once lysis is detected, `lysis_time` is
  the time at which the curve first falls `lysis_threshold × peak` below the peak
  (the plain 5 % level, **not** the noise-inflated detection level, so the number
  does not move with the curve's noise), linearly interpolated between the last
  point above that level and the first below it. The old value was the first
  *sampled* point already past the threshold, late by up to one acquisition
  interval and not comparable between runs sampled at different intervals.
* **Contiguous stationary phase (LC-17).** `stat_phase_dur` is the duration of
  the **longest contiguous run** of points ≥ 95 % of the peak. The old definition
  spanned the first to the last near-peak point, running straight through a lysis
  trough between two separate near-peak episodes. Note: the companion app uses the
  run *containing the peak*; this app uses the *longest* run (identical whenever
  the peak's own run is the longest, which is the usual case).

The peak itself is still the global maximum (or the `first_local` mode's peak);
the companion app's first-prominent-peak anchor (LC-2) is *not* ported.

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
| `centroid_index` | `1 − (centroid_x · centroid_y) / (centroid_x · centroid_y of the reference)` (Hosseini et al. 2024). With replicates the denominator is the **mean of the reference replicates' `centroid_x · centroid_y` products** (not the product of the means), which makes the reference's own index average exactly 0. |
| `local_virulence` | Storms local virulence (PHAGE 2020): `1 − A_sample / A_reference`, with both areas integrated only up to `t_stat`, the **onset of stationary phase in the reference** — the first time after the reference's fastest growth at which its centered rolling ln-slope (same window and non-positive-OD rule as `mu_max`) falls below 10 % of `mu_max`; the last time point if it never does. The areas are trapezoidal and the integral stops at an interpolated point exactly at `t_stat`. The reference's own value is 0; unlike `infection_strength` it ignores everything after the reference stops growing. Per replicate, each replicate's area is compared with the reference's replicate-mean curve. |

Metrics that don't apply are `NA`, never fabricated. Two deliberate
corrections vs the original reference implementation are documented in the
source: per-sample ratios (the reference recycled the first sample's values
via a length-1 `ifelse()`, fixed upstream 2026-08-19) and the 1e-10 μmax
epsilon.

## Growth-model fits, phenotype matrix, all-metrics statistics and extraction (v2.18 engine)

Beyond the rolling-window metrics above, the engine has pure functions for model-based growth fits (modified logistic, modified Gompertz and Baranyi-Roberts, fitted up to the peak so lysis curves are not fitted through the collapse, with the AIC-best model per sample), a samples x metrics phenotype matrix for heatmaps (z, range or no scaling), `compare_all_metrics()` (every metric's replicate comparison in one table, with an exploratory Benjamini-Hochberg adjustment across all tests next to the per-metric Holm p), and extraction helpers (OD at fixed times, time to cross an OD threshold, per-sample time windows). Their equations and definitions are in [`FITS_AND_EXTRACTION.md`](FITS_AND_EXTRACTION.md); the metrics in this file are unchanged by them.

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


**Series ending in a crash.** If the decline after the peak exceeds half of the peak, lysis onset is reported even when fewer than two post-peak points exist (a sample read only until it lysed). The sustained-run rule exists to reject noise, not an unambiguous collapse.
