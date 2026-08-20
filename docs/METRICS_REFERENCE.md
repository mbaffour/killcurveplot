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

## Curve metrics (per sample)

| Metric | Definition |
|---|---|
| `n_timepoints`, `t_start`, `t_end` | Size and span of the (filtered) curve |
| `initial_od`, `final_od` | First / last mean value |
| `max_od`, `time_max_od` | Peak and its time |
| `min_od`, `time_min_od` | Global minimum and its time |
| `auc` | Trapezoidal area under the mean curve |
| `mu_max` | Largest centered rolling-window regression slope of ln(mean) vs time (window = smoothing setting, default 5 points; slopes must exceed 1e-10 so flat curves report NA) |
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
Rscript tests/test_v28.R              # 52 integration checks (shiny::testServer)
Rscript tests/test_metrics_parity.R   # 29 parity checks vs lysis-curve-app
```

The parity suite needs a clone of `mbaffour/lysis-curve-app` as a sibling
directory (or `REF_APP=<path to its .R file>`); it skips politely otherwise.
