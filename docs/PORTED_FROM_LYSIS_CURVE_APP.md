# Ported from the Lysis Curve App (engine + ingest)

Reference: `Lysis Curve Ap 26.03.13.R` in `mbaffour/lysis-curve-app`
(findings in its `AUDIT-2026-09.md`). Output column names and the
`lysis_threshold` / `peak_mode` arguments are unchanged. Tests:
`tests/test_lc_port.R` (new), plus updated expectations in `test_v28.R`,
`test_v211_analysis.R` and `test_metrics_parity.R`.

## 1. Lysis-metric accuracy (LC-3, LC-9, LC-17)

Ported from the lysis block of `calculate_growth_metrics()`.

* `curve_noise_sd()` (new, top level) and `LYSIS_SUSTAIN_PTS <- 2L`: lysis is
  detected when the post-peak decline exceeds
  `max(lysis_threshold * max_od, 3 * curve_noise_sd(od))` for 2 consecutive
  points (LC-3).
* `lysis_time` is the linearly interpolated crossing of `lysis_threshold * max_od`
  below the peak (LC-9); the noise term is only in the *detection*, not in the
  interpolated level.
* `stat_phase_dur` is the longest contiguous run of points within 95 % of the
  peak (LC-17); new helper `longest_true_run()`.

Deliberate differences from the reference:

* Peak anchor: the reference anchors the lysis metrics on its first prominent
  peak (LC-2, `first_prominent_peak_idx()`); this app keeps `peak_mode`
  (`global` / `first_local`), so `max_od`/`time_max_od` and the lysis anchor are
  unchanged. LC-2 is not ported.
* `stat_phase_dur`: the longest run (as specified for this port), where the
  reference takes the run containing the peak. They coincide whenever the
  peak's own run is the longest.

Numbers that moved on purpose: the 4-point toy curve (0.1, 0.4, 0.2, 0.05 at
t = 0, 10, 20, 30) now has `lysis_time` 11 (was 20). On the demo data the
Uninfected control no longer gets a `lysis_time` on any replicate and the phage
curves' onsets are ~61.2 and ~106.3 (the true onsets are 60 and 105).

## 2. New metrics

New columns of `compute_one_curve_metrics()` (hence of `compute_curve_metrics()`,
`compute_replicate_metric_rows()` and the per-replicate mean +/- SD export):

* `first_peak_od`, `t_first_peak`: `first_prominent_peak_idx()` (ported verbatim)
  on a 3-point smoothed curve (`smooth3_raw_edges()`, the reference's
  `rollapply(..., fill = NA)` with raw values at the edges) with the plain
  `0.05 * range` prominence floor. Fallback when there is no prominent peak:
  the GLOBAL maximum, as in the reference, whatever `peak_mode` is.
* `extinction_time`: `curve_extinction_time()`, the reference's rule and floor
  (0.05 OD, interpolated; `od[1] < floor` reports the first time point). New
  argument `extinction_threshold = 0.05` on `compute_one_curve_metrics()`,
  `compute_curve_metrics()`; no UI control.
* `centroid_x`, `centroid_y`: `curve_centroid()`, the exact trapezoid-strip
  centroid.
* With a reference sample: `centroid_index` (`centroid_index_vs_ref()`, mean of
  the reference's `cx * cy` products) and `local_virulence`
  (`calculate_local_virulence()`, Storms 2020, default `t_stat` rule) in both
  `compute_curve_metrics()` and `compute_replicate_metric_rows()`.
* `ANALYSIS_METRIC_LABELS`, `METRIC_DEFINITIONS` and `metric_axis_label()` know
  the seven new metrics, so they are selectable in the metric summary / dose-response.

Known property of the reference rule, kept as specified: the demo inoculum is
0.05 OD, exactly the extinction floor, so with 4 % noise a curve's first point
falls below it about half the time and then reports `extinction_time` = the first
time point (e.g. the demo "Phage MOI 0.1" mean curve, 0 min). The demo
Uninfected mean curve is NA (first point 0.0506).
The 5 %-of-range floor of `first_peak_od` is noise-sensitive by design (the
reference's LC-18): on the demo "Phage MOI 10" mean curve the first prominent peak
is the t = 45 bump, not the t = 60 maximum.

`tests/test_metrics_parity.R` now also compares all of these with the reference
(`first_peak_od`, `t_first_peak`, `extinction_time`, `centroid_x/y`,
`centroid_index`, `local_virulence` and its `t_stat`).
