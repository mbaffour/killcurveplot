# Changelog

## v2.9.0 — 2026-09-14

Fixes from a read-only external audit (see [AUDIT-2026-09.md](AUDIT-2026-09.md)
for the full findings list, KC-1…KC-11, and what was fixed vs. deferred vs.
won't-fix).

> **Statistics/metrics from earlier versions are not directly comparable to
> this version** for any file where one of the following applied: a sample
> had exactly one replicate at some time point (SD/SEM/95% CI now export as
> `NA` there instead of a false `0` — KC-1); a curve had a non-positive OD
> reading (μmax/doubling_time/lag_phase now exclude the affected rolling
> window instead of clamping it, which had been inflating μmax — KC-2); or
> your Curve Metrics CSV read the `min_od`/`time_min_od` columns, now named
> `global_min_od`/`time_global_min_od` (KC-10). Re-export affected files to
> get corrected numbers.

### Fixes (P0/P1)
- **KC-1 (P0).** `n = 1` now exports SD/SEM/95% CI as `NA` (undefined),
  never a false `0`, in the plot, the Summary Statistics CSV, and both
  report formats. Corrected the README troubleshooting note that endorsed
  the old zero behaviour.
- **KC-2 (P1).** A non-positive OD reading (routine after blank
  subtraction) is now excluded from the μmax rolling-window regression
  instead of being clamped to `1e-9` before `log()`, which had inflated
  μmax by several-fold on an affected curve. The count of non-positive
  readings is reported as a new `n_nonpositive_od` column and a sidebar
  warning names the affected sample.

### Additive features (P2)
- **KC-3.** Optional peak-detection smoothing (a documented centered
  moving average), off by default, to stabilize the peak-anchored lysis
  metrics against replicate noise.
- **KC-4.** New "Per-Replicate Metrics, mean ± SD (CSV)" export: the same
  metrics engine run once per individual replicate curve, reported as
  mean ± SD across replicates — additive alongside the existing mean-curve
  Curve Metrics CSV, not a replacement for it (the two numbers are
  generally not equal; see docs/METRICS_REFERENCE.md).
- **KC-5.** `tests/test_metrics_parity.R` no longer silently exits 0 when
  its reference clone is missing or incompatible; it now fails by default
  (or skips visibly with `ALLOW_SKIP=1`), and its reference-free checks
  always run.
- **KC-6.** The error-bar caption (states SD/SEM/95% CI and n explicitly)
  now defaults to on, so a default SEM-bar figure is never unlabelled.
- **KC-7.** Optional "Subtract blank/media OD" field, off by default.

### Fixes (P3)
- **KC-8.** Saved settings JSON and the HTML report footer now stamp the
  real app version instead of a stale `"2.5"`.
- **KC-9.** Added `CITATION.cff` and a License/Citation section to
  README.md.
- **KC-10.** `min_od`/`time_min_od` renamed to `global_min_od`/
  `time_global_min_od` in the Curve Metrics export to disambiguate from
  the post-lysis floor (`residual_od`).
- **KC-11.** Decimal-comma / semicolon CSV exports (European locale) are
  now supported via a new "CSV format" selector (Auto-detect / US-UK /
  European).

### Tests
- `tests/test_v28.R` grew from 52 to 101 checks covering every fix above.

## v2.8 — 2026-08-19

### Accuracy fixes (verified by `tests/test_v28.R`)
- **CRITICAL — replicate columns pooled correctly.** Wide files with
  replicate columns sharing one name (the documented workflow) were
  silently collapsed to the *first* column of each name: means came from a
  single replicate and error bars vanished. Columns are now resolved by
  position, and every replicate contributes to mean/SD/SEM/CI.
- `n` counts only non-missing replicate values, so SEM/CI are not
  understated when readings are missing (wide and long formats).
- Settings save/load no longer drops `shape_size` and `color_palette`
  (per-sample prefix collision).
- `rgb_to_hex` no longer appends a spurious `FF` alpha channel.
- Format detection: a numeric flat/constant column can no longer flip a
  small wide file into long format (grouping candidates must be
  non-numeric).

### Metrics engine (verified by `tests/test_metrics_parity.R`)
- Growth/lysis metrics ported from the lysis-paper companion app
  (mbaffour/lysis-curve-app) and proven to reproduce its numbers exactly:
  μmax via centered rolling ln-regression, doubling time, lag phase,
  stationary duration, lysis onset/rate, OD drop, residual OD, recovery
  slope — 27 metrics per sample, plus 5 reference-normalized infection
  metrics. Two deliberate corrections vs the reference are documented in
  the source (per-sample ratios; flat-curve μmax epsilon).

### Features
- Data summary box (detected format, time column, replicate structure).
- Load Demo Data button (3 samples × 3 replicates).
- Summary-statistics CSV and curve-metrics CSV exports.
- Single-file **HTML report** and multi-page **PDF report**: figure,
  experiment notebook, statistics, metrics with definitions, provenance,
  session info. No pandoc/LaTeX.
- Experiment Notebook sidebar section feeding the reports.
- Legend font size control; report base font size control.
- TIFF export uses lossless LZW compression.

### Docs & tests
- `docs/METRICS_REFERENCE.md`, this changelog, and versioned test suites
  in `tests/`.

## v2.7 and earlier
See the header changelog inside `Lysis_Curve_App_v28.R`.
