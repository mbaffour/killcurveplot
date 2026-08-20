# Changelog

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
