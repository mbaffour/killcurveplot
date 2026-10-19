# Changelog

## v2.10.0 — 2026-10-18

Journal-standard figure export. Opt-in: a figure made with the default
settings is byte-identical to v2.9.0 (checked by the test suite).

### New
- **Journal Figure Preset** (sidebar). Pick a journal family and column
  (Nature 89 / 120 / 183 mm, Cell Press 85 / 114 / 174, ASM 84 / 175,
  PLOS 132, Science 55 / 120 / 183, half of a double column for two-panel
  figures, or a custom mm width), a height and an optional panel label, then
  **Apply**. It sets the export size in mm, Arial, 7 pt axis titles / 6 pt
  tick and legend text / 8 pt bold panel label, 0.75 pt data lines, 0.5 pt
  axes and error bars, 1.2 mm ticks, the Okabe-Ito colour-blind palette, the
  legend inside the panel, trimmed log tick labels (0.1 not 0.10), 7 pt
  theme spacing, SVG export and 600 dpi. Every value stays an ordinary
  control afterwards and is captured by Save Settings.
  Rationale and sources: `docs/FIGURE_STANDARDS.md`.
- **Figure Legend Text** box: a drafted legend paragraph (measure, curves,
  n, error statistic, MOI / host / phage / medium / temperature from the
  Experiment Notebook, log-axis and blank-correction notes) to paste into
  the manuscript. The preset turns the on-artwork error caption off and
  relies on this instead; the default figure still carries the caption
  (KC-6).
- **Figure Layout** controls (Axis Settings): legend position including
  the four inside corners, legend box border, legend key size (mm),
  axis / tick line width in pt, tick length in mm, panel label and size,
  theme spacing base size, Y tick-label style (as is / trim zeros / powers
  of ten). Defaults reproduce the previous hard-coded look.
- **Export size in mm or inches**, with a live "= W x H mm" note. Reports,
  PPTX and the preview all read the same size.
- **True export size preview**: the on-screen plot is rendered through the
  same ggsave pipeline as the download, at the physical export size, shown
  at 1-4x zoom, so fonts and line weights look exactly as Inkscape will
  open them. The pixel canvas mode is unchanged and still the default.
- Explicit **Arial** and **Helvetica** entries in the font list, and a
  warning when a chosen font is not installed (exports would substitute).
- Warning when the Colorblind / Publication palette (8 colours) is used
  with more than 8 samples (colours repeat) - the deferred item from
  `PUBLICATION_QUALITY_NOTES.md`.

### Fixed
- **PDF export now embeds fonts.** `grDevices::pdf()` never embedded them
  (journals require it) and crashed with "invalid font type" for Arial,
  Helvetica Neue, Garamond and Palatino. PDF now goes through
  `quartz(type = "pdf")` on macOS and `cairo_pdf` elsewhere.
- **SVG export is Inkscape-safe.** svglite's default `textLength`
  attribute made Inkscape stretch or squash a label as soon as it was
  edited; the app now writes SVG with `fix_text_size = FALSE`.
- PNG / TIFF / JPEG use `ragg` when installed (TIFF keeps LZW).
- The report provenance table records the export size with its units.

### Internal
- `APP_DEFAULTS` is now the single source of UI defaults (used by Reset
  and by the tests). `pt2lw()` / `lw2pt()` convert between pt and ggplot2
  linewidth units (1 unit = 2.134 pt). `export_device()` resolves the
  device for each format in one place.
- New suite `tests/test_v210_figure_export.R` (74 checks): every format x
  every font family through the real server, PDF font embedding, SVG
  physical size / no textLength / Arial / 6-8 pt / 0.25-1 pt strokes,
  legend text, layout inputs, Inkscape CLI round trip when Inkscape is
  installed, and the byte-identical default-figure check against v2.9.0.

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
