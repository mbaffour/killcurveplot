# Changelog

## v2.12.4 — 2026-10-19

Review round 4. Default main-figure output unchanged; new behaviour is opt-in
except the dashboard and report changes below.

- **Dashboard NA formatting**: "Key metrics per sample" printed "— ± 32" when
  the mean-curve value was NA but replicates had an SD. It now prints "—"
  alone; "value ± sd" only for a finite mean-curve value.
- **Adjustable lysis-onset threshold**: new Stats & Reports input "Lysis onset:
  decline from peak that counts as lysis (%)" (`metrics_lysis_threshold_pct`,
  default 5, 1 to 50), passed as `lysis_threshold = pct / 100` through
  `compute_one_curve_metrics()`, `compute_curve_metrics()`,
  `compute_replicate_metrics()` and `compute_replicate_metric_rows()`.
  Default output is unchanged. The report key-settings table has a
  "Lysis-onset threshold (%)" row; definitions, METRICS_REFERENCE and README
  updated.
- **Reports**: the HTML and PDF reports (with "Include analysis figures and
  statistics") always embed the Curve landmarks figure, and the MOI
  dose-response figure only when the selected samples have at least two
  distinct positive MOI values (previously any positive MOI, which embedded a
  "Figure not available" note for a single level). A dose-response figure that
  fails to build is skipped silently.
- Tests: dashboard "— ±" regression on the resistance demo, lysis-threshold
  default/6 % plateau dip, report image counts for the lysis and resistance
  scenarios, PDF page count.

## v2.12.3 — 2026-10-19

Review round 3. Default main-figure output unchanged; everything is opt-in.

- **Legend fallback**: `inside-auto` now falls back to a right legend above
  4 entries (was 5) and also when the emptiest corner box still holds the
  equivalent of 2 or more data points. Points are counted along the lines
  (8 interpolated points per segment, divided back to data-point equivalents),
  so a steep segment crossing the corner between two sparse points counts.
  Corner box: 9 % of the panel height per entry, 22 % + 1.7 % per label
  character of the width.
- **First-peak threshold**: peak mode `first_local` needs a decline of more
  than 10 % (was 5 %); 4 % plateau noise no longer counts as a peak followed
  by lysis. Label, definitions, README and METRICS_REFERENCE updated; a 3-point
  peak-detection smoothing window is recommended with it (the gallery sets it
  for `resistance`). A replicate whose own plateau maximum is followed by a
  > 5 % dip still reports the engine's lysis onset: peak mode chooses the
  peak, not the lysis definition.
- **Gallery alignment**: every panel is wrapped in `patchwork::free()`, so the
  45-degree labels of the metric summary no longer open a gap under the x axis
  of the panels beside it, and outside-legend panels no longer squeeze the
  panels beside them.
- **docs**: METRICS_REFERENCE notes that μmax is the maximum rolling slope
  anywhere on the curve (on a regrowth curve it can come from the regrowth).
- Tests: collision fallback, interpolation, 5-entry fallback, 6 % vs 15 %
  plateau dip, replicate peaks on the resistance demo with smoothing.

## v2.12.2 — 2026-10-19

Review round on the gallery figures. All changes are opt-in or limited to
analysis figures; the default main figure is byte-identical to v2.9.0.

- **Auto legend corner**: new legend position "Inside: emptiest corner
  (auto)" (`inside-auto`, used by the journal preset). The panel is split into
  quadrants at the x midpoint and the y midpoint (log10 when the y axis is
  log), and the key goes in the corner with the fewest plotted points (ties:
  top-left, top-right). The figures count points inside a legend-sized corner
  box (sized from the number of entries and the label length) rather than the whole
  quadrant, because a quadrant can look empty while a thin band of data along
  its edge (a lysis trough) is exactly where the legend lands; the quadrant
  rule is `auto_legend_corner(box = NULL)`. More than 5 legend entries fall back to a right
  legend. All inside placements now use a compact key
  (`legend.key.spacing.y` 1 pt, `legend.margin` 1/3/1/3 pt). Default stays
  "right".
- **Metric summary labels**: facet titles shortened ("OD drop", "Lysis
  efficiency", "Lysis rate", "Time to half peak", ...) and wrapped at 18
  characters; 5 or more samples use 45-degree x labels instead of staggered
  rows (3-4 names still stagger unless one is over 16 characters, which
  would overlap in a 44 mm facet, so those angle too); figure error messages
  are wrapped to fit the panel; rate metrics read "(OD per min)".
- **Dose-response axis**: MOI labels print as 0.01, 0.1, 1, 10, 100, not 1e-02.
- **Dose-response with one MOI level** shows "Dose-response needs at least two
  MOI levels; this selection has one." instead of a degenerate axis.
- **Dose-response series**: one connecting line per phage (the MOI token is
  removed from the sample label), in the colour of the series' first sample,
  with the phage name written at the end of each line when there is more than
  one series.
- **Peak mode** (Stats & Reports, "Peak used for lysis metrics"):
  `first_local` anchors the lysis metrics on the first peak followed by a
  > 5 % decline, so regrowth / resistance curves get a lysis onset, OD drop and
  half-peak time. The landmarks figure follows the same peak. Default
  "global" is unchanged. The rule also requires that the curve has not risen
  above the candidate before the decline; without it t = 0 counts as the
  peak on ordinary lysis curves (see docs/METRICS_REFERENCE.md).
- Tests: auto corner on synthetic clouds, inside-auto fallback, label and
  guide checks, one-MOI message, two-series dose-response, peak mode on a
  synthetic regrowth curve and the resistance demo.

## v2.12.1 — 2026-10-19

Five demo scenarios, so every figure has a meaningful example. The default
demo (and `make_demo_data()` with no argument) is unchanged.

- **Demo scenarios**: `make_demo_data(scenario)` and a picker next to
  **Load Demo Data**. `lysis` (the original 3 samples), `moi_series`
  (uninfected plus MOI 0.01 to 100, lysis onset 150 to 45 min, rate rising
  with MOI: feeds the dose-response), `resistance` (MOI 1 lyses at 90 min and
  stays down; "+ resistant" regrows from 150 min to about 60% of the
  uninfected plateau), `hours` (the lysis shapes on a 0 to 8 h axis, to
  exercise the time-unit logic) and `two_phages` (phages A and B at MOI 1 and
  10; B lyses later and less completely). Loading a scenario also sets the
  x-axis label ("Time (hours)" for `hours`, otherwise "Time (minutes)") and
  the reference sample to the uninfected/control sample.
- **Per-scenario gallery**: `tools/render_gallery.R` now renders all five
  scenarios into `<out_dir>/<scenario>/` (8 SVG + 8 PNG, `gallery.png`,
  `gallery.svg`, `legend_and_stats.txt` each) and still copies the `lysis`
  gallery to `docs/gallery/gallery.png` for the README.
- Tests: every scenario's shape, MOI parsing and the dose-response figure on
  the MOI series.

## v2.12.0 — 2026-10-19

Error display, sample order and a landmarks figure. Default main-figure
output is still byte-identical to v2.9.0 except where noted (log floor).

- **Error display** (Error Bars panel): individual replicate values as
  open circles behind the mean; error colour (sample / black / grey); cap
  width as % of the x range so caps look the same at any time scale; and
  on a log axis a bar whose lower bound crosses zero is now drawn down to
  the axis minimum instead of being dropped silently (switchable).
- **Analysis figures show the main figure's error measure** (SD / SEM /
  95% CI, with the multiplier): metric-summary whiskers, growth-rate band,
  dose-response whiskers. "None" draws no whiskers.
- **Sample order** control (Figure Layout): alphabetical (legacy) or as
  selected / as in the file, applied to the legend, every analysis panel
  and the facets, so a multi-panel figure reads in one order. The journal
  preset uses the file order (control first) and relative caps.
- **Curve landmarks** figure: lag end, μmax window centre, peak, lysis
  onset, half of peak and trough drawn on each mean curve, to audit the
  Curve Metrics by eye before reporting them.
- `tools/render_gallery.R` renders every figure from the demo data with the
  journal preset (one SVG + PNG each and a 183 mm two-column gallery) through
  the real app code; `docs/gallery/gallery.png` is its output.
- Figure Legend Text mentions the replicate points when shown.

## v2.11.1 — 2026-10-19

The HTML and PDF reports now double as a complete record of the v2.11
analysis.

- **Reports carry the analysis figures and replicate statistics.** After the
  main figure, both reports embed the metric summary, the specific growth
  rate, the normalized OD (or the replicate overlay when no reference sample
  is set) and, when any sample has an MOI, the dose-response figure, each
  rendered at the export size through the same PNG device as the main figure
  (one page each in the PDF), with a caption. A "Replicate statistics"
  section follows with the pairwise, omnibus and post-hoc tables from the
  Analysis figures tab, their warnings (excluded groups, n < 3, no usable
  reference, constant values), and a definitions paragraph (Welch t, Holm,
  Hedges' g, ANOVA + Tukey HSD, Mann-Whitney, Kruskal-Wallis). A figure that
  cannot be built is recorded as a one-line note rather than a placeholder
  image.
- **Include analysis figures and statistics** checkbox (Stats & Reports,
  on by default, in `APP_DEFAULTS`). Off, both reports are exactly what
  they were in v2.11.0.
- Internal: `build_html_report()` / `build_pdf_report()` take an optional
  `analysis` argument (NULL = v2.11.0 output); the report bodies are
  `html_report_string()` / `pdf_report_file()` in the server so the tests
  reach them. `tests/test_v211_analysis.R` gains 13 checks: headings, image
  count, figure choice with and without a reference, ANOVA + Tukey tables,
  PDF page count, and the off-path HTML being identical to the v2.11.0
  output for the same inputs.

## v2.11.0 — 2026-10-18 (in progress; code and tests complete, docs pending)

Analysis figures, replicate statistics and a dashboard. All new figures use
the main figure's theme (fonts, line weights, legend, journal preset) through
one shared `figure_theme_layers()`, so panels assemble without rescaling.
Default main-figure output remains byte-identical to v2.9.0.

- **Analysis figures** tab: metric summary (per-replicate points, mean ± SD,
  faceted by metric, significance marks vs the reference), specific growth
  rate μ(t) vs time (same rolling regression as μmax), normalized OD (vs the
  reference or t0), replicate overlay, MOI dose-response (MOI read from sample
  names or a `sample = MOI` box), OD heatmap. Own panel label; same export
  devices and size as the main figure.
- **Replicate statistics**: Welch t (or Mann-Whitney) per pair or vs the
  reference, Holm-adjusted p, Hedges' g, 95% CI; one-way ANOVA + Tukey (or
  Kruskal-Wallis) for ≥ 3 groups; explicit low-n warnings; CSV export.
- **Dashboard** tab: data cards, key metrics per sample (± SD across
  replicates), four small-multiple figures, statistics, notebook summary.
- Internal: `compute_replicate_metric_rows()` (per-replicate rows, with
  reference-normalised metrics), `compute_growth_rate_curves()`,
  `compute_normalized_curves()`, `compare_replicate_metric()`, `parse_moi()`.
  New suite `tests/test_v211_analysis.R` (63 checks).

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
