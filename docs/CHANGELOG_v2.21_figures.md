<!-- To merge into CHANGELOG.md under "## v2.21.0 — 2026-11-01" (the layout / sidebar / rename agent adds its own section under the same heading). -->

## v2.21.0 — 2026-11-01

### More figures

**Four new analysis figures, and the metric summary gains three more styles and significance brackets.**
The default main figure and the default metric summary are unchanged; every new input has an
`APP_DEFAULTS` entry (the last entries of the base list) and is saved with the settings. The new figures are
analysis types, so they are also composer panels and files of *Download all figures*.

- **Metric summary styles** (`metric_style`): "Dot plot (replicates, mean bar)" (default, the v2.11 look),
  "Bar (mean ± error, replicates as points)", "Box plot" and "Violin".
  - Bar: columns in the sample colour (alpha 0.85, width 0.6), whiskers = the main error measure (drawn dark
    when the whisker colour follows the sample, since a same-coloured whisker vanishes inside its bar),
    replicates as open circles on top, and the y axis starts at 0 (a facet with negative values gets its own
    room below the axis).
  - Box plot: one box per sample (outliers as open circles) with the replicate points. Violin: untrimmed,
    equal-width densities (`trim = FALSE`, `scale = "width"`) with the replicate points and a mean bar.
- **Significance brackets** (`metric_brackets`, off by default): a bracket with its stars for every pair of
  samples that reaches Holm-adjusted p < 0.05 in `compare_replicate_metric()` (all pairs: no reference), stacked above
  what the facet draws (points, whiskers, box whiskers, violin tails), the narrowest lowest, at most 6 per
  facet (the most significant), stars at the axis-text size. Plain `geom_segment` + `geom_text` (no `ggsignif`).
  The stars against the reference are off while brackets are on. A sample needs 2 or more replicate values;
  groups with n < 3 add the low-power warning to the figure's note, and a facet with more than 6 significant
  pairs says so. The note appears under the Analysis tab's controls, never on the artwork.
- **OD at chosen times** (`od_at_times`): grouped bars (or dots with `extract_style` = "Dots") of the mean
  OD at the times typed in the Statistics tab's *OD at chosen times* box (`extract_times`, `extract_method`
  are shared); the samples sit side by side, whiskers = the main error measure, replicate points optional
  (`extract_points`). With no times typed it shows the first, middle and last time points and says so in the
  note. A log y axis (the main figure's scale) draws the bars from a floor below the data, since `geom_col`
  anchors a log bar at OD 1. A time outside a curve's range is left out (never extrapolated) and counted in the note.
- **Time to an OD threshold** (`time_to_threshold`): per sample, the time its replicates cross
  `extract_threshold` in `extract_direction` (from the start or, with `extract_after`, from the peak), in the
  dot-plot look of the metric summary: replicates as open circles, a bar at the mean, whiskers = the main error
  measure; y label "Time to 0.15 (min)". Samples that never cross keep their (empty) slot and are listed in the
  note; a threshold nothing crosses is "Figure not available" with the reason.
- **Replicate small multiples** (`replicate_facets`): one panel per sample sharing the panel area (the
  fixed 65 mm is split between the panels), each showing its replicate curves thin in three shades of the
  sample colour (so they differ in greyscale too) and the mean bold in the sample colour; y axis shared,
  log when `y_scale_type` is log.
- **Fraction of replicates lysed** (`lysed_fraction`): a step curve from 0 to 1 per sample, the fraction of
  its replicates lysed at each time, with the first time the fraction reaches 0.5 marked by a point on a
  dashed 0.5 line. A replicate is lysed at a time when it is after the sample's peak (the highest per-time
  mean), its OD is below the threshold, and it had reached that threshold before, so the lag and growth phase and a
  threshold the curve never reached are not lysis. `lysed_mode` "relative" (default): `lysed_threshold` (0.5)
  × the sample's peak OD; "absolute": an OD. `lysed_fraction_curves()` and `lysed_fraction_t50()` are tested
  directly (on the demo Uninfected stays at 0 and Phage MOI 10 reaches 1 by 105 min).
- **Gallery**: the lysis panel list gains "Fraction of replicates lysed" and "OD at chosen times" after the
  dose-response, and the phenotype heatmap no longer prints its cell values in the composite
  (`pheno_values = FALSE` in `tools/render_gallery.R`; the cells are too narrow).
- Tests: `tests/test_v221_figures.R` (helpers, every style, brackets, the four figures, the fixed panel
  and the composer).
