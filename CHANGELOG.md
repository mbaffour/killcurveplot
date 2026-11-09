# Changelog

## v2.23.2 — 2026-11-09

**Copy figure to the clipboard.** Since v2.22.0 the Lysis curve preview is the export SVG drawn inline (so its labels
are clickable), and a browser offers no right-click "Copy image" on an inline SVG: the figure could no longer be copied
from the page.
- **Copy figure** button beside every figure's download button (Lysis curve, Analysis figures, Curve fitting, Compose,
  Compare) and **Copy key** by the legend key preview. The page draws the current preview to a PNG in the browser (the
  inline SVG through a canvas, the PNG previews redrawn) and writes it to the clipboard; a notification says it worked
  (with the size) or why it did not (no clipboard support, permission refused, no figure on the page).
- **Cmd/Ctrl+C** while the mouse is over a figure preview copies that figure (nothing happens while text is selected or a
  field has focus, so ordinary copying is untouched).
- The clipboard holds a screen-resolution PNG (at most 4000 px on the longer side); **Download Image** stays the path
  for print and journal files. No export path, the figure or the inlined SVG changed.
- The Lysis curve figure box shows a hint on hover pointing to Copy figure.

## v2.23.1 — 2026-11-08

**Legend key for every figure.** A one-click, colour-exact key to place beside a figure, and colour words in the legend
text, for the figure-preparation step: the legend you start writing from.
- **Legend key**: a separate image cut from the figure's own legend, so every plotted sample's colour, marker, fill, line
  type and display name are exactly as drawn, in the figure's font, white background, in the Export format (SVG, PDF,
  PNG, TIFF or JPEG). `legend_key_grob()` takes the legend of the figure (a hidden, inside or direct-label legend is
  rebuilt on the right) and `write_legend_key()` writes it, measured on an explicit `pdf(NULL)` device. A figure with no
  sample legend of its own (the metric summary, the time-to-threshold bars) gets the main figure's key: colours and names
  are by sample.
- **Where the buttons are**: *Lysis curve* tab, Figure Legend Text section, with a small preview under the legend text
  (`legend_key_main.<ext>`); *Analysis figures* tab under Download figure
  (`legend_key_<analysis type>.<ext>`); *Curve fitting* (`legend_key_fit`), *Compose* (`legend_key_compose`) and
  *Compare* (`legend_key_compare`) next to their figure downloads. With no data, or every point filtered away, a
  download is refused with a notification like the figure's own.
- The **figure bundle** adds `legend_key.svg` and `legend_key.pdf` (the main figure's key) and lists them in its README;
  **batch export** has a *Legend key* output (`legend_key_main.<format>`).
- **Legend text starter with colours**: "Curves: MG1655 (black, filled circles); MG1655 + WT (orange, filled squares);
  ..." from the colour, marker (only when points are drawn, filled or open as drawn) and line type (only when not solid)
  of each sample; `colour_word()` picks the nearest of twelve colour names.
- No new settings. The journal startup figure and the golden sizes are unchanged (`tests/test_v222_styling.R`,
  R2-REG01); new suite `tests/test_v223_legend_key.R`; the legend-text check of `tests/test_v210_figure_export.R` has the
  new sentence.

## v2.23.0 — 2026-11-04

**One tool instead of three, and a hardening pass.** The features of the N4-paper lysis-curve app and of the older
lysis-curve-app are now in this app (data tab, transforms, compare, presets, themes, notes, batch export, markup, error
modes, panels, matrix legend), with a statistics and curve-fitting layer comparable to GraphPad Prism. Then three rounds
of adversarial testing (about 20 000 server cells, 190 ingest files, 46 engine probes, 3 434 direct-input cells) and five
fixing passes. The
journal startup figure (Nature single column, 89 x 65 mm) is unchanged; see *Result changes* below for the four
numbers that moved on purpose.

### Data tab and ingest (`docs/DATA_TAB_AND_TRANSFORMS.md`, `tests/test_v222_data.R`)
- Detection: every non-time numeric column is a sample by position (replicate columns that share a name are pooled);
  only a temperature column (`T°`, `temp`, `°C`, whole-word match) or a non-numeric column is dropped, and the data
  summary names what was dropped and why. Long format recognises `strain` / `Strain`; wells are the samples when no
  other group column exists. `time_col_choice` overrides the time column without re-reading the file.
- *View & edit* (editable DT table, every edit goes through the ingest path, *Revert to loaded*, *Download current
  data*); *Enter data* (paste Excel / CSV text, separator auto-detected); `.xlsx` / `.xls` upload with a sheet selector.
- Exclusions that filter: OD-range exclusion on the raw readings; per-replicate exclusion (`excluded_reps`, drawn as
  hollow grey circles), applied in every mean, statistic, metric, fit and export and named in the provenance.
- Blank / baseline (`blank_mode`: constant, a blank sample, mean of the first k points) and transforms
  (`transform_mode`: t0, control sample, 0-100 % of own maximum, ln(OD)) in one place, `transform_frame()`.
- Replicate QC table (median / max CV %, high-CV flag), n = 1 warning, Samples All / None, extinction floor and
  t_stat override inputs. `run_app.R` installs `DT` and `readxl`.

### Statistics engine (`docs/STATISTICS_REFERENCE.md`, `tests/test_v222_stats.R`)
- Pure-function block `v2.22 stats engine`: column statistics (Prism-style skewness and kurtosis, geometric mean),
  `normality_table()`, `qq_frame()`, `variance_check()` (Brown-Forsythe, Bartlett), `omnibus_table()` (ANOVA with
  eta squared, Welch ANOVA, Kruskal-Wallis), `posthoc_table()` (Tukey, Holm-Welch, Games-Howell, Dunn, Dunnett,
  Bonferroni, Sidak), `two_way_time_anova()` (sample x time with wells as repeated-measures subject: `nlme::lme`
  with AR(1) errors, else `aov` with `Error(well)`), `per_time_comparisons()`, `outlier_tests()` (Grubbs, MAD, IQR;
  flags only), `auc_table()` / `auc_compare()` (trapezoid AUC above a baseline, relative AUC with a log-delta CI),
  `one_sample_tests()`, `correlation_table()`, `linear_fit_table()`, `stats_method_sentence()`.
- Every table carries a `note` column that is honest at n = 3; bad input returns NA rows with a note, never an error.
  `rstatix`, `car`, `nlme`, `mvtnorm` are optional (Dunnett falls back to Holm-Welch without `mvtnorm`, and says so).

### Fitting engine (`docs/FITS_V222.md`, `tests/test_v222_fits.R`)
- Block `v2.22 fits engine`: `fit_lysis_models()` (post-peak one-phase decay and decreasing 4PL: k, half-life, t50,
  plateau with profile / Wald intervals), `fit_quality()` (R2, adjusted R2, RMSE, AIC, AICc, BIC, runs test,
  Shapiro-Wilk), `akaike_weights()`, `fit_bands()` (delta-method confidence and prediction bands), `fit_growth_object()`,
  `fit_exponential_window()` (log-linear exponential phase, automatic window; vectorised, 5 000 points in 0.05 s),
  `compare_fits()` (Prism-style extra-sum-of-squares F test between samples), `fit_dose_response()` (4PL on log10 MOI,
  EC50 with interval), `fit_weights()`, `fit_residual_frame()`.

### Statistics and Curve fitting tabs (`docs/STATISTICS_AND_FITTING_UI.md`, `tests/test_v222_integration.R`)
- Statistics tab sections: column statistics, assumption checks with a Q-Q figure, post-hoc method selector with the
  omnibus table, time-course ANOVA with per-time comparisons, AUC, outliers, one-sample tests, an archive of all
  tables and a rectangular CSV for each (one long table per file, so `read.csv` and Excel open them).
- Analysis figures: *Per-time comparisons vs reference (stars)*, *AUC per sample*; metric-explorer correlation and
  straight-line fit with a band; MOI dose-response 4PL with its EC50.
- Curve fitting: fit statistics and Akaike weights for the growth fits, confidence / prediction bands, lysis models
  per sample or replicate with a weighting scheme, exponential-growth window, compare fits; a residual figure.
- Reports gain a "Statistics (v2.22)" section; the methods draft gains one sentence per section used; the dashboard
  gains AUC and exponential-window doubling-time cards.

### Figure styling (`docs/STYLING_AND_MARKUP.md`, `tests/test_v222_styling.R`)
- Text markup everywhere (`_{sub}`, `^{sup}`, `*italic*`, `**bold**`, `\*`) with buttons and Ctrl+= / Ctrl+Shift+= /
  Ctrl+I / Ctrl+B; the wavelength auto-subscript stays.
- Error display modes spaghetti, quantile bands, jitter, combo; error types Range and IQR (asymmetric).
- Panels section (panel source, panels per row, axis scales, header style), axis frame (Automatic / Box / L / None),
  legend title / style / wrap, direct labels at line ends, matrix (strain x MOI) legend, hide lines, open shapes.
- Linear / sqrt major and minor tick intervals with a guard against the zero-length-unit crash; minutes <-> hours
  conversion of plotted times and time metrics; `publication` palette is now Okabe-Ito (the old ColorBrewer set is
  `set1`; older settings files are migrated, `styling_schema`); Rate of OD change (dOD/dt) figure; metric-summary
  sort / horizontal / custom colours; heatmap colour scales; Band / Presentation / PDF-vector one-click looks.

### Compare, presets, themes, notes, batch export (`docs/COMPARE_PRESETS_EXPORTS.md`, `tests/test_v222_compare.R`)
- Compare tab: a second file through the same ingest code, overlay or facet, true-size preview, every export format,
  a composer panel.
- *N4 paper look* preset; the loader reads the N4 app's flat JSON and the lysis-curve-app structure and reports
  "N global, M sample settings applied, K ignored" with reasons.
- Project themes (named per-sample styles in `themes/`, auto-apply on load), notes caption and notes pages, batch
  export of any set of outputs with parity frames for PowerPoint and GIF, *Welch + Wilcoxon side by side*, night mode.

### Click-to-edit labels on the figure (v2.22.0) and Help tab (v2.22.1)
- See the two entries below; both are part of this release.

### Hardening (`tests/test_v222_hardening.R`, 320+ checks, fixtures in `tests/fixtures/`)
- **File load never ends the session.** The CSV branch of the file observer is wrapped like the Excel branch; empty,
  binary, UTF-16 (Excel "Unicode Text", SoftMax Pro), Latin-1 / cp1252 and gzip-renamed files either load (UTF-16,
  Latin-1) or refuse with a plain sentence ending "The file was not loaded." Header-only, time-only, one-column,
  all-OVER and no-readable-time files are refused with a specific message. Long files with `Time, Well, OD` load with
  the wells as samples. Quoted thousands / decimal commas, padded preambles, Excel merged titles and metadata blocks,
  empty first sheets, `write.csv` index columns, `Cycle` columns, AM/PM times, time-of-day clocks that cross midnight
  and Excel time cells that start at midnight are read correctly. A blank time cell between stacked blocks no longer
  hides the restart. The upload cap is 50 MB.
- **Nothing kills the R process.** Every export and preview size and dpi goes through one clamp (`safe_figure_size_in`),
  so a negative, zero or NA Compose size can no longer reach ragg / libjpeg; typed font sizes, thicknesses, tick length
  and counts are read through `num_or`; settings files are clamped to each box's range with a count in a notification.
- **No raw R text in the figure area.** Filters that leave no data show "No data left after the current filters ..."
  and every download refuses with a notification instead of writing the placeholder; cleared or reversed axis limits,
  log floors, single time points and non-finite tick intervals draw; deselecting every sample says so.
- **Right numbers.** Time-point exclusion matches within half the minimum time step (it used to swallow seven points on
  a seconds-scale run) and follows the displayed time unit; sample keys are exact (`A` and `A.4` no longer merge);
  Tukey with a reference reports group minus reference; Grubbs with ties gives NA with a note; zero within-group
  variance is "undefined", not p = 1e-61; MOI parsing needs the whole word; `safe_id` collisions get a hash suffix;
  the all-NA sample is dropped at load with a note; batch export of notes alone produces an archive.
- **No hangs.** `fit_exponential_window` is vectorised; the bootstrap has a run budget and a time budget
  (`n_boot_used` is shown); two-way ANOVA and post-hoc tables return a note above a size instead of running for
  minutes; `rolling_slope` is closed-form (a 96 x 150 plate select-all went from 98 s to 31 s CPU); time-limit stops are
  re-raised instead of swallowed.
- **Headless rendering** uses a null raster device: no "Arial not found" warning spam, no `Rplots.pdf`, no duplicated
  `override.aes` or "No shared levels" warnings. `shiny::validate` is called explicitly (jsonlite masks it).
- **Round 2 (after a second adversarial pass over the round-1 code).** Figure sizes are measured on an explicit
  `pdf(NULL)` device, so the journal startup figure is 217.94 x 210.21 pt whatever `options(device)` is (the null raster
  device had moved every figure by 1-3 pt; a golden-size test now pins all 20). Settings and theme files: every numeric
  key is type-checked and clamped to its own control's range (the ranges are collected from the UI), strings are capped
  at 10 000 characters, unknown keys are linear, and an unreadable or non-settings file is one plain sentence; `1e308`
  shape sizes / axis angles, 5 MB labels and 100 000-key files no longer hang. Ingest: odd-byte UTF-16, clocks before
  01:00 and "12:05 AM", a count of unreadable-time rows (a column that cannot be read is refused), one number
  convention per file, a one-line preamble, the Excel sheet selector before the first ingest, blank replicate headers
  filled from the left in upload and compare, blank group cells dropped with a count, the most distinct group column,
  a numeric dose-like long column labelled "MOI 0.1" and so on. Empty states: fit, QQ, residual, compare and analysis
  downloads refuse; replicate QC shows a sentence; no header-only CSV. Statistics and exports: the methods sentence
  gives the real wells per sample, extinction notes in every metrics table, bootstrap draws are equal across samples
  under the time budget and a clamped `boot_n` is noted, reversed per-sample windows are swapped, `MOI 1e400` is NA,
  no panel below 5 mm, one notification per distinct bad size, a one-time "may take a minute" note for large plates,
  and CSV cells that start with `=`, `+`, `-`, `@` or a tab are prefixed with a space (spreadsheet formula injection).
- **Round 3 (every input driven directly, 3 434 cells).** A scripted client or a settings file can send a select a value
  that is not one of its choices, a number box a word, a slider 1e308, or a custom-message handler a bare string; 31 of
  those ended the session (an unhandled observer error), one hung (a 1e308 point size) and 131 reached the figure area as
  raw R text. Now: the settings loader checks every static select and radio value against the control's choices (counted
  with the wrong-type keys); typed numbers are read through one clamp (the control's own range, the default when the box
  is empty or not a number); the point-size and stroke sliders are held to their ranges; the cell-edit, quick-edit and file
  observers ignore a malformed message; the sample selection keeps only samples of the loaded file; single selects are read
  as one string among the choices, else the default. A re-run of the 178 failing cells shows none left. The guard-message
  window is 20 s so a slow machine still shows a repeated guard once per cycle.

### Result changes (on purpose; all are visible in the tables and methods text)
- `extinction_time` is a crossing **from above** the floor. A curve that starts below 0.05 and never rises above it is
  NA with the note "starts below floor" (it used to report its first time point).
- Two-way time-course ANOVA uses `aov` with `Error(well)` above 12 samples or 1 500 readings (the AR(1) mixed model
  needs minutes there); the method text names the model actually used (`model_used`).
- Tukey with a reference sample reports group minus reference (the sign was reversed).
- Post-hoc and replicate-statistics CSVs are one rectangular long table per file (they were multi-section).
- A long-format file whose group column is numeric (`Time, MOI, OD`) now labels the samples `MOI 0`, `MOI 0.1`, ...
  instead of the bare numbers (a column named Sample / Group keeps its values).

## v2.22.1 — 2026-11-03

**A labelling tutorial and a Help tab.** Figures are unchanged.

- **How to label a figure** (`docs/LABELLING_HOWTO.md`): a ten-minute walkthrough on the demo data, then one
  section per kind of label: sample names and legend entries (and naming samples so the MOI is read), axis titles
  and the time unit taken from the x label, title and subtitle, legend position, end-of-line labels, panel letters
  (the four places that set one), marker / threshold / region labels, significance marks, point and dataset labels,
  the Figure Legend Text, a checklist before export and a table of common mistakes. The README gains a "How to label
  a figure" subsection under *Using the App*, and the user guide points to the tutorial. The two gallery renders it
  embeds (`docs/gallery/main.png`, `metric_summary.png`) are now tracked next to `gallery.png`, so they are there in
  a fresh clone (`tools/render_gallery.R` still regenerates them).
- **Help tab** (last main tab): the labelling summary, then every guide of `docs/` (the tutorial open by default,
  the user guide, figure standards, styling and markup, data tab, statistics and fitting, the references, phases
  and bootstrap, compare and presets) plus the README and the changelog, each a collapsible section. The files are
  read when a session first opens the tab (server output `help_guides`; `docs_dir_path()`: option
  `killcurve.docs_dir`, else `APP_DIR`, else `docs/` in the working directory) and rendered with commonmark (a
  plain `<pre>` without it), so no document text sits in the static page; images are served from the `kc_docs`
  resource path and a link between guides jumps to the other guide's section. A guide whose file is missing is
  skipped.
- Tests: `tests/test_v221_ui.R` checks the tab, its contents, the rewritten links and images.

## v2.22.0 — 2026-11-02

**Click-to-edit labels on the figure.** Renaming a legend entry or an axis title meant finding the box in the
quick-edit strip or the sidebar. The true-size preview of the main figure now is the export SVG, inline, and its
labels are clickable. The exported files are byte-for-byte unchanged (the export path is untouched).

- **Inline SVG preview.** With *Edit labels on the figure* ticked (`canvas_edit`, default on, saved with the
  settings, in the quick-edit strip's header) the Lysis curve preview is `output$od_plot_svg`: the file svglite
  writes for the export, with the `<?xml?>` prolog removed and the root width / height set to the preview size
  (`svg_inline_for_preview()`; the viewBox stays). It is literally the file Inkscape opens. Unticked, or in the
  pixel-canvas mode, the preview is the PNG as before; the Analysis, Compose and Curve fitting previews are
  unchanged. An SVG over 6 MB shows a note instead of the figure.
- **Click a label.** A legend entry opens a small popover with the name, a colour picker and a *show* box; the
  x-axis title, y-axis title, title and subtitle open a text box. Apply (or Enter) writes into the existing
  quick-edit / sidebar inputs (`qe_name_<id>`, `qe_color_<id>`, `x_axis_label`, ...), so renaming, recolouring
  and hiding behave exactly as in v2.21. Esc, a click outside or Close leaves everything as it was.
- **Limits.** Only the samples in the quick-edit strip (the first 96) are clickable; a hidden sample is
  re-shown from its *show* box in the strip. Tick labels, facet strips and the legend title are not editable on
  the figure. A label is matched by its text, so two samples with the same display name share the first match.

## v2.21.0 — 2026-11-01

**Sticky figure, accordion sidebar with a jump menu, quick-edit strip, rename samples everywhere.**
Three usability complaints: the figure scrolled out of view while a long sidebar was edited, renaming a sample
was buried in Variable Styling and only changed the legend, and finding a section in a sidebar of thirteen was
slow. The default main figure is unchanged (the v2.9 byte-identical check is unchanged); the two new inputs
have `APP_DEFAULTS` entries (placed right after `demo_scenario`).

- **Sticky figure.** The layout is now a flex row (`#kc_layout`, sidebar 25 %); the figure column
  (`#main_panel`: size bar, tabs, figure) is `position: sticky; top: 0; max-height: 100vh` with its own
  scrolling, so it stays in view while the sidebar scrolls. `sticky_figure` (checkbox at the top of the
  sidebar, default on, saved with the settings) removes the `kc-sticky` class in the browser; on a window
  narrower than 768 px the columns stack and nothing is pinned. While the column is pinned the auto-fit
  preview zoom also fits the window height (`sticky_viewport_height`, reported by the page; the main figure
  only; the quick-edit strip is reserved for while it is open), so the figure and the strip are in view
  together. Every existing input and output id is unchanged.
- **Accordion sidebar.** The thirteen sidebar sections are `kc_section()` details elements that close each
  other (one open at a time; native `<details>`, so Enter / Space on a summary still work). **Jump to a
  section...** (`sidebar_jump`, UI only, not saved) opens a section, scrolls it to the top of the window and
  focuses its summary; **Collapse all** closes them. The Statistics tab's sections stay independent.
- **Quick-edit strip** under the figure (Lysis curve tab; `quick_edit_open`, default on, *Hide quick edit*
  link). One row per loaded sample: a 12 px swatch, the name (`qe_name_<id>`), a colour picker
  (`qe_color_<id>`) and a **show** box (`qe_show_<id>`: a click removes / adds the sample in Select Samples,
  in plot order). The X- and Y-axis labels and the legend position were **moved** here from the sidebar
  (same input ids; a one-line note remains in Axis Settings and Figure Layout). A colour picked in the strip
  is the sample's colour (`rv$qe_colors`, read first by `resolve_aesthetics()`); the sidebar colour widgets
  follow it, a different colour chosen in the sidebar replaces it, and the swatches follow the sidebar.
- **Rename samples everywhere.** `rv$display_names` (raw column name -> display name) is fed by the name
  boxes. The legend labels read it (the Variable Styling "Legend Label" box is gone: one source of truth),
  and so do the tables of the Statistics tab and the Dashboard, the curve-metrics, per-replicate-metrics,
  statistics (`sample`, `versus`, Tukey `comparison`, warnings and notes), extraction, phases, bootstrap and
  fit tables, the all-metrics comparison, the QC flags, the reports (the data provenance lists
  "raw column name → display name"), the Figure Legend Text, the methods text and the PowerPoint slide titles.
  The CSV downloads and the bundle's tables carry `sample` (display) with `sample_raw` right after it (and
  `versus_raw`, `comparison_raw`). The raw column name stays the key in `rv$col_map`, `selected_samples`, the
  reference sample and the settings file (`sample_aesthetics` still keys by raw name; its `legend_label` is
  the display name, so older files load). MOI is read from the raw name; a display name fills in a sample
  whose raw name has no MOI. `curve_metrics_current()` gains a `sample_display` column while any sample is
  renamed. The bundle's `source_data_tidy.csv` keeps the file's own names.
- Tests: `tests/test_v221_ui.R`.

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

## v2.20.0 — 2026-10-20

**Curve phases figure, bootstrap intervals, separation from the reference, gallery update.**
The v2.20 engine functions (`curve_phases_by_sample`, `bootstrap_metrics`, `curve_difference`,
`first_separation_time`) now have a user interface. The default main figure is unchanged (the v2.9
byte-identical check is unchanged); every new input has an `APP_DEFAULTS` entry and is saved with
the settings (the Run bootstrap button is not).

- **Curve phases** (analysis type `phases`; also in the Analysis figures select, the composer and
  Download all figures). The mean curves of the landmarks figure (same colours, `y_scale_type` for the
  y axis) with each sample's phases shaded: lag grey, exponential green, deceleration yellow-green,
  stationary blue, lysis vermillion, recovery purple, post-lysis grey-blue (a fixed, muted,
  colour-blind-safe palette; phase key outside right). One sample: full-height translucent bands
  (alpha 0.12) behind its curve. Several samples: bands would overlap, so each sample gets a thin
  strip (6 % of the panel height, one row per sample, stacked at the top, labelled on the left in the
  sample's colour at the axis-text size) and the curves sit below; strips are drawn at alpha 0.55
  because 0.12 is unreadable on a strip. The phases follow the metrics smoothing window, peak mode and
  lysis threshold (not the peak-detection smoothing window, which `curve_phases()` does not take).
  - **Statistics tab, "Curve phases"**: the phases table (sample, phase, start, end, duration, OD
    start, OD end; 3 significant digits) and a CSV (unformatted). `phases.csv` goes into the figure
    bundle; the HTML and PDF reports show the figure and the table right after the landmarks figure.
- **Bootstrap intervals** (Statistics tab section). `boot_metrics` (multi-select, default lysis onset,
  OD drop, μmax, AUC, infection strength), `boot_n` (200-5000, default 1000), `boot_seed` (default 1) and
  a **Run bootstrap** button. The bootstrap is computed only when the button is pressed and cached
  (with a signature of the replicate data and the metric settings); a change to the data, the filters or
  windows, the blank, the sample selection or the metrics settings marks it **out of date** with a notice
  (the table stays visible; the metric summary, the reports and the bundle ignore it until it is run
  again). The table: sample, metric, estimate, CI low, CI high, n replicates, n ok draws; CSV; a note
  that it is a percentile interval from replicate-level resampling and is wide / degenerate at n = 3.
  `infection_strength` (and the other reference-relative metrics) are not bootstrapped: they are not
  produced by `compute_one_curve_metrics()`, which `bootstrap_metrics()` resamples, so a run drops them
  and says so in the note.
  - **Metric summary error source**: `metric_error_source` = "Replicate spread (SD/SEM/CI as in Error
    Bars)" (default) or "Bootstrap 95 % CI of the mean-curve metric": the bar is then the mean-curve
    estimate and the whiskers the bootstrap interval. A metric missing from the cached bootstrap (or no
    current cache) keeps the replicate spread, with a notification. The report caption follows.
  - Reports and bundle: a **Bootstrap intervals** section and `bootstrap.csv` when a current bootstrap is
    cached; the drafted methods text gains a sentence.
- **Separation from the reference** (analysis type `difference`, needs a reference sample; the
  placeholder says so without one). `diff_mode` ("Difference (OD)" or "log2 ratio") and `diff_mark`
  (default on). Each non-reference sample's `curve_difference()` against time, a dashed zero line, the
  figure's colours, shapes and theme, and an asterisk marker above the curve where
  `p_adj_holm < 0.05`. Under the figure (Analysis tab) and in the Statistics tab's Replicate comparison
  section ("First separation from reference"): `first_separation_time()` per sample (the reference is
  left out; "none" when never reached). Reports: the figure and the table when a reference is set.
- **Gallery**: `tools/render_gallery.R` draws phases, difference, fit_overlay and phenotype_heatmap too
  (order: main, landmarks, phases, replicate_overlay, growth_rate, normalized, difference,
  metric_summary, dose_response, fit_overlay, phenotype_heatmap, heatmap) and, with more than 8 panels,
  composes three columns at 183 mm with 55 mm per panel row (two columns as before up to 8).
  `docs/gallery/gallery.png` is regenerated.
- Tests: `tests/test_v220_integration.R`; the report image counts in `tests/test_v219_integration.R`
  rose by two (the phases and the separation figure).

## v2.19.0 — 2026-10-20

**Curve fitting tab, phenotype heatmap, extraction tables and the all-metrics comparison.**
The v2.18 engine functions (`fit_growth_models`, `phenotype_matrix`, `compare_all_metrics`,
`od_at_times`, `time_to_od`, ...) now have a user interface. Nothing changes the default
main figure (the v2.9 byte-identical check is unchanged); every new input has an
`APP_DEFAULTS` entry and is saved with the settings.

- **Curve fitting tab** (new, after Statistics). Inputs: `fit_models` (Logistic / Gompertz /
  Baranyi, all by default), `fit_until` ("Up to the peak" or "Whole curve"), `fit_log` (fit
  ln(OD/OD0)), `fit_level` ("Mean curve" or "Each replicate"), `fit_show` ("Best model by
  AIC" or "All converged models"). One reactive, `fit_results()`, holds the fit table, so the
  figure, the tables, the reports, the bundle and the methods sentence share one computation,
  made only when the fit inputs or the data change.
  - **Figure** (analysis type `fit_overlay`; also in the Analysis figures select, the composer
    and Download all figures): the mean curve as points with the main figure's error measure
    as whiskers, the fitted curve of each sample on a 200-point time grid from
    `predict_growth_model()` (solid = best model by AIC; with "All converged models" the others
    are thin and dashed; at replicate level one thin curve per replicate), the sample colours
    and shapes of the main figure, `y_scale_type` for the y axis, points after the peak in grey
    when the fit stops there. Same theme, fixed square panel and export as every analysis
    figure; its own preview and download on the tab.
  - **Parameter table**: sample, (replicate), model, best (a tick), converged, points, A, mu_max,
    "Lag (time units)" = the fit's start time + its `lag` (the fit lag is relative to the first
    fitted point), doubling time = ln 2 / mu_max (none when the fit is not on the ln scale), R²,
    AIC, the three standard errors and the engine's `message`, at 3 significant digits, with
    the poorly determined fits highlighted. CSV button (all rows, unformatted). At replicate
    level a second table gives mean ± SD of A, mu_max and the absolute lag per sample and model
    over the converged replicates, with its own CSV.
  - **Notes** under the controls: the fit range, the ln-scale meaning of mu_max, the lag
    convention, how to read AIC on short curves, and every row that carries a `message`
    ("only 5 points: parameters are poorly determined" in bold).
- **Phenotype heatmap** (analysis type `phenotype_heatmap`): `phenotype_matrix()` on the curve
  metrics, samples (rows, sample order) by metrics (columns). `pheno_metrics` (multi-select over
  every metric label plus the other numeric columns of the metrics table; default lysis onset,
  OD drop, lysis efficiency, μmax, AUC, infection strength, extinction time, time of first
  peak), `pheno_scale` (z-score / 0-1 range / raw), `pheno_values` (print the raw value in
  each cell, axis-text size). z-scores use a blue-white-red scale centred at 0, the other
  scales viridis; the legend title is "z-score", "scaled" or the unit. The panel is not forced
  square (a non-curve kind, like the OD heatmap). Metrics that are undefined or constant across
  the samples are left out and listed in a note.
- **Extraction** (Statistics tab, before Methods text). **OD at chosen times**: `extract_times`
  ("30, 60, 120"), `extract_method` (interpolate / nearest), a table of time, sample, mean, SD, n,
  SEM and CI95, and per-replicate and summary downloads. **Time to threshold**:
  `extract_threshold` (0.5), `extract_direction` (up / down), `extract_after` (start / peak),
  the per-sample summary and the per-replicate crossings. Both use `prepare_replicate_data()`,
  so they follow the filters, per-sample windows and blank. **All-metrics comparison**:
  `compare_all_metrics()` over every metric with finite replicate values, with the current
  statistics method and reference, its warnings, a CSV, and the note that `p_adj_bh_across` is
  exploratory.
- **Reports and bundle**: with "Include analysis figures and statistics" on, the HTML and PDF
  reports gain a **Curve fits** section (the fit figure at export size, the parameter table,
  the notes and warnings), the **Phenotype heatmap** figure, and **Extracted values** (the OD-at-times
  table when times are typed, the time-to-threshold table while the threshold box is not empty).
  The figure bundle adds `fits.csv` and the configured extraction CSVs. The drafted methods text
  gains a sentence on the fitted models and range once the Curve fitting tab has been used.
- Tests: `tests/test_v219_integration.R`; the report image counts in `tests/test_v211_analysis.R`
  rose by two (the heatmap and the fit figure).

## v2.18.0 — 2026-10-20

**Per-sample time windows, sample groups and facets, metric explorer, download all figures.**

- **Per-sample time windows**: Time Point Filtering has a table under the global filter
  with a *from* and a *to* box per selected sample (`tw_min_<id>`, `tw_max_<id>`; blank =
  no limit). `apply_time_windows_inputs(df)` drops the rows of each sample outside its
  window in `prepare_plot_data()` and `prepare_replicate_data()`, right after the global
  time filter (and also when the global filter is off), keeping the `variable` and `time`
  columns, so every figure, statistic, metric and export follows. The windows are saved in
  Save Settings (per sample, like the legend label; a blank limit is simply not written),
  restored by Load Settings, reset by Clear, and listed in the report provenance, the
  Figure Legend Text and the drafted methods.
- **Sample groups and facets**: a per-sample **Group** text box in Variable Styling
  (`group_<id>`, saved and loaded like the legend label). New checkbox `facet_by_group`
  (off; the Legacy look and the journal preset both set it off): with at least two distinct
  non-empty groups the main figure becomes `facet_wrap(~ group, scales = "fixed")`, the
  strip text is the group name at the axis-title size without a strip background, samples
  with an empty group go to a panel called "Other", and the legend stays outside. With the
  fixed panel size, `fixed_panel_grob()` already divided the panel area among the panel
  rows and columns of a facet figure, so the panel area you set is shared by the facets
  (unchanged). The composer has a new panel **Lysis curve, one panel per group**
  (`main_by_group`): one single-group main figure per group, titled with the group name, in
  group order with "Other" last and tagged a, b, c in that order; it is left out (and
  listed) when no sample has a group.
- **Metric explorer** (analysis figure `metric_explorer`): `explorer_x` and `explorer_y`
  choose any two metrics (defaults lysis onset and OD drop), `explorer_level` ("Per
  replicate" / "Per sample mean ± error") whether the replicate values are drawn as open
  circles, `explorer_labels` adds text labels at the points (`geom_text`, axis-text size),
  `explorer_logx` / `explorer_logy` log10 axes. One point per sample, the mean with error
  bars in both directions from the main figure's error measure (`error_half_width()`),
  sample colours and shapes from the per-sample styling, axis labels from
  `metric_axis_label()`. It uses the shared journal theme and, in fixed-panel mode, a square
  panel. Also available in the composer.
- **Download all figures (.zip)** (Plot Dimensions & Export): `all_figures_<W>x<H>mm.zip` with
  an SVG and a PNG of the main figure, of every analysis figure that renders for the current
  data and of the composed figure (the existing sized names), `figure_legend.txt` and
  `methods.txt`; figures that return the "not available" placeholder are listed with their
  reason in `SKIPPED.txt`. Same archive code as the figure bundle (`zip::zip`, else
  `utils::zip`, else a `.tar`).
- `fixed_panel_grob()` builds its grob on a scratch device when the plain build fails: a
  figure with a plotmath colour-bar title (the heatmap) used to fail with "failed to find or
  load PDF CID font" in fixed-panel mode when no graphics device was open.
- Tests: `tests/test_v218_features.R`; `tests/test_v211_analysis.R` now expects the composer's
  new `main_by_group` choice.

## v2.17.0 — 2026-10-19

**Log-axis clipping, QC flags, notebook exports, figure bundle, threshold line, gradient palettes.**

- **Log-axis clipping**: a post-lysis reading below the log-axis floor
  (10^`y_log_min_exponent`) used to be deleted by ggplot's default `oob` (censor), so
  the lysed curve lost its points. New input `y_oob` ("Readings below the axis
  floor"): **Clip to the floor (recommended)** = `scales::squish`, the default and the
  journal preset's value, or **Drop (legacy)** = `censor`, which the Legacy look sets
  so the v2.9 output stays byte-identical. Applied to every y scale in the main
  figure and to the log y scales of the replicate overlay and the landmarks figures.
- **QC flags**: `qc_flags(rep_pd, pd, ref_sample, ceiling = 2.5)` returns
  (sample, flag, detail) rows for CEILING (a reading above the ceiling), NEGATIVE
  (a reading <= 0), GAP (a time step of at least 2x the median step; "at least" so that
  one missing reading in a regular series is caught) and FLAT_REFERENCE (reference max
  OD < 2x its initial OD). Shown as a warning list under the sidebar data summary,
  beside the non-positive-OD warning, on the Dashboard, and as a "QC flags" section
  after the provenance in the HTML and PDF reports (the PDF gets a page only when a
  flag was raised, and a "QC flags: none" line otherwise). New input `qc_od_ceiling`.
- **Legend text in the reports**: the Figure Legend Text (now `figure_legend_string()`)
  is a "Figure legend" paragraph right under the main figure in the HTML and PDF
  reports.
- **Notebook downloads**: "Download notebook (.txt)" (filled fields as `key: value`,
  then the legend and methods text) and "Download notebook row (.csv)" (one row, one
  column per filled field, custom keys as column names). New notebook fields
  `nb_passage` (Passage #), `nb_inducer_conc` (Inducer concentration) and
  `nb_other_condition` (Other condition), in the reports and both downloads; the
  "Inducer (+ conc.)" label is now "Inducer".
- **Figure bundle**: "Download figure bundle (.zip)" writes the main figure (SVG +
  PDF), the composed figure (SVG, with two or more panels), `settings.json`,
  `source_data_tidy.csv`, `summary_statistics.csv`, `curve_metrics.csv`,
  `figure_legend.txt`, `methods.txt` and `README.txt` as
  `figure_bundle_<W>x<H>mm.zip` (`zip::zip`, else `utils::zip`, else a `.tar`).
- **Threshold line**: `enable_threshold`, `threshold_value`, `threshold_label`,
  `threshold_color`, `threshold_linetype` (Time Point Markers section): a horizontal
  `geom_hline` with an optional label at the right end in the axis-text size. Off by
  default and cleared by the journal preset and the Legacy look.
- **Gradient palettes**: "Blues (dose gradient)" and "Blue to red (dose gradient)"
  for MOI series: the first sample (the control) black, the others a ramp
  (`colorRampPalette` over #c6dbef-#08306b, or #2166ac-#b2182b). `get_palette_colors()`
  moved out of the server so it can be tested.
- **Robustness**: when the selected samples match no loaded column (for a moment when
  a new file replaces the old one), `prepare_plot_data()` and
  `prepare_replicate_data()` return zero-row frames with their usual columns instead of
  erroring, so the Statistics tab and the stats notes no longer fail in that moment.
- Dashboard hint now points to the Statistics tab; `.claude/` is git-ignored; the README
  gallery was regenerated.

## v2.16.0 — 2026-10-19

**Subscript axis labels and a Statistics tab.** The wavelength in the y label is
typeset as a subscript (A₅₅₀, OD₆₀₀), and the descriptive statistics and an
editable methods paragraph have a tab of their own.

- **Subscripts**: `label_with_subscripts()` turns a label containing `A<digits>`
  or `OD<digits>` ("A550", "OD600", "A550 relative to Uninfected", "log10 A550")
  into a plotmath expression with the digits as a subscript; a label without such
  a token is returned unchanged, so nothing else changes. New checkbox
  `axis_subscripts` ("Typeset wavelength as subscript"), on by default and set by
  the journal preset; the **Legacy look** turns it off, which keeps the v2.9 output
  byte-identical. It applies to the main figure, the analysis figures' y labels,
  the heatmap key and the dose-response axis. Metric facet strips, the Figure
  Legend Text and the report captions stay plain text ("A550"). Plotmath ignores
  the *Italic axis labels* setting for a converted label.
- **Statistics tab** (after Dashboard), one collapsible section each: summary
  statistics (time, sample, mean, SD, n, SEM, 95% CI half-width; searchable with
  25 rows per page when the `DT` package is installed, a striped table otherwise;
  capped at 2000 rows on screen, the CSV has all), curve metrics with the metric
  definitions, per-replicate metrics, and the replicate comparison (the same
  pairwise, omnibus and post-hoc tables as the Analysis tab, with the notes). Each
  has its own CSV button (`downloadStatsCSV_tab`, `downloadMetricsCSV_tab`,
  `downloadReplicateMetricsCSV_tab`, `downloadAnalysisStatsCSV_tab`) sharing the
  content function of the sidebar button.
- **Methods text**: `methods_text` box (6 rows, empty by default). **Draft from
  current settings** writes a paragraph from the settings: measurement and time
  unit, n, how the mean and the error statistic were computed, the metric
  definitions (μmax window, peak mode and smoothing, lysis threshold), and the
  statistical tests (method, Holm, reference). It overwrites only an empty box,
  or when **replace existing text** is ticked. The text is yours to edit; when
  non-empty it appears as a **Methods** section before the statistics in the
  HTML and PDF reports, and **Download methods (.txt)** saves it. New defaults:
  `axis_subscripts`, `methods_text`, `methods_replace`. New optional package: `DT`.
- **No panel letter unless you set one**: `panel_label` and `analysis_panel_label`
  now start empty (the journal preset sets a label only from its own *Panel label*
  field, empty by default), so the startup figures carry no "a" / "b". The Figure
  Legend Text then has no "(a) " prefix, and the sized file names use "main" /
  the figure type (`lysis_curve_main_...`, `metric_summary_metric_summary_...`).
  The composer keeps its a, b, c letters (composing is an explicit request); the
  new checkbox `compose_tags` ("Add panel letters", on by default) turns them off,
  and the report caption then says "N panels" instead of "Panels a-N".
- Tests: `tests/test_v211_analysis.R` covers the subscript helper, the SVG text
  elements (a smaller "550" next to "A" with the checkbox on, a single "A550"
  with it off), the Statistics tab outputs, `draft_methods_text()` and the
  Methods section of the reports.

## v2.15.0 — 2026-10-19

**Square panels.** The plotting panel is now a fixed size and the figure is
sized around it, so curve figures keep the same shape whatever the journal
width. The default panel is 65 x 65 mm; the figure's outer size comes from the
panel plus the axes and the legend. **Legacy look** restores the v2.14 behaviour.

- **Fixed panel**: new inputs `panel_size_mode` ("fixed" or "fit"),
  `panel_width_mm`, `panel_height_mm` and `panel_square`. The default is a fixed
  65 x 65 mm square.
- **Computed figure size**: `fixed_panel_grob()` builds the figure from the panel
  plus the axis and legend space. With the legend inside the panel (the default,
  in the emptiest corner) the figure is about 75 mm wide; an outside legend adds
  20-28 mm.
- **Size bar fits text**: "Panel 65 x 65 mm · figure W x H mm · ... fits: single
  column · two across double column" uses the chosen journal family's widths:
  single column if W <= single; two across if 2W + 5 <= double; three across if
  3W + 10 <= double.
- **Square curve figures**: the main lysis curve, growth rate, normalized OD,
  replicate overlay, curve landmarks and dose-response are square when
  `panel_square` is on. The metric summary shares the panel area across its
  facets, and the heatmap may be non-square.
- **Legacy look**: the "Legacy look" preset sets `panel_size_mode = "fit"`, so the
  panel fills the export size as before.
- **Composer**: the Compose figure tab uses the fixed-panel grobs, so every cell
  has the same panel size. Height per figure row is about the panel height plus
  8 mm.

## v2.14.1 — 2026-10-19

Hardening after a "vector memory limit reached" report. No change to normal output.

- **One raster guard**: `safe_raster_dpi(width_in, height_in, dpi, max_pixels = 40e6)`
  returns the largest dpi (floored, at least 36) that keeps the image within 40
  megapixels, with attribute `capped`. It is used by every raster render: the
  true-size preview (`renderImage`), PNG/TIFF/JPEG downloads of the main,
  analysis and composed figures, the report PNGs and the GIF frames. A
  notification says when the dpi was reduced.
- **Size and zoom clamps**: `export_size_in()` limits each side to 60 in
  (1524 mm) with a notification and falls back to 89 x 65 mm for an emptied or
  zero box; the preview zoom is clamped to 0.5-4 and the page width to 200-8000
  px; GIF and canvas sizes are bounded. (`ggsave(limitsize = FALSE)`: the app's
  own limit governs.)
- **Tick-count guards**: a manual x tick interval, or a time range, that would
  draw more than 500 major or minor ticks falls back to ggplot's default breaks
  (the break vectors are counted before they are built), and an exponent range
  over 20 decades (or reversed) on the log axis uses default minor breaks; each
  shows a notification.
- **Graceful failures**: preview and dashboard renders show the error figure
  instead of failing; every download (images, reports, CSV, PPTX, GIF) fails with
  a readable message and an error notification. Memory errors get the hint
  "Reduce DPI, export size, zoom or the number of points."
- **Data-size sanity**: more than 5,000 time points or 200 samples adds a
  persistent warning to the data summary; the dashboard's four figures are
  skipped (with a note) above 2,000,000 values (time points x samples x
  replicates).
- **Memory diagnostics**: the report's key-settings table has the R vector
  memory limit (`mem.maxVSize()`) and the figure raster sizes used.
- Tests: raster guard values, size clamps, tick guards (manual interval 0.001),
  a wrong-unit (89 x 65 in) preview and download, data-size warnings, report rows.

## v2.14.0 — 2026-10-19

**Change of default output.** The app now starts in the journal standard instead
of the old 10 x 8 in / 20 pt / PDF state. Everything the journal preset sets is
the start value (`APP_DEFAULTS = journal_preset_settings(89, 65, "a")`): Nature
single column 89 x 65 mm, Arial 7 / 6 pt, 0.75 / 0.5 pt lines, Okabe-Ito
palette, legend in the emptiest corner, trimmed log labels, SVG at 600 dpi,
panel label "a", true-size preview. Non-visual defaults (metrics, notebook,
analysis, compose, demo scenario) are unchanged.

- **Legacy look**: the button "Legacy look (v2.9 defaults)" in Journal Figure
  Preset restores the old values (`legacy_preset_settings()`); the v2.9
  byte-identical test now applies it. **Clear & Reset** returns to the journal
  standard. Preset, Legacy look and Reset share one update loop
  (`apply_settings()`), which also keeps the unit bookkeeping (mm / in) right.
- **True-size previews everywhere**: the zoom fits the main panel width
  (checkbox "Fit the preview to the panel width", on by default; rounded down to
  0.25, 0.5-4x; the slider applies when it is off). In canvas mode the height
  now follows the export aspect ratio instead of a fixed 600 px (the canvas
  height input is gone). The composer preview uses the same path.
- **Figure size bar** above the tabs: "Export: 89 x 65 mm · Nature single
  column · Arial 7 / 6 pt · lines 0.75 / 0.5 pt · SVG"; in warning style with an
  inline "Apply journal preset" button when units are not mm, text is outside
  5-8 pt, the font is not Arial/Helvetica/sans or data lines exceed 1.5 pt. On
  the Compose tab it also shows the composed size.
- **Sized file names without timestamps**: `lysis_curve_<label>_<W>x<H>mm.<ext>`,
  `<type>_<label>_<W>x<H>mm.<ext>`, `figure_composed_<W>x<H>mm.<ext>` (inches with
  one decimal when the units are inches). Reports, settings and GIF/PPTX files
  keep their timestamps.
- **PDF report**: opens the font-embedding device the image export uses, since
  `grDevices::pdf()` cannot draw Arial (now the default font).
- Widget start values come from `APP_DEFAULTS`; the font-size inputs accept 5 pt.
- Tests: `test_v210` applies `legacy_preset_settings()` for the v2.9 byte
  comparison and checks the startup state (UI HTML attributes and an 89 mm,
  6-8 pt Arial SVG from a session with only `APP_DEFAULTS`); the new section
  covers file names, the bar, zoom and canvas aspect.

## v2.13.0 — 2026-10-19

A multi-panel figure composer, so a complete journal figure can be assembled
inside the app. Opt-in: the default main figure is byte-identical to v2.9.0.

- **Compose figure tab** (after Dashboard): tick the main lysis curve and any
  analysis figures (same labels as the Analysis tab), choose 1 or 2 columns,
  the figure width (double or single column of the journal chosen under Journal
  Figure Preset, or a custom width in mm), the height of each panel row, and
  the panel-label size and case (a, b, c or A, B, C). Panels are composed in
  checkbox order at their native size, so text stays 6-8 pt. A note shows the
  resulting size in mm and warns when the height exceeds the journal maximum.
  The preview uses the same true-export-size and canvas mechanics as the other
  figures; **Download composed figure** writes the file at width x rows x panel
  height in the Export format (TIFF stays LZW).
- Panels whose figure is "not available" (e.g. the dose-response with one MOI
  level) are left out and listed in the note and in a notification.
- `compose_figure(panels, ncol, tag_size, font, tag_case)` is the single layout
  function (patchwork, `free()` panels, own tags removed). `tools/render_gallery.R`
  now calls it, so the gallery and the app agree.
- **Reports**: with "Include analysis figures and statistics" the HTML and PDF
  reports end with the "Composed figure" (when at least two panels are
  selected), at its own size and aspect ratio.
- Settings: the new inputs are in `APP_DEFAULTS`; Save/Load and Reset now handle
  the checkbox group `compose_panels` (and the compose radio buttons).
- Tests: composed SVG at 183 x 120 mm (518.74 pt wide, bold 8 pt tags a-d, all
  text 6-8 pt), 1 column, widths per journal, skipped panels, download, and the
  composed figure in the HTML and PDF reports.

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
