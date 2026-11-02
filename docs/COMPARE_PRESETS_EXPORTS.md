# Compare, presets, old-settings import, themes, notes and batch export (v2.22)

What this adds, how it works, and what it does not do. Code: `Lysis_Curve_App_v28.R` (the pure helpers sit in one block
before the UI, the server wiring in one block at the end of `server()`); tests: `tests/test_v222_compare.R`
(fixtures `tests/v222_n4_settings.json`, `tests/v222_lca_settings.json`).

## 1. Compare tab

A second file (CSV, or Excel when `readxl` is installed) is drawn against the main data.

* **Same ingest path.** `ingest_frame(df)` (in `server()`) returns what `ingest_data()` writes into `rv`, as a list: time column,
  samples, column map, long / wide, group / value / replicate columns, stacked blocks, time points. It calls the same detectors
  (`detect_time_column`, `is_long_format_detect`, `detect_od_column_idx`, `detect_od_columns`, `infer_wide_replicates`,
  `detect_replicate_column`, `coerce_sentinel_columns`, `parse_time_column`). The orchestration around them is repeated, so a test
  compares the two for a wide, a long and a stacked-block file. Folding `ingest_data()` onto `ingest_frame()` is possible later
  (the merge supervisor may prefer it); it was not done here to leave `ingest_data()` untouched.
* **Same data preparation.** `compare_scope(ds)` copies `prepare_plot_data`, `prepare_replicate_data` and `apply_time_filters`
  with an enclosing environment in which `rv` is the second dataset, so the second file gets the same filters, blank subtraction,
  replicate pooling and SD / SEM / CI as the first. The blank, time filter and per-sample windows are the main figure's.
* **Same figure.** The layers come from `build_plot()`.
  * *Overlay* (one panel): composite sample keys `A||<sample>` / `B||<sample>`; colour and shape by sample (display names from the
    quick-edit strip apply: legend entries read "Control (Run 1)"), the dataset by line type (B dashed; dotted when the sample line is
    already dashed) or by transparency (B drawn with an alpha channel, `compare_alpha_b`). Samples that exist in one dataset only
    keep a default colour by position.
  * *Facet*: one panel per dataset (`facet_wrap`, shared scales), one legend entry per sample. In fixed-panel mode every panel keeps
    the fixed panel size (the figure is as wide as two panels need).
  * The notes caption, journal theme, fonts, sizes, error bars and log floor are the main figure's.
* **Preview and export.** `register_preview("compare_plot", ...)` (canvas or true size), a download in the export format
  (`export_device`: SVG / PDF / PNG / TIFF / JPEG), the figure as a panel of the Compose tab (`compare`), and a file in
  "Download all figures" (`compare_<A>_vs_<B>_<size>.svg` + `.png`).
* Inputs: `compare_file`, `compare_label_a/b`, `compare_mode`, `compare_overlay_style`, `compare_alpha_b`, `compare_samples_b`
  (empty = all), `compare_common_only`. They are saved with the settings (the file and the sample picker are not).
* Not done: replicate points (`show_replicate_points`) are not drawn for the composite keys; a different blank per dataset.

## 2. N4 paper look

A button beside "Legacy look". `n4_preset_settings()` follows the `legacy_preset_settings()` pattern: the legacy values of every
journal-preset input, the axis / error-bar defaults of the N4 app, then the N4 specifics. Journal startup defaults are unchanged.

| Input | Value | Why |
|---|---|---|
| `axis_text_font_size`, `axis_label_font_size`, `title_font_size` | 20 | the N4 defaults (Legacy: 16 / 20 / 20) |
| `legend_font_size` | 11.2 | the N4 app had no control; read off a render: `theme_prism(base_size = 14)` legend text is 0.8 x 14 |
| `point_stroke`, `shape_size`, `line_thickness` | 1.5, 3, 1 | N4 defaults (Legacy stroke 0.5) |
| `label_font_size` | 57 | the N4 end-label size 20 is ggplot mm (20 x 2.845 pt); the input's maximum was raised from 36 to 60 |
| `show_error_caption` | off | the N4 figures have no caption (Legacy has one) |
| `y_oob` | squish | the audited N4 behaviour |
| `legend_position`, `legend_order` | right, alpha | ggplot factor order, as N4 |
| `export_format`, units, size, dpi | pdf, in, 10 x 8, 300 | N4 export defaults |
| `preview_mode`, `plot_width`, `panel_size_mode` | canvas, 800, fit | N4 canvas of 800 px |
| `theme_base_pt`, `axis_subscripts` | 0, off | theme spacing of the theme, plain "A550" |
| `error_log_floor`, `error_cap_relative` | off, off | N4 drops bars crossing the log floor; caps in time units |
| `error_bar_color`, `error_display_mode` | sample, bars | as N4 |
| `y_tick_label_style` | legacy | ggplot default labels |
| `x_tick_interval` | 30 | N4 major interval 30 (minor 10, see below) |

**Check by render.** The N4 app's default figure (its server run through `testServer`, long-format version of the demo data, the
per-sample inputs set to the N4 UI defaults: black circle, red square, blue triangle) and the v2.22 app with the preset were
rendered at 10 x 8 in. Axes, panel position, tick labels, legend box and legend text, line and point sizes match. Residual
differences, none of them settable from the preset:

* Log y axis minor ticks: N4 draws 2 to 9 only in the decades below the top exponent; here 1 to 9 in every decade, so the 1-10
  decade has minor ticks.
* X axis: no separate minor interval (minor ticks are half the major interval: 15, N4 10); the data end point is added to the major
  breaks when it is not one (a 0-175 axis shows a "175" tick).
* Linear / sqrt y tick intervals (N4 0.5 / 0.1) do not exist; the old inputs are ignored on import.
* Error bars are drawn under the line and points here, over them in N4.
* End labels: the N4 size 20 is mm; 57 pt reproduces it, but the repelling differs a little.
* Region rectangles: N4 draws one rectangle per data row, so the alpha stacks towards opaque; here one rectangle.
* Marker label y position: N4 from the axis limits (top = 95 % of the upper limit), here from the plotted data.
* Default colours and shapes beyond the fifth sample (N4: 6th shape open circle, 7th colour yellow; here 4, 8, 23 ... and brown).
* The N4 legend heading "variable" of older builds cannot be drawn (no legend title control); the audited file draws none.
* Fonts and pixels: PDFs embed the font via quartz / cairo, PNG / TIFF use ragg, so a pixel comparison needs a tolerance.

## 3. Old settings files

The loader first runs `translate_old_settings(s, od_vars, known_ids)` (pure, tested). The result has this app's structure
(`global_settings`, `sample_aesthetics` keyed by sample name), so the rest of the loader is unchanged. The status text and a
notification read "N4 app settings imported: N global, M sample settings applied, K ignored."; the settings status also lists the
ignored controls (with the reason) and notes. `rv$last_import` keeps the record.

Kinds: `n4` (no `global_settings`: the whole of `reactiveValuesToList(input)` written without `auto_unbox`, every scalar a
one-element array), `lca` (the lysis-curve-app file: `experiment_notes`, `notes_*`, floating labels, panel / matrix keys), `current`
(this app, unchanged).

| Old | Becomes |
|---|---|
| N4 `x_major_step` | `x_tick_interval` |
| N4 `font_family = helvetica` | `Helvetica` |
| `color_palette = publication` (Okabe-Ito in the old apps) | `colorblind` (note) |
| `export_width/height` without `export_units` | inches: `export_units = "in"` is added |
| N4 `color_selector_i` (+ `color_i` when `custom`) | colour of the i-th loaded sample |
| N4 `shape_i`; open shapes 1, 0, 2, 5, 6 | shape; 16 / 15 / 17 / 18 / 25 with Filled off |
| N4 `line_type_i`, `legend_label_i` | line type, display name (`qe_name_<id>` and `rv$display_names`) |
| N4 `exclude_var_i = true` | that sample is taken out of Select Samples |
| lca `notes_*` and `experiment_notes$*` | `nb_*` (`extra_cond` -> `nb_other_condition`, `date` -> `nb_date`) |
| lca `legend_label` | display name |
| lca `notes_show_caption`, `notes_caption_fields`, `notes_caption_size` | same ids (the notes caption, section 5) |

Ignored with a reason: N4 `x_minor_step`, `y_major_step`, `y_minor_step`, `plot_height`, `line_type` (global), `time_col_choice`,
`error_cap_type`, `use_od_filter` / `od_min` / `od_max`, `exclude_times`; lca `floating_labels`, per-sample `panel`, `leg_row`,
`leg_col`, `legend_position` values `direct` / `floating` / `matrix`, `error_display_mode` values that do not exist here
(spaghetti, quantile bands, jitter, combo), `axis_frame`, `exact_panel`, `legend_title`, facet controls; any key that is not an
input of this version. Per-sample keys are positional: the file's i-th sample is the i-th loaded sample; a different count is
reported (the extra keys are ignored). A file loaded before any data waits (`rv$pending_positional`) and is applied by position
when data is loaded.

## 4. Project themes

Named per-sample style sets in `themes/<name>.json` next to the app (`APP_DIR` if set, else the working directory; the option
`killcurve.themes_dir` overrides it, which the tests use). Section "Color Palettes": save, apply, delete (with confirmation) and
"auto-apply on data load" (`theme_autoapply`, default on; `theme_autoapply_on_load()` runs at the end of `ingest_data()` when a theme
is active). A theme holds colour (the quick-edit colour `rv$qe_colors` wins), shape, filled, line type and display name per sample,
plus the quick-edit colours and display names; applying styles the samples whose names match.

## 5. Notes

* **Caption on the figure**: `notes_show_caption`, the field checklist (`notes_caption_fields`), `notes_caption_size` (pt).
  `build_notes_caption()` joins "Label: value" with two spaces and a bar; the text is wrapped to the panel / export width and added
  below the error caption. It is on the main and the compare figure.
* **Exports**: notes page as PDF, PNG (300 dpi), HTML, JSON (next to the existing TXT / CSV); a PowerPoint notes slide
  (`notes_pptx_slide`).
* **Technical replicates**: when the long-format replicate column is called `well`, `well_id`, `wellid` or `technical_replicate`, a
  banner heads the statistics notes and a `caveat` column is added to the replicate-statistics, all-metrics and batch statistics
  CSVs, and a line to the notes HTML.

## 6. Batch export and parity frames

* **Batch export** (section "Plot Dimensions & Export"): tick the outputs (main figure, every analysis figure, composed figure,
  compare figure, summary / replicate-statistics / metrics CSVs, raw data wide CSV, notes HTML + JSON), the format (SVG / PDF / PNG /
  TIFF / JPEG), the dpi and the size (as in the app, or one size in mm for every single figure). Outputs that cannot be made are
  listed in `SKIPPED.txt`. `batch_export_build()` writes into a folder; the download archives it. "Download all figures" is
  unchanged (plus the compare figure).
* **Parity frames**: PowerPoint slides and GIF frames are built by `build_parity_frames()`: each is a fixed-panel gtable with the
  same panel and the same legend column width (the final frame's), pad rows for a legend taller than the panel
  (`expand_for_tall_legend()`), the final frame's width and height vectors when anything else still differs, and the axes of the
  whole data (`expand_limits`). The PowerPoint panel shrinks (the same in every slide) to fit the slide; GIF frames are centred in the
  chosen pixel size. An inside legend can still change corner between frames: use a fixed corner or an outside legend for builds.

## 7. Statistics option, night mode

* `stats_method = both` ("Welch + Wilcoxon side by side (BH within metric)"): the replicate-statistics table is Welch's plus
  `p_welch`, `p_wilcoxon`, `p_adj_bh_welch`, `p_adj_bh_wilcoxon` (Benjamini-Hochberg across the pairs of the metric), `p` stays
  Welch's; the omnibus table has the ANOVA and the Kruskal-Wallis row. Every single-test path (all-metrics table, brackets, dose
  response) uses Welch. Implemented by shadowing `compare_replicate_metric` / `compare_all_metrics` inside `server()` (no new
  statistics). **Merge note:** if the stats engine rewrites those two functions, only the two wrappers at the start of the v2.22
  server block and `compare_replicate_metric_both()` depend on their signatures.
* Night mode (`night_mode`): a CSS class on `<body>`; the figure, exports and settings are unchanged (not saved).

## 8. Merge notes

* New defaults are appended to the end of `LEGACY_BASE`; two existing assertions that pinned the tail of that list and the exact
  composer choice vector (`test_v221_figures.R`, `test_v211_analysis.R`) were loosened to "in order at their place" / "followed by
  later additions".
* Edits inside existing code: `generate_plot()` (notes caption), `compose_info()` (compare branch), `downloadAllFigures` (compare
  file), `fixed_figure()` and `CURVE_KINDS`, the settings load / save exclusion lists, the loader, `apply_imported_sample_aesthetics()`
  (now delegates to `apply_sample_aes()`), `ingest_data()` (one line: `theme_autoapply_on_load()`), `stats_notes_ui()`, the stats CSV
  writers, the methods text, the PowerPoint and GIF handlers (parity frames).
