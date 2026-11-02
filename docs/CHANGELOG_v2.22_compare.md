# v2.22 draft changelog: compare, presets, settings import, themes, notes, batch export

(Draft bullets for the supervisor to fold into CHANGELOG.md; details in docs/COMPARE_PRESETS_EXPORTS.md.)

- **Compare tab.** A second CSV (or Excel) file is ingested through the same detectors as the main data (`ingest_frame()`), prepared
  by the same `prepare_plot_data()` code and drawn with `build_plot()`. Overlay (dataset by line type or transparency, colours by sample,
  display names apply) or Facet (one panel per dataset, fixed panel size per panel); dataset labels, sample picker, "common samples only";
  true-size preview, download (SVG / PDF / PNG / TIFF / JPEG), a panel of the Compose tab and a file in "Download all figures".
- **N4 paper look** preset beside "Legacy look": the defaults of the N4 Lysis Curve Shiny App (axis text 20, stroke 1.5, no caption,
  10 x 8 in PDF at 300 dpi, x ticks every 30, N4 error-bar switches). Checked against a render of the N4 app; residual differences are
  documented. The end-label size input now goes up to 60.
- **Old settings import.** The loader reads the N4 app's flat JSON (positional per-sample keys mapped onto the loaded samples,
  `exclude_var_i` hides, inch sizes, `x_major_step` -> x tick interval) and the lysis-curve-app structure (`notes_*` -> `nb_*`,
  `legend_label` -> display names; floating labels, panel and matrix keys ignored). The status text and a notification report
  "N global, M sample settings applied, K ignored" with the reasons.
- **Project themes** (Color Palettes): named per-sample styles (colour including the quick-edit colour, shape, filled, line type,
  display name) saved as JSON in `themes/`; save, apply, delete, and auto-apply on load when sample names match.
- **Notes.** Notes caption on the figure (field checklist, size), notes pages as PDF / PNG / HTML / JSON, a PowerPoint notes slide;
  technical-replicate caveat banner and a `caveat` column on the statistics CSVs.
- **Batch export.** Choose the outputs (main, each analysis figure, composed, compare, statistics / metrics / raw wide CSVs, notes),
  the format, dpi and size. PowerPoint slides and GIF frames use parity frames: the panel, legend column and figure size are the same in
  every slide / frame, axes are those of the whole data.
- **Statistics.** New method "Welch + Wilcoxon side by side (BH within metric)": p_welch, p_wilcoxon and BH within the metric next to
  each other; Kruskal-Wallis next to the ANOVA.
- **Night mode** (page only).
- Tests: `tests/test_v222_compare.R`. Two older assertions that pinned the end of the defaults list and the composer choices were
  loosened for later additions.
