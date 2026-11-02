# v2.22 figure styling (draft changelog entries)

Draft bullets for the supervisor to fold into CHANGELOG.md and README.md (this branch does not touch either, nor APP_VERSION).
Details: docs/STYLING_AND_MARKUP.md. The default figure is unchanged: the SVG exports of the startup settings, the Legacy look
and the older option combinations are byte-identical to v2.21 (tests/test_v222_styling.R compares them against `git show 3c1f50e`).

## Added

- **Text markup everywhere** (ported from the companion app): `_{sub}`, `^{sup}`, `*italic*`, `**bold**`, `\*` in the plot title,
  subtitle, axis labels (main and analysis figures), legend labels (the Quick edit names), legend title, end and direct labels,
  marker labels, threshold label, panel (facet strip) names, matrix-legend labels, the report title and, as plain text with Unicode
  sub- and superscripts, the drafted figure legend text. Highlight-and-click buttons (x2, x^2, I, B) under the title and in the
  Quick edit strip, plus Ctrl+= / Ctrl+Shift+= / Ctrl+I / Ctrl+B in the figure-text boxes. The wavelength auto-subscript
  (`A550`) stays and combines with markup. The canvas-edit popover inherits it (same inputs).
- **Error display modes** for the main curve: spaghetti (every replicate), quantile bands (IQR inside 2.5-97.5 %), jitter,
  combo, next to error bars and shadow; own alpha / size / width controls; they do not need an error type. `show_replicate_points`
  stays as the alias of the jitter mode. Works with the fixed panel, the log floor, every export and the PowerPoint / GIF builds.
- **Error types Range (min-max) and IQR**, asymmetric about the mean (`error_half_width()` gained `lower` / `upper`); the main
  curve, the metric summary, the caption and the figure legend text know them.
- **Panels section**: panel source (typed Group, a panel name per sample, or a column of a long file), panels per row, axis
  scales (shared / free y / free x / free), header style (plain / boxed / none) and size, parsed (markup) headers; the fixed panel
  size pins every sub-panel. **Axis frame** control (Automatic / Box / L / None) independent of Advanced Tick Customization.
- **Legend options**: title, text style, wrap width; **Direct labels at line ends** as a legend position (uses the display names,
  makes room for the labels; the *Show end-of-line labels* checkbox now prints display names instead of raw column names);
  **Matrix (grid) legend** (strain x MOI) with per-sample Row / Column boxes, five placements, in the fixed panel, fit mode,
  exports, PowerPoint and GIF; **Hide lines** (points and error bars only, legend key a point); open diamond and open
  down-triangle shapes. Floating drag-to-place labels were not ported (inline SVG preview).
- **Axes**: linear / sqrt Y major and minor tick intervals, X minor interval (blank = auto) with the `interval_breaks()` /
  `prism_guide_if_safe()` guard (the zero-length-unit crash when fewer than two minor ticks are in range); **time-unit conversion**
  (minutes <-> hours) of the plotted and exported times and the time metrics with their labels (data stay in file units);
  marker label height by the axis limits; threshold line width and custom HEX.
- **Palettes**: `publication` is now Okabe-Ito (the N4 app's meaning); the old ColorBrewer set is `set1`, and a pre-v2.22 settings
  file that says `publication` is migrated to `set1` (new `styling_schema` key). Marker colours Purple / Orange, TwoDash marker
  line type, Light Red gridlines.
- **More figures**: Rate of OD change (dOD/dt) analysis figure (per replicate, mean +/- SD, zero line; also a composer panel, in the
  reports and the download-all archive); metric summary sort order (legend / alphabetical / value asc / desc), horizontal bars,
  custom per-bar colours, single-metric large mode; OD-heatmap colour scale (viridis / inferno / plasma / magma).
- **One-click looks**: Band style, Presentation slide, PDF vector, next to the journal preset and the Legacy look (they only set
  existing inputs).

## Changed

- The *Error Bars / Shadow* section is reorganised: the display mode no longer hides behind the error type; the facet checkbox
  moved from Variable Styling to the Panels section; per-sample `panel`, `leg_row`, `leg_col` are saved with the sample styles.
- 14 sidebar sections (Panels follows Axis Settings; tests/test_v221_ui.R and the LEGACY_BASE tail check of
  tests/test_v221_figures.R were adjusted accordingly).

## Tests

- New tests/test_v222_styling.R (markup conversions, error modes and widths, panels, frame, legend, matrix legend, intervals,
  time unit, palettes and migration, dOD/dt, metric extras, heatmap palette, byte-identical default figure).
