# Figure styling and text markup (v2.22)

This page describes the figure-styling controls added in v2.22, most of them ported from the two companion apps
(`Lysis Curve Ap 26.03.13.R` and the N4 paper app). Every new control defaults to what the figure looked like before
v2.22: a figure made with the startup settings, the Legacy look or any older settings file is byte-identical to the
v2.21 figure (the test suite compares the SVG exports). Nothing here adds a panel letter; one appears only when you type it.

## Text markup

Every piece of figure text accepts the same small markup:

| Type | You get | Notes |
|---|---|---|
| `A_{550}` | A with a subscript 550 | any text inside the braces |
| `10^{-7} PFU` | 10 with a superscript -7, then the unit | a leading `^{2}H` gets an empty base |
| `*mltG*` | *mltG* (italic) | for gene and species names |
| `**bold**` | **bold** | |
| `\*` | a literal asterisk | |

They compose: `Δ*bshC*`, `OD_{600} *x*`, `Time_{h}`. A text without markup is drawn exactly as before.
A text that mixes a line break with markup cannot be typeset (plotmath has no line break) and is drawn as typed.

**Where it works:** plot title, subtitle, the X and Y axis labels (also the labels of the analysis figures), the legend
labels (the names typed in the Quick edit strip), the legend title, end labels and direct labels, marker labels, the
threshold label, the facet strip labels (panel names), the matrix-legend labels and its corner title, the report title
(the HTML heading and the PDF cover) and the drafted Figure Legend Text (there the markup is written as plain text with
Unicode sub- and superscripts, `A_{550}` becoming A₅₅₀). Tables, CSV files and file names keep what you
typed: a sample renamed `Δ*bshC*` shows those characters in a table. The figure legend text and the report cover are
the only places that convert it.

**The wavelength subscript stays.** With *Typeset wavelength as subscript* ticked (the journal preset ticks it) `A550` and
`OD600` still become A₅₅₀ as in v2.16. If the same label also carries markup (`A550 *relative*`) both are
typeset. A label without markup takes exactly the v2.16 path.

**Buttons and shortcuts.** A small row of buttons (x₂, x², *I*, **B**) sits under the title and subtitle boxes and in
the Quick edit strip (it acts on the name or axis-label box you last clicked in). Highlight characters, then click a button
or press Ctrl+= (subscript), Ctrl+Shift+= (superscript), Ctrl+I (italic), Ctrl+B (bold). The button wraps the selection in
the markup (`_{...}`, `^{...}`, `*...*`, `**...**`); the same click again, with the wrapped text still selected, unwraps it.
The shortcuts work in the figure-text boxes only (title, labels, legend title, report title, sample names, marker and
threshold text, panel names, matrix row and column boxes), not in every text box of the page.

(The companion app's buttons wrote Unicode sub- and superscript characters. These write the markup instead, because Arial
has no glyph for most subscript letters and the markup is drawn as real sub- and superscript text in the SVG, PDF and
PowerPoint exports. Typed Unicode characters still work, of course.)

The on-canvas editing popover writes into the same boxes, so it inherits the markup.

## Error display and error types

*Error Bars / Shadow* now has a **Display mode** for the main curve:

| Mode | Draws |
|---|---|
| Error bars | as before (needs an error type other than None) |
| Shadow / ribbon | as before |
| Spaghetti | every replicate as a faint line behind the mean (trace alpha) |
| Quantile bands | the interquartile range inside the 2.5-97.5 % band of the replicates (two alphas) |
| Jitter | the replicate values as points (alpha, size, jitter width in time units) |
| Combo | a lighter ribbon plus the replicate traces |

Spaghetti, quantile bands and jitter draw the replicates themselves, so they do not read the SD / SEM selector and work
with error type None; bars, shadow and combo with None draw nothing, as before. *Show individual replicate values* stays
as the alias of the jitter mode (open circles behind the mean); with the Jitter mode chosen the points are drawn once.

Two **error types** join SD, SEM and 95 % CI: **Range** (minimum to maximum of the replicates) and **IQR** (25th to 75th
percentile). Both are asymmetric about the mean; the multiplier scales the distance from the mean. They work for the bars,
the shadow and the combo mode, on a log axis (the lower end is drawn down to the axis floor, as for the other types), and in
the metric summary (whiskers from minimum to maximum, or quartile to quartile). The figure caption says "range (minimum to
maximum)" or "interquartile range" instead of "mean ± ...". The dose-response, explorer, time-to-threshold and OD-at-times
figures draw the half width ((max - min) / 2, IQR / 2) symmetrically; the landmarks figure's band falls back to the SEM.

All modes work with the fixed panel size, the exports (SVG, PDF, PNG, TIFF, JPEG) and the cumulative PowerPoint and GIF builds.

## Panels and the axis frame

The **Panels** section (after Axis Settings) holds:

* *One panel per group* (moved here from Variable Styling) and the **panel source**: the Group typed for each sample in
  Variable Styling (default), a **panel name typed per sample** in this section, or, for a long-format file, **another
  column of the file** (the first non-blank value of the column for each sample). A blank entry goes to "Other".
* **Panels per row** (blank = the automatic rule), **axis scales** (shared, free y, free x, free both), the **panel header
  style** (plain text as before, boxed strip, none) and its size. Panel names accept the markup (parsed strips; plotmath
  cannot wrap, so a header with markup is not wrapped).
* In the fixed panel size every sub-panel is pinned and the panel area (the size you typed) is shared by the sub-panels, so
  the figure keeps the panel size you chose; free scales add their axis labels around it. (This is how v2.18 already behaved;
  the companion app gave every sub-panel the full size instead.)
* **Axis frame**: Automatic (default: the frame the figure has always had, a box, with the extra axis lines when Advanced Tick
  Customization is off), Box, L-shaped (left and bottom lines only) or None (tick marks only). It is independent of the
  Advanced Tick Customization box. The journal preset and the Legacy look leave it on Automatic, which is what they draw today.

## Legend options

* **Legend title** (blank = none, as before), **legend text** style (plain / bold / italic / bold italic) and **wrap width**
  (characters; blank = no wrapping) under Figure Layout. The same title and labels go on every scale that draws a key.
* **Direct labels at line ends** is a choice of the legend position (Quick edit strip). The legend box is hidden and each
  line is named, in its own colour, at its right end, with the **display name** from the strip (the *Show end-of-line labels*
  checkbox now uses the display names too: it used to print the raw column name). The figure gets a right margin wide enough
  for the longest label. Analysis figures have no end labels, so they keep an ordinary key.
* **Matrix (grid) legend** is another choice of the legend position: rows (a strain) by columns (an MOI), one swatch per
  cell drawn in that sample's colour, line type and point shape. Type a Row and a Column for each sample under Figure Layout
  (a sample missing either is left out); set a corner title, placement (right, left, top, bottom or inside the panel), swatch
  length, gaps and an optional box. It is attached in the gtable pipeline after the panels are pinned, so the fixed panel
  keeps its size and the figure grows around the legend; the SVG / PDF / PNG exports, the fit-mode figure, the PowerPoint and
  GIF builds draw it. The dashboard thumbnail and a non-fixed *Compose figure* panel show the figure without it.
* **Hide lines** (Line & Point Settings) leaves the points and the error bars; the legend key becomes a point.
* **Open diamond** and **open down-triangle** point shapes (hollow whatever the Filled box says; codes 123 and 125).
* *Not ported:* the companion app's floating, drag-to-place legend labels. Here the preview is an inline SVG, not an image
  the page can lay draggable labels over, so the placement maths would have to be rewritten against the SVG; direct labels
  and the inside legend positions cover the use.

## Axes

* **Y tick interval** and **minor interval** for linear and sqrt axes, and an **X minor interval** (next to the X major
  interval), all blank = automatic. Breaks are anchored below the lower limit; minor ticks never sit on a major one; an
  interval that would draw more than 500 ticks is ignored with a note; the prism minor-tick guide is attached only when two
  or more minor ticks fall inside the range (that guard fixes a zero-length-unit crash). The guard also protects the automatic
  X ticks.
* **Time unit of the figures** (Axis Settings): minutes to hours or hours to minutes. It is a **display conversion**: the
  file's times are multiplied for the plotted and exported curves, every analysis figure, the curve and replicate metric
  tables (lysis onset, time of the peak, doubling time ... are in the new unit, labelled with it) and the exports; the unit
  word of the X-axis label follows ("Time (minutes)" becomes "Time (hours)"; the typed label is unchanged). The data
  themselves stay in the file's unit. The time filter, the per-sample time windows and *Exclude time points* still use the
  file's unit; axis limits, tick intervals, marker times and the times typed for *OD at chosen times* use the converted unit.
  Bootstrap and fit tables are computed from the converted times.
* **Marker label height**: the Top / Middle / Bottom position can be placed by the axis limits (top = 0.95 x the upper limit,
  bottom = 1.05 x the lower limit, middle = geometric mean on a log axis) instead of a fraction of the plotted data.
* **Threshold line** width in pt (blank = the axis-line weight) and a custom HEX colour; its label takes markup.

## Palettes, marker colours, line types

* **Publication** is now the Okabe-Ito set (as in the N4 paper app); the ColorBrewer Set1 colours it used to be are the
  **Set1** palette. A settings file written before v2.22 (it has no `styling_schema` key) that says "publication" is loaded as
  "set1", so it draws what it always drew; files saved from v2.22 carry `styling_schema = 222`.
* Marker colours gain Purple and Orange, marker line types gain TwoDash, the gridline colours gain Light Red.

## More figures

* **Rate of OD change (dOD/dt)** (Analysis figures, the figure composer, the HTML and PDF reports and the download-all
  archive): d OD / dt per replicate by the same centred rolling regression as the specific growth rate (window = the
  μmax smoothing window, base R), mean ± SD across the replicates, a dashed zero line. Above it the culture grows, below
  it lyses; a zero crossing marks the peak.
* **Metric summary**: sample order (legend order, alphabetical, by value ascending or descending: by the mean of the first
  metric shown, samples without a value last), **horizontal bars**, **custom colour per bar** (type `name = #HEX` lines; the
  display or the file name), and a **single-metric large mode** (one metric: the panel is 1.5 x larger in the fixed panel size
  and the points 1.4 x).
* **OD heatmap** colour scale: viridis (default), inferno, plasma or magma.
* Three one-click looks next to the presets (Journal Figure Preset): **Band style** (thin lines, no points, a shaded band, L
  axes; keeps SD / SEM / 95 % CI if one is chosen), **Presentation slide** (10 x 7.5 in PNG, 18-22 pt text, thick lines) and
  **PDF vector** (7 x 5 in PDF, 11-14 pt). They only set ordinary controls, so you can adjust every value afterwards, and they
  are not saved into a settings file.

## Settings files

Saved settings gain the new global controls and, per sample, `panel`, `leg_row` and `leg_col` (the Panels and matrix-legend
boxes), keyed by sample name like the other sample styles. Files from older versions load as before (an absent key leaves the
control alone).

## Known limits

* Markup in a sample's name appears literally in tables, CSV files and file names.
* Plotmath text cannot contain a line break: a legend label wrapped with a markup entry is left unwrapped.
* The matrix legend is drawn in the main figure, its exports and the cumulative PowerPoint / GIF builds, not in the dashboard
  thumbnail or in a non-fixed composed figure.
* Range and IQR error types are drawn asymmetrically for the main curve and the metric summary; the other analysis figures
  draw their half width symmetrically (the landmarks figure keeps the SEM).
