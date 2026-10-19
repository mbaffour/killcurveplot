# Journal figure standards and the Inkscape workflow

This document is the reference behind the **Journal Figure Preset** section of
the app (v2.10). It records where each number comes from, what the preset
sets, and how to take the exported file through Inkscape into a thesis or a
manuscript without re-editing every figure.

## 1. What the preset produces

| Property | Preset value | Why |
|---|---|---|
| Width | the journal's column width (table below) | Journals reduce or reject figures that are not at print size; Nature asks for files "at about the size they are to be printed". |
| Height | 65 mm default (you choose) | Nature caps a figure at 170 mm so the legend fits beneath it. |
| Font | Arial | Nature: "standard fonts such as Arial or Helvetica". Arial ships with macOS and Windows, so an SVG that names it opens identically in Inkscape on both. |
| Axis titles | 7 pt | Nature: text between 5 and 7 pt; Cell Press: 6–8 pt. |
| Tick labels, legend | 6 pt | same |
| Panel label | 8 pt bold, top-left | Nature's panel-label style ("a", "b", ...). |
| Data lines | 0.75 pt | Nature: lines and strokes between 0.25 and 1 pt. |
| Axes, border, error bars | 0.5 pt | same |
| Points | 1.2 (≈1 mm) | legible at 89 mm without hiding error bars |
| Tick length | 1.2 mm | compact, proportionate to 6 pt labels |
| Colours | Okabe–Ito (Black, Orange, Sky blue, Bluish green, Yellow, Blue, Vermillion, Reddish purple) | the exact palette Nature's figure guide lists as colour-blind friendly |
| Legend | inside the panel in the emptiest corner (auto), no box border; outside right when there are more than 4 entries or the emptiest corner still overlaps data | journal panels rarely spend column width on an outside key; the corner with the fewest plotted points keeps the key off the data (top-left for a typical growth/lysis plot) |
| Spacing base size | 7 pt | margins and gaps scale with the theme's base size; the default 14 pt looks oversized next to 6–7 pt text |
| Y tick labels | 0.01, 0.1, 1 (trailing zeros trimmed) | reads like a published log axis |
| Error caption on the artwork | off | the measure and *n* go in the **Figure Legend Text** box (and the manuscript legend), not on the panel |
| Export | SVG, 600 dpi for rasters | SVG for Inkscape; 600 dpi line art for TIFF submissions (ASM: 300–600 dpi) |

Everything the preset sets is an ordinary control afterwards. Adjust, then
**Save Settings** to keep the result as a reusable style file for the next
figure.

## 2. Column widths

| Journal family | Single | 1.5 | Double | Max height | Source |
|---|---|---|---|---|---|
| Nature family | 89 mm | 120 mm (or 136) | 183 mm | 170 mm | Nature research figure guide, official ([building and exporting figure panels](https://research-figure-guide.nature.com/figures/building-and-exporting-figure-panels/), [extended data guidelines](http://research-figure-guide.nature.com/figures/extended-data-formatting-guidelines/)) |
| Cell Press (Cell, Mol Cell, Neuron ...) | 85 mm | 114 mm | 174 mm | 230 mm | secondary sources quoting cell.com/figureguidelines; Matter/Joule use 112/172 mm |
| ASM (JB, AEM, mBio, J Virol ...) | 84 mm (3 5/16 in) | — | 175 mm (6 7/8 in) | ≤ 9 in | ASM instructions to authors (older issues state the column widths; the current page says ≤ 7 × 9 in, TIFF or EPS, 300–600 dpi, Arial/Helvetica/Times) |
| PLOS | 132 mm (text column) | — | 190 mm (max 7.5 in) | 225 mm | [PLOS figure guidelines](https://journals.plos.org/plosntds/s/figures): width 2.63–7.5 in, Arial/Times/Symbol 8–12 pt |
| Science | 55 mm | 120 mm | 183 mm | 230 mm | Science instructions (2.25 / 4.75 / 7.25 in); not re-verified for this release |
| Custom | any | | | | for a thesis template, enter the text-block width |

Cell, ASM and Science pages block automated readers, so their numbers come
from secondary sources that agree with each other; open the journal's own
"figure preparation" page before you submit. Nature's values are official.

**Half of a double column** is for a two-panel figure: each panel is
(double − 5 mm gutter) / 2, so two SVGs placed side by side in Inkscape make
one full-width figure at native size.

## 3. Why the export devices changed

* **PDF** – the old `grDevices::pdf()` never embeds fonts (it only names the
  base-14 Helvetica) and errors on families such as "Arial" that are not in
  its PostScript font database. Nature requires embedded fonts. The app now
  uses `quartz(type = "pdf")` on macOS and `cairo_pdf` elsewhere; both embed
  the font. The test suite checks for `FontFile` objects in every PDF.
* **SVG** – `svglite` is still the writer, now with `fix_text_size = FALSE`.
  The default writes a `textLength` attribute on every `<text>` element.
  Inkscape honours it, so the moment you edit a label the glyphs are
  stretched or squashed to the old width. Without it, text reflows normally.
* **PNG / TIFF / JPEG** – `ragg` when installed (crisper text, same font
  lookup as svglite). TIFF keeps LZW compression.
* **Size units** – export size can be given in mm. The true-size preview and
  every report/PPTX export read the same value, so nothing drifts.

## 4. Inkscape loop

1. In the app: **Journal Figure Preset → Apply**, set the panel label, check
   the **True export size** preview (what you see is the physical figure at
   the chosen zoom), then **Download Image** (SVG).
2. Open the SVG in Inkscape. *Document Properties* shows the width in mm
   (89 mm = 336.4 px at Inkscape's 96 dpi). Do **not** rescale the drawing;
   if the figure must be narrower, re-export from the app at the new width so
   text stays 6–7 pt.
3. Edit text freely (double-click): it is real text in Arial, not outlines.
   Lines and points are plain paths with pt stroke widths.
4. For a standard layout (the main curve plus analysis panels in one or two columns) the app
   can do this step itself: the **Compose figure** tab lays the panels out at the journal's
   column width with a, b, c labels and exports the finished figure, so Inkscape is only
   needed for non-standard layouts or final touch-ups. Otherwise, assemble multi-panel
   figures by importing each panel SVG at its native
   size and aligning them. Nature's double column is 183 mm; two
   "half of a double column" panels plus the 5 mm gutter fit exactly.
5. Submit: **File → Save a Copy → Plain SVG** (Nature accepts plain .svg, not
   "Inkscape SVG"), or **Save a Copy → PDF** with "embed fonts" (default) for
   journals that want PDF/EPS, or **Export → PNG/TIFF at 600 dpi** for ASM.
6. Paste the **Figure Legend Text** from the app into the manuscript and edit
   it. It already states the measure (OD/A550), *n*, the error statistic
   (s.d. / s.e.m. / 95% CI), MOI, host, phage and medium from the Experiment
   Notebook.

## 5. What published lysis curves report

A survey of phage lysis / growth-curve figures in AEM, mSystems, Viruses and
BMC Microbiology shows the recurring pattern the preset is designed for:

* OD600 (or A550) on a log or linear axis against time in minutes or hours;
* mean of three biological replicates with SD (sometimes SEM) error bars, the
  replicate structure stated in the legend ("three biological replicates" vs
  "four technical replicates of one experiment");
* MOI, host strain and phage in the legend, not on the panel;
* one panel per phage or MOI in multi-panel figures, labelled a, b, c;
* colour-blind-safe palettes and no red/green pairs (Nature explicitly).

The Summary Statistics, Curve Metrics and Per-Replicate Metrics CSVs already
provide the numbers those papers report alongside the figure (lysis onset,
OD drop, μmax, doubling time, lag), see `docs/METRICS_REFERENCE.md`.

## 6. Line-width arithmetic (for the curious)

ggplot2's `linewidth` is not in pt. A stroke of 1 `linewidth` unit is
`.pt × 0.75 = 2.134 pt` (measured against svglite output). The app's new
controls take pt and convert with `pt2lw()`; the sliders that stay in ggplot
units say so in their label ("1 = 2.1 pt").
