# Publication-Quality Audit & Notes

Audit of `Lysis_Curve_App_v27.R` against a publication-figure rubric, with the
safe, low-risk improvements that were applied and a list of deferred
recommendations that need review/testing before adoption.

> **IMPORTANT:** The changes in this branch were made **without an R runtime
> available** — they have **not been executed or rendered**. They are
> conservative, additive edits (new optional inputs, default-off behavior), but
> you must run the app and export a figure to confirm before merging.

---

## Audit summary (what already met the rubric)

The app was already strong on most publication criteria:

| Rubric item | Status before this branch |
|---|---|
| Colorblind-safe palette option | **Present** — exact Okabe-Ito palette (`colorblind`) and viridis/plasma (lines ~504–508) |
| Clean publication theme | **Present** — `theme_prism(border=TRUE)` / `theme_pubr()`, panel border, black ticks, minimal gridlines (default off) |
| Axis titles with units | **Present** — default X = "Time (minutes)", Y = "A550" (an OD/absorbance label) |
| Vector + high-res raster export | **Present** — PDF/SVG/PNG/JPEG/TIFF via `ggsave`, DPI default **300**, configurable size in inches |
| Error types SD / SEM / 95% CI | **Present** — with per-group `n` already computed in `prepare_plot_data()` |
| Sensible line/point sizing | **Present** — configurable line thickness, point size, stroke |

The single clear rubric gap was that **the error measure and replicate count
were not stated on the figure** — a hard requirement for most journals.

---

## Changes applied in this branch (SAFE, additive, default-off)

All changes preserve existing output exactly unless the user opts in. No
existing function signatures were changed; no statistical computation was
altered.

1. **Optional error-annotation caption** — new checkbox
   *"Annotate error type + n in caption"* in the **Error Bars / Shadow** panel
   (`show_error_caption`, default **FALSE**).
   - UI: added in the Error Bars section (`ui`, ~line 415).
   - Logic: `build_plot()` now composes an `err_caption` and passes it to
     `labs(caption = ...)` (~lines 1571–1599). When enabled and error bars are
     active, it renders e.g. `Error bars: mean ± SEM (n = 3).` or
     `Error bars: mean ± SD (n = 2–4).` (n range across groups/time). Honors the
     error multiplier (`2× mean ± SD`) when it is not 1.
   - Caption is `NULL` when the box is unchecked or error type is `none`, so
     default figures are byte-identical to before.

2. **Caption theming** — added `plot.caption` element to the theme block
   (~line 1571) sized relative to axis text (`max(8, axis_text*0.7)`,
   right-aligned, grey30). Only visible when a caption exists.

3. **Clear & Reset coverage** — added `show_error_caption = FALSE` to the
   defaults list in the `clearSettings` handler (~line 826) so Reset restores it.

### Why these are safe without running R
- Purely additive `labs(caption=)` and `theme(plot.caption=)` — both are
  standard ggplot2 args; unused/NULL caption is a no-op.
- New input id follows the existing naming/wiring conventions and is included in
  the Save/Load global-settings sweep automatically (it is not data-specific and
  not a per-sample prefix).
- No change to `prepare_plot_data()`, error math, palettes, or scales.

---

## Deferred recommendations (do NOT apply blindly — review + test in R)

These would improve publication quality further but carry behavior/methodology
risk, so they are flagged rather than applied:

1. **Colorblind-safe by default.** Default palette is `custom` (black-first
   position defaults). Consider defaulting `color_palette` to `colorblind`
   (Okabe-Ito) for out-of-the-box accessibility. *Risk:* changes default
   appearance of every existing user's plots.

2. **Palette recycling beyond 8 samples.** `colorblind` and `publication`
   palettes use `rep_len(...)`, which **repeats identical colors** when there are
   >8 samples — two samples become indistinguishable. Recommend either capping at
   the palette size with a warning, or interpolating
   (`grDevices::colorRampPalette`) / switching to viridis for large n. *Risk:*
   changes the actual colors assigned — a visual/interpretation change.

3. **TIFF compression.** For journal submission, TIFF should use lossless LZW
   (`ggsave(..., compression = "lzw")`). Cannot be added unconditionally because
   that arg is invalid for PDF/SVG/PNG; needs a device-conditional branch in the
   `downloadPlot` handler. *Risk:* device-arg mismatch if mis-wired — test each
   format.

4. **Font embedding for PDF.** For camera-ready PDFs, embed fonts
   (`cairo_pdf` device or `extrafont`/`showtext`), especially when non-`sans`
   families (Garamond, Palatino, "Helvetica Neue") are selected — these may not
   render on the reviewer's machine otherwise. Needs an extra dependency and
   testing per-OS.

5. **Y-axis unit clarity.** Default Y label `"A550"` is fine for absorbance at
   550 nm, but consider offering a units-explicit preset (e.g. `"OD600"`,
   `"Absorbance (A550)"`) since the rubric emphasizes units. Trivial but
   user-facing wording — left to the author.

6. **Caption vs. legend duplication.** If a figure legend already states the
   error measure, the new caption may be redundant; it is opt-in precisely so the
   author controls this.

7. **SEM/CI with n = 1.** When a group has a single replicate, SEM/CI are 0 and
   the caption would read `n = 1`. Statistically these error measures are
   undefined for n = 1; consider suppressing error display (already effectively
   0-width) and/or noting minimum-n in the caption. Methodology-adjacent — left
   as-is.

---

*Audit and edits by Claude (Fable 5). Unverified — no R runtime in the audit
environment. Run the app and export a test figure before merging.*
