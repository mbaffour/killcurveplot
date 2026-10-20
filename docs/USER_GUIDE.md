# User guide: lysis-curve figures and statistics

This guide is for finishing phage lysis-curve figures and their statistics for a thesis or paper. It is organised by task. Installation is in the [README](../README.md). Control names are the labels shown in the app.

## 1. What a session looks like

1. **Start the app** with `Run Lysis Curve Plotter.command` (macOS), `Run Lysis Curve Plotter.bat` (Windows) or `./run.sh` (Linux).
2. **Load data.** Click **Browse...** next to **Choose CSV File**, or pick a scenario beside **Load Demo Data** (for example Lysis curve (3 samples) or MOI series (dose-response)). A scenario also sets the x-axis label and the reference sample.
3. **Check the data.** The green data summary box under the file picker states how many replicate columns were found for each sample. Below it, the QC flag list shows any of CEILING, NEGATIVE, GAP or FLAT_REFERENCE. Clean data shows nothing.
4. **Check the figure.** The app starts in the journal standard: a fixed 65 x 65 mm square panel, Arial with 7 pt axis titles and 6 pt tick and legend text, 0.75 pt data lines, Okabe-Ito colours, no panel letter and SVG export.
5. **Adjust.** Set **Legend Position:** in **Axis Settings**, colours in **Color Palettes** or per sample in **Variable Styling**, and error display in **Error Bars / Shadow**.
6. **Download.** In **Plot Dimensions & Export**, keep **Format:** as SVG and click **Download Image**.

## 2. Data formats the app reads

The layout is detected automatically. Check the data summary box after every load.

- **Wide, duplicate column names.** Give replicate columns the same name (`WT`, `WT`, `WT`). The app pools them for the mean, SD and n. Versions up to v2.7 used only the first column of a repeated name.
- **Wide, stacked time blocks.** If a plate export repeats the sample headers in a second block of rows, each block adds replicates. Confirm the replicate count in the summary matches what you expect.
- **Long format with a replicate column.** Use time, group and value columns. A column named `replicate`, `rep` or `well` gives explicit replicate IDs. Without one, the row order within each sample and time point is used, so each replicate's rows must keep the same relative order at every time.
- **HH:MM:SS times.** Times written as `H:MM:SS` or date-time are converted to minutes. Set the x-axis label in **Axis Settings** to match.
- **Instrument preambles.** Header lines above the table are skipped, and the separator (comma, semicolon or tab) is detected.
- **OVER, OVRFLW and #SAT cells.** A saturation marker in a numeric column becomes a missing value for that cell only. The column is kept, and a notification reports how many cells changed. A missing reading in a regular series is then flagged GAP.
- **Decimal commas.** Set **CSV format:** to **European (semicolon-separated, , decimal)**. Auto-detect, the default, handles most files.
- **Blank subtraction.** The app assumes the data are already blank-subtracted. If a media OD remains, enter it in **Subtract blank/media OD (optional):**. It is subtracted before plotting, statistics and metrics, and recorded in the report. Readings at or below zero are flagged NEGATIVE.
- **Known limitation.** A Gen5 temperature column (`T° 600`) is still read as a sample. Remove it from the CSV before loading.

## 3. One figure for a paper

1. Expand **Journal Figure Preset**. Choose **Journal:** and **Width:** (Single column, 1.5 columns, Double column, or Half of a double column for one panel of two).
2. Set **Height (mm):** (65 by default), and a **Panel label (optional):** only if the figure needs one.
3. Click **Apply journal preset**. This sets the sizes, font, colours, legend, SVG export and true-size preview. **Legacy look (v2.9 defaults)** restores the old look.

**Fits indicator.** The size bar gives the panel and figure size and says whether the figure fits a single column, two across a double column, or three across. With the legend inside, a 65 mm panel gives a figure of about 75 mm, which fits a single column, and two such figures fit across a 183 mm double column.

**Legend inside or outside.** The preset selects **Inside: emptiest corner (auto)**, which places the key in the corner with the fewest plotted points. It moves to the right, outside the panel, when there are more than four entries or the corner still overlaps data. Other positions are under **Legend Position:**.

**Subscripts.** With **Typeset wavelength as subscript** ticked (the default), `A550` and `OD600` in the y label are drawn as A₅₅₀ and OD₆₀₀. Untick it for plain text.

*Limitation:* the subscript digits are drawn by R's plotmath at 70 % of the axis-title size, which is 4.9 pt at 7 pt, just under Nature's 5 pt floor. Set the axis-title size to 7.2 pt or more, or untick the box if the journal checks.

**Panel letters** are drawn only when you set a **Panel label:**. Put the MOI, host and phage in the **Figure Legend Text**, not on the panel.

**File names** carry no timestamp. They use the export size and label, for example `lysis_curve_a_89x65mm.svg`, or `lysis_curve_main_89x65mm.svg` with no label.

**Finishing in Inkscape**

1. Apply the preset, check the size bar and preview, then download the figure as SVG.
2. Open the SVG in Inkscape. *Document Properties* shows the width in mm (89 mm is 336.4 px at 96 dpi). Do not scale the drawing; if it must be narrower, re-export from the app.
3. Edit text by double-clicking. It is real Arial, not outlines.
4. Assemble multi-panel figures at 100 %, or use the Compose figure tab (section 4).
5. Use **File > Save a Copy** as Plain SVG (Nature accepts plain SVG, not Inkscape SVG) or as PDF with fonts embedded. For ASM, export TIFF at 600 dpi. Paste the **Figure Legend Text** into the manuscript and edit it.

## 4. A multi-panel figure

**In the app: the Compose figure tab** (after Dashboard).

- **Panels (composed in this order):** tick the main lysis curve and any analysis figures. They are laid out in the order listed.
- **Columns:** 1 or 2.
- **Figure width:** the double or single column of the journal chosen in Journal Figure Preset, or a custom width in mm.
- **Height of each panel row (mm):** 60 by default. The preview warns if the total exceeds the journal maximum.
- **Add panel letters** is on by default. Untick it for no letters, and set the letter size and case beside it.
- **Download composed figure** writes the file in the format chosen under **Format:**.

Panels render at native size, so text stays at 6 to 8 pt. Apply the journal preset first. A panel that is not available for the selection, such as the dose-response with one MOI level, is left out and listed. The composer needs the `patchwork` package.

**In Inkscape instead.** Import each panel SVG at its native size and align them. Panels from one session share a panel size and align without resizing. Two "half of a double column" panels plus the 5 mm gutter fill a 183 mm double column exactly.

## 5. Analysis figures and what each shows

Open the **Analysis figures** tab, choose a **Figure:**, set its **Panel label:** if needed, and click **Download figure**. Every analysis figure uses the same error measure (SD, SEM or 95 % CI) and sample order as the main figure.

- **Metric summary:** one facet per metric (**Metrics (one facet each):**). Open circles are replicates, the bar is the mean and whiskers are ± SD. With a reference set and **Mark significance vs reference on the metric summary** ticked, Holm-adjusted stars appear.
- **Specific growth rate vs time:** μ(t) = d ln(OD)/dt per replicate, as a mean ± SD band. Lysis shows as a negative rate.
- **Normalized OD vs time:** each mean curve divided by the reference at every time point, or by its own t = 0 value (**Relative to:**).
- **Replicate overlay:** each replicate as a thin line with the mean in bold, showing the variance the error bars summarise.
- **Curve landmarks:** lag end, μmax window centre, peak, lysis onset, half of peak and trough on each mean curve, so reported numbers can be checked by eye.
- **MOI dose-response:** a metric against log10 MOI. MOI is read from names such as `Phage MOI 10`, or entered as `sample = MOI` lines in the MOI box. It needs at least two levels.
- **OD heatmap:** samples by time, for screens with many samples.
- **Metric explorer:** one metric against another, one point per sample (defaults: lysis onset and OD drop). Replicate values, labels and log axes are optional. Samples missing either metric are left out.

**Groups and facets.** In **Variable Styling**, give each sample a **Group** (for example the phage or medium). With two or more groups, tick **One panel per group (main figure)**. Samples without a group go to "Other".

**Per-sample time windows.** Under **Time Point Filtering**, the per-sample table gives each sample its own *from* and *to* time (blank means no limit). The windows apply to figures, statistics, metrics and exports, and are listed in the report and legend text.

## 6. Statistics and numbers for the text

**Where.** The **Statistics** tab has four collapsible sections: **Summary statistics** (mean, SD, n, SEM and 95 % CI half-width), **Curve metrics (mean curve)** with the definition of each metric, **Per-replicate metrics (mean ± SD)** and **Replicate comparison**. Each has its own CSV download.

**Curve metrics** are computed on the mean curve, giving one number per sample with no replicate uncertainty. The main definitions:

- **μmax:** the largest centred rolling-window slope of ln OD (5-point window by default). **Doubling time** is ln 2 / μmax.
- **Lag phase:** the first time the rolling slope reaches 10 % of μmax.
- **Lysis onset:** the interpolated time the mean falls below the peak by the threshold (5 % by default; **Lysis onset: decline from peak that counts as lysis (%)**).
- **OD drop** is the peak minus the post-peak minimum. **AUC** is the trapezoidal area.

Metrics that do not apply are reported as NA, never as an invented value.

**Per-replicate metrics** run the same engine on each replicate and report mean ± SD across replicates. These are not the same numbers as the mean-curve metrics, so state which one you report.

**Reference sample.** Set **Reference sample (infection metrics):** to your uninfected control to add relative growth, infection strength (1 − AUC ratio), relative μmax, relative max OD and lysis-onset Δ from lag.

**Peak-detection smoothing window** (off by default) locates the peak on a smoothed curve, which steadies the peak-anchored metrics against replicate noise.

**Replicate comparison.** Set **Statistics:** in the Analysis figures tab to Welch t / ANOVA + Tukey or Mann-Whitney / Kruskal-Wallis. Each sample is compared with the reference (all pairs if none is set), with Holm-adjusted p, Hedges' g and the 95 % CI of the difference. Groups with fewer than two values are excluded, and n < 3 is flagged. Three replicates give little power, so a non-significant result is not evidence of no difference.

**Methods text.** In the Statistics tab, **Draft from current settings** writes a methods paragraph. It fills only an empty box unless **replace existing text** is ticked. Edit it, then **Download methods (.txt)**.

**Limitations, stated plainly**

- **Lysis onset rule.** A single point 5 % below the peak is not lysis. Lysis is detected only when the decline exceeds both the threshold and three times the curve's noise, sustained over two consecutive points, so a noisy uninfected control reports NA. A curve that falls by more than half its peak is reported even with fewer than two points after the peak.
- **First-peak mode.** **Peak used for lysis metrics:** set to **First peak before a > 10 % decline** anchors the lysis metrics on the first peak, for regrowth and resistance curves. The 10 % threshold is deliberate, because plateau noise of about 4 % can look like a decline. Use a 3-point smoothing window with it. The `first_peak_od` metric uses a 5 %-of-range floor with no noise term, so a small early bump can qualify on noisy data.
- **Growth-model fits.** The logistic, Gompertz and Baranyi fits are engine functions in v2.18 and are not drawn or exported by the app. Lag is measured from the first fitted point (t0), not from time zero, so it shifts if the first reading is after time zero. Compare models by AIC, not by μmax.

## 7. Reports, notebook and bundles

- **HTML Report** is one self-contained file that opens in any browser. It holds the figure at export settings, the Experiment Notebook record, the summary and curve-metric tables with definitions, data provenance (file, format, time column, filters, error settings, blank value, per-sample windows), QC flags, key settings, and R and package versions. With **Include analysis figures and statistics** ticked, it adds the analysis figures and replicate-statistics tables.
- **PDF Report** has the same content as a multi-page PDF, with no LaTeX or pandoc needed.
- **Experiment Notebook** fields cover experiment, host, phage, MOI, medium, temperature, passage, inducer and custom keys. **Download notebook (.txt)** writes the filled fields, the Figure Legend Text and the methods text. **Download notebook row (.csv)** writes one row for a master spreadsheet.
- **Download figure bundle (.zip)** packs the main figure as SVG and PDF, the composed figure, `settings.json`, `source_data_tidy.csv`, `summary_statistics.csv`, `curve_metrics.csv`, `figure_legend.txt`, `methods.txt` and `README.txt`. The README records the app version, date, data source, figure size and fonts.
- **Download all figures (.zip)** writes an SVG and a PNG of every figure that can be drawn for the current data, plus the composed figure, legend and methods. Figures that cannot be drawn are listed with reasons in `SKIPPED.txt`.

## 8. Reproducibility checklist

- [ ] Click **Save Settings** and keep the `.json` with the figure. **Load Settings File** restores the style later.
- [ ] Keep the original CSV, the settings file and the report together.
- [ ] For submission, download the figure bundle and keep its `README.txt`.
- [ ] Check the report's provenance: blank value, filters, per-sample windows, lysis threshold, reference and error settings.
- [ ] Explain any QC flag in the methods (for example CEILING), or fix the data.
- [ ] Draft the methods text after the final settings, then edit it.
- [ ] Regenerate the report and figures after any change, since outputs reflect the current plot.
- [ ] Record the R and package versions from the HTML report.

## Where to look next

- [README](../README.md): installation, quick start and the full control list.
- [Figure standards](FIGURE_STANDARDS.md): journal widths, sources and the Inkscape loop.
- [Metrics reference](METRICS_REFERENCE.md): formulas and detection rules for each curve metric.
- [Fits and extraction](FITS_AND_EXTRACTION.md): growth-model fits, time-to-OD and all-metrics statistics.
- [Ported from the Lysis Curve App](PORTED_FROM_LYSIS_CURVE_APP.md): engine and CSV-reader changes.
- [Changelog](../CHANGELOG.md): v2.10.0 to v2.18.0.
- [Gallery](gallery/gallery.png): every figure from the demo data.
