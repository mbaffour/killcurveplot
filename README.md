# Lysis Curve OD Visualization App — User Guide

> **Docs:** [Metrics reference (formulas + sources)](docs/METRICS_REFERENCE.md) · [Changelog](CHANGELOG.md) · [Audit (2026-09)](AUDIT-2026-09.md) · Tests: `Rscript tests/test_v28.R` (101 checks), `Rscript tests/test_metrics_parity.R` (reference-free checks always run; reference-comparison checks need a compatible sibling clone of [lysis-curve-app](https://github.com/mbaffour/lysis-curve-app) — see [docs/METRICS_REFERENCE.md](docs/METRICS_REFERENCE.md#tests))
>
> **Looking for the full analysis platform** (Analysis tab, 30+ metrics, statistics, heatmaps, curve fitting, virulence metrics)? That's the companion app: [mbaffour/lysis-curve-app](https://github.com/mbaffour/lysis-curve-app). This repo is the lightweight plotter.

## Quick start — double-click to run (no command line)

If you have [R](https://cran.r-project.org/) installed, you don't need the terminal:

| OS | Double-click |
|----|--------------|
| **Windows** | `Run Lysis Curve Plotter.bat` |
| **macOS** | `Run Lysis Curve Plotter.command` |
| **Linux** | `./run.sh` |

The **first run** installs the required R packages (a few minutes); after that the app opens in your web browser instantly. You can also run it directly with `Rscript run_app.R`.

## What This App Does

This is an interactive tool for visualizing Optical Density (OD) time-series data. You upload a CSV file containing your experimental measurements, and the app generates publication-quality plots with full control over colors, shapes, error bars, axis scales, labels, and more.

You can export your plots as PDF, PNG, JPEG, SVG, TIFF, PowerPoint, or animated GIF — and (new in v2.8) export the underlying summary statistics and curve metrics as CSV, or generate a complete single-file **HTML or PDF analysis report** containing the figure, statistics tables, metric definitions, data provenance, and session info.

Not sure where to start? Click **"Load Demo Data"** under the file picker to explore the app with a built-in example dataset (3 samples × 3 replicates).

---

## Step 1: Install R and RStudio

You need two free programs. Install them in this order.

### Install R (the language)

1. Go to **https://cloud.r-project.org**
2. Click the link for your operating system:
   - **Windows**: Click "Download R for Windows" → "base" → "Download R-4.x.x for Windows". Run the installer and accept all defaults.
   - **Mac**: Click "Download R for macOS". Choose the `.pkg` file that matches your Mac (Apple Silicon or Intel). Open the file and follow the prompts.
3. You do not need to open R directly — RStudio will use it behind the scenes.

### Install RStudio (the interface)

1. Go to **https://posit.co/download/rstudio-desktop/**
2. Click the big "Download RStudio" button. It should auto-detect your operating system.
3. Run the installer and accept all defaults.
4. Open **RStudio** (not R). You should see a window with several panels.

---

## Step 2: Install Required Packages

Packages are add-ons that the app depends on. You only need to do this once.

1. Open **RStudio**.
2. In the bottom-left panel (called the **Console**), you'll see a blinking cursor next to a `>` symbol.
3. Copy and paste the following command into the Console, then press **Enter**:

```r
install.packages(c(
  "shiny", "tidyverse", "ggpubr", "scales",
  "ggrepel", "ggprism", "svglite", "jsonlite",
  "officer", "rvg", "gifski", "base64enc", "gridExtra", "ragg"
))
```

4. Wait for the installation to finish. This may take several minutes. You'll see lots of text scrolling by — that's normal. When it's done, you'll see the `>` cursor again.

If you see any errors mentioning a specific package, try installing that package individually:

```r
install.packages("package_name_here")
```

**Note:** The `officer`, `rvg`, `gifski`, `base64enc`, `gridExtra` and `ragg` packages are optional. Without them, the PowerPoint, GIF, HTML-report and PDF-report buttons (respectively) will be disabled, and PNG/TIFF exports fall back to R's built-in devices; everything else works fine.

---

## Step 3: Run the App

1. In RStudio, go to **File → Open File** and navigate to the `Lysis_Curve_App_v28.R` file.
2. The file will open in the top-left editor panel.
3. Click the **"Run App"** button at the top-right of the editor panel. It has a green play icon and the text "Run App".
   - If you don't see "Run App", make sure the `.R` file is the active tab in the editor.
4. A new window will pop up with the app. If it opens in a small RStudio window, click **"Open in Browser"** at the top for a better experience.

**To stop the app**, click the red stop-sign icon in the RStudio Console, or press **Esc** in the Console.

---

## Step 4: Prepare Your Data

The app accepts **CSV files** (`.csv`). Your data should be structured in one of two formats.

### Wide Format (most common)

Each column is a different sample, and each row is a time point. The first column should be time.

| Time | Sample_A | Sample_B | Sample_C |
|------|----------|----------|----------|
| 0    | 0.05     | 0.06     | 0.04     |
| 30   | 0.12     | 0.15     | 0.10     |
| 60   | 0.35     | 0.40     | 0.28     |
| 90   | 0.70     | 0.82     | 0.61     |

If you have replicates, give them the same column name. The app will automatically calculate the mean and standard deviation.

> **Accuracy note (fixed in v2.8):** versions up to v2.7 silently used only the *first* column of each repeated name, so means were based on a single replicate and error bars disappeared. v2.8 pools all replicate columns correctly. After loading a file, check the green **data summary box** under the file picker — it shows exactly how many replicate columns were detected per sample, so you can confirm the app read your data the way you intended.

| Time | WT | WT | WT | Mutant | Mutant | Mutant |
|------|----|----|----|--------|--------|--------|
| 0    |0.05|0.06|0.04| 0.05   | 0.04   | 0.06   |
| 30   |0.12|0.15|0.10| 0.08   | 0.09   | 0.07   |

### Long Format

Three columns: time, a grouping column (e.g., "Sample" or "Condition"), and a value column.

| Time | Sample  | OD    |
|------|---------|-------|
| 0    | WT      | 0.05  |
| 0    | WT      | 0.06  |
| 0    | Mutant  | 0.04  |
| 30   | WT      | 0.12  |
| 30   | Mutant  | 0.08  |

The app auto-detects which format you're using.

### Blank / media OD

The app **assumes your data is already blank-subtracted** — it has no media-blank
handling of its own beyond the optional **"Subtract blank/media OD"** field in the
sidebar (0 = off, the default). If your CSV still has a constant media/blank OD baked
into every reading, enter that value there: it's subtracted from every reading before
plotting, statistics, and curve metrics, and the value used is recorded in the report
provenance. A small negative OD after subtraction is expected and handled correctly —
see the μmax note in [Stats & Reports](#stats--reports-new-in-v28) below.

### Decimal-comma / semicolon CSV exports

Some plate-reader software exports European-locale CSVs (`;`-separated, `,` as the
decimal mark, e.g. `1,5E+08`). Use the **"CSV format"** selector next to the file
picker — Auto-detect (the default, sniffed from the header line), US/UK, or European.

---

## Using the App

### Loading Data

Click **"Browse..."** next to "Choose CSV File" and select your file. The plot will appear automatically on the right side.

### Selecting Samples

Under **Variable Styling → Sample Selection**, use the dropdown to check or uncheck which samples appear on the plot.

### Adjusting the Plot

All settings are in the left sidebar, organized into collapsible sections. Click on any section header to expand it.

**Axis Settings** — Change between linear, logarithmic, square root, or reverse axis scales. Customize tick marks, axis labels, fonts, and gridlines. The default Y-axis is logarithmic (common for OD data).

**Time Point Filtering** — Restrict the displayed time range or exclude specific time points (e.g., removing an outlier at time 0).

**Region Highlighting** — Add colored rectangular regions to highlight specific areas of the plot (e.g., a growth phase).

**Time Point Markers** — Add vertical lines at specific time points (e.g., when a treatment was added).

**Color Palettes** — Choose from preset palettes (Viridis, Colorblind-friendly, Publication, etc.) or use custom colors per sample.

**Line & Point Settings** — Control line thickness and point appearance. Toggle points on/off and adjust their size.

**Label Options** — Add labels at the end of each line to identify samples without needing the legend.

**Error Bars / Shadow** — Display variability as error bars (T-shaped or lines) or as a shaded ribbon around each line. Choose between SD, SEM, or 95% CI. If you only have one replicate per sample per time point, error bars will simply not appear (there's no variability to show).

**Variable Styling** — Fine-tune each sample individually: set its color (from presets, HEX code, or RGB sliders), point shape (circle, square, triangle, etc.), line type (solid, dashed, dotted, etc.), and legend label.

### Saving and Loading Settings

Under **Save / Load Settings**, you can:

- **Save Settings** — Downloads a `.json` file containing all your visual settings (colors, shapes, line types, axis options, etc.). Does not save your data.
- **Load Settings** — Upload a previously saved `.json` file to restore your settings. If the sample names in the settings file match your current data, their colors/shapes/linetypes are applied automatically.
- **Clear & Reset** — Restores everything to default values.

This is useful when you want to apply the same visual style to different datasets.

---

## Journal-ready figures (new in v2.10)

For a thesis or a manuscript, make the figure at its final printed size and
never rescale it afterwards. Expand **Journal Figure Preset**:

1. Pick the **Journal** family and the **Width** (single column, 1.5 columns,
   double column, or *half of a double column* for one panel of a two-panel
   figure). Nature is 89 / 120 / 183 mm; Cell Press 85 / 114 / 174; ASM 84 /
   175; PLOS 132; Science 55 / 120 / 183; or type a custom width in mm.
2. Set the **Height** (65 mm is a good single-column default; Nature's
   maximum is 170 mm) and, for multi-panel figures, the **Panel label**
   (`a`, `b`, ...).
3. Click **Apply journal preset**. The figure becomes Arial with 7 pt axis
   titles, 6 pt tick/legend text and an 8 pt bold panel label; 0.75 pt data
   lines and 0.5 pt axes/error bars; the Okabe-Ito colour-blind-safe palette;
   the legend inside the panel; trimmed log labels (0.1, not 0.10); export in
   mm as **SVG**. The preview switches to **True export size** so what you see
   is what Inkscape will open (use the zoom slider).
4. Adjust anything you like (every preset value is an ordinary control; the
   **Figure Layout** block in Axis Settings has the legend position, line
   widths in pt, tick length in mm and the panel label), then **Download
   Image**. **Save Settings** keeps the result as a reusable style.
5. Copy the **Figure Legend Text** into the manuscript: it already states the
   measure, the curves, *n*, the error statistic, and the MOI / host / phage /
   medium you entered in the Experiment Notebook.

Then in **Inkscape**: open the SVG (Document Properties shows the width in
mm), edit text directly (it is real Arial text, not outlines), assemble panels
at native size, and *Save a Copy* as Plain SVG or PDF for submission.
The reasoning, the sources for each number and the full Inkscape loop are in
[docs/FIGURE_STANDARDS.md](docs/FIGURE_STANDARDS.md).

---

## Exporting Your Plot

Expand the **Plot Dimensions & Export** section.

### Image Export

1. **On-screen preview**: *Screen canvas* (width/height in pixels, the old
   behaviour) or *True export size* (the figure at its physical export size,
   zoomed 1-4x, rendered through the same pipeline as the download).
2. Set the export **width and height** in inches or mm and, for raster
   formats, the **DPI**. A note shows the size in both units.
3. Choose a format: **SVG** (Inkscape-ready: real text, no `textLength`
   distortion when you edit labels), **PDF** (fonts embedded, as journals
   require), **TIFF** (LZW, 600 dpi for line art), **PNG** or **JPEG**.
4. Click **"Download Image"**.

Recommended settings: use the **Journal Figure Preset** above (column width
in mm, SVG) and finish in Inkscape. For a quick slide figure, PNG at 300 dpi
and the pixel canvas are fine.

### PowerPoint Export

Click **"Download PowerPoint (.pptx)"**. This creates a slide deck where each slide adds one more sample to the plot (a "cumulative build"), which is great for presentations where you want to walk through each sample one at a time. The final slide shows all samples together.

### Animated GIF Export

Set the frames per second and GIF dimensions, then click **"Download Animated GIF"**. Like the PowerPoint, each frame adds one more sample. The GIF loops automatically.

---

## Stats & Reports (new in v2.8)

Expand the **Stats & Reports** section in the sidebar. Everything here reflects the *current* sample selection and time filtering — exactly what the plot shows.

### Statistics Export

- **Summary Statistics (CSV)** — per sample and time point: mean, SD, n, SEM, and the 95% CI half-width (computed as t₀.₉₇₅,ₙ₋₁ × SEM). `n` counts only non-missing replicate values.
- **Curve Metrics (CSV)** — per-sample growth & lysis metrics using the **same engine as the lysis-paper companion app** (verified to reproduce its numbers by an automated parity suite): initial/max/final OD and their times, the curve's global minimum, AUC (trapezoidal), **μmax** (centered rolling-window regression on ln OD; window adjustable, default 5 points; non-positive OD readings are excluded from a window rather than clamped), **doubling time**, **lag phase** (first time the rolling slope reaches 10% of μmax), **stationary-phase duration**, **lysis onset** (first post-peak decline > 5% of peak), **lysis rate** (regression slope peak→trough), **OD drop**, **residual OD**, **recovery slope**, lysis efficiency, recovery ratio, fold change, % drop from max, time to half-max after peak, and steepest point-to-point decline. Metrics that don't apply (e.g. doubling time for a curve that never grows) are reported as `NA`, never fabricated. This runs on the *mean* curve — one number per sample, with no replicate-to-replicate uncertainty. An optional **peak-detection smoothing window** (off by default) stabilizes the peak-anchored metrics against replicate noise; see [docs/METRICS_REFERENCE.md](docs/METRICS_REFERENCE.md) for why that matters.
- **Per-Replicate Metrics, mean ± SD (CSV)** — the *same* metrics engine run once per individual replicate curve, then reported as mean ± SD across replicates, alongside (not instead of) the mean-curve metrics above. These two are not interchangeable — see [docs/METRICS_REFERENCE.md](docs/METRICS_REFERENCE.md#per-replicate-metrics-optional-additive--per-replicate-metrics-mean--sd-csv).
- **Infection metrics** — pick your uninfected control in *Reference sample* to add the five reference-normalized metrics: relative growth (AUC ratio), infection strength (1 − AUC ratio), relative μmax, relative max OD, and lysis-onset Δ from lag. *(Note: the reference implementation in the companion app has an `ifelse()` recycling bug that assigns the first sample's relative values to every sample; this app computes each sample's own ratio correctly.)*

### Experiment Notebook

The **Experiment Notebook** sidebar section (modeled on the companion app's Experiment Notes tab) records the full experimental context: experiment ID, date, experimenter, project, lab; experiment type, host strain(s), phage/plasmid, MOI, replicate/batch; medium, temperature, time of infection, inducer; observations, issues, next steps, tags; plus three free key–value custom fields. Every non-empty field is embedded in the HTML and PDF reports automatically, so the report doubles as a complete lab-notebook record.

### Analysis Report (single file)

Fill in an optional title, author, and notes, then click:

- **HTML Report** — one self-contained `.html` file (openable in any browser, easily shared or printed to PDF) containing: the figure at export settings, the experiment notebook record, the summary statistics table, the curve metrics table *with the definition of every metric*, data provenance (file name, detected format, time column, filters applied, error settings), key visual settings, and the R/package versions used — everything needed to reproduce or defend the analysis.
- **PDF Report** — the same content as a multi-page PDF (title/provenance page, notebook page, figure page, paginated tables). No LaTeX or pandoc required.
- **Report base font size** — scales all report text and tables in both formats.

### Everything stays editable

All of this is live: change the Y-axis label (Axis Settings → Labels & Formatting), rename any legend entry (Variable Styling → Legend Label), switch fonts and sizes (title, axis labels, axis text, and legend each have their own size control), recolor any sample, then re-download the figure or regenerate the report — the outputs always reflect the current state of the plot.

---

## Troubleshooting

**"Error: could not find function..."** — You're missing a package. Go back to Step 2 and install the missing package.

**The app opens but the plot is blank.** — Make sure you've uploaded a CSV file and that at least one sample is selected in the Sample Selection dropdown.

**The plot shows an error message in red text.** — This usually means something is wrong with the data or a setting combination. Try resetting settings with the "Clear & Reset" button. Check that your CSV has numeric values in the data columns and a proper time column.

**Error bars / shadow ribbon doesn't show.** — If you have only one replicate per sample per time point, the standard deviation is **undefined** (not zero — you cannot measure spread from a single value), so no bar is drawn at that point and the Summary Statistics CSV / report tables show `NA` for SD, SEM, and the 95% CI at that time point. This is expected behavior. You need at least two replicates for error bars — and for the exported statistics — to be meaningful.

**The app is slow or laggy.** — If you have a very large number of samples, the app may take a moment to update. The app debounces inputs (waits 400ms after you stop adjusting before redrawing), so slight delays are intentional.

**"Run App" button doesn't appear.** — Make sure the `.R` file is open and is the active tab in the editor. Also confirm that the `shiny` package is installed.

**I want to use this on another computer.** — Copy the `.R` file and repeat Steps 1 and 2 on the new computer. Your saved settings `.json` files are portable and can be shared with collaborators.

---

## Quick-Start Checklist

1. Install R from https://cloud.r-project.org
2. Install RStudio from https://posit.co/download/rstudio-desktop/
3. Open RStudio, paste the `install.packages(...)` command into the Console, press Enter
4. Open the `.R` file in RStudio → click "Run App"
5. Upload your CSV → adjust settings → export your plot

---

## License

This project is released under the [GNU General Public License v3.0](LICENSE) (GPL-3.0).

## Citation

Citation metadata is provided in [`CITATION.cff`](CITATION.cff). If you use this app in
published work, please cite it using that file's metadata (GitHub and Zenodo both read
`CITATION.cff` automatically to generate a formatted citation).

---

*Lysis Curve OD Visualization App v2.9 — Michael Baffour Awuah / Ramsey Lab*
