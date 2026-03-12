# Lysis Curve OD Visualization App — User Guide

## What This App Does

This is an interactive tool for visualizing Optical Density (OD) time-series data. You upload a CSV file containing your experimental measurements, and the app generates publication-quality plots with full control over colors, shapes, error bars, axis scales, labels, and more.

You can export your plots as PDF, PNG, JPEG, SVG, TIFF, PowerPoint, or animated GIF.

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
  "officer", "rvg", "gifski"
))
```

4. Wait for the installation to finish. This may take several minutes. You'll see lots of text scrolling by — that's normal. When it's done, you'll see the `>` cursor again.

If you see any errors mentioning a specific package, try installing that package individually:

```r
install.packages("package_name_here")
```

**Note:** The `officer`, `rvg`, and `gifski` packages are optional. Without them, the PowerPoint and GIF export buttons will be disabled, but everything else works fine.

---

## Step 3: Run the App

1. In RStudio, go to **File → Open File** and navigate to the `Lysis_Curve_App_v27.R` file.
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

## Exporting Your Plot

Expand the **Plot Dimensions & Export** section.

### Image Export

1. Set the canvas size (width/height in pixels) to control how the plot looks on screen.
2. Set the export dimensions (width/height in inches) and DPI for the saved file.
3. Choose a format: **PDF** (best for publications), **PNG** (best for presentations), **JPEG**, **SVG** (scalable), or **TIFF**.
4. Click **"Download Image"**.

Recommended settings for publications: PDF format, 10×8 inches, 300 DPI.

### PowerPoint Export

Click **"Download PowerPoint (.pptx)"**. This creates a slide deck where each slide adds one more sample to the plot (a "cumulative build"), which is great for presentations where you want to walk through each sample one at a time. The final slide shows all samples together.

### Animated GIF Export

Set the frames per second and GIF dimensions, then click **"Download Animated GIF"**. Like the PowerPoint, each frame adds one more sample. The GIF loops automatically.

---

## Troubleshooting

**"Error: could not find function..."** — You're missing a package. Go back to Step 2 and install the missing package.

**The app opens but the plot is blank.** — Make sure you've uploaded a CSV file and that at least one sample is selected in the Sample Selection dropdown.

**The plot shows an error message in red text.** — This usually means something is wrong with the data or a setting combination. Try resetting settings with the "Clear & Reset" button. Check that your CSV has numeric values in the data columns and a proper time column.

**Error bars / shadow ribbon doesn't show.** — If you have only one replicate per sample per time point, the standard deviation is zero, so there's nothing to display. This is expected behavior. You need multiple replicates for error bars to be meaningful.

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

*Lysis Curve OD Visualization App v2.7 — Michael Baffour Awuah / Ramsey Lab*
