# Data tab, exclusions, blanks and transforms (v2.22)

This page describes how the app reads and conditions your data before anything is plotted or computed: what is
detected as a sample, the **Data** tab, the exclusions that really filter, the blank / baseline and transform
options, and the replicate QC table. Tests: `tests/test_v222_data.R`.

Everything below is applied in **one place** (`prepare_replicate_data()` and, through it, `prepare_plot_data()`),
so the figures, statistics, curve metrics, fits, extraction tables, bootstrap, reports and downloads always see the
same numbers. With every control at its default nothing runs and the numbers are exactly what they were before.

## 1. What is read as a sample

### Wide format (one column per sample)

* The **time column** is auto-detected (a column named Time / t, then any column with "time" in its name, then the
  first column of non-decreasing numbers, then column 1). **Time column** under the file picker overrides it; the
  data already in memory are re-processed (the file is not read again) and the sample list, time points and axis
  range follow.
* **Every numeric column that is not the time column is a sample**, taken by position, so replicate columns that
  share a name (`WT, WT, WT`) are all kept and pooled as replicates. The old rule ("only columns whose name contains
  OD / absorbance / value") silently dropped samples called `Mod` or `Product`.
* Two kinds of column are **not** samples, and the green data summary always lists them with the reason:
  * a **temperature column**: a name that starts with `T°` or contains `temp` (any case) or `°C` (Gen5 writes `T° 600`);
  * a **non-numeric column** (notes, well labels).
* Stacked blocks (the time restarts) are replicates of every sample; see section 4 for how they are numbered.
* Out-of-range cells such as `OVER` / `OVRFLW` / `#SAT` become missing readings and are reported on load.

### Long format (one row per reading)

Recognised by a group column named `variable`, `condition`, `treatment`, `sample`, `group` or **`strain`**
(each also capitalised); the first one found, in that order, is the sample column. Without one, a small file with a
low-diversity text column is read as long. The value column is the first column named like `od` / `absorbance` /
`value`, else the first numeric one. A `replicate` / `rep` / `well` column is the replicate identity.

## 2. The Data tab

The main tab **Data** sits right after **Lysis curve**.

**View & edit.** The working data in a table. With the DT package installed a cell is edited in place (double-click);
without it a *Row / Column / New value / Set cell* form does the same. Every edit runs the **same loading step as a
file** (`ingest_data(keep_state = TRUE)`): the columns, replicate ids, sample list, QC flags, figures, statistics and
metrics are recomputed; your sample selection, display names and exclusions are kept. An **empty** cell is a missing
reading, which is the documented way to exclude **a single point**; text in a numeric column also becomes empty
(with a notice). **Revert to loaded** returns to the data as first read (the loaded copy is never modified by edits).
**Download current data (CSV)** writes the working data, missing readings as empty cells; the file loads back into the
app unchanged. A clock-time column (`0:15:00`) is shown and saved in the minutes it was converted to.

**Enter data.** Paste a table from Excel or a text file into the box: a header row (Time, then one column per sample,
a repeated name for replicates) and one row per time point. The separator (tab, comma or semicolon) is
auto-detected by the same sniffer that reads files (decimal commas included); *Separator* can force it.
**Apply** loads it like a file (source name "Pasted data") and returns you to the figure; **Clear** empties the
box. Empty columns and rows (the tail Excel often adds) are dropped.

**Excel files.** The file picker accepts `.xlsx` / `.xls` (needs the `readxl` package, offered by `run_app.R`). The
first sheet is read; a workbook with several sheets shows an **Excel sheet** box. Duplicate headers (replicate
columns) are kept; Excel time cells arrive as clock times and are converted to minutes like a Gen5 export.

## 3. Exclusions

Order of operations on the raw readings: time filters and per-sample windows, then the OD range, then excluded
replicates, then the blank, then the transform. Every active step is named in the **data summary**, the **report
provenance** table (row *Data handling*), the **Figure Legend Text**, the **methods draft** and the **figure bundle**.

| Exclusion | Where | Effect |
|---|---|---|
| One reading | Data tab: empty the cell | that reading is missing; `n` at that time point drops |
| OD range | Time Point Filtering, *Exclude by Value* (`use_od_filter`, `od_min`, `od_max`) | readings outside `[min, max]` are dropped from the **raw** data before averaging; `n` shrinks, mean and SD follow |
| Replicate / well | Time Point Filtering, *Exclude Replicates* (`excluded_reps`) | every reading of the ticked replicate leaves every mean, statistic, metric, fit and export |
| Time points / windows | unchanged (v2.18) | |

**Replicate numbering** (the checkbox labels name the file column / well so you can match them to the plate):

* wide, one block: the sample's columns in file order, `rep 1 .. rep k`;
* wide with stacked blocks: replicate = `(block - 1) * columns_per_sample + column`;
* long with a replicate / well column: the dense rank of its values within the sample;
* long without one: the nth row at each (sample, time).

Excluded replicates stay visible when **Show replicate points** is on, as **hollow grey circles**, and the figure legend
says so. They remain in the **Replicate QC** tables (flagged `excluded`) so the decision can be reviewed. Replicate
exclusions belong to the loaded file: they are not saved with the settings and start empty on every new load (a cell
edit, a time-column change or Revert keeps them). If you exclude every replicate of a sample the sample leaves the data.

## 4. Blank / baseline

*Blank / baseline* (under the file picker):

* **A constant** (`blank_od`, the old control): subtracted from every reading.
* **A blank sample / column**: the blank's mean curve (over its non-excluded replicates, linearly interpolated at each
  time) is subtracted row-wise from every other sample at each time point, and the blank is then **left out of the
  samples** (it is removed from Select Samples as well). A blank with no readings does nothing and says so.
* **Mean of the first k points**: each replicate curve has the mean of its own first *k* readings subtracted (a
  per-replicate baseline).

## 5. Transforms

*Transform* is applied after the blank, per replicate, and then the replicates are averaged:

| Transform | Value shown |
|---|---|
| None | as measured (default) |
| Normalise to t0 | `value / (the replicate's own first reading)`; replicates that start at 0 or below are left out, with a notice |
| Normalise to a control sample | `value / (mean curve of the control at that time)`; the control itself becomes 1; where the control is 0 or below the reading is left out |
| 0-100 % of own maximum | `100 * value / (maximum of the sample's mean curve)` |
| ln(OD) | natural log; readings at or below 0 are left out, with a notice |

A transform changes **every number downstream**, including the curve metrics (μmax is the slope of ln OD, so
applying ln or a 0-100 % scale to it is rarely what you want). The axis labels are yours to change.
Journal / Legacy presets do not touch these controls; *Clear & Reset* returns them to the defaults.

## 6. Replicate QC (Statistics tab)

`replicate_cv_table()`:

* **Per sample**: replicates, time points with a CV, **median CV %**, **max CV %**, the **time of the maximum**, how
  many time points exceed the limit, and a **high-CV flag** when the median CV exceeds the limit (default 20 %,
  `rep_qc_flag`). CV at a time point is `sd / |mean|` over the replicates there (needs two or more).
* **Per replicate**: the **distance from the sample's median curve** (mean absolute difference over the replicate's time
  points, also as a percentage of the median curve's mean level), and `most_distant` for the farthest replicate of a
  sample with three or more. With two replicates both are equidistant from the median, so nothing is singled out.
* Built from all replicates including excluded ones. Downloads: per-replicate CSV (with the sample columns joined)
  and per-sample CSV.

## 7. Other inputs added with this change

* **Samples: All / None** buttons next to Select Samples.
* **Extinction floor (OD)** (`extinction_threshold`, default 0.05) and **Storms integration limit t_stat**
  (`t_stat_override`, blank = automatic) in Stats & Reports; both go straight to the metrics engine (curve metrics and
  per-replicate metrics). A readout shows *Storms integration window: 0 to t_stat (auto)* with the value used, or
  *(override)*.
* A one-time warning when error bars are requested but every sample has a single replicate at every time point.

## 8. Settings

Saved with the settings: `use_od_filter`, `od_min`, `od_max`, `blank_mode`, `blank_sample`, `blank_k`,
`transform_mode`, `transform_control`, `extinction_threshold`, `t_stat_override`. Not saved (they belong to the loaded
file): the time-column choice, the Excel sheet, the replicate exclusions, the pasted text and the table state.
