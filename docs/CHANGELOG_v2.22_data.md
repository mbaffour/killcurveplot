# Draft changelog: v2.22 data tab and ingest (to merge into CHANGELOG.md by the supervisor)

- **Detection fixes (old audit items N4-3 / N4-4).** Wide format: every non-time numeric column is a sample, by position
  (replicate columns that share a name are all kept); only a temperature column (`T°`, `temp`, `°C`) or a non-numeric
  column is dropped, and the green data summary lists the columns used and the columns dropped with the reason.
  Long format: `strain` / `Strain` is a recognised group column. New top-level `wide_sample_columns()` and
  `LONG_GROUP_NAMES`.
- **Time column override.** `time_col_choice` under the file picker, filled on load; picking another column re-runs the
  post-detection step on the data in memory (`ingest_data()` is split into `ingest_data()` + `apply_time_col()`), with no
  re-read; a column without usable numbers is refused.
- **Data tab** (after Lysis curve). *View & edit*: editable DT table (plain table + a Row / Column / Value form when DT
  is missing); every edit goes through the same ingest path so replicates, metrics and figures refresh; *Revert to
  loaded*; *Download current data (CSV)*. *Enter data*: paste Excel / CSV text, separator auto-detected through
  `read_csv_smart` (`parse_pasted_text()`), Apply / Clear. `.xlsx` / `.xls` upload through `readxl` (first sheet, sheet
  selector for several).
- **Exclusions that filter.** OD-range exclusion (`use_od_filter`, `od_min`, `od_max`) on the raw readings before
  averaging; replicate / well exclusion (`excluded_reps`, a checkbox per replicate of every sample, numbered as the app
  numbers them: `replicate_catalog()`), applied in `prepare_replicate_data()` and so in every mean, statistic, metric,
  fit and export; excluded replicates are drawn as hollow grey circles with the replicate points; both are named in the
  data summary, report provenance, figure legend text and methods draft. Per-point exclusion = empty a cell.
- **Blank / baseline and transforms.** `blank_mode` = constant (`blank_od`) | a blank sample (its mean curve subtracted
  row-wise, then left out of the samples) | mean of the first k points per replicate; `transform_mode` = none | t0 |
  control sample | 0-100 % of own maximum | ln(OD). One place: `transform_frame()`, applied inside the prepare step;
  recorded in provenance, methods draft, legend and data summary. Defaults reproduce the old behaviour exactly.
- **Replicate QC** (Statistics tab): `replicate_cv_table()` gives per sample median / max CV %, time of the maximum and a
  high-CV flag (default > 20 %), and per replicate the distance from the sample's median curve; two CSV downloads.
- **n = 1 warning**: error bars requested while every sample has a single replicate raises a one-time notification.
- **Samples All / None** buttons.
- **Extinction floor and t_stat**: `extinction_threshold` and `t_stat_override` inputs wired to
  `compute_curve_metrics()` / `compute_replicate_metric_rows()` / `compute_replicate_metrics()` (new trailing arguments
  `t_stat_override`, `extinction_threshold`); a readout shows the Storms integration window (auto / override).
- New `APP_DEFAULTS` entries (appended at the end): `use_od_filter`, `od_min`, `od_max`, `blank_mode`, `blank_sample`,
  `blank_k`, `transform_mode`, `transform_control`, `extinction_threshold`, `t_stat_override`; all are saved with the
  settings; the per-file inputs (time column, sheet, replicate exclusions, pasted text, table state) are not.
- `run_app.R` installs `DT` and `readxl`.
- Tests: `tests/test_v222_data.R`; docs: `docs/DATA_TAB_AND_TRANSFORMS.md`.
