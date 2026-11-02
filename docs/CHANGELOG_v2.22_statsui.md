# v2.22: statistics and fitting UI (draft changelog entries)

Wires the v2.22 stats engine and fits engine into the app. Pure additions: a session that does not touch the new inputs behaves as before.

**Statistics tab** (new collapsible sections before the Methods text)
- Column statistics: n, mean, SD, SEM, CV %, 95 % CI, median, IQR, min, max, skewness, kurtosis, geometric mean per sample and metric.
- Assumption checks: Shapiro-Wilk per sample and pooled residuals, Brown-Forsythe and Bartlett, a fixed-panel Q-Q figure, and a plain note on n = 3 power.
- Post-hoc comparison (method): Holm-Welch (default, identical to the existing `p_adj_holm`), Tukey, Games-Howell, Dunn, Dunnett, Bonferroni, Sidak; the omnibus table (ANOVA, Welch ANOVA, Kruskal-Wallis, eta^2, epsilon^2) above the pairwise table with stars.
- Time-course ANOVA: two-way sample x time repeated-measures ANOVA (mixed model by well when `nlme` is present; the method and note shown) and per-time comparisons vs the reference with a Sidak / Holm / Bonferroni adjustment.
- Area under the curve: baseline (none / first / value / min) and time window, per-sample mean, SD, SEM, CI, relative AUC vs the reference, and pairwise comparison by the chosen post-hoc method.
- Outliers: Grubbs / MAD / IQR per sample and metric, replicate-curve flags; flags only, with a pointer to the Data tab for exclusions.
- One-sample tests against a hypothetical value per metric (t test and Wilcoxon).
- All v2.22 tables (archive) and a CSV for every table.

**Analysis figures**
- New: "Per-time comparisons vs reference (stars)" (mean curves, stars where p_adj < 0.05, labelled curve ends, no legend, fixed panel) and "AUC per sample" (dot / bar with replicates); both in the composer, the reports and the download-all archive.
- Metric explorer: optional Pearson / Spearman correlation (r, CI, p) and a straight-line fit with a confidence / prediction band, numbers in a table under the figure.
- MOI dose-response: optional four-parameter logistic per phage series with its confidence band, the EC50 and its 95 % interval (dashed line and bar), optional fixed plateaus; EC50 table and CSV under the figure.

**Curve fitting tab**
- Fit statistics: 95 % intervals (profile or Wald), adjusted R^2, RMSE, AICc, Akaike weights, runs test and Shapiro-Wilk of the residuals for the existing growth fits; a refit that does not reproduce the engine's parameters is never used (Wald from the row's SEs instead); a lag on its bound gets no interval; a fixed-panel residual figure.
- Overlays on the fit figure (all off by default): confidence / prediction band, fitted lysis decay, shaded exponential window.
- Lysis models (one-phase decay, logistic decay) per sample or replicate: k, half-life, t50, plateau with CIs, R^2, AICc, Akaike weights; weighting scheme selector (the growth fits stay unweighted).
- Exponential-growth window per sample (auto, typed or all points): mu and doubling time with CIs, next to the rolling mu_max.
- Compare fits: extra-sum-of-squares F test between samples (all parameters, mu, lag or A) with ΔAICc and low-n warnings.

**Reports, bundle, methods, dashboard**
- HTML and PDF reports: a "Statistics (v2.22)" section (tables with rows, the per-time and AUC figures, methods sentences) once the tabs have been used.
- Figure bundle: the v2.22 CSV tables. Methods draft: one sentence per section used. Dashboard: AUC and exponential-window doubling-time cards.
- New inputs are saved in the settings JSON (`cmp_samples`, the per-file sample choice, is not); new APP_DEFAULTS entries are at the end of the defaults.
- Tests: `tests/test_v222_integration.R`. Docs: `docs/STATISTICS_AND_FITTING_UI.md`.
