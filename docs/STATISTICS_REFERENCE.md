# Statistics reference (v2.22 engine)

These are pure functions in `Lysis_Curve_App_v28.R` (block "v2.22 stats engine"), tested by
`tests/test_v222_stats.R`. They give Prism-style column, grouped and time-course statistics for
lysis-curve data. The table of one-line definitions is `STATS_V222_DEFINITIONS`
(`name, definition, reference`).

## Read this first: n = 3

Lysis experiments usually have **three biological replicates per sample**. At n = 3:

* a non-significant result is **not** evidence of no difference (power is low unless the effect is huge);
* Shapiro-Wilk, Levene / Brown-Forsythe and Bartlett cannot reject anything meaningful, so "passed the normality test"
  means "no evidence against", never "is normal";
* the smallest attainable two-sided p of a rank test (Kruskal-Wallis, Dunn, Wilcoxon) is large: Wilcoxon signed-rank
  cannot go below 0.25 at n = 3 (0.125 at n = 4, 0.0625 at n = 5);
* Grubbs, MAD and IQR outlier rules have almost no power or no meaning with 3 values: they flag, they never remove;
* confidence intervals are very wide (t with 2 df: 4.30 x SEM) and Welch degrees of freedom are often 2-3.

Every function says this in its `note` column (or `note` entry) when it applies. Nothing in the engine removes data.

## Conventions

* Tidy data.frames; replicate frames use `sample` / `replicate` / `time` / `od` (and `well`). The app's own replicate frame
  (`variable` / `time` / `value` / `replicate`) is accepted everywhere a `long` frame is expected. A missing `well` id is built
  as `<sample>_<replicate>`.
* Functions never error on bad input (n = 1, constant values, NA, a missing reference): they return NA rows with a `note`.
* Base R + `stats` first. `nlme` is used for the repeated-measures model when installed (fallback below); `mvtnorm` for
  Dunnett's test when installed (fallback below). `rstatix` / `car` are only used by the tests as cross-checks.
* In every pairwise table `group1 - group2 = estimate`; `group2` is the earlier level, or the **reference** when one is given
  (this is TukeyHSD's "b-a" labelling). Stars come from `sig_stars(p_adj)`.

## 1. Column statistics

`describe_column(x, level = 0.95)` returns one row: `n, mean, sd, sem, cv_pct, ci_lo, ci_hi, median, q1, q3, iqr, min, max,
range, skewness, kurtosis, geo_mean, shapiro_p, note`.
`describe_metrics(rep_rows, metrics = NULL, by = "sample", level = 0.95)` applies it to every sample x metric of the table that
`compute_replicate_metric_rows()` returns (long: `sample, metric,` describe columns).

| Statistic | Formula | Notes |
|---|---|---|
| SEM | SD / sqrt(n) | n >= 2 |
| CV % | 100 SD / abs(mean) | NA if mean = 0 |
| CI of the mean | mean +- t(1 - alpha/2, n - 1) SEM | t-based; n >= 2 |
| quartiles | type 7 (R default, Excel QUARTILE.INC) | Prism uses the same method |
| skewness | G1 = sqrt(n(n-1))/(n-2) x m3/m2^1.5 | bias-corrected as in Prism / Excel / SAS; n >= 3 |
| kurtosis | excess G2 = (n-1)/((n-2)(n-3)) x ((n+1) g2 + 6), g2 = m4/m2^2 - 3 | bias-corrected as in Prism; n >= 4 |
| geometric mean | exp(mean(log x)) | NA if any x <= 0 |
| shapiro_p | Shapiro-Wilk | NA if n < 3 or constant |

*Prism: Column statistics -> Descriptive statistics.* **n = 3:** SD and CI are computed but are themselves very uncertain
(the SD of 3 values can be off by a factor of 2); shape statistics are undefined or meaningless below n = 4.

## 2. Normality and variance

`normality_table(rep_rows, metrics = NULL)`: Shapiro-Wilk W and p for every sample x metric, plus a `(pooled residuals)` row
per metric (residuals from the sample means, n = N - k pooled). `qq_frame(x)` returns `data.frame(theoretical, sample)`
(normal quantiles at `ppoints(n)` against the sorted values) for a Q-Q plot.

`variance_check(values, groups)`: **Brown-Forsythe** (Levene with the median as centre, hand-coded as the ANOVA of
`|x - group median|` on the groups) and **Bartlett**. Groups with fewer than 2 finite values are excluded.

*Prism: Normality and lognormality tests; Test for equal variances (Brown-Forsythe, Bartlett).*
**n = 3:** use these as warnings only. Bartlett is sensitive to non-normality. With n = 3 per group the practical rule is to
use the Welch / Games-Howell family regardless, which does not need equal variances.

## 3. Omnibus tests

`omnibus_table(values, groups)` returns three rows (`test, statistic, df1, df2, p, eta_sq, epsilon_sq, note`):

| Test | Statistic | Effect size | When |
|---|---|---|---|
| One-way ANOVA | F = MSB / MSW | eta^2 = SSB / SST | equal variances, roughly normal residuals |
| Welch ANOVA (`oneway.test`) | Welch F, df2 Welch-Satterthwaite | n/a | unequal variances; the safe default |
| Kruskal-Wallis | H (tie-corrected), chi-squared df k - 1 | epsilon^2 = H / (N - 1) | distribution-free; ranks |

*Prism: One-way ANOVA (ordinary, Brown-Forsythe and Welch ANOVA, Kruskal-Wallis).* **n = 3:** Kruskal-Wallis with 3 groups x 3
values has few attainable p-values; do not read a p of 0.03-0.05 as precise.

## 4. Post-hoc / multiple comparisons

`posthoc_table(values, groups, method, reference = NULL, level = 0.95, dunn_adjust = c("holm", "bonferroni"))` returns one row per
pair: `group1, group2, estimate, ci_lo, ci_hi, statistic, p_raw, p_adj, method, stars, note`; attribute `n` holds n per group.
With `reference` the family is "each sample vs the reference" (n - 1 pairs); without it, all pairs. Groups with fewer than 2
finite values are excluded and the note says so.

| `method` | Procedure | estimate / CI | p_raw | p_adj | Prism name | Use when |
|---|---|---|---|---|---|---|
| `tukey` | Tukey-Kramer from `TukeyHSD(aov())`, pooled variance | diff; simultaneous CI | NA | studentised range | Tukey | equal variances, all pairs |
| `holm_welch` | Welch t per pair, Welch-Satterthwaite df | diff; unadjusted CI | Welch p | Holm step-down | (Holm-Sidak is the Prism analogue) | default for unequal variances, any family |
| `games_howell` | Welch t per pair, p from `ptukey(|t| sqrt 2, k, df)` | diff; CI = diff +- qtukey/sqrt2 x SE | NA | Games-Howell | Games-Howell | unequal variances, all pairs |
| `dunn` | Kruskal-Wallis rank sums, tie-corrected z | mean-rank difference; no CI | 2 pnorm(-|z|) | Holm (or Bonferroni) | Dunn | after Kruskal-Wallis |
| `dunnett` | each sample vs one reference, pooled MSE, multivariate t | diff; simultaneous CI | NA | multivariate-t integral | Dunnett | many treatments vs one control |
| `bonferroni` | Welch t per pair | diff; CI at 1 - alpha/m | Welch p | min(1, m p) | Bonferroni | few comparisons, conservative |
| `sidak` | Welch t per pair | diff; CI at 1 - (1 - alpha)^(1/m) | Welch p | 1 - (1 - p)^m | Sidak | as Bonferroni, slightly less conservative |

Details and caveats:

* **Games-Howell** (hand-coded; checked against `rstatix::games_howell_test` to 1e-6): t_ij = (mean_i - mean_j) / sqrt(s_i^2/n_i +
  s_j^2/n_j); df by Welch-Satterthwaite; p = `ptukey(|t| sqrt 2, nmeans = k, df, lower.tail = FALSE)`. Known to be slightly
  liberal at n = 3 per group (the Welch df are 2-3).
* **Dunn** (hand-coded; checked against `rstatix::dunn_test`): z = (mean rank_i - mean rank_j) / sqrt((N(N+1)/12 -
  sum(t^3 - t)/(12(N-1))) (1/n_i + 1/n_j)), ties corrected. With 3 values per group the smallest attainable p is large, and
  Holm over many pairs often cannot reach 0.05 at all.
* **Dunnett** needs `mvtnorm` (`pmvt` for the adjusted p with the Dunnett correlation r_ij = sqrt(n_i n_j / ((n_i + n_ref)(n_j +
  n_ref))), `qmvt` for the simultaneous CI; df = N - k). **mvtnorm is not installed on every machine; when it is absent (or the
  integral fails) the function falls back to Holm-adjusted Welch t against the reference and writes
  `Dunnett (fallback: Holm-adjusted Welch t vs reference; mvtnorm not installed)` in the `method` column.** The multivariate-t
  branch is guarded and could not be exercised in the development environment (no mvtnorm): treat it as untested until run with
  the package present.
* **Tukey with a reference** shows only the pairs involving the reference but keeps the all-pairs adjustment (the note says so).
* `holm_welch` CIs are unadjusted (a Holm procedure has no natural simultaneous interval); Bonferroni / Sidak CIs are widened.

**n = 3:** every pairwise p here is exploratory. Prefer the effect size and its CI to the star.

## 5. Time courses

### Two-way ANOVA: sample x time

`two_way_time_anova(long, response = "od", time = "time", sample = "sample", id = "well", log = TRUE, rm = TRUE)` returns
`list(anova = data.frame(term, df_num, df_den, F, p), method, note)` with the terms Sample, Time and Sample x Time. Readings are
log-transformed by default (readings <= 0 are dropped and the note says so); time is a factor; tests are sequential (type I).

| Order tried | Model | `method` text |
|---|---|---|
| 1 (`rm = TRUE`, nlme installed) | `nlme::lme(y ~ sample * time)` with a random intercept per well and `corAR1` errors within well (REML) | two-way repeated-measures: mixed model ... AR(1) |
| 2 | `aov(y ~ sample * time + Error(well))`: Sample against the between-well stratum, Time and the interaction within wells | two-way repeated-measures ANOVA: aov Error(well) |
| 3 (or `rm = FALSE`) | plain `aov(y ~ sample * time)` | two-way ANOVA (ignores the repeated measures) |

The readings of one well over time are the repeated measures. Option 2 assumes compound symmetry (no Greenhouse-Geisser correction); option 3
treats all readings as independent and is **too liberal** for Sample and the interaction, because readings from one well are
correlated. The note records why a fallback was used (for example "nlme not installed"). *Prism: Two-way ANOVA (repeated measures,
mixed-effects model).* **n = 3:** with 3 wells per sample the between-well error has only k x 2 df; the interaction F is
informative, the main effect of Sample is weak.

### Multiple comparisons at each time

`per_time_comparisons(long, reference, adjust = c("sidak", "holm", "bonferroni"), level = 0.95)`: at every time point each
non-reference sample is compared with the reference by Welch t (unadjusted CI of the difference at `level`); p-values are adjusted
**across the time points of that sample** (the family is a sample's time points; Prism's "compare each cell mean with the control
at each time" corrects over a larger family when there are several samples, so use `adjust = "bonferroni"` for a stricter
reading). Columns: `time, sample, diff, ci_lo, ci_hi, t, df, p_raw, p_adj, stars, n, n_ref, note`; one row per time x non-reference
sample, NA row + note when a cell has fewer than 2 readings or constant values.

## 6. Outliers

`outlier_tests(x, method = c("grubbs", "mad", "iqr"), alpha = 0.05)` returns `index, value, flagged, method, statistic, p, note`,
one row per value of `x`.

* **Grubbs** (two-sided, n >= 3, assumes normality): G = max|x - mean| / SD; t = sqrt(n(n-2)G^2 / ((n-1)^2 - nG^2));
  p = min(1, 2n x pt(t, n-2, upper)). Only the most extreme value is tested: `statistic` and `p` are filled on that row.
  `grubbs_critical(n, alpha)` gives the critical G (n = 10: 2.290, matching the published table; NIST e-Handbook 1.3.5.17);
  `grubbs_p(G, n)` the p-value. *Prism: Identify outliers (Grubbs); ROUT is not implemented.*
* **MAD**: |x - median| / (1.4826 MAD) > 3.
* **IQR**: outside q1 - 1.5 IQR / q3 + 1.5 IQR (type 7 quartiles); `statistic` is the distance beyond the fence in IQR units.

**n = 3:** Grubbs can only flag a value at G > 1.1543 (maximum 1.1547), which needs an extreme, one-sided outlier; MAD and IQR
rules are unreliable below about 6 values. A flag is a prompt to look at the well, not a reason to delete it.

`replicate_outlier_flags(long)`: per sample, the mean absolute deviation (over time) of each replicate curve from the per-time
**median** curve of the sample, divided by the median of those deviations across the sample's replicates; scaled distance > 3 is
flagged. Needs >= 3 replicates per sample. Columns: `sample, replicate, well, abs_dev, distance (scaled), flagged, note`.

## 7. Area under the curve

`auc_table(long, baseline = c("none", "first", "value", "min"), baseline_value = 0, t_from = NULL, t_to = NULL, relative_to = NULL,
level = 0.95)` returns `list(replicates, summary)`.

* Per replicate curve: trapezoid area of `pmax(od - baseline, 0)` over `[t_from, t_to]` (defaults: the whole curve); the window
  edges are linear interpolations; where the curve crosses the baseline the crossing point is inserted, so the area above the
  baseline is exact for a piecewise-linear curve. Baseline: none (0), the replicate's first reading, a fixed `baseline_value`, or
  the replicate's minimum. A window beyond the data is clipped (noted).
* `summary`: per sample `n, mean, sd, sem, ci_lo, ci_hi` (t-based).
* `relative_to = <reference>` adds `rel_auc` (mean AUC / reference mean AUC) with a **log-delta (Katz) CI**: SE(log R) =
  sqrt(CV_a^2 / n_a + CV_r^2 / n_r) (each term is (SEM / mean)^2), df by Welch-Satterthwaite on the two SEMs, CI
  = R exp(+-t SE). It was chosen over Fieller's interval because it is always defined and simple to state; it needs positive means
  and is approximate at n = 3 (slightly narrow). The reference row is 1 with no CI.

`auc_compare(auc_rows, reference, method = "holm_welch", level = 0.95)` runs `posthoc_table()` on the per-replicate AUCs.
*Prism: Area under curve.*

## 8. One-sample tests

`one_sample_tests(x, mu = 0, level = 0.95)`: two rows, **one-sample t** (CI of the mean) and **Wilcoxon signed-rank** (exact p when
there are no ties or zeros, otherwise the normal approximation; the note gives the smallest attainable p). *Prism: One sample t test
and Wilcoxon test.* **n = 3:** the Wilcoxon p cannot go below 0.25, so it is uninformative; use the t test and its CI.

## 9. Correlation and straight-line fit (metric explorer)

* `correlation_table(x, y, method = c("pearson", "spearman"), level = 0.95)`: one row `method, n, r, r2, ci_lo, ci_hi, statistic,
  df, p, note`. Pearson CI by Fisher's z (`cor.test`); Spearman CI by Fisher's z with the Bonett-Wright SE sqrt((1 + r^2/2)/(n-3)).
  Needs n >= 3 for a test, n >= 4 for a CI. *Prism: Correlation.*
* `linear_fit_table(x, y, level = 0.95)`: ordinary least squares, two rows (intercept, slope) with `estimate, se, ci_lo, ci_hi, t, df,
  p`; `r2, adj_r2, n` repeated on both rows. *Prism: Simple linear regression.*
* `linear_band(x, y, newx = NULL, level = 0.95, type = c("confidence", "prediction"))`: `data.frame(x, fit, lo, hi)`; `newx` defaults
  to 50 points over the range of x.

**n = 3** (three points, one per sample): r is meaningless; the table says so below n = 5.

## 10. Extra error measures

`error_half_width_extra(values, type = c("range", "iqr", "ci90", "ci99"))` returns `list(lower, upper, ymin, ymax, center)`:

* `center` is the mean; `lower = mean - (bar bottom)` and `upper = (bar top) - mean` are non-negative **offsets from the mean**
  (asymmetric for range and IQR); `ymin` / `ymax` are the absolute bar ends.
* `range` = min .. max; `iqr` = q1 .. q3 (type 7); `ci90` / `ci99` = mean +- t SEM (symmetric).
* n < 2 gives NA.

Wiring in the UI: the existing `error_half_width()` returns a single symmetric half-width. For these four types the plot layer
needs `geom_errorbar(aes(ymin = mean - lower, ymax = mean + upper))`, or `ymin` / `ymax` directly; the symmetric measures
(`ci90`, `ci99`) can still be passed through `lower`/`upper` (they are equal). **n = 3:** the range of 3 values and the IQR
(which interpolates between them) are crude; they are shown because journals ask for them, not because they estimate anything well.

## 11. Methods text

`stats_method_sentence(result, kind, what = "The measured quantity", n = NULL, level = 0.95)` returns one sentence for the editable
methods text. `kind` is one of `describe, normality, variance, omnibus, posthoc, two_way, per_time, outlier, auc, one_sample,
correlation, linear`; `result` is the matching function's output (the post-hoc sentence reads the method from the `method` column
and n from the `n` attribute). Example:

> Lysis onset was compared between samples by one-way Welch ANOVA followed by Games-Howell post-hoc tests (n = 3 biological
> replicates per sample).

## Validation

`tests/test_v222_stats.R`: describe_column against base R (mean, sd, quantile type 7, `t.test` CI, independent sum forms of G1 and
G2, `shapiro.test`); Tukey rows against `TukeyHSD` (and a verified numeric example, hyphenated names, unbalanced groups);
Games-Howell and Dunn against `rstatix` and against fixed values from a verified run; Brown-Forsythe against `car::leveneTest`;
omnibus tests against `aov`, `oneway.test`, `kruskal.test`; Grubbs against the published two-sided critical values (n = 3, 8, 10,
20); AUC against analytic integrals (triangle, trapezoid, window edges, baseline crossings); two-way repeated-measures ANOVA on
the demo data (interaction p < 0.05); every function on n = 1, constant, NA and empty input; and the optional packages masked
(nlme, mvtnorm absent) to prove the fallbacks.

## References

Brown & Forsythe 1974, JASA 69:364. Bonett & Wright 2000, Psychometrika 65:23. Dunn 1964, Technometrics 6:241. Dunnett 1955, JASA
50:1096. Games & Howell 1976, J Educ Stat 1:113. Grubbs 1969, Technometrics 11:1. Holm 1979, Scand J Stat 6:65. Joanes & Gill 1998,
Statistician 47:183. Katz et al. 1978, Biometrics 34:469. Kramer 1956, Biometrics 12:307. Kruskal & Wallis 1952, JASA 47:583.
Leys et al. 2013, J Exp Soc Psychol 49:764. Pinheiro & Bates 2000, Mixed-Effects Models in S and S-PLUS. Shapiro & Wilk 1965,
Biometrika 52:591. Sidak 1967, JASA 62:626. Tomczak & Tomczak 2014, Trends Sport Sci 21:19. Tukey 1949, Biometrics 5:99. Welch 1951,
Biometrika 38:330.
