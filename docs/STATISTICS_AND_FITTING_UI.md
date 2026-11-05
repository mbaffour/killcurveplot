# Statistics and curve fitting in the app (v2.22)

What each new section of the **Statistics** tab and the **Curve fitting** tab shows, when to use which test, and what the Prism
equivalent is. The calculations are the pure engines described in `STATISTICS_REFERENCE.md` (statistics) and `FITS_V222.md`
(fits); this page is about how they are wired into the app. Everything on this page is computed from the **per-replicate metrics**
(Statistics tab) or the **replicate / mean curves** (Curve fitting tab) of the samples currently selected, after the Data tab's
exclusions, blank and transform. Nothing is removed or changed automatically.

> **Read this first: n = 3.** Three biological replicates per sample is the normal case. Then a non-significant result is not
> evidence of no difference, Shapiro-Wilk / Brown-Forsythe / Bartlett cannot reject anything meaningful, a rank test cannot reach
> p < 0.05 with 3 values per group (Wilcoxon signed-rank cannot go below 0.25), Grubbs only flags a value when G exceeds 1.154,
> and confidence intervals are very wide (t with 2 df is 4.30 x SEM). Every table carries a note when this applies. Prefer effect
> sizes and their intervals to stars.

Every table has a CSV button (the CSV carries the unformatted numbers and the file's own sample names in a `sample_raw` column
next to the display name). The **All v2.22 tables (archive)** section downloads all of them in one archive with the methods
sentences; the figure bundle and the HTML / PDF reports ("Statistics (v2.22)" section) include the tables of the sections you
have used (the tab has been visited), and the methods draft gains one sentence per section.

## Statistics tab

### Column statistics  (Prism: Column statistics)
n, mean, SD, SEM, CV %, 95 % t-based CI of the mean, median, IQR, min, max, skewness, kurtosis and geometric mean, one row per
sample and metric. The **metrics** chosen here are shared by the assumption checks, the outliers and the one-sample tests.
Use it to report "mean +- SD (n = 3)"; at n = 3 the SD itself is very uncertain and skewness (n >= 3) / kurtosis (n >= 4) are
undefined or meaningless.

### Assumption checks  (Prism: Normality and lognormality tests; Test for equal variances)
Shapiro-Wilk per sample and on the pooled residuals (value minus its sample mean), Brown-Forsythe (median-centred Levene) and
Bartlett across the samples, and a **Q-Q figure** of the pooled residuals of one metric (fixed panel like the other figures). At n = 3
these are warnings, not proof: with three values per group the practical rule is to use the Welch / Games-Howell family, which does
not assume equal variances.

### Post-hoc comparison (method)  (Prism: One-way ANOVA with multiple comparisons)
The metric (first metric of the Analysis figures) and the reference sample (Stats & Reports) are those of the Replicate comparison
above it. The **omnibus** table shows the ordinary one-way ANOVA (F, eta^2), the Welch ANOVA and the Kruskal-Wallis test (H, epsilon^2);
the **pairwise** table is `posthoc_table()` of the chosen method with stars:

| Method | Use when | Prism name |
|---|---|---|
| Holm-adjusted Welch t (default) | unequal variances, any family; reproduces the `p_adj_holm` column of the Replicate comparison | Holm-Sidak is the Prism analogue |
| Tukey HSD | equal variances, all pairs | Tukey |
| Games-Howell | unequal variances, all pairs (slightly liberal at n = 3) | Games-Howell |
| Dunn | after Kruskal-Wallis; distribution-free (Holm over many pairs often cannot reach 0.05 at n = 3) | Dunn |
| Dunnett | many treatments vs one control: needs the reference sample (and the `mvtnorm` package; without it the table says "fallback: Holm-adjusted Welch t vs reference") | Dunnett |
| Bonferroni / Sidak (Welch t) | a few planned comparisons | Bonferroni / Sidak |

The existing Replicate comparison tables (Welch / Mann-Whitney, Hedges' g, Tukey) are unchanged. The methods draft names the chosen
method when it is not the default.

### Time-course ANOVA  (Prism: Two-way ANOVA, repeated measures; multiple comparisons at each time)
Two-way ANOVA of log(OD): sample x time with the **well** as the repeated-measures subject (a mixed model with a random intercept
per well and AR(1) errors when `nlme` is installed, else an `Error(well)` ANOVA; the method actually used and why are shown; untick
"Repeated measures by well" for the plain, too-liberal, two-way ANOVA). The Sample x Time interaction tests whether the curves differ
in *shape*; the Sample main effect has only (wells - samples) degrees of freedom. Below it, every non-reference sample is compared with
the reference at each time point by Welch t, with p adjusted **across the time points of that sample** (Sidak, Holm or Bonferroni).
The Analysis figure **Per-time comparisons vs reference** draws the mean curves with a star (*, **, ***) above every time at which
p_adj < 0.05, in the sample's colour; the curves are labelled at their ends (no legend), the panel is fixed-size. It needs a reference.

### Area under the curve  (Prism: Area under curve)
Trapezoid area above a baseline (none, the first reading, a fixed value, or the minimum of each replicate) over a time window, per replicate
curve; the table gives n, mean, SD, SEM and the 95 % CI per sample and, with "Relative to the reference", the ratio of mean AUCs with a
log-delta CI (approximate at n = 3). The second table compares the per-replicate AUCs by the post-hoc method above. The Analysis figure
**AUC per sample** is the dot / bar plot of the metric summary (replicates as open circles, the main figure's error measure).

### Outliers  (Prism: Identify outliers)
Grubbs, MAD or IQR screening of the replicate values of each metric within each sample, and a screen of whole replicate curves (mean absolute
distance from the sample's median curve, scaled; > 3 flagged). These tables **flag only**. To exclude a replicate use the Data tab (replicate
exclusions): there is no automatic exclusion. At n = 3 a flag is a prompt to look at the well, not a reason to delete it.

### One-sample tests  (Prism: One sample t test and Wilcoxon test)
Type a hypothetical value per metric (`doubling_time = 20; lysis_time = 90`) and every sample gets a one-sample t test (with the CI of the
mean) and a Wilcoxon signed-rank test. At n = 3 the Wilcoxon p cannot go below 0.25: use the t test and the CI.

### Metric explorer: correlation and straight-line fit  (Prism: Correlation; Simple linear regression)
In the Analysis figures tab, with the *Metric explorer*: choose Pearson r or Spearman rho (r, 95 % CI by Fisher's z, p) and/or a least-squares line
with its 95 % confidence (or prediction) band; the numbers are in a small table under the figure and its CSV. The points are those plotted (every replicate,
or the mean of each sample); a log axis is fitted on log10. With fewer than 5 points r is not meaningful and the table says so.

## Curve fitting tab

### Overlays and options
Above the figure: a **95 % confidence or prediction band** on the growth fits (delta method on ln(OD/OD0), back-transformed with `exp`; mean-curve fits),
the **fitted lysis decay**, and the **exponential-growth window** shaded; the **interval method** (profile likelihood, asymmetric, or Wald); and the
**weighting** (none, 1/Y, 1/Y^2, 1/SD^2) for the lysis and dose-response fits. The v2.18 growth fitter takes no weights, so the growth fits stay
unweighted (said in the tab). Weighted AICc values are comparable only between fits with the same weights.

### Fit statistics  (Prism: confidence intervals, AICc, Akaike weights, runs test, normality of residuals)
Each growth fit is rebuilt as an `nls` object, with 95 % intervals for A, mu_max and the lag (and the doubling time), adjusted R^2, RMSE, AICc, the
Akaike weight among the converged models of the same curve (a difference of about 2 AICc units or less is a tie; the weights are relative support
in the set compared, not probabilities), the **runs test** (are the points systematically on one side of the curve?) and the Shapiro-Wilk test of the
residuals. Two caveats are built in: a refit is only used when its A, mu and lag agree with the engine's fit (otherwise the interval is Wald from the fit's
own standard errors and the residual tests are blank, said in the *Note* column), and a **lag on its lower bound (0)** has no meaningful interval (left
blank). The **residual figure** plots observed minus fitted on the fit's own scale for the best model of each curve (the first reading, the baseline of every
growth fit, is not plotted).

### Lysis models  (Prism: One phase decay; four-parameter logistic)
After the peak (or from the first reading) the mean curve of each sample, or every replicate, is fitted with a one-phase decay to a plateau and/or a decreasing
four-parameter logistic. The table gives k, half-life (ln 2 / k), t50 (the absolute time at which the fitted OD is half-way between its fitted peak and the plateau)
and the plateau, each with a 95 % interval, with R^2, AICc and the Akaike weight between the two models. A plateau below 0 means the decay has not levelled off in
the data; a curve that does not decay (an uninfected control) does not converge and the table says why.

### Exponential-growth window  (Prism: semi-log line fit)
A log-linear regression of ln(OD) on time over an automatic window (the one of at least *min. points* points with the highest R^2), a typed window, or all points:
mu and the doubling time with 95 % intervals, next to the rolling mu_max of the curve metrics. R^2 favours short windows, so raise the minimum number of points or type
the window you chose by eye; the window is data-driven, so its interval is optimistic.

### Compare fits  (Prism: Compare: do the best-fit values differ between data sets?)
Extra-sum-of-squares F test between the chosen samples for one growth model: one curve for all samples against separate curves (or only mu, the lag or A separate; each sample
with its own baseline). The table gives F, df, p, delta AICc and the preferred model; the second table the per-sample parameters of the separate-curves model. Low-n warnings are
shown (fewer than 3 replicate curves, fewer than 10 residual degrees of freedom, "all parameters" being one test while mu, lag and A separately are three).

### Dose-response (4PL)  (Prism: log(agonist) vs response, variable slope)
In the Analysis figures tab, *MOI dose-response*: tick **Fit a four-parameter logistic**, optionally fix the bottom and/or top plateau; the fitted curve (per phage series, i.e. per
label with its MOI token removed), its 95 % confidence band and the **EC50 with its 95 % interval** (dashed line and bar) are drawn on the figure and listed in the table under it (CSV).
With 4 free parameters and 5 or fewer MOIs the interval is wide or missing; fix a plateau where the experiment defines it (for example the bottom at 0).

## Methods text
The "Draft from current settings" paragraph gains, once the Statistics tab has been visited, sentences for the column statistics, assumption checks, the post-hoc method (when it is not the default), the time-course
ANOVA and per-time comparisons, the AUC, the outlier screen, the one-sample tests and the correlation / straight-line fit; once the Curve fitting tab has been used, sentences for the intervals and AICc, the
lysis models, the exponential window, the fit comparison and the 4PL. A session that has not touched them gets exactly the previous draft.

## Dashboard
Two cards: the **AUC** and the **doubling time from the exponential window** of the reference sample (the first sample when none is set), "-" when the quantity cannot be computed.
