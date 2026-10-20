# Curve phases, bootstrap intervals and curve differences (v2.20 engine)

These are pure functions in `Lysis_Curve_App_v28.R` (block "v2.20 engine"), tested by
`tests/test_v220_engine.R`. They add views of the data; they do not change any existing metric.
Everything here is built from the mean curve as plotted (or, for the bootstrap and the
separation test, from the replicate readings behind it).

## 1. Curve phases (`curve_phases`, `curve_phases_by_sample`)

`curve_phases(t, od, smooth_window = 5, peak_mode = "global", lysis_threshold = 0.05)` splits one
curve into consecutive phases and returns `phase, t_start, t_end, od_start, od_end, duration`
(`od_*` are the curve values at the boundaries, linearly interpolated between readings).
`curve_phases_by_sample(pd, ...)` does it for every sample of a mean-curve frame
(`time, variable, mean_value`) and stacks the rows with a `sample` column; `...` goes to
`curve_phases()`.

**The phases are reconstructed from the reported metrics.** The function runs
`compute_one_curve_metrics()` on the curve and reads every boundary back from it, so a phase
edge is always a number the metrics table (or the landmarks figure) already shows. They can be
audited against the landmarks figure: the lag edge is `lag_phase`, the lysis edge is `lysis_time`,
the peak is `time_max_od`, and so on. Nothing is fitted and no new threshold is introduced
except the 50 % rule below.

| Phase | From | To | Notes |
|---|---|---|---|
| `lag` | `t[1]` | `lag_phase` | first time the rolling ln-slope reaches 10 % of `mu_max`. Absent when `lag_phase = t[1]` |
| `exponential` | `lag_phase` | first time after the fastest-growth window at which the rolling ln-slope falls below 50 % of `mu_max`; the peak if that happens later or never | same rolling regression as `mu_max` (window = `smooth_window`, windows touching an OD <= 0 give no slope) |
| `deceleration` | end of exponential | start of the stationary run if there is one, else lysis onset, else `t[n]` | growth slowing towards the peak, plus the stretch from the peak to lysis onset when there is no plateau to call stationary |
| `stationary` | start of the near-peak run (never before the end of the exponential phase) | `lysis_time`, or `t[n]` when no lysis is detected | the run is the **longest contiguous run of points >= 95 % of the peak**, the one behind `stat_phase_dur`; omitted when that run does not exist (a single near-peak point) or lysis starts at or inside it |
| `lysis` | `lysis_time` | trough (post-peak minimum) | `lysis_time` is the interpolated onset (peak minus `lysis_threshold`) and the trough is the point behind `residual_od` / `lysis_rate` |
| `recovery` | trough | `t[n]` | only when `recovery_slope` is finite |
| `post-lysis` | trough | `t[n]` | when `recovery_slope` is NA |

Rules worth knowing:

* Phases that do not apply, or have zero length, are **omitted**. An uninfected curve has no
  `lysis`, `recovery` or `post-lysis`; a trough at the last reading leaves no post-trough phase.
* A curve with **no measurable growth** (`mu_max` NA: flat, or monotone decay) is a single `lag`
  phase over the whole series.
* The returned phases are contiguous, never overlap, and cover `t[1]..t[n]` exactly.
* Because `lysis_time` is the 5 %-below-peak crossing (not the peak), the interval between the
  peak and lysis onset belongs to the phase before lysis (`stationary` if there is a plateau,
  otherwise `deceleration`).
* The trough is the **minimum** of the post-peak points. On a curve that has collapsed to a flat
  floor the minimum can be a noise-level point late in the run (the demo's Phage MOI 10 curve
  bottoms out at its last reading, 180 min), in which case `lysis` runs to the end of the series
  and there is no `post-lysis` row. This follows the metrics deliberately (`residual_od`,
  `lysis_rate` and `od_drop` use the same trough); it is not smoothed or re-defined here.
* `peak_mode` and `lysis_threshold` are passed straight to the metrics engine, so
  `first_local` peaks and a different lysis threshold move the phases exactly as they move the
  metrics.

The 50 % rule for the end of the exponential phase is the only boundary that is not itself a
reported metric. Its rolling slope is recomputed with the same recipe as `mu_max` and checked
against the reported `mu_max`; if the two disagree the exponential phase simply runs to the peak.
Because the regression window is centred (5 points by default), a window that reaches into the
first lysis points already has a low slope, so on a sharp lysis curve the exponential phase can
end one sampling interval before the peak.

## 2. Bootstrap confidence intervals (`bootstrap_metrics`)

```r
bootstrap_metrics(rep_pd, metrics, n_boot = 500, seed = 1, conf = 0.95,
                  smooth_window = 5, peak_smooth_window = 1,
                  peak_mode = "global", lysis_threshold = 0.05)
```

`rep_pd` is the un-averaged long data (`time, variable, replicate, value`). For each sample:

1. the replicate curves form a time x replicate matrix;
2. **replicates are resampled with replacement** (`n_replicates` draws), the drawn curves are
   averaged at each time point, and `compute_one_curve_metrics()` is run on that resampled mean
   curve;
3. the requested metric is collected over the `n_boot` draws and the **percentile interval**
   (the `(1-conf)/2` and `1-(1-conf)/2` quantiles of the finite draws, type 7) is reported.

The output has one row per sample and metric: `estimate` (the metric of the *observed* mean
curve, i.e. the Curve Metrics table value), `boot_mean`, `ci_low`, `ci_high`, `n_replicates`,
`n_boot_ok` (finite draws) and `method = "percentile"`. The interval is `NA` when the sample has
fewer than 2 replicates or fewer than 20 finite draws, and `NA` metrics (for example
`lysis_time` of an uninfected curve, which never lyses in any resample) give `NA` with
`n_boot_ok = 0`. When a metric is defined only in some resamples (lysis detected in some but not
all), the interval describes those draws and `n_boot_ok` says how many there were.

**What the interval means.** It describes how much the metric of the *mean curve* would move if
the experiment were repeated with a different set of replicate cultures drawn from the same
population. It is a statement about replicate-to-replicate variability, not about measurement
precision of one well, and not a prediction interval for a single new replicate.

**Why n = 3 gives wide and coarse intervals.** With three replicates there are only
`C(5,3) = 10` distinct resamples (as multisets), so the bootstrap distribution has at most ten
distinct values with very unequal probabilities; the percentile interval then sits on a few
discrete values and is typically very wide relative to its apparent precision. The bootstrap
cannot invent information the replicates do not contain: it uses the observed spread, which is
itself badly estimated from three curves, and the percentile method has no small-sample
correction. Metrics that depend on a threshold crossing between sampled points can also come
out with zero-width intervals when every resample crosses between the same pair of readings
(the demo's `lysis_time` for the high-MOI phage is an example); that reflects the sampling
interval, not certainty. Treat intervals from n = 3 as a rough indication, and prefer more
replicates before reading anything into overlap or non-overlap.

Implementation notes: the per-sample draws depend only on `seed` and the replicate count (so
loading another sample does not change them) and the caller's random-number stream is restored
afterwards. Identical resamples are evaluated once, which is what keeps 500 draws of three
samples well under a second. Replicates that are missing a reading are averaged over the
replicates that have it.

## 3. Curve differences and the separation test (`curve_difference`, `first_separation_time`)

```r
curve_difference(pd, ref_sample, rep_pd = NULL, mode = c("difference", "log2_ratio"))
first_separation_time(x, alpha = 0.05)
```

`curve_difference()` returns, for every sample and time, `mean_value`, `ref_mean` and
`diff`: the mean minus the reference mean (`"difference"`), or `log2(mean / reference mean)`
(`"log2_ratio"`, `NA` where either mean is <= 0). The reference's own rows have `diff = 0`.

With `rep_pd` (`time, variable, replicate, value`) each time point also gets a **Welch
two-sample t-test** (unequal variances) of the sample's replicate readings against the
reference's at that time (`n`, `n_ref` give the group sizes), a Holm-adjusted p
(`p_adj_holm`) computed **across the time points of that sample**, and `stars` (`***` < 0.001,
`**` < 0.01, `*` < 0.05, `ns` otherwise, the same marks as the replicate comparison). In
`"log2_ratio"` mode the test is on the log2 readings (positive readings only), so it asks
whether the *ratio* differs from 1. The reference's own rows and any time with fewer than two
readings in either group, or essentially constant readings, have `NA` p.

`first_separation_time()` returns, for each sample, the first time with `p_adj_holm < alpha`
(`NA` if none; always `NA` for the reference). A figure can use `diff` as the line and mark the
times with `stars`.

This is **exploratory**. Each time point is a separate two-group test on a handful of readings,
Holm only controls the family-wise error across the time points of one sample (not across
samples, and not for the choice of reference), the readings at neighbouring times are not
independent, and with n = 3 per group the Welch test has little power and an unreliable
reference distribution. A first-separation time is "the first sampled time at which this test
could tell the curves apart", not an estimate of when they truly diverge; with coarse sampling
it is later than the true divergence by up to one interval and later still when power is low.

## Tests

```bash
Rscript tests/test_v220_engine.R
```
