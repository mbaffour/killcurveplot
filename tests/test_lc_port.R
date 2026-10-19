# ============================================================================
# Tests for the engine-level improvements ported from the companion app
# ("Lysis Curve Ap 26.03.13.R"); see docs/PORTED_FROM_LYSIS_CURVE_APP.md.
#   1. Lysis accuracy        LC-2/3/9/17   noise-aware onset, interpolated
#                                          lysis_time, contiguous stat phase
#   2. New metrics                         first peak, extinction, centroid,
#                                          local virulence, centroid index
#   3. Robust ingest                       sentinel cells, HH:MM:SS time,
#                                          preamble, stacked blocks, replicate col
# Run from the repository root: Rscript tests/test_lc_port.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_lc_port.R")

pass <- 0L; fail <- 0L
check <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
near <- function(a, b, tol = 1e-9) length(a) == 1 && is.finite(a) && abs(a - b) < tol

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))

# ── 1. Lysis-metric accuracy (LC-3, LC-9, LC-17) ─────────────────────────────
cat("\n--- LC-3: curve_noise_sd and the noise-aware, sustained lysis onset ---\n")
check("curve_noise_sd: < 5 points -> 0", env$curve_noise_sd(c(1, 2, 3, 4)) == 0)
check("curve_noise_sd: a straight line (zero curvature) -> 0",
      near(env$curve_noise_sd(seq(0.1, 1, length.out = 12)), 0))
alt <- rep(c(1.03, 0.97), 10)   # +-3 % alternating: |d2| = 4 * 0.03 everywhere
check("curve_noise_sd: alternating +-0.03 -> 0.12 * 1.4826 / sqrt(6)",
      near(env$curve_noise_sd(alt), 0.12 * 1.4826 / sqrt(6), 1e-9))
check("LYSIS_SUSTAIN_PTS is 2", identical(env$LYSIS_SUSTAIN_PTS, 2L))
set.seed(11)
tt <- seq(0, 300, by = 20)
ctrl <- function() { y <- 1.4 / (1 + (1.4 / 0.05 - 1) * exp(-0.045 * tt)); y * (1 + rnorm(length(tt), 0, 0.04)) }
fp <- mean(vapply(seq_len(200), function(i)
  is.finite(env$compute_one_curve_metrics(tt, ctrl(), "U")$lysis_time), logical(1)))
check(sprintf("an uninfected, 4%%-CV noisy plateau is not scored as lysis (false-positive rate %.1f%% <= 5%%)", 100 * fp),
      fp <= 0.05)
dd <- env$make_demo_data()
demo_u <- rowMeans(dd[, colnames(dd) == "Uninfected", drop = FALSE])
check("demo data: the Uninfected control gets no lysis_time",
      is.na(env$compute_one_curve_metrics(dd$Time, demo_u, "U")$lysis_time))
# noise inflates the detection threshold: the SAME sustained 6 % dip is lysis on a clean
# plateau but not on a plateau whose own point-to-point noise exceeds it
plateau_clean <- c(0.05, 0.2, 0.5, 0.8, rep(1.0, 10), 0.94, 0.94, rep(1.0, 9))
n_pc <- length(plateau_clean)
check("a sustained 6 % dip on a clean plateau is lysis", is.finite(
  env$compute_one_curve_metrics(seq_len(n_pc) - 1, plateau_clean, "P")$lysis_time))
plateau_noisy <- c(0.05, 0.3, 0.7, rep(c(1.03, 0.97), 6), 0.93, 0.91, rep(c(1.03, 0.97), 5))
m_pn <- env$compute_one_curve_metrics(seq_along(plateau_noisy) - 1, plateau_noisy, "P")
check("the same dip inside 3 x noise SD is not lysis (and all lysis metrics are NA)",
      is.na(m_pn$lysis_time) && is.na(m_pn$od_drop) && is.na(m_pn$residual_od) && is.na(m_pn$lysis_rate))
single <- c(0.05, 0.2, 0.5, 0.8, rep(1.0, 10), 0.90, rep(1.0, 10))
check("a lone 10 % one-point dip (not sustained for 2 points) is not lysis",
      is.na(env$compute_one_curve_metrics(seq_along(single) - 1, single, "P")$lysis_time))
check("a curve with a single post-peak point cannot sustain a decline (documented consequence)",
      is.na(env$compute_one_curve_metrics(c(0, 10, 20), c(0.1, 0.5, 0.1), "P")$lysis_time))

cat("\n--- LC-9: lysis_time is interpolated, not the first sampled point past the threshold ---\n")
# rise linearly 0.05 -> 1.0 over t = 0..60, peak 1.0 at t = 60, then fall 0.02 per time unit.
# 5 % level = 0.95 is crossed 0.05 / 0.02 = 2.5 time units after the peak -> 62.5 on ANY grid.
lin_curve <- function(by) {
  t <- seq(0, 130, by = by)
  data.frame(t = t, y = ifelse(t <= 60, 0.05 + (1 - 0.05) * t / 60, pmax(1 - 0.02 * (t - 60), 0.05)))
}
for (by in c(5, 10)) {
  lc <- lin_curve(by)
  mm <- env$compute_one_curve_metrics(lc$t, lc$y, "L")
  check(sprintf("clean lysis curve sampled every %g: lysis_time = 62.5 (interpolated between grid points)", by),
        near(mm$lysis_time, 62.5, 1e-9) && !(mm$lysis_time %in% lc$t))
}
mm_a <- env$compute_one_curve_metrics(lin_curve(5)$t,  lin_curve(5)$y,  "L")
mm_b <- env$compute_one_curve_metrics(lin_curve(10)$t, lin_curve(10)$y, "L")
check("lysis_time does not depend on the sampling interval", near(mm_a$lysis_time, mm_b$lysis_time))
mm_t <- env$compute_one_curve_metrics(lin_curve(10)$t, lin_curve(10)$y, "L", lysis_threshold = 0.10)
check("lysis_threshold argument still moves the interpolated level (10 %: 60 + 0.10/0.02 = 65)",
      near(mm_t$lysis_time, 65, 1e-9))
check("lysis_time sits between the peak and the sampled point past the threshold",
      mm_b$lysis_time > mm_b$time_max_od && mm_b$lysis_time < 70)
check("output columns unchanged by the lysis port (lysis_time/lysis_rate/od_drop/residual_od/recovery_slope present)",
      all(c("lysis_time", "lysis_rate", "od_drop", "residual_od", "recovery_slope", "stat_phase_dur") %in% names(mm_b)))

cat("\n--- LC-17: stat_phase_dur is the longest contiguous near-peak run ---\n")
# two separate near-peak episodes: 1.00 at t = 20, 30 (span 10), then 0.97 at t = 60..90 (span 30)
t2 <- seq(0, 120, by = 10)
y2 <- c(0.10, 0.40, 1.00, 1.00, 0.50, 0.30, 0.97, 0.97, 0.97, 0.97, 0.50, 0.30, 0.20)
m2 <- env$compute_one_curve_metrics(t2, y2, "T")
check("two near-peak episodes: stat_phase_dur = the longer run (30), not the 70-unit span of all of them",
      near(m2$stat_phase_dur, 30))
y3 <- c(0.10, 0.40, 1.00, 1.00, 1.00, 1.00, 0.50, 0.30, 0.97, 0.97, 0.50, 0.30, 0.20)
check("the longer run can be the one containing the peak (30)", near(env$compute_one_curve_metrics(t2, y3, "T")$stat_phase_dur, 30))
check("a single near-peak point has no duration (NA), as before",
      is.na(env$compute_one_curve_metrics(t2, c(0.1, 0.2, 0.4, 0.6, 0.8, 1, 0.7, 0.5, 0.4, 0.3, 0.2, 0.2, 0.2), "T")$stat_phase_dur))

# ── 2. New metrics ───────────────────────────────────────────────────────────
cat("\n--- first prominent peak (first_peak_od / t_first_peak) ---\n")
# lyse-then-regrow: first peak 0.6 at t = 60, trough 0.10, regrowth to 1.0 at the end (global max)
t_rg <- seq(0, 240, by = 15)
y_rg <- c(0.05, 0.10, 0.20, 0.40, 0.60, 0.40, 0.25, 0.15, 0.10, 0.10, 0.15, 0.30, 0.55, 0.80, 0.95, 1.00, 1.00)
m_rg <- env$compute_one_curve_metrics(t_rg, y_rg, "R")
check("regrowth curve: max_od is the global maximum (1.0) but first_peak_od is the first peak (0.6 at t = 60)",
      near(m_rg$max_od, 1.0) && near(m_rg$first_peak_od, 0.6) && near(m_rg$t_first_peak, 60))
m_gr <- env$compute_one_curve_metrics(seq(0, 120, 10), c(0.05, 0.07, 0.1, 0.15, 0.22, 0.33, 0.5, 0.7, 0.9, 1.05, 1.15, 1.2, 1.22), "G")
check("monotone growth (no prominent peak): falls back to the global maximum",
      near(m_gr$first_peak_od, 1.22) && near(m_gr$t_first_peak, 120))
pk <- env$first_prominent_peak_idx(c(0.1, 0.5, 0.2, 0.9, 0.1), prom_min = 0.05)
check("first_prominent_peak_idx: earliest local maximum with enough prominence", pk == 2L)
yb <- c(0.1, 0.30, 0.28, 0.31, 0.2, 0.9, 0.1)   # bumps of prominence 0.02 (t=2) and 0.11 (t=4), then the real peak
check("first_prominent_peak_idx: bumps below the prominence floor are skipped (floor 0.15 -> the 0.9 peak)",
      env$first_prominent_peak_idx(yb, prom_min = 0.15) == 6L)
check("first_prominent_peak_idx: lowering the floor admits the earlier bumps (0.05 -> index 4, 0.01 -> index 2)",
      env$first_prominent_peak_idx(yb, prom_min = 0.05) == 4L && env$first_prominent_peak_idx(yb, prom_min = 0.01) == 2L)
check("first_prominent_peak_idx: NA when < 3 points or no peak",
      is.na(env$first_prominent_peak_idx(c(1, 2), 0.1)) && is.na(env$first_prominent_peak_idx(c(1, 2, 3), 0.1)))
check("smooth3_raw_edges keeps raw values at both edges and averages inside",
      identical(env$smooth3_raw_edges(c(3, 6, 9, 3)), c(3, 6, 6, 3)))

cat("\n--- extinction_time ---\n")
check("extinction_time interpolates the crossing of 0.05 (0.15 at t=10 -> 0.03 at t=20 -> 10 + 10 * 0.10/0.12)",
      near(env$curve_extinction_time(c(0, 10, 20), c(0.5, 0.15, 0.03)), 10 + 10 * 0.10 / 0.12, 1e-9))
check("extinction_time: a curve that starts below the floor reports its first time point",
      near(env$curve_extinction_time(c(5, 10, 20), c(0.04, 0.03, 0.02)), 5))
check("extinction_time: NA when the curve never falls below the floor",
      is.na(env$curve_extinction_time(c(0, 10, 20), c(0.1, 0.5, 0.9))))
check("extinction_time: NA for non-finite floor / single point above it",
      is.na(env$curve_extinction_time(c(0, 10), c(0.5, 0.2), NA)) && is.na(env$curve_extinction_time(0, 0.5)))
check("extinction_threshold argument of the engine moves the floor (0.1 -> 0.1 crossed at t = 10 + 10*0.05/0.12)",
      near(env$compute_one_curve_metrics(c(0, 10, 20), c(0.5, 0.15, 0.03), "E", extinction_threshold = 0.10)$extinction_time,
           10 + 10 * 0.05 / 0.12, 1e-9))

cat("\n--- centroid_x / centroid_y (area under the curve) ---\n")
ce <- env$curve_centroid(c(0, 10), c(0.5, 0.5))
check("rectangle 0..10 x 0.5: centroid (5, 0.25)", near(ce[["x"]], 5) && near(ce[["y"]], 0.25))
ct <- env$curve_centroid(c(0, 10), c(0, 10))
check("triangle y = t on 0..10: centroid (20/3, 10/3)", near(ct[["x"]], 20 / 3) && near(ct[["y"]], 10 / 3))
check("all-zero curve: centroid NA, not NaN or an error",
      all(is.na(env$curve_centroid(c(0, 10, 20), c(0, 0, 0)))))
mc <- env$compute_one_curve_metrics(c(0, 10), c(0, 10), "C")
check("engine reports centroid_x / centroid_y as columns", near(mc$centroid_x, 20 / 3) && near(mc$centroid_y, 10 / 3))
m_one <- env$compute_one_curve_metrics(5, 0.3, "one")
check("NA-safe on a single point: new columns exist and are NA",
      all(c("first_peak_od", "t_first_peak", "extinction_time", "centroid_x", "centroid_y") %in% names(m_one)) &&
      is.na(m_one$centroid_x) && is.na(m_one$extinction_time))
m_two <- env$compute_one_curve_metrics(c(0, 10), c(0.2, 0.4), "two")
check("NA-safe on two points (no prominent peak -> global maximum)", near(m_two$first_peak_od, 0.4))

cat("\n--- demo data: new metrics are finite where expected, NA for the uninfected extinction ---\n")
demo_rep <- function(sc) {
  d <- env$make_demo_data(sc); smp <- unique(colnames(d)[-1])
  do.call(rbind, lapply(smp, function(vn) {
    idx <- which(colnames(d) == vn)
    do.call(rbind, lapply(seq_along(idx), function(r)
      data.frame(time = d[[1]], variable = vn, replicate = r, value = d[[idx[r]]])))
  }))
}
rep_l <- demo_rep("lysis")
mean_l <- aggregate(value ~ time + variable, rep_l, mean); names(mean_l)[3] <- "mean_value"
ml <- env$compute_curve_metrics(mean_l, ref_sample = "Uninfected")
newcols <- c("first_peak_od", "t_first_peak", "centroid_x", "centroid_y", "centroid_index", "local_virulence")
check("demo: first peak, centroid, centroid_index and local_virulence are finite for every sample",
      all(vapply(newcols, function(cc) all(is.finite(ml[[cc]])), logical(1))))
check("demo: the uninfected control's extinction_time is NA (never falls below 0.05 after growth)",
      is.na(ml$extinction_time[ml$sample == "Uninfected"]))
check("demo: a lysing phage curve reaches extinction after its peak (extinction_time > time_max_od)",
      ml$extinction_time[ml$sample == "Phage MOI 10"] > ml$time_max_od[ml$sample == "Phage MOI 10"])
check("demo: first_peak_od <= max_od and the first peak is not after the global peak",
      all(ml$first_peak_od <= ml$max_od + 1e-12) && all(ml$t_first_peak <= ml$time_max_od + 1e-12))

cat("\n--- reference-normalised: centroid_index and local_virulence ---\n")
check("the reference sample's own local_virulence is exactly 0", near(ml$local_virulence[ml$sample == "Uninfected"], 0))
check("the reference sample's own centroid_index is exactly 0", near(ml$centroid_index[ml$sample == "Uninfected"], 0))
check("infected samples have positive local_virulence and centroid_index (< 1)",
      all(ml$local_virulence[ml$sample != "Uninfected"] > 0) &&
      all(ml$centroid_index[ml$sample != "Uninfected"] > 0 & ml$centroid_index[ml$sample != "Uninfected"] < 1))
check("without a reference sample neither column is added",
      !any(c("local_virulence", "centroid_index") %in% names(env$compute_curve_metrics(mean_l))))
# hand-computed: reference y = t on 0..10, sample y = t/2; area to t_stat = 10 -> 1 - 25/50 = 0.5
lv_pd <- rbind(data.frame(time = 0:10, variable = "ref", mean_value = 0:10),
               data.frame(time = 0:10, variable = "S",   mean_value = (0:10) / 2))
lv10 <- env$calculate_local_virulence(lv_pd, "ref", t_stat_override = 10)
check("local_virulence hand check: 1 - A_S / A_ref = 0.5 for a half-height sample",
      near(lv10$local_virulence[lv10$sample == "S"], 0.5) && near(lv10$local_virulence[lv10$sample == "ref"], 0))
lv5.5 <- env$calculate_local_virulence(lv_pd, "ref", t_stat_override = 5.5)
check("local_virulence integrates up to an interpolated point AT t_stat (5.5): still 0.5",
      near(lv5.5$local_virulence[lv5.5$sample == "S"], 0.5) && near(lv5.5$t_stat_used[1], 5.5))
check("local_virulence: NULL for an absent reference",
      is.null(env$calculate_local_virulence(lv_pd, "nope")) && is.null(env$calculate_local_virulence(lv_pd, "")))
# t_stat default: reference grows then plateaus; the default must land at the end of the growth burst
t_r <- seq(0, 120, by = 10)
y_r <- 0.05 + 1.0 / (1 + exp(-(t_r - 40) / 8))
lv_d <- env$calculate_local_virulence(data.frame(time = t_r, variable = "ref", mean_value = y_r), "ref", smooth_window = 3)
check("default t_stat is the first time after fastest growth that the ln-slope drops below 10% of mu_max",
      lv_d$t_stat_used[1] > 40 && lv_d$t_stat_used[1] < max(t_r))
rl_rows <- env$compute_replicate_metric_rows(rep_l, ref_sample = "Uninfected")
check("per-replicate rows carry the new columns including local_virulence / centroid_index",
      all(c(newcols, "extinction_time") %in% names(rl_rows)))
check("the reference replicates' centroid_index averages to exactly 0 (mean of cx*cy products)",
      near(mean(rl_rows$centroid_index[rl_rows$sample == "Uninfected"]), 0, 1e-12))
check("replicate-level metric summary (mean +/- SD) includes the new metrics",
      all(c("first_peak_od_mean", "centroid_x_sd", "extinction_time_mean") %in%
          names(env$compute_replicate_metrics(rep_l))))
check("new metrics are selectable in the metric summary (ANALYSIS_METRIC_LABELS) and defined (METRIC_DEFINITIONS)",
      all(c("first_peak_od", "t_first_peak", "extinction_time", "centroid_x", "centroid_y",
            "centroid_index", "local_virulence") %in% names(env$ANALYSIS_METRIC_LABELS)) &&
      all(c("first_peak_od", "t_first_peak", "extinction_time", "centroid_x", "centroid_y",
            "centroid_index", "local_virulence") %in% names(env$METRIC_DEFINITIONS)) &&
      env$metric_axis_label("extinction_time", "min") == "Extinction time (min)" &&
      env$metric_axis_label("first_peak_od", "min", "OD") == "First-peak OD (OD)")

cat("\n============ SUMMARY:", pass, "passed /", fail, "failed ============\n")
if (fail > 0) quit(status = 1)
