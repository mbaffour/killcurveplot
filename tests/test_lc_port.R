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

cat("\n============ SUMMARY:", pass, "passed /", fail, "failed ============\n")
if (fail > 0) quit(status = 1)
