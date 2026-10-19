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

# ── 3. Robust ingest ─────────────────────────────────────────────────────────
write_fixture <- function(lines) { p <- tempfile(fileext = ".csv"); writeLines(lines, p); p }
load_file <- function(path, session) session$setInputs(file = list(name = basename(path), datapath = path))

cat("\n--- ingest helpers (unit) ---\n")
cs <- env$coerce_sentinel_columns(
  data.frame(Time = 1:4, A = c("0.1", "OVER", "0.3", "0.4"), Note = c("a", "b", "c", "d"),
             B = c("0.5", "#SAT", "OVRFLW", "0.8"), stringsAsFactors = FALSE),
  skip_cols = "Time", time_values = c(0, 10, 20, 30))
check("coerce_sentinel_columns: a column with OVER becomes numeric with that cell NA (column kept)",
      is.numeric(cs$data$A) && is.na(cs$data$A[2]) && near(cs$data$A[3], 0.3))
check("coerce_sentinel_columns: #SAT / OVRFLW cells -> NA; genuine text column untouched",
      is.numeric(cs$data$B) && sum(is.na(cs$data$B)) == 2 && is.character(cs$data$Note))
check("coerce_sentinel_columns: report names sample, time and the offending string",
      nrow(cs$report) == 3 && "OVER" %in% cs$report$value &&
      cs$report$time[cs$report$value == "OVER"] == "10" && "A" %in% cs$report$sample)
check("coerce_sentinel_columns: skip_cols are left alone",
      identical(env$coerce_sentinel_columns(data.frame(Time = c("a", "1", "2")), skip_cols = "Time")$data$Time, c("a", "1", "2")))
check("coerce_sentinel_columns: numeric-only data has no report",
      is.null(env$coerce_sentinel_columns(data.frame(Time = 1:3, A = c(0.1, 0.2, 0.3)))$report))

pt <- env$parse_time_column(c("0:00:00", "0:15:00", "1:00:30", "1:30"))
check("parse_time_column: HH:MM:SS (and H:MM) -> minutes", pt$converted && near(pt$values[2], 15) &&
      near(pt$values[3], 60.5) && near(pt$values[4], 90) && identical(pt$unit, "minutes"))
pd_t <- env$parse_time_column(c("2026-03-01 10:00:00", "2026-03-01 10:30:00", "2026-03-01 12:00:00"))
check("parse_time_column: date-times -> minutes from the earliest", pd_t$converted && isTRUE(all.equal(pd_t$values, c(0, 30, 120))))
pn <- env$parse_time_column(c("0", "15", "30"))
check("parse_time_column: plain numbers are not converted", !pn$converted && isTRUE(all.equal(pn$values, c(0, 15, 30))))
check("parse_time_column: unparseable text is not converted (all NA)",
      !env$parse_time_column(c("a", "b"))$converted && all(is.na(env$parse_time_column(c("a", "b"))$values)))

wb <- env$infer_wide_replicates(data.frame(Time = c(0, 10, 20, 0, 10, 20, 0, 10, 20), x = 1), "Time")
check("infer_wide_replicates: 3 stacked blocks, block ids 1,1,1,2,2,2,3,3,3",
      wb$reps == 3 && identical(wb$block_id, rep(1:3, each = 3)))
check("infer_wide_replicates: a monotone time column is one block",
      env$infer_wide_replicates(data.frame(Time = 0:5), "Time")$reps == 1L)
check("infer_wide_replicates: unreadable time -> NULL",
      is.null(env$infer_wide_replicates(data.frame(Time = c("a", "b")), "Time")$reps))
check("detect_replicate_column: finds replicate / well columns, never time/group/value",
      identical(env$detect_replicate_column(data.frame(Time = 1, Sample = "a", Replicate = 1, OD = 1), "Time", "Sample", "OD"), "Replicate") &&
      identical(env$detect_replicate_column(data.frame(t = 1, s = "a", Well = "A1", v = 1), "t", "s", "v"), "Well") &&
      is.null(env$detect_replicate_column(data.frame(Time = 1, Sample = "a", OD = 1), "Time", "Sample", "OD")) &&
      is.null(env$detect_replicate_column(data.frame(Time = 1, rep = 1), "Time", "rep", "Time")))

f_tab <- write_fixture(c("Time\tA\tB", "0\t0.1\t0.2", "10\t0.3\t0.4"))
check("read_csv_locale_aware: a tab-separated file (one column under the comma assumption) is sniffed",
      identical(colnames(env$read_csv_locale_aware(f_tab, "auto")), c("Time", "A", "B")))
f_eu_pre <- write_fixture(c("Instrument;X", "Run;7", "", "Time;A;B", "0;0,1;0,2", "10;0,3;0,4"))
d_eup <- env$read_csv_locale_aware(f_eu_pre, "auto")
check("read_csv_locale_aware: preamble WITH A BLANK LINE above a decimal-comma table (skip counts raw lines)",
      identical(colnames(d_eup), c("Time", "A", "B")) && near(d_eup$A[2], 0.3))
f_us <- write_fixture(c("Time,A,B", "0,0.1,0.2", "10,0.3,0.4"))
check("read_csv_locale_aware: an ordinary file takes the unchanged path (no preamble skip)",
      env$csv_preamble_skip(f_us, ",") == 0L && identical(colnames(env$read_csv_locale_aware(f_us, "auto")), c("Time", "A", "B")))

cat("\n--- ingest through the server (testServer) ---\n")
testServer(app, {
  # (a) OVER / OVRFLW sentinel: the column is KEPT with NA cells
  p_over <- write_fixture(c("Time,WT,WT,Mutant",
                            "0,0.05,0.06,0.04",
                            "10,0.10,OVER,0.08",
                            "20,0.20,0.22,0.12",
                            "30,0.35,0.37,0.15"))
  load_file(p_over, session)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "")
  check("OVER: wide format (not flipped to long by the text column)", !rv$is_long_format)
  check("OVER: both WT replicate columns are kept in col_map", length(rv$col_map[["WT"]]) == 2)
  pd <- prepare_plot_data()
  check("OVER: n = 2 at the clean time points, 1 at the time point with the OVER cell",
        all(pd$n[pd$variable == "WT" & pd$time != 10] == 2) && pd$n[pd$variable == "WT" & pd$time == 10] == 1)
  check("OVER: the mean at that time point is the remaining replicate (0.10)",
        near(pd$mean_value[pd$variable == "WT" & pd$time == 10], 0.10))
  check("OVER: the sentinel report is recorded", !is.null(rv$sentinel_report) && "OVER" %in% rv$sentinel_report$value)

  # (b) Gen5-style preamble above the header (with a blank line inside it)
  p_pre <- write_fixture(c("Software Version,3.08.01",
                           "Experiment File Path:,C:\\Users\\lab\\kinetic.xpt",
                           "Protocol File Path:,C:\\Users\\lab\\growth.prt",
                           "",
                           "Plate Number,Plate 1",
                           "Date,2026-01-02",
                           "Reader Type,Synergy H1",
                           "",
                           "Time,WT,WT,Mutant",
                           "0,0.05,0.06,0.04",
                           "15,0.09,0.10,0.07",
                           "30,0.18,0.20,0.10"))
  load_file(p_pre, session)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "")
  check("preamble: the table header is found (time column 'Time', samples WT and Mutant)",
        identical(rv$time_col, "Time") && setequal(rv$od_vars, c("WT", "Mutant")))
  pd <- prepare_plot_data()
  check("preamble: 3 time points, WT n = 2 at each",
        identical(sort(unique(pd$time)), c(0, 15, 30)) && all(pd$n[pd$variable == "WT"] == 2))

  # (c) HH:MM:SS elapsed time
  p_hms <- write_fixture(c("Time,WT,WT,Mutant",
                           "0:00:00,0.05,0.06,0.04",
                           "0:15:00,0.09,0.10,0.07",
                           "1:00:00,0.30,0.32,0.10"))
  load_file(p_hms, session)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "")
  pd <- prepare_plot_data()
  check("HH:MM:SS: time parsed to minutes (0, 15, 60)", identical(sort(unique(pd$time)), c(0, 15, 60)))
  check("HH:MM:SS: the time axis range and the time-point list follow", identical(rv$all_timepoints, c(0, 15, 60)))
  check("HH:MM:SS: every point survives the is.finite() filter (3 times x 2 samples)", nrow(pd) == 6)

  # (d) stacked time blocks (wide): n = number of blocks per time point
  p_blk <- write_fixture(c("Time,WT,Mutant",
                           "0,0.05,0.04", "10,0.10,0.06", "20,0.20,0.08",
                           "0,0.06,0.05", "10,0.12,0.07", "20,0.22,0.09",
                           "0,0.04,0.03", "10,0.08,0.05", "20,0.18,0.07"))
  load_file(p_blk, session)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "")
  pd <- prepare_plot_data()
  check("stacked blocks: 3 blocks detected", identical(as.integer(rv$n_blocks), 3L))
  check("stacked blocks: n = 3 at every time point of every sample", all(pd$n == 3) && nrow(pd) == 6)
  check("stacked blocks: mean at t=10 for WT = mean(0.10, 0.12, 0.08)",
        near(pd$mean_value[pd$variable == "WT" & pd$time == 10], mean(c(0.10, 0.12, 0.08))))
  rp <- prepare_replicate_data()
  check("stacked blocks: 3 replicate curves per sample, none with a repeated time point",
        setequal(unique(rp$replicate[rp$variable == "WT"]), 1:3) &&
        all(table(rp$variable, rp$replicate, rp$time) <= 1))
  check("stacked blocks: replicate 2 of WT is block 2 (0.06, 0.12, 0.22)",
        isTRUE(all.equal(rp$value[rp$variable == "WT" & rp$replicate == 2][order(rp$time[rp$variable == "WT" & rp$replicate == 2])],
                         c(0.06, 0.12, 0.22))))
  check("stacked blocks: per-replicate metrics run on clean curves (3 replicates reported)",
        env$compute_replicate_metrics(rp)$n_replicates[1] == 3)
  # blocks x duplicate columns: replicates = blocks * columns
  p_blk2 <- write_fixture(c("Time,WT,WT,Mutant", "0,1,2,3", "10,4,5,6", "0,7,8,9", "10,10,11,12"))
  load_file(p_blk2, session)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "")
  check("stacked blocks x replicate columns: n = 2 blocks x 2 columns = 4 for WT; 2 for Mutant",
        { pd2 <- prepare_plot_data(); all(pd2$n[pd2$variable == "WT"] == 4) && all(pd2$n[pd2$variable == "Mutant"] == 2) })
  check("stacked blocks x replicate columns: 4 WT replicate ids",
        setequal(unique(prepare_replicate_data()$replicate[prepare_replicate_data()$variable == "WT"]), 1:4))

  # (e) long format with a replicate column; row order scrambled so that the occurrence
  # order within a time point disagrees with the replicate column
  val <- function(s, r, t) (if (s == "A") 0.1 else 0.5) * r + t / 100
  rows <- expand.grid(t = c(0, 10, 20), s = c("A", "B"), r = 1:3, stringsAsFactors = FALSE)
  rows <- rows[order(rows$s, rows$t, rows$r), ]
  rows <- rows[c(1, 3, 2, 4:nrow(rows)), ]               # swap two rows of the first (A, t=0) group
  rows$v <- mapply(val, rows$s, rows$r, rows$t)
  p_long <- write_fixture(c("Time,Sample,Replicate,OD",
                            paste(rows$t, rows$s, rows$r, rows$v, sep = ",")))
  load_file(p_long, session)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "")
  check("long + replicate column: long format, group = Sample, value = OD (not the numeric Replicate)",
        rv$is_long_format && identical(rv$group_col, "Sample") && identical(rv$value_col, "OD"))
  check("long + replicate column: the Replicate column is detected", identical(rv$replicate_col, "Replicate"))
  pd <- prepare_plot_data()
  check("long + replicate column: n = 3 per (sample, time)", all(pd$n == 3) && nrow(pd) == 6)
  rp <- prepare_replicate_data()
  a2 <- rp[rp$variable == "A" & rp$replicate == 2, ]
  check("long + replicate column: replicate identity comes from the column, not the row order",
        nrow(a2) == 3 && isTRUE(all.equal(a2$value[order(a2$time)], val("A", 2, c(0, 10, 20)))))
  check("long without a replicate column: occurrence order is still used (rv$replicate_col NULL)",
        { load_file(write_fixture(c("Time,Sample,OD", "0,A,0.1", "0,A,0.2", "10,A,0.3", "10,A,0.4")), session)
          is.null(rv$replicate_col) })
})

cat("\n============ SUMMARY:", pass, "passed /", fail, "failed ============\n")
if (fail > 0) quit(status = 1)
