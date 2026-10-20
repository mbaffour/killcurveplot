# ============================================================================
# v2.20 engine suite -- pure functions only (no Shiny session):
#   1. curve phases (lag / exponential / deceleration / stationary / lysis / recovery / post-lysis)
#   2. bootstrap confidence intervals for mean-curve metrics
#   3. curve differences versus a reference, with a per-time Welch test and first separation time
# Run from the repository root:  Rscript tests/test_v220_engine.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v220_engine.R")

pass <- 0L; fail <- 0L
check <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
near <- function(a, b, tol = 1e-9) length(a) == 1 && is.finite(a) && abs(a - b) < tol

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))

# demo helpers: mean-curve frame and replicate frame from a demo scenario
demo_frames <- function(scenario = "lysis") {
  demo <- env$make_demo_data(scenario)
  vars <- unique(colnames(demo)[-1])
  rep_pd <- do.call(rbind, lapply(vars, function(vn) {
    idx <- which(colnames(demo) == vn)
    do.call(rbind, lapply(seq_along(idx), function(r)
      data.frame(time = demo$Time, variable = vn, replicate = r, value = demo[[idx[r]]], stringsAsFactors = FALSE)))
  }))
  pd <- do.call(rbind, lapply(vars, function(vn)
    data.frame(time = demo$Time, variable = vn,
               mean_value = rowMeans(demo[, colnames(demo) == vn, drop = FALSE]), stringsAsFactors = FALSE)))
  list(pd = pd, rep_pd = rep_pd)
}

# ── 1. Curve phases ───────────────────────────────────────────────────────────
cat("\n--- curve phases: demo mean curves ---\n")
dm <- demo_frames("lysis")
cv <- function(pd, v) { g <- pd[pd$variable == v, ]; list(t = g$time, od = g$mean_value) }
covers <- function(ph, t) nrow(ph) >= 1 && near(ph$t_start[1], min(t)) && near(ph$t_end[nrow(ph)], max(t)) &&
  all(abs(ph$t_start[-1] - ph$t_end[-nrow(ph)]) < 1e-9) && all(ph$duration > 0) &&
  all(abs(ph$duration - (ph$t_end - ph$t_start)) < 1e-9)

c10 <- cv(dm$pd, "Phage MOI 10"); m10 <- env$compute_one_curve_metrics(c10$t, c10$od, "x")
ph10 <- env$curve_phases(c10$t, c10$od)
check("phase table has the documented columns",
      identical(names(ph10), c("phase", "t_start", "t_end", "od_start", "od_end", "duration")))
check("Phage MOI 10: lag -> exponential -> (deceleration) -> lysis, in order, no recovery",
      identical(ph10$phase[1:2], c("lag", "exponential")) && identical(tail(ph10$phase, 1), "lysis") &&
      all(ph10$phase %in% c("lag", "exponential", "deceleration", "stationary", "lysis")))
check("Phage MOI 10: lag ends at lag_phase, lysis starts at lysis_time",
      near(ph10$t_end[ph10$phase == "lag"], m10$lag_phase) && near(ph10$t_start[ph10$phase == "lysis"], m10$lysis_time))
check("Phage MOI 10: the post-peak minimum is the last point, so there is no post-lysis phase to show",
      near(c10$t[which.min(c10$od[(which.max(c10$od) + 1):length(c10$od)]) + which.max(c10$od)], max(c10$t)) &&
      !"post-lysis" %in% ph10$phase)
check("Phage MOI 10: lysis ends where the post-peak minimum is (t[n])",
      near(ph10$t_end[ph10$phase == "lysis"], max(c10$t)) &&
      near(ph10$od_end[ph10$phase == "lysis"], m10$residual_od))
check("Phage MOI 10: phases do not overlap and cover t[1]..t[n]", covers(ph10, c10$t))
check("Phage MOI 10: od_start of the lag phase is the first reading; od_end of lysis the last",
      near(ph10$od_start[1], c10$od[1]) && near(ph10$od_end[nrow(ph10)], tail(c10$od, 1)))

c01 <- cv(dm$pd, "Phage MOI 0.1"); m01 <- env$compute_one_curve_metrics(c01$t, c01$od, "x")
ph01 <- env$curve_phases(c01$t, c01$od)
check("Phage MOI 0.1: lag, exponential, lysis, post-lysis (trough 165 < t[n] = 180)",
      identical(ph01$phase[1:2], c("lag", "exponential")) && identical(tail(ph01$phase, 2), c("lysis", "post-lysis")))
check("Phage MOI 0.1: lysis runs from lysis_time to the trough, post-lysis from the trough to t[n]",
      near(ph01$t_start[ph01$phase == "lysis"], m01$lysis_time) &&
      near(ph01$t_end[ph01$phase == "lysis"], 165) && near(ph01$t_start[ph01$phase == "post-lysis"], 165) &&
      near(ph01$od_start[ph01$phase == "post-lysis"], m01$residual_od))
check("Phage MOI 0.1: phases do not overlap and cover t[1]..t[n]", covers(ph01, c01$t))

cu <- cv(dm$pd, "Uninfected"); phu <- env$curve_phases(cu$t, cu$od)
check("Uninfected: no lysis, recovery or post-lysis phase",
      !any(c("lysis", "recovery", "post-lysis") %in% phu$phase) && identical(phu$phase[1:2], c("lag", "exponential")))
check("Uninfected: phases cover t[1]..t[n]", covers(phu, cu$t))

cat("\n--- curve phases: synthetic curves ---\n")
tt <- seq(0, 300, by = 10)
flat <- env$curve_phases(tt, rep(0.1, length(tt)))
check("a flat curve is one lag phase over the whole series",
      nrow(flat) == 1 && flat$phase == "lag" && near(flat$t_start, 0) && near(flat$t_end, 300) && near(flat$duration, 300))
dec <- env$curve_phases(tt, 1 * exp(-0.01 * tt))
check("a curve that never grows (monotone decay) is one lag phase", nrow(dec) == 1 && dec$phase == "lag")
check("too short / empty input gives a zero-row table, not an error",
      nrow(env$curve_phases(5, 1)) == 0 && nrow(env$curve_phases(numeric(0), numeric(0))) == 0)
check("non-finite points are dropped before the phases are built",
      { x <- env$curve_phases(c(tt, NA), c(rep(0.1, length(tt)), 1)); nrow(x) == 1 && near(x$t_end, 300) })

grow <- function(t, k = 0.04, y0 = 0.05, cap = 1.0) cap / (1 + ((cap - y0) / y0) * exp(-k * t))
od_plat <- grow(tt)
php <- env$curve_phases(tt, od_plat)
mp <- env$compute_one_curve_metrics(tt, od_plat, "x")
check("growth to a plateau, no lysis: lag, exponential, deceleration, stationary to t[n]",
      identical(php$phase, c("lag", "exponential", "deceleration", "stationary")) && near(php$t_end[4], 300))
check("the stationary phase starts inside the run behind stat_phase_dur (>= 95 % of the peak)",
      { s0 <- php$t_start[php$phase == "stationary"]
        run <- env$longest_true_run(od_plat >= 0.95 * mp$max_od)
        s0 >= tt[run[["start"]]] - 1e-9 && near(300 - tt[run[["start"]]], mp$stat_phase_dur) })
check("the exponential phase ends where the rolling slope falls below 50 % of mu_max",
      { rs <- env$phase_ln_slopes(tt, od_plat, 5); mu_i <- which.max(rs)
        j <- which(seq_along(tt) > mu_i & is.finite(rs) & rs < 0.5 * max(rs, na.rm = TRUE))[1]
        near(php$t_end[php$phase == "exponential"], tt[j]) && near(max(rs, na.rm = TRUE), mp$mu_max) })
check("growth to a plateau: phases cover and do not overlap", covers(php, tt))

# growth, a plateau, then lysis: stationary ends at lysis onset
od_pl <- ifelse(tt < 180, grow(tt), grow(180) * exp(-0.05 * (tt - 180)))
pl <- env$curve_phases(tt, od_pl); mpl <- env$compute_one_curve_metrics(tt, od_pl, "x")
check("plateau then lysis: stationary ends exactly at lysis_time and lysis follows",
      "stationary" %in% pl$phase && near(pl$t_end[pl$phase == "stationary"], mpl$lysis_time) &&
      near(pl$t_start[pl$phase == "lysis"], mpl$lysis_time) && identical(tail(pl$phase, 1), "lysis"))
check("plateau then lysis: covers t[1]..t[n], no overlap", covers(pl, tt))

# lysis followed by regrowth: recovery instead of post-lysis
od_rec <- ifelse(tt < 120, grow(tt, k = 0.06), ifelse(tt < 180, grow(120, k = 0.06) * exp(-0.08 * (tt - 120)),
                 grow(120, k = 0.06) * exp(-0.08 * 60) + 0.004 * (tt - 180)))
rc <- env$curve_phases(tt, od_rec); mrc <- env$compute_one_curve_metrics(tt, od_rec, "x")
check("lysis then regrowth (recovery_slope finite): the last phase is 'recovery', not 'post-lysis'",
      is.finite(mrc$recovery_slope) && identical(tail(rc$phase, 1), "recovery") && !"post-lysis" %in% rc$phase)
check("lysis then regrowth: recovery starts at the trough and ends at t[n]; covers, no overlap",
      near(rc$t_start[rc$phase == "recovery"], 180) && covers(rc, tt))

# peak_mode and threshold pass through to the engine
dmr <- demo_frames("resistance")
rcv <- cv(dmr$pd, unique(dmr$pd$variable)[length(unique(dmr$pd$variable))])
phg <- env$curve_phases(rcv$t, rcv$od, peak_mode = "global")
phf <- env$curve_phases(rcv$t, rcv$od, peak_mode = "first_local")
check("peak_mode is passed to the engine (first_local finds lysis on a regrowth curve)",
      covers(phg, rcv$t) && covers(phf, rcv$t) && "lysis" %in% phf$phase)
check("a stricter lysis threshold can only delay or remove the lysis phase",
      { a <- env$curve_phases(c10$t, c10$od, lysis_threshold = 0.05); b <- env$curve_phases(c10$t, c10$od, lysis_threshold = 0.30)
        b$t_start[b$phase == "lysis"] >= a$t_start[a$phase == "lysis"] })

# every demo scenario, every sample: contiguous coverage, no overlap, only known phase names
known <- c("lag", "exponential", "deceleration", "stationary", "lysis", "recovery", "post-lysis")
allok <- TRUE; nsamp <- 0L
for (sc in env$DEMO_SCENARIOS) {
  f <- demo_frames(sc)
  for (v in unique(f$pd$variable)) {
    g <- cv(f$pd, v); ph <- env$curve_phases(g$t, g$od); nsamp <- nsamp + 1L
    allok <- allok && covers(ph, g$t) && all(ph$phase %in% known) && !anyDuplicated(ph$phase)
  }
}
check(sprintf("all %d demo curves (every scenario): contiguous, non-overlapping, one row per phase", nsamp), allok)

bys <- env$curve_phases_by_sample(dm$pd)
check("curve_phases_by_sample: stacked rows with a sample column, matching the per-curve result",
      identical(names(bys), c("sample", "phase", "t_start", "t_end", "od_start", "od_end", "duration")) &&
      setequal(unique(bys$sample), unique(dm$pd$variable)) &&
      identical(bys$phase[bys$sample == "Phage MOI 10"], ph10$phase) &&
      near(bys$t_end[bys$sample == "Uninfected"][1], phu$t_end[1]))
check("curve_phases_by_sample forwards options and handles NULL / empty",
      is.null(env$curve_phases_by_sample(NULL)) && is.null(env$curve_phases_by_sample(dm$pd[0, ])) &&
      { b <- env$curve_phases_by_sample(dm$pd, lysis_threshold = 0.30)
        b$t_start[b$sample == "Phage MOI 10" & b$phase == "lysis"] > ph10$t_start[ph10$phase == "lysis"] })

# ── 2. Bootstrap confidence intervals ─────────────────────────────────────────
cat("\n--- bootstrap_metrics: demo replicates ---\n")
mets <- c("lysis_time", "max_od", "mu_max")
tm <- system.time(bt <- env$bootstrap_metrics(dm$rep_pd, mets, n_boot = 500, seed = 1))
check(sprintf("500 draws x 3 samples x 13 points run in < 5 s (took %.2f s)", tm[["elapsed"]]), tm[["elapsed"]] < 5)
check("result: one row per sample x metric with the documented columns",
      identical(names(bt), c("sample", "metric", "estimate", "boot_mean", "ci_low", "ci_high",
                             "n_replicates", "n_boot_ok", "method")) &&
      nrow(bt) == 9 && all(bt$method == "percentile") && all(bt$n_replicates == 3L))
bl <- function(s, m) bt[bt$sample == s & bt$metric == m, ]
for (s in c("Phage MOI 10", "Phage MOI 0.1")) {
  r <- bl(s, "lysis_time")
  check(sprintf("%s: lysis_time CI is finite and contains the estimate (%.1f in [%.1f, %.1f])",
                s, r$estimate, r$ci_low, r$ci_high),
        is.finite(r$ci_low) && is.finite(r$ci_high) && r$ci_low <= r$estimate && r$estimate <= r$ci_high)
  check(sprintf("%s: every draw lysed (n_boot_ok = 500)", s), r$n_boot_ok == 500L)
}
ru <- bl("Uninfected", "lysis_time")
check("Uninfected: lysis_time is NA with no successful draws",
      is.na(ru$estimate) && is.na(ru$ci_low) && is.na(ru$ci_high) && is.na(ru$boot_mean) && ru$n_boot_ok == 0L)
check("estimate = the metric of the observed mean curve (same as the Curve Metrics table)",
      { cm <- env$compute_curve_metrics(dm$pd)
        all(vapply(seq_len(nrow(bt)), function(i) {
          e <- cm[[bt$metric[i]]][cm$sample == bt$sample[i]]
          (is.na(e) && is.na(bt$estimate[i])) || isTRUE(all.equal(e, bt$estimate[i])) }, logical(1))) })
check("ci_low <= ci_high wherever defined; boot_mean and the interval are NA together",
      all(bt$ci_low <= bt$ci_high, na.rm = TRUE) && all(!is.na(bt$ci_low) == !is.na(bt$boot_mean)))

cat("\n--- bootstrap_metrics: reproducibility, options and edge cases ---\n")
bt2 <- env$bootstrap_metrics(dm$rep_pd, mets, n_boot = 500, seed = 1)
check("same seed -> identical result", identical(bt, bt2))
bt3 <- env$bootstrap_metrics(dm$rep_pd, mets, n_boot = 500, seed = 2)
check("a different seed changes the draws (boot_mean of max_od differs)",
      !isTRUE(all.equal(bt$boot_mean[bt$metric == "max_od"], bt3$boot_mean[bt3$metric == "max_od"])))
only10 <- env$bootstrap_metrics(dm$rep_pd[dm$rep_pd$variable == "Phage MOI 10", ], "lysis_time", n_boot = 500, seed = 1)
check("a sample's draws do not depend on which other samples are loaded",
      isTRUE(all.equal(only10[, setdiff(names(only10), "sample")],
                       bl("Phage MOI 10", "lysis_time")[, setdiff(names(only10), "sample")], check.attributes = FALSE)))
set.seed(123); a1 <- runif(3)
set.seed(123); invisible(env$bootstrap_metrics(dm$rep_pd, "max_od", n_boot = 50, seed = 9)); a2 <- runif(3)
check("the caller's RNG stream is left untouched", identical(a1, a2))
b50 <- env$bootstrap_metrics(dm$rep_pd, mets, n_boot = 50, seed = 1)
check("n_boot = 50 runs; the successful-draw count is at most 50",
      nrow(b50) == 9 && all(b50$n_boot_ok <= 50L) && b50$n_boot_ok[b50$sample == "Phage MOI 10" & b50$metric == "lysis_time"] == 50L)
b10 <- env$bootstrap_metrics(dm$rep_pd, "lysis_time", n_boot = 10, seed = 1)
check("fewer than 20 successful draws -> NA interval, but the estimate and draw count are still reported",
      all(is.na(b10$ci_low)) && all(is.na(b10$boot_mean)) && any(is.finite(b10$estimate)) && max(b10$n_boot_ok) == 10L)
one <- dm$rep_pd[dm$rep_pd$replicate == 1, ]
b1 <- env$bootstrap_metrics(one, c("max_od", "lysis_time"), n_boot = 200)
check("a single replicate gives NA intervals (estimate kept, n_replicates = 1)",
      all(b1$n_replicates == 1L) && all(is.na(b1$ci_low)) && all(b1$n_boot_ok == 0L) && is.finite(b1$estimate[b1$metric == "max_od"][1]))
check("unknown metric names are an error; empty input and no metrics give a zero-row frame",
      inherits(try(env$bootstrap_metrics(dm$rep_pd, "no_such_metric"), silent = TRUE), "try-error") &&
      nrow(env$bootstrap_metrics(NULL, "max_od")) == 0 && nrow(env$bootstrap_metrics(dm$rep_pd, character(0))) == 0)
gap <- dm$rep_pd; gap$value[gap$variable == "Uninfected" & gap$replicate == 2 & gap$time == 75] <- NA
bg <- env$bootstrap_metrics(gap, "max_od", n_boot = 100)
check("a missing reading in one replicate is averaged over the replicates that have it (no error)",
      nrow(bg) == 3 && all(is.finite(bg$estimate)) && all(is.finite(bg$ci_low)))
bc <- env$bootstrap_metrics(dm$rep_pd, "max_od", n_boot = 500, conf = 0.5)
bw <- bt[bt$metric == "max_od", ]
check("conf = 0.5 gives a narrower interval than conf = 0.95",
      all((bc$ci_high - bc$ci_low) <= (bw$ci_high - bw$ci_low) + 1e-12) && any((bc$ci_high - bc$ci_low) < (bw$ci_high - bw$ci_low)))
allm <- setdiff(names(env$compute_one_curve_metrics(1:6, c(1, 2, 3, 4, 5, 6), "p")), c("sample", "steepest_decline_interval"))
tm2 <- system.time(ba <- env$bootstrap_metrics(dm$rep_pd, allm, n_boot = 500))
check(sprintf("every numeric metric at once (%d) also runs in < 5 s (%.2f s)", length(allm), tm2[["elapsed"]]),
      nrow(ba) == 3 * length(allm) && tm2[["elapsed"]] < 5)

# n = 3 intervals are wide and coarse: the width shrinks with more replicates from the same process
set.seed(7)
tt2 <- seq(0, 180, by = 15)
mk_rep <- function(R) do.call(rbind, lapply(seq_len(R), function(r)
  data.frame(time = tt2, variable = "S", replicate = r,
             value = 1.4 / (1 + (1.4 / 0.05 - 1) * exp(-0.028 * tt2)) * (1 + rnorm(length(tt2), 0, 0.12)))))
w3  <- env$bootstrap_metrics(mk_rep(3),  "max_od", n_boot = 500); w3  <- w3$ci_high - w3$ci_low
w12 <- env$bootstrap_metrics(mk_rep(12), "max_od", n_boot = 500); w12 <- w12$ci_high - w12$ci_low
check(sprintf("a larger replicate count gives a narrower interval (width %.3f at n = 3 vs %.3f at n = 12)", w3, w12),
      is.finite(w3) && is.finite(w12) && w12 < w3)

# ── 3. Curve differences versus the reference ─────────────────────────────────
cat("\n--- curve_difference / first_separation_time: demo ---\n")
cd0 <- env$curve_difference(dm$pd, "Uninfected")
check("without rep_pd: sample, time, mean_value, ref_mean, diff only; one row per sample x time",
      identical(names(cd0), c("sample", "time", "mean_value", "ref_mean", "diff")) && nrow(cd0) == nrow(dm$pd))
cd <- env$curve_difference(dm$pd, "Uninfected", dm$rep_pd)
check("with rep_pd: adds n, n_ref, p, p_adj_holm and stars",
      identical(names(cd), c("sample", "time", "mean_value", "ref_mean", "diff", "n", "n_ref", "p", "p_adj_holm", "stars")) &&
      all(cd$n == 3L) && all(cd$n_ref == 3L))
ru <- cd[cd$sample == "Uninfected", ]
check("the reference's own diff is 0 and p / p_adj_holm / stars are NA", all(ru$diff == 0) && all(is.na(ru$p)) &&
      all(is.na(ru$p_adj_holm)) && all(is.na(ru$stars)))
p10 <- cd[cd$sample == "Phage MOI 10", ]
check("diff = mean - reference mean", all(abs(p10$diff - (p10$mean_value - ru$mean_value)) < 1e-12))
check("p is the Welch t-test of the replicates at that time (t = 75)", {
  x <- dm$rep_pd$value[dm$rep_pd$variable == "Phage MOI 10" & dm$rep_pd$time == 75]
  y <- dm$rep_pd$value[dm$rep_pd$variable == "Uninfected" & dm$rep_pd$time == 75]
  near(p10$p[p10$time == 75], t.test(x, y, var.equal = FALSE)$p.value) })
check("p_adj_holm = Holm over the sample's time points, never below the raw p",
      near(max(abs(p10$p_adj_holm - p.adjust(p10$p, "holm")), na.rm = TRUE), 0) &&
      all(p10$p_adj_holm >= p10$p - 1e-12, na.rm = TRUE) && all(p10$p_adj_holm <= 1, na.rm = TRUE))
check("stars follow the app's convention (sig_stars of p_adj_holm)",
      identical(p10$stars, ifelse(is.na(p10$p_adj_holm), NA_character_, env$sig_stars(p10$p_adj_holm))))
check("a phage curve is not separated from the reference before the first sampled decline (t = 0 is NS)",
      !(p10$p_adj_holm[p10$time == 0] < 0.05))

fs <- env$first_separation_time(cd)
check("first_separation_time: one row per sample (reference included, NA)",
      identical(names(fs), c("sample", "first_separation_time")) && nrow(fs) == 3 &&
      is.na(fs$first_separation_time[fs$sample == "Uninfected"]))
for (s in c("Phage MOI 10", "Phage MOI 0.1")) {
  f <- fs$first_separation_time[fs$sample == s]
  check(sprintf("%s separates from Uninfected after its lysis onset (first separation t = %s > 45)", s, f),
        is.finite(f) && f > 45)
}
check("first separation is not before the lysis onset of the sample",
      all(vapply(c("Phage MOI 10", "Phage MOI 0.1"), function(s) {
        g <- cv(dm$pd, s); lt <- env$compute_one_curve_metrics(g$t, g$od, s)$lysis_time
        fs$first_separation_time[fs$sample == s] >= lt - 15 }, logical(1))))
check("alpha is honoured: alpha = 1e-12 leaves nothing, alpha = 1 gives the first testable time",
      all(is.na(env$first_separation_time(cd, 1e-12)$first_separation_time)) &&
      env$first_separation_time(cd, 1.01)$first_separation_time[2] == min(p10$time[is.finite(p10$p_adj_holm)]))

cat("\n--- curve_difference: log2 ratio, edge cases ---\n")
cl <- env$curve_difference(dm$pd, "Uninfected", dm$rep_pd, mode = "log2_ratio")
p10l <- cl[cl$sample == "Phage MOI 10", ]
check("log2_ratio: diff = log2(mean / reference mean); the reference is 0",
      all(abs(p10l$diff - log2(p10l$mean_value / p10l$ref_mean)) < 1e-12) && all(cl$diff[cl$sample == "Uninfected"] == 0))
check("log2_ratio: the Welch test is on log2 readings", {
  x <- log2(dm$rep_pd$value[dm$rep_pd$variable == "Phage MOI 10" & dm$rep_pd$time == 90])
  y <- log2(dm$rep_pd$value[dm$rep_pd$variable == "Uninfected" & dm$rep_pd$time == 90])
  near(p10l$p[p10l$time == 90], t.test(x, y)$p.value) })
nz <- dm$pd; nz$mean_value[nz$variable == "Phage MOI 10" & nz$time == 90] <- 0
check("log2_ratio: a non-positive mean gives NA, not -Inf",
      is.na(env$curve_difference(nz, "Uninfected", mode = "log2_ratio")$diff[
        nz$variable == "Phage MOI 10" & nz$time == 90]))
check("unknown reference is an error; NULL / empty pd gives NULL",
      inherits(try(env$curve_difference(dm$pd, "nope"), silent = TRUE), "try-error") &&
      is.null(env$curve_difference(NULL, "Uninfected")) && is.null(env$curve_difference(dm$pd[0, ], "Uninfected")))
one <- dm$rep_pd[dm$rep_pd$replicate == 1, ]
c1 <- env$curve_difference(dm$pd, "Uninfected", one)
check("a single replicate per group gives NA p (cannot test) but a diff",
      all(is.na(c1$p)) && all(is.finite(c1$diff)) && all(is.na(c1$stars)))
check("essentially constant readings in both groups give NA p, not an error",
      { k <- dm$rep_pd; k$value <- 0.5; z <- env$curve_difference(dm$pd, "Uninfected", k); all(is.na(z$p)) })
check("first_separation_time needs p-values and handles empty input",
      inherits(try(env$first_separation_time(cd0), silent = TRUE), "try-error") &&
      nrow(env$first_separation_time(cd[0, ])) == 0)

# ── SUMMARY ───────────────────────────────────────────────────────────────────
cat(sprintf("\n============ SUMMARY: %d passed / %d failed ============\n", pass, fail))
if (fail > 0) quit(status = 1)
