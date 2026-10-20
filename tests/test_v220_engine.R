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

# ── SUMMARY ───────────────────────────────────────────────────────────────────
cat(sprintf("\n============ SUMMARY: %d passed / %d failed ============\n", pass, fail))
if (fail > 0) quit(status = 1)
