# ============================================================================
# Parity test: killcurveplot v2.8 metrics engine vs the reference
# implementation from the lysis-paper companion app ("Lysis Curve Ap
# 26.03.13.R"). The port must reproduce the reference numbers exactly.
# ============================================================================
suppressPackageStartupMessages({
  library(tidyverse); library(zoo)
})

pass <- 0L; fail <- 0L
check <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}

# ── Load v2.8 app helpers ────────────────────────────────────────────────────
# Run from the repository root: Rscript tests/test_metrics_parity.R
# Needs a clone of mbaffour/lysis-curve-app for the reference implementation;
# set REF_APP to its app file, or clone it as a sibling directory.
if (!file.exists("Lysis_Curve_App_v28.R"))
  stop("Run from the repository root: Rscript tests/test_metrics_parity.R")
suppressPackageStartupMessages({
  library(shiny); library(ggpubr); library(scales); library(ggrepel)
  library(ggprism); library(svglite); library(jsonlite)
})
env <- new.env()
invisible(source("Lysis_Curve_App_v28.R", local = env))

# ── Extract reference functions from the paper app source ────────────────────
ref_file <- Sys.getenv("REF_APP",
                       file.path("..", "lysis-curve-app", "Lysis Curve Ap 26.03.13.R"))
if (!file.exists(ref_file)) {
  cat("SKIP: reference app not found at", ref_file,
      "\n     clone https://github.com/mbaffour/lysis-curve-app as a sibling",
      "directory or set REF_APP.\n")
  quit(status = 0)
}
ref_src <- readLines(ref_file, warn = FALSE)
grab <- function(start_pat, end_pat) {
  s <- grep(start_pat, ref_src)[1]
  e <- grep(end_pat, ref_src); e <- e[e > s][1]
  ref_src[s:(e - 1)]
}
ref_env <- new.env()
eval(parse(text = grab("^calculate_growth_metrics <- function",
                       "Derivative data")), envir = ref_env)
eval(parse(text = grab("^calculate_infection_metrics <- function",
                       "Replicate Summary")), envir = ref_env)
check("reference functions extracted",
      is.function(ref_env$calculate_growth_metrics) &&
      is.function(ref_env$calculate_infection_metrics))

# ── rolling_slope parity vs zoo::rollapply (odd AND even windows) ────────────
set.seed(7)
t_r <- seq(0, 120, by = 10); y_r <- log(pmax(cumsum(rnorm(13, 0.05, 0.2)) + 1, 1e-9))
for (w in c(3, 4, 5, 6)) {
  zoo_out <- rollapply(seq_along(t_r), width = w, FUN = function(idx) {
    tt <- t_r[idx]; ll <- y_r[idx]
    if (diff(range(tt)) == 0) return(0)
    coef(lm(ll ~ tt))[2]
  }, align = "center", fill = NA)
  mine <- env$rolling_slope(t_r, y_r, w)
  check(paste0("rolling_slope == zoo::rollapply, window ", w),
        isTRUE(all.equal(unname(zoo_out), mine, tolerance = 1e-12)))
}

# ── Metric parity on a battery of curves ─────────────────────────────────────
make_pd <- function(t, y, name = "S") data.frame(time = t, variable = name, mean_value = y)

curves <- list(
  lysis_classic = make_pd(seq(0, 180, 15),
    c(0.05, 0.08, 0.13, 0.22, 0.35, 0.30, 0.15, 0.08, 0.05, 0.04, 0.04, 0.05, 0.06)),
  growth_only   = make_pd(seq(0, 180, 15),
    c(0.05, 0.06, 0.09, 0.15, 0.25, 0.40, 0.60, 0.85, 1.05, 1.20, 1.28, 1.32, 1.33)),
  decline_only  = make_pd(seq(0, 120, 10), exp(-seq(0, 120, 10) / 40)),
  flat          = make_pd(seq(0, 60, 10), rep(0.3, 7)),
  short4        = make_pd(c(0, 10, 20, 30), c(0.1, 0.4, 0.2, 0.05)),   # even-window path
  noisy         = make_pd(seq(0, 300, 20), {set.seed(1); pmax(0.05 + cumsum(rnorm(16, 0.02, 0.08)), 0.01)})
)

shared_cols <- c("initial_od","max_od","time_max_od","final_od","auc","mu_max",
                 "doubling_time","lag_phase","stat_phase_dur","lysis_time",
                 "lysis_rate","od_drop","residual_od","recovery_slope")

both_equal <- function(a, b) {
  (is.na(a) && is.na(b)) || isTRUE(all.equal(a, b, tolerance = 1e-9))
}

for (nm in names(curves)) {
  pd  <- curves[[nm]]
  ref <- as.data.frame(ref_env$calculate_growth_metrics(pd, smooth_window = 5))
  new <- env$compute_curve_metrics(pd, smooth_window = 5)
  # deliberate deviation: on a flat curve the reference reports numerical
  # noise (~1e-18) as mu_max; ours reports NA. Skip those cols there.
  cols <- if (nm == "flat")
    setdiff(shared_cols, c("mu_max", "doubling_time", "lag_phase"))
  else shared_cols
  ok  <- TRUE
  for (cc in cols) {
    if (!both_equal(ref[[cc]], new[[cc]])) {
      cat(sprintf("  MISMATCH %s / %s: ref=%s new=%s\n", nm, cc,
                  format(ref[[cc]]), format(new[[cc]])))
      ok <- FALSE
    }
  }
  check(paste0("base metrics match reference: ", nm), ok)
}

# different smoothing window
pd  <- curves$noisy
ref <- as.data.frame(ref_env$calculate_growth_metrics(pd, smooth_window = 7))
new <- env$compute_curve_metrics(pd, smooth_window = 7)
check("window=7 parity (mu_max, lag)",
      both_equal(ref$mu_max, new$mu_max) && both_equal(ref$lag_phase, new$lag_phase))

# ── Infection metrics parity on multi-sample data ────────────────────────────
multi <- bind_rows(
  make_pd(seq(0, 180, 15),
    c(0.05, 0.08, 0.13, 0.22, 0.35, 0.55, 0.80, 1.00, 1.15, 1.22, 1.26, 1.28, 1.29), "Uninfected"),
  make_pd(seq(0, 180, 15),
    c(0.05, 0.08, 0.13, 0.22, 0.35, 0.30, 0.15, 0.08, 0.05, 0.04, 0.04, 0.05, 0.06), "Infected"))
ref_m  <- as.data.frame(ref_env$calculate_growth_metrics(multi, smooth_window = 5))
ref_i  <- as.data.frame(ref_env$calculate_infection_metrics(ref_m, "Uninfected"))
new_i  <- env$compute_curve_metrics(multi, smooth_window = 5, ref_sample = "Uninfected")
# HISTORICAL NOTE: the reference app originally used ifelse() with a length-1
# condition, so every sample received the FIRST row's relative value (fixed
# upstream on 2026-08-19). Parity is checked on the alphabetically-first
# sample ("Infected"), which was correct under both versions; the Uninfected
# row is checked against ground truth (self-relative must be 1 / 0 / 1 / 1),
# which passes against the fixed reference and fails against the buggy one.
ok <- TRUE
for (cc in c("relative_growth","infection_strength","relative_mu_max",
             "relative_max_od","lysis_onset_delta")) {
  a <- ref_i[[cc]][ref_i$sample == "Infected"]; b <- new_i[[cc]][new_i$sample == "Infected"]
  if (!both_equal(a, b)) { cat("  MISMATCH", cc, "Infected:", a, "vs", b, "\n"); ok <- FALSE }
}
check("infection metrics match reference on its non-buggy row", ok)
u <- new_i[new_i$sample == "Uninfected", ]
check("reference sample vs itself: relative_growth = 1",    both_equal(u$relative_growth, 1))
check("reference sample vs itself: infection_strength = 0", both_equal(u$infection_strength, 0))
check("reference sample vs itself: relative_mu_max = 1",    both_equal(u$relative_mu_max, 1))
check("reference sample vs itself: relative_max_od = 1",    both_equal(u$relative_max_od, 1))
# and hand-check the infected row against direct arithmetic
inf <- new_i[new_i$sample == "Infected", ]
ref_row <- new_i[new_i$sample == "Uninfected", ]
check("infected relative_growth = auc_inf / auc_ref",
      both_equal(inf$relative_growth, inf$auc / ref_row$auc))
check("infected infection_strength = 1 - auc ratio",
      both_equal(inf$infection_strength, 1 - inf$auc / ref_row$auc))
check("infection metrics absent when no reference chosen",
      !"infection_strength" %in% colnames(env$compute_curve_metrics(multi)))
check("infection metrics absent for unknown reference",
      !"infection_strength" %in% colnames(
        env$compute_curve_metrics(multi, ref_sample = "NotASample")))

# ── Hand-computed spot checks (independent of both implementations) ──────────
m4 <- env$compute_curve_metrics(curves$short4, smooth_window = 5)
check("short4: lysis_time = 20 (first >5% decline post-peak)", m4$lysis_time == 20)
check("short4: od_drop = 0.35", both_equal(m4$od_drop, 0.35))
check("short4: residual_od = 0.05", both_equal(m4$residual_od, 0.05))
check("short4: lysis_rate = -0.0175 (lm slope peak->trough)",
      both_equal(m4$lysis_rate, -0.0175))
check("short4: lysis_efficiency = 0.875", both_equal(m4$lysis_efficiency, 0.875))
check("short4: recovery_ratio = 0.125", both_equal(m4$recovery_ratio, 0.125))
check("short4: recovery_slope NA (trough at end)", is.na(m4$recovery_slope))
check("flat: lysis + growth metrics all NA",
      is.na(env$compute_curve_metrics(curves$flat)$lysis_time) &&
      is.na(env$compute_curve_metrics(curves$flat)$mu_max))

cat("\n============ SUMMARY:", pass, "passed /", fail, "failed ============\n")
if (fail > 0) quit(status = 1)
