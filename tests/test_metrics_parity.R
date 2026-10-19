# ============================================================================
# Parity test: killcurveplot v2.8 metrics engine vs the reference
# implementation from the lysis-paper companion app ("Lysis Curve Ap
# 26.03.13.R"). The port must reproduce the reference numbers exactly.
#
# KC-5: this suite used to `quit(status = 0)` the moment the reference
# clone was missing -- a silent, green no-op for anyone (including CI)
# without `lysis-curve-app` cloned as a sibling directory. Two changes:
#   1. Checks that don't need the reference (rolling_slope vs zoo, the
#      infection-metrics self-consistency / absence checks, and the
#      hand-computed spot checks) now run unconditionally, regardless of
#      whether the reference is available.
#   2. When the reference is missing OR present but unusable (its function
#      signature has drifted since this suite's grab()-based extraction was
#      written -- it is a brittle, line-slicing extraction by design, see
#      the summary in AUDIT-2026-09.md), that is now a visible FAILURE by
#      default, not a silent pass. Set ALLOW_SKIP=1 to downgrade it to a
#      clearly-printed SKIP instead (still a non-green run either way).
# ============================================================================
suppressPackageStartupMessages({
  library(tidyverse); library(zoo)
})

pass <- 0L; fail <- 0L; skip <- 0L
check <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}

# ── Load v2.8 app helpers ────────────────────────────────────────────────────
# Run from the repository root: Rscript tests/test_metrics_parity.R
if (!file.exists("Lysis_Curve_App_v28.R"))
  stop("Run from the repository root: Rscript tests/test_metrics_parity.R")
suppressPackageStartupMessages({
  library(shiny); library(ggpubr); library(scales); library(ggrepel)
  library(ggprism); library(svglite); library(jsonlite)
})
env <- new.env()
invisible(source("Lysis_Curve_App_v28.R", local = env))

# ── rolling_slope parity vs zoo::rollapply (odd AND even windows) ────────────
# Reference-free: exercises only killcurveplot's own rolling_slope() against
# an independent implementation (zoo::rollapply), so this always runs.
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

# ── Extract reference functions from the paper app source (best-effort) ─────
# Default location: the sibling clone. A git worktree under .claude/worktrees/<name>/
# is four directories deeper than the main checkout, so that location is tried too.
ref_name <- "Lysis Curve Ap 26.03.13.R"
ref_candidates <- c(file.path("..", "lysis-curve-app", ref_name),
                    file.path("..", "..", "..", "..", "lysis-curve-app", ref_name))
ref_file <- Sys.getenv("REF_APP", "")
if (!nzchar(ref_file)) {
  hit <- ref_candidates[file.exists(ref_candidates)]
  ref_file <- if (length(hit)) hit[1] else ref_candidates[1]
}
ref_env  <- new.env()
ref_ok   <- FALSE
ref_status <- NULL

if (!file.exists(ref_file)) {
  ref_status <- paste0("reference app not found at ", ref_file,
                       " -- clone https://github.com/mbaffour/lysis-curve-app",
                       " as a sibling directory or set REF_APP")
} else {
  ref_src <- readLines(ref_file, warn = FALSE)
  grab <- function(start_pat, end_pat) {
    s <- grep(start_pat, ref_src)[1]
    e <- grep(end_pat, ref_src); e <- e[e > s][1]
    if (is.na(s) || is.na(e))
      stop("anchor pattern not found -- the reference source has changed shape")
    ref_src[s:(e - 1)]
  }
  # Helper functions / constants the reference engine calls (noise estimate, peak
  # finder, sustain length, ...). Extracted by PARSING the file and evaluating just
  # the named top-level assignments, which is robust to the line-slicing grab()
  # below going stale when a helper is added or moved.
  grab_defs <- function(names_wanted) {
    exprs <- parse(file = ref_file, keep.source = FALSE)
    for (e in exprs)
      if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
          as.character(e[[2]]) %in% names_wanted)
        eval(e, envir = ref_env)
    invisible(NULL)
  }
  ref_ok <- tryCatch({
    grab_defs(c("curve_noise_sd", "LYSIS_SUSTAIN_PTS", "first_prominent_peak_idx",
                  "calculate_local_virulence"))
    eval(parse(text = grab("^calculate_growth_metrics <- function",
                           "Derivative data")), envir = ref_env)
    eval(parse(text = grab("^calculate_infection_metrics <- function",
                           "Replicate Summary")), envir = ref_env)
    if (!is.function(ref_env$calculate_growth_metrics) ||
        !is.function(ref_env$calculate_infection_metrics))
      stop("extraction produced no usable functions")
    # Smoke-test the call itself: grab() only pulls the two named function
    # bodies, so a reference version that split out a helper the function
    # now calls (e.g. a noise-estimate or peak-finder) extracts "cleanly"
    # but errors the first time it actually runs. Catch that here, once,
    # with a clear message, rather than mid-loop below.
    probe <- ref_env$calculate_growth_metrics(
      data.frame(time = c(0, 10, 20), variable = "P", mean_value = c(0.1, 0.2, 0.15)),
      smooth_window = 3)
    if (is.null(probe)) stop("calculate_growth_metrics(probe) returned NULL")
    TRUE
  }, error = function(e) {
    ref_status <<- paste0("reference extraction/call failed: ", conditionMessage(e))
    FALSE
  })
}

if (isTRUE(ref_ok)) {
  check("reference functions extracted and callable", TRUE)
} else {
  msg <- paste0("Reference-app comparison unavailable (", ref_status, ").")
  if (Sys.getenv("ALLOW_SKIP", "0") == "1") {
    skip <- skip + 1L
    cat("SKIP:", msg, "(ALLOW_SKIP=1 set)\n")
  } else {
    check(paste0(msg, " Set ALLOW_SKIP=1 to treat this as a skip instead of a failure."),
          FALSE)
  }
}

# ── Metric parity on a battery of curves (only when the reference works) ────
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
                 "lysis_rate","od_drop","residual_od","recovery_slope",
                 # ported new metrics (first peak / extinction / centroid)
                 "first_peak_od","t_first_peak","extinction_time","centroid_x","centroid_y")

both_equal <- function(a, b) {
  (is.na(a) && is.na(b)) || isTRUE(all.equal(a, b, tolerance = 1e-9))
}

if (isTRUE(ref_ok)) {
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
} else {
  cat("SKIPPING reference-comparison checks (", length(curves) + 1,
      "of them) --", ref_status, "\n")
}

# ── Infection metrics on multi-sample data ───────────────────────────────────
# Building `multi` and running compute_curve_metrics() on it needs no
# reference, so this — and everything below that only reads `new_i` — runs
# unconditionally. Only the direct ref_env comparison is gated on ref_ok.
multi <- bind_rows(
  make_pd(seq(0, 180, 15),
    c(0.05, 0.08, 0.13, 0.22, 0.35, 0.55, 0.80, 1.00, 1.15, 1.22, 1.26, 1.28, 1.29), "Uninfected"),
  make_pd(seq(0, 180, 15),
    c(0.05, 0.08, 0.13, 0.22, 0.35, 0.30, 0.15, 0.08, 0.05, 0.04, 0.04, 0.05, 0.06), "Infected"))
new_i <- env$compute_curve_metrics(multi, smooth_window = 5, ref_sample = "Uninfected")

if (isTRUE(ref_ok)) {
  # HISTORICAL NOTE: the reference app originally used ifelse() with a
  # length-1 condition, so every sample received the FIRST row's relative
  # value (fixed upstream on 2026-08-19). Parity is checked on the
  # alphabetically-first sample ("Infected"), which was correct under both
  # versions; the Uninfected row is checked against ground truth
  # (self-relative must be 1 / 0 / 1 / 1) elsewhere below, unconditionally.
  ref_m <- as.data.frame(ref_env$calculate_growth_metrics(multi, smooth_window = 5))
  ref_i <- as.data.frame(ref_env$calculate_infection_metrics(ref_m, "Uninfected"))
  ok <- TRUE
  for (cc in c("relative_growth","infection_strength","relative_mu_max",
               "relative_max_od","lysis_onset_delta")) {
    a <- ref_i[[cc]][ref_i$sample == "Infected"]; b <- new_i[[cc]][new_i$sample == "Infected"]
    if (!both_equal(a, b)) { cat("  MISMATCH", cc, "Infected:", a, "vs", b, "\n"); ok <- FALSE }
  }
  check("infection metrics match reference on its non-buggy row", ok)
  # centroid_index (reference calculate_infection_metrics) and local_virulence
  # (reference calculate_local_virulence) on both samples
  ok <- TRUE
  for (smp in c("Infected", "Uninfected")) {
    a <- ref_i$centroid_index[ref_i$sample == smp]; b <- new_i$centroid_index[new_i$sample == smp]
    if (!both_equal(a, b)) { cat("  MISMATCH centroid_index", smp, ":", a, "vs", b, "\n"); ok <- FALSE }
  }
  check("centroid_index matches the reference (both samples)", ok)
  ref_lv <- ref_env$calculate_local_virulence(multi, "Uninfected", smooth_window = 5)
  ok <- TRUE
  for (smp in c("Infected", "Uninfected")) {
    a <- ref_lv$local_virulence[ref_lv$sample == smp]; b <- new_i$local_virulence[new_i$sample == smp]
    if (!both_equal(a, b)) { cat("  MISMATCH local_virulence", smp, ":", a, "vs", b, "\n"); ok <- FALSE }
  }
  check("local_virulence matches the reference (both samples)", ok)
  check("local_virulence t_stat matches the reference",
        both_equal(ref_lv$t_stat_used[1],
                   env$calculate_local_virulence(multi, "Uninfected", 5)$t_stat_used[1]))
} else {
  cat("SKIPPING infection-metrics reference comparison --", ref_status, "\n")
}

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
# LC-9: interpolated crossing of the 5 % level (0.38) between t=10 (0.4) and t=20 (0.2) = 11, not the first sampled point past it (20)
check("short4: lysis_time = 11 (interpolated 5% decline crossing, LC-9)", both_equal(m4$lysis_time, 11))
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

cat("\n============ SUMMARY:", pass, "passed /", fail, "failed /", skip, "skipped ============\n")
if (fail > 0) quit(status = 1)
