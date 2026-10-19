# Lysis Curve OD Visualization App  v2.11
# Michael Baffour Awuah / Ramsey Lab
#
# v2.11 -- Analysis figures, replicate statistics, dashboard:
#   SHARED THEME       -- figure_theme_layers(): the main figure's fonts, line
#                         weights, legend and spacing (journal preset included)
#                         as one list applied to every figure the app draws.
#   ANALYSIS FIGURES   -- New main-panel tab: metric summary (per-replicate
#                         points + mean +/- SD, faceted by metric, optional
#                         significance marks vs the reference), specific growth
#                         rate mu(t) vs time (same rolling regression as mu_max),
#                         normalized OD (vs reference or t0), replicate overlay,
#                         MOI dose-response, OD heatmap. Same export devices and
#                         size as the main figure; own panel label.
#   STATISTICS         -- compare_replicate_metric(): Welch t (or Mann-Whitney)
#                         per pair / vs reference, Holm-adjusted p, Hedges' g,
#                         95% CI; one-way ANOVA + Tukey (or Kruskal-Wallis) for
#                         >= 3 groups; explicit low-n warnings. CSV export.
#   DASHBOARD          -- Tab with data cards, key metrics per sample, four
#                         small-multiple figures, statistics, notebook summary.
#   MOI PARSING        -- parse_moi()/moi_map(): MOI read from sample names
#                         ("Phage MOI 10") or a 'sample = MOI' override box.
#
# v2.10 -- Journal-standard figure export (see docs/FIGURE_STANDARDS.md):
#   JOURNAL PRESET     -- One-click "Apply journal preset": sets the figure to a
#                         real column width (Nature 89/120/183 mm, Cell 85/114/174,
#                         ASM 84/175, PLOS 132, Science 55/120/183, or custom mm),
#                         Arial 6-8 pt text, 0.5-0.75 pt lines, Okabe-Ito colours,
#                         legend inside the panel, bold 8 pt panel label (a, b...).
#                         Opt-in: default output is unchanged.
#   EXPORT DEVICES     -- PDF now embeds fonts (quartz on macOS, cairo elsewhere;
#                         grDevices::pdf never embedded and crashed on "Arial").
#                         SVG is written with svglite(fix_text_size = FALSE) so
#                         text stays freely editable in Inkscape (no textLength).
#                         PNG/TIFF/JPEG use ragg when installed; TIFF keeps LZW.
#   MM UNITS + WYSIWYG -- Export size can be given in mm; "True export size"
#                         preview renders the on-screen plot at the exact
#                         physical size (x zoom) so what you see is what
#                         Inkscape opens.
#   FIGURE LEGEND TEXT -- Auto-drafted legend paragraph (measure, n, error type,
#                         MOI/host/phage/medium from the notebook) to paste into
#                         the manuscript, so the artwork itself stays clean.
#   LAYOUT INPUTS      -- Legend position (incl. inside corners), legend border,
#                         legend key size, axis/tick line width (pt), tick length
#                         (mm), panel label + size, explicit Arial/Helvetica fonts.
#   PALETTE WARNING    -- Colorblind/Publication palettes warn when > 8 samples
#                         are loaded (colours would repeat).
#
# v2.8 -- Accuracy fixes + stats/reporting features:
#   REPLICATE FIX      -- CRITICAL: wide-format files with replicate columns
#                         sharing the same name (the documented workflow) were
#                         silently collapsed to only the FIRST column of each
#                         name, so means used n=1 and error bars disappeared.
#                         Columns are now resolved by position (col_map), and
#                         every replicate column contributes to mean/SD/SEM/CI.
#   SETTINGS SAVE FIX  -- 'shape_size' and 'color_palette' were misclassified
#                         as per-sample inputs (prefix collision with 'shape_'
#                         / 'color_') and silently dropped from saved settings
#                         files. They are now saved and restored correctly.
#   RGB->HEX FIX       -- rgb_to_hex() no longer appends a redundant 'FF'
#                         alpha channel when alpha = 1, so preset color
#                         matching works after using the RGB sliders.
#   DATA SUMMARY       -- After loading a CSV the sidebar shows what was
#                         detected: format, time column, samples, and the
#                         number of replicate columns per sample.
#   DEMO DATA          -- One-click demo dataset (3 samples x 3 replicates)
#                         for trying the app without a file.
#   STATS EXPORT       -- Download the summary statistics table (time, sample,
#                         mean, SD, n, SEM, 95% CI) as CSV.
#   CURVE METRICS      -- Per-sample metrics table (initial/final/min/max OD,
#                         AUC, time to half-of-max after peak, steepest decline
#                         rate, max specific growth rate, doubling time) with
#                         CSV export. All metrics computed from the plotted
#                         mean curve (i.e. after time filtering) with
#                         transparent, documented formulas.
#   HTML REPORT        -- One-click single-file HTML report: figure (embedded),
#                         stats + metrics tables, metric definitions, data
#                         provenance, key settings, and R session info.
#   PDF REPORT         -- Same content as a multi-page PDF (no LaTeX/pandoc
#                         needed; uses gridExtra).
#   TIFF LZW           -- TIFF export now uses lossless LZW compression
#                         (journal requirement).
#
# v2.7 -- Bug fixes:
#   SHADOW FIX         -- Shadow/ribbon error display no longer crashes when
#                         points are also enabled. The fill scale conflict
#                         between geom_ribbon (scale_fill_manual) and
#                         geom_point (scale_fill_identity) is resolved by
#                         using a unified scale_fill_manual when shadow mode
#                         is active.
#   NA ERROR FIX       -- Single-replicate samples (where sd() returns NA)
#                         no longer crash error bars or ribbons. NA sd values
#                         are replaced with 0 before computing error bounds.
#   OPERATOR ORDER FIX -- The %||% (null-coalescing) operator is now defined
#                         before its first use in renderUI.
#   ERROR GUARD        -- Error column values are explicitly guarded against
#                         NA before computing err_lo / err_hi bounds.
#
# v2.6 -- Stability & performance improvements for large replicate counts:
#   OBSERVER LEAK FIX  -- Color observeEvent handlers are registered at most
#                         once per sample ID (tracked in registered_color_obs).
#   POINT LOOP -> O(1) -- build_plot() no longer adds one geom_point() layer
#                         per sample; shape/fill are vectorised into the data
#                         frame and a single geom_point() call covers all.
#   PIVOT GUARD        -- prepare_plot_data() pre-selects only the requested
#                         sample columns before pivot_longer().
#   VECTORISED FILTER  -- apply_time_filters() uses outer() matrix comparison
#                         instead of per-row vapply for time-point exclusions.
#   DEBOUNCE           -- Main plot render waits 400 ms after the last input
#                         change before rebuilding, preventing render pile-up.
#   CRASH GUARD        -- build_plot() wrapped in tryCatch(); errors display
#                         as an in-plot message rather than crashing the session.
#   PAGINATION         -- Variable Styling panel shows at most 20 samples at a
#                         time with Prev/Next navigation; keeps the DOM small.on App  v2.5
# Michael Baffour Awuah / Ramsey Lab
#
# v2.5 changes:
#   SETTINGS  – Save/load now stores BOTH per-sample aesthetics (color, shape,
#               linetype keyed by sample name) AND all global visual settings.
#               On import: matched samples get their saved aesthetics applied
#               automatically; a status panel shows exactly which names matched
#               and which didn't.  "Clear Imported" fully revokes everything and
#               restores defaults — per-sample overrides included.
#
#   GIF       – Rebuilt without gganimate. Each frame is a real ggplot rendered
#               to PNG via ragg/png, then stitched with gifski. Full fidelity:
#               same theme, axes, error bars, shapes, colors as the main plot.
#               Each frame adds one more line (cumulative reveal).
#
#   PPTX      – Each slide is a pixel-perfect render of build_plot() with the
#               cumulative subset + grey-out of non-active samples. Final slide
#               shows all samples in full color. Slide dimensions driven by the
#               export width/height inputs.

# ── Packages ──────────────────────────────────────────────────────────────────
library(shiny)
library(tidyverse)
library(ggpubr)
library(scales)
library(ggrepel)
library(ggprism)
library(svglite)
library(jsonlite)

has_officer   <- requireNamespace("officer",   quietly = TRUE)
has_rvg       <- requireNamespace("rvg",       quietly = TRUE)
has_gifski    <- requireNamespace("gifski",    quietly = TRUE)
has_base64enc <- requireNamespace("base64enc", quietly = TRUE)
has_gridExtra <- requireNamespace("gridExtra", quietly = TRUE)
has_ragg      <- requireNamespace("ragg",      quietly = TRUE)

# Single source of truth for the app version. Used in the HTML report footer
# and the saved-settings JSON so a bundle/session can be tied back to the code
# that produced it (audit KC-8: the settings JSON used to hardcode "2.5").
APP_VERSION <- "2.11.0"

# ── Helpers ───────────────────────────────────────────────────────────────────

safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

normalize_hex_color <- function(hex) {
  if (is.null(hex) || is.na(hex) || nchar(trimws(hex)) == 0) return("#000000")
  hex <- trimws(hex)
  if (!grepl("^#", hex)) hex <- paste0("#", hex)
  if (grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", hex)) return(toupper(hex))
  "#000000"
}

rgb_to_hex <- function(r, g, b, a = 1) {
  rr <- min(max(as.integer(r), 0), 255) / 255
  gg <- min(max(as.integer(g), 0), 255) / 255
  bb <- min(max(as.integer(b), 0), 255) / 255
  aa <- min(max(a, 0), 1)
  # Only emit an 8-digit hex when alpha is actually < 1; a trailing "FF"
  # breaks equality checks against the 6-digit preset colors.
  if (aa >= 1) rgb(rr, gg, bb) else rgb(rr, gg, bb, aa)
}

# KC-11: read.csv() with no arguments assumes the US/UK locale (comma field
# separator, "." decimal mark). A European export ("1,5E+08" values with
# ";" field separators) then parses every numeric column to character, and
# those get silently dropped later by the numeric coercion + !is.na(cfu)-
# style filters -- the user sees missing data with no clue why. Sniff the
# separator from the header line (a semicolon-delimited file has no commas
# in its header) unless the user picked a locale explicitly.
detect_csv_locale <- function(path) {
  first_line <- tryCatch(readLines(path, n = 1, warn = FALSE), error = function(e) "")
  if (length(first_line) == 0) return("us")
  if (grepl(";", first_line, fixed = TRUE)) "eu" else "us"
}

read_csv_locale_aware <- function(path, locale = "auto") {
  loc <- if (identical(locale, "auto")) detect_csv_locale(path) else locale
  sep <- if (identical(loc, "eu")) ";" else ","
  dec <- if (identical(loc, "eu")) "," else "."
  tryCatch(
    read.csv(path, sep = sep, dec = dec, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e)
      read.csv(path, sep = sep, dec = dec, stringsAsFactors = FALSE, check.names = TRUE)
  )
}

parse_excluded_timepoints <- function(text, all_timepoints) {
  if (is.null(text) || nchar(trimws(text)) == 0) return(numeric(0))
  parts <- strsplit(trimws(text), "[,; ]+")[[1]]
  vals  <- suppressWarnings(as.numeric(parts))
  vals  <- vals[!is.na(vals)]
  if (length(vals) == 0 || is.null(all_timepoints)) return(numeric(0))
  matched <- sapply(vals, function(v) {
    diffs <- abs(all_timepoints - v)
    tol   <- max(abs(all_timepoints)) * 0.001 + 0.001
    if (min(diffs) <= tol) all_timepoints[which.min(diffs)] else NA_real_
  })
  unique(matched[!is.na(matched)])
}

# ── Curve metrics ─────────────────────────────────────────────────────────────
# All metrics are computed from the per-sample MEAN curve exactly as plotted
# (i.e. after time filtering / point exclusion), sorted by time.
#
# The growth/lysis engine (mu_max, lag_phase, stat_phase_dur, lysis_time,
# lysis_rate, od_drop, residual_od, recovery_slope, and the reference-based
# infection metrics) is a port of calculate_growth_metrics() /
# calculate_infection_metrics() from the companion analysis app used for the
# N4 lysis paper ("Lysis Curve Ap 26.03.13.R"), verified against it by a
# parity test suite. The rolling regression is implemented in base R (no zoo
# dependency) with identical centered-window semantics.
#
# Two DELIBERATE corrections vs the reference implementation:
#  1. Infection metrics: the reference uses ifelse() with a length-1
#     condition, which silently recycles the FIRST sample's relative value
#     to every sample. Here each sample gets its own correct ratio.
#  2. mu_max requires a rolling slope > 1e-10 (reference: > 0), so an exactly
#     flat curve is not reported as growing with doubling time ~ 1e17 due to
#     floating-point noise in the regression.

# Centered rolling slope of y ~ t over consecutive index windows of `win`
# points; replicates zoo::rollapply(..., align = "center", fill = NA).
rolling_slope <- function(t, y, win) {
  n   <- length(t)
  out <- rep(NA_real_, n)
  if (win > n || win < 2) return(out)
  offset <- floor((win - 1) / 2)   # zoo places even-width windows left of center
  for (s in seq_len(n - win + 1)) {
    idx <- s:(s + win - 1)
    tt  <- t[idx]; yy <- y[idx]
    out[s + offset] <- if (diff(range(tt)) == 0) 0 else
      unname(coef(lm(yy ~ tt))[2])
  }
  out
}

# KC-3: simple centered moving average, used ONLY to locate the peak (see
# peak_smooth_window below) -- not to alter any reported OD value. Edge
# points use a smaller, still-centered window rather than padding with NA,
# so every point gets a value. w = 1 (the default) is a no-op: half = 0
# makes every window just the point itself, returning y unchanged.
moving_average <- function(y, w) {
  n <- length(y)
  if (!is.finite(w) || w <= 1 || n < 2) return(y)
  half <- (w - 1) %/% 2
  vapply(seq_len(n), function(i) {
    lo <- max(1, i - half); hi <- min(n, i + half)
    mean(y[lo:hi], na.rm = TRUE)
  }, numeric(1))
}

# KC-4: the full per-curve metrics engine, factored out into its own function
# so the EXACT SAME formulas run on a per-replicate curve
# (compute_replicate_metrics(), below) as on the mean curve
# (compute_curve_metrics(), below) -- not a re-implementation that could
# silently drift from it. Returns one row of metrics for a single
# (t_vec, od_vec) curve.
#
# peak_smooth_window: OFF by default (1 = no smoothing). At the app's own demo
# noise level (~4%), which(max)/lysis-onset detection on the raw curve can
# move by a whole timepoint between otherwise-identical noise draws (see
# AUDIT-2026-09.md, KC-3), which cascades into lysis_time/lysis_rate/od_drop/
# residual_od/lysis_efficiency/recovery_ratio/t_half_max_decline, all anchored
# on the peak index. When > 1, the peak (and only the peak) is located on a
# centered moving average of the curve instead of the raw curve; every
# reported OD value still comes from the raw data at that index -- smoothing
# only decides WHERE the peak is, never WHAT its value is.
compute_one_curve_metrics <- function(t_vec, od_vec, sample_name,
                                      smooth_window = 5, peak_smooth_window = 1) {
    n_pts  <- length(t_vec)
    peak_vec <- if (peak_smooth_window > 1) moving_average(od_vec, peak_smooth_window) else od_vec
    max_idx <- which.max(peak_vec); min_idx <- which.min(od_vec)
    initial_od <- od_vec[1]
    max_od     <- od_vec[max_idx]
    final_od   <- od_vec[n_pts]

    auc <- if (n_pts >= 2)
      sum(diff(t_vec) * (head(od_vec, -1) + tail(od_vec, -1)) / 2) else NA_real_

    # ── mu_max + lag phase via centered rolling regression on log(OD) ────────
    # (paper method: window of `smooth_window` points; lag = first time the
    # rolling slope reaches 10% of mu_max)
    mu_max <- NA_real_; lag_phase <- NA_real_
    win <- min(smooth_window, n_pts)
    # KC-2: a non-positive OD has no logarithm. The old clamp,
    # log(pmax(od_vec, 1e-9)), mapped e.g. a -0.002 blank-subtraction
    # artefact (routine after plate-reader blanking) onto log(1e-9) = -20.7
    # -- about twenty log units below every real reading -- so the rolling
    # window containing it fit an enormous artificial slope that always won
    # max(), inflating mu_max ~2.8x on a representative test curve and
    # understating doubling time to match. Treat non-positive OD as missing
    # for this log-scale metric instead of clamping it: any window that
    # touches one gets no slope at all, and the count is reported below so
    # an affected sample is easy to spot.
    n_nonpositive_od <- sum(is.finite(od_vec) & od_vec <= 0)
    if (n_pts >= win && win >= 3) {
      pos_od <- is.finite(od_vec) & od_vec > 0
      log_od <- rep(NA_real_, n_pts)
      log_od[pos_od] <- log(od_vec[pos_od])
      roll_slopes <- tryCatch(rolling_slope(t_vec, log_od, win),
                              error = function(e) rep(NA_real_, n_pts))
      if (n_nonpositive_od > 0) {
        offset <- floor((win - 1) / 2)
        for (s in seq_len(n_pts - win + 1)) {
          idx <- s:(s + win - 1)
          if (any(!pos_od[idx])) roll_slopes[s + offset] <- NA_real_
        }
      }
      # deviation from the reference engine: require slopes > 1e-10 (not > 0)
      # so floating-point noise on a flat curve is not reported as growth
      # (the reference can emit mu_max ~ 1e-18 -> doubling time ~ 1e17)
      valid <- which(is.finite(roll_slopes) & roll_slopes > 1e-10)
      if (length(valid) > 0) {
        mu_max    <- max(roll_slopes[valid], na.rm = TRUE)
        threshold <- 0.10 * mu_max
        lag_idx   <- which(roll_slopes >= threshold & is.finite(roll_slopes))
        lag_phase <- if (length(lag_idx) > 0) t_vec[lag_idx[1]] else NA_real_
      }
    }
    doubling_time <- if (is.finite(mu_max) && mu_max > 0) log(2) / mu_max else NA_real_

    # ── Stationary phase duration: time spent within 95% of max OD ───────────
    stat_phase_dur <- NA_real_
    if (max_idx > 1) {
      near_max <- which(od_vec >= 0.95 * max_od)
      if (length(near_max) >= 2)
        stat_phase_dur <- t_vec[max(near_max)] - t_vec[min(near_max)]
    }

    # ── Lysis detection (paper method) ───────────────────────────────────────
    # onset = first post-peak point whose decline from peak exceeds 5% of peak;
    # rate  = linear-regression slope from peak to the post-peak trough
    lysis_time <- NA_real_; lysis_rate <- NA_real_
    od_drop <- NA_real_; residual_od <- NA_real_; recovery_slope <- NA_real_
    if (max_idx < n_pts) {
      post_peak  <- (max_idx + 1):n_pts
      od_decline <- max_od - od_vec[post_peak]
      lysis_idx  <- which(od_decline > 0.05 * max_od)
      if (length(lysis_idx) > 0) {
        lysis_time <- t_vec[post_peak[lysis_idx[1]]]
        trough_idx <- max_idx + which.min(od_vec[post_peak])
        if (trough_idx > max_idx) {
          lw <- max_idx:trough_idx
          if (length(lw) >= 2)
            lysis_rate <- tryCatch(
              unname(coef(lm(od_vec[lw] ~ t_vec[lw]))[2]),
              error = function(e) NA_real_)
        }
        post_min    <- min(od_vec[post_peak], na.rm = TRUE)
        od_drop     <- max_od - post_min
        residual_od <- post_min
        if (trough_idx < n_pts) {
          rw <- trough_idx:n_pts
          if (length(rw) >= 3) {
            rec <- tryCatch(unname(coef(lm(od_vec[rw] ~ t_vec[rw]))[2]),
                            error = function(e) NA_real_)
            recovery_slope <- if (is.finite(rec) && rec > 0) rec else NA_real_
          }
        }
      }
    }

    # ── Additional transparent metrics ───────────────────────────────────────
    half <- max_od * 0.5
    t50  <- NA_real_
    if (max_idx < n_pts) {
      at <- t_vec[max_idx:n_pts]; ay <- od_vec[max_idx:n_pts]
      below <- which(ay < half)
      if (length(below)) {
        j <- below[1]
        if (j == 1) t50 <- at[1]
        else {
          t1 <- at[j-1]; t2 <- at[j]; y1 <- ay[j-1]; y2 <- ay[j]
          t50 <- if (y2 == y1) t2 else t1 + (half - y1) * (t2 - t1) / (y2 - y1)
        }
      }
    }
    dec_rate <- NA_real_; dec_int <- NA_character_
    if (n_pts >= 2) {
      dt <- diff(t_vec)
      slopes <- ifelse(dt > 0, diff(od_vec) / dt, NA_real_)
      if (any(is.finite(slopes))) {
        k <- which.min(ifelse(is.finite(slopes), slopes, Inf))
        dec_rate <- slopes[k]
        dec_int  <- paste0(t_vec[k], "–", t_vec[k+1])
      }
    }

    data.frame(
      sample          = sample_name,
      n_timepoints    = n_pts,
      t_start         = t_vec[1],
      t_end           = t_vec[n_pts],
      initial_od      = initial_od,
      max_od          = max_od,
      time_max_od     = t_vec[max_idx],
      # KC-10: named global_min_od (not min_od) because on a lysis curve
      # "minimum OD" invites the other reading -- the post-lysis floor,
      # which is residual_od, not this. This is genuinely the curve's
      # global minimum, usually the inoculum at t_start.
      global_min_od      = od_vec[min_idx],
      time_global_min_od = t_vec[min_idx],
      final_od        = final_od,
      auc             = auc,
      mu_max          = mu_max,
      n_nonpositive_od = n_nonpositive_od,
      doubling_time   = doubling_time,
      lag_phase       = lag_phase,
      stat_phase_dur  = stat_phase_dur,
      lysis_time      = lysis_time,
      lysis_rate      = lysis_rate,
      od_drop         = od_drop,
      residual_od     = residual_od,
      recovery_slope  = recovery_slope,
      lysis_efficiency = if (max_od > 0 && is.finite(od_drop)) od_drop / max_od else NA_real_,
      recovery_ratio  = if (max_od > 0) final_od / max_od else NA_real_,
      fold_change_final_vs_initial = if (initial_od != 0) final_od / initial_od else NA_real_,
      pct_drop_from_max = if (max_od != 0) (max_od - final_od) / max_od * 100 else NA_real_,
      t_half_max_decline        = t50,
      steepest_decline_rate     = dec_rate,
      steepest_decline_interval = dec_int,
      stringsAsFactors = FALSE
    )
}

# ref_sample: optional name of an uninfected/reference sample; when given (and
# present in the data) the five reference-normalised infection metrics are added.
compute_curve_metrics <- function(pd, smooth_window = 5, ref_sample = NULL,
                                  peak_smooth_window = 1) {
  if (is.null(pd) || nrow(pd) == 0) return(NULL)
  pd <- pd[order(pd$variable, pd$time), , drop = FALSE]
  out <- lapply(split(pd, pd$variable, drop = TRUE), function(g) {
    compute_one_curve_metrics(g$time, g$mean_value, g$variable[1],
                              smooth_window, peak_smooth_window)
  })
  m <- do.call(rbind, c(out, list(make.row.names = FALSE)))

  # ── Reference-normalised infection metrics (paper method) ──────────────────
  if (!is.null(ref_sample) && nzchar(ref_sample) && ref_sample %in% m$sample) {
    ref <- m[m$sample == ref_sample, , drop = FALSE]
    ref_auc    <- mean(ref$auc,    na.rm = TRUE)
    ref_mu_max <- mean(ref$mu_max, na.rm = TRUE)
    ref_max_od <- mean(ref$max_od, na.rm = TRUE)
    m$relative_growth    <- if (is.finite(ref_auc) && ref_auc > 0)
      m$auc / ref_auc else NA_real_
    m$infection_strength <- if (is.finite(ref_auc) && ref_auc > 0)
      1 - (m$auc / ref_auc) else NA_real_
    m$relative_mu_max    <- if (is.finite(ref_mu_max) && ref_mu_max > 0)
      m$mu_max / ref_mu_max else NA_real_
    m$relative_max_od    <- if (is.finite(ref_max_od) && ref_max_od > 0)
      m$max_od / ref_max_od else NA_real_
    m$lysis_onset_delta  <- m$lysis_time - m$lag_phase
  }
  m
}

# KC-4: compute_curve_metrics() above runs the metrics engine ONCE per
# sample, on the already-averaged mean curve -- so every one of its 27+
# metrics is a single number with no replicate-to-replicate uncertainty and
# no path to "is this difference real". This is additive alongside it, not
# a replacement: it runs compute_one_curve_metrics() once per INDIVIDUAL
# replicate curve, then reports mean +/- SD across replicates for every
# metric. The two are NOT the same number -- the mean of per-replicate
# metrics is generally not equal to the metric of the mean curve, because
# most of these metrics (mu_max, lysis_time, ...) are nonlinear functions
# of the curve -- so both are kept, clearly labelled, rather than one being
# presented as if it were the other.
#
# rep_pd: long, UN-averaged data with columns time, variable, replicate,
# value (see prepare_replicate_data() in the server). "replicate" is an
# arbitrary but stable per-sample index -- for wide files it is the
# replicate column's position (matching rv$col_map); for long files with no
# explicit replicate-ID column it is the row's occurrence order within its
# (sample, time) group, which assumes same-replicate rows appear in a
# consistent relative order across time points.
# v2.11: the per-replicate rows (one metrics row per replicate curve) are
# factored out so the analysis figures and the statistics can use them
# directly. With ref_sample, the reference-normalised metrics are added per
# replicate, each replicate divided by the reference sample's replicate MEAN
# (same definitions as compute_curve_metrics).
compute_replicate_metric_rows <- function(rep_pd, smooth_window = 5, peak_smooth_window = 1,
                                          ref_sample = NULL) {
  if (is.null(rep_pd) || nrow(rep_pd) == 0) return(NULL)
  rep_pd <- rep_pd[is.finite(rep_pd$value), , drop = FALSE]
  if (nrow(rep_pd) == 0) return(NULL)
  rep_pd <- rep_pd[order(rep_pd$variable, rep_pd$replicate, rep_pd$time), , drop = FALSE]
  rep_key <- interaction(rep_pd$variable, rep_pd$replicate, drop = TRUE)

  per_rep <- lapply(split(rep_pd, rep_key, drop = TRUE), function(g) {
    row <- compute_one_curve_metrics(g$time, g$value, g$variable[1],
                                     smooth_window, peak_smooth_window)
    row$replicate <- g$replicate[1]
    row
  })
  rm <- do.call(rbind, c(per_rep, list(make.row.names = FALSE)))
  if (!is.null(ref_sample) && nzchar(ref_sample) && ref_sample %in% rm$sample) {
    ref     <- rm[rm$sample == ref_sample, , drop = FALSE]
    ref_auc <- mean(ref$auc,    na.rm = TRUE)
    ref_mu  <- mean(ref$mu_max, na.rm = TRUE)
    ref_max <- mean(ref$max_od, na.rm = TRUE)
    rm$relative_growth    <- if (is.finite(ref_auc) && ref_auc > 0) rm$auc / ref_auc        else NA_real_
    rm$infection_strength <- if (is.finite(ref_auc) && ref_auc > 0) 1 - rm$auc / ref_auc    else NA_real_
    rm$relative_mu_max    <- if (is.finite(ref_mu)  && ref_mu  > 0) rm$mu_max / ref_mu      else NA_real_
    rm$relative_max_od    <- if (is.finite(ref_max) && ref_max > 0) rm$max_od / ref_max     else NA_real_
  }
  rm
}

compute_replicate_metrics <- function(rep_pd, smooth_window = 5, peak_smooth_window = 1) {
  rm <- compute_replicate_metric_rows(rep_pd, smooth_window, peak_smooth_window)
  if (is.null(rm)) return(NULL)

  safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  safe_sd   <- function(x) if (sum(!is.na(x)) < 2) NA_real_ else sd(x, na.rm = TRUE)

  metric_cols <- setdiff(names(rm)[vapply(rm, is.numeric, logical(1))],
                         c("replicate"))
  out <- lapply(split(rm, rm$sample, drop = TRUE), function(g) {
    row <- data.frame(sample = g$sample[1], n_replicates = nrow(g),
                      stringsAsFactors = FALSE)
    for (cc in metric_cols) {
      row[[paste0(cc, "_mean")]] <- safe_mean(g[[cc]])
      row[[paste0(cc, "_sd")]]   <- safe_sd(g[[cc]])
    }
    row
  })
  do.call(rbind, c(out, list(make.row.names = FALSE)))
}

METRIC_DEFINITIONS <- c(
  n_timepoints    = "Number of time points in the (filtered) mean curve",
  t_start         = "First time point plotted",
  t_end           = "Last time point plotted",
  initial_od      = "Mean value at the first time point",
  max_od          = "Maximum (peak) of the mean curve",
  time_max_od     = "Time at which the peak occurs",
  global_min_od      = "Global minimum of the mean curve (usually the inoculum, near t_start) -- NOT the post-lysis floor; see residual_od for that",
  time_global_min_od = "Time at which the global minimum occurs",
  final_od        = "Mean value at the last time point",
  auc             = "Area under the mean curve (trapezoidal rule)",
  mu_max          = "Maximum specific growth rate: largest centered rolling-window regression slope of ln(mean) vs time (window = smoothing setting, default 5 points); NA if the curve never grows",
  n_nonpositive_od = "Count of time points with OD <= 0 (e.g. a blank-subtraction artefact). Any mu_max rolling window touching one of these is excluded rather than clamped, so mu_max/doubling_time/lag_phase may be NA or based on fewer windows when this is > 0",
  doubling_time   = "ln(2) / mu_max; NA when there is no growth phase",
  lag_phase       = "First time at which the rolling ln-slope reaches 10% of mu_max",
  stat_phase_dur  = "Time spanned by all points within 95% of the peak",
  lysis_time      = "Lysis onset: first post-peak time at which the decline from peak exceeds 5% of the peak; NA if the curve never declines past that",
  lysis_rate      = "Linear-regression slope of the mean from the peak to the post-peak trough (negative = decline)",
  od_drop         = "Peak minus post-peak minimum (absolute decline)",
  residual_od     = "Post-peak minimum (residual OD after lysis) -- this is the post-lysis floor; see global_min_od for the curve's overall minimum",
  recovery_slope  = "Positive linear-regression slope from the post-peak trough to the end (regrowth / burst proxy); NA if no recovery",
  lysis_efficiency = "od_drop / max_od (fraction of the peak lost to lysis)",
  recovery_ratio  = "final_od / max_od",
  fold_change_final_vs_initial = "final_od / initial_od (NA if initial is 0)",
  pct_drop_from_max = "(max_od - final_od) / max_od x 100",
  t_half_max_decline = "First time after the peak at which the mean falls below 50% of the peak (linear interpolation); NA if never",
  steepest_decline_rate = "Most negative slope between consecutive mean points",
  steepest_decline_interval = "Time interval over which the steepest decline occurs",
  relative_growth    = "auc / auc(reference sample)",
  infection_strength = "1 - auc / auc(reference sample)",
  relative_mu_max    = "mu_max / mu_max(reference sample)",
  relative_max_od    = "max_od / max_od(reference sample)",
  lysis_onset_delta  = "lysis_time - lag_phase"
)

# ── Report helpers ────────────────────────────────────────────────────────────

fmt_sig <- function(x, digits = 5) {
  if (is.null(x)) return("")
  vapply(x, function(v) {
    if (is.na(v)) return("NA")
    if (is.numeric(v)) format(signif(v, digits), trim = TRUE, scientific = FALSE)
    else as.character(v)
  }, character(1))
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  gsub(">", "&gt;",  x, fixed = TRUE)
}

html_table <- function(df, digits = 5, class = NULL) {
  if (is.null(df) || nrow(df) == 0) return("<p><em>No data.</em></p>")
  head_row <- paste0("<th>", html_escape(colnames(df)), "</th>", collapse = "")
  body <- vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(seq_len(ncol(df)), function(j) {
      v <- df[i, j]
      txt <- if (is.numeric(v)) fmt_sig(v, digits) else html_escape(as.character(v))
      if (is.na(df[i, j])) txt <- "NA"
      txt <- gsub("\n", "<br/>", txt, fixed = TRUE)
      paste0("<td>", txt, "</td>")
    }, character(1))
    paste0("<tr>", paste0(cells, collapse = ""), "</tr>")
  }, character(1))
  cls <- if (is.null(class)) "" else paste0(" class='", class, "'")
  paste0("<div class='tblwrap'><table", cls, "><thead><tr>", head_row,
         "</tr></thead><tbody>",
         paste0(body, collapse = ""), "</tbody></table></div>")
}

# Builds the entire self-contained HTML report as one string.
# meta: named list with title, author, notes, generated, data_file, format,
#       time_col, samples_txt, filter_txt, error_txt, settings_df
build_html_report <- function(meta, img_b64, stats_df, metrics_df) {
  def_keys <- if (!is.null(metrics_df))
    intersect(names(METRIC_DEFINITIONS), colnames(metrics_df))
  else names(METRIC_DEFINITIONS)
  defs_df <- data.frame(metric = def_keys,
                        definition = unname(METRIC_DEFINITIONS[def_keys]),
                        stringsAsFactors = FALSE)
  si <- utils::sessionInfo()
  pkg_names <- sort(names(si$otherPkgs %||% list()))
  pkg_txt <- paste(vapply(pkg_names, function(p)
    paste0(p, " ", as.character(si$otherPkgs[[p]]$Version)), character(1)),
    collapse = ", ")

  notes_html <- if (nzchar(trimws(meta$notes %||% "")))
    paste0("<h2>Notes</h2><p class='notes'>",
           gsub("\n", "<br/>", html_escape(meta$notes)), "</p>") else ""

  notebook_html <- if (!is.null(meta$notebook_df))
    paste0("<h2>Experiment notebook</h2>",
           html_table(meta$notebook_df, class = "wrap prov")) else ""

  font_pt <- meta$font_pt %||% 14

  paste0(
"<!DOCTYPE html><html><head><meta charset='utf-8'>
<title>", html_escape(meta$title), "</title>
<style>
  html { background: #ffffff; }
  body { font-family: -apple-system, 'Segoe UI', Helvetica, Arial, sans-serif;
         max-width: 1000px; margin: 24px auto; padding: 0 16px;
         color: #212529; background: #ffffff; font-size: ", font_pt, "px; }
  h1 { border-bottom: 3px solid #343a40; padding-bottom: 8px; margin-bottom: 4px; }
  h2 { color: #343a40; border-bottom: 1px solid #dee2e6; padding-bottom: 4px; margin-top: 32px; }
  .meta { color: #6c757d; font-size: 0.9em; margin-bottom: 20px; }
  .tblwrap { overflow-x: auto; }
  table { border-collapse: collapse; font-size: 0.85em; margin: 12px 0; }
  th, td { border: 1px solid #dee2e6; padding: 5px 9px; text-align: left; white-space: nowrap; }
  th { background: #f1f3f5; }
  tr:nth-child(even) td { background: #fafbfc; }
  img.figure { max-width: 100%; height: auto; border: 1px solid #dee2e6; }
  .notes { background: #fff9db; border: 1px solid #ffe066; border-radius: 6px; padding: 12px; }
  .prov td:first-child { font-weight: bold; background: #f1f3f5; }
  table.wrap td { white-space: normal; }
  footer { margin-top: 40px; color: #adb5bd; font-size: 0.8em; border-top: 1px solid #dee2e6; padding-top: 8px; }
  @media print { body { max-width: none; } h2 { page-break-after: avoid; } .tblwrap { overflow-x: visible; } }
</style></head><body>
<h1>", html_escape(meta$title), "</h1>
<div class='meta'>",
  if (nzchar(meta$author %||% "")) paste0(html_escape(meta$author), " &middot; ") else "",
  "Generated ", html_escape(meta$generated), "</div>",
notes_html,
"<h2>Data provenance</h2>
<div class='tblwrap'><table class='prov'>
<tr><td>Data file</td><td>",        html_escape(meta$data_file),   "</td></tr>
<tr><td>Detected format</td><td>",  html_escape(meta$format),      "</td></tr>
<tr><td>Time column</td><td>",      html_escape(meta$time_col),    "</td></tr>
<tr><td>Samples plotted</td><td>",  html_escape(meta$samples_txt), "</td></tr>
<tr><td>Time filtering</td><td>",   html_escape(meta$filter_txt),  "</td></tr>
<tr><td>Error display</td><td>",    html_escape(meta$error_txt),   "</td></tr>
</table></div>",
notebook_html,
"<h2>Figure</h2>
<img class='figure' src='data:image/png;base64,", img_b64, "' alt='Lysis curve figure'/>
<h2>Summary statistics</h2>
<p>Per sample and time point, computed from the currently loaded (and filtered) data.
SEM = SD / &radic;n; 95% CI = t<sub>0.975, n-1</sub> &times; SEM.</p>",
html_table(stats_df),
"<h2>Curve metrics</h2>
<p>Computed from each sample's mean curve as plotted.</p>",
html_table(metrics_df),
"<h3>Metric definitions</h3>",
html_table(defs_df),
"<h2>Key visual settings</h2>",
html_table(meta$settings_df),
"<h2>Reproducibility</h2>
<p>", html_escape(paste0(si$R.version$version.string, "; platform ", si$platform)), "</p>
<p style='font-size:0.85em;color:#6c757d;'>Attached packages: ", html_escape(pkg_txt), "</p>
<footer>Lysis Curve OD Visualization App v", APP_VERSION, " &middot; Michael Baffour Awuah / Ramsey Lab</footer>
</body></html>")
}

# Builds a multi-page PDF report (no LaTeX/pandoc; base pdf device + gridExtra).
# Page 1: title + provenance. Page 2: figure. Later pages: stats + metrics tables.
build_pdf_report <- function(file, meta, plot_obj, stats_df, metrics_df,
                             width = 10, height = 8) {
  stopifnot(requireNamespace("gridExtra", quietly = TRUE))
  grDevices::pdf(file, width = width, height = height, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)

  fscale <- (meta$font_pt %||% 14) / 14   # report font size control
  tt  <- gridExtra::ttheme_default(
    core    = list(fg_params = list(cex = 0.55 * fscale)),
    colhead = list(fg_params = list(cex = 0.6 * fscale)))

  # ── Page 1: title + provenance ──
  grid::grid.newpage()
  grid::grid.text(meta$title, y = 0.90, gp = grid::gpar(fontsize = 22 * fscale, fontface = "bold"))
  sub <- paste0(if (nzchar(meta$author %||% "")) paste0(meta$author, "  •  ") else "",
                "Generated ", meta$generated)
  grid::grid.text(sub, y = 0.84, gp = grid::gpar(fontsize = 11 * fscale, col = "grey40"))
  prov <- c(paste0("Data file: ",       meta$data_file),
            paste0("Detected format: ", meta$format),
            paste0("Time column: ",     meta$time_col),
            paste0("Samples: ",         meta$samples_txt),
            paste0("Time filtering: ",  meta$filter_txt),
            paste0("Error display: ",   meta$error_txt))
  grid::grid.text(paste(prov, collapse = "\n"), y = 0.68, just = "top",
                  gp = grid::gpar(fontsize = 11 * fscale, lineheight = 1.4))
  if (nzchar(trimws(meta$notes %||% ""))) {
    grid::grid.text(paste0("Notes:\n", meta$notes), y = 0.40, just = "top",
                    gp = grid::gpar(fontsize = 10 * fscale, lineheight = 1.3, col = "grey20"))
  }

  # ── Experiment notebook page (only when fields were filled in) ──
  if (!is.null(meta$notebook_df)) {
    nb <- meta$notebook_df
    nb$value <- vapply(nb$value, function(s)
      paste(strwrap(s, 60), collapse = "\n"), character(1))
    grid::grid.newpage()
    grid::grid.text("Experiment notebook", y = 0.97,
                    gp = grid::gpar(fontsize = 13 * fscale, fontface = "bold"))
    g <- gridExtra::tableGrob(nb, rows = NULL, theme = tt)
    gw <- grid::convertWidth(sum(g$widths), "npc", valueOnly = TRUE)
    vp <- if (gw > 0.98) grid::viewport(y = 0.5, width = 0.98 / gw) else grid::viewport(y = 0.5)
    grid::pushViewport(vp); grid::grid.draw(g); grid::popViewport()
  }

  # ── Figure page ──
  print(plot_obj)

  # ── Stats table pages (chunked) ──
  fmt_df <- function(df) {
    df2 <- df
    for (j in seq_along(df2)) if (is.numeric(df2[[j]])) df2[[j]] <- fmt_sig(df2[[j]])
    df2
  }
  page_table <- function(df, title, rows_per_page = 28) {
    df <- fmt_df(df)
    n  <- nrow(df)
    starts <- seq(1, n, by = rows_per_page)
    for (s in starts) {
      chunk <- df[s:min(s + rows_per_page - 1, n), , drop = FALSE]
      grid::grid.newpage()
      grid::grid.text(paste0(title,
                             if (length(starts) > 1)
                               paste0("  (rows ", s, "–", min(s + rows_per_page - 1, n),
                                      " of ", n, ")") else ""),
                      y = 0.97, gp = grid::gpar(fontsize = 13 * fscale, fontface = "bold"))
      g <- gridExtra::tableGrob(chunk, rows = NULL, theme = tt)
      # scale down if wider than page
      gw <- grid::convertWidth(sum(g$widths), "npc", valueOnly = TRUE)
      vp <- if (gw > 0.98) grid::viewport(y = 0.5, width = 0.98 / gw) else grid::viewport(y = 0.5)
      grid::pushViewport(vp); grid::grid.draw(g); grid::popViewport()
    }
  }
  page_table(stats_df,   "Summary statistics (mean / SD / n / SEM / 95% CI)")
  if (!is.null(metrics_df)) {
    # metrics are wide; page through blocks of columns, repeating the sample col
    other  <- setdiff(seq_len(ncol(metrics_df)), 1)
    blocks <- split(other, ceiling(seq_along(other) / 7))
    for (bi in seq_along(blocks)) {
      mb <- metrics_df[, c(1, blocks[[bi]]), drop = FALSE]
      page_table(mb, paste0("Curve metrics (", bi, " of ", length(blocks), ")"))
    }
    defs_df <- data.frame(metric = intersect(names(METRIC_DEFINITIONS), colnames(metrics_df)),
                          stringsAsFactors = FALSE)
    defs_df$definition <- unname(METRIC_DEFINITIONS[defs_df$metric])
    defs_df$definition <- vapply(defs_df$definition, function(s)
      paste(strwrap(s, 70), collapse = "\n"), character(1))
    page_table(defs_df, "Metric definitions", rows_per_page = 14)
  }
  invisible(file)
}

# ── Demo dataset (used by the "Load Demo Data" button) ───────────────────────
make_demo_data <- function() {
  set.seed(42)
  times <- seq(0, 180, by = 15)
  grow  <- function(t, k = 0.028, y0 = 0.05, cap = 1.4)
    cap / (1 + ((cap - y0) / y0) * exp(-k * t))
  lyse  <- function(t, t_lysis, y_pre, rate = 0.06)
    ifelse(t < t_lysis, y_pre(t), pmax(y_pre(t_lysis) * exp(-rate * (t - t_lysis)), 0.02))
  noisy <- function(y) pmax(y * (1 + rnorm(length(y), 0, 0.04)), 0.01)
  wt   <- function(t) grow(t)
  ph1  <- function(t) lyse(t, 60,  wt)
  ph2  <- function(t) lyse(t, 105, wt)
  # build with duplicate column names (replicates), as documented
  reps <- function(f) replicate(3, noisy(f(times)))
  m <- cbind(reps(wt), reps(ph1), reps(ph2))
  # higher MOI infects more cells at once -> earlier lysis (ph1 lyses at t=60)
  colnames(m) <- rep(c("Uninfected", "Phage MOI 10", "Phage MOI 0.1"), each = 3)
  df <- data.frame(Time = times, check.names = FALSE)
  df <- cbind(df, as.data.frame(m, check.names = FALSE))
  df
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── v2.10: figure-standard helpers ───────────────────────────────────────────
# ggplot2 `linewidth` is not in pt: stroke(pt) = linewidth * .pt * 0.75
# (2.134 pt per unit, measured against svglite output). Journals specify line
# weights in pt, so the new UI controls take pt and convert here.
pt2lw <- function(pt) pt / (ggplot2::.pt * 0.75)
lw2pt <- function(lw) lw * ggplot2::.pt * 0.75

# Column widths in mm. Nature values are from its official figure guide; the
# others are the journals' author instructions as quoted by secondary sources
# (docs/FIGURE_STANDARDS.md lists them) -- confirm on the journal page before
# submission. max_h = maximum figure height the journal allows (mm).
JOURNAL_PRESETS <- list(
  nature  = list(label = "Nature family (89 / 120 / 183 mm)",      single = 89,  onehalf = 120, double = 183, max_h = 170),
  cell    = list(label = "Cell Press (85 / 114 / 174 mm)",         single = 85,  onehalf = 114, double = 174, max_h = 230),
  asm     = list(label = "ASM journals (84 / 175 mm; <= 7 x 9 in)", single = 84, onehalf = NA,  double = 175, max_h = 228),
  plos    = list(label = "PLOS (text column 132 mm; max 190 mm)",  single = 132, onehalf = NA,  double = 190, max_h = 225),
  science = list(label = "Science (55 / 120 / 183 mm)",            single = 55,  onehalf = 120, double = 183, max_h = 230),
  custom  = list(label = "Custom width (mm)",                      single = NA,  onehalf = NA,  double = NA,  max_h = NA)
)
PANEL_GUTTER_MM <- 5   # gap assumed between two side-by-side panels

journal_width_mm <- function(family, columns, custom_mm = 89) {
  family <- family %||% "nature"
  jp     <- JOURNAL_PRESETS[[family]]
  if (is.null(jp) || identical(family, "custom")) return(custom_mm)
  switch(columns %||% "single",
         single      = jp$single,
         onehalf     = jp$onehalf,
         double      = jp$double,
         half_double = (jp$double - PANEL_GUTTER_MM) / 2,
         jp$single)
}

# Everything a journal preset pushes into the UI. Each is an ordinary input
# afterwards, so Save Settings captures it and the user can still adjust it.
# Text 6-8 pt (Nature: 5-7 pt max, Cell: 6-8 pt), data lines 0.75 pt, axes and
# error bars 0.5 pt (Nature: 0.25-1 pt), Okabe-Ito palette (Nature's own
# recommendation), legend inside the panel, SVG for Inkscape, 600 dpi rasters.
journal_preset_settings <- function(width_mm, height_mm, panel_label = "", x_range = NULL) {
  s <- list(
    export_units = "mm", export_width = width_mm, export_height = height_mm,
    export_format = "svg", export_dpi = 600,
    preview_mode = "wysiwyg", preview_zoom = 2,
    font_family = "Arial",
    title_font_size = 8, axis_label_font_size = 7, axis_text_font_size = 6,
    legend_font_size = 6, label_font_size = 6,
    bold_title = FALSE, italic_axis_labels = FALSE,
    line_thickness = 0.35,        # 0.75 pt data lines
    shape_size = 1.2, point_stroke = 0.25,
    error_bar_thickness = 0.25,   # ~0.5 pt error bars
    axis_line_pt = 0.5, tick_length_mm = 1.2,
    theme_base_pt = 7, y_tick_label_style = "trim",
    legend_position = "inside-top-left", legend_box_border = FALSE, legend_key_mm = 4,
    panel_label = panel_label, panel_label_size = 8,
    color_palette = "colorblind",
    show_major_gridlines = FALSE, show_minor_gridlines = FALSE,
    show_error_caption = FALSE,   # the Figure Legend Text box carries measure + n instead
    custom_aspect_ratio = FALSE
  )
  if (!is.null(x_range) && is.numeric(x_range) && is.finite(x_range) && x_range > 0)
    s$error_bar_width <- round(x_range * 0.015, 2)   # cap width ~1.5 % of the x span
  s
}

# ── v2.10: export devices ────────────────────────────────────────────────────
# One resolver so every export path agrees on which device writes a format.
#  pdf  : journals want fonts EMBEDDED. grDevices::pdf() never embeds (it only
#         names the base-14 Helvetica) and errors on families such as "Arial"
#         that are not in its PostScript font database. macOS: quartz(type =
#         "pdf") embeds any installed font; elsewhere cairo_pdf; last resort pdf.
#  svg  : svglite with fix_text_size = FALSE. The default adds textLength= to
#         every <text>, which makes Inkscape stretch/squash glyphs the moment a
#         label is edited. Without it, text reflows naturally.
#  png/tiff/jpeg : ragg when installed (crisper text, consistent font lookup).
quartz_pdf_device <- function(filename, width, height, bg = "white", pointsize = 12, ...) {
  grDevices::quartz(type = "pdf", file = filename, width = width, height = height,
                    bg = bg, pointsize = pointsize)
}
svglite_inkscape_device <- function(filename, width, height, bg = "white", ...) {
  svglite::svglite(filename, width = width, height = height, bg = bg, fix_text_size = FALSE)
}
export_device <- function(format) {
  switch(format,
         pdf  = if (isTRUE(capabilities("aqua"))) quartz_pdf_device
                else if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else "pdf",
         svg  = svglite_inkscape_device,
         png  = if (has_ragg) ragg::agg_png  else "png",
         tiff = if (has_ragg) ragg::agg_tiff else "tiff",
         jpeg = if (has_ragg) ragg::agg_jpeg else "jpeg",
         format)
}

# ── v2.11: analysis figures, replicate statistics, MOI ───────────────────────
ANALYSIS_METRIC_LABELS <- c(
  lysis_time         = "Lysis onset",
  od_drop            = "OD drop (peak − trough)",
  lysis_efficiency   = "Lysis efficiency (fraction of peak)",
  lysis_rate         = "Lysis rate (OD per time)",
  t_half_max_decline = "Time to half of peak",
  residual_od        = "Residual OD after lysis",
  recovery_slope     = "Recovery slope (OD per time)",
  mu_max             = "μmax (ln OD per time)",
  doubling_time      = "Doubling time",
  lag_phase          = "Lag phase",
  max_od             = "Peak OD",
  time_max_od        = "Time of peak",
  auc                = "Area under curve",
  final_od           = "Final OD",
  relative_growth    = "Relative growth (AUC / reference)",
  infection_strength = "Infection strength (1 − AUC ratio)",
  relative_mu_max    = "Relative μmax",
  relative_max_od    = "Relative peak OD"
)
metric_label <- function(m) { l <- unname(ANALYSIS_METRIC_LABELS[m]); ifelse(is.na(l), m, l) }
# Axis / facet label with units, from the x-axis time unit and the y-axis label.
metric_axis_label <- function(m, tunit = "min", ylab = "OD") {
  base <- metric_label(m)
  if (m %in% c("lysis_time", "t_half_max_decline", "doubling_time", "lag_phase", "time_max_od"))
    paste0(base, " (", tunit, ")")
  else if (m == "mu_max") paste0("\u03bcmax (", tunit, "\u207b\u00b9)")
  else if (m %in% c("lysis_rate", "recovery_slope"))
    paste0(sub(" \\(OD per time\\)", "", base), " (", ylab, " per ", tunit, ")")
  else if (m %in% c("od_drop", "residual_od", "max_od", "final_od")) paste0(base, " (", ylab, ")")
  else if (m == "auc") paste0(base, " (", ylab, " \u00d7 ", tunit, ")")
  else base
}

sig_stars <- function(p)
  ifelse(is.na(p), "", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "ns"))))

time_unit_from_label <- function(xlab) {
  x <- tolower(xlab %||% "")
  if (grepl("min", x)) "min"
  else if (grepl("hour|\\(h\\)|\\bhr", x)) "h"
  else if (grepl("sec", x)) "s"
  else "time unit"
}

# MOI from sample names: "Phage MOI 10", "MOI=0.1", "moi_1e-2", "MOI: 5" ...
parse_moi <- function(x) {
  m <- regmatches(x, regexec("(?i)MOI[[:space:]_:=]*([0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?)", x, perl = TRUE))
  vapply(m, function(v) if (length(v) >= 2) suppressWarnings(as.numeric(v[2])) else NA_real_, numeric(1))
}
# override text: one "sample = MOI" per line or separated by ";"
moi_map <- function(samples, override = "") {
  moi <- setNames(parse_moi(samples), samples)
  ov  <- trimws(override %||% "")
  if (nzchar(ov)) {
    parts <- trimws(unlist(strsplit(ov, "[;\n]+")))
    for (p in parts[nzchar(parts)]) {
      kv <- trimws(unlist(strsplit(p, "=", fixed = TRUE)))
      if (length(kv) == 2 && kv[1] %in% samples) moi[kv[1]] <- suppressWarnings(as.numeric(kv[2]))
    }
  }
  moi
}

# Specific growth rate mu(t) = d ln(OD)/dt per replicate, by the SAME centered
# rolling regression (window = the mu_max smoothing window) and the same
# non-positive-OD exclusion as the mu_max metric, then mean +/- SD across
# replicates at each time point. Lysis shows as a negative rate.
compute_growth_rate_curves <- function(rep_pd, window = 5) {
  if (is.null(rep_pd) || nrow(rep_pd) == 0) return(NULL)
  rep_pd <- rep_pd[is.finite(rep_pd$value), , drop = FALSE]
  if (nrow(rep_pd) == 0) return(NULL)
  rep_pd <- rep_pd[order(rep_pd$variable, rep_pd$replicate, rep_pd$time), , drop = FALSE]
  key <- interaction(rep_pd$variable, rep_pd$replicate, drop = TRUE)
  pieces <- lapply(split(rep_pd, key, drop = TRUE), function(g) {
    n <- nrow(g); win <- min(window, n)
    mu <- rep(NA_real_, n)
    if (n >= win && win >= 3) {
      pos <- g$value > 0
      lo  <- rep(NA_real_, n); lo[pos] <- log(g$value[pos])
      mu  <- tryCatch(rolling_slope(g$time, lo, win), error = function(e) rep(NA_real_, n))
      if (any(!pos)) {
        off <- floor((win - 1) / 2)
        for (s in seq_len(n - win + 1)) if (any(!pos[s:(s + win - 1)])) mu[s + off] <- NA_real_
      }
    }
    data.frame(time = g$time, variable = g$variable[1], replicate = g$replicate[1], mu = mu,
               stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, c(pieces, list(make.row.names = FALSE)))
  d <- d[is.finite(d$mu), , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  out <- lapply(split(d, list(d$variable, d$time), drop = TRUE), function(g)
    data.frame(time = g$time[1], variable = g$variable[1], n = nrow(g), mu_mean = mean(g$mu),
               mu_sd = if (nrow(g) >= 2) sd(g$mu) else NA_real_, stringsAsFactors = FALSE))
  out <- do.call(rbind, c(out, list(make.row.names = FALSE)))
  out[order(out$variable, out$time), , drop = FALSE]
}

# OD relative to the uninfected reference at each time point (mean curves),
# or to each sample's own first value ("t0"). The reference-normalised curve
# is the infection-strength metric (1 - AUC ratio) drawn out over time.
compute_normalized_curves <- function(pd, ref_sample = NULL, mode = c("reference", "t0")) {
  mode <- match.arg(mode)
  if (is.null(pd) || nrow(pd) == 0) return(NULL)
  pd <- pd[order(pd$variable, pd$time), , drop = FALSE]
  if (mode == "reference") {
    if (is.null(ref_sample) || !nzchar(ref_sample) || !ref_sample %in% pd$variable) return(NULL)
    ref <- pd[pd$variable == ref_sample, c("time", "mean_value")]
    names(ref)[2] <- "ref_value"
    m <- merge(pd, ref, by = "time")
    m$ratio <- ifelse(is.finite(m$ref_value) & m$ref_value > 0, m$mean_value / m$ref_value, NA_real_)
  } else {
    m <- pd
    m$ratio <- ave(m$mean_value, m$variable,
                   FUN = function(v) if (is.finite(v[1]) && v[1] > 0) v / v[1] else rep(NA_real_, length(v)))
  }
  m <- m[is.finite(m$ratio), c("time", "variable", "ratio"), drop = FALSE]
  m[order(m$variable, m$time), , drop = FALSE]
}

# ── Replicate statistics ─────────────────────────────────────────────────────
# Compares one per-replicate metric between samples. With a reference sample
# each sample is tested against it; otherwise all pairs. Welch's t-test
# (unequal variances) or Mann-Whitney; Holm-adjusted p across the comparisons;
# Hedges' g (bias-corrected standardized mean difference); the 95% CI of the
# difference from the t-test. Omnibus for >= 3 groups: one-way ANOVA + Tukey
# HSD, or Kruskal-Wallis. Groups with fewer than 2 finite values are excluded.
# Any n < 3 is flagged: three biological replicates give little power, so a
# non-significant result is not evidence of no difference.
compare_replicate_metric <- function(rm, metric, ref_sample = NULL,
                                     method = c("welch", "nonparametric")) {
  method <- match.arg(method)
  empty  <- list(pairwise = NULL, omnibus = NULL, posthoc = NULL, warnings = character(0), metric = metric)
  if (is.null(rm) || !metric %in% names(rm)) return(empty)
  d <- rm[is.finite(rm[[metric]]), c("sample", metric), drop = FALSE]
  names(d)[2] <- "value"
  ns      <- table(d$sample)
  dropped <- names(ns)[ns < 2]
  keep    <- names(ns)[ns >= 2]
  warns   <- character(0)
  if (length(dropped))
    warns <- c(warns, paste0("Excluded (fewer than 2 finite values): ", paste(dropped, collapse = ", "), "."))
  if (length(keep) && any(ns[keep] < 3))
    warns <- c(warns, "Some groups have n < 3: low power -- a non-significant result is not evidence of no difference.")
  if (length(keep) < 2) {
    empty$warnings <- c(warns, "Need at least two samples with 2+ replicate values for this metric.")
    return(empty)
  }
  d <- d[d$sample %in% keep, , drop = FALSE]
  has_ref <- !is.null(ref_sample) && nzchar(ref_sample)
  if (has_ref && !ref_sample %in% keep)
    warns <- c(warns, paste0("Reference '", ref_sample, "' has no usable values for this metric ",
                             "(e.g. an uninfected control never lyses), so all pairs are compared instead."))
  pairs <- if (has_ref && ref_sample %in% keep)
    lapply(setdiff(keep, ref_sample), function(s) c(s, ref_sample))
  else combn(keep, 2, simplify = FALSE)
  hedges_g <- function(a, b) {
    na <- length(a); nb <- length(b)
    sp <- sqrt(((na - 1) * var(a) + (nb - 1) * var(b)) / (na + nb - 2))
    if (!is.finite(sp) || sp == 0) return(NA_real_)
    (mean(a) - mean(b)) / sp * (1 - 3 / (4 * (na + nb) - 9))
  }
  rows <- lapply(pairs, function(pr) {
    a  <- d$value[d$sample == pr[1]]; b <- d$value[d$sample == pr[2]]
    tt <- tryCatch(if (method == "welch") t.test(a, b) else wilcox.test(a, b, exact = FALSE),
                   error = function(e) NULL)
    ci <- if (!is.null(tt) && !is.null(tt$conf.int)) tt$conf.int else c(NA_real_, NA_real_)
    data.frame(sample = pr[1], versus = pr[2], n = length(a), n_versus = length(b),
               mean = mean(a), mean_versus = mean(b), difference = mean(a) - mean(b),
               ci_low = ci[1], ci_high = ci[2], hedges_g = hedges_g(a, b),
               test = if (method == "welch") "Welch t" else "Mann-Whitney",
               p = if (is.null(tt)) NA_real_ else unname(tt$p.value),
               stringsAsFactors = FALSE)
  })
  pw <- do.call(rbind, c(rows, list(make.row.names = FALSE)))
  if (any(is.na(pw$p)))   # t.test() refuses constant data (e.g. identical lysis onsets at a coarse sampling interval)
    warns <- c(warns, paste0("No test possible (values constant within both groups): ",
                             paste(paste(pw$sample[is.na(pw$p)], "vs", pw$versus[is.na(pw$p)]), collapse = "; "), "."))
  pw$p_adj_holm <- p.adjust(pw$p, method = "holm")
  pw$stars      <- sig_stars(pw$p_adj_holm)
  om <- NULL; ph <- NULL
  if (length(keep) >= 3) {
    d$sample <- factor(d$sample)
    if (method == "welch") {
      fit <- tryCatch(aov(value ~ sample, data = d), error = function(e) NULL)
      if (!is.null(fit)) {
        s  <- summary(fit)[[1]]
        om <- data.frame(test = "One-way ANOVA", statistic = s[["F value"]][1],
                         df1 = s[["Df"]][1], df2 = s[["Df"]][2], p = s[["Pr(>F)"]][1],
                         stringsAsFactors = FALSE)
        tk <- tryCatch(TukeyHSD(fit)$sample, error = function(e) NULL)
        if (!is.null(tk)) {
          ph <- data.frame(comparison = rownames(tk), difference = tk[, "diff"],
                           ci_low = tk[, "lwr"], ci_high = tk[, "upr"], p_adj_tukey = tk[, "p adj"],
                           row.names = NULL, stringsAsFactors = FALSE)
          ph$stars <- sig_stars(ph$p_adj_tukey)
        }
      }
    } else {
      kw <- tryCatch(kruskal.test(value ~ sample, data = d), error = function(e) NULL)
      if (!is.null(kw))
        om <- data.frame(test = "Kruskal-Wallis", statistic = unname(kw$statistic),
                         df1 = unname(kw$parameter), df2 = NA_real_, p = kw$p.value,
                         stringsAsFactors = FALSE)
    }
  }
  list(pairwise = pw, omnibus = om, posthoc = ph, warnings = warns, metric = metric)
}

# ── App defaults (v2.10: single source of truth) ─────────────────────────────
# Used by Clear/Reset Settings and by the test suites to seed a server session
# with the same values the UI widgets start with.
APP_DEFAULTS <- list(
  x_scale_type          = "linear",  y_scale_type           = "log",
  use_advanced_ticks    = TRUE,       y_log_min_exponent     = -2,
  y_log_max_exponent    = 0,          custom_x_limits        = FALSE,
  custom_y_limits       = FALSE,      x_expand_left          = 0,
  x_expand_right        = 0.05,       y_expand_bottom        = 0,
  y_expand_top          = 0.05,       x_axis_label           = "Time (minutes)",
  y_axis_label          = "A550",     plot_title             = "",
  plot_subtitle         = "",         show_major_gridlines   = FALSE,
  show_minor_gridlines  = FALSE,      font_family            = "sans",
  title_font_size       = 20,         axis_label_font_size   = 20,
  axis_text_font_size   = 16,         bold_title             = TRUE,
  italic_axis_labels    = FALSE,      axis_text_angle        = "0",
  enable_highlighting   = FALSE,      enable_time_markers    = FALSE,
  color_palette         = "custom",   line_thickness         = 1,
  show_points           = TRUE,       shape_size             = 3,
  point_stroke          = 0.5,        show_end_labels        = FALSE,
  label_font_size       = 12,         label_bold             = TRUE,
  label_offset          = 3.5,        error_type             = "sem",
  error_display_mode    = "bars",     error_multiplier       = 1,
  asymmetric_error      = FALSE,      error_bar_style        = "T",
  error_bar_width       = 1,          error_bar_thickness    = 0.8,
  error_bar_position    = "middle",   shadow_alpha           = 0.2,
  show_error_caption    = TRUE,       # KC-6: default figure states the error measure + n
  custom_aspect_ratio   = FALSE,      aspect_ratio           = 1,
  export_format         = "pdf",      export_width           = 10,
  export_height         = 8,          export_dpi             = 300,
  plot_width            = 820,        plot_height            = 600,
  gif_fps               = 1,          gif_width              = 800,
  gif_height            = 600,        x_extra_ticks          = "",
  report_title          = "Lysis Curve Analysis Report",
  report_author         = "",         report_notes           = "",
  metrics_smooth_window = 5,          legend_font_size       = 14,
  metrics_peak_smooth_window = 1,     # KC-3: off by default
  blank_od               = 0,         # KC-7: off by default
  report_font_size      = 14,
  # v2.10 journal-figure inputs
  export_units = "in", preview_mode = "canvas", preview_zoom = 2,
  axis_line_pt = 1.07, tick_length_mm = 1.5, legend_position = "right",
  legend_box_border = TRUE, legend_key_mm = 0, panel_label = "", panel_label_size = 8,
  theme_base_pt = 0, y_tick_label_style = "legacy",
  # v2.11 analysis / dashboard inputs
  analysis_type = "metric_summary", analysis_metrics = c("lysis_time", "od_drop"),
  analysis_dose_metric = "lysis_time", analysis_norm_mode = "reference",
  analysis_panel_label = "b", stats_method = "welch", stats_mark = TRUE, moi_override = "",
  journal_family = "nature", journal_columns = "single", journal_custom_width_mm = 89,
  journal_height_mm = 65, journal_panel_label = "",
  nb_exp_id = "", nb_experimenter = "", nb_project = "", nb_institution = "",
  nb_exp_type = "Phage Infection", nb_host_strain = "", nb_phage_plasmid = "",
  nb_moi = "", nb_replicate = "", nb_media = "", nb_temperature = "",
  nb_time_inf = "", nb_inducer = "", nb_observations = "", nb_issues = "",
  nb_next_steps = "", nb_tags = "",
  nb_custom_key1 = "", nb_custom_val1 = "", nb_custom_key2 = "", nb_custom_val2 = "",
  nb_custom_key3 = "", nb_custom_val3 = ""
)
# ── Optional UI widgets — must be pre-computed before fluidPage() ─────────────

pptx_ui <- if (has_officer && has_rvg) {
  downloadButton("downloadPPTX", "Download PowerPoint (.pptx)",
                 style = "background:#C0392B;color:white;border:none;width:100%;")
} else {
  p(style = "color:#999;font-size:.85em;",
    "Install 'officer' + 'rvg' to enable PPTX export.")
}

gif_ui <- if (has_gifski) {
  downloadButton("downloadGIF", "Download Animated GIF",
                 style = "background:#27AE60;color:white;border:none;width:100%;")
} else {
  p(style = "color:#999;font-size:.85em;",
    "Install 'gifski' to enable GIF export.")
}

html_report_ui <- if (has_base64enc) {
  downloadButton("downloadHTMLReport", "HTML Report (single file)",
                 style = "background:#2C3E50;color:white;border:none;width:100%;")
} else {
  p(style = "color:#999;font-size:.85em;",
    "Install 'base64enc' to enable the HTML report.")
}

pdf_report_ui <- if (has_gridExtra) {
  downloadButton("downloadPDFReport", "PDF Report (multi-page)",
                 style = "background:#8E44AD;color:white;border:none;width:100%;margin-top:6px;")
} else {
  p(style = "color:#999;font-size:.85em;",
    "Install 'gridExtra' to enable the PDF report.")
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("Optical Density (OD) Time-Series Visualization"),
  
  tags$head(
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('updateColorPreview', function(msg) {
         var el = document.getElementById(msg.id);
         if (el) el.style.backgroundColor = msg.color;
       });"
    )),
    tags$style(HTML("
      .panel-section {
        background:#f8f9fa; padding:12px; border-radius:5px;
        margin:10px 0; border:1px solid #e9ecef;
      }
      .panel-title { margin-top:0; margin-bottom:10px; font-weight:bold; color:#495057; }
      details > summary {
        cursor:pointer; font-weight:bold; padding:6px 10px;
        background:#e9ecef; border-radius:3px; list-style:none; user-select:none;
      }
      details > summary::-webkit-details-marker { display:none; }
      details[open] > summary::before { content:'\\25BC  '; font-size:.75em; }
      details > summary::before       { content:'\\25B6  '; font-size:.75em; }
      details { margin-bottom:6px; }
      .color-preview {
        display:inline-block; width:28px; height:28px;
        border:1px solid #ccc; border-radius:4px; flex-shrink:0;
      }
      .settings-status { padding:8px 12px; border-radius:4px; font-size:.85em; margin-top:6px; }
      #figure_legend_text { white-space: pre-wrap; font-size: .82em; line-height: 1.35; }
      .analysis-controls { background:#f8f9fa; border:1px solid #e9ecef; border-radius:6px; padding:10px 12px 0; margin-bottom:10px; }
      .dash-cards { display:flex; flex-wrap:wrap; gap:10px; }
      .dash-card  { flex:1 1 150px; background:#f8f9fa; border:1px solid #e9ecef; border-radius:6px; padding:10px 12px; }
      .dash-card-title { font-size:.75em; color:#6c757d; text-transform:uppercase; letter-spacing:.04em; }
      .dash-card-value { font-size:1.35em; font-weight:bold; color:#2C3E50; margin:2px 0; }
      .dash-card-sub   { font-size:.78em; color:#6c757d; }
      .dash-empty { color:#6c757d; padding:30px; text-align:center; }
      .stats-note { background:#fff3cd; border:1px solid #ffe69c; border-radius:4px; padding:6px 8px; margin:4px 0 8px; font-size:.8em; color:#664d03; }
      .settings-status.ok   { background:#d4edda; color:#155724; border:1px solid #c3e6cb; }
      .settings-status.warn { background:#fff3cd; color:#856404; border:1px solid #ffeeba; }
      .match-list { margin:4px 0 0 0; padding-left:16px; }
      .excl-preview {
        background:#fff3cd; color:#856404; padding:6px 10px;
        border-radius:4px; font-size:.82em; margin-top:4px;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 
                 fileInput("file", "Choose CSV File", accept = c("text/csv", ".csv")),
                 selectInput("csv_locale", "CSV format:",
                             choices = c("Auto-detect"                              = "auto",
                                        "US/UK (comma-separated, . decimal)"        = "us",
                                        "European (semicolon-separated, , decimal)" = "eu"),
                             selected = "auto"),
                 numericInput("blank_od", "Subtract blank/media OD (optional):", 0, step = 0.001),
                 p(style = "font-size:.78em;color:#777;margin-top:-8px;",
                   "0 = off (default). This app assumes pre-blanked input; if your CSV still has ",
                   "a media/blank OD baked in, enter it here — it's subtracted from every ",
                   "reading before plotting, statistics, and curve metrics, and noted in the ",
                   "report provenance. Small negative values after subtraction are expected and ",
                   "handled correctly (see the μmax non-positive-OD note above)."),
                 actionButton("load_demo", "Load Demo Data", icon = icon("flask"),
                              style = "width:100%;margin:-8px 0 10px 0;font-size:.85em;"),
                 uiOutput("data_summary_ui"),

                 # ── Settings ─────────────────────────────────────────────────────────────
                 div(class = "panel-section",
                     h4("Save / Load Settings", class = "panel-title"),
                     p(style = "font-size:.82em;color:#555;margin-bottom:8px;",
                       "Saves all visual settings: axis scales, labels, error bars, fonts, line/point options, ",
                       "AND per-sample aesthetics (color, shape, linetype) keyed by sample name. ",
                       "Loading restores all those settings — but never touches your data, ",
                       "sample selection, or time filter."),
                     fluidRow(
                       column(6, downloadButton("saveSettings",  "Save Settings",  style = "width:100%")),
                       column(6, actionButton("clearSettings", "Clear & Reset",
                                              icon  = icon("times"),
                                              style = "width:100%;background:#dc3545;color:white;border:none;"))
                     ),
                     br(),
                     fileInput("loadSettings", "Load Settings File",
                               accept = c("application/json", ".json")),
                     uiOutput("settings_status_ui")
                 ),
                 
                 # ── Axis Settings ─────────────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Axis Settings"),
                   div(class = "panel-section",
                       h4("Axis Scales", class = "panel-title"),
                       selectInput("x_scale_type", "X-axis Scale:",
                                   choices  = c("Linear" = "linear", "Logarithmic" = "log",
                                                "Square Root" = "sqrt", "Reverse" = "reverse"),
                                   selected = "linear"),
                       selectInput("y_scale_type", "Y-axis Scale:",
                                   choices  = c("Linear" = "linear", "Logarithmic" = "log",
                                                "Square Root" = "sqrt", "Reverse" = "reverse"),
                                   selected = "log"),
                       checkboxInput("use_advanced_ticks", "Advanced Tick Customization", value = TRUE),
                       conditionalPanel(
                         condition = "input.use_advanced_ticks == true && input.y_scale_type == 'log'",
                         h5("Y-axis Log Tick Range"),
                         fluidRow(
                           column(6, numericInput("y_log_min_exponent", "Min Exponent:", value = -2)),
                           column(6, numericInput("y_log_max_exponent", "Max Exponent:", value =  0))
                         )
                       ),
                       conditionalPanel(
                         condition = "input.use_advanced_ticks == true && input.x_scale_type == 'linear'",
                         h5("X-axis Tick Control", style = "margin:10px 0 4px;"),
                         fluidRow(
                           column(6,
                                  numericInput("x_tick_interval", "Major Interval:",
                                               value = NA, min = 0.001, step = 1)
                           ),
                           column(6,
                                  p(style = "font-size:.8em;color:#777;margin-top:28px;",
                                    "Leave blank = auto")
                           )
                         ),
                         textInput("x_extra_ticks", "Extra tick positions:",
                                   value = "", placeholder = "e.g. 270, 450"),
                         p(style = "font-size:.8em;color:#777;margin-top:-6px;",
                           "Values always shown regardless of interval. Comma-separated.")
                       ),
                       checkboxInput("custom_x_limits", "Custom X-axis limits", value = FALSE),
                       conditionalPanel(condition = "input.custom_x_limits == true",
                                        fluidRow(
                                          column(6, numericInput("x_min", "X-min:", value = 0)),
                                          column(6, numericInput("x_max", "X-max:", value = 100))
                                        )
                       ),
                       checkboxInput("custom_y_limits", "Custom Y-axis limits", value = FALSE),
                       conditionalPanel(condition = "input.custom_y_limits == true",
                                        fluidRow(
                                          column(6, numericInput("y_min", "Y-min:", value = 0.01)),
                                          column(6, numericInput("y_max", "Y-max:", value = 2))
                                        )
                       ),
                       sliderInput("x_expand_left",   "X Left Expand:",   0, 0.1, 0,    0.01),
                       sliderInput("x_expand_right",  "X Right Expand:",  0, 0.1, 0.05, 0.01),
                       sliderInput("y_expand_bottom", "Y Bottom Expand:", 0, 0.1, 0,    0.01),
                       sliderInput("y_expand_top",    "Y Top Expand:",    0, 0.1, 0.05, 0.01)
                   ),
                   div(class = "panel-section",
                       h4("Labels & Formatting", class = "panel-title"),
                       textInput("x_axis_label",  "X-axis Label:",  "Time (minutes)"),
                       textInput("y_axis_label",  "Y-axis Label:",  "A550"),
                       textInput("plot_title",    "Plot Title:",    ""),
                       textInput("plot_subtitle", "Plot Subtitle:", ""),
                       h5("Gridlines", style = "margin-top:12px;"),
                       checkboxInput("show_major_gridlines", "Show Major Gridlines", FALSE),
                       checkboxInput("show_minor_gridlines", "Show Minor Gridlines", FALSE),
                       conditionalPanel(condition = "input.show_major_gridlines == true",
                                        selectInput("major_gridline_color", "Major Gridline Color:",
                                                    choices  = c("Light Gray" = "#E6E6E6", "Medium Gray" = "#CCCCCC",
                                                                 "Dark Gray"  = "#999999", "Light Blue"  = "#DEEBF7",
                                                                 "Custom"     = "custom"),
                                                    selected = "#E6E6E6"),
                                        conditionalPanel(condition = "input.major_gridline_color == 'custom'",
                                                         textInput("major_gridline_color_custom", "HEX:", "#E6E6E6")),
                                        sliderInput("major_gridline_size", "Width:", 0.1, 1, 0.5, 0.1)
                       ),
                       conditionalPanel(condition = "input.show_minor_gridlines == true",
                                        selectInput("minor_gridline_color", "Minor Gridline Color:",
                                                    choices  = c("Light Gray" = "#F2F2F2", "Medium Gray" = "#E6E6E6",
                                                                 "Dark Gray"  = "#CCCCCC", "Light Blue"  = "#EFF6FB",
                                                                 "Custom"     = "custom"),
                                                    selected = "#F2F2F2"),
                                        conditionalPanel(condition = "input.minor_gridline_color == 'custom'",
                                                         textInput("minor_gridline_color_custom", "HEX:", "#F2F2F2")),
                                        sliderInput("minor_gridline_size", "Width:", 0.1, 1, 0.3, 0.1)
                       ),
                       h5("Font Settings", style = "margin-top:12px;"),
                       selectInput("font_family", "Font Family:",
                                   choices  = c("Sans Serif (Arial/Helvetica)" = "sans",
                                                "Arial"                        = "Arial",
                                                "Helvetica"                    = "Helvetica",
                                                "Serif (Times New Roman)"      = "serif",
                                                "Monospace (Courier New)"      = "mono",
                                                "Helvetica Neue"               = "Helvetica Neue",
                                                "Garamond"                     = "Garamond",
                                                "Palatino"                     = "Palatino"),
                                   selected = "sans"),
                       numericInput("title_font_size",      "Title Font Size:",  20, 8, 48),
                       numericInput("axis_label_font_size", "Axis Label Size:",  20, 8, 36),
                       numericInput("axis_text_font_size",  "Axis Text Size:",   16, 6, 32),
                       numericInput("legend_font_size",     "Legend Font Size:", 14, 6, 32),
                       checkboxInput("bold_title",         "Bold Title",         TRUE),
                       checkboxInput("italic_axis_labels", "Italic Axis Labels", FALSE),
                       selectInput("axis_text_angle", "X-axis Text Angle:",
                                   choices  = c("Horizontal (0\u00b0)" = "0",
                                                "Angled (45\u00b0)"    = "45",
                                                "Vertical (90\u00b0)"  = "90"),
                                   selected = "0")
                   ),
                   div(class = "panel-section",
                       h4("Figure Layout", class = "panel-title"),
                       p(style = "font-size:.8em;color:#777;margin-top:-4px;",
                         "Line widths in pt and lengths in mm, as journals specify them. ",
                         "Defaults reproduce the pre-v2.10 look."),
                       selectInput("legend_position", "Legend Position:",
                                   choices = c("Right (outside)"      = "right",
                                               "Top (outside)"        = "top",
                                               "Bottom (outside)"     = "bottom",
                                               "Left (outside)"       = "left",
                                               "Inside: top-left"     = "inside-top-left",
                                               "Inside: top-right"    = "inside-top-right",
                                               "Inside: bottom-left"  = "inside-bottom-left",
                                               "Inside: bottom-right" = "inside-bottom-right",
                                               "None"                 = "none"),
                                   selected = "right"),
                       checkboxInput("legend_box_border", "Legend box border", TRUE),
                       numericInput("legend_key_mm", "Legend key size (mm; 0 = auto):", 0, 0, 20, 0.5),
                       fluidRow(
                         column(6, numericInput("axis_line_pt",   "Axis / tick line (pt):", 1.07, 0.1, 4, 0.05)),
                         column(6, numericInput("tick_length_mm", "Tick length (mm):",     1.5,  0,  10, 0.1))
                       ),
                       fluidRow(
                         column(6, numericInput("theme_base_pt", "Spacing base size (pt; 0 = theme default):", 0, 0, 30, 0.5)),
                         column(6, selectInput("y_tick_label_style", "Y tick labels:",
                                               choices = c("As is (0.10)"     = "legacy",
                                                           "Trim zeros (0.1)" = "trim",
                                                           "Powers of ten"    = "log10"),
                                               selected = "legacy"))
                       ),
                       p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                         "Spacing base size scales margins, tick/label gaps and legend spacing; ",
                         "set it near your text size (7 pt) for journal figures."),
                       fluidRow(
                         column(6, textInput("panel_label", "Panel label:", "", placeholder = "a")),
                         column(6, numericInput("panel_label_size", "Panel label (pt):", 8, 5, 20, 0.5))
                       )
                   )
                 ),
                 
                 # ── Time Point Filtering ──────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Time Point Filtering"),
                   div(class = "panel-section",
                       h4("Restrict Displayed Time Range", class = "panel-title"),
                       p(style = "font-size:.85em;color:#555;",
                         "Filters data before plotting. All statistics, error bars, and exports reflect the filtered data."),
                       checkboxInput("enable_time_filter", "Enable Time Filtering", value = FALSE),
                       conditionalPanel(condition = "input.enable_time_filter == true",
                                        h5("Time Range", style = "margin:8px 0 4px;"),
                                        uiOutput("time_range_slider_ui"),
                                        hr(style = "margin:10px 0;"),
                                        h5("Exclude Specific Time Points", style = "margin:8px 0 4px;"),
                                        p(style = "font-size:.82em;color:#666;", "Comma-separated values to remove (e.g. 0, 5, 180)."),
                                        textInput("exclude_timepoints", label = NULL, value = "", placeholder = "e.g. 0, 5, 180"),
                                        uiOutput("excluded_timepoints_preview")
                       )
                   )
                 ),
                 
                 # ── Region Highlighting ───────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Region Highlighting"),
                   div(class = "panel-section",
                       checkboxInput("enable_highlighting", "Enable Region Highlighting", FALSE),
                       conditionalPanel(condition = "input.enable_highlighting == true",
                                        numericInput("region_count", "Number of Regions:", 1, 1, 5),
                                        uiOutput("region_settings")
                       )
                   )
                 ),
                 
                 # ── Time Point Markers ────────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Time Point Markers"),
                   div(class = "panel-section",
                       checkboxInput("enable_time_markers", "Enable Time Markers", FALSE),
                       conditionalPanel(condition = "input.enable_time_markers == true",
                                        numericInput("marker_count", "Number of Markers:", 1, 1, 10),
                                        uiOutput("time_marker_settings")
                       )
                   )
                 ),
                 
                 # ── Color Palettes ────────────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Color Palettes"),
                   div(class = "panel-section",
                       selectInput("color_palette", "Color Palette:",
                                   choices  = c("Custom"              = "custom",
                                                "Viridis"             = "viridis",
                                                "Plasma"              = "plasma",
                                                "Colorblind-friendly" = "colorblind",
                                                "Publication"         = "publication",
                                                "Rainbow"             = "rainbow",
                                                "Grayscale"           = "gray"),
                                   selected = "custom"),
                       htmlOutput("palette_preview")
                   )
                 ),
                 
                 # ── Line & Point Settings ─────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Line & Point Settings"),
                   div(class = "panel-section",
                       h4("Line Options", class = "panel-title"),
                       sliderInput("line_thickness", "Line Thickness (1 = 2.1 pt):", 0.1, 3, 1, 0.05)
                   ),
                   div(class = "panel-section",
                       h4("Point Options", class = "panel-title"),
                       checkboxInput("show_points", "Show Points", TRUE),
                       conditionalPanel(condition = "input.show_points == true",
                                        sliderInput("shape_size",   "Point Size:",   0.5, 8, 3,   0.1),
                                        sliderInput("point_stroke", "Stroke Width:", 0,   2, 0.5, 0.05)
                       )
                   )
                 ),
                 
                 # ── Label Options ─────────────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Label Options"),
                   div(class = "panel-section",
                       checkboxInput("show_end_labels", "Show End-of-Line Labels", FALSE),
                       conditionalPanel(condition = "input.show_end_labels == true",
                                        numericInput("label_font_size", "Label Font Size (pt):", 12, 3, 36),
                                        checkboxInput("label_bold",  "Bold Labels",              TRUE),
                                        numericInput("label_offset", "Label Offset (% x-axis):", 3.5, 0, 20)
                       )
                   )
                 ),
                 
                 # ── Error Bars / Shadow ───────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Error Bars / Shadow"),
                   div(class = "panel-section",
                       selectInput("error_type", "Error Type:",
                                   choices  = c("None" = "none", "SD" = "sd",
                                                "SEM"  = "sem",  "95% CI" = "ci95"),
                                   selected = "sem"),
                       conditionalPanel(condition = "input.error_type != 'none'",
                                        selectInput("error_display_mode", "Display Mode:",
                                                    choices  = c("Error Bars" = "bars", "Shadow/Ribbon" = "shadow"),
                                                    selected = "bars"),
                                        numericInput("error_multiplier", "Error Multiplier:", 1, 0.1, 5, 0.1),
                                        checkboxInput("asymmetric_error", "Floor errors at zero", FALSE),
                                        conditionalPanel(condition = "input.error_display_mode == 'bars'",
                                                         selectInput("error_bar_style", "Bar Style:",
                                                                     choices  = c("T-shaped"     = "T",
                                                                                  "Solid lines"  = "solid",
                                                                                  "Dashed lines" = "dashed"),
                                                                     selected = "T"),
                                                         sliderInput("error_bar_width",     "Bar Width (x data units):", 0.01, 50, 1, 0.01),
                                                         sliderInput("error_bar_thickness", "Bar Thickness (1 = 2.1 pt):", 0.1,  2, 0.8, 0.05),
                                                         selectInput("error_bar_position", "Position:",
                                                                     choices  = c("Middle" = "middle", "Dodge" = "dodge"),
                                                                     selected = "middle")
                                        ),
                                        conditionalPanel(condition = "input.error_display_mode == 'shadow'",
                                                         sliderInput("shadow_alpha", "Ribbon Alpha:", 0.05, 0.6, 0.2, 0.05)
                                        ),
                                        checkboxInput("show_error_caption",
                                                      "Annotate error type + n in caption",
                                                      value = TRUE),
                                        p(style = "font-size:.8em;color:#777;margin-top:-6px;",
                                          "Adds a figure caption stating the error measure (SD / SEM / 95% CI) ",
                                          "and the replicate count — on by default (KC-6) so a default SEM-bar ",
                                          "figure never leaves the app unlabelled; recommended for publication figures.")
                       )
                   )
                 ),
                 
                 # ── Journal Figure Preset (v2.10) ─────────────────────────────────────────
                 tags$details(
                   tags$summary("Journal Figure Preset"),
                   div(class = "panel-section",
                       h4("Target Journal & Width", class = "panel-title"),
                       p(style = "font-size:.8em;color:#777;margin-top:-4px;",
                         "Sets the export size to a real column width, text to Arial 6-8 pt, lines to ",
                         "0.5-0.75 pt, the Okabe-Ito colour-blind-safe palette, and the legend inside the ",
                         "panel. Everything it sets stays an ordinary control you can adjust. ",
                         "Export as SVG, edit in Inkscape, never rescale. See docs/FIGURE_STANDARDS.md."),
                       selectInput("journal_family", "Journal:",
                                   choices  = setNames(names(JOURNAL_PRESETS),
                                                       vapply(JOURNAL_PRESETS, `[[`, "", "label")),
                                   selected = "nature"),
                       selectInput("journal_columns", "Width:",
                                   choices  = c("Single column"                               = "single",
                                                "1.5 columns"                                 = "onehalf",
                                                "Double column (full width)"                  = "double",
                                                "Half of a double column (one panel of two)" = "half_double"),
                                   selected = "single"),
                       conditionalPanel(condition = "input.journal_family == 'custom'",
                                        numericInput("journal_custom_width_mm", "Width (mm):", 89, 20, 300, 1)),
                       numericInput("journal_height_mm", "Height (mm):", 65, 20, 250, 1),
                       textInput("journal_panel_label", "Panel label (optional):", "", placeholder = "a"),
                       uiOutput("journal_preset_summary"),
                       actionButton("apply_journal_preset", "Apply journal preset",
                                    style = "background:#2C3E50;color:white;border:none;width:100%;"),
                       p(style = "font-size:.78em;color:#777;margin-top:6px;",
                         "Use Save Settings afterwards to keep the result as a reusable style file.")
                   ),
                   div(class = "panel-section",
                       h4("Figure Legend Text", class = "panel-title"),
                       p(style = "font-size:.8em;color:#777;margin-top:-4px;",
                         "Drafted from the current plot and the Experiment Notebook: measure, replicate n, ",
                         "error type, MOI / host / phage / medium. Copy into the manuscript and edit."),
                       verbatimTextOutput("figure_legend_text", placeholder = TRUE)
                   )
                 ),
                 
                 # ── Plot Dimensions & Export ──────────────────────────────────────────────
                 tags$details(
                   tags$summary("Plot Dimensions & Export"),
                   div(class = "panel-section",
                       h4("On-screen Preview", class = "panel-title"),
                       radioButtons("preview_mode", NULL,
                                    choices  = c("Screen canvas (pixels)"                      = "canvas",
                                                 "True export size (what Inkscape will open)" = "wysiwyg"),
                                    selected = "canvas"),
                       conditionalPanel(condition = "input.preview_mode == 'canvas'",
                                        fluidRow(
                                          column(6, numericInput("plot_width",  "Width (px):",  820, 400, 2000)),
                                          column(6, numericInput("plot_height", "Height (px):", 600, 300, 1500))
                                        )
                       ),
                       conditionalPanel(condition = "input.preview_mode == 'wysiwyg'",
                                        sliderInput("preview_zoom", "Zoom (x physical size):", 1, 4, 2, 0.5),
                                        p(style = "font-size:.8em;color:#777;margin-top:-6px;",
                                          "Rendered at the export width/height below through the same ",
                                          "pipeline as the download; text and lines appear at their true pt size.")
                       ),
                       checkboxInput("custom_aspect_ratio", "Custom Aspect Ratio", FALSE),
                       conditionalPanel(condition = "input.custom_aspect_ratio == true",
                                        numericInput("aspect_ratio", "Width/Height:", 1, 0.3, 4, 0.1)
                       ),
                       h4("Image Export", class = "panel-title"),
                       selectInput("export_format", "Format:",
                                   choices  = c("PDF (fonts embedded)" = "pdf",
                                                "SVG (Inkscape-ready)"  = "svg",
                                                "PNG"                   = "png",
                                                "TIFF (LZW)"            = "tiff",
                                                "JPEG"                  = "jpeg"),
                                   selected = "pdf"),
                       radioButtons("export_units", "Size units:",
                                    choices = c("inches" = "in", "mm" = "mm"),
                                    selected = "in", inline = TRUE),
                       fluidRow(
                         column(6, numericInput("export_width",  "Width (in):",  10, 0.5, 400)),
                         column(6, numericInput("export_height", "Height (in):", 8,  0.5, 400))
                       ),
                       uiOutput("export_size_note"),
                       numericInput("export_dpi", "DPI (raster formats):", 300, 72, 1200),
                       downloadButton("downloadPlot", "Download Image"),
                       hr(),
                       h4("PowerPoint Export", class = "panel-title"),
                       p(style = "font-size:.85em;color:#666;",
                         "Cumulative build: slide 1 shows sample 1, slide 2 adds sample 2, etc. Each slide title shows the newly-added sample name. Final slide = all samples, identical to the main plot."),
                       pptx_ui,
                       hr(),
                       h4("Animated GIF Export", class = "panel-title"),
                       p(style = "font-size:.85em;color:#666;",
                         "Cumulative build: each frame adds one more line. No grey masks — lines already drawn stay fully visible. Axes, theme, and error bars match the main plot exactly."),
                       numericInput("gif_fps",    "Frames per second:", 1,   0.2, 10,   0.2),
                       numericInput("gif_width",  "GIF Width (px):",    800, 300, 2000, 50),
                       numericInput("gif_height", "GIF Height (px):",   600, 200, 1500, 50),
                       gif_ui
                   )
                 ),

                 # ── Experiment Notebook ───────────────────────────────────────────────────
                 # Structured lab-notebook record (modeled on the companion app's
                 # Experiment Notes tab). Everything filled in here is embedded in
                 # the HTML / PDF reports; empty fields are skipped.
                 tags$details(
                   tags$summary("Experiment Notebook"),
                   div(class = "panel-section",
                       h4("Experiment Identity", class = "panel-title"),
                       textInput("nb_exp_id",       "Experiment ID / Name:", ""),
                       fluidRow(
                         column(6, dateInput("nb_date", "Date:", value = Sys.Date())),
                         column(6, textInput("nb_experimenter", "Experimenter:", ""))
                       ),
                       fluidRow(
                         column(6, textInput("nb_project",     "Project:", "")),
                         column(6, textInput("nb_institution", "Lab / Institution:", ""))
                       )
                   ),
                   div(class = "panel-section",
                       h4("Biology & Conditions", class = "panel-title"),
                       selectInput("nb_exp_type", "Experiment Type:",
                                   choices = c("Phage Infection", "Plasmid Induction",
                                               "Growth Curve", "Lysis Curve",
                                               "Dose-Response", "Other"),
                                   selected = "Phage Infection"),
                       fluidRow(
                         column(6, textInput("nb_host_strain",   "Host Strain(s):", "")),
                         column(6, textInput("nb_phage_plasmid", "Phage / Plasmid:", ""))
                       ),
                       fluidRow(
                         column(6, textInput("nb_moi",       "MOI:", "")),
                         column(6, textInput("nb_replicate", "Replicate / Batch:", ""))
                       ),
                       fluidRow(
                         column(6, textInput("nb_media",       "Growth Medium:", "")),
                         column(6, textInput("nb_temperature", "Temperature (°C):", ""))
                       ),
                       fluidRow(
                         column(6, textInput("nb_time_inf", "Time of Infection/Induction:", "")),
                         column(6, textInput("nb_inducer",  "Inducer (+ conc.):", ""))
                       )
                   ),
                   div(class = "panel-section",
                       h4("Notes", class = "panel-title"),
                       textAreaInput("nb_observations", "Observations / Results:", "", rows = 3),
                       textAreaInput("nb_issues",       "Issues / Anomalies:",     "", rows = 2),
                       textAreaInput("nb_next_steps",   "Next Steps:",             "", rows = 2),
                       textInput("nb_tags", "Tags (comma-separated):", "",
                                 placeholder = "e.g. pilot, MG1655, N4, low-MOI"),
                       h5("Custom Fields", style = "margin-top:10px;"),
                       fluidRow(
                         column(5, textInput("nb_custom_key1", "Key:",   "", placeholder = "Field")),
                         column(7, textInput("nb_custom_val1", "Value:", ""))
                       ),
                       fluidRow(
                         column(5, textInput("nb_custom_key2", "Key:",   "", placeholder = "Field")),
                         column(7, textInput("nb_custom_val2", "Value:", ""))
                       ),
                       fluidRow(
                         column(5, textInput("nb_custom_key3", "Key:",   "", placeholder = "Field")),
                         column(7, textInput("nb_custom_val3", "Value:", ""))
                       )
                   )
                 ),

                 # ── Stats & Reports ───────────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Stats & Reports"),
                   div(class = "panel-section",
                       h4("Statistics Export", class = "panel-title"),
                       p(style = "font-size:.82em;color:#555;",
                         "Tables reflect the current sample selection and time filtering — ",
                         "exactly what the plot shows."),
                       downloadButton("downloadStatsCSV", "Summary Statistics (CSV)",
                                      style = "width:100%;"),
                       br(), br(),
                       downloadButton("downloadMetricsCSV", "Curve Metrics (CSV)",
                                      style = "width:100%;"),
                       p(style = "font-size:.78em;color:#777;margin-top:4px;",
                         "Per-sample growth & lysis metrics (same engine as the lysis-paper ",
                         "companion app): μmax, doubling time, lag phase, stationary ",
                         "duration, lysis onset/rate, OD drop, residual OD, recovery slope, ",
                         "AUC, lysis efficiency, and more. Computed from the MEAN curve — a ",
                         "single number per sample, with no replicate-to-replicate uncertainty."),
                       br(),
                       downloadButton("downloadReplicateMetricsCSV",
                                      "Per-Replicate Metrics, mean ± SD (CSV)",
                                      style = "width:100%;"),
                       p(style = "font-size:.78em;color:#777;margin-top:4px;",
                         "KC-4: the SAME metrics engine run once per individual replicate curve, ",
                         "then reported as mean ± SD across replicates per sample — additive ",
                         "alongside Curve Metrics above, not a replacement for it. These two are ",
                         "not the same number: most of these metrics are nonlinear functions of ",
                         "the curve, so the mean of the per-replicate metrics is generally not ",
                         "equal to the metric of the mean curve. For long-format files with no ",
                         "explicit replicate-ID column, replicate identity is inferred from row ",
                         "order within each (sample, time) group."),
                       numericInput("metrics_smooth_window",
                                    "μmax smoothing window (points):", 5, 3, 15, 1),
                       uiOutput("nonpositive_od_warning_ui"),
                       numericInput("metrics_peak_smooth_window",
                                    "Peak-detection smoothing window (points, 1 = off):",
                                    1, 1, 15, 2),
                       p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                         "OFF by default. At typical replicate noise, which point counts as ",
                         "“the peak” can shift by a whole time point between runs — ",
                         "and lysis_time, lysis_rate, od_drop, residual_od, lysis_efficiency, ",
                         "recovery_ratio and t_half_max_decline are all anchored on it. Raising ",
                         "this locates the peak on a centered moving average (a simple, ",
                         "documented smoother) instead of the raw curve; every reported OD ",
                         "value still comes from the raw data — smoothing only decides ",
                         "WHERE the peak is."),
                       selectInput("metrics_ref_sample",
                                   "Reference sample (infection metrics):",
                                   choices = c("None" = "")),
                       p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                         "Pick your uninfected control to add relative growth, infection ",
                         "strength, relative μmax, relative max OD, and lysis-onset Δ."),
                       hr(),
                       h4("Analysis Report", class = "panel-title"),
                       p(style = "font-size:.82em;color:#555;",
                         "One self-contained file: figure, summary statistics, curve metrics ",
                         "with definitions, data provenance, key settings, and session info."),
                       textInput("report_title",  "Report Title:", "Lysis Curve Analysis Report"),
                       textInput("report_author", "Author / Lab:", ""),
                       textAreaInput("report_notes", "Summary / notes (optional):", "",
                                     rows = 3, resize = "vertical"),
                       numericInput("report_font_size", "Report base font size (pt):", 14, 9, 24),
                       p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                         "Scales all report text and tables (HTML and PDF). ",
                         "Everything in the Experiment Notebook section is included too."),
                       html_report_ui,
                       pdf_report_ui
                   )
                 ),

                 # ── Variable Styling ──────────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Variable Styling"),
                   div(class = "panel-section",
                       h4("Sample Selection", class = "panel-title"),
                       selectInput("selected_samples", "Select Samples:",
                                   choices = character(0), multiple = TRUE),
                       uiOutput("var_settings")
                   )
                 )
                 
    ), # end sidebarPanel
    
    mainPanel(width = 9,
      tabsetPanel(id = "main_tabs",
        tabPanel("Lysis curve", br(), uiOutput("plot_container")),
        # ── Analysis figures (v2.11) ──────────────────────────────────────────
        tabPanel("Analysis figures", br(),
          div(class = "analysis-controls",
            fluidRow(
              column(3,
                selectInput("analysis_type", "Figure:",
                            choices = c("Metric summary (replicates, mean \u00b1 SD)" = "metric_summary",
                                        "Specific growth rate vs time"              = "growth_rate",
                                        "Normalized OD vs time"                     = "normalized",
                                        "Replicate overlay"                         = "replicate_overlay",
                                        "MOI dose-response"                         = "dose_response",
                                        "OD heatmap (samples \u00d7 time)"           = "heatmap"),
                            selected = "metric_summary"),
                textInput("analysis_panel_label", "Panel label:", "b")
              ),
              column(3,
                conditionalPanel("input.analysis_type == 'metric_summary'",
                  selectInput("analysis_metrics", "Metrics (one facet each):",
                              choices  = setNames(names(ANALYSIS_METRIC_LABELS), ANALYSIS_METRIC_LABELS),
                              selected = c("lysis_time", "od_drop"), multiple = TRUE),
                  p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                    "Open circles: one per replicate; bar: mean; whiskers: \u00b1 SD. ",
                    "Relative metrics need a reference sample (Stats & Reports).")),
                conditionalPanel("input.analysis_type == 'dose_response'",
                  selectInput("analysis_dose_metric", "Metric vs MOI:",
                              choices  = setNames(names(ANALYSIS_METRIC_LABELS), ANALYSIS_METRIC_LABELS),
                              selected = "lysis_time"),
                  textAreaInput("moi_override", "MOI per sample (if not in the name):", "",
                                rows = 2, placeholder = "Phage A = 10; Phage B = 0.1"),
                  p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                    "MOI is read from names like 'Phage MOI 10'. Samples without a positive MOI ",
                    "(e.g. the uninfected control) are left out; the x axis is log10.")),
                conditionalPanel("input.analysis_type == 'normalized'",
                  radioButtons("analysis_norm_mode", "Relative to:",
                               c("Reference sample" = "reference", "Own t = 0 value" = "t0"),
                               selected = "reference", inline = TRUE),
                  p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                    "Reference mode divides each mean curve by the uninfected control at every ",
                    "time point: the infection-strength metric drawn out over time.")),
                conditionalPanel("input.analysis_type == 'growth_rate'",
                  p(style = "font-size:.78em;color:#777;",
                    "\u03bc(t) = d ln(OD)/dt per replicate by the same rolling regression as ",
                    "\u03bcmax (window under Stats & Reports), then mean \u00b1 SD (band). ",
                    "Lysis shows as a negative rate.")),
                conditionalPanel("input.analysis_type == 'replicate_overlay'",
                  p(style = "font-size:.78em;color:#777;",
                    "Every replicate curve (thin) with the mean (bold): the variance the error ",
                    "bars summarise.")),
                conditionalPanel("input.analysis_type == 'heatmap'",
                  p(style = "font-size:.78em;color:#777;",
                    "Mean OD per sample and time point; for screens with many samples."))
              ),
              column(3,
                selectInput("stats_method", "Statistics:",
                            choices = c("Welch t / ANOVA + Tukey"        = "welch",
                                        "Mann-Whitney / Kruskal-Wallis"  = "nonparametric"),
                            selected = "welch"),
                checkboxInput("stats_mark", "Mark significance vs reference on the metric summary", TRUE),
                p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                  "Tested on the per-replicate metric (first selected metric, or the dose-response ",
                  "metric) against the reference sample, Holm-adjusted; all pairs when no reference is set.")
              ),
              column(3,
                p(style = "font-size:.82em;color:#555;",
                  "Same fonts, line weights, legend, colours and export size as the main figure ",
                  "(journal preset included), so panels assemble without rescaling."),
                downloadButton("downloadAnalysisPlot", "Download figure", style = "width:100%;"),
                br(), br(),
                downloadButton("downloadAnalysisStatsCSV", "Download statistics (CSV)", style = "width:100%;")
              )
            )
          ),
          uiOutput("analysis_container"),
          h4("Replicate statistics", style = "margin-top:18px;"),
          uiOutput("analysis_stats_notes"),
          tableOutput("analysis_stats_table"),
          tableOutput("analysis_stats_omnibus"),
          tableOutput("analysis_stats_posthoc")
        ),
        # ── Dashboard (v2.11) ─────────────────────────────────────────────────
        tabPanel("Dashboard", br(),
          uiOutput("dash_cards"),
          h4("Key metrics per sample", style = "margin-top:16px;"),
          p(style = "font-size:.8em;color:#777;margin-top:-4px;",
            "From the mean curve; lysis onset and OD drop also show \u00b1 SD across replicates. ",
            "Full tables: Stats & Reports \u2192 CSV downloads."),
          tableOutput("dash_metrics_table"),
          fluidRow(
            column(6, h5("Lysis curve"),           plotOutput("dash_fig_main",    height = "280px")),
            column(6, h5("Specific growth rate"),  plotOutput("dash_fig_rate",    height = "280px"))
          ),
          fluidRow(
            column(6, h5(textOutput("dash_fig_norm_title", inline = TRUE)),
                      plotOutput("dash_fig_norm",    height = "280px")),
            column(6, h5("Metric summary"),        plotOutput("dash_fig_metrics", height = "280px"))
          ),
          h4("Statistics", style = "margin-top:16px;"),
          uiOutput("dash_notes"),
          tableOutput("dash_stats"),
          h4("Experiment", style = "margin-top:16px;"),
          uiOutput("dash_notebook")
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data              = NULL,
    data_source       = NULL,   # file name (or "Demo data") for provenance
    time_col          = NULL,
    od_vars           = NULL,
    od_vars_raw       = NULL,
    col_map           = NULL,   # wide format: sample name -> integer column indices
    group_col         = NULL,
    value_col         = NULL,
    is_long_format    = FALSE,
    palette_colors    = NULL,
    all_timepoints    = NULL,
    # imported_settings is a list with two named sublists:
    #   $global  — named list of scalar values for global inputs
    #   $samples — named list keyed by SAMPLE NAME, each a list of per-sample aesthetics
    imported_settings = NULL,
    settings_status   = NULL,
    style_page        = 1L      # current page in Variable Styling panel (pagination)
  )
  
  # ── Palette helpers ──────────────────────────────────────────────────────────
  get_palette_colors <- function(pal, n) {
    switch(pal,
           viridis     = scales::viridis_pal(option = "D")(n),
           plasma      = scales::viridis_pal(option = "C")(n),
           colorblind  = rep_len(c("#000000","#E69F00","#56B4E9","#009E73",
                                   "#F0E442","#0072B2","#D55E00","#CC79A7"), n),
           publication = rep_len(c("#E41A1C","#377EB8","#4DAF4A","#984EA3",
                                   "#FF7F00","#FFFF33","#A65628","#F781BF"), n),
           rainbow     = rainbow(n),
           gray        = gray.colors(n, start = 0.15, end = 0.85),
           NULL
    )
  }
  
  # v2.10: warn when the chosen font family is not installed -- every device
  # would silently substitute another face, and the SVG would name a font the
  # Inkscape machine may not have either.
  observeEvent(input$font_family, {
    fam <- input$font_family
    if (is.null(fam) || fam %in% c("sans", "serif", "mono")) return()
    hit <- tryCatch(basename(systemfonts::match_fonts(fam)$path), error = function(e) "")
    if (!grepl(gsub(" ", "", fam), gsub(" ", "", hit), ignore.case = TRUE))
      showNotification(paste0("Font '", fam, "' is not installed on this computer; exports will ",
                              "substitute ", sub("\\.[A-Za-z]+$", "", hit), ". Install it, or choose ",
                              "Arial / Helvetica (accepted by every journal)."),
                       type = "warning", duration = 10)
  }, ignoreInit = TRUE)
  
  observeEvent(input$color_palette, {
    rv$palette_colors <- if (input$color_palette != "custom" && !is.null(rv$od_vars))
      get_palette_colors(input$color_palette, length(rv$od_vars)) else NULL
    # v2.10: these two palettes have 8 distinct colours; beyond that they repeat.
    if (input$color_palette %in% c("colorblind", "publication") &&
        !is.null(rv$od_vars) && length(rv$od_vars) > 8)
      showNotification(paste0("The '", input$color_palette, "' palette has 8 distinct colours but ",
                              length(rv$od_vars), " samples are loaded, so colours will repeat. ",
                              "Use Viridis, or set colours per sample."),
                       type = "warning", duration = 10)
  })
  
  output$palette_preview <- renderUI({
    if (input$color_palette == "custom" || is.null(rv$od_vars)) return(NULL)
    cols <- get_palette_colors(input$color_palette, min(8, length(rv$od_vars)))
    div(p("Preview:"),
        div(style = "margin-top:6px;",
            lapply(cols, function(col)
              div(style = paste0("display:inline-block;width:28px;height:20px;",
                                 "background:", col, ";margin-right:4px;",
                                 "border:1px solid #ccc;")))))
  })
  
  # ── Time filter UI ───────────────────────────────────────────────────────────
  output$time_range_slider_ui <- renderUI({
    tp <- rv$all_timepoints
    if (is.null(tp) || length(tp) < 2)
      return(p(style = "color:#999;font-size:.85em;", "Load a data file to enable the range slider."))
    t_min  <- min(tp); t_max <- max(tp)
    t_step <- max(min(diff(tp)), 0.001)
    sliderInput("time_filter_range", label = NULL,
                min = t_min, max = t_max, value = c(t_min, t_max),
                step = t_step, width = "100%")
  })
  
  output$excluded_timepoints_preview <- renderUI({
    if (is.null(rv$all_timepoints)) return(NULL)
    excl <- parse_excluded_timepoints(input$exclude_timepoints, rv$all_timepoints)
    if (length(excl) == 0) return(NULL)
    div(class = "excl-preview",
        paste0("Will exclude ", length(excl), " point(s): ", paste(sort(excl), collapse = ", ")))
  })
  
  # ── Data loading ─────────────────────────────────────────────────────────────
  is_long_format_detect <- function(data) {
    if (nrow(data) == 0) return(FALSE)
    potential <- c("variable","Variable","condition","Condition","treatment","Treatment",
                   "sample","Sample","group","Group")
    if (any(potential %in% colnames(data))) return(TRUE)
    if (ncol(data) <= 4) {
      # Only NON-NUMERIC columns can be grouping candidates: a numeric OD
      # column with repeated values (e.g. a flat/constant control) must not
      # flip a small wide file into long format.
      nn <- !vapply(data, is.numeric, logical(1))
      if (!any(nn)) return(FALSE)
      vd <- sapply(data[nn], function(x) length(unique(x)) / nrow(data))
      return(any(vd < 0.3 & vd > 0))
    }
    FALSE
  }
  
  detect_time_column <- function(data) {
    hits <- grep("^(time|Time|TIME|t|T)$", colnames(data), value = TRUE)
    if (length(hits)) return(hits[1])
    hits2 <- grep("time|Time", colnames(data), value = TRUE)
    if (length(hits2)) return(hits2[1])
    for (nm in colnames(data)) {
      v <- suppressWarnings(as.numeric(data[[nm]]))
      if (!any(is.na(v)) && length(v) > 1 && all(diff(v) >= 0)) return(nm)
    }
    colnames(data)[1]
  }
  
  detect_od_columns <- function(data, exclude_cols) {
    cols <- setdiff(colnames(data), exclude_cols)
    hits <- grep("od|OD|absorbance|Absorbance|value|Value", cols,
                 ignore.case = TRUE, value = TRUE)
    if (length(hits)) return(hits)
    cols[sapply(data[cols], is.numeric)]
  }

  # Wide format: detect OD columns BY POSITION so replicate columns that share
  # a name (the documented replicate workflow) are all kept. Returns integer
  # indices into colnames(data).
  detect_od_column_idx <- function(data, time_col) {
    cn       <- colnames(data)
    time_idx <- which(cn == time_col)
    cand     <- setdiff(seq_along(cn), time_idx)
    hits     <- cand[grepl("od|absorbance|value", cn[cand], ignore.case = TRUE)]
    if (length(hits)) return(hits)
    cand[vapply(cand, function(i) is.numeric(data[[i]]), logical(1))]
  }

  # Shared ingestion for both file upload and demo data
  ingest_data <- function(data, source_name) {
    rv$is_long_format <- is_long_format_detect(data)
    rv$time_col       <- detect_time_column(data)
    rv$col_map        <- NULL

    if (rv$is_long_format) {
      potential <- c("variable","Variable","condition","Condition","treatment","Treatment",
                     "sample","Sample","group","Group")
      found     <- potential[potential %in% colnames(data)]
      group_col <- if (length(found) > 0) found[1] else {
        dv    <- sapply(data, function(x) length(unique(x)) / nrow(data))
        cands <- setdiff(names(which(dv < 0.3 & dv > 0)), rv$time_col)
        if (length(cands)) cands[1] else setdiff(colnames(data), rv$time_col)[1]
      }
      vc <- detect_od_columns(data, c(rv$time_col, group_col))
      if (length(vc) > 1) vc <- vc[1]
      rv$group_col   <- group_col
      rv$value_col   <- vc
      rv$od_vars     <- as.character(unique(data[[group_col]]))
      rv$od_vars_raw <- rv$od_vars
    } else {
      # Index-based detection keeps ALL replicate columns, including
      # duplicates of the same name (v2.8 REPLICATE FIX).
      idx <- detect_od_column_idx(data, rv$time_col)
      cn  <- colnames(data)
      oc  <- unique(cn[idx])
      rv$col_map     <- lapply(setNames(oc, oc), function(nm) idx[cn[idx] == nm])
      rv$od_vars     <- oc
      rv$od_vars_raw <- oc
    }
    rv$data        <- data
    rv$data_source <- source_name

    tr <- range(suppressWarnings(as.numeric(as.character(data[[rv$time_col]]))), na.rm = TRUE)
    updateNumericInput(session, "x_min", value = tr[1])
    updateNumericInput(session, "x_max", value = tr[2])
    updateSelectInput(session, "selected_samples", choices = rv$od_vars, selected = rv$od_vars)
    updateSelectInput(session, "metrics_ref_sample",
                      choices = c("None" = "", setNames(rv$od_vars, rv$od_vars)),
                      selected = "")

    all_t <- sort(unique(suppressWarnings(as.numeric(as.character(data[[rv$time_col]])))))
    rv$all_timepoints <- all_t[is.finite(all_t)]
    updateTextInput(session, "exclude_timepoints", value = "")

    if (!is.null(input$color_palette) && input$color_palette != "custom")
      rv$palette_colors <- get_palette_colors(input$color_palette, length(rv$od_vars))

    # After loading data, auto-apply any imported per-sample aesthetics for matching names
    apply_imported_sample_aesthetics()
  }

  observeEvent(input$file, {
    req(input$file)
    data <- read_csv_locale_aware(input$file$datapath, input$csv_locale %||% "auto")
    ingest_data(data, input$file$name)
  })

  observeEvent(input$load_demo, {
    ingest_data(make_demo_data(), "Demo data (built-in)")
  })

  # ── Data summary (what the app detected) ─────────────────────────────────────
  output$data_summary_ui <- renderUI({
    req(rv$data)
    rep_txt <- if (rv$is_long_format) {
      cnts <- table(rv$data[[rv$group_col]])
      ntp  <- length(rv$all_timepoints)
      per  <- if (ntp > 0) round(as.numeric(cnts) / ntp, 1) else NA
      paste0(names(cnts), " (~", per, " rep/time)", collapse = ", ")
    } else {
      paste0(names(rv$col_map), " (", lengths(rv$col_map),
             ifelse(lengths(rv$col_map) > 1, " replicate cols)", " col)"),
             collapse = ", ")
    }
    div(class = "settings-status ok", style = "margin-bottom:10px;",
        HTML(paste0(
          "<b>", html_escape(rv$data_source %||% "data"), "</b><br/>",
          "Format: <b>", if (rv$is_long_format) "long" else "wide", "</b> &middot; ",
          "Time column: <b>", html_escape(rv$time_col), "</b> &middot; ",
          length(rv$all_timepoints), " time points<br/>",
          "Samples: ", html_escape(rep_txt))))
  })
  
  # ── Settings: Save ───────────────────────────────────────────────────────────
  # Saves global visual settings AND per-sample aesthetics keyed by sample name.
  # Explicitly excluded: file path, loaded CSV, sample selection, time filter range —
  # anything that belongs to the data rather than the visualization.
  output$saveSettings <- downloadHandler(
    filename = function()
      paste0("plot_settings_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"),
    content = function(file) {
      all_inputs <- reactiveValuesToList(input)
      
      # Inputs that belong to the data/session — never saved
      data_inputs     <- c("file", "loadSettings", "selected_samples",
                           "time_filter_range", "exclude_timepoints",
                           "apply_journal_preset")
      # Per-sample dynamic input prefixes — handled separately below
      sample_prefixes <- c("line_type_","shape_","shape_filled_","color_selector_",
                           "color_","use_rgb_","red_","green_","blue_","alpha_",
                           "use_hex_","hex_color_","legend_label_",
                           "apply_rgb_","apply_hex_")
      # v2.8 SETTINGS SAVE FIX: these GLOBAL inputs collide with per-sample
      # prefixes ("shape_", "color_") and were silently dropped from saved files
      global_exceptions <- c("shape_size", "color_palette")
      is_sample_inp <- function(nm) !(nm %in% global_exceptions) &&
        any(vapply(sample_prefixes, startsWith, logical(1), x = nm))
      
      # Global visual settings: everything that isn't data-specific or per-sample
      global_settings <- all_inputs[vapply(names(all_inputs), function(nm)
        !(nm %in% data_inputs) && !is_sample_inp(nm), logical(1))]
      
      # Per-sample aesthetics keyed by sample name
      sample_aesthetics <- list()
      samps <- isolate(input$selected_samples)
      if (!is.null(samps)) {
        for (vn in samps) {
          vid     <- safe_id(vn)
          leg     <- input[[paste0("legend_label_", vid)]]
          col_sel <- input[[paste0("color_selector_", vid)]]
          color   <- if (!is.null(col_sel) && col_sel == "custom")
            normalize_hex_color(input[[paste0("color_", vid)]])
          else if (!is.null(col_sel)) col_sel
          else "#000000"
          sample_aesthetics[[vn]] <- list(
            color        = color,
            shape        = input[[paste0("shape_",        vid)]],
            shape_filled = input[[paste0("shape_filled_", vid)]],
            line_type    = input[[paste0("line_type_",    vid)]],
            legend_label = if (!is.null(leg) && nchar(leg) > 0) leg else vn
          )
        }
      }
      
      out <- list(
        version           = APP_VERSION,
        saved_at          = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        global_settings   = global_settings,
        sample_aesthetics = sample_aesthetics
      )
      write_json(out, file, pretty = TRUE, auto_unbox = TRUE)
    }
  )
  
  # ── Settings: Load ───────────────────────────────────────────────────────────
  # Applies global visual settings AND per-sample aesthetics from the file.
  # Never touches: file input, sample selection, time filter range.
  observeEvent(input$loadSettings, {
    req(input$loadSettings)
    s <- tryCatch(
      fromJSON(input$loadSettings$datapath, simplifyVector = FALSE),
      error = function(e) {
        rv$settings_status <- list(type = "warn",
                                   msg = paste("Could not parse settings file:", e$message), details = NULL)
        NULL
      }
    )
    if (is.null(s)) return()
    
    # Support both v2.5 structured format and legacy flat format
    global     <- if (!is.null(s$global_settings))   s$global_settings   else list()
    sample_aes <- if (!is.null(s$sample_aesthetics)) s$sample_aesthetics
    else if (!is.null(s$samples))       s$samples
    else list()
    
    rv$imported_settings <- list(global = global, samples = sample_aes)
    
    # ── Apply global visual settings ──────────────────────────────────────────
    # Skip anything data-specific or per-sample — only visual controls
    data_inputs     <- c("file","loadSettings","selected_samples",
                         "time_filter_range","exclude_timepoints",
                         "apply_journal_preset")
    sample_prefixes <- c("line_type_","shape_","shape_filled_","color_selector_",
                         "color_","use_rgb_","red_","green_","blue_","alpha_",
                         "use_hex_","hex_color_","legend_label_","apply_rgb_","apply_hex_")
    global_exceptions <- c("shape_size", "color_palette")   # see save handler note
    is_sample_inp <- function(nm) !(nm %in% global_exceptions) &&
      any(vapply(sample_prefixes, startsWith, logical(1), x = nm))
    
    # v2.10: a file saved in mm must not be re-converted by the units observer;
    # a pre-2.10 file has no export_units and its sizes are inches, so the
    # radio is switched back to inches before the sizes arrive.
    if (is.character(global$export_units)) {
      rv$units_seen <- global$export_units
    } else {
      rv$units_seen <- "in"
      updateRadioButtons(session, "export_units", selected = "in")
    }
    n_global <- 0L
    for (nm in names(global)) {
      if (nm %in% data_inputs || is_sample_inp(nm)) next
      if (!(nm %in% names(input)))                  next
      tryCatch({
        val <- global[[nm]]
        if (is.logical(val) && length(val) == 1) {
          updateCheckboxInput(session, nm, value = val)
        } else if (is.numeric(val) && length(val) == 1) {
          updateNumericInput(session, nm, value = val)
        } else if (is.character(val) && length(val) == 1) {
          updateSelectInput(session, nm, selected = val)
          updateTextInput(session,   nm, value    = val)
        }
        n_global <- n_global + 1L
      }, error = function(e) invisible(NULL))
    }
    
    # ── Apply per-sample aesthetics for any matching names ────────────────────
    apply_imported_sample_aesthetics()
    
    cur_samps   <- isolate(input$selected_samples)
    saved_names <- names(sample_aes)
    matched     <- intersect(cur_samps,  saved_names)
    unmatched   <- setdiff(saved_names,  cur_samps)
    
    sample_msg <- if (length(saved_names) == 0) "No sample styles in file."
    else paste0(length(matched), "/", length(saved_names), " sample style(s) applied.")
    
    details_html <- if (length(saved_names) > 0) {
      match_lines   <- if (length(matched)   > 0)
        paste0("<li style='color:#155724'>&#10003; ", matched, "</li>", collapse = "")
      else ""
      unmatch_lines <- if (length(unmatched) > 0)
        paste0("<li style='color:#856404'>&#9675; ", unmatched,
               " <em>(not in current data)</em></li>", collapse = "")
      else ""
      paste0("<ul class='match-list'>", match_lines, unmatch_lines, "</ul>")
    } else NULL
    
    rv$settings_status <- list(
      type    = "ok",
      msg     = paste0(n_global, " global setting(s) applied. ", sample_msg),
      details = details_html
    )
  })
  
  # Applies imported per-sample aesthetics to any matching currently-loaded samples.
  # Called both after loadSettings and after a new CSV is loaded.
  apply_imported_sample_aesthetics <- function() {
    imp <- rv$imported_settings
    if (is.null(imp) || is.null(imp$samples) || length(imp$samples) == 0) return()
    cur_samps <- isolate(input$selected_samples)
    if (is.null(cur_samps) || length(cur_samps) == 0) return()
    for (vn in cur_samps) {
      if (!vn %in% names(imp$samples)) next
      aes   <- imp$samples[[vn]]
      vid   <- safe_id(vn)
      if (!is.null(aes$color)) {
        updateSelectInput(session, paste0("color_selector_", vid), selected = "custom")
        updateTextInput(session,   paste0("color_",          vid), value   = aes$color)
        updateTextInput(session,   paste0("hex_color_",      vid), value   = aes$color)
      }
      if (!is.null(aes$shape))
        updateSelectInput(session, paste0("shape_", vid), selected = as.character(aes$shape))
      if (!is.null(aes$shape_filled))
        updateCheckboxInput(session, paste0("shape_filled_", vid), value = aes$shape_filled)
      if (!is.null(aes$line_type))
        updateSelectInput(session, paste0("line_type_", vid), selected = aes$line_type)
      if (!is.null(aes$legend_label))
        updateTextInput(session, paste0("legend_label_", vid), value = aes$legend_label)
    }
  }
  
  # ── Settings: Clear ──────────────────────────────────────────────────────────
  # Revokes all imported settings: resets global visual settings to app defaults
  # AND restores per-sample aesthetics to position-based defaults.
  # Never touches: data, sample selection, time filter.
  observeEvent(input$clearSettings, {
    if (is.null(rv$imported_settings)) {
      rv$settings_status <- list(type = "warn", msg = "No imported settings to clear.", details = NULL)
      return()
    }
    
    # ── Reset global visual settings to app defaults ──────────────────────────
    defs <- APP_DEFAULTS
    rv$units_seen <- "in"   # v2.10: see observeEvent(input$export_units)
    for (nm in names(defs)) {
      tryCatch({
        val <- defs[[nm]]
        if (is.logical(val))      updateCheckboxInput(session, nm, value    = val)
        else if (is.numeric(val)) updateNumericInput( session, nm, value    = val)
        else { updateSelectInput(session, nm, selected = val)
          updateTextInput(session,  nm, value    = val) }
      }, error = function(e) invisible(NULL))
    }
    
    # ── Reset per-sample aesthetics to position-based defaults ───────────────
    cur_samps  <- isolate(input$selected_samples)
    def_colors <- c("#000000","#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00",
                    "#A65628","#F781BF","#999999","#66C2A5","#FC8D62","#8DA0CB")
    shape_opts <- c(16, 15, 17, 18, 25, 4, 8, 23, 3, 10)
    if (!is.null(cur_samps)) {
      for (i in seq_along(cur_samps)) {
        vn        <- cur_samps[i]; vid <- safe_id(vn)
        def_col   <- def_colors[(i - 1) %% length(def_colors) + 1]
        def_shape <- as.character(shape_opts[(i - 1) %% length(shape_opts) + 1])
        tryCatch({
          updateSelectInput(session,   paste0("color_selector_", vid), selected = "custom")
          updateTextInput(session,     paste0("color_",          vid), value    = def_col)
          updateTextInput(session,     paste0("hex_color_",      vid), value    = def_col)
          updateSelectInput(session,   paste0("shape_",          vid), selected = def_shape)
          updateCheckboxInput(session, paste0("shape_filled_",   vid), value    = TRUE)
          updateSelectInput(session,   paste0("line_type_",      vid), selected = "solid")
          updateTextInput(session,     paste0("legend_label_",   vid), value    = vn)
        }, error = function(e) invisible(NULL))
      }
    }
    
    rv$imported_settings <- NULL
    rv$settings_status   <- list(
      type    = "ok",
      msg     = "All imported settings cleared. Global and per-sample settings restored to defaults.",
      details = NULL
    )
  })
  
  output$settings_status_ui <- renderUI({
    if (is.null(rv$settings_status)) return(NULL)
    cls <- if (rv$settings_status$type == "ok") "settings-status ok" else "settings-status warn"
    tagList(
      div(class = cls, rv$settings_status$msg),
      if (!is.null(rv$settings_status$details))
        HTML(paste0('<div style="font-size:.82em;margin-top:4px;">',
                    rv$settings_status$details, '</div>'))
    )
  })
  
  # Null-coalescing operator (must be defined before first use in renderUI below)
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # ── Dynamic UI: var_settings (paginated for large sample counts) ─────────────
  # When there are more than PAGE_SIZE samples, render only one page at a time.
  # This keeps the DOM small and prevents Shiny from serialising hundreds of inputs.
  PAGE_SIZE <- 20L
  
  # Reset to page 1 whenever the sample selection changes
  observeEvent(input$selected_samples, { rv$style_page <- 1L }, ignoreNULL = FALSE)
  observeEvent(input$style_prev_page,  { rv$style_page <- max(1L, rv$style_page - 1L) })
  observeEvent(input$style_next_page,  {
    n_pages <- ceiling(length(input$selected_samples) / PAGE_SIZE)
    rv$style_page <- min(n_pages, rv$style_page + 1L)
  })
  
  output$var_settings <- renderUI({
    req(rv$od_vars, input$selected_samples)
    all_samps <- input$selected_samples
    n_total   <- length(all_samps)
    n_pages   <- ceiling(n_total / PAGE_SIZE)
    page      <- rv$style_page
    idx_start <- (page - 1L) * PAGE_SIZE + 1L
    idx_end   <- min(page * PAGE_SIZE, n_total)
    page_samps <- all_samps[idx_start:idx_end]
    
    shape_opts <- c("Circle" = 16, "Square" = 15, "Triangle Up" = 17,
                    "Diamond" = 18, "Triangle Down" = 25, "Cross" = 4,
                    "X" = 8, "Open Circle" = 21, "Open Square" = 22,
                    "Open Triangle" = 24, "Plus" = 3, "Star" = 10)
    default_colors <- c("#000000","#E41A1C","#377EB8","#4DAF4A",
                        "#984EA3","#FF7F00","#A65628","#F781BF",
                        "#999999","#66C2A5","#FC8D62","#8DA0CB")
    named_colors   <- c("#000000","#E41A1C","#377EB8","#4DAF4A",
                        "#984EA3","#FF7F00","#FFFF33","#A65628",
                        "#F781BF","#999999","#009E73")
    
    # Pagination nav bar (only shown when needed)
    nav_bar <- if (n_pages > 1L) {
      div(style = "display:flex;align-items:center;justify-content:space-between;margin-bottom:10px;",
          actionButton("style_prev_page", "\u25C4 Prev",
                       style = "font-size:.8em;padding:3px 8px;",
                       disabled = if (page <= 1L) "disabled" else NULL),
          span(style = "font-size:.85em;color:#555;",
               paste0("Page ", page, " of ", n_pages,
                      " (samples ", idx_start, "\u2013", idx_end, " of ", n_total, ")")),
          actionButton("style_next_page", "Next \u25BA",
                       style = "font-size:.8em;padding:3px 8px;",
                       disabled = if (page >= n_pages) "disabled" else NULL)
      )
    } else NULL
    
    sample_widgets <- lapply(seq_along(page_samps), function(j) {
      i   <- idx_start + j - 1L   # global index across all samples
      vn  <- page_samps[j]
      vid <- safe_id(vn)
      shape_def <- as.character(shape_opts[1 + (i - 1L) %% length(shape_opts)])
      
      imp_aes <- rv$imported_settings$samples[[vn]]
      
      if (!is.null(imp_aes)) {
        color_def <- imp_aes$color %||% default_colors[1]
        sel_def   <- "custom"
      } else if (!is.null(rv$palette_colors) && input$color_palette != "custom") {
        idx       <- match(vn, rv$od_vars)
        color_def <- if (!is.na(idx)) rv$palette_colors[idx] else default_colors[1]
        sel_def   <- "custom"
      } else {
        color_def <- default_colors[1 + (i - 1L) %% length(default_colors)]
        sel_def   <- if (color_def %in% named_colors) color_def else "custom"
      }
      if (is.na(color_def) || is.null(color_def)) color_def <- "#000000"
      
      default_shape  <- if (!is.null(imp_aes$shape))       as.character(imp_aes$shape)    else shape_def
      default_lt     <- if (!is.null(imp_aes$line_type))    imp_aes$line_type              else "solid"
      default_filled <- if (!is.null(imp_aes$shape_filled)) imp_aes$shape_filled           else TRUE
      default_leg    <- if (!is.null(imp_aes$legend_label)) imp_aes$legend_label           else vn
      
      div(
        style = "margin-bottom:12px;padding:10px;border:1px solid #ddd;border-radius:6px;background:#fafafa;",
        h4(vn, style = "margin:0 0 8px 0;font-size:1em;color:#333;"),
        selectInput(paste0("line_type_", vid), "Line Type:",
                    choices  = c("Solid" = "solid", "Dashed" = "dashed",
                                 "Dotted" = "dotted", "DotDash" = "dotdash",
                                 "LongDash" = "longdash", "TwoDash" = "twodash"),
                    selected = default_lt),
        h5("Shape", style = "margin:8px 0 4px;"),
        fluidRow(
          column(6, selectInput(paste0("shape_", vid), "Shape:",
                                choices = shape_opts, selected = default_shape)),
          column(6, checkboxInput(paste0("shape_filled_", vid), "Filled", default_filled))
        ),
        div(class = "panel-section",
            h5("Color", class = "panel-title"),
            selectInput(paste0("color_selector_", vid), "Pick Color:",
                        choices  = c("Black" = "#000000", "Red"    = "#E41A1C",
                                     "Blue"  = "#377EB8", "Green"  = "#4DAF4A",
                                     "Purple"= "#984EA3", "Orange" = "#FF7F00",
                                     "Yellow"= "#FFFF33", "Brown"  = "#A65628",
                                     "Pink"  = "#F781BF", "Gray"   = "#999999",
                                     "Teal"  = "#009E73", "Custom" = "custom"),
                        selected = sel_def),
            conditionalPanel(
              condition = paste0("input['color_selector_", vid, "'] == 'custom'"),
              div(style = "display:flex;align-items:center;gap:8px;",
                  textInput(paste0("color_", vid), "HEX:", value = color_def),
                  div(id    = paste0("color_preview_", vid),
                      class = "color-preview",
                      style = paste0("background-color:", color_def, ";"))
              )
            ),
            checkboxInput(paste0("use_rgb_", vid), "RGB Sliders", FALSE),
            conditionalPanel(
              condition = paste0("input['use_rgb_", vid, "'] == true"),
              sliderInput(paste0("red_",   vid), "R:", 0, 255, 0, 1),
              sliderInput(paste0("green_", vid), "G:", 0, 255, 0, 1),
              sliderInput(paste0("blue_",  vid), "B:", 0, 255, 0, 1),
              sliderInput(paste0("alpha_", vid), "A:", 0,   1, 1, 0.01),
              actionButton(paste0("apply_rgb_", vid), "Apply RGB",
                           style = "margin-top:4px;font-size:.85em;")
            ),
            checkboxInput(paste0("use_hex_", vid), "Direct HEX Entry", FALSE),
            conditionalPanel(
              condition = paste0("input['use_hex_", vid, "'] == true"),
              div(style = "display:flex;align-items:center;gap:8px;",
                  textInput(paste0("hex_color_", vid), "HEX Code:", value = color_def),
                  actionButton(paste0("apply_hex_", vid), "Apply",
                               style = "font-size:.85em;")
              )
            )
        ),
        textInput(paste0("legend_label_", vid), "Legend Label:", value = default_leg)
      )
    })
    
    tagList(nav_bar, do.call(tagList, sample_widgets))
  })
  
  # ── Color reactivity ─────────────────────────────────────────────────────────
  # Observer registry: track which sample IDs have already had observers registered
  # so we never register the same observeEvent twice (prevents accumulation / leak).
  registered_color_obs <- character(0)
  
  observe({
    req(input$selected_samples, rv$data)
    new_samps <- setdiff(safe_id(input$selected_samples), registered_color_obs)
    if (length(new_samps) == 0) return()
    
    for (vn in input$selected_samples[safe_id(input$selected_samples) %in% new_samps]) {
      local({
        vi <- safe_id(vn)
        registered_color_obs <<- c(registered_color_obs, vi)
        
        observeEvent(input[[paste0("color_selector_", vi)]], {
          sel <- input[[paste0("color_selector_", vi)]]
          if (!is.null(sel) && sel != "custom") {
            rc <- tryCatch(col2rgb(sel), error = function(e) NULL)
            if (!is.null(rc)) {
              updateSliderInput(session, paste0("red_",   vi), value = rc[1, 1])
              updateSliderInput(session, paste0("green_", vi), value = rc[2, 1])
              updateSliderInput(session, paste0("blue_",  vi), value = rc[3, 1])
              updateTextInput(session,   paste0("color_",     vi), value = sel)
              updateTextInput(session,   paste0("hex_color_", vi), value = sel)
            }
          }
        }, ignoreInit = TRUE)
        
        observeEvent(input[[paste0("color_", vi)]], {
          col <- normalize_hex_color(input[[paste0("color_", vi)]])
          session$sendCustomMessage("updateColorPreview",
                                    list(id = paste0("color_preview_", vi), color = col))
          rc <- tryCatch(col2rgb(col), error = function(e) NULL)
          if (!is.null(rc)) {
            updateSliderInput(session, paste0("red_",   vi), value = rc[1, 1])
            updateSliderInput(session, paste0("green_", vi), value = rc[2, 1])
            updateSliderInput(session, paste0("blue_",  vi), value = rc[3, 1])
          }
        }, ignoreInit = TRUE)
        
        observeEvent(input[[paste0("apply_rgb_", vi)]], {
          hex <- rgb_to_hex(input[[paste0("red_",   vi)]],
                            input[[paste0("green_", vi)]],
                            input[[paste0("blue_",  vi)]],
                            input[[paste0("alpha_", vi)]])
          updateSelectInput(session, paste0("color_selector_", vi), selected = "custom")
          updateTextInput(session,   paste0("color_",     vi), value = hex)
          updateTextInput(session,   paste0("hex_color_", vi), value = hex)
        })
        
        observeEvent(input[[paste0("apply_hex_", vi)]], {
          hex <- normalize_hex_color(input[[paste0("hex_color_", vi)]])
          updateSelectInput(session, paste0("color_selector_", vi), selected = "custom")
          updateTextInput(session, paste0("color_", vi), value = hex)
          rc <- tryCatch(col2rgb(hex), error = function(e) NULL)
          if (!is.null(rc)) {
            updateSliderInput(session, paste0("red_",   vi), value = rc[1, 1])
            updateSliderInput(session, paste0("green_", vi), value = rc[2, 1])
            updateSliderInput(session, paste0("blue_",  vi), value = rc[3, 1])
          }
        })
      })
    }
  })
  
  # ── Dynamic UI: region / marker settings ────────────────────────────────────
  output$region_settings <- renderUI({
    req(input$region_count, input$enable_highlighting)
    do.call(tagList, lapply(seq_len(input$region_count), function(i) {
      div(style = "border:1px solid #ddd;padding:10px;margin-bottom:8px;border-radius:5px;",
          h5(paste("Region", i)),
          fluidRow(
            column(6, numericInput(paste0("region_x_min_", i), "X min:", 0)),
            column(6, numericInput(paste0("region_x_max_", i), "X max:", 60))
          ),
          fluidRow(
            column(6, numericInput(paste0("region_y_min_", i), "Y min:", 0.01)),
            column(6, numericInput(paste0("region_y_max_", i), "Y max:", 1))
          ),
          selectInput(paste0("region_color_", i), "Fill Color:",
                      choices  = c("Light Gray"   = "#ebebeb", "Light Blue"  = "#e6f3ff",
                                   "Light Red"    = "#ffebeb", "Light Green" = "#ebffeb",
                                   "Light Yellow" = "#ffffeb", "Custom"      = "custom"),
                      selected = "#ebebeb"),
          conditionalPanel(
            condition = paste0("input['region_color_", i, "'] == 'custom'"),
            textInput(paste0("region_color_custom_", i), "HEX:", "#ebebeb")
          ),
          sliderInput(paste0("region_alpha_", i), "Transparency:", 0, 1, 0.3, 0.05)
      )
    }))
  })
  
  output$time_marker_settings <- renderUI({
    req(input$marker_count, input$enable_time_markers)
    do.call(tagList, lapply(seq_len(input$marker_count), function(i) {
      div(style = "border:1px solid #ddd;padding:10px;margin-bottom:8px;border-radius:5px;",
          h5(paste("Marker", i)),
          numericInput(paste0("marker_time_", i), "Time Point:", 30),
          selectInput(paste0("marker_line_type_", i), "Line Type:",
                      choices  = c("Dashed" = "dashed", "Solid" = "solid",
                                   "Dotted" = "dotted", "DotDash" = "dotdash",
                                   "LongDash" = "longdash"),
                      selected = "dashed"),
          selectInput(paste0("marker_color_", i), "Color:",
                      choices  = c("Black" = "#000000", "Red"   = "#E41A1C",
                                   "Blue"  = "#377EB8", "Green" = "#4DAF4A",
                                   "Gray"  = "#999999", "Custom" = "custom"),
                      selected = "#000000"),
          conditionalPanel(
            condition = paste0("input['marker_color_", i, "'] == 'custom'"),
            textInput(paste0("marker_color_custom_", i), "HEX:", "#000000")
          ),
          sliderInput(paste0("marker_size_", i), "Width:", 0.1, 3, 1, 0.1),
          checkboxInput(paste0("marker_label_", i), "Add Label", FALSE),
          conditionalPanel(
            condition = paste0("input['marker_label_", i, "'] == true"),
            textInput(paste0("marker_text_", i),           "Label:",    paste0("t=", 30)),
            numericInput(paste0("marker_label_size_", i),  "Size:",     4, 2, 12),
            selectInput(paste0("marker_label_position_", i), "Position:",
                        choices = c("Top" = "top", "Middle" = "middle", "Bottom" = "bottom"),
                        selected = "top"),
            numericInput(paste0("marker_label_hjust_", i), "H-Offset:", 0, -5, 5, 0.1)
          )
      )
    }))
  })
  
  # ── Time filtering ───────────────────────────────────────────────────────────
  apply_time_filters <- function(d, time_col) {
    d[[time_col]] <- suppressWarnings(as.numeric(as.character(d[[time_col]])))
    if (!isTRUE(input$enable_time_filter)) return(d)
    rng <- input$time_filter_range
    if (!is.null(rng) && length(rng) == 2) {
      t_vec <- d[[time_col]]
      d <- d[!is.na(t_vec) & t_vec >= rng[1] & t_vec <= rng[2], , drop = FALSE]
    }
    excl <- parse_excluded_timepoints(input$exclude_timepoints, rv$all_timepoints)
    if (length(excl) > 0) {
      t_vec <- d[[time_col]]
      tol   <- max(abs(rv$all_timepoints), na.rm = TRUE) * 0.001 + 0.001
      # Vectorised: matrix of abs differences, any column within tol means exclude
      hit   <- rowSums(abs(outer(t_vec, excl, "-")) <= tol) == 0L
      d     <- d[hit, , drop = FALSE]
    }
    d
  }
  
  # ── Data prep ────────────────────────────────────────────────────────────────
  prepare_plot_data <- function(samples = NULL) {
    req(rv$data, rv$time_col, input$selected_samples)
    samps <- if (!is.null(samples)) samples else input$selected_samples
    if (rv$is_long_format) {
      req(rv$group_col, rv$value_col)
      d  <- apply_time_filters(rv$data, rv$time_col)
      # Pre-filter rows to only requested samples before any grouping
      d  <- d[d[[rv$group_col]] %in% samps, , drop = FALSE]
      pd <- d %>%
        group_by(across(all_of(c(rv$time_col, rv$group_col)))) %>%
        summarise(mean_value = mean(.data[[rv$value_col]], na.rm = TRUE),
                  sd_value   = sd(  .data[[rv$value_col]], na.rm = TRUE),
                  # count only the non-NA values that actually enter the mean —
                  # counting NA rows would silently understate SEM / CI
                  n          = sum(!is.na(.data[[rv$value_col]])), .groups = "drop") %>%
        # KC-1: SD/SEM/95% CI are UNDEFINED at n < 2, not zero. sd() of a
        # single value already returns NA; the old code overwrote that NA
        # with 0, which reads as a perfectly precise measurement in the
        # exported CSV and report tables. Keep NA and let the plot layer
        # decide whether/how to draw it (see the error-bar block below).
        mutate(sem_value  = ifelse(n >= 2, sd_value / sqrt(n), NA_real_),
               ci95_value = ifelse(n >= 2, qt(0.975, df = pmax(n - 1, 1)) * sem_value, NA_real_))
      names(pd)[names(pd) == rv$group_col] <- "variable"
      names(pd)[names(pd) == rv$time_col]  <- "time"
    } else {
      d    <- apply_time_filters(rv$data, rv$time_col)
      meas <- intersect(rv$od_vars_raw, samps)
      # v2.8 REPLICATE FIX: resolve sample columns BY POSITION via rv$col_map
      # so replicate columns sharing one name all contribute. Name-based
      # subsetting (d[, meas]) silently picked only the FIRST column per name.
      t_vec  <- d[[rv$time_col]]
      pieces <- lapply(meas, function(vn) {
        idxs <- rv$col_map[[vn]]
        if (is.null(idxs)) idxs <- which(colnames(d) == vn)
        vals <- unlist(lapply(idxs, function(i)
          suppressWarnings(as.numeric(as.character(d[[i]])))), use.names = FALSE)
        data.frame(time     = rep(t_vec, times = length(idxs)),
                   variable = vn, value = vals, stringsAsFactors = FALSE)
      })
      pd <- bind_rows(pieces) %>%
        group_by(time, variable) %>%
        summarise(mean_value = mean(value, na.rm = TRUE),
                  sd_value   = sd(  value, na.rm = TRUE),
                  n          = sum(!is.na(value)), .groups = "drop") %>%
        # KC-1: see the long-format branch above — NA (undefined), not 0.
        mutate(sem_value  = ifelse(n >= 2, sd_value / sqrt(n), NA_real_),
               ci95_value = ifelse(n >= 2, qt(0.975, df = pmax(n - 1, 1)) * sem_value, NA_real_))
    }
    # KC-7: optional blank/media OD subtraction. A constant offset applied
    # equally to every reading commutes with averaging, so subtracting it
    # here (once, from the already-aggregated mean) is exactly equivalent
    # to subtracting it from every raw value before averaging -- sd/n are
    # unaffected either way (SD is shift-invariant).
    blank <- input$blank_od %||% 0
    if (is.numeric(blank) && !is.na(blank) && blank != 0) pd$mean_value <- pd$mean_value - blank
    pd %>% filter(is.finite(time) & is.finite(mean_value))
  }

  # KC-4: the un-averaged counterpart of prepare_plot_data() -- one row per
  # (time, sample, replicate), never collapsed to a mean. Feeds
  # compute_replicate_metrics() so per-replicate metrics can be computed
  # with mean +/- SD reported across replicates, alongside (not instead of)
  # the mean-curve metrics prepare_plot_data()/compute_curve_metrics() give.
  prepare_replicate_data <- function(samples = NULL) {
    req(rv$data, rv$time_col, input$selected_samples)
    samps <- if (!is.null(samples)) samples else input$selected_samples
    if (rv$is_long_format) {
      req(rv$group_col, rv$value_col)
      d <- apply_time_filters(rv$data, rv$time_col)
      d <- d[d[[rv$group_col]] %in% samps, , drop = FALSE]
      d <- d[order(d[[rv$group_col]], d[[rv$time_col]]), , drop = FALSE]
      # Long files have no explicit replicate-ID column: the nth row within
      # each (sample, time) cell is treated as "replicate n". This assumes
      # same-replicate rows appear in a consistent relative order across
      # time points (true whenever each time point's rows were entered/
      # exported in the same replicate order, the common case).
      rep_idx <- ave(seq_len(nrow(d)),
                     d[[rv$group_col]], d[[rv$time_col]], FUN = seq_along)
      out <- data.frame(time      = d[[rv$time_col]],
                        variable  = d[[rv$group_col]],
                        replicate = rep_idx,
                        value     = suppressWarnings(as.numeric(as.character(d[[rv$value_col]]))),
                        stringsAsFactors = FALSE)
    } else {
      d    <- apply_time_filters(rv$data, rv$time_col)
      meas <- intersect(rv$od_vars_raw, samps)
      t_vec <- d[[rv$time_col]]
      pieces <- lapply(meas, function(vn) {
        idxs <- rv$col_map[[vn]]
        if (is.null(idxs)) idxs <- which(colnames(d) == vn)
        bind_rows(lapply(seq_along(idxs), function(r)
          data.frame(time      = t_vec, variable = vn, replicate = r,
                    value     = suppressWarnings(as.numeric(as.character(d[[idxs[r]]]))),
                    stringsAsFactors = FALSE)))
      })
      out <- bind_rows(pieces)
    }
    # KC-7: see prepare_plot_data() above -- same constant blank offset,
    # applied per replicate value here since this frame is un-averaged.
    blank <- input$blank_od %||% 0
    if (is.numeric(blank) && !is.na(blank) && blank != 0) out$value <- out$value - blank
    out %>% filter(is.finite(time) & is.finite(value))
  }

  # ── Aesthetics resolver ──────────────────────────────────────────────────────
  resolve_aesthetics <- function(samples) {
    n          <- length(samples)
    shapes     <- setNames(numeric(n),   samples)
    colors     <- setNames(character(n), samples)
    line_types <- setNames(character(n), samples)
    leg_labels <- setNames(character(n), samples)
    filled_map <- setNames(logical(n),   samples)
    def_colors <- c("#000000","#E41A1C","#377EB8","#4DAF4A",
                    "#984EA3","#FF7F00","#A65628","#F781BF",
                    "#999999","#66C2A5","#FC8D62","#8DA0CB")
    def_shapes <- c(16, 15, 17, 18, 25, 4, 8, 23, 3, 10)
    
    for (i in seq_len(n)) {
      vn  <- samples[i]; vid <- safe_id(vn)
      si  <- input[[paste0("shape_", vid)]]
      shapes[i] <- if (!is.null(si)) as.numeric(si)
      else def_shapes[(i - 1) %% length(def_shapes) + 1]
      
      sel <- input[[paste0("color_selector_", vid)]]
      if (!is.null(sel) && sel == "custom")
        colors[i] <- normalize_hex_color(input[[paste0("color_", vid)]])
      else if (!is.null(sel))
        colors[i] <- sel
      else if (!is.null(rv$palette_colors)) {
        idx       <- match(vn, rv$od_vars)
        colors[i] <- if (!is.na(idx)) rv$palette_colors[idx]
        else def_colors[(i - 1) %% length(def_colors) + 1]
      } else
        colors[i] <- def_colors[(i - 1) %% length(def_colors) + 1]
      
      lt <- input[[paste0("line_type_", vid)]]
      line_types[i] <- if (!is.null(lt)) lt else "solid"
      
      ll <- input[[paste0("legend_label_", vid)]]
      leg_labels[i] <- if (!is.null(ll) && nchar(ll) > 0) ll else vn
      
      fi <- input[[paste0("shape_filled_", vid)]]
      filled_map[i] <- if (!is.null(fi)) fi else TRUE
    }
    list(shapes = shapes, colors = colors, line_types = line_types,
         leg_labels = leg_labels, filled_map = filled_map)
  }
  
  # ── Shared figure theme (v2.11) ─────────────────────────────────────────────
  # Everything that makes the main lysis curve look the way it does, as a list
  # of theme layers, so the analysis figures and the dashboard panels use the
  # same fonts, line weights, legend, spacing and journal preset. Adding the
  # list to a ggplot applies the layers in order; the main figure's output is
  # byte-identical to v2.10 (test suite).
  figure_theme_layers <- function() {
    tf      <- if (isTRUE(input$bold_title))        "bold"   else "plain"
    af      <- if (isTRUE(input$italic_axis_labels)) "italic" else "plain"
    # v2.10: theme spacing (margins, gaps) follows the theme's base_size, which
    # defaults to 14 (prism) / 12 (pubr) and looks oversized next to 6-7 pt text.
    # 0 keeps each theme's own default, so existing figures do not change.
    bs      <- input$theme_base_pt %||% 0
    base_t  <- if (isTRUE(input$use_advanced_ticks)) {
      if (is.numeric(bs) && is.finite(bs) && bs > 0) theme_prism(border = TRUE, base_size = bs)
      else theme_prism(border = TRUE)
    } else {
      if (is.numeric(bs) && is.finite(bs) && bs > 0) theme_pubr(base_size = bs) else theme_pubr()
    }
    maj_col <- if (!is.null(input$major_gridline_color) && input$major_gridline_color == "custom")
      input$major_gridline_color_custom else input$major_gridline_color
    min_col <- if (!is.null(input$minor_gridline_color) && input$minor_gridline_color == "custom")
      input$minor_gridline_color_custom else input$minor_gridline_color
    x_ang   <- as.numeric(input$axis_text_angle %||% "0")
    
    # v2.10 figure-layout inputs. Defaults equal the previous hard-coded values
    # (1.07 pt == linewidth 0.5; 1.5 mm == 0.15 cm; legend right, grey border).
    axis_lw  <- pt2lw(input$axis_line_pt %||% 1.07)
    tick_cm  <- (input$tick_length_mm %||% 1.5) / 10
    leg_pos  <- input$legend_position %||% "right"
    leg_bord <- if (isTRUE(input$legend_box_border %||% TRUE)) "gray80" else NA
    leg_key  <- input$legend_key_mm %||% 0
    
    layers <- list(base_t, theme(
      text             = element_text(family = input$font_family),
      plot.title       = element_text(size = input$title_font_size,       face = tf, hjust = 0.5),
      plot.caption     = element_text(size = max(8, input$axis_text_font_size * 0.7),
                                      hjust = 1, color = "grey30"),
      plot.tag         = element_text(size = input$panel_label_size %||% 8, face = "bold"),
      axis.title       = element_text(size = input$axis_label_font_size,  face = af),
      axis.text        = element_text(size = input$axis_text_font_size),
      axis.text.x      = element_text(angle = x_ang,
                                      hjust = if (x_ang > 0) 1 else 0.5,
                                      vjust = if (x_ang > 0) 1 else 0.5),
      panel.grid.major = if (isTRUE(input$show_major_gridlines))
        element_line(color = maj_col, linewidth = input$major_gridline_size) else element_blank(),
      panel.grid.minor = if (isTRUE(input$show_minor_gridlines))
        element_line(color = min_col, linewidth = input$minor_gridline_size) else element_blank(),
      axis.ticks        = element_line(color = "black", linewidth = axis_lw),
      axis.ticks.length = unit(tick_cm, "cm"),
      legend.text       = element_text(size = input$legend_font_size %||% 14),
      legend.title      = element_text(size = input$legend_font_size %||% 14),
      legend.background = element_rect(fill = "white", color = leg_bord),
      legend.key        = element_rect(fill = NA),
      panel.border      = element_rect(color = "black", fill = NA, linewidth = axis_lw)
    ))
    # Legend placement: outside positions as before; "inside-*" anchors the key
    # just inside a panel corner (journal panels rarely spend column width on
    # an outside key).
    if (startsWith(leg_pos, "inside-")) {
      just <- switch(sub("^inside-", "", leg_pos),
                     "top-left" = c(0, 1), "top-right" = c(1, 1),
                     "bottom-left" = c(0, 0), "bottom-right" = c(1, 0), c(1, 1))
      pos  <- ifelse(just == 1, 0.98, 0.02)
      layers <- c(layers, list(theme(legend.position = "inside",
                                     legend.position.inside = pos, legend.justification.inside = just,
                                     legend.margin = margin(2, 4, 2, 4))))
    } else {
      layers <- c(layers, list(theme(legend.position = leg_pos)))
    }
    if (!isTRUE(input$use_advanced_ticks))          # theme_pubr draws axis lines
      layers <- c(layers, list(theme(axis.line = element_line(linewidth = axis_lw))))
    if (is.numeric(leg_key) && is.finite(leg_key) && leg_key > 0)
      layers <- c(layers, list(theme(legend.key.size = unit(leg_key, "mm"))))
    layers
  }
  
  # v2.10: tick-label style for log axes. "legacy" is ggplot2's default (0.10).
  y_tick_labels <- function() {
    switch(input$y_tick_label_style %||% "legacy",
           trim  = scales::label_number(drop0trailing = TRUE, big.mark = ""),
           log10 = scales::label_log(),
           waiver())
  }
  
  # ── Core plot builder ────────────────────────────────────────────────────────
  # highlight_samples: if non-NULL, all other samples are greyed out (not removed)
  build_plot <- function(plot_data, samples, aes_vals, highlight_samples = NULL) {
    shapes     <- aes_vals$shapes;     colors     <- aes_vals$colors
    line_types <- aes_vals$line_types; leg_labels <- aes_vals$leg_labels
    filled_map <- aes_vals$filled_map; n_vars     <- length(samples)
    
    # Grey out non-highlighted samples while keeping axis scale stable
    display_colors <- colors
    if (!is.null(highlight_samples))
      for (i in seq_len(n_vars))
        if (!samples[i] %in% highlight_samples) display_colors[i] <- "#DDDDDD"
    
    p <- ggplot(plot_data,
                aes(x = time, y = mean_value, group = variable,
                    color = variable, shape = variable, linetype = variable))
    
    # Region highlighting
    if (isTRUE(input$enable_highlighting) && !is.null(input$region_count)) {
      for (i in seq_len(input$region_count)) {
        x1 <- input[[paste0("region_x_min_", i)]]; x2 <- input[[paste0("region_x_max_", i)]]
        y1 <- input[[paste0("region_y_min_", i)]]; y2 <- input[[paste0("region_y_max_", i)]]
        rc  <- input[[paste0("region_color_", i)]]
        col <- if (!is.null(rc) && rc == "custom") input[[paste0("region_color_custom_", i)]] else rc
        alp <- input[[paste0("region_alpha_", i)]]
        if (!is.null(x1) && !is.null(col) && !is.null(alp))
          p <- p + annotate("rect", xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=col, alpha=alp)
      }
    }
    
    # Time markers
    if (isTRUE(input$enable_time_markers) && !is.null(input$marker_count)) {
      yr <- range(plot_data$mean_value, na.rm = TRUE)
      if (input$y_scale_type == "log") yr[1] <- max(1e-9, yr[1])
      for (i in seq_len(input$marker_count)) {
        tp <- input[[paste0("marker_time_", i)]]
        lt <- input[[paste0("marker_line_type_", i)]]
        mc <- input[[paste0("marker_color_", i)]]
        lc <- if (!is.null(mc) && mc == "custom") input[[paste0("marker_color_custom_", i)]] else mc
        ls <- input[[paste0("marker_size_", i)]]
        if (!is.null(tp) && !is.null(lc))
          p <- p + geom_vline(xintercept = tp, linetype = lt, color = lc, linewidth = ls)
        if (isTRUE(input[[paste0("marker_label_", i)]])) {
          lbl  <- input[[paste0("marker_text_",         i)]]
          lsz  <- input[[paste0("marker_label_size_",   i)]]
          lpos <- input[[paste0("marker_label_position_",i)]]
          lhj  <- input[[paste0("marker_label_hjust_",  i)]]; if (is.null(lhj)) lhj <- 0
          yp <- switch(lpos,
                       top    = if (input$y_scale_type == "log") 10^(log10(yr[2]) - diff(log10(yr)) * 0.08) else yr[1] + diff(yr) * 0.92,
                       bottom = if (input$y_scale_type == "log") 10^(log10(yr[1]) + diff(log10(yr)) * 0.08) else yr[1] + diff(yr) * 0.08,
                       middle = if (input$y_scale_type == "log") 10^(mean(log10(yr))) else mean(yr))
          p <- p + annotate("text", x = tp + lhj, y = yp, label = lbl, size = lsz, color = lc)
        }
      }
    }
    
    # Error bars / shadow
    # Track whether shadow/ribbon mode is used so we can unify the fill scale
    # with the points layer later (ggplot2 only allows ONE scale per aesthetic).
    using_shadow <- FALSE
    
    if (input$error_type != "none") {
      ec     <- switch(input$error_type, sd="sd_value", sem="sem_value", ci95="ci95_value")
      em     <- input$error_multiplier
      # KC-1: an NA error value means "undefined" (n < 2 replicates at that
      # point), not zero. Leave it NA — pmax()/arithmetic below propagate it
      # to err_lo/err_hi automatically — and draw the error-bar/ribbon layer
      # from a subset that excludes those rows, rather than drawing a false
      # zero-height bar for a single replicate.
      err_vals <- plot_data[[ec]]

      lo_val <- if (input$y_scale_type == "log")
        pmax(plot_data$mean_value - err_vals * em, 1e-9)
      else if (isTRUE(input$asymmetric_error))
        pmax(plot_data$mean_value - err_vals * em, 0)
      else
        plot_data$mean_value - err_vals * em
      plot_data$err_lo <- lo_val
      plot_data$err_hi <- plot_data$mean_value + err_vals * em
      eb_data <- plot_data[!is.na(plot_data$err_lo) & !is.na(plot_data$err_hi), , drop = FALSE]

      dm <- if (!is.null(input$error_display_mode)) input$error_display_mode else "bars"
      if (dm == "shadow") {
        using_shadow <- TRUE
        p <- p +
          geom_ribbon(data = eb_data,
                      aes(x = time, ymin = err_lo, ymax = err_hi,
                          group = variable, fill = variable),
                      alpha = input$shadow_alpha, color = NA, inherit.aes = FALSE)
        # NOTE: scale_fill_manual is added AFTER the points section to unify
        # the fill scale for both ribbon and points (if points are enabled).
      } else {
        pos <- if (!is.null(input$error_bar_position) && input$error_bar_position == "dodge")
          position_dodge(0.2) else position_identity()
        blt <- if (!is.null(input$error_bar_style) && input$error_bar_style == "dashed") "dashed" else "solid"
        if (!is.null(input$error_bar_style) && input$error_bar_style == "T") {
          p <- p + geom_errorbar(data = eb_data,
                                 aes(x = time, ymin = err_lo, ymax = err_hi, color = variable),
                                 width = input$error_bar_width, linewidth = input$error_bar_thickness,
                                 linetype = blt, position = pos, inherit.aes = FALSE)
        } else {
          p <- p + geom_linerange(data = eb_data,
                                  aes(x = time, ymin = err_lo, ymax = err_hi, color = variable),
                                  linewidth = input$error_bar_thickness,
                                  linetype = blt, position = pos, inherit.aes = FALSE)
        }
      }
    }
    
    p <- p + geom_line(aes(group = variable), linewidth = input$line_thickness)
    
    if (input$show_points) {
      # Build lookup tables of shape and fill per variable.
      shape_map <- integer(n_vars)
      fill_map  <- character(n_vars)
      for (i in seq_len(n_vars)) {
        pt_s <- as.integer(shapes[i])
        if (!filled_map[i]) {
          pt_s         <- switch(as.character(pt_s),
                                 "16" = 21L, "15" = 22L, "17" = 24L, "18" = 23L, pt_s)
          fill_map[i]  <- "white"
        } else {
          fill_map[i]  <- display_colors[i]
        }
        shape_map[i] <- pt_s
      }
      names(shape_map) <- samples
      names(fill_map)  <- samples
      
      if (using_shadow) {
        # When shadow/ribbon is active, both ribbon and points share the 'fill'
        # aesthetic mapped to 'variable'.  We use a single scale_fill_manual that
        # maps each variable to its display color.  For open (unfilled) shapes the
        # fill is overridden to white via `override.aes` — open shapes use stroke-
        # based variants (21/22/24/23) whose interior respects the fill scale, so
        # we simply accept the fill color from the scale for filled shapes.
        # 
        # Trade-off: open shapes will show the ribbon color fill instead of white
        # inside the point.  This is acceptable because the alternative (two
        # conflicting fill scales) crashes ggplot2 entirely.  Users who need white-
        # filled open shapes can switch error display to "bars" mode.
        p <- p + geom_point(
          data        = plot_data,
          aes(x = time, y = mean_value, color = variable, shape = variable,
              fill = variable),
          size        = input$shape_size,
          stroke      = input$point_stroke,
          inherit.aes = FALSE
        ) +
          scale_shape_manual(values = shape_map, labels = leg_labels,
                             name = NULL, guide = "none") +
          scale_fill_manual(values = setNames(display_colors, samples),
                            labels = leg_labels, guide = "none")
      } else {
        # No shadow: use scale_fill_identity for per-point fill control
        pt_data          <- plot_data
        pt_data$pt_fill  <- fill_map[pt_data$variable]
        
        p <- p + geom_point(
          data        = pt_data,
          aes(x = time, y = mean_value, color = variable, shape = variable,
              fill = pt_fill),
          size        = input$shape_size,
          stroke      = input$point_stroke,
          inherit.aes = FALSE
        ) +
          scale_shape_manual(values = shape_map, labels = leg_labels,
                             name = NULL, guide = "none") +
          scale_fill_identity()
      }
    } else if (using_shadow) {
      # Points disabled but shadow is active: still need the fill scale for ribbon
      p <- p + scale_fill_manual(values = setNames(display_colors, samples),
                                  labels = leg_labels, guide = "none")
    }
    
    if (input$show_end_labels) {
      mt  <- max(plot_data$time, na.rm = TRUE)
      off <- mt * (input$label_offset / 100)
      ep  <- plot_data %>% group_by(variable) %>% filter(time == max(time)) %>% ungroup()
      p   <- p + geom_text_repel(data = ep,
                                 aes(label = variable, color = variable, x = Inf, y = mean_value),
                                 direction = "y", xlim = c(mt + off, Inf),
                                 min.segment.length = Inf, hjust = 0,
                                 size = input$label_font_size / 2.835,
                                 fontface = if (input$label_bold) "bold" else "plain")
    }
    
    p <- p +
      scale_color_manual(   values = setNames(display_colors, samples), labels = leg_labels, name = NULL) +
      scale_linetype_manual(values = line_types,                         labels = leg_labels, name = NULL)
    
    # When points are disabled the shape scale wasn't added inside the points block;
    # add it now for a consistent legend.
    if (!isTRUE(input$show_points))
      p <- p + scale_shape_manual(values = shapes, labels = leg_labels, name = NULL)
    
    p <- p +
      guides(shape = guide_legend(override.aes = list(alpha = 1)),
             color = guide_legend(override.aes = list(alpha = 1)))
    
    x_exp  <- expansion(mult = c(input$x_expand_left,   input$x_expand_right))
    y_exp  <- expansion(mult = c(input$y_expand_bottom, input$y_expand_top))
    x_lims <- if (isTRUE(input$custom_x_limits)) c(input$x_min, input$x_max) else NULL
    y_lims <- if (isTRUE(input$custom_y_limits)) c(input$y_min, input$y_max) else NULL
    
    if (input$x_scale_type == "log") {
      p <- p + scale_x_log10(limits = x_lims, expand = x_exp)
    } else if (input$x_scale_type == "sqrt") {
      p <- p + scale_x_sqrt(limits = x_lims, expand = x_exp)
    } else if (input$x_scale_type == "reverse") {
      p <- p + scale_x_reverse(limits = x_lims, expand = x_exp)
    } else if (isTRUE(input$use_advanced_ticks)) {
      # Determine the upper bound for tick generation
      mt2 <- if (isTRUE(input$custom_x_limits)) input$x_max
      else max(plot_data$time, na.rm = TRUE)
      x0  <- if (isTRUE(input$custom_x_limits)) input$x_min else 0
      
      # Parse any user-specified extra tick positions
      extra_ticks <- tryCatch({
        raw <- trimws(input$x_extra_ticks)
        if (nchar(raw) == 0) numeric(0)
        else {
          vals <- suppressWarnings(as.numeric(strsplit(raw, "[,;[:space:]]+")[[1]]))
          vals[is.finite(vals)]
        }
      }, error = function(e) numeric(0))
      
      # Determine major interval: manual if provided, otherwise auto
      manual_interval <- input$x_tick_interval
      use_manual <- !is.null(manual_interval) &&
        !is.na(manual_interval) &&
        is.numeric(manual_interval) &&
        manual_interval > 0
      
      if (use_manual) {
        iv  <- manual_interval
        mjb <- seq(x0, ceiling((mt2 - x0) / iv) * iv + x0, by = iv)
        # Always include the actual data endpoint if it isn't already a break
        if (!isTRUE(input$custom_x_limits) && !mt2 %in% mjb)
          mjb <- sort(unique(c(mjb, mt2)))
        mnb <- seq(x0, max(mjb), by = iv / 2)
      } else {
        # Auto interval (original logic)
        if (mt2 <= 60)       { iv <- 10;  mjb <- seq(0, ceiling(mt2/10)*10,  10);  mnb <- seq(0, ceiling(mt2/10)*10,  5) }
        else if (mt2 <= 120) { iv <- 30;  mjb <- seq(0, ceiling(mt2/30)*30,  30);  mnb <- seq(0, ceiling(mt2/30)*30, 10) }
        else                 { iv <- 60;  mjb <- seq(0, ceiling(mt2/60)*60,  60);  mnb <- seq(0, ceiling(mt2/60)*60, 30) }
        # Always include the actual data endpoint even in auto mode
        if (!isTRUE(input$custom_x_limits) && !mt2 %in% mjb)
          mjb <- sort(unique(c(mjb, mt2)))
      }
      
      # Union in any extra positions the user explicitly requested
      if (length(extra_ticks) > 0)
        mjb <- sort(unique(c(mjb, extra_ticks)))
      
      p <- p + scale_x_continuous(limits = x_lims, expand = x_exp, breaks = mjb,
                                  minor_breaks = mnb, guide = guide_prism_minor())
    } else {
      p <- p + scale_x_continuous(limits = x_lims, expand = x_exp)
    }
    
    if (input$y_scale_type == "log") {
      y_labs <- y_tick_labels()
      if (isTRUE(input$use_advanced_ticks)) {
        minE <- input$y_log_min_exponent; maxE <- input$y_log_max_exponent
        if (is.null(y_lims)) y_lims <- c(10^minE, if (maxE >= 0) 10^(maxE+1) else 10^maxE)
        ymn <- c(rep(1:9, maxE-minE+1) * 10^rep(minE:maxE, each=9))
        if (maxE >= 0) ymn <- c(ymn, 10^(maxE+1))
        p <- p + scale_y_log10(limits = y_lims, expand = y_exp, minor_breaks = ymn,
                               labels = y_labs, guide = guide_prism_minor())
      } else {
        p <- p + scale_y_log10(limits = y_lims, expand = y_exp, labels = y_labs)
      }
    } else if (input$y_scale_type == "sqrt") {
      p <- p + scale_y_sqrt(limits = y_lims, expand = y_exp)
    } else if (input$y_scale_type == "reverse") {
      p <- p + scale_y_reverse(limits = y_lims, expand = y_exp)
    } else {
      p <- p + scale_y_continuous(limits = y_lims, expand = y_exp)
    }
    
    bs     <- input$theme_base_pt %||% 0
    pl_lab <- trimws(input$panel_label %||% "")
    p <- p + figure_theme_layers()
    
    if (isTRUE(input$custom_aspect_ratio))
      p <- p + theme(aspect.ratio = 1 / input$aspect_ratio)

    # Optional publication caption describing the error measure and replicate count.
    # Purely additive: NULL (unchanged output) unless the user opts in AND error bars
    # are being drawn. Reads n from plot_data, which is populated in prepare_plot_data().
    err_caption <- NULL
    if (isTRUE(input$show_error_caption) && input$error_type != "none" &&
        !is.null(plot_data$n) && length(plot_data$n) > 0) {
      err_lab <- switch(input$error_type,
                        sd   = "SD",
                        sem  = "SEM",
                        ci95 = "95% CI",
                        input$error_type)
      n_rng <- range(plot_data$n, na.rm = TRUE)
      n_txt <- if (is.finite(n_rng[1]) && n_rng[1] == n_rng[2])
        paste0("n = ", n_rng[1])
      else
        paste0("n = ", n_rng[1], "–", n_rng[2])
      mult_txt <- if (!is.null(input$error_multiplier) &&
                      is.numeric(input$error_multiplier) &&
                      input$error_multiplier != 1)
        paste0(input$error_multiplier, "× ") else ""
      err_caption <- paste0("Error bars: ", mult_txt, "mean ± ", err_lab,
                            " (", n_txt, ").")
    }

    p + coord_cartesian(clip = "off") +
      labs(x        = input$x_axis_label,
           y        = input$y_axis_label,
           # v2.10: with journal spacing (theme_base_pt > 0) a blank title is NULL,
           # so no empty title row (~5 mm) is reserved above the panel. Legacy
           # spacing keeps "" so pre-2.10 figures reproduce exactly.
           title    = if (nchar(trimws(input$plot_title %||% "")) > 0) input$plot_title
                      else if (is.numeric(bs) && is.finite(bs) && bs > 0) NULL else "",
           subtitle = if (nchar(trimws(input$plot_subtitle)) > 0) input$plot_subtitle else NULL,
           caption  = err_caption,
           tag      = if (nzchar(pl_lab)) pl_lab else NULL)
  }
  
  generate_plot <- function() {
    req(rv$data, input$selected_samples)
    tryCatch(
      build_plot(prepare_plot_data(), input$selected_samples,
                 resolve_aesthetics(input$selected_samples)),
      error = function(e) {
        # Return a blank ggplot with the error message rather than crashing the session
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = paste("Plot error:", conditionMessage(e)),
                   color = "red", size = 4, hjust = 0.5) +
          theme_void()
      }
    )
  }
  
  # Debounced reactive: coalesces rapid input changes (slider drags, typing) into
  # a single plot rebuild after 400 ms of inactivity — prevents render pile-up.
  plot_inputs_debounced <- debounce(reactive({
    list(
      selected_samples    = input$selected_samples,
      error_type          = input$error_type,
      error_display_mode  = input$error_display_mode,
      error_multiplier    = input$error_multiplier,
      line_thickness      = input$line_thickness,
      show_points         = input$show_points,
      shape_size          = input$shape_size,
      x_scale_type        = input$x_scale_type,
      y_scale_type        = input$y_scale_type,
      custom_x_limits     = input$custom_x_limits,
      custom_y_limits     = input$custom_y_limits,
      x_min = input$x_min, x_max = input$x_max,
      y_min = input$y_min, y_max = input$y_max,
      enable_time_filter  = input$enable_time_filter,
      time_filter_range   = input$time_filter_range,
      exclude_timepoints  = input$exclude_timepoints
    )
  }), millis = 400)
  
  # ── Export size (v2.10): one source of truth, in inches ─────────────────────
  # Every export path (image, reports, PPTX, true-size preview) reads this, so a
  # journal preset given in mm cannot drift from what the reports embed.
  export_size_in <- function() {
    w <- input$export_width  %||% 10
    h <- input$export_height %||% 8
    if (identical(input$export_units, "mm")) c(w, h) / 25.4 else c(w, h)
  }
  
  # Switching units converts the current numbers so the physical size is kept
  # (10 in -> 254 mm). A preset or a settings file sets units + numbers
  # together and pre-marks the unit it sends (rv$units_seen), so those values
  # are not converted a second time.
  rv$units_seen <- "in"
  observeEvent(input$export_units, {
    u <- input$export_units
    if (!identical(u, rv$units_seen)) {
      w <- isolate(input$export_width); h <- isolate(input$export_height)
      f  <- if (identical(u, "mm")) 25.4 else 1 / 25.4
      dg <- if (identical(u, "mm")) 1 else 3     # 89 mm -> 3.504 in -> 89.0 mm, no drift
      if (is.numeric(w) && is.finite(w)) updateNumericInput(session, "export_width",  value = round(w * f, dg))
      if (is.numeric(h) && is.finite(h)) updateNumericInput(session, "export_height", value = round(h * f, dg))
      rv$units_seen <- u
    }
    updateNumericInput(session, "export_width",  label = paste0("Width (",  u, "):"))
    updateNumericInput(session, "export_height", label = paste0("Height (", u, "):"))
  }, ignoreInit = TRUE)
  
  output$export_size_note <- renderUI({
    sz <- export_size_in()
    if (!all(is.finite(sz))) return(NULL)
    mm <- round(sz * 25.4, 1)
    p(style = "font-size:.8em;color:#777;margin-top:-6px;",
      sprintf("= %s x %s mm (%s x %s in). PDF/SVG are vector; DPI applies to PNG/TIFF/JPEG.",
              mm[1], mm[2], round(sz[1], 2), round(sz[2], 2)))
  })
  
  # Preview helpers (v2.11): the main figure and the analysis figure share the
  # pixel-canvas / true-export-size preview mechanics.
  preview_container <- function(plot_id, img_id) {
    if (identical(input$preview_mode, "wysiwyg")) {
      sz <- export_size_in(); z <- input$preview_zoom %||% 2
      imageOutput(img_id,
                  width  = paste0(round(sz[1] * 96 * z), "px"),
                  height = paste0(round(sz[2] * 96 * z), "px"))
    } else {
      plotOutput(plot_id,
                 height = paste0(input$plot_height, "px"),
                 width  = paste0(input$plot_width,  "px"))
    }
  }
  # True-size preview (v2.10): the figure goes through the same ggsave path as
  # the download, at the export's physical size, and is shown at `zoom` x.
  # Text and line weights therefore appear at their real size relative to the
  # figure -- what Inkscape will open.
  register_preview <- function(plot_id, img_id, plot_fun) {
    output[[plot_id]] <- renderPlot({
      plot_inputs_debounced()   # take debounced dependency
      plot_fun()
    }, res = 96)
    output[[img_id]] <- renderImage({
      plot_inputs_debounced()
      sz <- export_size_in(); z <- input$preview_zoom %||% 2
      pr <- session$clientData$pixelratio %||% 1
      f  <- tempfile(fileext = ".png")
      ggsave(f, plot = plot_fun(), device = export_device("png"),
             width = sz[1], height = sz[2], units = "in", dpi = 96 * z * pr, bg = "white")
      list(src = f, contentType = "image/png",
           width = round(sz[1] * 96 * z), height = round(sz[2] * 96 * z),
           alt = "Figure preview at export size")
    }, deleteFile = TRUE)
  }
  output$plot_container <- renderUI(preview_container("od_plot", "od_plot_img"))
  register_preview("od_plot", "od_plot_img", generate_plot)
  
  output$downloadPlot <- downloadHandler(
    filename = function()
      paste0("od_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$export_format),
    content  = function(file) {
      sz   <- export_size_in()
      fmt  <- input$export_format
      args <- list(filename = file, plot = generate_plot(), device = export_device(fmt),
                   width = sz[1], height = sz[2],
                   units = "in", dpi = input$export_dpi, bg = "white")
      # Journals require lossless compression for TIFF; the arg is only valid
      # for the tiff device so it must be added conditionally.
      if (identical(fmt, "tiff")) args$compression <- "lzw"
      do.call(ggsave, args)
    }
  )
  
  # ── Journal preset (v2.10) ───────────────────────────────────────────────────
  journal_target_width <- reactive({
    journal_width_mm(input$journal_family, input$journal_columns,
                     input$journal_custom_width_mm %||% 89)
  })
  
  output$journal_preset_summary <- renderUI({
    w  <- journal_target_width(); h <- input$journal_height_mm %||% 65
    if (!is.numeric(w) || !is.finite(w))
      return(p(style = "font-size:.85em;color:#856404;margin:6px 0;",
               "This journal publishes no 1.5-column width; choose single or double."))
    if (!is.numeric(h) || !is.finite(h)) return(NULL)
    jp   <- JOURNAL_PRESETS[[input$journal_family %||% "nature"]]
    warn <- if (!is.null(jp$max_h) && is.finite(jp$max_h) && h > jp$max_h)
      sprintf(" Height exceeds this journal's maximum (%s mm).", jp$max_h) else ""
    p(style = "font-size:.85em;margin:6px 0;",
      strong(sprintf("Figure: %s x %s mm", round(w, 1), h)),
      sprintf(" (%s x %s in).", round(w / 25.4, 2), round(h / 25.4, 2)),
      if (nzchar(warn)) span(style = "color:#856404;", warn))
  })
  
  observeEvent(input$apply_journal_preset, {
    w <- journal_target_width(); h <- input$journal_height_mm %||% 65
    if (!is.numeric(w) || !is.finite(w) || !is.numeric(h) || !is.finite(h)) return()
    x_range <- tryCatch({
      pd <- prepare_plot_data(); diff(range(pd$time, na.rm = TRUE))
    }, error = function(e) NULL)
    vals <- journal_preset_settings(w, h, input$journal_panel_label %||% "", x_range)
    rv$units_seen <- "mm"   # see observeEvent(input$export_units)
    for (nm in names(vals)) {
      val <- vals[[nm]]
      tryCatch({
        if (is.logical(val))      updateCheckboxInput(session, nm, value = val)
        else if (is.numeric(val)) updateNumericInput( session, nm, value = val)
        else { updateSelectInput(session,   nm, selected = val)
               updateRadioButtons(session, nm, selected = val)
               updateTextInput(session,    nm, value    = val) }
      }, error = function(e) invisible(NULL))
    }
    updateNumericInput(session, "export_width",  label = "Width (mm):")
    updateNumericInput(session, "export_height", label = "Height (mm):")
    showNotification(sprintf(paste0("Journal preset applied: %s x %s mm, Arial 6-8 pt, ",
                                    "SVG export. Preview switched to true export size."),
                             round(w, 1), h),
                     type = "message", duration = 8)
  })
  
  # ── Figure legend text (v2.10) ──────────────────────────────────────────────
  # Journals put the measure, n and the error definition in the legend, not on
  # the artwork. Drafted from the plot state + Experiment Notebook; edit freely.
  output$figure_legend_text <- renderText({
    if (is.null(rv$data) || is.null(input$selected_samples))
      return("Load data and select samples to draft a legend.")
    pd <- tryCatch(prepare_plot_data(), error = function(e) NULL)
    if (is.null(pd) || nrow(pd) == 0) return("Load data and select samples to draft a legend.")
    aes_v <- resolve_aesthetics(input$selected_samples)
    nb    <- function(id) { v <- input[[id]]
                            if (is.null(v) || !nzchar(trimws(as.character(v)))) NULL else trimws(as.character(v)) }
    pl    <- nb("panel_label"); host <- nb("nb_host_strain"); phage <- nb("nb_phage_plasmid")
    moi   <- nb("nb_moi");      med  <- nb("nb_media");       temp  <- nb("nb_temperature")
    ylab  <- nb("y_axis_label") %||% "optical density"
    s1 <- paste0(if (!is.null(pl)) paste0("(", pl, ") ") else "",
                 "Growth and lysis of ", host %||% "the host strain",
                 if (!is.null(phage)) paste0(" infected with ", phage) else "",
                 if (!is.null(moi))   paste0(" (MOI ", moi, ")") else "",
                 ", measured as ", ylab, " over time",
                 if (!is.null(med))   paste0(" in ", med) else "",
                 if (!is.null(temp))  paste0(" at ", temp, " °C") else "", ".")
    s2 <- paste0("Curves: ", paste(unname(aes_v$leg_labels[input$selected_samples]), collapse = "; "), ".")
    n_rng <- suppressWarnings(range(pd$n, na.rm = TRUE))
    n_txt <- if (all(is.finite(n_rng)) && n_rng[1] == n_rng[2]) paste0("n = ", n_rng[1])
             else if (all(is.finite(n_rng))) paste0("n = ", n_rng[1], "–", n_rng[2])
             else "n not available"
    err  <- switch(input$error_type %||% "none",
                   sd = "s.d.", sem = "s.e.m.", ci95 = "the 95% confidence interval", NULL)
    mult <- input$error_multiplier %||% 1
    s3 <- paste0(if (isTRUE(input$show_points)) "Symbols and lines" else "Lines",
                 " show the mean of ", n_txt, " replicates",
                 if (!is.null(nb("nb_replicate"))) paste0(" (", nb("nb_replicate"), ")") else "",
                 if (!is.null(err)) paste0(
                   "; ", if (identical(input$error_display_mode, "shadow")) "shaded bands" else "error bars",
                   " indicate ", if (is.numeric(mult) && mult != 1) paste0(mult, " × ") else "", err,
                   if (any(pd$n < 2, na.rm = TRUE)) " (omitted where n = 1)" else "") else "")
    s3 <- paste0(sub("\\.$", "", s3), ".")   # "s.e.m." already ends with a period
    s4 <- if (identical(input$y_scale_type, "log")) "The y axis is log10-scaled." else NULL
    blank <- input$blank_od %||% 0
    s5 <- if (is.numeric(blank) && is.finite(blank) && blank != 0)
      sprintf("Readings are blank-corrected (−%s).", blank) else NULL
    s6 <- if (isTRUE(input$enable_time_filter)) "Only the displayed time window was analysed." else NULL
    paste(c(s1, s2, s3, s4, s5, s6), collapse = " ")
  })

  # ── Analysis figures (v2.11) ─────────────────────────────────────────────────
  replicate_metric_rows_current <- function() {
    compute_replicate_metric_rows(prepare_replicate_data(),
                                  smooth_window      = input$metrics_smooth_window %||% 5,
                                  peak_smooth_window = input$metrics_peak_smooth_window %||% 1,
                                  ref_sample         = input$metrics_ref_sample)
  }
  analysis_stat_metric <- function() {
    if (identical(input$analysis_type, "dose_response")) input$analysis_dose_metric %||% "lysis_time"
    else (input$analysis_metrics %||% "lysis_time")[1]
  }
  analysis_stats_current <- function() {
    compare_replicate_metric(replicate_metric_rows_current(), analysis_stat_metric(),
                             ref_sample = input$metrics_ref_sample,
                             method     = input$stats_method %||% "welch")
  }
  
  # One builder for every analysis figure. Each returns a ggplot that then gets
  # figure_theme_layers(), so it matches the main figure exactly.
  build_analysis_plot <- function(type) {
    samps  <- input$selected_samples
    aes_v  <- resolve_aesthetics(samps)
    cols   <- setNames(aes_v$colors, samps); labs_v <- aes_v$leg_labels
    lts    <- aes_v$line_types;              shp    <- aes_v$shapes
    tunit  <- time_unit_from_label(input$x_axis_label)
    lw     <- input$line_thickness %||% 1
    ptsz   <- input$shape_size %||% 3
    tag    <- trimws(input$analysis_panel_label %||% "")
    ref    <- input$metrics_ref_sample %||% ""
    base_scales <- list(scale_color_manual(   values = cols, labels = labs_v, name = NULL),
                        scale_fill_manual(    values = cols, labels = labs_v, name = NULL),
                        scale_linetype_manual(values = lts,  labels = labs_v, name = NULL))
    p <- switch(type,
      metric_summary = {
        rm   <- replicate_metric_rows_current()
        mets <- intersect(input$analysis_metrics %||% c("lysis_time", "od_drop"), names(rm))
        if (is.null(rm) || !length(mets)) stop("No per-replicate metrics for the selected samples.")
        long <- do.call(rbind, lapply(mets, function(m)
          data.frame(sample = rm$sample, replicate = rm$replicate, metric = m, value = rm[[m]],
                     stringsAsFactors = FALSE)))
        long <- long[is.finite(long$value), , drop = FALSE]
        if (nrow(long) == 0) stop("The selected metrics are NA for every replicate (a relative metric needs a reference sample).")
        long$metric <- factor(long$metric, levels = mets,
                              labels = vapply(mets, metric_axis_label, "", tunit = tunit, ylab = input$y_axis_label %||% "OD"))
        long$sample <- factor(long$sample, levels = samps, labels = unname(labs_v[samps]))
        x_guide <- if (as.numeric(input$axis_text_angle %||% "0") == 0 && max(nchar(levels(long$sample))) > 6)
          guide_axis(n.dodge = 2) else waiver()   # long sample names: two staggered rows
        summ <- long %>% group_by(metric, sample) %>%
          summarise(mean = mean(value), sd = if (n() >= 2) sd(value) else NA_real_, n = n(), .groups = "drop")
        g <- ggplot(long, aes(x = sample, y = value)) +
          geom_errorbar(data = summ, aes(x = sample, ymin = mean - sd, ymax = mean + sd, color = sample),
                        width = 0.3, linewidth = pt2lw(0.5), inherit.aes = FALSE, na.rm = TRUE) +
          geom_crossbar(data = summ, aes(x = sample, y = mean, ymin = mean, ymax = mean, color = sample),
                        width = 0.5, linewidth = pt2lw(0.75), inherit.aes = FALSE) +
          geom_point(aes(color = sample), shape = 21, fill = "white", size = ptsz * 0.6,
                     stroke = pt2lw(0.5), position = position_jitter(width = 0.12, height = 0, seed = 1)) +
          facet_wrap(~ metric, scales = "free_y", labeller = label_wrap_gen(width = 22)) +
          scale_color_manual(values = setNames(unname(cols[samps]), unname(labs_v[samps])), guide = "none") +
          scale_x_discrete(guide = x_guide) +
          scale_y_continuous(expand = expansion(mult = c(0.08, 0.18))) +   # headroom for marks
          labs(x = NULL, y = NULL)
        if (isTRUE(input$stats_mark) && nzchar(ref) && ref %in% rm$sample) {
          marks <- do.call(rbind, lapply(mets, function(m) {
            st <- compare_replicate_metric(rm, m, ref_sample = ref, method = input$stats_method %||% "welch")
            if (is.null(st$pairwise)) return(NULL)
            lab_m <- metric_axis_label(m, tunit = tunit, ylab = input$y_axis_label %||% "OD")
            top <- long[long$metric == lab_m, , drop = FALSE] %>% group_by(sample) %>%
              summarise(y = max(value), .groups = "drop")
            pwd <- st$pairwise[st$pairwise$versus == ref, , drop = FALSE]   # never show all-pairs results as "vs reference"
            if (nrow(pwd) == 0) return(NULL)
            pwd$sample <- factor(unname(labs_v[pwd$sample]), levels = levels(long$sample))
            d <- merge(pwd[, c("sample", "stars")], top, by = "sample")
            d$metric <- factor(lab_m, levels = levels(long$metric)); d
          }))
          if (!is.null(marks) && nrow(marks))
            g <- g + geom_text(data = marks, aes(x = sample, y = y, label = stars), vjust = -0.6,
                               size = (input$legend_font_size %||% 14) / ggplot2::.pt, inherit.aes = FALSE)
        }
        g
      },
      growth_rate = {
        gr <- compute_growth_rate_curves(prepare_replicate_data(), window = input$metrics_smooth_window %||% 5)
        if (is.null(gr)) stop("Growth rate needs at least 3 positive OD points per curve.")
        gr$variable <- factor(gr$variable, levels = samps)
        g <- ggplot(gr, aes(x = time, y = mu_mean, color = variable, linetype = variable, group = variable)) +
          geom_hline(yintercept = 0, linewidth = pt2lw(0.5), color = "grey50", linetype = "dashed")
        if (any(is.finite(gr$mu_sd)))
          g <- g + geom_ribbon(aes(ymin = mu_mean - mu_sd, ymax = mu_mean + mu_sd, fill = variable),
                               alpha = 0.15, color = NA, linetype = "solid", na.rm = TRUE)
        g + geom_line(linewidth = lw) + base_scales +
          labs(x = input$x_axis_label,
               y = paste0("Specific growth rate, μ (", tunit, "⁻¹)"))
      },
      normalized = {
        mode <- input$analysis_norm_mode %||% "reference"
        nd   <- compute_normalized_curves(prepare_plot_data(), ref, mode)
        if (is.null(nd)) stop(if (mode == "reference")
          "Pick the uninfected reference sample under Stats & Reports first." else "No data.")
        nd$variable <- factor(nd$variable, levels = samps)
        ref_lab <- if (mode == "reference") unname(labs_v[ref]) else "t = 0"
        ggplot(nd, aes(x = time, y = ratio, color = variable, linetype = variable,
                       shape = variable, group = variable)) +
          geom_hline(yintercept = 1, linewidth = pt2lw(0.5), color = "grey50", linetype = "dashed") +
          geom_line(linewidth = lw) + geom_point(size = ptsz * 0.7) +
          base_scales + scale_shape_manual(values = shp, labels = labs_v, name = NULL) +
          labs(x = input$x_axis_label, y = paste0(input$y_axis_label, " relative to ", ref_lab))
      },
      replicate_overlay = {
        rp <- prepare_replicate_data(); pd <- prepare_plot_data()
        rp$variable <- factor(rp$variable, levels = samps); pd$variable <- factor(pd$variable, levels = samps)
        rp$curve <- interaction(rp$variable, rp$replicate)
        g <- ggplot() +
          geom_line(data = rp, aes(x = time, y = value, color = variable, group = curve),
                    linewidth = lw * 0.5, alpha = 0.45) +
          geom_line(data = pd, aes(x = time, y = mean_value, color = variable, linetype = variable,
                                   group = variable), linewidth = lw * 1.3) +
          base_scales + labs(x = input$x_axis_label, y = input$y_axis_label)
        if (identical(input$y_scale_type, "log")) g + scale_y_log10(labels = y_tick_labels()) else g
      },
      dose_response = {
        rm     <- replicate_metric_rows_current()
        metric <- input$analysis_dose_metric %||% "lysis_time"
        if (is.null(rm) || !metric %in% names(rm)) stop("Metric not available for these samples.")
        moi    <- moi_map(samps, input$moi_override)
        rm$moi <- unname(moi[rm$sample])
        d <- rm[is.finite(rm$moi) & rm$moi > 0 & is.finite(rm[[metric]]),
                c("sample", "replicate", "moi", metric), drop = FALSE]
        names(d)[4] <- "value"
        if (nrow(d) == 0) stop("No sample has a positive MOI for this metric. Put the MOI in the sample name ('Phage MOI 10') or in the MOI box as 'sample = MOI'.")
        summ <- d %>% group_by(sample, moi) %>%
          summarise(mean = mean(value), sd = if (n() >= 2) sd(value) else NA_real_, .groups = "drop")
        d$sample <- factor(d$sample, levels = samps); summ$sample <- factor(summ$sample, levels = samps)
        ggplot(summ, aes(x = moi, y = mean)) +
          geom_line(color = "grey40", linewidth = pt2lw(0.5)) +
          geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, color = sample),
                        width = 0, linewidth = pt2lw(0.5), na.rm = TRUE) +
          geom_point(data = d, aes(x = moi, y = value, color = sample), shape = 21, fill = "white",
                     size = ptsz * 0.6, stroke = pt2lw(0.5)) +
          geom_point(aes(color = sample, shape = sample), size = ptsz) +
          base_scales + scale_shape_manual(values = shp, labels = labs_v, name = NULL) +
          scale_x_log10() +
          labs(x = "MOI", y = metric_axis_label(metric, tunit = tunit, ylab = input$y_axis_label %||% "OD"))
      },
      heatmap = {
        pd <- prepare_plot_data()
        pd$variable <- factor(pd$variable, levels = rev(samps), labels = rev(unname(labs_v[samps])))
        logit <- identical(input$y_scale_type, "log")
        pd$fillv <- if (logit) log10(pmax(pd$mean_value, 1e-6)) else pd$mean_value
        ggplot(pd, aes(x = time, y = variable, fill = fillv)) + geom_tile() +
          scale_fill_viridis_c(name = if (logit) paste0("log10 ", input$y_axis_label) else input$y_axis_label) +
          labs(x = input$x_axis_label, y = NULL)
      },
      stop("Unknown analysis figure type"))
    p <- p + figure_theme_layers()
    if (type == "metric_summary")   # facet strips: axis-title size, no grey box (theme default is 0.8 x base)
      p <- p + theme(legend.position = "none",
                     strip.text = element_text(size = input$axis_label_font_size, face = "plain"),
                     strip.background = element_blank())
    if (type == "heatmap")        p <- p + theme(legend.position = "right")
    if (type == "dose_response" && all(grepl("MOI", unname(labs_v[samps]), ignore.case = TRUE)))
      p <- p + theme(legend.position = "none")   # x axis already says it; the key would hide points
    if (isTRUE(input$custom_aspect_ratio)) p <- p + theme(aspect.ratio = 1 / input$aspect_ratio)
    p + coord_cartesian(clip = "off") + labs(tag = if (nzchar(tag)) tag else NULL)
  }
  
  generate_analysis_plot <- function(type = input$analysis_type %||% "metric_summary") {
    req(rv$data, input$selected_samples)
    tryCatch(build_analysis_plot(type), error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = paste("Figure not available:", conditionMessage(e)),
                 color = "grey30", size = 3.5, hjust = 0.5) +
        theme_void()
    })
  }
  output$analysis_container <- renderUI(preview_container("analysis_plot", "analysis_img"))
  register_preview("analysis_plot", "analysis_img", generate_analysis_plot)
  
  output$downloadAnalysisPlot <- downloadHandler(
    filename = function()
      paste0("analysis_", input$analysis_type %||% "figure", "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$export_format),
    content  = function(file) {
      sz   <- export_size_in(); fmt <- input$export_format
      args <- list(filename = file, plot = generate_analysis_plot(), device = export_device(fmt),
                   width = sz[1], height = sz[2], units = "in", dpi = input$export_dpi, bg = "white")
      if (identical(fmt, "tiff")) args$compression <- "lzw"
      do.call(ggsave, args)
    }
  )
  
  # ── Statistics tables ───────────────────────────────────────────────────────
  fmt_stats_table <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    for (cc in names(df)) if (is.numeric(df[[cc]]))
      df[[cc]] <- ifelse(is.finite(df[[cc]]), formatC(signif(df[[cc]], 3), format = "g"), "—")
    df
  }
  stats_notes_ui <- function(st) {
    notes <- st$warnings
    if (!is.null(st$pairwise))
      notes <- c(paste0("Metric: ", metric_label(st$metric), ". ",
                        if (nzchar(input$metrics_ref_sample %||% "")) paste0("Each sample vs reference '", input$metrics_ref_sample, "'. ") else "All pairs. ",
                        "p values Holm-adjusted; Hedges' g = standardized mean difference; ",
                        "95% CI of the difference (Welch)."), notes)
    else notes <- c("No comparison possible yet: load data with 2+ replicates per sample.", notes)
    div(lapply(notes, function(n) div(class = "stats-note", n)))
  }
  output$analysis_stats_notes   <- renderUI({ req(rv$data); stats_notes_ui(analysis_stats_current()) })
  output$analysis_stats_table   <- renderTable({ req(rv$data); fmt_stats_table(analysis_stats_current()$pairwise) },
                                               striped = TRUE, spacing = "xs")
  output$analysis_stats_omnibus <- renderTable({ req(rv$data); fmt_stats_table(analysis_stats_current()$omnibus) },
                                               striped = TRUE, spacing = "xs")
  output$analysis_stats_posthoc <- renderTable({ req(rv$data); fmt_stats_table(analysis_stats_current()$posthoc) },
                                               striped = TRUE, spacing = "xs")
  output$downloadAnalysisStatsCSV <- downloadHandler(
    filename = function() paste0("replicate_stats_", analysis_stat_metric(), "_",
                                 format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      st <- analysis_stats_current()
      con <- file(file, open = "w", encoding = "UTF-8"); on.exit(close(con))
      writeLines(paste0("# metric,", st$metric, " ; method,", input$stats_method %||% "welch",
                        " ; reference,", input$metrics_ref_sample %||% ""), con)
      for (w in st$warnings) writeLines(paste0("# ", w), con)
      if (!is.null(st$pairwise)) { writeLines("# pairwise", con); write.csv(st$pairwise, con, row.names = FALSE) }
      if (!is.null(st$omnibus))  { writeLines("# omnibus",  con); write.csv(st$omnibus,  con, row.names = FALSE) }
      if (!is.null(st$posthoc))  { writeLines("# posthoc (Tukey HSD)", con); write.csv(st$posthoc, con, row.names = FALSE) }
    }
  )
  
  # ── Dashboard (v2.11) ───────────────────────────────────────────────────────
  output$dash_cards <- renderUI({
    if (is.null(rv$data) || is.null(input$selected_samples))
      return(div(class = "dash-empty", "Load a CSV or the demo data to populate the dashboard."))
    pd <- tryCatch(prepare_plot_data(), error = function(e) NULL)
    if (is.null(pd) || nrow(pd) == 0) return(NULL)
    n_rng <- range(pd$n, na.rm = TRUE); t_rng <- range(pd$time, na.rm = TRUE)
    sz <- export_size_in()
    card <- function(title, value, sub = NULL)
      div(class = "dash-card", div(class = "dash-card-title", title),
          div(class = "dash-card-value", value), if (!is.null(sub)) div(class = "dash-card-sub", sub))
    div(class = "dash-cards",
        card("Samples", length(input$selected_samples), rv$data_source %||% ""),
        card("Replicates", if (n_rng[1] == n_rng[2]) n_rng[1] else paste0(n_rng[1], "–", n_rng[2]),
             "per sample and time point"),
        card("Time points", length(unique(pd$time)),
             paste0(t_rng[1], " – ", t_rng[2], " ", time_unit_from_label(input$x_axis_label))),
        card("Error measure", toupper(input$error_type %||% "none"),
             if (identical(input$error_display_mode, "shadow")) "shaded band" else "error bars"),
        card("Reference", if (nzchar(input$metrics_ref_sample %||% "")) input$metrics_ref_sample else "none set",
             "for infection metrics and statistics"),
        card("Figure", sprintf("%s × %s mm", round(sz[1] * 25.4), round(sz[2] * 25.4)),
             paste0(input$font_family, " ", input$axis_label_font_size, "/", input$axis_text_font_size,
                    " pt, ", toupper(input$export_format %||% "pdf"))))
  })
  
  output$dash_metrics_table <- renderTable({
    req(rv$data, input$selected_samples)
    m  <- tryCatch(curve_metrics_current(),     error = function(e) NULL); req(m)
    rm <- tryCatch(replicate_metrics_current(), error = function(e) NULL)
    cols <- c(sample = "Sample", max_od = "Peak OD", time_max_od = "t (peak)", mu_max = "μmax",
              doubling_time = "Doubling", lag_phase = "Lag", lysis_time = "Lysis onset",
              od_drop = "OD drop", lysis_efficiency = "Lysis eff.", residual_od = "Residual OD",
              recovery_slope = "Recovery", infection_strength = "Infection strength")
    cols <- cols[names(cols) %in% names(m)]
    out  <- m[, names(cols), drop = FALSE]
    f3   <- function(x) ifelse(is.finite(x), formatC(signif(x, 3), format = "g"), "—")
    for (cc in setdiff(names(out), "sample")) out[[cc]] <- f3(out[[cc]])
    if (!is.null(rm)) {
      idx <- match(m$sample, rm$sample)
      for (cc in intersect(c("lysis_time", "od_drop"), names(out))) {
        sdv <- rm[[paste0(cc, "_sd")]][idx]
        out[[cc]] <- ifelse(is.finite(sdv), paste0(out[[cc]], " ± ", formatC(signif(sdv, 2), format = "g")), out[[cc]])
      }
    }
    names(out) <- unname(cols)
    out
  }, striped = TRUE, spacing = "xs")
  
  # Dashboard panels are a working view, not an export: they keep the user's
  # colours, line weights and legend placement but use fixed screen-sized text,
  # so they read the same whether the figure is set to 20 pt or to 6 pt.
  dash_theme <- theme(
    text         = element_text(size = 10),  plot.title   = element_text(size = 11),
    axis.title   = element_text(size = 10),  axis.text    = element_text(size = 8),
    legend.text  = element_text(size = 8),   legend.title = element_text(size = 8),
    strip.text   = element_text(size = 9),   plot.tag     = element_blank(),
    legend.key.size = unit(4, "mm"))
  dash_plot <- function(fun) renderPlot({
    req(rv$data, input$selected_samples)
    p <- tryCatch(fun(), error = function(e) NULL)
    if (is.null(p)) { plot.new(); return(invisible(NULL)) }
    print(p + labs(tag = NULL, caption = NULL) + dash_theme)
  }, res = 96, bg = "white")
  output$dash_fig_main    <- dash_plot(function() generate_plot())
  output$dash_fig_rate    <- dash_plot(function() generate_analysis_plot("growth_rate"))
  output$dash_fig_norm    <- dash_plot(function()
    if (nzchar(input$metrics_ref_sample %||% "")) generate_analysis_plot("normalized")
    else generate_analysis_plot("replicate_overlay"))
  output$dash_fig_norm_title <- renderText(
    if (nzchar(input$metrics_ref_sample %||% "")) "Normalized OD (vs reference)" else "Replicate overlay (set a reference for normalized OD)")
  output$dash_fig_metrics <- dash_plot(function() generate_analysis_plot("metric_summary"))
  output$dash_notes <- renderUI({ req(rv$data); stats_notes_ui(analysis_stats_current()) })
  output$dash_stats <- renderTable({ req(rv$data); fmt_stats_table(analysis_stats_current()$pairwise) },
                                   striped = TRUE, spacing = "xs")
  output$dash_notebook <- renderUI({
    fields <- c(nb_exp_id = "Experiment", nb_experimenter = "Experimenter", nb_project = "Project",
                nb_exp_type = "Type", nb_host_strain = "Host", nb_phage_plasmid = "Phage / plasmid",
                nb_moi = "MOI", nb_replicate = "Replicates", nb_media = "Medium",
                nb_temperature = "Temperature (°C)", nb_time_inf = "Time of infection",
                nb_observations = "Observations")
    vals <- vapply(names(fields), function(id) { v <- input[[id]]; if (is.null(v)) "" else trimws(as.character(v)) }, "")
    keep <- nzchar(vals)
    if (!any(keep)) return(p(style = "color:#6c757d;font-size:.85em;",
                             "Fill in the Experiment Notebook (sidebar) and it is summarised here and in the reports."))
    tags$table(class = "table table-condensed", style = "width:auto;font-size:.85em;",
               lapply(which(keep), function(i) tags$tr(tags$th(fields[i]), tags$td(vals[i]))))
  })

  # ── Stats & Reports ──────────────────────────────────────────────────────────

  # Summary stats table with reader-friendly column names
  stats_table_export <- function() {
    pd <- prepare_plot_data()
    data.frame(time      = pd$time,
               sample    = pd$variable,
               mean      = pd$mean_value,
               sd        = pd$sd_value,
               n         = pd$n,
               sem       = pd$sem_value,
               ci95_half_width = pd$ci95_value,
               stringsAsFactors = FALSE)
  }

  output$downloadStatsCSV <- downloadHandler(
    filename = function()
      paste0("summary_stats_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      req(rv$data, input$selected_samples)
      write.csv(stats_table_export(), file, row.names = FALSE)
    }
  )

  curve_metrics_current <- function() {
    compute_curve_metrics(prepare_plot_data(),
                          smooth_window       = input$metrics_smooth_window %||% 5,
                          ref_sample          = input$metrics_ref_sample,
                          peak_smooth_window  = input$metrics_peak_smooth_window %||% 1)
  }

  # KC-2: warn when any sample has non-positive OD readings, since those
  # points are excluded from the mu_max/doubling_time/lag_phase windows
  # (see compute_curve_metrics) rather than silently clamped.
  output$nonpositive_od_warning_ui <- renderUI({
    m <- tryCatch(curve_metrics_current(), error = function(e) NULL)
    if (is.null(m) || !"n_nonpositive_od" %in% colnames(m)) return(NULL)
    bad <- m[m$n_nonpositive_od > 0, c("sample", "n_nonpositive_od"), drop = FALSE]
    if (nrow(bad) == 0) return(NULL)
    txt <- paste0(bad$sample, " (", bad$n_nonpositive_od, ")", collapse = ", ")
    div(style = "background:#fff3cd;border:1px solid #ffe69c;border-radius:4px;
                 padding:6px 8px;margin:4px 0 8px;font-size:.78em;color:#664d03;",
        HTML(paste0("<b>Non-positive OD detected</b> (samples, count of points): ", txt,
                    ". Those time points are excluded from the &mu;max/doubling-time/",
                    "lag-phase windows rather than clamped &mdash; a common cause is a ",
                    "small negative reading after blank subtraction.")))
  })

  output$downloadMetricsCSV <- downloadHandler(
    filename = function()
      paste0("curve_metrics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      req(rv$data, input$selected_samples)
      write.csv(curve_metrics_current(), file, row.names = FALSE)
    }
  )

  # KC-4: per-replicate metrics (mean +/- SD across replicates), additive
  # alongside curve_metrics_current() above.
  replicate_metrics_current <- function() {
    compute_replicate_metrics(prepare_replicate_data(),
                              smooth_window      = input$metrics_smooth_window %||% 5,
                              peak_smooth_window = input$metrics_peak_smooth_window %||% 1)
  }

  output$downloadReplicateMetricsCSV <- downloadHandler(
    filename = function()
      paste0("replicate_metrics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      req(rv$data, input$selected_samples)
      write.csv(replicate_metrics_current(), file, row.names = FALSE)
    }
  )

  # Shared metadata for both report formats
  report_meta <- function() {
    samps <- input$selected_samples
    filter_txt <- if (isTRUE(input$enable_time_filter)) {
      rng  <- input$time_filter_range
      excl <- parse_excluded_timepoints(input$exclude_timepoints, rv$all_timepoints)
      paste0("range ", if (!is.null(rng)) paste0(rng[1], "–", rng[2]) else "full",
             if (length(excl)) paste0("; excluded points: ", paste(sort(excl), collapse = ", "))
             else "")
    } else "none (all time points shown)"
    error_txt <- if (input$error_type == "none") "none" else
      paste0(switch(input$error_type, sd = "SD", sem = "SEM", ci95 = "95% CI"),
             if (!is.null(input$error_multiplier) && input$error_multiplier != 1)
               paste0(" × ", input$error_multiplier) else "",
             ", shown as ", if (identical(input$error_display_mode, "shadow"))
               "shaded ribbon" else "error bars")
    settings_df <- data.frame(
      setting = c("X scale", "Y scale", "X label", "Y label", "Palette",
                  "Line thickness", "Points shown", "Point size",
                  "Export size", "Export DPI",
                  "Blank/media OD subtracted",
                  "μmax smoothing window", "Peak-detection smoothing window",
                  "Reference sample"),
      value = c(input$x_scale_type, input$y_scale_type,
                input$x_axis_label, input$y_axis_label, input$color_palette,
                input$line_thickness, input$show_points, input$shape_size,
                paste0(input$export_width, " × ", input$export_height, " ", input$export_units %||% "in"),
                input$export_dpi,
                { b <- input$blank_od %||% 0; if (is.na(b) || b == 0) "none (off)" else b },
                input$metrics_smooth_window %||% 5,
                paste0(input$metrics_peak_smooth_window %||% 1,
                       if (identical(input$metrics_peak_smooth_window %||% 1, 1)) " (off)" else ""),
                if (nzchar(input$metrics_ref_sample %||% "")) input$metrics_ref_sample
                else "none (infection metrics omitted)"),
      stringsAsFactors = FALSE)
    # ── Experiment notebook: collect non-empty fields ─────────────────────────
    nb_pairs <- list(
      "Experiment ID"      = input$nb_exp_id,
      "Date"               = as.character(input$nb_date %||% ""),
      "Experimenter"       = input$nb_experimenter,
      "Project"            = input$nb_project,
      "Lab / Institution"  = input$nb_institution,
      "Experiment type"    = input$nb_exp_type,
      "Host strain(s)"     = input$nb_host_strain,
      "Phage / plasmid"    = input$nb_phage_plasmid,
      "MOI"                = input$nb_moi,
      "Replicate / batch"  = input$nb_replicate,
      "Growth medium"      = input$nb_media,
      "Temperature (°C)"   = input$nb_temperature,
      "Time of infection/induction" = input$nb_time_inf,
      "Inducer"            = input$nb_inducer,
      "Observations / results" = input$nb_observations,
      "Issues / anomalies" = input$nb_issues,
      "Next steps"         = input$nb_next_steps,
      "Tags"               = input$nb_tags
    )
    for (i in 1:3) {
      k <- input[[paste0("nb_custom_key", i)]]
      v <- input[[paste0("nb_custom_val", i)]]
      if (nzchar(trimws(k %||% ""))) nb_pairs[[trimws(k)]] <- v %||% ""
    }
    keep <- vapply(nb_pairs, function(v) nzchar(trimws(v %||% "")), logical(1))
    # Date always has a default; only emit the notebook when a real field is set
    notebook_df <- if (any(keep & names(nb_pairs) != "Date" &
                           names(nb_pairs) != "Experiment type")) {
      data.frame(field = names(nb_pairs)[keep],
                 value = unlist(nb_pairs[keep], use.names = FALSE),
                 stringsAsFactors = FALSE)
    } else NULL

    list(title       = if (nzchar(trimws(input$report_title))) input$report_title
                       else "Lysis Curve Analysis Report",
         author      = input$report_author,
         notes       = input$report_notes,
         generated   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
         data_file   = rv$data_source %||% "(unknown)",
         format      = if (rv$is_long_format) "long" else
                       paste0("wide (replicate columns pooled by name)"),
         time_col    = rv$time_col %||% "",
         samples_txt = paste(samps, collapse = ", "),
         filter_txt  = filter_txt,
         error_txt   = error_txt,
         settings_df = settings_df,
         notebook_df = notebook_df,
         font_pt     = input$report_font_size %||% 14)
  }

  if (has_base64enc) {
    output$downloadHTMLReport <- downloadHandler(
      filename = function()
        paste0("lysis_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"),
      content = function(file) {
        req(rv$data, input$selected_samples)
        metrics <- curve_metrics_current()
        p       <- generate_plot()
        tmp_png <- tempfile(fileext = ".png")
        on.exit(unlink(tmp_png), add = TRUE)
        sz <- export_size_in()
        ggsave(tmp_png, plot = p, device = export_device("png"),
               width = sz[1], height = sz[2],
               units = "in", dpi = if (sz[1] < 6) 300 else 150, bg = "white")
        img_b64 <- base64enc::base64encode(tmp_png)
        html <- build_html_report(report_meta(), img_b64,
                                  stats_table_export(), metrics)
        writeLines(html, file, useBytes = TRUE)
      }
    )
  }

  if (has_gridExtra) {
    output$downloadPDFReport <- downloadHandler(
      filename = function()
        paste0("lysis_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
      content = function(file) {
        req(rv$data, input$selected_samples)
        metrics <- curve_metrics_current()
        build_pdf_report(file, report_meta(), generate_plot(),
                         stats_table_export(), metrics,
                         width  = max(export_size_in()[1], 8),
                         height = max(export_size_in()[2], 6))
      }
    )
  }
  
  # ── PowerPoint Export ────────────────────────────────────────────────────────
  # Cumulative build: slide k shows samples 1..k with full colour (no grey mask).
  # Axes stay consistent across slides because pd_all (full dataset) is used for
  # every plot — only the subset of visible samples changes.
  # Each slide has a title text box naming the sample added on that slide.
  # Final slide = all samples, identical to the main plot.
  if (has_officer && has_rvg) {
    output$downloadPPTX <- downloadHandler(
      filename = function()
        paste0("od_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pptx"),
      content = function(file) {
        library(officer); library(rvg)
        req(rv$data, input$selected_samples)
        samps  <- input$selected_samples
        aes_v  <- resolve_aesthetics(samps)
        pd_all <- prepare_plot_data(samples = samps)
        sw     <- max(export_size_in()[1], 4)   # slide width  (inches)
        sh     <- max(export_size_in()[2], 3)   # slide height (inches)
        
        # Title bar: 0.5 in tall at top of slide; plot fills the rest
        title_h  <- 0.5
        plot_top <- title_h
        plot_h   <- sh - title_h
        
        # Font size for title: scale with slide width, min 12 pt
        title_pt <- max(12, round(sw * 2.2))
        
        prs <- read_pptx()
        
        for (k in seq_along(samps)) {
          visible_samps <- samps[1:k]
          new_samp      <- samps[k]   # the sample added on this slide
          
          # Slice aesthetics to only the visible subset (preserving original colours)
          aes_k <- list(
            shapes     = aes_v$shapes[    visible_samps],
            colors     = aes_v$colors[    visible_samps],
            line_types = aes_v$line_types[visible_samps],
            leg_labels = aes_v$leg_labels[visible_samps],
            filled_map = aes_v$filled_map[visible_samps]
          )
          
          # Data: only visible samples, but axes come from pd_all limits via build_plot
          pd_k <- pd_all[pd_all$variable %in% visible_samps, , drop = FALSE]
          
          # highlight_samples = NULL → all drawn samples in full colour, none greyed
          p_k <- build_plot(pd_k, visible_samps, aes_k, highlight_samples = NULL)
          
          prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
          
          # Title text box
          prs <- ph_with(prs,
                         value    = new_samp,
                         location = ph_location(left = 0, top = 0, width = sw, height = title_h))
          
          # Plot area below the title
          prs <- ph_with(prs,
                         dml(ggobj = p_k, bg = "white"),
                         location = ph_location(left = 0, top = plot_top,
                                                width = sw, height = plot_h))
        }
        
        # Final slide: all samples, full colour, no title override
        p_final <- build_plot(pd_all, samps, aes_v, highlight_samples = NULL)
        prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
        prs <- ph_with(prs,
                       dml(ggobj = p_final, bg = "white"),
                       location = ph_location(left = 0, top = 0, width = sw, height = sh))
        
        print(prs, target = file)
      }
    )
  }
  
  # ── GIF Export ───────────────────────────────────────────────────────────────
  # Cumulative build: frame k shows samples 1..k in full colour (no grey mask).
  # Axes stay consistent because pd_all establishes the full data range — only
  # the subset of plotted samples changes each frame.
  # Rendered by ggsave → gifski (no gganimate dependency).
  if (has_gifski) {
    output$downloadGIF <- downloadHandler(
      filename = function()
        paste0("od_anim_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".gif"),
      content = function(file) {
        library(gifski)
        req(rv$data, input$selected_samples)
        samps  <- input$selected_samples
        aes_v  <- resolve_aesthetics(samps)
        pd_all <- prepare_plot_data(samples = samps)
        w_px   <- max(as.integer(input$gif_width),  200L)
        h_px   <- max(as.integer(input$gif_height), 150L)
        fps    <- max(input$gif_fps, 0.2)
        
        tmp_dir <- tempfile(); dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        
        frame_files <- character(length(samps))
        for (k in seq_along(samps)) {
          visible_samps <- samps[1:k]
          
          # Slice aesthetics to only the visible subset
          aes_k <- list(
            shapes     = aes_v$shapes[    visible_samps],
            colors     = aes_v$colors[    visible_samps],
            line_types = aes_v$line_types[visible_samps],
            leg_labels = aes_v$leg_labels[visible_samps],
            filled_map = aes_v$filled_map[visible_samps]
          )
          
          # Filter data to visible samples; axes scale from pd_all range implicitly
          # via the scale limits set in build_plot using the supplied data
          pd_k <- pd_all[pd_all$variable %in% visible_samps, , drop = FALSE]
          
          p_k   <- build_plot(pd_k, visible_samps, aes_k, highlight_samples = NULL)
          fpath <- file.path(tmp_dir, sprintf("frame_%03d.png", k))
          ggsave(fpath, plot = p_k, device = "png",
                 width = w_px / 96, height = h_px / 96,
                 units = "in", dpi = 96)
          frame_files[k] <- fpath
        }
        
        gifski(frame_files, gif_file = file,
               width = w_px, height = h_px,
               delay = 1 / fps, loop = TRUE)
      }
    )
  }
  
}

shinyApp(ui, server)
