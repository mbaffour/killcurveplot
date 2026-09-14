# ============================================================================
# v2.8 verification suite — tests the REAL app code:
#  - top-level helpers via source() environment
#  - server internals (ingest_data / prepare_plot_data) via shiny::testServer
#  - report builders end-to-end (HTML string, PDF bytes)
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v28.R")

pass <- 0L; fail <- 0L
check <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
near <- function(a, b, tol = 1e-9) is.finite(a) && abs(a - b) < tol

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("v28 sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))

cat("\n--- Top-level helper fixes ---\n")
check("rgb_to_hex(255,0,0) == #FF0000 (no alpha suffix)",
      toupper(env$rgb_to_hex(255, 0, 0)) == "#FF0000")
check("rgb_to_hex keeps alpha when < 1",
      nchar(env$rgb_to_hex(255, 0, 0, 0.5)) == 9)

cat("\n--- KC-11: decimal-comma / semicolon CSV support ---\n")
us_csv <- tempfile(fileext = ".csv")
writeLines(c("Time,WT,Mutant", "0,0.05,0.06", "10,0.12,0.15"), us_csv)
eu_csv <- tempfile(fileext = ".csv")
writeLines(c("Time;WT;Mutant", "0;0,05;0,06", "10;0,12;0,15"), eu_csv)

check("detect_csv_locale: US/UK comma file -> \"us\"",
      env$detect_csv_locale(us_csv) == "us")
check("detect_csv_locale: European semicolon file -> \"eu\"",
      env$detect_csv_locale(eu_csv) == "eu")

d_us <- env$read_csv_locale_aware(us_csv, "auto")
check("read_csv_locale_aware: US file auto-detected and parsed numeric",
      is.numeric(d_us$WT) && near(d_us$WT[1], 0.05))

d_eu <- env$read_csv_locale_aware(eu_csv, "auto")
check("read_csv_locale_aware: EU file auto-detected (3 columns, not collapsed to 1)",
      ncol(d_eu) == 3)
check("read_csv_locale_aware: EU decimal comma parsed as numeric, not character/NA",
      is.numeric(d_eu$WT) && near(d_eu$WT[1], 0.05) && near(d_eu$Mutant[2], 0.15))

d_eu_explicit <- env$read_csv_locale_aware(eu_csv, "eu")
check("read_csv_locale_aware: explicit \"eu\" locale matches auto-detect",
      identical(d_eu$WT, d_eu_explicit$WT))

cat("\n--- make_demo_data ---\n")
dd <- env$make_demo_data()
check("demo: duplicate replicate columns, 3 per sample",
      identical(unname(table(colnames(dd)[-1])), c(3L, 3L, 3L)) ||
      all(table(colnames(dd)[-1]) == 3))
check("demo: 13 time points", nrow(dd) == 13)
check("demo: all values positive numeric", all(sapply(dd[-1], is.numeric)) && all(dd[-1] > 0))

cat("\n--- compute_curve_metrics: hand-computed ground truth ---\n")
pd_toy <- data.frame(time = c(0, 10, 20, 30),
                     variable = "S1",
                     mean_value = c(0.1, 0.4, 0.2, 0.05))
m <- env$compute_curve_metrics(pd_toy)
check("metrics: one row",              nrow(m) == 1)
check("initial 0.1 / final 0.05",      near(m$initial_od, 0.1) && near(m$final_od, 0.05))
check("max 0.4 @ t=10",                near(m$max_od, 0.4) && near(m$time_max_od, 10))
check("min 0.05 @ t=30",               near(m$global_min_od, 0.05) && near(m$time_global_min_od, 30))
check("AUC trapezoid = 6.75",          near(m$auc, 6.75))
check("fold change = 0.5",             near(m$fold_change_final_vs_initial, 0.5))
check("pct drop from max = 87.5",      near(m$pct_drop_from_max, 87.5))
check("t half-max decline = 20",       near(m$t_half_max_decline, 20))
check("steepest decline = -0.02 over 10-20",
      near(m$steepest_decline_rate, -0.02) && grepl("^10", m$steepest_decline_interval))
check("lysis onset = 20, od_drop = 0.35, residual = 0.05",
      near(m$lysis_time, 20) && near(m$od_drop, 0.35) && near(m$residual_od, 0.05))
check("doubling = ln2/mu_max when growth detected",
      is.na(m$mu_max) || near(m$doubling_time, log(2)/m$mu_max))

# monotone-decreasing curve: growth metrics must be NA, not fabricated
pd_dec <- data.frame(time = c(0, 10, 20), variable = "D",
                     mean_value = c(1.0, 0.5, 0.25))
md <- env$compute_curve_metrics(pd_dec)
check("no-growth curve: mu_max & doubling NA",
      is.na(md$mu_max) && is.na(md$doubling_time))
check("no-growth curve: t50 interpolated (0->10, half=0.5 -> 10)",
      near(md$t_half_max_decline, 10))

# flat curve edge case
pd_flat <- data.frame(time = c(0, 10), variable = "F", mean_value = c(0.3, 0.3))
mf <- env$compute_curve_metrics(pd_flat)
check("flat curve: no crash, t50 NA, growth NA",
      is.na(mf$t_half_max_decline) && is.na(mf$mu_max))

cat("\n--- KC-2: non-positive OD is excluded from mu_max windows, not clamped ---\n")
# A -0.002 first reading (routine after blank subtraction) followed by a
# curve that exactly doubles every 10 time units. With window=3 the two
# windows touching the negative point (output positions 1-2) must be NA;
# the two clean windows (positions 3-4) give the true slope ln(2)/10.
pd_neg <- data.frame(time = c(0, 10, 20, 30, 40), variable = "N",
                     mean_value = c(-0.002, 0.05, 0.1, 0.2, 0.4))
mn <- env$compute_curve_metrics(pd_neg, smooth_window = 3)
check("KC-2: n_nonpositive_od counted correctly", mn$n_nonpositive_od == 1)
check("KC-2: mu_max = ln(2)/10 (from the clean windows only, not inflated)",
      near(mn$mu_max, log(2) / 10, tol = 1e-6))
check("KC-2: doubling_time = 10", near(mn$doubling_time, 10, tol = 1e-6))

# Directly demonstrate the regression the old code had: clamping the
# negative reading to 1e-9 before log() (log(pmax(od, 1e-9))) puts an
# enormous artificial slope into the window that contains it, which wins
# max() and inflates mu_max well above the true ln(2)/10.
old_clamped_log <- log(pmax(pd_neg$mean_value, 1e-9))
old_slopes  <- env$rolling_slope(pd_neg$time, old_clamped_log, 3)
old_mu_max  <- max(old_slopes[is.finite(old_slopes) & old_slopes > 1e-10])
check("KC-2: the old clamp-based slope would have been far larger than the fixed mu_max",
      old_mu_max > mn$mu_max * 2)

# A sample with no non-positive readings reports n_nonpositive_od = 0
pd_allpos <- data.frame(time = c(0, 10, 20), variable = "P",
                        mean_value = c(0.1, 0.2, 0.4))
check("KC-2: n_nonpositive_od = 0 when all OD > 0",
      env$compute_curve_metrics(pd_allpos, smooth_window = 3)$n_nonpositive_od == 0)

cat("\n--- KC-3: optional peak-detection smoothing (off by default) ---\n")
check("moving_average(w=1) is a no-op (the default)",
      identical(env$moving_average(c(1, 2, 3, 4, 5), 1), c(1, 2, 3, 4, 5)))
check("moving_average(w=3): centered, smaller window at the edges",
      near(env$moving_average(c(1, 2, 3, 4, 5), 3)[1], 1.5) &&
      near(env$moving_average(c(1, 2, 3, 4, 5), 3)[3], 3) &&
      near(env$moving_average(c(1, 2, 3, 4, 5), 3)[5], 4.5))

# Omitting peak_smooth_window must reproduce the exact pre-KC-3 numbers.
m_default   <- env$compute_curve_metrics(pd_toy)
m_explicit1 <- env$compute_curve_metrics(pd_toy, peak_smooth_window = 1)
check("KC-3: peak_smooth_window defaults to 1 (identical to the old, unsmoothed behaviour)",
      isTRUE(all.equal(m_default, m_explicit1)))

# A symmetric growth/lysis curve, true peak at t=60. Two "noisy" variants bump
# a DIFFERENT neighbor above the true peak (deterministic, not random, so this
# is reproducible): variant A's raw peak lands at t=50, variant B's at t=70 --
# exactly the kind of single-timepoint jitter the audit measured from 4%
# multiplicative noise. With peak_smooth_window = 3 both must stabilize back
# to the true peak at t=60.
od_clean <- c(0.05, 0.08, 0.13, 0.22, 0.35, 0.50, 0.55, 0.50, 0.35, 0.22, 0.13, 0.08, 0.05)
t_clean  <- seq(0, 120, 10)
od_A <- od_clean; od_A[6] <- 0.56   # bump t=50 above the true peak (0.55)
od_B <- od_clean; od_B[8] <- 0.56   # bump t=70 above the true peak instead
pd_A <- data.frame(time = t_clean, variable = "A", mean_value = od_A)
pd_B <- data.frame(time = t_clean, variable = "B", mean_value = od_B)

raw_A <- env$compute_curve_metrics(pd_A)
raw_B <- env$compute_curve_metrics(pd_B)
check("KC-3: without smoothing, noise moves the detected peak (unstable)",
      raw_A$time_max_od == 50 && raw_B$time_max_od == 70)

sm_A <- env$compute_curve_metrics(pd_A, peak_smooth_window = 3)
sm_B <- env$compute_curve_metrics(pd_B, peak_smooth_window = 3)
check("KC-3: with peak_smooth_window = 3, both variants find the true peak at t=60",
      sm_A$time_max_od == 60 && sm_B$time_max_od == 60)
check("KC-3: max_od is still the RAW value at the (smoothed-located) peak, not a smoothed value",
      near(sm_A$max_od, 0.55) && near(sm_B$max_od, 0.55))

cat("\n--- Server integration via testServer: replicate pooling ---\n")
tmp <- tempfile(fileext = ".csv")
writeLines(c("Time,WT,WT,WT,Mutant,Mutant,Mutant",
             "0,0.05,0.06,0.04,0.05,0.04,0.06",
             "30,0.12,0.15,0.10,0.08,0.09,0.07"), tmp)

testServer(app, {
  d <- read.csv(tmp, stringsAsFactors = FALSE, check.names = FALSE)
  ingest_data(d, "reps.csv")
  session$setInputs(selected_samples = c("WT", "Mutant"),
                    error_type = "sem", exclude_timepoints = "")
  check("col_map: WT -> 3 columns", length(rv$col_map[["WT"]]) == 3)
  pd <- prepare_plot_data()
  wt0 <- pd[pd$variable == "WT" & pd$time == 0, ]
  check("REPLICATE FIX: WT t=0 n = 3",   wt0$n == 3)
  check("REPLICATE FIX: WT t=0 mean",    near(wt0$mean_value, 0.05))
  check("REPLICATE FIX: WT t=0 sd 0.01", near(wt0$sd_value, 0.01, 1e-12))
  check("REPLICATE FIX: sem = sd/sqrt(3)", near(wt0$sem_value, 0.01/sqrt(3), 1e-12))
  check("REPLICATE FIX: ci95 = qt(.975,2)*sem",
        near(wt0$ci95_value, qt(0.975, 2) * 0.01/sqrt(3), 1e-12))
  mu0 <- pd[pd$variable == "Mutant" & pd$time == 30, ]
  check("Mutant t=30 mean over 3 reps", near(mu0$mean_value, mean(c(0.08, 0.09, 0.07))))

  st <- stats_table_export()
  check("stats export columns",
        identical(colnames(st),
                  c("time","sample","mean","sd","n","sem","ci95_half_width")))

  # NA replicate: n must count only non-NA values
  tmp2 <- tempfile(fileext = ".csv")
  writeLines(c("Time,A,A,A", "0,0.1,NA,0.3", "10,0.2,0.4,0.6"), tmp2)
  ingest_data(read.csv(tmp2, stringsAsFactors = FALSE, check.names = FALSE), "na.csv")
  session$setInputs(selected_samples = "A")
  pd2 <- prepare_plot_data()
  a0 <- pd2[pd2$time == 0, ]
  check("NA replicate: n = 2 (not 3)", a0$n == 2)
  check("NA replicate: mean of 2 values", near(a0$mean_value, 0.2))
  check("NA replicate: sem uses n = 2", near(a0$sem_value, sd(c(0.1,0.3))/sqrt(2), 1e-12))

  # demo data path through real server
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  session$setInputs(selected_samples = rv$od_vars)
  pd3 <- prepare_plot_data()
  check("demo via server: 3 samples x 13 timepoints = 39 rows", nrow(pd3) == 39)
  check("demo via server: all n = 3", all(pd3$n == 3))

  # long format through real server
  tmp3 <- tempfile(fileext = ".csv")
  writeLines(c("Time,Sample,OD",
               "0,WT,0.05","0,WT,0.06","0,WT,NA",
               "30,WT,0.12","30,WT,0.15","30,WT,0.10"), tmp3)
  ingest_data(read.csv(tmp3, stringsAsFactors = FALSE, check.names = FALSE), "long.csv")
  session$setInputs(selected_samples = "WT")
  pd4 <- prepare_plot_data()
  l0 <- pd4[pd4$time == 0, ]
  check("long + NA: n = 2", l0$n == 2)
  check("long + NA: mean = 0.055", near(l0$mean_value, 0.055))

  # KC-1: n = 1 must export SD/SEM/95% CI as NA (undefined), never 0
  tmp4 <- tempfile(fileext = ".csv")
  writeLines(c("Time,S1", "0,0.1", "10,0.5"), tmp4)
  ingest_data(read.csv(tmp4, stringsAsFactors = FALSE, check.names = FALSE), "single_rep.csv")
  session$setInputs(selected_samples = "S1")
  pd5 <- prepare_plot_data()
  check("KC-1: n = 1 at every time point", all(pd5$n == 1))
  check("KC-1: sd_value is NA (not 0) at n = 1",  all(is.na(pd5$sd_value)))
  check("KC-1: sem_value is NA (not 0) at n = 1", all(is.na(pd5$sem_value)))
  check("KC-1: ci95_value is NA (not 0) at n = 1", all(is.na(pd5$ci95_value)))
  st5 <- stats_table_export()
  check("KC-1: exported CSV has NA sd/sem/ci95 at n = 1",
        all(is.na(st5$sd)) && all(is.na(st5$sem)) && all(is.na(st5$ci95_half_width)))

  # A mix of n=1 and n>=2 time points: only the n=1 rows should be NA
  tmp5 <- tempfile(fileext = ".csv")
  writeLines(c("Time,S2,S2", "0,0.10,0.12", "10,0.50,NA"), tmp5)
  ingest_data(read.csv(tmp5, stringsAsFactors = FALSE, check.names = FALSE), "mixed_n.csv")
  session$setInputs(selected_samples = "S2")
  pd6   <- prepare_plot_data()
  r_n2  <- pd6[pd6$time == 0, ]
  r_n1  <- pd6[pd6$time == 10, ]
  check("KC-1: n = 2 row keeps a real (non-NA) sd/sem/ci95",
        r_n2$n == 2 && !is.na(r_n2$sd_value) && !is.na(r_n2$sem_value) && !is.na(r_n2$ci95_value))
  check("KC-1: n = 1 row (NA replicate dropped) is NA, not 0",
        r_n1$n == 1 && is.na(r_n1$sd_value) && is.na(r_n1$sem_value) && is.na(r_n1$ci95_value))
})

cat("\n--- Report builders (headless, real functions) ---\n")
meta <- list(title = "Test Report", author = "Tester", notes = "A note\nline2",
             generated = "2026-08-19 12:00:00", data_file = "reps.csv",
             format = "wide", time_col = "Time", samples_txt = "WT, Mutant",
             filter_txt = "none", error_txt = "SEM, error bars",
             settings_df = data.frame(setting = c("X scale"), value = c("linear"),
                                      stringsAsFactors = FALSE),
             notebook_df = data.frame(
               field = c("Experiment ID", "Host strain(s)", "Observations / results"),
               value = c("EXP-042", "E. coli MG1655", "Rapid lysis at MOI 10\nsecond line"),
               stringsAsFactors = FALSE),
             font_pt = 16)
stats_df <- data.frame(time = c(0, 30), sample = "WT", mean = c(0.05, 0.123456789),
                       sd = c(0.01, 0.02), n = 3L, sem = c(0.005, 0.01),
                       ci95_half_width = c(0.02, 0.05), stringsAsFactors = FALSE)
metrics_df <- env$compute_curve_metrics(
  data.frame(time = c(0, 30), variable = "WT", mean_value = c(0.05, 0.12)))

html <- env$build_html_report(meta, base64enc::base64encode(charToRaw("fakepng")),
                              stats_df, metrics_df)
check("HTML: single string", is.character(html) && length(html) == 1)
check("HTML: contains title", grepl("Test Report", html, fixed = TRUE))
check("HTML: embeds base64 image", grepl("data:image/png;base64,", html, fixed = TRUE))
check("HTML: contains stats table header", grepl("<th>ci95_half_width</th>", html, fixed = TRUE))
check("HTML: contains metric definitions", grepl("trapezoidal rule", html, fixed = TRUE))
check("HTML: notes escaped + preserved", grepl("A note<br/>line2", html, fixed = TRUE))
check("HTML: escapes special chars", {
  meta2 <- meta; meta2$title <- "A<B & C"
  h2 <- env$build_html_report(meta2, "", stats_df, metrics_df)
  grepl("A&lt;B &amp; C", h2, fixed = TRUE)
})
htmlfile <- tempfile(fileext = ".html"); writeLines(html, htmlfile, useBytes = TRUE)
check("HTML: file written > 5 KB", file.info(htmlfile)$size > 5000)
check("HTML: notebook section present", grepl("Experiment notebook", html, fixed = TRUE))
check("HTML: notebook field rendered", grepl("EXP-042", html, fixed = TRUE))
check("HTML: notebook multiline -> <br/>",
      grepl("Rapid lysis at MOI 10<br/>second line", html, fixed = TRUE))
check("HTML: report font size applied", grepl("font-size: 16px", html, fixed = TRUE))
meta_nonb <- meta; meta_nonb$notebook_df <- NULL
check("HTML: notebook omitted when empty",
      !grepl("Experiment notebook",
             env$build_html_report(meta_nonb, "", stats_df, metrics_df), fixed = TRUE))

p_test <- ggplot(data.frame(x = 1:5, y = (1:5)^2), aes(x, y)) + geom_line()
# big stats table to exercise pagination
big_stats <- do.call(rbind, replicate(40, stats_df, simplify = FALSE))
pdffile <- tempfile(fileext = ".pdf")
ok_pdf <- tryCatch({
  env$build_pdf_report(pdffile, meta, p_test, big_stats, metrics_df)
  TRUE
}, error = function(e) { cat("PDF ERROR:", conditionMessage(e), "\n"); FALSE })
check("PDF: builds without error", ok_pdf)
check("PDF: file magic %PDF", ok_pdf &&
        identical(readBin(pdffile, "raw", 4), charToRaw("%PDF")))
check("PDF: > 10 KB (multi-page with tables)", ok_pdf && file.info(pdffile)$size > 10000)

cat("\n--- Settings prefix fix present in both handlers ---\n")
src <- readLines("Lysis_Curve_App_v28.R")
check("global_exceptions appears twice (save + load)",
      sum(grepl("global_exceptions <- c(\"shape_size\", \"color_palette\")", src, fixed = TRUE)) == 2)

cat("\n--- KC-6: default figure labels its SEM error bars ---\n")
check("show_error_caption defaults to TRUE in the UI widget",
      any(grepl('checkboxInput\\("show_error_caption"', src)) &&
      { i <- grep('checkboxInput\\("show_error_caption"', src)[1]
        any(grepl("value = TRUE", src[i:(i + 3)])) })
check("Clear-settings reset also defaults show_error_caption to TRUE",
      any(grepl("show_error_caption\\s*=\\s*TRUE", src)) &&
      !any(grepl("show_error_caption\\s*=\\s*FALSE", src)))

cat("\n--- KC-9: CITATION.cff and a licence/citation section in the README ---\n")
check("CITATION.cff exists at repo root", file.exists("CITATION.cff"))
check("CITATION.cff declares the GPL-3 licence",
      any(grepl("^license:.*GPL-3", readLines("CITATION.cff"))))
readme <- readLines("README.md")
check("README has a License section", any(grepl("^## License", readme)))
check("README has a Citation section", any(grepl("^## Citation", readme)))

cat("\n--- KC-8: settings JSON version matches the app, not a stale \"2.5\" ---\n")
check("APP_VERSION constant is defined and is not the stale \"2.5\"",
      !is.null(env$APP_VERSION) && is.character(env$APP_VERSION) && env$APP_VERSION != "2.5")
check("saved settings JSON stamps APP_VERSION rather than a literal \"2.5\"",
      any(grepl("version\\s*=\\s*APP_VERSION", src)) &&
      !any(grepl('version\\s*=\\s*"2\\.5"', src)))

cat("\n============ SUMMARY:", pass, "passed /", fail, "failed ============\n")
if (fail > 0) quit(status = 1)
