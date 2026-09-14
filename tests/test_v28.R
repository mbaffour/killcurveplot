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
check("min 0.05 @ t=30",               near(m$min_od, 0.05) && near(m$time_min_od, 30))
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

cat("\n--- KC-8: settings JSON version matches the app, not a stale \"2.5\" ---\n")
check("APP_VERSION constant is defined and is not the stale \"2.5\"",
      !is.null(env$APP_VERSION) && is.character(env$APP_VERSION) && env$APP_VERSION != "2.5")
check("saved settings JSON stamps APP_VERSION rather than a literal \"2.5\"",
      any(grepl("version\\s*=\\s*APP_VERSION", src)) &&
      !any(grepl('version\\s*=\\s*"2\\.5"', src)))

cat("\n============ SUMMARY:", pass, "passed /", fail, "failed ============\n")
if (fail > 0) quit(status = 1)
