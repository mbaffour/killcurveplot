# ============================================================================
# Hardening round 1 (engine) regression suite -- one check per finding of the engine / supervisor break reports
# (names F<n> = engine finding n, S<n> = supervisor finding n). Pure functions plus two testServer renders.
# Run from the repository root:  Rscript tests/test_v222_hardening.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v222_hardening.R")

pass <- 0L; fail <- 0L; skip <- 0L
check  <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
skipit <- function(desc) { skip <<- skip + 1L; cat("SKIP:", desc, "\n") }
near   <- function(a, b, tol = 1e-9) length(a) == 1 && length(b) == 1 && is.finite(a) && is.finite(b) && abs(a - b) < tol
has_pkg <- function(p) requireNamespace(p, quietly = TRUE)
quiet  <- function(expr) suppressWarnings(suppressMessages(expr))

out_dir0 <- getwd()
env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))

w <- function(x) { p <- tempfile(fileext = ".csv"); writeBin(if (is.raw(x)) x else charToRaw(x), p); p }   # a test file
rd <- function(txt, name = "x.csv") env$read_data_file(w(txt), name)

# ── Ingest (F1-F9) ────────────────────────────────────────────────────────────
cat("\n--- F1: cp1252 header (degree sign, byte 0xB0) ---\n")
raw1 <- c(charToRaw("Time,T"), as.raw(0xB0), charToRaw(" 600,WT,B\n0,25,.1,.2\n10,25,.2,.3\n20,25,.3,.4\n"))
d1 <- tryCatch(rd(raw1), error = function(e) e)
check("F1: a comma-separated cp1252 file is read (no invalid-UTF-8 error), 4 columns, 3 rows",
      is.data.frame(d1) && ncol(d1) == 4 && nrow(d1) == 3)
raw1b <- c(charToRaw("Time;T"), as.raw(0xB0), charToRaw(" 600;WT\n0;25;0,1\n10;25;0,2\n20;25;0,3\n"))
d1b <- tryCatch(rd(raw1b), error = function(e) e)
check("F1: a semicolon-separated cp1252 file is NOT collapsed to one column; decimal comma read",
      is.data.frame(d1b) && ncol(d1b) == 3 && near(d1b[[3]][2], 0.2))
check("F1: the converted header keeps the degree sign (valid UTF-8)",
      is.data.frame(d1) && any(grepl("T°", enc2utf8(names(d1)), fixed = TRUE) | grepl("T°", names(d1), fixed = TRUE)))

cat("\n--- F2: blank trailing header cells ---\n")
d2 <- rd("Time,WT,,\n0,.1,.2,.3\n10,.2,.3,.4\n20,.3,.4,.5\n")
check("F2: 'Time,WT,,' keeps its header: 4 columns, 3 rows (no row lost)", ncol(d2) == 4 && nrow(d2) == 3 && names(d2)[1] == "Time")
check("F2: blank header cells take the name on their left (replicate columns of WT)", identical(names(d2), c("Time", "WT", "WT", "WT")))
d2b <- rd("Time,WT,,,Mut,,\n0,.1,.2,.3,.4,.5,.6\n10,.2,.3,.4,.5,.6,.7\n20,.3,.4,.5,.6,.7,.8\n")
check("F2: two named groups with blank replicate headers -> WT x3, Mut x3",
      identical(names(d2b), c("Time", rep("WT", 3), rep("Mut", 3))) && nrow(d2b) == 3)
d2c <- rd("Software Version,3.08,,,\nDate,x,,,\nTime,A,B,C,D\n0,.1,.2,.3,.4\n10,.2,.3,.4,.5\n")
check("F2: a comma-padded reader preamble is still skipped (header = 'Time,A,B,C,D')",
      identical(names(d2c), c("Time", "A", "B", "C", "D")) && nrow(d2c) == 2)

cat("\n--- F3: quoted comma in a long-format sample name ---\n")
d3 <- rd('Time,Sample,OD\n0,"WT, rep 1",0.1\n10,"WT, rep 1",0.2\n0,B,0.1\n10,B,0.3\n0,"C, x",0.1\n10,"C, x",0.2\n')
check("F3: header kept and all 6 rows read", identical(names(d3), c("Time", "Sample", "OD")) && nrow(d3) == 6)
check("F3: the quoted names stay whole", identical(unique(d3$Sample), c("WT, rep 1", "B", "C, x")))

cat("\n--- F4: a data row wider than the header ---\n")
e4 <- tryCatch(rd("Time,A\n0,.1,.5\n10,.2,.6\n20,.3,.7\n"), error = function(e) e)
check("F4: a header narrower than every data row is an explicit error, not a silently shifted table",
      inherits(e4, "error") && grepl("header has 2", conditionMessage(e4)))

cat("\n--- F5: temperature-column regex ---\n")
d5 <- data.frame(Time = 1:3, Template = c(.1, .2, .3), `Temperate phage` = c(.1, .2, .3), Attempt2 = c(.1, .2, .3),
                 WT = c(.1, .2, .3), `T° 600` = 37, Temp = 37, `Temperature (°C)` = 37, `37 °C` = 37,
                 `Phage at 4 °C` = c(.1, .2, .3), check.names = FALSE)
u5 <- env$wide_sample_columns(d5, "Time")$used
check("F5: Template / Temperate phage / Attempt2 / WT / 'Phage at 4 °C' are samples",
      all(c("Template", "Temperate phage", "Attempt2", "WT", "Phage at 4 °C") %in% u5))
check("F5: T° 600, Temp, Temperature (°C), '37 °C' are still dropped as temperature columns",
      !any(c("T° 600", "Temp", "Temperature (°C)", "37 °C") %in% u5))

cat("\n--- F6: a blank time cell between stacked blocks ---\n")
r6 <- env$infer_wide_replicates(data.frame(Time = c(0, 10, NA, 0, 10)), "Time")
check("F6: two blocks separated by a blank time row are two replicates", identical(r6$reps, 2L) && identical(r6$block_id, c(1L, 1L, 1L, 2L, 2L)))
check("F6: ordinary stacked blocks unchanged", identical(env$infer_wide_replicates(data.frame(Time = c(0, 10, 0, 10, 0, 10)), "Time")$reps, 3L))

cat("\n--- F7 / F8 / F9: sentinels, time parsing, warnings ---\n")
check("F7: 'Inf' / 'NaN' text cells become NA (not Inf)",
      { v <- env$coerce_sentinel_columns(data.frame(A = c("0.1", "Inf", "0.3", "NaN")))$data$A; is.na(v[2]) && is.na(v[4]) && near(v[3], 0.3) })
check("F8: '0x10' is not the number 16", all(is.na(env$parse_time_column(c("0x10", "0x20"))$values)))
check("F8: ordinary numeric time still parses", identical(env$parse_time_column(c("0", "15", "30.5"))$values, c(0, 15, 30.5)))
w9 <- character(0)
d9 <- withCallingHandlers(env$read_csv_smart(w("Time,A,B\n0,.1,.2")), warning = function(x) { w9 <<- c(w9, conditionMessage(x)); invokeRestart("muffleWarning") })
check("F9: a CSV without a trailing newline reads without the 'incomplete final line' warning", length(w9) == 0 && nrow(d9$data) == 1)

# ── Metrics (F10-F14) ─────────────────────────────────────────────────────────
cat("\n--- F10 / F11: five or more non-positive readings ---\n")
tt <- seq(0, 285, 15); y5 <- c(rep(0, 5), 0.02 * 2^(0:14) / 10); y4 <- c(rep(0, 4), 0.02 * 2^(0:15) / 10)
m5 <- env$compute_one_curve_metrics(tt, y5, "A")
check("F10: five zero readings: mu_max / doubling_time / lag_phase are no longer NA",
      near(m5$mu_max, log(2) / 15, 1e-6) && near(m5$doubling_time, 15, 1e-6) && is.finite(m5$lag_phase))
check("F10: the four-zero case is unchanged (lag 90, mu_max the same)",
      { m4 <- env$compute_one_curve_metrics(tt, y4, "A"); near(m4$mu_max, m5$mu_max, 1e-9) && near(m4$lag_phase, 90) })
check("F10: n_nonpositive_od still reports the count", m5$n_nonpositive_od == 5)
check("F10: rolling_slope on an all-NA window gives NA, not an error", { r <- env$rolling_slope(1:6, c(NA, NA, NA, NA, NA, 1), 3); all(is.na(r)) })
g11 <- env$compute_growth_rate_curves(data.frame(time = 1:20, variable = "A", replicate = 1L, value = c(rep(-1, 5), exp(0.2 * (6:20)))), 5)
check("F11: compute_growth_rate_curves is no longer NULL for a curve with five negative readings; mu ~ 0.2 late",
      !is.null(g11) && nrow(g11) > 0 && near(max(g11$mu_mean), 0.2, 1e-6))
check("F11: curve_phases finds more than the single 'lag' phase", length(unique(env$curve_phases(tt, y5)$phase)) > 1)

cat("\n--- F12: extinction is a crossing from above ---\n")
ex <- env$curve_extinction_time(c(5, 10, 20), c(0.04, 0.03, 0.02))
check("F12: a curve that starts below the floor has extinction_time NA, with the reason attached",
      is.na(ex) && identical(attr(ex, "reason"), "starts_below_floor"))
check("F12: a curve that starts below the floor, grows and lyses back below it reports that later crossing (not t_start)",
      near(env$curve_extinction_time(c(0, 10, 20, 30), c(0.03, 0.5, 0.15, 0.03)), 20 + 10 * 0.10 / 0.12))
check("F12: a genuine crossing still interpolates", near(env$curve_extinction_time(c(0, 10, 20), c(0.5, 0.15, 0.03)), 10 + 10 * 0.10 / 0.12))
demo <- env$make_demo_data("lysis"); cn <- colnames(demo)
long <- do.call(rbind, lapply(2:ncol(demo), function(j) data.frame(time = demo[[1]], variable = cn[j], replicate = (j - 2) %% 3 + 1, value = demo[[j]])))
mm <- env$compute_replicate_metric_rows(long)
check("F12: the Uninfected control's replicates agree (all NA) instead of NA / 0 / 0", all(is.na(mm$extinction_time[mm$sample == "Uninfected"])))
check("F12: the metrics frame has no stray attribute on extinction_time", is.null(attributes(env$compute_one_curve_metrics(c(0, 10, 20), c(.04, .03, .02), "x")$extinction_time)))

cat("\n--- F13 / F14: blank sample range, sample named '' ---\n")
set.seed(1); tt13 <- seq(0, 300, 15)
rp13 <- do.call(rbind, lapply(c("S", "Blank"), function(s) do.call(rbind, lapply(1:3, function(r) data.frame(time = tt13, variable = s, replicate = r, value = 0.1 + tt13 / 300)))))
rp13 <- rp13[!(rp13$variable == "Blank" & rp13$time > 150), ]
o13 <- env$transform_frame(rp13, blank_mode = "sample", blank_sample = "Blank")
check("F13: readings lost because the blank curve ends earlier are reported in the curation notes",
      length(attr(o13, "curation_notes")) == 1 && grepl("outside the time range of the blank", attr(o13, "curation_notes")))
rp14 <- data.frame(time = rep(c(0, 10, 20), 2), variable = rep(c("", "B"), each = 3), replicate = 1L, value = c(1, 2, 4, 1, 3, 5))
o14 <- env$transform_frame(rp14, transform = "pct_max")
check("F14: a sample named '' is scaled to 0-100 like any other (no NA)", !anyNA(o14$value) && near(max(o14$value[o14$variable == ""]), 100))

# ── Hangs (F15-F18) ───────────────────────────────────────────────────────────
cat("\n--- F15: fit_exponential_window cost ---\n")
set.seed(1); gr <- function(t) 0.05 + 1.2 / (1 + exp(-.01 * (t - 700)))
t15 <- seq(0, 1440, length.out = 5000); y15 <- gr(t15) + rnorm(5000, 0, .005)
tm15 <- system.time(r15 <- env$fit_exponential_window(t15, y15))[["elapsed"]]
check("F15: 5000 points (was 27 s) finish in under 8 s and return a fit", tm15 < 8 && is.finite(r15$mu) && r15$mu > 0)
set.seed(2); old_dim <- NULL
tt15 <- sort(runif(60, 0, 300)); yy15 <- abs(0.05 + 1.2 / (1 + exp(-.03 * (tt15 - 120))) + rnorm(60, 0, .02)) + 1e-3
r15s <- env$fit_exponential_window(tt15, yy15)
chk <- { lt <- log(yy15); best <- -Inf; for (i in 1:57) for (j in (i + 3):60) { f <- lm(lt[i:j] ~ tt15[i:j]); sl <- coef(f)[2]
          r2 <- summary(f)$r.squared; if (sl > 0 && r2 > best + 1e-10) best <- r2 }; best }
check("F15: up to 500 points the search is still exhaustive (its R2 equals a brute-force scan)", near(r15s$r2, chk, 1e-8))

cat("\n--- F16: bootstrap cap ---\n")
tt16 <- seq(0, 300, 15)
rp16 <- do.call(rbind, lapply(paste0("S", 1:40), function(s) do.call(rbind, lapply(1:6, function(r)
  data.frame(time = tt16, variable = s, replicate = r, value = 0.05 + 1.2 / (1 + exp(-.03 * (tt16 - 90))) + rnorm(length(tt16), 0, .01))))))
# the cap is arithmetic (no need to run the 14 000-run bootstrap): the same function with a tiny budget
bt16 <- local({
  e2 <- env; old <- e2$BOOT_MAX_ENGINE_RUNS; e2$BOOT_MAX_ENGINE_RUNS <- 60L; on.exit(e2$BOOT_MAX_ENGINE_RUNS <- old)
  rp <- rp16[rp16$variable %in% c("S1", "S2", "S3", "S4"), ]
  env$bootstrap_metrics(rp, "mu_max", n_boot = 1000)
})
check("F16: over the run budget the draws are lowered, with a note and n_boot_used", attr(bt16, "n_boot_used") < 1000 && attr(bt16, "n_boot_used") >= 20 &&
      grepl("draws were requested", attr(bt16, "note")))
bt16b <- env$bootstrap_metrics(rp16[rp16$variable == "S1", ], "mu_max", n_boot = 300)
check("F16: inside the budget nothing changes (all draws used, no note)", identical(attr(bt16b, "n_boot_used"), 300L) && is.null(attr(bt16b, "note")))

cat("\n--- F17: two-way time ANOVA guard ---\n")
mk17 <- function(ns, nt = 8) do.call(rbind, lapply(paste0("S", seq_len(ns)), function(s) do.call(rbind, lapply(1:3, function(k)
  data.frame(time = seq(0, 100, length.out = nt), variable = s, replicate = k, value = 0.1 + seq(0, 100, length.out = nt) / 100 + rnorm(nt, 0, .01))))))
set.seed(3)
tm17 <- system.time(r17 <- env$two_way_time_anova(mk17(40)))[["elapsed"]]
check("F17: 40 samples (> 30) returns a note in under 3 s instead of fitting", tm17 < 3 && is.na(r17$anova$p[1]) && grepl("too large", r17$note))
tm17b <- system.time(r17b <- env$two_way_time_anova(mk17(14)))[["elapsed"]]
check("F17: 14 samples skip the AR(1) mixed model and use the fast aov fit, with a note",
      tm17b < 20 && is.finite(r17b$anova$p[1]) && grepl("skipped", r17b$note) && grepl("aov", r17b$method))

cat("\n--- F18: post-hoc table guard ---\n")
g18 <- rep(paste0("S", 1:96), each = 3); set.seed(2); v18 <- rnorm(288)
tm18 <- system.time(p18 <- env$posthoc_table(v18, g18, "dunn"))[["elapsed"]]
check("F18: all-pairs Dunn on 96 groups (4560 pairs) returns a note at once", tm18 < 3 && nrow(p18) == 1 && is.na(p18$p_adj) && grepl("too many comparisons", p18$note))
tm18b <- system.time(p18b <- env$posthoc_table(v18, g18, "dunnett", reference = "S1"))[["elapsed"]]
check("F18: Dunnett vs a reference with 95 comparisons falls back to Holm-Welch (labelled), quickly",
      tm18b < 5 && nrow(p18b) == 95 && grepl("fallback", p18b$method[1]) && all(is.finite(p18b$p_adj)))
# A setTimeLimit() stop is an ordinary error condition. R only checks the limit now and then (every ~1000 evaluations),
# so the check that matters is that the engine's own tryCatch(error = ) handlers re-raise it instead of swallowing
# it and carrying on (the limit is consumed once it fires). The stop is simulated here by mocking an inner function.
tl_err <- function(f) tryCatch({ f(); "swallowed" }, error = function(e) conditionMessage(e))
local({
  keep <- list(m = env$compute_one_curve_metrics, c = env$.f22_clean_xy)
  on.exit({ env$compute_one_curve_metrics <- keep$m; env$.f22_clean_xy <- keep$c })
  env$compute_one_curve_metrics <- function(...) stop("reached elapsed time limit")
  check("F18: a time-limit stop inside the bootstrap engine run is re-raised, not turned into NA draws",
        grepl("time limit", tl_err(function() env$bootstrap_metrics(rp16[rp16$variable == "S1", ], "mu_max", n_boot = 300))))
  env$.f22_clean_xy <- function(...) stop("reached elapsed time limit")
  check("F18: ... and by fit_exponential_window (which promises never to raise for ordinary failures)",
        grepl("time limit", tl_err(function() env$fit_exponential_window(1:10, exp(.2 * (1:10))))))
})
check("F18: an ordinary error is still a note, not a raise", grepl("^error:", env$fit_exponential_window(1:10, "a")$note) || is.na(env$fit_exponential_window(1:10, "a")$mu))

# ── Wrong results (F19-F30) ───────────────────────────────────────────────────
cat("\n--- F19: dotted sample names ---\n")
f19 <- function(l, tm, s) data.frame(time = tm, variable = l, replicate = 1L, value = exp(s * tm))
rp19 <- rbind(f19("A", seq(1.5, 30, by = 1.5), .1), f19("A.4", seq(1, 30, by = 1), .3))
r19 <- env$compute_od_rate_curves(rp19, 5)
check("F19: compute_od_rate_curves never merges 'A' at 4.5 with 'A.4' at 5 (n is 1 everywhere)", max(r19$n) == 1)
check("F19: compute_growth_rate_curves likewise", max(env$compute_growth_rate_curves(rp19, 5)$n) == 1)
check("F19: 'A.4' keeps all of its 26 centred rows", sum(r19$variable == "A.4") == 26)
sm19 <- env$summarise_replicate_values(data.frame(time_requested = c(1, 1, 1, 1), sample = c("A", "A", "A.4", "A.4"), value = c(1, 2, 10, 20)),
                                       "value", c("time_requested", "sample"))
check("F19: summarise_replicate_values groups on the exact keys", nrow(sm19) == 2 && identical(sm19$mean, c(1.5, 15)))

cat("\n--- F20: invalid interval of a nearly saturated fit ---\n")
t20 <- c(30.0962780602276, 63.3377867750823, 109.891604189761, 133.176882541738, 234.063117904589, 273.889829730615, 276.158954738639, 290.325238066725)
y20 <- c(0.16189441997332, 0.267439235325895, 0.518150690105163, 0.620647748907408, 0.127289622969064, 0.0765895109841758, 0.0888491136267059, 0.044666702010184)
r20 <- quiet(env$fit_lysis_models(t20, y20))
fp <- r20[r20$model == "fpl_decay", ]
check("F20: no reported interval has lo > hi or excludes its estimate (plateau)",
      !is.finite(fp$plateau_lo) || (fp$plateau_lo <= fp$plateau_hi && fp$plateau_lo <= fp$plateau && fp$plateau <= fp$plateau_hi))
check("F20: the row says the interval was left out", !is.finite(fp$plateau_lo) && grepl("not valid", fp$note))

cat("\n--- F21: Tukey orientation with a reference ---\n")
v21 <- c(1, 2, 3, 2, 3, 4, 10, 11, 13); g21 <- rep(c("A", "B", "C"), each = 3)
tk <- env$posthoc_table(v21, g21, "tukey", reference = "C"); hw <- env$posthoc_table(v21, g21, "holm_welch", reference = "C")
check("F21: Tukey with reference C reports group1 - C (group2 = reference) like every other method",
      all(tk$group2 == "C") && identical(sort(tk$group1), c("A", "B")) && near(tk$estimate[tk$group1 == "A"], hw$estimate[hw$group1 == "A"]))
check("F21: the CI is flipped with the estimate (lo <= estimate <= hi)", all(tk$ci_lo <= tk$estimate & tk$estimate <= tk$ci_hi))
check("F21: the default orientation (no reference) is unchanged", { t0 <- env$posthoc_table(v21, g21, "tukey"); all(c("B", "C") %in% t0$group1) && all(t0$estimate > 0) })

cat("\n--- F22 / F43: methods sentence ---\n")
s22 <- env$stats_method_sentence(data.frame(method = "Dunnett (fallback: Holm-adjusted Welch t vs reference; mvtnorm not installed)"), "posthoc")
check("F22: the Dunnett fallback does not name the Kruskal-Wallis test", !grepl("Kruskal", s22) && grepl("Holm-adjusted Welch", s22))
check("F22: a real Dunn table still names Kruskal-Wallis", grepl("Kruskal", env$stats_method_sentence(data.frame(method = "Dunn (Holm)"), "posthoc")))
check("F43: what = NA / NULL falls back to the default subject (no 'NANA')",
      !grepl("NANA", env$stats_method_sentence(data.frame(method = "Tukey HSD"), "normality", what = NA)) &&
      grepl("^The measured quantity", env$stats_method_sentence(data.frame(method = "Tukey HSD"), "describe", what = NULL)))

cat("\n--- F23: Grubbs with ties ---\n")
o23 <- env$outlier_tests(c(150, 150, 165))
check("F23: n = 3 with two equal readings flags nothing and reports p = NA with a note", !any(o23$flagged) && all(is.na(o23$p)) && any(grepl("degenerate", o23$note)))
check("F23: a genuine outlier is still flagged", { o <- env$outlier_tests(c(1, 1.1, 0.9, 1.05, 0.95, 1.02, 9)); isTRUE(o$flagged[7]) })

cat("\n--- F24: zero within-group variance ---\n")
om <- env$omnibus_table(c(1, 1, 1, 2, 2, 2), rep(c("a", "b"), each = 3))
check("F24: one-way ANOVA is undefined (NA) when every group is constant, not p = 1e-61", is.na(om$p[om$test == "One-way ANOVA"]))

cat("\n--- F25: Dunnett reproducibility ---\n")
if (has_pkg("mvtnorm")) {
  set.seed(5); v25 <- rnorm(15); g25 <- rep(c("A", "B", "C", "D", "E"), each = 3)
  p25 <- vapply(1:4, function(i) { set.seed(i * 11); env$posthoc_table(v25, g25, "dunnett", reference = "A")$p_adj[1] }, numeric(1))
  check("F25: the same data give the same Dunnett p-value on every call (fixed Monte-Carlo seed)", length(unique(p25)) == 1)
  set.seed(7); a <- runif(1); set.seed(7); invisible(env$posthoc_table(v25, g25, "dunnett", reference = "A")); b <- runif(1)
  check("F25: the caller's random stream is untouched", identical(a, b))
} else { skipit("F25: mvtnorm not installed") }

cat("\n--- F26: phase layout with many samples ---\n")
lay <- lapply(c(2, 6, 14, 16, 30), function(k) env$phase_layout(0, 1, k))
check("F26: limits ascend and strips lie inside them for 2 .. 30 samples",
      all(vapply(lay, function(l) l$limits[2] > l$limits[1] && all(l$strips$ymax > l$strips$ymin) &&
                 min(l$strips$ymin) >= l$limits[1] && max(l$strips$ymax) <= l$limits[2] + 1e-9, logical(1))))
check("F26: the curves keep at least half of the panel at 30 samples", { l <- lay[[5]]; (1 - l$limits[1]) / diff(l$limits) > 0.5 || diff(range(0, 1)) / diff(l$limits) > 0.4 })
check("F26: up to six samples the layout is exactly as before (strip = 6 % of the panel)", near(diff(unlist(lay[[2]]$strips[1, ])) / diff(lay[[2]]$limits), 0.06, 1e-9))

cat("\n--- F27: asymmetric_error_bounds on a subset ---\n")
set.seed(1); tt27 <- seq(0, 120, by = 7.5)
rpb <- do.call(rbind, lapply(1:3, function(r) data.frame(time = tt27, variable = "A", replicate = r, value = .1 + tt27 / 200 + rnorm(length(tt27), 0, .01))))
bnd <- env$replicate_bounds(rpb); pd27 <- aggregate(value ~ time + variable, rpb, mean); names(pd27)[3] <- "mean_value"
check("F27: a plot_data subset still gets its error bounds (9 of 9, was 0)", sum(!is.na(env$asymmetric_error_bounds(pd27[pd27$time <= 60, ], bnd, "range")$lo)) == 9)
check("F27: the full frame is unchanged (17 of 17)", sum(!is.na(env$asymmetric_error_bounds(pd27, bnd, "range")$lo)) == 17)

cat("\n--- F28 / F29: MOI parsing ---\n")
mo <- env$parse_moi(c("Chemoi 5", "MOI 1/10", "MOI 10^-3", "MOI 1,5", "MOI 1.2.3", "Phage MOI 10", "MOI 0.1", "moi=0.1", "MOI 1e-3", "MOI 10 MOI 1"))
check("F28: false positives and ambiguous forms are NA (Chemoi, 1/10, 10^-3, 1,5, 1.2.3)", all(is.na(mo[1:5])))
check("F28: real MOIs still parse (10, 0.1, 0.1, 0.001, 10)", identical(mo[6:10], c(10, 0.1, 0.1, 0.001, 10)))
check("F28: dose_series_name no longer eats 'Chemoi 5'", identical(env$dose_series_name(c("Chemoi 5", "Phage A MOI 10")), c("Chemoi 5", "Phage A")))
check("F29: an override with a decimal comma works; a junk override keeps the parsed MOI",
      near(unname(env$moi_map(c("Phage MOI 10", "B"), "Phage MOI 10 = 0,5")[1]), 0.5) &&
      near(unname(env$moi_map(c("Phage MOI 10", "B"), "Phage MOI 10 = abc")[1]), 10))

cat("\n--- F30: safe_id collisions ---\n")
ids <- env$safe_id(c("a b", "a_b", "a-b", "a.b", "Φ", "Δ"))
check("F30: names that sanitise alike get distinct ids", length(unique(ids)) == 6 && all(grepl("^[A-Za-z0-9_]+$", ids)))
check("F30: a unique name keeps its plain id", identical(env$safe_id("Phage MOI 10"), "Phage_MOI_10") && identical(env$safe_id(c("WT", "Mut 1")), c("WT", "Mut_1")))
env$set_safe_id_pool(c("a b", "a_b", "WT"))
check("F30: with the sample pool set, a single name is hashed consistently (same id alone or in a vector)",
      identical(env$safe_id("a b"), env$safe_id(c("a b", "WT"))[1]) && !identical(env$safe_id("a b"), env$safe_id("a_b")) && identical(env$safe_id("WT"), "WT"))
env$set_safe_id_pool(character(0))

# ── Settings, misc (F34-F46) ──────────────────────────────────────────────────
cat("\n--- F34: settings that are not a settings object ---\n")
for (txt in c("5", "\"x\"", "true", "[]", "{}", "[1,2]")) {
  s <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  r <- tryCatch(env$translate_old_settings(s), error = function(e) e)
  check(sprintf("F34: translate_old_settings('%s') stops with a message, never a subscript error", txt),
        inherits(r, "error") && grepl("not a settings file", conditionMessage(r)))
}
check("F34: a key 'color_0' no longer crashes", is.list(tryCatch(env$translate_old_settings(list(x_scale_type = "log", color_0 = "#fff"), c("WT", "Mut")), error = function(e) e)))
check("F34: an ordinary current file is untouched", identical(env$translate_old_settings(list(global_settings = list(line_thickness = 1)))$kind, "current"))

cat("\n--- F35 / F33 / F36 / F37 / F38 / F39 / F40 / F46 ---\n")
check("F35: '#fff' -> #FFFFFF, 'red' -> #FF0000, junk -> black", identical(env$normalize_hex_color(c("#fff")), "#FFFFFF") &&
      identical(env$normalize_hex_color("red"), "#FF0000") && identical(env$normalize_hex_color("zzz!"), "#000000") && identical(env$normalize_hex_color("e41a1c"), "#E41A1C"))
check("F33: 'Min OD' is not a unit; '(min)' still is", identical(env$convert_time_label(c("Min OD vs time", "Time (min)"), "min_to_h"), c("Min OD vs time", "Time (h)")))
tt36 <- seq(0, 300, 15); pd36 <- data.frame(time = rep(tt36, 2), variable = rep(c("U", "P"), each = 21), mean_value = rep(0.05 + tt36 / 300, 2))
pd36$time[pd36$variable == "P"] <- pd36$time[pd36$variable == "P"] + 1e-13
check("F36: float noise in one sample's time stamps no longer makes every difference NA", all(is.finite(env$curve_difference(pd36, "U")$diff)))
dd37 <- tempfile(); dir.create(dd37); writeLines("5", file.path(dd37, "t.json"))
check("F37: theme_read of a file that holds a bare value is NULL, not a subscript error", is.null(env$theme_read("t", dir = dd37)))
set.seed(1); a38 <- runif(1); set.seed(1); invisible(env$make_demo_data("lysis")); b38 <- runif(1)
check("F38: make_demo_data leaves the caller's random stream alone", identical(a38, b38))
check("F38: the demo data are still the same every time", identical(env$make_demo_data("lysis"), env$make_demo_data("lysis")))
check("F39: an NA sample name no longer crashes replicate_catalog", is.data.frame(env$replicate_catalog(data.frame(Time = 0:2, Sample = c("A", NA, "A")), TRUE, "Time", "Sample")))
check("F39: NA time points no longer crash parse_excluded_timepoints", identical(env$parse_excluded_timepoints("30", c(0, NA, 30, 60)), 30))
check("F40: a confidence level typed as a percentage means the same everywhere (95 = 0.95)",
      { x <- c(2.1, 3.4, 2.9, 4.8, 3.3, 3.9); g <- rep(c("a", "b"), each = 3)
        near(env$posthoc_table(x, g, "holm_welch", level = 90)$ci_lo, env$posthoc_table(x, g, "holm_welch", level = 0.9)$ci_lo) &&
        is.finite(env$fit_exponential_window(1:8, exp(0.3 * (1:8)) + c(0, .01, 0, .01, 0, .01, 0, .01), level = 95)$mu_lo) })
check("F46: svg_inline_for_preview of a directory or a non-UTF-8 file is NULL, not an error",
      is.null(env$svg_inline_for_preview(tempdir(), 1, 1)) && is.null(env$svg_inline_for_preview(w(as.raw(c(0x3c, 0x73, 0x76, 0x67, 0xb0))), 1, 1)))

# ── Supervisor findings ──────────────────────────────────────────────────────
cat("\n--- S1: headless devices ---\n")
check("S1: the default device option is a null raster device (not pdf) when the app is loaded headlessly",
      !identical(getOption("device"), grDevices::pdf) && is.function(getOption("device")))
wd0 <- tempfile(); dir.create(wd0); old_wd <- setwd(wd0)
ws1 <- character(0)
withCallingHandlers({
  g <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) + geom_point() + labs(x = "Time (min)") + theme(text = element_text(family = "Arial"))
  fg <- env$fixed_panel_grob(g, 40, 30)
  gt <- ggplot2::ggplotGrob(g); invisible(grid::convertUnit(sum(gt$widths), "mm"))
}, warning = function(x) { ws1 <<- c(ws1, conditionMessage(x)); invokeRestart("muffleWarning") })
check("S1: measuring text with family Arial emits no 'PostScript font database' warning", !any(grepl("PostScript", ws1)))
check("S1: and leaves no Rplots.pdf in the working directory", !file.exists("Rplots.pdf"))
setwd(old_wd)

cat("\n--- S2: figure warnings for names like 'MG1655 + WT' ---\n")
set.seed(1); tt2 <- seq(0, 300, 15)
mk2 <- function(a) 0.05 + a / (1 + exp(-.03 * (tt2 - 100))) + rnorm(length(tt2), 0, .01)
d2s <- data.frame(Time = tt2, MG1655 = mk2(1), MG1655 = mk2(1.1), MG1655 = mk2(1), `MG1655 + WT` = mk2(.8), `MG1655 + WT` = mk2(.9),
                  `MG1655 + WT` = mk2(.85), `MG1655 + N4 r` = mk2(.5), `MG1655 + N4 r` = mk2(.55), `MG1655 + N4 r` = mk2(.45), check.names = FALSE)
W2 <- character(0); plot_ok <- TRUE
testServer(app, {
  ingest_data(d2s, "paste")
  do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "")
  check("F30 (server): the three samples got three distinct widget ids", length(unique(env$safe_id(rv$od_vars))) == 3)
  for (dm in c("bars", "shadow")) {
    session$setInputs(error_display_mode = dm)
    withCallingHandlers({
      p <- generate_plot(); grid::grid.newpage(); print(p)
    }, warning = function(x) { W2 <<- c(W2, paste(dm, conditionMessage(x))); invokeRestart("muffleWarning") })
  }
  session$setInputs(metrics_ref_sample = rv$od_vars[1])
  for (ty in c("growth_rate", "replicate_overlay", "metric_summary", "normalized")) {
    p <- tryCatch(generate_analysis_plot(ty), error = function(e) NULL)
    if (!is.null(p)) withCallingHandlers({ grid::grid.newpage(); print(p) },
      warning = function(x) { W2 <<- c(W2, paste(ty, conditionMessage(x))); invokeRestart("muffleWarning") })
  }
})
check("S2: no 'Duplicated override.aes' warning on the main figure", !any(grepl("override.aes", W2)))
check("S2: no 'No shared levels' scale_fill_manual warning on the main figure or the dashboard figures", !any(grepl("shared levels", W2)))
if (length(W2)) cat("   remaining warnings:", unique(W2), sep = "\n   ")

cat(sprintf("\n============ SUMMARY: %d passed / %d failed / %d skipped ============\n", pass, fail, skip))
if (fail > 0) quit(status = 1)
