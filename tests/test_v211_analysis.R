# ============================================================================
# v2.11 analysis-figure / statistics / dashboard suite — tests the REAL app:
#  - helpers: parse_moi / moi_map, compute_growth_rate_curves (recovers a known
#    exponential rate), compute_normalized_curves, compute_replicate_metric_rows
#    (+ reference-normalised columns), compare_replicate_metric (matches
#    t.test / aov / TukeyHSD, Holm, Hedges' g, low-n warnings)
#  - server: every analysis figure renders a real figure with the demo data,
#    the metric summary exports at journal size with 6-8 pt text, the stats
#    tables and the dashboard outputs populate
# Set KC_RENDER_DIR=<dir> to also save a PNG of every analysis figure.
# Run from the repository root:  Rscript tests/test_v211_analysis.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root")

pass <- 0L; fail <- 0L; skip <- 0L
check <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
near <- function(a, b, tol = 1e-6) is.finite(a) && abs(a - b) < tol
quiet <- function(expr) suppressWarnings(suppressMessages(expr))
plot_error <- function(p) {
  lab <- tryCatch(p$layers[[1]]$aes_params$label, error = function(e) NULL)
  if (is.character(lab) && any(grepl("^(Plot error|Figure not available)", lab))) lab else NULL
}
real_plot <- function(desc, p) {
  err <- plot_error(p); if (!is.null(err)) cat("   ->", err, "\n")
  check(desc, inherits(p, "ggplot") && is.null(err))
}

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.11.0 or later", package_version(env$APP_VERSION) >= "2.11.0")
render_dir <- Sys.getenv("KC_RENDER_DIR", "")
if (nzchar(render_dir)) dir.create(render_dir, showWarnings = FALSE, recursive = TRUE)

cat("\n--- MOI parsing ---\n")
check("parse_moi reads 'Phage MOI 10' and 'MOI 0.1'",
      isTRUE(all.equal(env$parse_moi(c("Phage MOI 10", "Phage MOI 0.1", "Uninfected")), c(10, 0.1, NA))))
check("parse_moi handles '=', ':', '_' and exponents",
      isTRUE(all.equal(env$parse_moi(c("MOI=5", "moi: 2.5", "T4_MOI_1e-2")), c(5, 2.5, 0.01))))
mm <- env$moi_map(c("A", "B", "Phage MOI 3"), "A = 10; B=0.5")
check("moi_map: override by 'sample = MOI' lines wins, names still parsed",
      isTRUE(all.equal(unname(mm), c(10, 0.5, 3))) && identical(names(mm), c("A", "B", "Phage MOI 3")))
check("moi_map ignores overrides for unknown samples", is.na(env$moi_map("A", "Z = 4")[["A"]]))

cat("\n--- growth-rate curves recover a known exponential rate ---\n")
tt  <- seq(0, 120, by = 10)
rep_pd <- do.call(rbind, lapply(1:3, function(r)
  data.frame(time = tt, variable = "S", replicate = r, value = 0.05 * exp(0.02 * tt), stringsAsFactors = FALSE)))
gr <- env$compute_growth_rate_curves(rep_pd, window = 5)
check("growth rate: one row per interior time point", nrow(gr) == length(tt) - 4)   # window 5 -> 2 lost each end
check("growth rate: mu(t) == 0.02 everywhere on an exact exponential", all(abs(gr$mu_mean - 0.02) < 1e-9))
check("growth rate: SD across identical replicates is 0, n = 3", all(gr$mu_sd < 1e-12) && all(gr$n == 3))
rep_pd2 <- rep_pd; rep_pd2$value[rep_pd2$replicate == 1 & rep_pd2$time == 50] <- -0.01
gr2 <- env$compute_growth_rate_curves(rep_pd2, window = 5)
check("growth rate: a non-positive OD removes every window touching it (n drops to 2 there)",
      all(gr2$n[gr2$time %in% c(30, 40, 50, 60, 70)] == 2) && all(gr2$n[!gr2$time %in% c(30, 40, 50, 60, 70)] == 3))
check("growth rate: NULL on empty input", is.null(env$compute_growth_rate_curves(rep_pd[0, ])))

cat("\n--- normalized curves ---\n")
pd <- data.frame(time = rep(c(0, 10, 20), 2), variable = rep(c("ref", "inf"), each = 3),
                 mean_value = c(0.1, 0.2, 0.4, 0.1, 0.1, 0.05), stringsAsFactors = FALSE)
nd <- env$compute_normalized_curves(pd, "ref", "reference")
check("normalized (reference): ref is 1 everywhere, infected = inf / ref",
      all(nd$ratio[nd$variable == "ref"] == 1) &&
      isTRUE(all.equal(nd$ratio[nd$variable == "inf"], c(1, 0.5, 0.125))))
nd0 <- env$compute_normalized_curves(pd, NULL, "t0")
check("normalized (t0): each curve divided by its own first value",
      isTRUE(all.equal(nd0$ratio[nd0$variable == "ref"], c(1, 2, 4))))
check("normalized (reference) is NULL without a valid reference", is.null(env$compute_normalized_curves(pd, "", "reference")))

cat("\n--- per-replicate rows and reference-normalised metrics ---\n")
demo <- env$make_demo_data()
long <- do.call(rbind, lapply(unique(colnames(demo)[-1]), function(vn) {
  idx <- which(colnames(demo) == vn)
  do.call(rbind, lapply(seq_along(idx), function(r)
    data.frame(time = demo$Time, variable = vn, replicate = r, value = demo[[idx[r]]], stringsAsFactors = FALSE)))
}))
rows <- env$compute_replicate_metric_rows(long, ref_sample = "Uninfected")
check("rows: 3 samples x 3 replicates = 9 rows", nrow(rows) == 9)
check("rows: relative columns present with a reference",
      all(c("relative_growth", "infection_strength", "relative_mu_max", "relative_max_od") %in% names(rows)))
ref_auc <- mean(rows$auc[rows$sample == "Uninfected"])
check("rows: infection_strength = 1 - auc / mean(reference auc)",
      isTRUE(all.equal(rows$infection_strength, 1 - rows$auc / ref_auc)))
check("rows: reference replicates average to infection strength 0",
      near(mean(rows$infection_strength[rows$sample == "Uninfected"]), 0))
rows0 <- env$compute_replicate_metric_rows(long)
check("rows: no relative columns without a reference", !"infection_strength" %in% names(rows0))
summ_old <- env$compute_replicate_metrics(long)
check("compute_replicate_metrics still returns one row per sample with *_mean / *_sd columns",
      nrow(summ_old) == 3 && all(c("lysis_time_mean", "lysis_time_sd", "n_replicates") %in% names(summ_old)) &&
      !"infection_strength_mean" %in% names(summ_old))

cat("\n--- replicate statistics ---\n")
st <- env$compare_replicate_metric(rows, "max_od", ref_sample = "Uninfected")
check("stats: with a reference, one row per non-reference sample", nrow(st$pairwise) == 2 && all(st$pairwise$versus == "Uninfected"))
# lysis_time is NA for the uninfected curves -> reference excluded -> falls back to all pairs, and says so
st2 <- env$compare_replicate_metric(rows, "lysis_time", ref_sample = "Uninfected")
check("stats: a reference with no usable values falls back to all pairs with a warning",
      nrow(st2$pairwise) == 1 && any(grepl("all pairs", st2$warnings)))
a <- rows$od_drop[rows$sample == "Phage MOI 10"]; b <- rows$od_drop[rows$sample == "Phage MOI 0.1"]
st3 <- env$compare_replicate_metric(rows, "od_drop")
r3  <- st3$pairwise[st3$pairwise$sample == "Phage MOI 10" & st3$pairwise$versus == "Phage MOI 0.1", ]
if (nrow(r3) == 0) r3 <- st3$pairwise[st3$pairwise$sample == "Phage MOI 0.1" & st3$pairwise$versus == "Phage MOI 10", ]
tt3 <- t.test(a, b)
check("stats: Welch p matches t.test()", near(r3$p, tt3$p.value, 1e-12))
check("stats: CI matches t.test() (up to sign/order)",
      near(abs(r3$ci_high - r3$ci_low), abs(diff(tt3$conf.int)), 1e-9))
g_manual <- (mean(a) - mean(b)) / sqrt(((2) * var(a) + 2 * var(b)) / 4) * (1 - 3 / (4 * 6 - 9))
check("stats: Hedges' g matches the hand formula (up to sign)", near(abs(r3$hedges_g), abs(g_manual), 1e-9))
check("stats: Holm adjustment equals p.adjust(..., 'holm')",
      isTRUE(all.equal(st3$pairwise$p_adj_holm, p.adjust(st3$pairwise$p, "holm"))))
check("stats: n = 3 everywhere triggers the low-power warning",
      any(grepl("n < 3", st3$warnings)) == FALSE)   # n is exactly 3 -> no warning
rows_small <- rows[!(rows$sample == "Phage MOI 10" & rows$replicate == 3), ]
st_small <- env$compare_replicate_metric(rows_small, "od_drop")
check("stats: a group with n = 2 triggers the low-power warning", any(grepl("n < 3", st_small$warnings)))
st_one <- env$compare_replicate_metric(rows[rows$replicate == 1 | rows$sample == "Uninfected", ], "od_drop")
check("stats: groups with < 2 values are excluded and reported", any(grepl("Excluded", st_one$warnings)))
# three groups -> ANOVA + Tukey, compare with aov()
rows3 <- rows; rows3$max_od <- rows3$max_od   # max_od is finite for all 9 replicates
st_an <- env$compare_replicate_metric(rows3, "max_od")
fit   <- aov(max_od ~ factor(sample), data = rows3)
check("stats: one-way ANOVA F and p match aov()",
      !is.null(st_an$omnibus) && near(st_an$omnibus$statistic, summary(fit)[[1]][["F value"]][1], 1e-9) &&
      near(st_an$omnibus$p, summary(fit)[[1]][["Pr(>F)"]][1], 1e-12))
tk <- TukeyHSD(aov(max_od ~ sample, data = transform(rows3, sample = factor(sample))))$sample
check("stats: Tukey HSD table matches TukeyHSD()",
      !is.null(st_an$posthoc) && nrow(st_an$posthoc) == 3 &&
      isTRUE(all.equal(sort(st_an$posthoc$p_adj_tukey), sort(unname(tk[, "p adj"])))))
st_np <- env$compare_replicate_metric(rows3, "max_od", method = "nonparametric")
kw <- kruskal.test(max_od ~ factor(sample), data = rows3)
check("stats: nonparametric path = Mann-Whitney pairs + Kruskal-Wallis omnibus",
      all(st_np$pairwise$test == "Mann-Whitney") && near(st_np$omnibus$p, kw$p.value, 1e-12))
check("stats: unknown metric returns an empty result, not an error",
      is.null(env$compare_replicate_metric(rows, "nope")$pairwise))
check("sig_stars thresholds", identical(env$sig_stars(c(0.0001, 0.005, 0.03, 0.2, NA)), c("***", "**", "*", "ns", "")))
check("time unit from label", identical(env$time_unit_from_label("Time (minutes)"), "min") &&
        identical(env$time_unit_from_label("Time (h)"), "h"))

cat("\n--- v2.12.1: demo scenarios ---\n")
check("make_demo_data() with no argument is identical to make_demo_data('lysis')",
      identical(env$make_demo_data(), env$make_demo_data("lysis")))
for (sc in c("lysis", "moi_series", "resistance", "hours", "two_phages")) {
  d <- env$make_demo_data(sc)
  cn <- colnames(d)[-1]
  check(sprintf("demo '%s': data.frame with Time first", sc),
        is.data.frame(d) && identical(colnames(d)[1], "Time"))
  check(sprintf("demo '%s': exactly 3 replicate columns per sample name", sc),
        length(cn) > 0 && all(table(cn) == 3))
  check(sprintf("demo '%s': Time finite; every OD value finite and > 0", sc),
        all(is.finite(d$Time)) && all(is.finite(as.matrix(d[-1]))) && all(as.matrix(d[-1]) > 0))
  if (sc == "moi_series") {
    moi <- env$parse_moi(unique(cn))
    check("demo 'moi_series': parse_moi gives 5 finite MOIs (Uninfected has none)",
          sum(is.finite(moi)) == 5 && length(unique(moi[is.finite(moi)])) == 5)
  }
  if (sc == "hours")
    check("demo 'hours': Time runs 0-8 h", min(d$Time) == 0 && max(d$Time) == 8)
}

# ── Through the real server ──────────────────────────────────────────────────
out_dir <- tempfile("v211_"); dir.create(out_dir)
of <- function(x) file.path(out_dir, x)
types <- c("metric_summary", "growth_rate", "normalized", "replicate_overlay", "dose_response", "heatmap")

testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "",
                    metrics_ref_sample = "Uninfected")

  cat("\n--- main figure still byte-identical through the shared theme ---\n")
  p_main <- generate_plot()
  real_plot("main figure renders", p_main)
  check("figure_theme_layers() returns a list of theme layers",
        is.list(figure_theme_layers()) && length(figure_theme_layers()) >= 3)

  cat("\n--- every analysis figure renders with the demo data ---\n")
  for (ty in types) {
    p <- generate_analysis_plot(ty)
    real_plot(sprintf("analysis figure '%s' renders", ty), p)
    if (nzchar(render_dir))
      quiet(ggsave(file.path(render_dir, paste0("analysis_", ty, ".png")), p, device = env$export_device("png"),
                   width = 89, height = 65, units = "mm", dpi = 300, bg = "white"))
  }
  session$setInputs(metrics_ref_sample = "")
  p_norm0 <- generate_analysis_plot("normalized")
  check("normalized figure explains itself without a reference", !is.null(plot_error(p_norm0)))
  session$setInputs(analysis_norm_mode = "t0")
  real_plot("normalized vs t0 renders without a reference", generate_analysis_plot("normalized"))
  session$setInputs(analysis_norm_mode = "reference", metrics_ref_sample = "Uninfected")

  cat("\n--- journal preset applies to the analysis figure too ---\n")
  pd <- prepare_plot_data()
  do.call(session$setInputs, env$journal_preset_settings(89, 65, "a", diff(range(pd$time))))
  # max_od is finite for the uninfected reference, so marks vs reference can be drawn
  session$setInputs(analysis_panel_label = "b", analysis_metrics = c("lysis_time", "max_od"))
  p_ms <- generate_analysis_plot("metric_summary")
  real_plot("metric summary renders under the journal preset", p_ms)
  svg_f <- of("metric_summary_89x65.svg"); sz <- export_size_in()
  quiet(ggsave(svg_f, p_ms, device = env$export_device("svg"), width = sz[1], height = sz[2], units = "in", bg = "white"))
  x <- readLines(svg_f); txt <- grep("<text", x, value = TRUE)
  sizes <- as.numeric(sub('.*font-size: ([0-9.]+)px.*', '\\1', txt))
  check("analysis SVG is 89 mm wide", grepl("width='252.28pt'", grep("<svg", x, value = TRUE)[1]))
  check("analysis SVG text is Arial 6-8 pt, no textLength",
        all(grepl('font-family: "Arial"', txt)) && all(sizes >= 6 - 1e-6 & sizes <= 8 + 1e-6) && !any(grepl("textLength", txt)))
  check("analysis SVG carries panel label 'b' in bold 8 pt",
        any(grepl(">b</text>", txt) & grepl("font-size: 8.00px", txt) & grepl("font-weight: bold", txt)))
  check("significance marks drawn vs reference (stars or ns present)",
        any(grepl(">(\\*+|ns)</text>", txt)))
  if (nzchar(render_dir))
    quiet(ggsave(file.path(render_dir, "analysis_metric_summary_journal.png"), p_ms, device = env$export_device("png"),
                 width = sz[1], height = sz[2], units = "in", dpi = 300, bg = "white"))
  for (ty in setdiff(types, "metric_summary")) {
    p <- generate_analysis_plot(ty)
    real_plot(sprintf("'%s' renders under the journal preset", ty), p)
    if (nzchar(render_dir))
      quiet(ggsave(file.path(render_dir, paste0("analysis_", ty, "_journal.png")), p, device = env$export_device("png"),
                   width = sz[1], height = sz[2], units = "in", dpi = 300, bg = "white"))
  }

  cat("\n--- v2.12: error display options and curve landmarks ---\n")
  real_plot("curve landmarks figure renders", generate_analysis_plot("landmarks"))
  layer_names <- function(p) vapply(p$layers, function(l) class(l$geom)[1], "")
  n_pts_before <- sum(layer_names(generate_plot()) == "GeomPoint")
  session$setInputs(show_replicate_points = TRUE)
  p_rp <- generate_plot(); real_plot("replicate points option renders", p_rp)
  check("replicate points add a GeomPoint layer behind the mean", sum(layer_names(p_rp) == "GeomPoint") == n_pts_before + 1)
  session$setInputs(show_replicate_points = FALSE, error_bar_color = "black", error_cap_relative = TRUE, error_bar_width = 1.5)
  p_bk <- generate_plot(); real_plot("black bars with relative caps render", p_bk)
  eb <- p_bk$layers[[which(layer_names(p_bk) == "GeomErrorbar")[1]]]
  check("black error bars carry a fixed colour, not the sample colour", identical(eb$aes_params$colour, "black"))
  session$setInputs(error_bar_color = "sample", error_cap_relative = FALSE, error_bar_width = 1)
  session$setInputs(error_type = "sd", error_multiplier = 15)  # SD x 15 crosses zero on the log axis (x 5 does not for the demo data)
  p_fl <- generate_plot(); real_plot("log-floor bars render", p_fl)
  eb_d <- layer_data(p_fl, which(layer_names(p_fl) == "GeomErrorbar")[1])
  check("bars that cross zero are floored at the axis minimum (10^-2), not dropped",
        nrow(eb_d) > 0 && all(eb_d$ymin >= log10(0.01) - 1e-9) && any(abs(eb_d$ymin - log10(0.01)) < 1e-9))
  session$setInputs(error_log_floor = FALSE)
  eb_d2 <- layer_data(generate_plot(), which(layer_names(generate_plot()) == "GeomErrorbar")[1])
  check("with the floor off, those bars are dropped (old behaviour)",
        sum(is.finite(eb_d2$ymin)) < sum(is.finite(eb_d$ymin)))
  session$setInputs(error_log_floor = TRUE, error_multiplier = 1, error_type = "sem")
  session$setInputs(legend_order = "selected")
  p_ord <- generate_plot()
  check("sample order 'selected' keeps the file order in the main figure (Uninfected first)",
        identical(levels(layer_data(p_ord, 1)$group %>% factor()), c("1", "2", "3")) &&
        identical(as.character(levels(p_ord$data$variable))[1], "Uninfected"))
  ms <- generate_analysis_plot("metric_summary")
  check("analysis figures follow the same order", identical(levels(ms$data$sample)[1], "Uninfected"))
  session$setInputs(legend_order = "alpha")
  check("journal preset switches the order to 'selected' and relative caps",
        identical(env$journal_preset_settings(89, 65)$legend_order, "selected") &&
        isTRUE(env$journal_preset_settings(89, 65)$error_cap_relative))
  session$setInputs(error_type = "none")
  ms0 <- generate_analysis_plot("metric_summary")
  check("error type 'none' draws no whiskers on the metric summary",
        all(is.na(layer_data(ms0, which(layer_names(ms0) == "GeomErrorbar")[1])$ymin)))
  session$setInputs(error_type = "sem")

  cat("\n--- statistics outputs ---\n")
  st <- analysis_stats_current()
  check("stats use the first selected metric (lysis_time)", identical(st$metric, "lysis_time"))
  html <- output$analysis_stats_table
  check("pairwise table renders as HTML with the sample names", grepl("Phage MOI 10", html) && grepl("Holm|p_adj_holm", html))
  session$setInputs(analysis_type = "dose_response", analysis_dose_metric = "od_drop")
  check("dose-response switches the tested metric", identical(analysis_stats_current()$metric, "od_drop"))
  session$setInputs(analysis_type = "metric_summary")

  cat("\n--- dashboard outputs ---\n")
  cards <- output$dash_cards$html
  check("dashboard cards render (samples, replicates, figure size)",
        grepl("Samples", cards) && grepl("Replicates", cards) && grepl("89", cards))
  tbl <- output$dash_metrics_table
  check("dashboard metrics table lists every sample with +/- SD on lysis onset",
        grepl("Uninfected", tbl) && grepl("Phage MOI 0.1", tbl) && grepl("±", tbl))
  nb <- output$dash_notebook$html
  check("dashboard notebook shows the default experiment type (only non-empty fields)",
        grepl("Phage Infection", nb) && !grepl("Host", nb))
  session$setInputs(nb_host_strain = "E. coli K-12", nb_moi = "10")
  check("dashboard notebook shows filled fields", grepl("E. coli K-12", output$dash_notebook$html))
  check("dashboard normalized-panel title follows the reference", grepl("Normalized", output$dash_fig_norm_title))

  cat("\n--- v2.11.1: analysis figures and replicate statistics in the reports ---\n")
  check("APP_DEFAULTS includes the report option, on by default", isTRUE(env$APP_DEFAULTS$report_include_analysis))
  n_img   <- function(h) lengths(regmatches(h, gregexpr("<img ", h, fixed = TRUE)))
  no_b64  <- function(h) gsub("base64,[A-Za-z0-9+/=]+", "base64,", h)
  n_pages <- function(f) length(grepRaw("/Type /Page[^s]", readBin(f, "raw", file.size(f)), all = TRUE))
  if (env$has_base64enc && env$has_gridExtra) {
    # pin the timestamp so two builds of the same inputs can be compared
    meta <- report_meta(); meta$generated <- "2026-10-19 00:00:00"
    session$setInputs(report_include_analysis = TRUE, metrics_ref_sample = "Uninfected",
                      analysis_type = "metric_summary", analysis_metrics = c("lysis_time", "od_drop"))
    html_on  <- html_report_string(meta)
    # the v2.11.0 report for the same inputs = the builder without the analysis argument
    html_old <- env$build_html_report(meta, report_png_b64(generate_plot()), stats_table_export(),
                                      curve_metrics_current())
    check("HTML report (on): 'Analysis figures' and 'Replicate statistics' headings",
          grepl("<h2>Analysis figures</h2>", html_on, fixed = TRUE) &&
          grepl("<h2>Replicate statistics</h2>", html_on, fixed = TRUE))
    check("HTML report (on): at least two more <img> elements than before (demo data: +4)",
          n_img(html_on) >= n_img(html_old) + 2)
    check("HTML report (on): every analysis figure rendered (no 'Figure not available')",
          !grepl("Figure not available", html_on, fixed = TRUE))
    check("HTML report (on): metric summary, growth rate, normalized (reference set) and dose-response (MOI in names)",
          all(vapply(c("Metric summary", "Specific growth rate vs time", "Normalized OD vs time", "MOI dose-response"),
                     grepl, logical(1), x = html_on, fixed = TRUE)) &&
          !grepl("<h3>Replicate overlay</h3>", html_on, fixed = TRUE))
    check("HTML report (on): pairwise table, the lysis_time warnings and the definitions paragraph",
          grepl("Pairwise comparisons", html_on, fixed = TRUE) && grepl("<th>p_adj_holm</th>", html_on, fixed = TRUE) &&
          grepl("all pairs are compared instead", html_on, fixed = TRUE) &&
          grepl("Hedges' g is the standardized mean difference", html_on, fixed = TRUE) &&
          grepl("Tukey", html_on, fixed = TRUE) && grepl("Kruskal-Wallis", html_on, fixed = TRUE))
    check("HTML report (on): the analysis section sits between the main figure and the summary statistics",
          regexpr("<h2>Figure</h2>", html_on, fixed = TRUE) < regexpr("<h2>Analysis figures</h2>", html_on, fixed = TRUE) &&
          regexpr("<h2>Replicate statistics</h2>", html_on, fixed = TRUE) < regexpr("<h2>Summary statistics</h2>", html_on, fixed = TRUE))
    session$setInputs(analysis_metrics = c("max_od", "od_drop"))   # a metric the reference has -> ANOVA + Tukey
    html_an <- html_report_string(meta)
    check("HTML report (on): omnibus and Tukey post-hoc tables when 3 groups are testable",
          grepl("Omnibus test", html_an, fixed = TRUE) && grepl("One-way ANOVA", html_an, fixed = TRUE) &&
          grepl("Post-hoc comparisons (Tukey HSD)", html_an, fixed = TRUE) && grepl("<th>p_adj_tukey</th>", html_an, fixed = TRUE))
    session$setInputs(analysis_metrics = c("lysis_time", "od_drop"), metrics_ref_sample = "")
    html_noref <- html_report_string(meta)
    check("HTML report (on): replicate overlay replaces the normalized figure without a reference sample",
          grepl("<h3>Replicate overlay</h3>", html_noref, fixed = TRUE) && !grepl("<h3>Normalized OD", html_noref, fixed = TRUE))
    session$setInputs(metrics_ref_sample = "Uninfected")
    # The PDF report draws on grDevices::pdf, whose font database has no "Arial"
    # (set above by the journal preset): a pre-existing limit of the PDF report,
    # not of the analysis pages, so build the PDFs with the default family.
    session$setInputs(font_family = "sans")
    pdf_on <- of("report_on.pdf"); pdf_report_file(pdf_on, meta)
    session$setInputs(report_include_analysis = FALSE)
    html_off <- html_report_string(meta)
    pdf_off  <- of("report_off.pdf"); pdf_report_file(pdf_off, meta)
    check("HTML report (off): identical to the v2.11 output for the same inputs",
          identical(no_b64(html_off), no_b64(html_old)))
    check("HTML report (off): no analysis section, one <img>",
          !grepl("Analysis figures", html_off, fixed = TRUE) && n_img(html_off) == 1)
    check("PDF report: both build and start with %PDF",
          identical(readBin(pdf_on, "raw", 4), charToRaw("%PDF")) && identical(readBin(pdf_off, "raw", 4), charToRaw("%PDF")))
    cat(sprintf("   -> PDF pages: on = %d, off = %d\n", n_pages(pdf_on), n_pages(pdf_off)))
    check("PDF report (on): more pages than without the option (4 figure pages + statistics)",
          n_pages(pdf_on) >= n_pages(pdf_off) + 5)
    session$setInputs(report_include_analysis = TRUE)
  } else skip <<- skip + 1L
})

cat("\n--- v2.12.1: MOI-series demo through the server ---\n")
testServer(app, {
  ingest_data(env$make_demo_data("moi_series"), "demo")
  do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "",
                    metrics_ref_sample = "Uninfected", analysis_dose_metric = "lysis_time")
  real_plot("moi_series demo: main figure renders", generate_plot())
  p_dr <- generate_analysis_plot("dose_response")
  real_plot("moi_series demo: dose-response renders a real figure", p_dr)
  check("moi_series demo: dose-response data has 5 distinct MOI values",
        length(unique(p_dr$data$moi)) == 5)
})

cat("\n--- v2.12.2: auto legend corner ---\n")
set.seed(7)
# a cloud in one quadrant, with one anchor point in each of two other corners to pin the axis ranges
bx <- c(0, 10, runif(30, 7, 9.5)); by <- c(0, 10, runif(30, 0.5, 3))     # cloud bottom-right; anchors BL, TR
check("auto_legend_corner: data in the bottom-right -> top-left",
      identical(env$auto_legend_corner(bx, by), "top-left"))
tx <- c(0, 10, runif(30, 0.5, 3)); ty <- c(0, 10, runif(30, 7, 9.5))     # cloud top-left; anchors BL, TR
check("auto_legend_corner: data in the top-left -> bottom-right (emptiest)",
      identical(env$auto_legend_corner(tx, ty), "bottom-right"))
ux <- c(0, 10, runif(30, 0.5, 3)); uy <- c(0, 0, runif(30, 7, 9.5))      # cloud top-left; anchors BL, BR
check("auto_legend_corner: only the top-left populated (anchors BL, BR) -> top-right",
      identical(env$auto_legend_corner(ux, uy), "top-right"))
check("auto_legend_corner: empty vector -> top-left", identical(env$auto_legend_corner(numeric(0), numeric(0)), "top-left"))
check("auto_legend_corner: ties go top-left, then top-right, then the bottom corners",
      identical(env$auto_legend_corner(c(0, 10), c(0, 0)), "top-left") &&
      identical(env$auto_legend_corner(c(0, 0, 10), c(0, 10, 0)), "top-right") &&
      identical(env$auto_legend_corner(c(0, 0, 10, 10, 0, 10), c(10, 9, 10, 9, 0, 0)), "bottom-left"))
# log axis: the midpoint is taken on log10(y): 0.01..1 splits at 0.1, not at 0.505
lx <- c(0, 0, 0, 10); ly <- c(0.01, 0.02, 0.5, 1)
check("auto_legend_corner: log_y splits at the log midpoint",
      identical(env$auto_legend_corner(lx, ly, log_y = TRUE), "bottom-right") &&
      identical(env$auto_legend_corner(lx, ly, log_y = FALSE), "top-left"))
# corner-box variant (what the figures use): a rising curve leaves the top-left corner free even though
# that quadrant holds points, and the bottom-right band of a lysis trough is not mistaken for empty
cx <- seq(0, 10, length.out = 40); cy <- 10^(-2 + 2 * cx / 10)                 # log-linear rise
check("auto_legend_corner (corner box): a rising log curve -> top-left; box = NULL is the quadrant rule",
      identical(env$auto_legend_corner(cx, cy, TRUE, box = env$inside_legend_box(3)), "top-left") &&
      identical(env$auto_legend_corner(c(0, 10), c(0, 0), box = NULL), "top-left"))
check("auto_legend_corner (corner box): empty vector -> top-left",
      identical(env$auto_legend_corner(numeric(0), numeric(0), box = env$inside_legend_box(3)), "top-left"))
check("journal preset uses inside-auto", identical(env$journal_preset_settings(89, 65)$legend_position, "inside-auto"))
check("inside legends use a compact key",
      isTRUE(all.equal(as.numeric(env$inside_legend_theme("top-left")$legend.key.spacing.y), 1)) &&
      identical(as.numeric(env$inside_legend_theme("top-left")$legend.margin), c(1, 3, 1, 3)))

testServer(app, {
  ingest_data(env$make_demo_data("lysis"), "demo")
  do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "",
                    metrics_ref_sample = "Uninfected", legend_position = "inside-auto")
  check("inside-auto: main figure (3 samples) gets an inside legend",
        identical(generate_plot()$theme$legend.position, "inside"))
  check("inside-auto: growth-rate figure (3 samples) gets an inside legend",
        identical(generate_analysis_plot("growth_rate")$theme$legend.position, "inside"))
  th <- generate_plot()$theme
  check("inside-auto: justification is a corner and the key is compact",
        all(th$legend.justification.inside %in% c(0, 1)) && isTRUE(all.equal(as.numeric(th$legend.key.spacing.y), 1)))
  session$setInputs(legend_position = "right")
  check("default legend position stays outside (right)", identical(generate_plot()$theme$legend.position, "right"))
})
testServer(app, {
  ingest_data(env$make_demo_data("moi_series"), "demo")
  do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "",
                    metrics_ref_sample = "Uninfected", legend_position = "inside-auto")
  check("inside-auto: 6 samples fall back to a right legend in the main figure",
        identical(generate_plot()$theme$legend.position, "right"))
  check("inside-auto: 6 samples fall back to a right legend in the analysis figures",
        identical(generate_analysis_plot("replicate_overlay")$theme$legend.position, "right"))
  # metric summary: 5+ samples -> angled labels instead of staggered rows
  ms <- generate_analysis_plot("metric_summary")
  xg <- ggplot_build(ms)$layout$panel_scales_x[[1]]$guide
  check("metric summary with 6 samples uses guide_axis(angle = 45)", isTRUE(xg$params$angle == 45))
  check("metric facet labels are the short ones",
        all(c(paste0("OD drop (", input$y_axis_label, ")"), "Lysis onset (min)") %in% levels(ms$data$metric)) &&
        env$metric_axis_label("lysis_rate", "min", "OD") == "Lysis rate (OD per min)" &&
        env$metric_axis_label("t_half_max_decline", "min", "OD") == "Time to half peak (min)")
  # dose-response axis labels read as plain numbers
  p_dr <- generate_analysis_plot("dose_response")
  sx <- ggplot_build(p_dr)$layout$panel_scales_x[[1]]
  labs_x <- sx$get_labels(sx$get_breaks())
  labs_x <- labs_x[!is.na(labs_x) & nzchar(labs_x)]
  check("dose-response x labels are plain numbers (0.01 ... 100), not 1e-02",
        !any(grepl("e", labs_x)) && all(c("0.01", "0.1", "1", "10", "100") %in% labs_x))
  check("dose-response with one phage series has one series name", length(unique(p_dr$data$series)) == 1)
  # a single MOI level -> message, not a degenerate plot
  session$setInputs(selected_samples = c("Uninfected", "Phage MOI 10"))
  e1 <- plot_error(generate_analysis_plot("dose_response"))
  check("dose-response with one MOI level shows the 'needs at least two MOI levels' message",
        !is.null(e1) && grepl("Dose-response needs at least two MOI levels; this selection has one.", gsub("\n", " ", e1), fixed = TRUE))
})
testServer(app, {
  ingest_data(env$make_demo_data("two_phages"), "demo")
  do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "",
                    metrics_ref_sample = "Uninfected", analysis_dose_metric = "lysis_time")
  p2 <- generate_analysis_plot("dose_response"); real_plot("two_phages: dose-response renders", p2)
  check("two_phages: dose-response data has two series (Phage A, Phage B)",
        identical(sort(unique(p2$data$series)), c("Phage A", "Phage B")))
  gn <- vapply(p2$layers, function(l) class(l$geom)[1], "")
  check("two_phages: one connecting line and one end label per series",
        sum(gn == "GeomLine") == 2 && sum(gn == "GeomText") == 2)
})

cat("\n--- v2.12.2: first-peak mode ---\n")
tt_r <- seq(0, 240, by = 30)
od_r <- c(0.05, 0.275, 0.5, 0.275, 0.05, 0.3, 0.55, 0.7, 0.8)   # lyses after t = 60, regrows above the first peak
mg <- env$compute_one_curve_metrics(tt_r, od_r, "R")
mf <- env$compute_one_curve_metrics(tt_r, od_r, "R", peak_mode = "first_local")
check("peak mode global: the peak is the late regrowth maximum (t = 240) and lysis is NA",
      mg$time_max_od == 240 && is.na(mg$lysis_time) && is.na(mg$od_drop))
check("peak mode first_local: peak at t = 60, finite lysis_time and od_drop",
      mf$time_max_od == 60 && is.finite(mf$lysis_time) && near(mf$od_drop, 0.45) && is.finite(mf$t_half_max_decline))
check("peak mode default equals 'global'", identical(env$compute_one_curve_metrics(tt_r, od_r, "R"), mg))
check("first_local falls back to the global maximum when no decline follows",
      env$compute_one_curve_metrics(1:5, c(0.05, 0.1, 0.3, 0.6, 0.9), "M", peak_mode = "first_local")$time_max_od == 5)
lys <- c(0.05, 0.2, 0.5, 0.8, 0.4, 0.1, 0.03)   # the trough is below the inoculum: t = 0 must not count as "the peak"
check("first_local on an ordinary lysis curve equals the global peak",
      identical(env$compute_one_curve_metrics(0:6, lys, "L", peak_mode = "first_local"),
                env$compute_one_curve_metrics(0:6, lys, "L")))
pd_r <- data.frame(time = tt_r, variable = "R", mean_value = od_r)
check("compute_curve_metrics threads peak_mode",
      env$compute_curve_metrics(pd_r)$time_max_od == 240 &&
      env$compute_curve_metrics(pd_r, peak_mode = "first_local")$time_max_od == 60)
rp_r <- data.frame(time = rep(tt_r, 2), variable = "R", replicate = rep(1:2, each = length(tt_r)), value = rep(od_r, 2))
check("replicate rows and summary thread peak_mode",
      all(env$compute_replicate_metric_rows(rp_r, peak_mode = "first_local")$time_max_od == 60) &&
      all(env$compute_replicate_metric_rows(rp_r)$time_max_od == 240) &&
      env$compute_replicate_metrics(rp_r, peak_mode = "first_local")$time_max_od_mean == 60)
testServer(app, {
  ingest_data(env$make_demo_data("resistance"), "demo")
  do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "",
                    metrics_ref_sample = "Uninfected")
  mg <- curve_metrics_current()
  check("resistance demo, global peak: the regrowing sample has no lysis onset",
        is.na(mg$lysis_time[mg$sample == "Phage MOI 1 + resistant"]))
  session$setInputs(metrics_peak_mode = "first_local")
  mf <- curve_metrics_current()
  check("resistance demo, first_local: lysis onset and OD drop are found at the 90 min peak",
        is.finite(mf$lysis_time[mf$sample == "Phage MOI 1 + resistant"]) &&
        mf$time_max_od[mf$sample == "Phage MOI 1 + resistant"] == 90 && mf$time_max_od[mf$sample == "Uninfected"] == 360)
  rr <- replicate_metric_rows_current()
  check("replicate metrics follow the peak mode too",
        all(rr$time_max_od[rr$sample == "Phage MOI 1 + resistant"] <= 105))
  pl <- generate_analysis_plot("landmarks"); real_plot("landmarks figure renders with first_local", pl)
  gl <- vapply(pl$layers, function(l) class(l$geom)[1], "")
  pk <- layer_data(pl, which(gl == "GeomPoint")[1])
  check("landmarks mark the first peak (shape 16 at t = 90 for the regrowth curve)",
        any(pk$shape == 16 & abs(pk$x - 90) < 1e-9))
  check("metrics_peak_mode has an APP_DEFAULTS entry", identical(env$APP_DEFAULTS$metrics_peak_mode, "global"))
})

cat(sprintf("\n============ SUMMARY: %d passed / %d failed / %d skipped ============\n", pass, fail, skip))
if (nzchar(render_dir)) cat("renders in:", render_dir, "\n")
if (fail > 0) quit(status = 1)
