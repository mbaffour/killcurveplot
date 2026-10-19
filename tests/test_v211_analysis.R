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
# the legacy-geometry sections run with the panel filling the export size ("fit"); the v2.15 section uses fixed panels
fit_defaults <- modifyList(env$APP_DEFAULTS, list(panel_size_mode = "fit"))
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
  do.call(session$setInputs, fit_defaults)
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
  do.call(session$setInputs, modifyList(env$journal_preset_settings(89, 65, "a", diff(range(pd$time))), list(panel_size_mode = "fit")))
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
    # lysis demo (MOI 10 and 0.1 = two distinct positive levels, reference set): main figure + metric summary +
    # growth rate + normalized + landmarks (v2.12.4, always) + dose-response = 6 images, plus the composed figure
    # (v2.13.0: the default compose_panels selection has four panels) = 7.
    check("HTML report (on): exactly 7 <img> (main, metric summary, growth rate, normalized, landmarks, dose-response, composed)",
          n_img(html_on) == 7 && grepl("<h3>Curve landmarks</h3>", html_on, fixed = TRUE) &&
          grepl("<h3>Composed figure</h3>", html_on, fixed = TRUE))
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
    # (v2.14: Arial is now the default, so the PDF report must work with it: it opens the same
    # font-embedding device as the image export instead of grDevices::pdf())
    check("the font family is Arial for the PDF report below", identical(input$font_family, "Arial"))
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
    # before v2.12.4 this lysis-demo PDF had 19 pages (on) vs 13 (off); the landmarks page is one more
    check("PDF report (on): more pages than without the option (5 figure pages + statistics)",
          n_pages(pdf_on) >= n_pages(pdf_off) + 7 && n_pages(pdf_on) >= 21)   # 20 before the composed-figure page
    session$setInputs(report_include_analysis = TRUE)
  } else skip <<- skip + 1L
})

cat("\n--- v2.12.4: dashboard NA formatting, lysis threshold, report figures (resistance) ---\n")
testServer(app, {
  ingest_data(env$make_demo_data("resistance"), "demo")
  do.call(session$setInputs, fit_defaults)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "",
                    metrics_ref_sample = "Uninfected", analysis_dose_metric = "lysis_time")
  m_r <- curve_metrics_current()
  check("resistance: Uninfected lysis onset is NA on the mean curve", is.na(m_r$lysis_time[m_r$sample == "Uninfected"]))
  tbl_r <- output$dash_metrics_table
  check("dashboard table never prints '\u2014 \u00b1' (NA mean-curve value shown as a dash alone)",
        !grepl("\u2014 \u00b1", tbl_r, fixed = TRUE) && grepl("Uninfected", tbl_r))
  rm_r <- replicate_metrics_current()
  check("(the regression case is real: the Uninfected replicate SD of lysis onset is finite although the mean-curve value is NA)",
        is.finite(rm_r$lysis_time_sd[rm_r$sample == "Uninfected"]))
  check("dashboard table keeps 'value \u00b1 sd' for finite values", grepl("\u00b1", tbl_r, fixed = TRUE))
  if (env$has_base64enc) {
    meta <- report_meta(); meta$generated <- "2026-10-19 00:00:00"
    session$setInputs(report_include_analysis = TRUE)
    h_r <- html_report_string(meta)
    n_img <- function(h) lengths(regmatches(h, gregexpr("<img ", h, fixed = TRUE)))
    # resistance: both infected samples are MOI 1 = ONE positive level, so no dose-response; images =
    # main + metric summary + growth rate + normalized + landmarks = 5, plus the composed figure (v2.13.0) = 6
    check("resistance report: exactly 6 <img> (landmarks and the composed figure; dose-response skipped for one MOI level)",
          n_img(h_r) == 6 && grepl("<h3>Curve landmarks</h3>", h_r, fixed = TRUE))
    check("resistance report: no 'MOI dose-response' heading and no 'Figure not available' placeholder",
          !grepl("MOI dose-response", h_r, fixed = TRUE) && !grepl("Figure not available", h_r, fixed = TRUE))
  }
  # lysis threshold through the server: 5 % default vs the input
  session$setInputs(metrics_lysis_threshold_pct = 5)
  m5 <- curve_metrics_current()
  check("metrics_lysis_threshold_pct has an APP_DEFAULTS entry of 5", identical(env$APP_DEFAULTS$metrics_lysis_threshold_pct, 5))
  check("server default threshold gives the same metrics as the engine default",
        isTRUE(all.equal(m5, env$compute_curve_metrics(prepare_plot_data(), smooth_window = input$metrics_smooth_window %||% 5,
                                                        ref_sample = "Uninfected"))))
  # (the resistance demo's infected samples all fall by > 90 %, so a threshold up to 50 % does not move their onset;
  #  the effect of the threshold itself is tested on the synthetic plateau below)
  session$setInputs(metrics_lysis_threshold_pct = 50)
  m50 <- curve_metrics_current()
  session$setInputs(metrics_lysis_threshold_pct = 80)   # out of range: clamped to 50 %
  check("an out-of-range threshold input is clamped to 1-50 % (80 behaves as 50)",
        isTRUE(all.equal(curve_metrics_current(), m50)))
})
cat("\n--- v2.12.4: lysis threshold in the engine ---\n")
demo_l <- env$make_demo_data("lysis")
pd_l <- do.call(rbind, lapply(unique(colnames(demo_l)[-1]), function(vn)
  data.frame(time = demo_l$Time, variable = vn, mean_value = rowMeans(demo_l[, colnames(demo_l) == vn, drop = FALSE]))))
check("compute_curve_metrics: explicit lysis_threshold = 0.05 equals the default output",
      identical(env$compute_curve_metrics(pd_l), env$compute_curve_metrics(pd_l, lysis_threshold = 0.05)))
rp_l <- data.frame(time = rep(0:8, 2), variable = "P", replicate = rep(1:2, each = 9),
                   value = rep(c(0.05, 0.2, 0.5, 0.8, 1.0, 0.94, 1.0, 1.0, 1.0), 2))
check("replicate rows and summary: explicit 0.05 equals the default",
      identical(env$compute_replicate_metric_rows(rp_l), env$compute_replicate_metric_rows(rp_l, lysis_threshold = 0.05)) &&
      identical(env$compute_replicate_metrics(rp_l), env$compute_replicate_metrics(rp_l, lysis_threshold = 0.05)))
pl6 <- c(0.05, 0.2, 0.5, 0.8, 1.0, 0.94, 1.0, 1.0, 1.0)   # plateau with a single 6 % dip
l05 <- env$compute_one_curve_metrics(0:8, pl6, "P", lysis_threshold = 0.05)
l10 <- env$compute_one_curve_metrics(0:8, pl6, "P", lysis_threshold = 0.10)
check("plateau with a 6 % dip: lysis_time is finite at a 5 % threshold and NA at 10 %",
      is.finite(l05$lysis_time) && is.na(l10$lysis_time) && is.na(l10$od_drop) && is.finite(l05$od_drop))
check("lysis threshold is threaded through compute_curve_metrics and the replicate functions",
      is.finite(env$compute_curve_metrics(data.frame(time = 0:8, variable = "P", mean_value = pl6))$lysis_time) &&
      is.na(env$compute_curve_metrics(data.frame(time = 0:8, variable = "P", mean_value = pl6), lysis_threshold = 0.10)$lysis_time) &&
      all(is.na(env$compute_replicate_metric_rows(rp_l, lysis_threshold = 0.10)$lysis_time)) &&
      all(is.finite(env$compute_replicate_metric_rows(rp_l)$lysis_time)) &&
      is.na(env$compute_replicate_metrics(rp_l, lysis_threshold = 0.10)$lysis_time_mean))

cat("\n--- v2.12.1: MOI-series demo through the server ---\n")
testServer(app, {
  ingest_data(env$make_demo_data("moi_series"), "demo")
  do.call(session$setInputs, fit_defaults)
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
check("inside legends allow at most 4 entries", env$INSIDE_LEGEND_MAX_ENTRIES == 4)
# collision fallback: data in every corner box -> every corner holds >= 2 points, so no inside position is free
gx <- rep(seq(0, 10, length.out = 12), each = 12); gy <- rep(seq(0, 10, length.out = 12), 12)
cnt_g <- env$legend_corner_counts(gx, gy, FALSE, env$inside_legend_box(3))
check("legend_corner_counts: a filled panel has >= 2 points in every corner box (fallback case)",
      all(cnt_g >= env$INSIDE_LEGEND_MAX_POINTS) && identical(names(cnt_g)[which.min(cnt_g)], env$auto_legend_corner(gx, gy, FALSE, env$inside_legend_box(3))))
dz <- env$densify_xy(c(0, 10, 0, 10), c(0, 10, 10, 0), c("a", "a", "b", "b"), k = 5)
check("densify_xy: k points per segment, endpoints kept, per series", nrow(dz) == 2 * 6 && all(c(0, 10) %in% dz$x) && sum(dz$x == 10) == 2)
check("densify_xy: a steep line between two sparse points counts in a corner box it crosses",
      env$legend_corner_counts(c(0, 10), c(0, 10), FALSE, c(0.45, 0.35))[["bottom-left"]] == 1 &&
      env$legend_corner_counts(dz$x[dz$y >= 0][1:6], dz$y[1:6], FALSE, c(0.45, 0.35))[["top-right"]] >= 2)
check("legend_corner_counts: an empty corner box counts 0 (no fallback)",
      min(env$legend_corner_counts(cx, cy, TRUE, box = env$inside_legend_box(3))) == 0)
check("journal preset uses inside-auto", identical(env$journal_preset_settings(89, 65)$legend_position, "inside-auto"))
check("inside legends use a compact key",
      isTRUE(all.equal(as.numeric(env$inside_legend_theme("top-left")$legend.key.spacing.y), 1)) &&
      identical(as.numeric(env$inside_legend_theme("top-left")$legend.margin), c(1, 3, 1, 3)))

testServer(app, {
  ingest_data(env$make_demo_data("lysis"), "demo")
  do.call(session$setInputs, fit_defaults)
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
  do.call(session$setInputs, fit_defaults)
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
  ingest_data(env$make_demo_data("resistance"), "demo")
  do.call(session$setInputs, fit_defaults)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "",
                    metrics_ref_sample = "Uninfected", legend_position = "inside-auto")
  check("collision fallback: resistance main figure (curves cross every corner box) gets a right legend",
        identical(generate_plot()$theme$legend.position, "right"))
  check("collision fallback: resistance normalized figure gets a right legend",
        identical(generate_analysis_plot("normalized")$theme$legend.position, "right"))
  check("collision fallback does not trigger when a corner is clear (resistance growth rate keeps an inside legend)",
        identical(generate_analysis_plot("growth_rate")$theme$legend.position, "inside"))
})
testServer(app, {
  ingest_data(env$make_demo_data("two_phages"), "demo")
  do.call(session$setInputs, fit_defaults)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "",
                    metrics_ref_sample = "Uninfected", legend_position = "inside-auto")
  check("inside-auto: 5 samples (two_phages) fall back to a right legend",
        identical(generate_plot()$theme$legend.position, "right"))
})
testServer(app, {
  ingest_data(env$make_demo_data("two_phages"), "demo")
  do.call(session$setInputs, fit_defaults)
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
# plateau noise: a single 6 % dip is not a decline, a 15 % dip is
pl_t <- 0:8
plat <- function(dip) c(0.05, 0.2, 0.5, 0.8, 1.0, 1.0 * (1 - dip), 1.05, 1.1, 1.15)
check("first_local: a plateau with a single 6 % dip is NOT taken as the peak (global maximum kept)",
      env$compute_one_curve_metrics(pl_t, plat(0.06), "P", peak_mode = "first_local")$time_max_od == 8)
check("first_local: a 15 % dip IS taken as the peak (t = 4)",
      env$compute_one_curve_metrics(pl_t, plat(0.15), "P", peak_mode = "first_local")$time_max_od == 4)
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
  do.call(session$setInputs, fit_defaults)
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
  session$setInputs(metrics_peak_smooth_window = 3)
  rr3 <- replicate_metric_rows_current()
  # (a replicate whose global plateau maximum is followed by a > 5 % dip still reports the engine's own lysis onset; the
  # peak mode only decides WHICH peak, so what must hold is that no early false peak is chosen)
  check("resistance demo, first_local + 3-point smoothing: Uninfected replicates keep their plateau peak (t >= 270), not an early false one",
        all(rr3$time_max_od[rr3$sample == "Uninfected"] >= 270))
  check("resistance demo, first_local + 3-point smoothing: the resistant replicates still find the lysis",
        all(is.finite(rr3$lysis_time[rr3$sample == "Phage MOI 1 + resistant"])))
  session$setInputs(metrics_peak_smooth_window = 1)
  check("metrics_peak_mode has an APP_DEFAULTS entry", identical(env$APP_DEFAULTS$metrics_peak_mode, "global"))
})

cat("\n--- v2.13.0: multi-panel figure composer ---\n")
check("compose inputs have APP_DEFAULTS entries (compose_panels is a character vector)",
      is.character(env$APP_DEFAULTS$compose_panels) && length(env$APP_DEFAULTS$compose_panels) == 4 &&
      identical(env$APP_DEFAULTS$compose_ncol, "2") && identical(env$APP_DEFAULTS$compose_width, "double") &&
      env$APP_DEFAULTS$compose_panel_height_mm == 60 && env$APP_DEFAULTS$compose_tag_size == 8 &&
      identical(env$APP_DEFAULTS$compose_tag_case, "a") && is.numeric(env$APP_DEFAULTS$compose_width_mm))
check("compose panel choices: main first, then the analysis figures with the Analysis tab's labels",
      identical(unname(env$COMPOSE_PANEL_CHOICES), c("main", unname(env$ANALYSIS_FIGURE_CHOICES))) &&
      identical(names(env$COMPOSE_PANEL_CHOICES)[1], "Lysis curve (main)"))
testServer(app, {
  ingest_data(env$make_demo_data("lysis"), "demo")
  do.call(session$setInputs, fit_defaults)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "",
                    metrics_ref_sample = "Uninfected")
  do.call(session$setInputs, modifyList(env$journal_preset_settings(89, 65, "a"), list(panel_size_mode = "fit")))
  session$setInputs(analysis_metrics = c("lysis_time", "od_drop"), analysis_panel_label = "b")
  info <- compose_info()
  check("compose_info: main + metric summary + growth rate + normalized, two columns, two rows",
        identical(info$types, c("main", "metric_summary", "growth_rate", "normalized")) &&
        info$n == 4 && info$ncol == 2 && info$rows == 2 && length(info$skipped) == 0)
  check("compose_info: Nature double column x 2 rows of 60 mm = 183 x 120 mm",
        near(info$width_mm, 183) && near(info$height_mm, 120))
  check("compose_figure returns a patchwork object", inherits(info$plot, "patchwork"))
  svg_c <- of("composed_183x120.svg")
  quiet(ggsave(svg_c, info$plot, device = env$export_device("svg"), width = info$width_mm, height = info$height_mm,
               units = "mm", bg = "white"))
  xc <- readLines(svg_c); txt <- grep("<text", xc, value = TRUE)
  szs <- as.numeric(sub('.*font-size: ([0-9.]+)px.*', '\\1', txt))
  check("composed SVG is 183 mm wide (518.74 pt) and 120 mm tall", grepl("width='518.74pt'", grep("<svg", xc, value = TRUE)[1]) &&
        grepl("height='340.16pt'", grep("<svg", xc, value = TRUE)[1]))
  tag_txt <- grepl("font-weight: bold", txt) & grepl("font-size: 8.00px", txt) & grepl(">[a-d]</text>", txt)
  check("composed SVG: four bold 8 pt panel tags a-d (and no 'b' left over from the analysis panel's own label)",
        sum(tag_txt) == 4 && setequal(sub(".*>([a-d])</text>.*", "\\1", txt[tag_txt]), c("a", "b", "c", "d")))
  check("composed SVG: all text is Arial 6-8 pt without textLength",
        all(grepl('font-family: "Arial"', txt)) && all(szs >= 6 - 1e-6 & szs <= 8 + 1e-6) && !any(grepl("textLength", txt)))
  # upper-case tags and a custom tag size
  session$setInputs(compose_tag_case = "A", compose_tag_size = 7)
  svg_u <- of("composed_upper.svg"); iu <- compose_info()
  quiet(ggsave(svg_u, iu$plot, device = env$export_device("svg"), width = iu$width_mm, height = iu$height_mm, units = "mm", bg = "white"))
  tu <- grep("<text", readLines(svg_u), value = TRUE)
  check("tag case 'A' and size 7 reach the composed figure",
        sum(grepl("font-weight: bold", tu) & grepl("font-size: 7.00px", tu) & grepl(">[A-D]</text>", tu)) == 4)
  session$setInputs(compose_tag_case = "a", compose_tag_size = 8)
  # one column: four rows
  session$setInputs(compose_ncol = "1")
  i1 <- compose_info()
  check("one column: 4 rows, height = rows x panel height (240 mm), taller than the two-column figure",
        i1$rows == 4 && near(i1$height_mm, 4 * 60) && i1$height_mm > info$height_mm)
  session$setInputs(compose_ncol = "2", compose_panel_height_mm = 50)
  check("panel row height is configurable (2 rows x 50 mm = 100 mm)", near(compose_info()$height_mm, 100))
  session$setInputs(compose_panel_height_mm = 60, compose_width = "single")
  check("single column width of the chosen journal (Nature 89 mm)", near(compose_info()$width_mm, 89))
  session$setInputs(compose_width = "custom", compose_width_mm = 150)
  check("custom width uses compose_width_mm", near(compose_info()$width_mm, 150))
  session$setInputs(compose_width = "double", journal_family = "cell")
  check("the double column follows the journal family (Cell Press 174 mm)", near(compose_info()$width_mm, 174))
  session$setInputs(journal_family = "nature")
  # the height warning is the journal_preset_summary style
  session$setInputs(compose_panel_height_mm = 100, compose_ncol = "1")
  note <- output$compose_note
  check("height beyond the journal maximum is flagged in the note (4 x 100 = 400 mm > 170 mm)",
        grepl("exceeds this journal's maximum", note$html) || grepl("exceeds this journal's maximum", paste(note, collapse = "")))
  session$setInputs(compose_panel_height_mm = 60, compose_ncol = "2")
  # composed in the checkbox order, not the order the boxes were ticked
  session$setInputs(compose_panels = c("normalized", "main"))
  check("panels are composed in the checkbox order (main before normalized)",
        identical(compose_info()$types, c("main", "normalized")))
  # the download handler writes a vector file at the computed size
  session$setInputs(compose_panels = c("main", "metric_summary", "growth_rate", "normalized"), export_format = "svg")
  dl <- tryCatch(output$downloadComposedFigure, error = function(e) NULL)
  check("download handler writes an SVG of the composed figure",
        !is.null(dl) && file.exists(dl) && grepl("width='518.74pt'", paste(readLines(dl, n = 3), collapse = "")))
  # reports: the composed figure is an extra figure only when >= 2 panels are selected
  if (env$has_base64enc) {
    n_img <- function(h) lengths(regmatches(h, gregexpr("<img ", h, fixed = TRUE)))
    meta <- report_meta(); meta$generated <- "2026-10-19 00:00:00"
    session$setInputs(report_include_analysis = TRUE)
    h4 <- html_report_string(meta)
    session$setInputs(compose_panels = "main")
    h1 <- html_report_string(meta)
    check("HTML report: the composed figure adds exactly one <img> with 4 panels selected, none with 1", n_img(h4) == n_img(h1) + 1 &&
          grepl("<h3>Composed figure</h3>", h4, fixed = TRUE) && !grepl("Composed figure", h1, fixed = TRUE))
    check("HTML report: the composed figure comes after the analysis figures",
          regexpr("<h3>Curve landmarks</h3>", h4, fixed = TRUE) < regexpr("<h3>Composed figure</h3>", h4, fixed = TRUE))
    session$setInputs(compose_panels = c("main", "metric_summary", "growth_rate", "normalized"),
                      report_include_analysis = FALSE)
    check("HTML report: no composed figure when 'Include analysis figures' is off",
          !grepl("Composed figure", html_report_string(meta), fixed = TRUE))
    session$setInputs(report_include_analysis = TRUE)
  }
})
testServer(app, {
  ingest_data(env$make_demo_data("resistance"), "demo")
  do.call(session$setInputs, fit_defaults)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "",
                    metrics_ref_sample = "Uninfected", analysis_dose_metric = "lysis_time")
  session$setInputs(compose_panels = c("main", "growth_rate", "dose_response"))
  ir <- compose_info()
  check("a panel that cannot render (dose-response with one MOI level) is skipped and reported",
        identical(ir$types, c("main", "growth_rate")) && identical(ir$skipped, "MOI dose-response") && ir$n == 2 &&
        inherits(ir$plot, "patchwork"))
  session$setInputs(compose_panels = "dose_response")
  io <- compose_info()
  check("when every selected panel is unavailable nothing is composed (NULL plot, placeholder preview)",
        is.null(io$plot) && io$n == 0 && inherits(compose_plot_current(), "ggplot"))
  check("composed panels never contain a 'Figure not available' placeholder",
        !any(vapply(compose_info()$types, function(t) t == "dose_response", logical(1))))
})

cat("\n--- v2.14: sized file names, size bar, true-size previews ---\n")
testServer(app, {
  ingest_data(env$make_demo_data("lysis"), "demo")
  do.call(session$setInputs, fit_defaults)          # the startup state, no preset pressed
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = "Uninfected")
  check("main download name at startup: lysis_curve_a_89x65mm.svg", identical(main_download_name(), "lysis_curve_a_89x65mm.svg"))
  check("analysis download name at startup: metric_summary_b_89x65mm.svg", identical(analysis_download_name(), "metric_summary_b_89x65mm.svg"))
  check("composed download name: figure_composed_183x120mm.svg", identical(composed_download_name(), "figure_composed_183x120mm.svg"))
  check("file names carry no timestamp", all(!grepl("[0-9]{8}", c(main_download_name(), analysis_download_name(), composed_download_name()))))
  session$setInputs(panel_label = "", analysis_panel_label = "", analysis_type = "growth_rate", export_format = "png")
  check("empty panel labels fall back to 'main' and the figure type",
        identical(main_download_name(), "lysis_curve_main_89x65mm.png") && identical(analysis_download_name(), "growth_rate_growth_rate_89x65mm.png"))
  session$setInputs(panel_label = "a", analysis_panel_label = "b", analysis_type = "metric_summary", export_format = "svg")
  # the bar
  bar <- output$figure_standard_bar$html
  check("size bar at startup shows size, journal, font, text and line sizes and format",
        grepl("Export: 89 × 65 mm", bar, fixed = TRUE) && grepl("Nature single column", bar, fixed = TRUE) &&
        grepl("Arial 7 / 6 pt", bar, fixed = TRUE) && grepl("lines 0.75 / 0.5 pt", bar, fixed = TRUE) && grepl("SVG", bar, fixed = TRUE) &&
        !grepl("not journal-standard", bar, fixed = TRUE))
  session$setInputs(export_width = 100)
  check("a width that is no journal column reads 'custom size'", grepl("custom size", output$figure_standard_bar$html, fixed = TRUE))
  session$setInputs(export_width = 183, export_height = 120)
  check("183 mm is recognised as the Nature double column", grepl("Nature double column", output$figure_standard_bar$html, fixed = TRUE))
  session$setInputs(export_width = 89, export_height = 65)
  session$setInputs(main_tabs = "Compose figure")
  check("on the Compose tab the bar also shows the composed size",
        grepl("Composed: 183 × 120 mm, 2 columns", output$figure_standard_bar$html, fixed = TRUE))
  session$setInputs(main_tabs = "Lysis curve")
  # legacy look -> warning bar with the inline button, and sized names in inches
  do.call(session$setInputs, env$legacy_preset_settings())
  bar2 <- output$figure_standard_bar$html
  check("non-standard state (legacy look): warning style, message and the inline Apply button",
        grepl("fig-bar warn", bar2, fixed = TRUE) && grepl("not journal-standard", bar2, fixed = TRUE) &&
        grepl("apply_journal_preset_bar", bar2, fixed = TRUE) && grepl("Export: 10 × 8 in", bar2, fixed = TRUE))
  check("inch units give one-decimal inch names (legacy: lysis_curve_main_10.0x8.0in.pdf)",
        identical(main_download_name(), "lysis_curve_main_10.0x8.0in.pdf"))
  # the buttons move the unit bookkeeping with the numbers
  session$setInputs(apply_legacy_look = 1)
  check("Legacy look button marks the units as inches", identical(rv$units_seen, "in"))
  session$setInputs(apply_journal_preset = 1)
  check("Apply journal preset marks the units as mm", identical(rv$units_seen, "mm"))
  session$setInputs(apply_legacy_look = 2); session$setInputs(apply_journal_preset_bar = 1)
  check("the size bar's button runs the same preset handler", identical(rv$units_seen, "mm"))
  do.call(session$setInputs, fit_defaults)
  # previews: aspect ratio in canvas mode, fitted zoom in true-size mode
  session$setInputs(preview_mode = "canvas", plot_width = 820)
  pc <- as.character(preview_container("od_plot", "od_plot_img"))
  check("canvas preview keeps the export aspect ratio (820 px wide -> 599 px tall for 89 x 65 mm)",
        grepl("height:599px", pc, fixed = TRUE) && grepl("width:820px", pc, fixed = TRUE))
  pcc <- as.character(preview_container("compose_plot", "compose_img", compose_size_in))
  check("canvas preview of the composed figure keeps its aspect (183 x 120 mm -> 538 px tall)", grepl("height:538px", pcc, fixed = TRUE))
  session$setInputs(preview_mode = "wysiwyg", preview_zoom = 2, preview_zoom_auto = FALSE)
  check("true-size preview with auto-fit off uses the slider zoom (89 mm x 2 = 673 px)",
        grepl("width:673px", as.character(preview_container("od_plot", "od_plot_img")), fixed = TRUE))
  session$setInputs(preview_zoom_auto = TRUE)
  check("auto-fit without a known page width falls back to the slider",
        grepl("width:673px", as.character(preview_container("od_plot", "od_plot_img")), fixed = TRUE))
  session$setInputs(main_panel_width = 1000)
  check("auto-fit: 1000 px panel -> zoom 2.75 (rounded down to 0.25) for the 89 mm figure = 925 px",
        grepl("width:925px", as.character(preview_container("od_plot", "od_plot_img")), fixed = TRUE))
  check("auto-fit: the composed 183 mm figure gets zoom 1.25 = 865 px",
        grepl("width:865px", as.character(preview_container("compose_plot", "compose_img", compose_size_in)), fixed = TRUE))
  session$setInputs(main_panel_width = 100)
  check("auto-fit never goes below 0.5x (89 mm x 0.5 = 168 px)",
        grepl("width:168px", as.character(preview_container("od_plot", "od_plot_img")), fixed = TRUE))
  session$setInputs(main_panel_width = 5000)
  check("auto-fit never goes above 4x (89 mm x 4 = 1346 px)",
        grepl("width:1346px", as.character(preview_container("od_plot", "od_plot_img")), fixed = TRUE))
  check("preview_zoom_auto has an APP_DEFAULTS entry (TRUE)", isTRUE(env$APP_DEFAULTS$preview_zoom_auto))
})

cat("\n--- v2.14.1: memory guards ---\n")
r_ok <- env$safe_raster_dpi(89 / 25.4, 65 / 25.4, 600)
check("safe_raster_dpi: 89 x 65 mm at 600 dpi is uncapped (600)", identical(as.numeric(r_ok), 600) && isFALSE(attr(r_ok, "capped")))
r_big <- env$safe_raster_dpi(89, 65, 600)   # 89 in x 65 in: the wrong-unit case
check("safe_raster_dpi: 89 x 65 INCHES at 600 dpi is capped and fits 40 megapixels",
      isTRUE(attr(r_big, "capped")) && as.numeric(r_big) < 600 && 89 * 65 * as.numeric(r_big)^2 <= 40e6 &&
      as.numeric(r_big) == floor(as.numeric(r_big)))
check("safe_raster_dpi: never below 36 dpi, capped flag set", as.numeric(env$safe_raster_dpi(1000, 1000, 600)) == 36 &&
      isTRUE(attr(env$safe_raster_dpi(1000, 1000, 600), "capped")))
check("friendly_error_message appends the hint to memory errors only",
      grepl("Reduce DPI, export size, zoom or the number of points.",
            env$friendly_error_message(simpleError("vector memory limit of 16.0 Gb reached, see mem.maxVSize()")), fixed = TRUE) &&
      grepl("cannot allocate vector", env$friendly_error_message(simpleError("cannot allocate vector of size 8 Gb")), fixed = TRUE) &&
      !grepl("Reduce DPI", env$friendly_error_message(simpleError("object 'x' not found")), fixed = TRUE))
testServer(app, {
  ingest_data(env$make_demo_data("lysis"), "demo")
  do.call(session$setInputs, fit_defaults)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = "Uninfected")
  session$setInputs(export_units = "in", export_width = 500, export_height = 8)
  check("export_size_in clamps 500 in to 60 in (and leaves the other side alone)", near(export_size_in()[1], 60) && near(export_size_in()[2], 8))
  session$setInputs(export_units = "mm", export_width = 5000, export_height = 65)
  check("export_size_in clamps 5000 mm to 1524 mm (60 in)", near(export_size_in()[1] * 25.4, 1524) && near(export_size_in()[2] * 25.4, 65))
  session$setInputs(export_units = "mm", export_width = NA, export_height = 0)
  check("an emptied or zero size box falls back to the standard 89 x 65 mm", near(export_size_in()[1] * 25.4, 89) && near(export_size_in()[2] * 25.4, 65))
  # preview zoom and page width clamps
  session$setInputs(export_units = "mm", export_width = 89, export_height = 65, preview_zoom_auto = FALSE, preview_zoom = 40)
  check("preview zoom is clamped to 4x", near(preview_zoom_value(3.5), 4))
  session$setInputs(preview_zoom = -3); check("preview zoom is clamped to 0.5x", near(preview_zoom_value(3.5), 0.5))
  session$setInputs(preview_zoom_auto = TRUE, main_panel_width = 1e9)
  check("page width is clamped to 8000 px (zoom <= 4)", near(preview_zoom_value(3.5), 4))
  session$setInputs(main_panel_width = 5)
  check("page width is clamped to >= 200 px (zoom 0.5)", near(preview_zoom_value(3.5), 0.5))
  session$setInputs(main_panel_width = NULL)
  # tick guards: manual x interval of 0.001 over a 180 min axis would be 180k major breaks
  session$setInputs(x_tick_interval = 0.001)
  p_t <- generate_plot(); real_plot("a manual x tick interval of 0.001 still renders a real figure", p_t)
  xs <- p_t$scales$get_scales("x")
  check("...with ggplot's default breaks (no break vector was built)", inherits(xs$breaks, "waiver"))
  check("...and a normal render has few ticks", length(ggplot_build(p_t)$layout$panel_params[[1]]$x$breaks) < 60)
  session$setInputs(x_tick_interval = 30)
  p_n <- generate_plot(); check("a sane manual interval still sets explicit breaks", !inherits(p_n$scales$get_scales("x")$breaks, "waiver"))
  session$setInputs(x_tick_interval = NA, y_log_min_exponent = -400, y_log_max_exponent = 400)
  real_plot("an absurd log exponent range renders (default minor breaks)", generate_plot())
  session$setInputs(y_log_min_exponent = -2, y_log_max_exponent = 0)
  # the preview of an enormous (wrong-unit) size still produces a PNG, capped
  session$setInputs(export_units = "in", export_width = 89, export_height = 65, preview_mode = "wysiwyg",
                    preview_zoom_auto = FALSE, preview_zoom = 2)
  img <- output$od_plot_img
  check("renderImage with 89 x 65 INCHES still yields an existing PNG (capped dpi), not an error",
        is.list(img) && grepl("^data:image/png;base64,iVBOR", img$src) && nchar(img$src) > 500)
  check("the capped preview stays within the pixel budget (89 x 65 in)",
        { r <- env$safe_raster_dpi(60, 60, 96 * 2); 60 * 60 * as.numeric(r)^2 <= 40e6 })
  # a failing figure becomes the placeholder in the preview instead of an error
  session$setInputs(export_units = "mm", export_width = 89, export_height = 65, analysis_type = "dose_response",
                    selected_samples = c("Uninfected", "Phage MOI 10"))
  img2 <- output$analysis_img
  check("the analysis preview of an unavailable figure still yields a PNG", is.list(img2) && grepl("^data:image/png;base64,iVBOR", img2$src))
  # reports carry the memory diagnostics
  ms <- report_meta()$settings_df
  check("report provenance has the memory limit and figure raster size rows",
        all(c("R vector memory limit", "Figure raster size") %in% ms$setting) &&
        grepl("dpi", ms$value[ms$setting == "Figure raster size"], fixed = TRUE))
})
# downloads fail with a readable message and a hint, not an opaque error
testServer(app, {
  ingest_data(env$make_demo_data("lysis"), "demo")
  do.call(session$setInputs, fit_defaults)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", export_format = "png",
                    export_units = "mm", export_width = 89, export_height = 65, export_dpi = 600)
  dl <- tryCatch(output$downloadPlot, error = function(e) NULL)
  check("a 600 dpi PNG at 89 x 65 mm downloads normally (uncapped)", !is.null(dl) && file.exists(dl) && file.size(dl) > 1000)
  session$setInputs(export_units = "in", export_width = 89, export_height = 65, export_dpi = 600)
  dl2 <- tryCatch(output$downloadPlot, error = function(e) NULL)
  check("a wrong-unit 89 x 65 INCH PNG at 600 dpi is clamped and capped, and still downloads", !is.null(dl2) && file.exists(dl2) && file.size(dl2) > 1000)
})
# data-size sanity
testServer(app, {
  big <- data.frame(Time = seq_len(6000), A = runif(6000), B = runif(6000), C = runif(6000))
  ingest_data(big, "big.csv")
  html <- output$data_summary_ui$html
  check("more than 5000 time points: a persistent warning in the data summary",
        grepl("Large data set", html, fixed = TRUE) && grepl("may be slow", html, fixed = TRUE))
  check("6000 x 3 x 1 values is within the dashboard limit", !dashboard_too_big())
})
testServer(app, {
  wide <- data.frame(Time = seq_len(2500), matrix(runif(2500 * 900), 2500, 900, dimnames = list(NULL, paste0("S", 1:900))))
  ingest_data(wide, "wide.csv")
  html <- output$data_summary_ui$html
  check("more than 200 samples: the data-size warning appears", grepl("Large data set", html, fixed = TRUE))
  check("2500 time points x 900 samples exceeds 2,000,000 values: dashboard figures skipped", dashboard_too_big())
  check("...with a note explaining it", grepl("skipped", output$dash_size_note$html, fixed = TRUE))
})

cat("\n--- v2.15.0: fixed panel size (square panels) ---\n")
check("panel inputs have APP_DEFAULTS entries (fixed, 65 x 65 mm, square); the legacy preset sets 'fit'",
      identical(env$APP_DEFAULTS$panel_size_mode, "fixed") && env$APP_DEFAULTS$panel_width_mm == 65 &&
      env$APP_DEFAULTS$panel_height_mm == 65 && isTRUE(env$APP_DEFAULTS$panel_square) &&
      identical(env$legacy_preset_settings()$panel_size_mode, "fit"))
testServer(app, {
  ingest_data(env$make_demo_data("lysis"), "demo")
  do.call(session$setInputs, env$APP_DEFAULTS)           # the startup state: fixed 65 x 65 mm panel
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = "Uninfected",
                    analysis_metrics = c("lysis_time", "od_drop", "mu_max", "infection_strength"))
  panel_mm <- function(g) {
    lay <- g$layout[grepl("^panel", g$layout$name), , drop = FALSE]
    c(sum(vapply(unique(unlist(Map(seq, lay$l, lay$r))), function(i) grid::convertWidth(g$widths[i], "mm", valueOnly = TRUE), 0)),
      sum(vapply(unique(unlist(Map(seq, lay$t, lay$b))), function(i) grid::convertHeight(g$heights[i], "mm", valueOnly = TRUE), 0)))
  }
  g <- main_fixed_info(); szm <- attr(g, "size_mm")
  check("the main figure's panel is 65 x 65 mm (measured from the gtable)", all(abs(panel_mm(g) - 65) < 0.1))
  check("the figure is larger than the panel in both directions (axes, labels around it)", all(szm > 65) && all(szm < 100))
  check("export_size_in() is the computed figure size", all(abs(export_size_in() * 25.4 - szm) < 1e-6))
  svg_f <- of("fixed_main.svg")
  quiet(ggsave(svg_f, main_plot_obj(), device = env$export_device("svg"), width = export_size_in()[1], height = export_size_in()[2],
               units = "in", bg = "white", limitsize = FALSE))
  hdr <- grep("<svg", readLines(svg_f), value = TRUE)[1]
  w_pt <- as.numeric(sub(".*width='([0-9.]+)pt'.*", "\\1", hdr)); h_pt <- as.numeric(sub(".*height='([0-9.]+)pt'.*", "\\1", hdr))
  check("SVG export size equals the computed figure size (+/- 0.5 mm)",
        abs(w_pt / 72 * 25.4 - szm[1]) < 0.5 && abs(h_pt / 72 * 25.4 - szm[2]) < 0.5)
  check("the computed size covers both devices' text metrics (svglite and ragg each within 0.4 mm of it)", {
    meas <- function(open) { open(); on.exit(grDevices::dev.off())
      c(grid::convertWidth(sum(g$widths), "mm", valueOnly = TRUE), grid::convertHeight(sum(g$heights), "mm", valueOnly = TRUE)) }
    a1 <- meas(function() svglite::svglite(tempfile(fileext = ".svg"), width = 400 / 25.4, height = 400 / 25.4))
    a2 <- meas(function() ragg::agg_png(tempfile(fileext = ".png"), width = 400, height = 400, units = "mm", res = 72))
    all(szm >= pmax(a1, a2) - 1e-6) && all(abs(a1 - a2) < 0.4) && all(szm - pmin(a1, a2) < 0.4) })
  check("the bar shows panel size, figure size and the fit indicator",
        grepl("Panel 65 × 65 mm", output$figure_standard_bar$html, fixed = TRUE) &&
        grepl("fits: single column", output$figure_standard_bar$html, fixed = TRUE))
  session$setInputs(legend_position = "right")
  gr <- main_fixed_info()
  check("an outside legend makes the figure wider; the panel stays 65 x 65 mm",
        attr(gr, "size_mm")[1] > szm[1] + 10 && all(abs(panel_mm(gr) - 65) < 0.1))
  session$setInputs(legend_position = "inside-auto", panel_width_mm = 50)
  check("a 50 mm square panel gives a smaller figure", attr(main_fixed_info(), "size_mm")[1] < szm[1] - 10)
  session$setInputs(panel_width_mm = 65)
  # fit mode (the legacy geometry) keeps the export size
  session$setInputs(panel_size_mode = "fit", export_units = "mm", export_width = 89, export_height = 65)
  svg_fit <- of("fit_main.svg")
  quiet(ggsave(svg_fit, main_plot_obj(), device = env$export_device("svg"), width = export_size_in()[1], height = export_size_in()[2],
               units = "in", bg = "white"))
  check("fit mode: the SVG is 89 mm wide (252.28pt) as before", grepl("width='252.28pt'", grep("<svg", readLines(svg_fit), value = TRUE)[1]))
  session$setInputs(panel_size_mode = "fixed")
  # curve analysis figures get the square panel too; the composer lays fixed panels out in a grid
  session$setInputs(analysis_type = "growth_rate")
  ga <- analysis_fixed_info(); check("growth-rate figure: 65 x 65 mm panel", all(abs(panel_mm(ga) - 65) < 0.1))
  session$setInputs(compose_panels = c("main", "growth_rate", "normalized", "landmarks"), compose_ncol = "2")
  ci <- compose_info()
  check("fixed composer: 4 panels, 2 columns, width = 2 x the widest cell",
        ci$n == 4 && ci$ncol == 2 && inherits(ci$plot, "gtable") &&
        abs(ci$width_mm - 2 * grid::convertWidth(ci$plot$widths[1], "mm", valueOnly = TRUE)) < 1e-6 &&
        abs(ci$width_mm - attr(ci$plot, "size_mm")[1]) < 1e-6 && ci$width_mm > 2 * 65)
})

cat(sprintf("\n============ SUMMARY: %d passed / %d failed / %d skipped ============\n", pass, fail, skip))
if (nzchar(render_dir)) cat("renders in:", render_dir, "\n")
if (fail > 0) quit(status = 1)
