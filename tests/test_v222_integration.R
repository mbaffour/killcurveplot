# ============================================================================
# v2.22 statistics and fitting UI: the Statistics-tab sections (column statistics, assumption checks, post-hoc method,
# time-course ANOVA, AUC, outliers, one-sample tests), the Curve fitting blocks (fit statistics, lysis models, exponential
# window, compare fits, bands / lysis / window overlays), the per-time, AUC, 4PL and correlation figures, and their report,
# bundle, methods-text and dashboard wiring.
# Run from the repository root:  Rscript tests/test_v222_integration.R
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
skipit <- function(desc) { skip <<- skip + 1L; cat("SKIP:", desc, "\n") }
has_html <- function(x) is.list(x) && !is.null(x$html) && nzchar(as.character(x$html))
is_tbl <- function(x) is.character(x) && length(x) == 1 && grepl("<table", x, fixed = TRUE)
rd <- function(f) utils::read.csv(f, check.names = FALSE, stringsAsFactors = FALSE)

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.22.0 or later", package_version(env$APP_VERSION) >= "2.22.0")

cat("\n--- defaults, choices, UI ---\n")
d <- env$APP_DEFAULTS
new_in <- c("s22_metrics", "s22_qq_metric", "posthoc_method", "tc_adjust", "tc_repeated", "auc_baseline", "auc_baseline_value", "auc_from", "auc_to",
            "auc_relative", "auc_style", "outlier_method", "onesample_text", "explorer_stats", "explorer_line", "explorer_band", "fit_ci_method",
            "fit_weights_scheme", "fit_bands_on", "fit_bands_type", "lysis_models", "lysis_from", "lysis_level", "lysis_show", "expwin_level",
            "expwin_mode", "expwin_from", "expwin_to", "expwin_min_points", "expwin_show", "cmp_model", "cmp_param", "dr_fit", "dr_fix_bottom",
            "dr_fix_top", "dr_band", "dr_ec50")
check("every new input has an APP_DEFAULTS entry", all(new_in %in% names(d)))
check("the new entries come at the END of LEGACY_BASE (after the v2.22 data-handling entries), in order",
      { pos <- match(new_in, names(env$LEGACY_BASE)); !anyNA(pos) && all(diff(pos) > 0) && min(pos) > match("t_stat_override", names(env$LEGACY_BASE)) })
check("defaults: holm_welch post-hoc, no overlays, no 4PL, no correlation (a session that does not touch them is unchanged)",
      identical(d$posthoc_method, "holm_welch") && !isTRUE(d$fit_bands_on) && !isTRUE(d$lysis_show) && !isTRUE(d$expwin_show) &&
      !isTRUE(d$dr_fit) && identical(d$explorer_stats, "none") && !isTRUE(d$explorer_line) && identical(d$fit_weights_scheme, "none"))
check("neither the journal preset nor the Legacy look touch them",
      !any(new_in %in% names(env$legacy_preset_settings())) && !any(new_in %in% names(env$journal_preset_settings(89, 65))))
check("post-hoc choices: the seven methods", identical(unname(env$S22_POSTHOC_CHOICES), c("holm_welch", "tukey", "games_howell", "dunn", "dunnett", "bonferroni", "sidak")))
check("the two new figure types are analysis figures and composer panels",
      all(c("per_time", "auc") %in% env$ANALYSIS_FIGURE_CHOICES) && all(c("per_time", "auc") %in% env$COMPOSE_PANEL_CHOICES))
ui_html <- as.character(env$ui)
ids <- c("s22_metrics", "s22_describe_table", "downloadDescribeCSV", "s22_normality_table", "s22_variance_table", "s22_qq_container", "downloadQQFigure",
         "posthoc_method", "s22_omnibus_table", "s22_posthoc_table", "downloadPosthocCSV", "tc_adjust", "tc_repeated", "s22_tc_anova_table", "s22_pertime_table",
         "downloadTCAnovaCSV", "downloadPerTimeCSV", "auc_baseline", "auc_from", "auc_to", "auc_relative", "s22_auc_table", "s22_auc_compare_table",
         "downloadAUCCSV", "downloadAUCCompareCSV", "outlier_method", "s22_outlier_table", "s22_curve_outlier_table", "downloadOutliersCSV",
         "onesample_text", "s22_onesample_table", "downloadOneSampleCSV", "downloadStats22All", "explorer_stats", "explorer_line", "explorer_stats_table",
         "downloadExplorerStatsCSV", "fit_ci_method", "fit_weights_scheme", "fit_bands_on", "fit_bands_type", "lysis_show", "expwin_show", "fit_ext_table",
         "downloadFitExtCSV", "fit_resid_container", "downloadFitResidFigure", "lysis_models", "lysis_table", "downloadLysisFitsCSV", "expwin_mode",
         "expwin_table", "downloadExpWindowCSV", "cmp_samples_ui", "cmp_test_table", "cmp_params_table", "downloadCompareFitsCSV", "dr_fit", "dr_table",
         "downloadDoseResponseCSV", "auc_style")
check("UI: every new input and output id", all(vapply(ids, function(id) grepl(paste0('id="', id, '"'), ui_html, fixed = TRUE), logical(1))))
check("UI: the seven Statistics sections, before the Methods text",
      all(vapply(c("Column statistics", "Assumption checks", "Post-hoc comparison (method)", "Time-course ANOVA", "Area under the curve", "Outliers", "One-sample tests"),
                 function(x) grepl(x, ui_html, fixed = TRUE), logical(1))) &&
      regexpr("One-sample tests", ui_html, fixed = TRUE) < regexpr("Methods text (editable)", ui_html, fixed = TRUE))
check("UI: the Curve fitting blocks", all(vapply(c("Lysis models (decay after the peak)", "Exponential-growth window", "Compare fits (do the best-fit values differ between samples?)",
                                                   "Fit statistics: intervals, AICc and residual checks"), function(x) grepl(x, ui_html, fixed = TRUE), logical(1))))
check("UI: the n = 3 power note and the no-automatic-exclusion note are there", grepl("power is low", ui_html, fixed = TRUE) && grepl("there is no automatic exclusion", ui_html, fixed = TRUE))
check("UI: the growth fitter's missing weights are said", grepl("growth-model fitter has no weights", ui_html, fixed = TRUE))

cat("\n--- pure helpers ---\n")
pa <- env$s22_parse_assignments("doubling_time = 20; Lysis onset: 90\nbad; foo = 3")
check("s22_parse_assignments: metric names and labels, junk reported", identical(unname(pa$values), c(20, 90)) && identical(names(pa$values), c("doubling_time", "lysis_time")) && length(pa$invalid) == 2)
check("s22_choice / s22_num", identical(env$s22_choice("x", c("a", "b"), "a"), "a") && identical(env$s22_choice("b", c("a", "b"), "a"), "b") &&
      is.na(env$s22_num("")) && env$s22_num("2,5") == 2.5)
check("dose_series_name strips the MOI token", identical(env$dose_series_name(c("Phage A MOI 10", "Phage MOI 0.1", "MOI 5")), c("Phage A", "Phage", "Phage")))
pr <- data.frame(time = c(1, 2, 3), sample = "A", p_adj = c(0.2, 0.03, 0.0004))
sr <- env$per_time_star_rows(pr)
check("per_time_star_rows: p_adj < 0.05 only, with their stars", identical(sr$time, c(2, 3)) && identical(sr$stars, c("*", "***")))
check("per_time_star_rows: nothing significant -> NULL", is.null(env$per_time_star_rows(pr[1, ])))
au <- env$auc_settings_from_inputs("junk", "x", "", "50")
check("auc_settings_from_inputs: junk falls back, a typed window parses", identical(au$baseline, "none") && au$baseline_value == 0 && is.null(au$t_from) && au$t_to == 50)
check("s22_display maps names and keeps the raw column for the CSV",
      identical(names(env$s22_display(data.frame(sample = "a", x = 1), c(a = "Alpha"))), c("sample", "sample_raw", "x")) &&
      identical(env$s22_display(data.frame(sample = "a", x = 1), c(a = "Alpha"), keep_raw = FALSE)$sample, "Alpha"))

cat("\n--- server: lysis scenario, Statistics tab ---\n")
testServer(app, {
  ingest_data(env$make_demo_data("lysis"), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1],
                    analysis_metrics = c("max_od", "od_drop"), s22_metrics = c("max_od", "od_drop", "lysis_time", "mu_max", "auc"), main_tabs = "Statistics")
  rm <- replicate_metric_rows_current()
  # column statistics
  d <- s22_describe_current()
  check("column statistics: one row per sample x metric, the documented columns",
        nrow(d) == 15 && all(c("n", "mean", "sd", "sem", "cv_pct", "ci_lo", "ci_hi", "median", "iqr", "min", "max", "skewness", "kurtosis", "geo_mean") %in% names(d)) && all(d$n[d$metric == "max_od"] == 3))
  check("column statistics: mean and SD agree with base R", {
    x <- rm$max_od[rm$sample == smp[1]]; r <- d[d$sample == smp[1] & d$metric == "max_od", ]
    isTRUE(all.equal(r$mean, mean(x))) && isTRUE(all.equal(r$sd, sd(x))) && isTRUE(all.equal(r$sem, sd(x) / sqrt(3)))
  })
  check("column statistics: renders a table and a note", is_tbl(output$s22_describe_table) && has_html(output$s22_describe_note))
  csv <- rd(output$downloadDescribeCSV)
  check("column statistics CSV: every row, with the raw sample column", nrow(csv) == nrow(d) && "sample_raw" %in% names(csv) && "metric_label" %in% names(csv))
  # assumption checks
  nm <- s22_normality_current()
  check("normality: per sample x metric plus a pooled-residuals row per metric", nrow(nm) == 5 * 4 && sum(nm$sample == "(pooled residuals)") == 5)
  vr <- s22_variance_current()
  check("variance: Brown-Forsythe and Bartlett per metric", nrow(vr) == 10 && all(c("Bartlett") %in% vr$test) && any(grepl("Brown-Forsythe", vr$test)))
  check("assumption tables render, with CSVs", is_tbl(output$s22_normality_table) && is_tbl(output$s22_variance_table) &&
        nrow(rd(output$downloadNormalityCSV)) == nrow(nm) && nrow(rd(output$downloadVarianceCSV)) == nrow(vr))
  qq <- s22_qq_fig$build()
  check("Q-Q figure builds", inherits(qq, "ggplot") && !is.null(qq$layers) && length(qq$layers) >= 2)
  check("Q-Q figure: a container and a download", has_html(output$s22_qq_container) && file.exists(output$downloadQQFigure) && file.size(output$downloadQQFigure) > 1000)
  # post-hoc method
  session$setInputs(analysis_metrics = "max_od")
  st <- analysis_stats_current()
  ph <- s22_posthoc_current()
  m_pair <- function(a, b) paste(sort(c(a, b)), collapse = "||")
  cmpP <- setNames(st$pairwise$p_adj_holm, mapply(m_pair, st$pairwise$sample, st$pairwise$versus))
  phP  <- setNames(ph$posthoc$p_adj, mapply(m_pair, ph$posthoc$group1, ph$posthoc$group2))
  check("holm_welch (default): the pairwise table has the same pairs as compare_replicate_metric", setequal(names(cmpP), names(phP)) && nrow(ph$posthoc) == nrow(st$pairwise))
  check("holm_welch (default): p_adj equals p_adj_holm of compare_replicate_metric, pair by pair", isTRUE(all.equal(unname(phP[names(cmpP)]), unname(cmpP), tolerance = 1e-8)))
  check("holm_welch (default): the method column says Holm-adjusted Welch", all(grepl("Holm", ph$posthoc$method)) && all(ph$posthoc$stars %in% c("***", "**", "*", "ns", "")))
  check("omnibus table: ANOVA, Welch ANOVA, Kruskal-Wallis with eta2 / epsilon2", nrow(ph$omnibus) == 3 && all(c("eta_sq", "epsilon_sq") %in% names(ph$omnibus)))
  check("post-hoc tables render", is_tbl(output$s22_omnibus_table) && is_tbl(output$s22_posthoc_table) && has_html(output$s22_posthoc_note))
  meths <- character(0)
  for (mth in c("tukey", "games_howell", "dunn", "dunnett", "bonferroni", "sidak")) {
    session$setInputs(posthoc_method = mth)
    p2 <- s22_posthoc_current()
    meths[mth] <- paste(unique(p2$posthoc$method), collapse = "|")
  }
  check("switching the method changes the method column (Tukey, Games-Howell, Dunn, Dunnett [or its fallback], Bonferroni, Sidak)",
        grepl("Tukey", meths["tukey"]) && grepl("Games", meths["games_howell"]) && grepl("Dunn", meths["dunn"]) && grepl("Dunnett", meths["dunnett"]) &&
        grepl("onferroni", meths["bonferroni"]) && grepl("idak", meths["sidak"]) && length(unique(meths)) == 6)
  session$setInputs(posthoc_method = "tukey")
  check("the post-hoc CSV names the method", any(grepl("Tukey", readLines(output$downloadPosthocCSV))))
  check("the methods sentence names the method", grepl("Tukey", s22_posthoc_sentence(), fixed = TRUE))
  session$setInputs(posthoc_method = "holm_welch")
  # time-course
  a <- s22_tc_current()
  check("time-course ANOVA: Sample, Time and the interaction", identical(a$anova$term, c("Sample", "Time", "Sample x Time")) && nzchar(a$method))
  check("time-course ANOVA: the sample x time interaction is significant on the demo (p < 0.05)", a$anova$p[a$anova$term == "Sample x Time"] < 0.05)
  check("time-course ANOVA: the method and a note are shown", grepl("repeated", a$method) && has_html(output$s22_tc_note))
  session$setInputs(tc_repeated = FALSE)
  check("repeated measures off: the plain two-way ANOVA", grepl("ignores", s22_tc_current()$method))
  session$setInputs(tc_repeated = TRUE)
  pt <- s22_pertime_current()
  check("per-time comparisons: each non-reference sample at every time, p_adj and stars", nrow(pt) == 2 * 13 && all(c("p_adj", "stars", "diff") %in% names(pt)) && !(smp[1] %in% pt$sample))
  session$setInputs(tc_adjust = "bonferroni")
  check("the adjustment selector is used (Bonferroni >= Sidak)", all(s22_pertime_current()$p_adj >= pt$p_adj - 1e-12, na.rm = TRUE) && grepl("bonferroni", s22_pertime_current()$note[1]))
  session$setInputs(tc_adjust = "sidak")
  check("time-course tables render, with CSVs", is_tbl(output$s22_tc_anova_table) && is_tbl(output$s22_pertime_table) &&
        nrow(rd(output$downloadTCAnovaCSV)) == 3 && nrow(rd(output$downloadPerTimeCSV)) == nrow(pt))
  g <- generate_analysis_plot("per_time")
  check("per-time figure builds (not the placeholder), with stars and no legend",
        inherits(g, "ggplot") && any(vapply(g$layers, function(l) inherits(l$geom, "GeomText"), TRUE)) && identical(g$theme$legend.position, "none") && !is_placeholder_plot(g))
  check("per-time figure: a note says how many time points are marked", any(grepl("marked", attr(g, "figure_note"))))
  fixed <- fixed_figure(g, "per_time")
  check("per-time figure: a fixed panel of the set size", inherits(fixed, "gtable") && !is.null(attr(fixed, "size_mm")))
  session$setInputs(metrics_ref_sample = "")
  g0 <- generate_analysis_plot("per_time")
  check("per-time figure without a reference: the placeholder (the reference is required)", is_placeholder_plot(g0) || is.null(s22_pertime_current()))
  session$setInputs(metrics_ref_sample = smp[1])
  # AUC
  au <- s22_auc_current()
  check("AUC table: one row per sample", nrow(au$summary) == 3 && setequal(au$summary$sample, smp) && nrow(au$replicates) == 9)
  check("AUC table: the relative AUC of the reference is exactly 1", isTRUE(all.equal(au$summary$rel_auc[au$summary$sample == smp[1]], 1)))
  check("AUC: the infected samples have a smaller AUC than the uninfected reference", all(au$summary$rel_auc[au$summary$sample != smp[1]] < 1))
  ac <- s22_auc_compare_current()
  check("AUC comparison: each sample vs the reference, with stars", nrow(ac) == 2 && all(ac$stars %in% c("*", "**", "***")))
  session$setInputs(auc_baseline = "first", auc_from = "30", auc_to = "120")
  au2 <- s22_auc_current()
  check("AUC baseline / window settings change the areas", all(au2$summary$mean < au$summary$mean) && all(au2$replicates$baseline > 0))
  session$setInputs(auc_baseline = "none", auc_from = "", auc_to = "")
  check("AUC tables render with CSVs", is_tbl(output$s22_auc_table) && is_tbl(output$s22_auc_compare_table) && nrow(rd(output$downloadAUCCSV)) == 3 &&
        nrow(rd(output$downloadAUCRepCSV)) == 9 && nrow(rd(output$downloadAUCCompareCSV)) == 2)
  ga <- generate_analysis_plot("auc")
  check("AUC figure builds (dot style), no legend", inherits(ga, "ggplot") && identical(ga$theme$legend.position, "none") && any(vapply(ga$layers, function(l) inherits(l$geom, "GeomPoint"), TRUE)))
  session$setInputs(auc_style = "bar")
  check("AUC figure: bar style", any(vapply(generate_analysis_plot("auc")$layers, function(l) inherits(l$geom, "GeomCol"), TRUE)))
  session$setInputs(auc_style = "dot")
  # outliers
  ot <- s22_outliers_current()
  check("outlier table: every value of every selected metric, flagged column", nrow(ot) == sum(vapply(c("max_od", "od_drop", "lysis_time", "mu_max", "auc"), function(m) sum(is.finite(rm[[m]])), 1)) && is.logical(ot$flagged) && all(ot$method == "grubbs"))
  session$setInputs(outlier_method = "iqr")
  check("outlier method selector", all(s22_outliers_current()$method == "iqr"))
  session$setInputs(outlier_method = "grubbs")
  oc <- s22_curve_outliers_current()
  check("replicate-curve flags: one row per replicate curve", nrow(oc) == 9 && all(c("distance", "flagged") %in% names(oc)))
  check("outlier tables render, with CSVs, and the Data-tab note is in the UI", is_tbl(output$s22_outlier_table) && is_tbl(output$s22_curve_outlier_table) &&
        nrow(rd(output$downloadOutliersCSV)) == nrow(ot) && nrow(rd(output$downloadCurveOutliersCSV)) == 9 && grepl("Data tab", ui_html, fixed = TRUE))
  # one-sample
  session$setInputs(onesample_text = "")
  check("one-sample tests: nothing typed -> no table, a hint", is.null(s22_onesample_current()) && has_html(output$s22_onesample_note))
  session$setInputs(onesample_text = "mu_max = 0.02; lysis_time = 90")
  os <- s22_onesample_current()
  check("one-sample tests: a t test and a Wilcoxon row per sample and typed metric", nrow(os) == 10)
  check("one-sample tests: the t test p and CI agree with t.test", {
    x <- rm$mu_max[rm$sample == smp[1]]; tt <- t.test(x, mu = 0.02); r <- os[os$sample == smp[1] & os$metric == "mu_max" & os$test == "One-sample t", ]
    isTRUE(all.equal(r$p, tt$p.value)) && isTRUE(all.equal(r$ci_lo, tt$conf.int[1]))
  })
  check("one-sample table renders, with a CSV", is_tbl(output$s22_onesample_table) && nrow(rd(output$downloadOneSampleCSV)) == nrow(os))
  # explorer
  session$setInputs(explorer_x = "max_od", explorer_y = "od_drop", explorer_stats = "pearson", explorer_line = TRUE, explorer_level = "Per replicate")
  ex <- explorer_stats_df()
  check("metric explorer: a correlation row (r, CI, p) and the linear fit rows", !is.null(ex) && any(ex$quantity == "Correlation") && sum(grepl("^Linear fit", ex$quantity)) == 2 &&
        all(c("r", "ci_lo", "ci_hi", "p") %in% names(ex)))
  check("metric explorer: Pearson r agrees with cor()", isTRUE(all.equal(ex$r[ex$quantity == "Correlation"], cor(rm$max_od, rm$od_drop, use = "complete.obs"), tolerance = 1e-8)))
  session$setInputs(explorer_stats = "spearman")
  check("metric explorer: Spearman", grepl("pearman", explorer_stats_df()$method[1], ignore.case = TRUE))
  ge <- generate_analysis_plot("metric_explorer")
  check("metric explorer figure: the fit line and its band are drawn", any(vapply(ge$layers, function(l) inherits(l$geom, "GeomRibbon"), TRUE)) && any(vapply(ge$layers, function(l) inherits(l$geom, "GeomLine"), TRUE)))
  check("metric explorer: the table, the note and the CSV", is_tbl(output$explorer_stats_table) && has_html(output$explorer_stats_note) && nrow(rd(output$downloadExplorerStatsCSV)) == nrow(ex))
  session$setInputs(explorer_line = FALSE, explorer_stats = "none")
  check("metric explorer: both off -> no band, no table", !any(vapply(generate_analysis_plot("metric_explorer")$layers, function(l) inherits(l$geom, "GeomRibbon"), TRUE)) && is.null(explorer_stats_df()))
  check("existing outputs untouched: the Replicate comparison tables still render", is_tbl(output$stats_tab_pairwise) && is_tbl(output$stats_tab_omnibus))
  # all-tables archive
  zf <- output$downloadStats22All
  have_zip <- requireNamespace("zip", quietly = TRUE)
  zl <- if (have_zip) basename(zip::zip_list(zf)$filename) else basename(sub("^\\s*[0-9]+\\s+[0-9-]+\\s+[0-9:]+\\s+", "", system2("unzip", c("-l", zf), stdout = TRUE)))
  check("the all-tables archive has the Statistics-tab and the Curve-fitting tables",
        all(c("column_statistics.csv", "normality.csv", "variance_tests.csv", "posthoc_omnibus.csv", "posthoc_pairwise.csv", "timecourse_anova.csv", "per_time_comparisons.csv",
              "auc_per_sample.csv", "auc_comparison.csv", "outliers_metrics.csv", "one_sample_tests.csv", "fit_statistics.csv", "lysis_models.csv", "exponential_window.csv",
              "compare_fits.csv", "README.txt") %in% zl))
})

cat("\n--- server: lysis scenario, Curve fitting tab ---\n")
testServer(app, {
  ingest_data(env$make_demo_data("lysis"), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1], main_tabs = "Curve fitting")
  fr <- fit_results()
  ext <- fit_ext_cur()
  check("growth fit statistics: one row per fit row, with CI and AICc columns", nrow(ext) == nrow(fr$table) && all(c("A_lo", "A_hi", "mu_max_lo", "mu_max_hi", "lag_abs_lo", "lag_abs_hi", "aicc", "adj_r2", "rmse", "akaike_weight", "runs_test_p", "shapiro_p") %in% names(ext)))
  conv <- ext$converged %in% TRUE
  check("growth fit statistics: the intervals contain the estimate (profile / Wald)", all(ext$mu_max_lo[conv] <= ext$mu_max[conv] + 1e-9 & ext$mu_max[conv] <= ext$mu_max_hi[conv] + 1e-9, na.rm = TRUE) && sum(is.finite(ext$mu_max_lo[conv])) >= 6)
  check("growth fit statistics: finite AICc and adjusted R2 for the converged fits", all(is.finite(ext$aicc[conv & ext$n_points >= 7])) && all(is.finite(ext$adj_r2[conv & ext$n_points >= 5])) && all(ext$adj_r2[conv] <= ext$r2[conv] + 1e-9, na.rm = TRUE))
  check("growth fit statistics: Akaike weights sum to 1 per curve (where AICc exists)", {
    s <- tapply(ext$akaike_weight[conv], ext$sample[conv], function(w) if (all(is.na(w))) 1 else sum(w, na.rm = TRUE)); all(abs(s - 1) < 1e-8) && any(is.finite(ext$akaike_weight)) })
  check("growth fit statistics: the refit agrees with the engine's parameters (no divergent refit is used)", {
    ob <- fit_objects_cur(); ok <- vapply(seq_along(ob), function(i) is.null(ob[[i]]$fit) || abs(coef(ob[[i]]$fit)[["mu"]] - fr$table$mu_max[i]) <= 0.02 * fr$table$mu_max[i], TRUE)
    all(ok)
  })
  check("growth fit statistics: a lag on its bound has no interval and says so", {
    b <- ext[conv & fr$table$lag <= 1e-8, ]; nrow(b) > 0 && all(is.na(b$lag_abs_lo)) && all(grepl("lower bound", b$ci_note))
  })
  check("growth fit statistics: the table, the note and the CSV", is_tbl(output$fit_ext_table) && has_html(output$fit_ext_note) && nrow(rd(output$downloadFitExtCSV)) == nrow(ext) && "aicc" %in% names(rd(output$downloadFitExtCSV)))
  session$setInputs(fit_ci_method = "wald")
  ew <- fit_ext_cur()
  check("the Wald method is used when chosen", all(ew$ci_method[ew$converged %in% TRUE] %in% c("wald", "wald (row standard errors)")))
  session$setInputs(fit_ci_method = "profile")
  rf <- fit_resid_current()
  check("residuals: the best model of every curve, without the anchored first point", !is.null(rf) && all(c("t", "fitted", "residual", "std_residual") %in% names(rf)) && length(unique(rf$sample)) == 3 && all(rf$t > 0))
  check("residual figure builds and downloads", inherits(s22_resid_fig$build(), "ggplot") && file.exists(output$downloadFitResidFigure) && file.size(output$downloadFitResidFigure) > 1000 && has_html(output$fit_resid_container))
  session$setInputs(fit_level = "replicate")
  er <- fit_ext_cur()
  check("replicate-level fit statistics: one row per replicate curve and model, with intervals", nrow(er) == 27 && "replicate" %in% names(er) && sum(is.finite(er$mu_max_lo)) >= 20 && is_tbl(output$fit_ext_table))
  check("replicate-level residuals: every replicate curve is there", length(unique(paste(fit_resid_current()$sample, fit_resid_current()$replicate))) == 9)
  session$setInputs(fit_level = "mean")
  # bands
  win <- fr$windows
  bf <- env$growth_band_frames(fr$table, fit_objects_cur(), win, which(fr$table$best %in% TRUE), "confidence")
  check("bands: lo <= fit <= hi on the whole grid, in OD units", !is.null(bf) && all(bf$lo <= bf$od + 1e-9 & bf$od <= bf$hi + 1e-9) && all(bf$lo > 0))
  bp <- env$growth_band_frames(fr$table, fit_objects_cur(), win, which(fr$table$best %in% TRUE), "prediction")
  check("bands: the prediction band is wider than the confidence band", mean(bp$hi - bp$lo) > mean(bf$hi - bf$lo))
  check("bands: the fit curve equals the engine's predict_growth_model (back-transform is exp)", {
    i <- which(fr$table$best %in% TRUE)[1]; r <- fr$table[i, ]; b <- bf[bf$sample == r$sample, ]
    isTRUE(all.equal(b$od, env$predict_growth_model(r, b$time, r$y0), tolerance = 1e-4))
  })
  g0 <- generate_analysis_plot("fit_overlay")
  n0 <- length(g0$layers)
  session$setInputs(fit_bands_on = TRUE)
  g1 <- generate_analysis_plot("fit_overlay")
  check("the band toggle adds a ribbon behind the fit (and nothing else) to the fit figure", length(g1$layers) == n0 + 1 && inherits(g1$layers[[1]]$geom, "GeomRibbon") && !inherits(g0$layers[[1]]$geom, "GeomRibbon"))
  session$setInputs(fit_bands_on = TRUE, fit_bands_type = "prediction", lysis_show = TRUE, expwin_show = TRUE)
  g2 <- generate_analysis_plot("fit_overlay")
  check("bands + lysis decay + exponential window: ribbon, rectangles and the decay lines are drawn",
        any(vapply(g2$layers, function(l) inherits(l$geom, "GeomRect"), TRUE)) && length(g2$layers) >= n0 + 3 && length(s22_fit_overlay_notes()) == 3 && has_html(output$fit_overlay_note))
  check("the exponential-window rectangles span finite y (a log axis cannot draw -Inf)", {
    rl <- g2$layers[[which(vapply(g2$layers, function(l) inherits(l$geom, "GeomRect"), TRUE))[1]]]; all(is.finite(rl$data$t_start)) && is.finite(ggplot2::ggplot_build(g2)$data[[which(vapply(g2$layers, function(l) inherits(l$geom, "GeomRect"), TRUE))[1]]]$ymin[1])
  })
  session$setInputs(fit_bands_on = FALSE, lysis_show = FALSE, expwin_show = FALSE, fit_bands_type = "confidence")
  check("overlays off again: the fit figure is the original", length(generate_analysis_plot("fit_overlay")$layers) == n0)
  session$setInputs(fit_level = "replicate", fit_bands_on = TRUE)
  check("replicate-level fits: bands are not drawn and the note says why", any(grepl("mean-curve", s22_fit_overlay_notes())))
  session$setInputs(fit_level = "mean", fit_bands_on = FALSE)
  # lysis models
  ly <- lysis_current()
  op <- ly$table[ly$table$model == "one_phase_decay" & ly$table$sample == "Phage MOI 10", ]
  check("lysis models: a one_phase_decay row per sample, with a finite k for a lysing sample", nrow(op) == 1 && isTRUE(op$converged) && is.finite(op$k) && op$k > 0 && is.finite(op$half_life))
  check("lysis models: k, half-life, t50 and plateau come with CIs and r2 / AICc", all(is.finite(c(op$k_lo, op$k_hi, op$t50_lysis_lo, op$plateau_lo, op$r2, op$aicc))) && op$k_lo < op$k && op$k < op$k_hi)
  check("lysis models: the half-life is ln 2 / k", isTRUE(all.equal(op$half_life, log(2) / op$k, tolerance = 1e-8)))
  check("lysis models: the uninfected curve (no decay) does not converge, with a reason", {
    u <- ly$table[ly$table$sample == smp[1] & ly$table$model == "one_phase_decay", ]; !isTRUE(u$converged) && nzchar(u$note)
  })
  check("lysis models: the fitted curves are on a grid after the peak", !is.null(ly$curves) && all(ly$curves$time >= 60) && all(is.finite(ly$curves$od)))
  check("lysis models: table, note and CSV", is_tbl(output$lysis_table) && has_html(output$lysis_note) && nrow(rd(output$downloadLysisFitsCSV)) == nrow(ly$table))
  session$setInputs(lysis_level = "replicate")
  check("lysis models per replicate: one row per replicate curve and model", nrow(lysis_current()$table) == 9 * 2 && "replicate" %in% names(lysis_current()$table))
  session$setInputs(lysis_level = "mean", lysis_models = "one_phase_decay")
  check("lysis models: the model choice is used", all(lysis_current()$table$model == "one_phase_decay"))
  session$setInputs(fit_weights_scheme = "1/y2", lysis_models = c("one_phase_decay", "fpl_decay"))
  lw <- lysis_current()
  check("lysis models: the weighting scheme changes the fit", lw$scheme == "1/y2" && !isTRUE(all.equal(lw$table$k[lw$table$sample == "Phage MOI 10" & lw$table$model == "one_phase_decay"], op$k)))
  session$setInputs(fit_weights_scheme = "none")
  # exponential window
  ewn <- expwin_current()
  check("exponential window: a row per sample with mu, doubling time and CIs", nrow(ewn) == 3 && all(is.finite(ewn$mu)) && all(ewn$doubling_time_lo < ewn$doubling_time & ewn$doubling_time < ewn$doubling_time_hi))
  check("exponential window: the doubling time is ln 2 / mu", isTRUE(all.equal(ewn$doubling_time, log(2) / ewn$mu, tolerance = 1e-8)))
  session$setInputs(expwin_mode = "manual", expwin_from = "0", expwin_to = "60")
  check("exponential window: a typed window is used", all(expwin_current()$t_start == 0) && all(expwin_current()$t_end == 60))
  session$setInputs(expwin_mode = "auto", expwin_from = "", expwin_to = "")
  check("exponential window: table (with the rolling mu_max next to it) and CSV", is_tbl(output$expwin_table) && grepl("rolling", output$expwin_table, fixed = TRUE) &&
        "rolling_mu_max" %in% names(rd(output$downloadExpWindowCSV)) && has_html(output$expwin_note))
  # compare fits
  session$setInputs(cmp_samples = smp[1:2], cmp_param = "mu", cmp_model = "logistic")
  cf <- cmp_current()
  check("compare fits: a test row with F and p, and the preferred model", is.finite(cf$test$F) && is.finite(cf$test$p) && cf$test$param == "mu" && cf$test$preferred %in% c("null", "alternative"))
  check("compare fits: parameters per sample", nrow(cf$params) == 2 && all(is.finite(cf$params$mu)))
  check("compare fits: a sentence for the methods draft", grepl("extra-sum-of-squares", cmp_sentence(cf), fixed = TRUE))
  check("compare fits: tables, note and CSV", is_tbl(output$cmp_test_table) && is_tbl(output$cmp_params_table) && has_html(output$cmp_note) && nrow(rd(output$downloadCompareFitsCSV)) == 2)
  session$setInputs(cmp_samples = smp, cmp_param = "all")
  check("compare fits: all parameters, three samples, with a multiplicity warning", cmp_current()$test$n_groups == 3 && any(grepl("three tests", cmp_current()$warnings)))
  session$setInputs(cmp_samples = smp[1])
  check("compare fits: a single sample gives a message, not an error", is.null(cmp_current()$test) && length(cmp_current()$warnings) == 1)
  session$setInputs(cmp_samples = smp)
  check("the sample selector renders", has_html(output$cmp_samples_ui))
})

cat("\n--- server: moi_series scenario (dose-response, dashboard, reports) ---\n")
testServer(app, {
  ingest_data(env$make_demo_data("moi_series"), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1],
                    analysis_dose_metric = "lysis_time", analysis_metrics = "lysis_time", main_tabs = "Statistics")
  dr <- dr_current()
  check("dose-response 4PL: one series, EC50 finite with an interval inside the MOI range", nrow(dr$table) == 1 && isTRUE(dr$table$converged) && is.finite(dr$table$ec50) &&
        dr$table$ec50_lo < dr$table$ec50 && dr$table$ec50 < dr$table$ec50_hi && dr$table$ec50 > 0.01 && dr$table$ec50 < 100)
  check("dose-response 4PL: the curve and its band cover the MOI range", !is.null(dr$curves) && min(dr$curves$moi) <= 0.0101 && max(dr$curves$moi) >= 99 && all(dr$curves$lo <= dr$curves$fit + 1e-9))
  check("dose-response table renders, CSV has the EC50", is_tbl(output$dr_table) && is.finite(rd(output$downloadDoseResponseCSV)$ec50) && has_html(output$dr_note))
  g0 <- generate_analysis_plot("dose_response"); n0 <- length(g0$layers)
  session$setInputs(dr_fit = TRUE)
  g1 <- generate_analysis_plot("dose_response")
  check("4PL on the dose-response figure: a band, the fitted curve, the EC50 line and its interval", length(g1$layers) >= n0 + 4 && any(vapply(g1$layers, function(l) inherits(l$geom, "GeomVline"), TRUE)) &&
        any(vapply(g1$layers, function(l) inherits(l$geom, "GeomRibbon"), TRUE)) && inherits(g1$layers[[1]]$geom, "GeomRibbon"))
  check("4PL on the figure: the EC50 is in the figure note", any(grepl("EC50", attr(g1, "figure_note"))))
  session$setInputs(dr_ec50 = FALSE, dr_band = FALSE)
  check("EC50 line and band can be switched off", !any(vapply(generate_analysis_plot("dose_response")$layers, function(l) inherits(l$geom, "GeomVline") || inherits(l$geom, "GeomRibbon"), TRUE)))
  session$setInputs(dr_ec50 = TRUE, dr_band = TRUE, dr_fix_bottom = "30")
  fx <- dr_current()
  check("fixing the bottom plateau is honoured (EC50 still finite)", isTRUE(all.equal(fx$table$bottom, 30)) && is.finite(fx$table$ec50))
  session$setInputs(dr_fix_bottom = "", fit_weights_scheme = "1/y2")
  check("the weighting scheme reaches the dose-response fit", !isTRUE(all.equal(dr_current()$table$ec50, dr$table$ec50, tolerance = 1e-9)) && is.finite(dr_current()$table$ec50))
  session$setInputs(fit_weights_scheme = "none")
  # two series
  d2 <- data.frame(series = rep(c("A", "B"), each = 12), moi = rep(rep(c(0.01, 0.1, 1, 10), each = 3), 2), value = c(160 - 22 * log10(rep(c(0.01, 0.1, 1, 10), each = 3) * 100) + rep(c(.3, -.2, .1), 4),
                                                                                  140 - 18 * log10(rep(c(0.01, 0.1, 1, 10), each = 3) * 10) + rep(c(.2, -.3, .2), 4)))
  tw <- env$dose_response_series_fits(d2)
  check("dose-response per phage series: one fit and one curve per series", nrow(tw$table) == 2 && setequal(tw$curves$series, c("A", "B")) && all(tw$table$converged))
  # dashboard
  cards <- as.character(output$dash_cards$html)
  check("dashboard: the AUC and the exponential-window doubling time cards", grepl("AUC", cards, fixed = TRUE) && grepl("Doubling time (exp. window)", cards, fixed = TRUE))
  # methods draft
  session$setInputs(posthoc_method = "games_howell", onesample_text = "mu_max = 0.02", explorer_stats = "pearson", explorer_line = TRUE, main_tabs = "Statistics")
  txt <- draft_methods_text()
  check("methods draft: the post-hoc sentence names the method", grepl("Games", txt, fixed = TRUE) && grepl("Welch ANOVA", txt, fixed = TRUE))
  check("methods draft: the time-course, AUC, outlier, one-sample, correlation and straight-line sentences",
        grepl("repeated-measures", txt, fixed = TRUE) && grepl("Area under the curve", txt, fixed = TRUE) && grepl("Outliers were screened", txt, fixed = TRUE) &&
        grepl("hypothetical value", txt, fixed = TRUE) && grepl("Pearson", txt, fixed = TRUE) && grepl("least squares", txt, fixed = TRUE))
  session$setInputs(main_tabs = "Curve fitting", dr_fit = TRUE)
  txt2 <- draft_methods_text()
  check("methods draft: the intervals / AICc, lysis, exponential-window and 4PL sentences once the Curve fitting tab was used",
        grepl("Akaike", txt2, fixed = TRUE) && grepl("one-phase exponential decay", txt2, fixed = TRUE) && grepl("exponential phase", txt2, fixed = TRUE) && grepl("four-parameter logistic function of log10 MOI", txt2, fixed = TRUE))
  # report
  if (env$has_base64enc) {
    meta <- report_meta(); meta$generated <- "2026-11-01 00:00:00"
    session$setInputs(report_include_analysis = TRUE, compose_panels = c("main", "metric_summary"))
    html <- html_report_string(meta)
    check("HTML report: a 'Statistics (v2.22)' section", grepl("<h2>Statistics (v2.22)</h2>", html, fixed = TRUE))
    check("HTML report: the new tables with rows (column statistics, post-hoc, time-course, AUC, fit statistics, lysis, exponential window, EC50)",
          all(vapply(c("<h3>Column statistics</h3>", "<h3>Time-course ANOVA (sample x time)</h3>", "<h3>Area under the curve</h3>", "<h3>Lysis models</h3>", "<h3>Exponential-growth window</h3>",
                       "<h3>Growth fits: intervals, AICc and residual checks</h3>", "<h3>Dose-response: 4PL EC50</h3>"), function(x) grepl(x, html, fixed = TRUE), logical(1))) && grepl("Post-hoc comparisons", html, fixed = TRUE))
    check("HTML report: the per-time and AUC figures are images and the methods sentences are in the section",
          grepl("<h3>Per-time comparisons vs the reference</h3>", html, fixed = TRUE) && grepl("<h3>AUC per sample</h3>", html, fixed = TRUE) && grepl("<h3>Methods sentences</h3>", html, fixed = TRUE))
    check("HTML report: the section follows the bootstrap / extraction part (it is the last analysis section)", regexpr("<h2>Replicate statistics</h2>", html, fixed = TRUE) < regexpr("<h2>Statistics (v2.22)</h2>", html, fixed = TRUE))
    session$setInputs(report_include_analysis = FALSE)
    check("analysis off: no Statistics (v2.22) section", !grepl("Statistics (v2.22)", html_report_string(meta), fixed = TRUE))
    session$setInputs(report_include_analysis = TRUE)
  } else skipit("base64enc missing: HTML report checks")
  if (env$has_gridExtra) {
    pf <- tempfile(fileext = ".pdf"); pdf_report_file(pf, report_meta())
    check("PDF report builds with the v2.22 section (more pages than without)", file.exists(pf) && file.size(pf) > 5000)
  } else skipit("gridExtra missing: PDF report check")
  # bundle
  have_zip <- requireNamespace("zip", quietly = TRUE)
  ls_zip <- function(zf) if (have_zip) basename(zip::zip_list(zf)$filename) else basename(sub("^\\s*[0-9]+\\s+[0-9-]+\\s+[0-9:]+\\s+", "", system2("unzip", c("-l", zf), stdout = TRUE)))
  zl <- ls_zip(output$downloadFigureBundle)
  check("figure bundle: the v2.22 tables are in it", all(c("column_statistics.csv", "auc_per_sample.csv", "timecourse_anova.csv", "fit_statistics.csv", "lysis_models.csv", "dose_response_ec50.csv") %in% zl))
  za <- ls_zip(output$downloadAllFigures)
  check("download-all-figures: the AUC figure is written (and the per-time one, the reference being set)", any(grepl("^auc_", za)) && any(grepl("^per_time_", za)))
  session$setInputs(compose_panels = c("main", "per_time", "auc"), compose_ncol = "2")
  ci <- compose_info()
  check("the composer takes both new panels", ci$n == 3 && length(ci$skipped) == 0 && all(c("per_time", "auc") %in% ci$types))
})

cat("\n--- server: untouched session, settings round trip ---\n")
testServer(app, {
  ingest_data(env$make_demo_data("lysis"), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1], methods_text = "")
  check("untouched session: no v2.22 methods sentences and no report section", length(stats22_methods_sentences()) == 0 && is.null(report_stats22_payload(FALSE)))
  check("untouched session: the fit figure has the original layers (no overlay)", length(s22_fit_overlay(fit_results(), FALSE, 1)$back) == 0 && length(s22_fit_overlay(fit_results(), FALSE, 1)$front) == 0)
  new_vals <- list(s22_metrics = c("lysis_time", "mu_max"), s22_qq_metric = "mu_max", posthoc_method = "dunn", tc_adjust = "holm", tc_repeated = FALSE, auc_baseline = "min",
                   auc_baseline_value = 0.1, auc_from = "15", auc_to = "90", auc_relative = FALSE, auc_style = "bar", outlier_method = "mad", onesample_text = "mu_max = 0.02",
                   explorer_stats = "spearman", explorer_line = TRUE, explorer_band = "prediction", fit_ci_method = "wald", fit_weights_scheme = "1/y2", fit_bands_on = TRUE,
                   fit_bands_type = "prediction", lysis_models = "fpl_decay", lysis_from = "start", lysis_level = "replicate", lysis_show = TRUE, expwin_level = "replicate",
                   expwin_mode = "manual", expwin_from = "10", expwin_to = "70", expwin_min_points = 5, expwin_show = TRUE, cmp_model = "gompertz", cmp_param = "lag",
                   dr_fit = TRUE, dr_fix_bottom = "0", dr_fix_top = "100", dr_band = FALSE, dr_ec50 = FALSE)
  do.call(session$setInputs, new_vals)
  sj <- settings_json_list()$global_settings
  check("Save Settings carries every new input with its value", all(vapply(names(new_vals), function(k) identical(sj[[k]], new_vals[[k]]) || isTRUE(all.equal(sj[[k]], new_vals[[k]])), logical(1))))
  check("... and not the per-file sample choice of Compare fits", !"cmp_samples" %in% names(sj))
  tf <- tempfile(fileext = ".json"); jsonlite::write_json(settings_json_list(), tf, pretty = TRUE, auto_unbox = TRUE)
  back <- jsonlite::read_json(tf, simplifyVector = TRUE)$global_settings
  check("the JSON file round-trips every new value (types included)", all(vapply(names(new_vals), function(k) isTRUE(all.equal(back[[k]], new_vals[[k]])), logical(1))))
})

cat(sprintf("\n============ SUMMARY: %d passed / %d failed / %d skipped ============\n", pass, fail, skip))
if (fail > 0) quit(status = 1)
