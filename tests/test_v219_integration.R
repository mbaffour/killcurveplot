# ============================================================================
# v2.19 UI integration of the v2.18 engine: the Curve fitting tab (fit_overlay figure, parameter tables), the
# phenotype heatmap, the extraction tables and the all-metrics comparison in the Statistics tab, the report /
# bundle / methods additions.
# Run from the repository root:  Rscript tests/test_v219_integration.R
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
quiet <- function(expr) suppressWarnings(suppressMessages(expr))

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.19.0 or later", package_version(env$APP_VERSION) >= "2.19.0")

cat("\n--- defaults, choices, UI ---\n")
d <- env$APP_DEFAULTS
check("fit inputs have APP_DEFAULTS entries (all three models, up to the peak, ln scale, mean curve, best model)",
      identical(d$fit_models, c("logistic", "gompertz", "baranyi")) && identical(d$fit_until, "peak") && isTRUE(d$fit_log) &&
      identical(d$fit_level, "mean") && identical(d$fit_show, "best"))
check("phenotype heatmap inputs: the eight default metrics, z-scores, cell values on",
      identical(d$pheno_metrics, c("lysis_time", "od_drop", "lysis_efficiency", "mu_max", "auc", "infection_strength",
                                   "extinction_time", "t_first_peak")) && identical(d$pheno_scale, "z") && isTRUE(d$pheno_values))
check("extraction inputs: no times, interpolate, threshold 0.5 up from the start",
      identical(d$extract_times, "") && identical(d$extract_method, "interpolate") && d$extract_threshold == 0.5 &&
      identical(d$extract_direction, "up") && identical(d$extract_after, "start"))
check("the Legacy look does not touch them (journal preset names only)",
      !any(grepl("^(fit_|pheno_|extract_)", names(env$legacy_preset_settings()))) &&
      !any(grepl("^(fit_|pheno_|extract_)", names(env$journal_preset_settings(89, 65)))))
check("both figures are analysis figures and composer panels",
      all(c("fit_overlay", "phenotype_heatmap") %in% env$ANALYSIS_FIGURE_CHOICES) &&
      all(c("fit_overlay", "phenotype_heatmap") %in% env$COMPOSE_PANEL_CHOICES))
ui_html <- as.character(env$ui)
check("UI: the Curve fitting tab and every new input / output",
      grepl("Curve fitting", ui_html, fixed = TRUE) &&
      all(vapply(c("fit_models", "fit_until", "fit_log", "fit_level", "fit_show", "pheno_metrics", "pheno_scale", "pheno_values",
                   "extract_times", "extract_method", "extract_threshold", "extract_direction", "extract_after",
                   "fit_container", "fit_table_ui", "downloadFitsCSV", "downloadFitFigure", "extract_od_table", "extract_tt_table",
                   "allmetrics_table", "downloadAllMetricsCSV", "downloadExtractODCSV", "downloadExtractTTCSV"),
                 function(id) grepl(paste0('id="', id, '"'), ui_html, fixed = TRUE), logical(1))))
check("the Curve fitting tab comes after the Statistics tab",
      regexpr('data-value="Statistics"', ui_html, fixed = TRUE) < regexpr('data-value="Curve fitting"', ui_html, fixed = TRUE))
check("the new Statistics sections come before the Methods text",
      regexpr("OD at chosen times", ui_html, fixed = TRUE) > 0 &&
      regexpr("All-metrics comparison", ui_html, fixed = TRUE) < regexpr("Methods text (editable)", ui_html, fixed = TRUE))

cat("\n--- pure helpers ---\n")
pt <- env$parse_extract_times("60, 30; 30  abc 120")
check("parse_extract_times: sorted unique numbers, words reported", identical(pt$times, c(30, 60, 120)) && identical(pt$invalid, "abc"))
check("parse_extract_times: empty / NULL", length(env$parse_extract_times("")$times) == 0 && length(env$parse_extract_times(NULL)$times) == 0)
tt <- seq(0, 300, by = 10)
f_log <- 3 / (1 + exp(4 * 0.03 / 3 * (20 - tt) + 2))
od_log <- 0.05 * exp(f_log - f_log[1])
raw <- env$fit_growth_models_by_sample(data.frame(time = tt, variable = "S", mean_value = od_log))
tab <- env$fit_table_prepare(raw)
check("fit_table_prepare: best flagged once, lag_abs = t0 + lag, doubling time = ln2 / mu_max",
      sum(tab$best) == 1 && tab$best[tab$model == "logistic"] && all.equal(tab$lag_abs, tab$t0 + tab$lag) &&
      isTRUE(all.equal(tab$doubling_time, log(2) / tab$mu_max)))
raw0 <- env$fit_growth_models_by_sample(data.frame(time = tt, variable = "S", mean_value = od_log), log_transform = FALSE)
check("without the ln transform there is no doubling time", all(is.na(env$fit_table_prepare(raw0)$doubling_time)))
win <- env$fit_windows_from(pd = data.frame(time = c(tt, 310), variable = "S", mean_value = c(od_log, 0.1)))
check("fit_windows_from: the fitted range stops at the peak (the point at 310 is after it)", max(win$S$t) == 300 && length(win$S$t) == 31)
cf <- env$fit_curve_frames(tab, win, "all")
check("fit_curve_frames: one curve per converged model, 200 grid points over the fitted range, starting at the first reading",
      length(unique(cf$model)) == 3 && sum(cf$model == "logistic") == 200 && min(cf$time) == 0 && max(cf$time) == 300 &&
      abs(cf$od[cf$model == "logistic"][1] - od_log[1]) < 1e-9)
check("fit_curve_frames(show = best): only the best model", identical(unique(env$fit_curve_frames(tab, win, "best")$model), "logistic"))
disp <- env$fit_display_table(tab)
check("fit_display_table: tick for the best model, 'Lag (time units)', 3 significant digits",
      identical(disp$Best, c("✓", "", "")) && "Lag (time units)" %in% names(disp) && all(nchar(disp$A) <= 6))

cat("\n--- server: Curve fitting ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1])
  ft <- fit_table_current()
  check("fit table: a row for every sample and model (3 x 3)", nrow(ft) == 9 && all(table(ft$sample) == 3) &&
        setequal(ft$sample, smp) && all(table(ft$model) == 3))
  check("every sample has exactly one best model, and it is the lowest AIC among its converged models",
        all(tapply(ft$best, ft$sample, sum) == 1) &&
        all(vapply(smp, function(s) { g <- ft[ft$sample == s & ft$converged, ]; g$model[which.min(g$aic)] == g$model[g$best] }, logical(1))))
  check("the table is in sample order", identical(unique(ft$sample), sample_levels(smp)))
  check("lag_abs = t0 + lag; doubling time = ln2 / mu_max", isTRUE(all.equal(ft$lag_abs, ft$t0 + ft$lag)) &&
        isTRUE(all.equal(ft$doubling_time, log(2) / ft$mu_max)))
  mo10 <- ft[ft$sample == "Phage MOI 10", ]
  check("MOI 10 peaks at 60: 5 points up to the peak, flagged as poorly determined in the message column",
        all(mo10$n_points == 5) && all(grepl("poorly determined", mo10$message)))
  p <- generate_analysis_plot("fit_overlay")
  check("fit_overlay renders a real figure (not the placeholder)",
        inherits(p, "ggplot") && !is_placeholder_plot(p) && !is.null(tryCatch(ggplot_build(p), error = function(e) NULL)))
  lay <- vapply(p$layers, function(l) class(l$geom)[1], "")
  b <- ggplot_build(p)
  check("figure layers: grey tail points, whiskers, one fitted line per sample (best only), the data points",
        sum(lay == "GeomLine") == 1 && sum(lay == "GeomLinerange") >= 1 && sum(lay == "GeomPoint") == 2)
  line_d <- b$data[[which(lay == "GeomLine")]]
  check("the fitted curves are drawn on a fine grid (200 points each)", all(table(line_d$group) == 200))
  pts_in <- b$data[[tail(which(lay == "GeomPoint"), 1)]]
  pts_out <- b$data[[head(which(lay == "GeomPoint"), 1)]]
  check("points after the peak are grey (the demo's lysed phage curves), the others in the sample colours",
        nrow(pts_out) > 0 && all(pts_out$colour == "grey60") && nrow(pts_in) + nrow(pts_out) == 39 &&
        !any(pts_in$colour == "grey60"))
  check("x axis spans the whole curve (to 180) and the legend / labels follow the main figure",
        max(b$layout$panel_params[[1]]$x.range) >= 180 && identical(p$labels$x, input$x_axis_label))
  # all converged models: the others are thin, dashed
  session$setInputs(fit_show = "all")
  p_all <- generate_analysis_plot("fit_overlay"); lay_a <- vapply(p_all$layers, function(l) class(l$geom)[1], "")
  dashed <- Filter(function(l) inherits(l$geom, "GeomLine") && identical(l$aes_params$linetype, "dashed"), p_all$layers)
  b_a <- ggplot_build(p_all)
  check("fit_show = all: solid best + dashed thin others (3 curves per sample in total)",
        sum(lay_a == "GeomLine") == 2 && length(dashed) == 1 &&
        sum(vapply(which(lay_a == "GeomLine"), function(i) length(unique(b_a$data[[i]]$group)), 1L)) == 9)
  session$setInputs(fit_show = "best")
  # log y follows y_scale_type
  session$setInputs(y_scale_type = "log")
  p_l <- generate_analysis_plot("fit_overlay"); bl <- ggplot_build(p_l)
  check("y_scale_type = log: a log10 y scale, still a real figure",
        !is_placeholder_plot(p_l) && grepl("log", bl$layout$panel_scales_y[[1]]$trans$name))
  session$setInputs(y_scale_type = "linear")
  # whole curve: nothing is grey
  session$setInputs(fit_until = "all")
  ft_all <- fit_table_current(); b_w <- ggplot_build(generate_analysis_plot("fit_overlay"))
  check("fit_until = all: more points fitted (13 for the phage that lyse) and no grey tail",
        all(ft_all$n_points[ft_all$sample == "Phage MOI 10"] == 13) && !any(vapply(b_w$data, function(x) "colour" %in% names(x) && any(x$colour == "grey60"), logical(1))))
  session$setInputs(fit_until = "peak", fit_log = FALSE)
  ft_nl <- fit_table_current()
  check("fit_log = FALSE: fits on the OD change (A is in OD units), no doubling time",
        all(is.na(ft_nl$doubling_time)) && all(!ft_nl$log_transform) && nrow(ft_nl) == 9)
  session$setInputs(fit_log = TRUE)
  # models subset and none
  session$setInputs(fit_models = "baranyi")
  check("one model selected: 3 rows, every row baranyi, still one best per sample",
        nrow(fit_table_current()) == 3 && all(fit_table_current()$model == "baranyi") && all(fit_table_current()$best))
  session$setInputs(fit_models = character(0))
  check("no model selected: no table and the 'not available' placeholder carrying the reason",
        is.null(fit_table_current()) && is_placeholder_plot(generate_analysis_plot("fit_overlay")) &&
        grepl("at least one model", placeholder_reason(generate_analysis_plot("fit_overlay")), fixed = TRUE))
  session$setInputs(fit_models = c("logistic", "gompertz", "baranyi"))
  # replicate level
  session$setInputs(fit_level = "replicate")
  ftr <- fit_table_current()
  check("fit_level = replicate: sample x replicate x model rows (27), a replicate column, best once per replicate",
        nrow(ftr) == 27 && "replicate" %in% names(ftr) && all(tapply(ftr$best, paste(ftr$sample, ftr$replicate), sum) == 1))
  sm <- env$fit_replicate_summary(ftr)
  check("replicate summary: per sample and model, mean and SD of A, mu_max, lag_abs over the converged replicates",
        nrow(sm) == 9 && all(c("A_mean", "A_sd", "mu_max_mean", "mu_max_sd", "lag_abs_mean", "lag_abs_sd", "n") %in% names(sm)) && all(sm$n <= 3))
  p_r <- generate_analysis_plot("fit_overlay"); b_r <- ggplot_build(p_r)
  lr <- which(vapply(p_r$layers, function(l) inherits(l$geom, "GeomLine"), logical(1)))
  check("replicate level: one thin curve per replicate (9) drawn over the mean-curve points",
        !is_placeholder_plot(p_r) && length(unique(b_r$data[[lr[1]]]$group)) == 9)
  tbl_ui <- as.character(output$fit_table_ui$html)
  check("the tab shows the table with a Replicate column and the 'Lag (time units)' label",
        grepl("Replicate", tbl_ui, fixed = TRUE) && grepl("Lag (time units)", tbl_ui, fixed = TRUE))
  check("'poorly determined' rows are highlighted in the table and listed in the note",
        grepl("background:#fff3cd", tbl_ui, fixed = TRUE) &&
        grepl("poorly determined", as.character(output$fit_note$html), fixed = TRUE))
  check("the note explains the fit range and the lag convention",
        grepl("up to its highest point", as.character(output$fit_note$html), fixed = TRUE) &&
        grepl("start time of the fit + lag", as.character(output$fit_note$html), fixed = TRUE))
  session$setInputs(fit_level = "mean")
  # downloads
  f1 <- output$downloadFitsCSV
  csv <- utils::read.csv(f1)
  check("parameters download: every row, unformatted, with lag_abs and the best flag",
        nrow(csv) == 9 && all(c("sample", "model", "best", "converged", "A", "mu_max", "lag_abs", "doubling_time", "r2", "aic", "se_A", "message") %in% names(csv)))
  session$setInputs(fit_level = "replicate")
  csv2 <- utils::read.csv(output$downloadFitsSummaryCSV)
  check("replicate summary download", nrow(csv2) == 9 && "mu_max_sd" %in% names(csv2))
  session$setInputs(fit_level = "mean")
  # computed once: the figure and the tables share one reactive
  check("fit_results is one reactive shared by the figure, the tables, the reports and the bundle (same table every call)",
        identical(isolate(fit_results())$table, fit_table_current()) && inherits(fit_results, "reactiveExpr"))
  # analysis download name, export svg, fixed-panel (square) and composer
  session$setInputs(analysis_type = "fit_overlay")
  check("analysis download name uses the type", grepl("^fit_overlay_fit_overlay_[0-9]+x[0-9]+mm\\.", analysis_download_name()))
  do.call(session$setInputs, env$journal_preset_settings(89, 65, "a"))
  fg <- fixed_figure(generate_analysis_plot("fit_overlay"), "fit_overlay")
  pc <- fg$layout[grepl("^panel", fg$layout$name), ]
  pw <- grid::convertWidth(fg$widths[pc$l[1]], "mm", valueOnly = TRUE); ph <- grid::convertHeight(fg$heights[pc$t[1]], "mm", valueOnly = TRUE)
  check("journal look + fixed panel: the fit figure's panel is square (65 x 65 mm)", abs(pw - 65) < 1e-6 && abs(ph - 65) < 1e-6)
  svgf <- tempfile(fileext = ".svg"); sz <- attr(fg, "size_mm")
  quiet(ggsave(svgf, fg, device = env$export_device("svg"), width = sz[1] / 25.4, height = sz[2] / 25.4, units = "in", bg = "white"))
  svg <- readLines(svgf)
  check("the fit figure SVG is Arial text with no textLength (shared theme)", any(grepl('font-family: "Arial"', svg)) && !any(grepl("textLength", svg)))
  fsvg <- output$downloadFitFigure
  check("Curve fitting tab figure download works", file.exists(fsvg) && file.size(fsvg) > 1000)
  session$setInputs(compose_panels = c("main", "fit_overlay", "phenotype_heatmap"))
  check("the composer accepts both new panels", compose_info()$n == 3 && length(compose_info()$skipped) == 0)
  session$setInputs(panel_size_mode = "fit", compose_panels = c("main", "metric_summary", "growth_rate", "normalized"))
  # methods sentence only once the tab was used
  m0 <- draft_methods_text()
  check("methods draft: no fitting sentence before the Curve fitting tab was used", !grepl("growth model", m0, fixed = TRUE))
  session$setInputs(main_tabs = "Curve fitting")
  m1 <- draft_methods_text()
  check("methods draft: a sentence on the models and the range once the tab was used",
        grepl("modified logistic", m1, fixed = TRUE) && grepl("Gompertz", m1, fixed = TRUE) && grepl("Baranyi-Roberts", m1, fixed = TRUE) &&
        grepl("up to the peak", m1, fixed = TRUE) && grepl("lowest Akaike", m1, fixed = TRUE) && grepl("ln(OD/OD0)", m1, fixed = TRUE) &&
        grepl("mean curve", m1, fixed = TRUE))
  session$setInputs(fit_until = "all", fit_level = "replicate", fit_models = "gompertz")
  m2 <- draft_methods_text()
  check("the sentence follows the settings (whole curve, replicates, one model)",
        grepl("over the whole curve", m2, fixed = TRUE) && grepl("individual replicate curves", m2, fixed = TRUE) &&
        grepl("Gompertz", m2, fixed = TRUE) && !grepl("Baranyi-Roberts", m2, fixed = TRUE))
  session$setInputs(fit_until = "peak", fit_level = "mean", fit_models = c("logistic", "gompertz", "baranyi"))
})

cat("\n--- server: fits are computed once, and only when their inputs or the data change ---\n")
n_fit_calls <- 0L
orig_by_sample <- env$fit_growth_models_by_sample
env$fit_growth_models_by_sample <- function(...) { n_fit_calls <<- n_fit_calls + 1L; orig_by_sample(...) }
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1],
                    methods_text = "", compose_panels = c("main", "metric_summary"), report_include_analysis = TRUE)
  # (testServer runs the outputs eagerly, so the Curve fitting tab's figure has already asked for the fits here)
  invisible(generate_analysis_plot("fit_overlay")); invisible(fit_table_current()); invisible(output$fit_table_ui)
  invisible(output$fit_note); invisible(output$downloadFitsCSV); invisible(generate_analysis_plot("fit_overlay"))
  check("figure, tables, note and CSV share one computation (1 fit call)", n_fit_calls == 1L)
  invisible(report_analysis_payload(FALSE))
  check("the report payload reuses it", n_fit_calls == 1L)
  session$setInputs(error_type = "sd", y_axis_label = "OD600", legend_position = "right")
  invisible(generate_analysis_plot("fit_overlay"))
  check("inputs that do not affect the fits (error measure, labels, legend) do not refit", n_fit_calls == 1L)
  session$setInputs(fit_until = "all")
  invisible(fit_table_current()); invisible(generate_analysis_plot("fit_overlay"))
  check("a fit input changes -> one refit", n_fit_calls == 2L)
  session$setInputs(selected_samples = smp[1:2])
  invisible(fit_table_current())
  check("the data (sample selection) changes -> one refit", n_fit_calls == 3L)
})
env$fit_growth_models_by_sample <- orig_by_sample

cat("\n--- server: phenotype heatmap ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1],
                    analysis_type = "phenotype_heatmap")
  pm <- pheno_current()
  p  <- generate_analysis_plot("phenotype_heatmap")
  check("phenotype_heatmap renders a real figure", inherits(p, "ggplot") && !is_placeholder_plot(p) &&
        !is.null(tryCatch(ggplot_build(p), error = function(e) NULL)))
  check("the matrix has the default metrics minus the dropped ones, one row per sample",
        identical(pm$metrics, setdiff(env$APP_DEFAULTS$pheno_metrics, pm$dropped)) && nrow(pm$matrix) == 3 &&
        ncol(pm$matrix) == length(env$APP_DEFAULTS$pheno_metrics) - length(pm$dropped))
  b <- ggplot_build(p)
  tiles <- b$data[[1]]
  check("tiles: samples x metrics", nrow(tiles) == nrow(pm$matrix) * ncol(pm$matrix) && inherits(p$layers[[1]]$geom, "GeomTile"))
  check("z scale: a diverging blue-white-red fill centred at 0 with symmetric limits, legend title 'z-score'",
        identical(p$scales$get_scales("fill")$name, "z-score") &&
        isTRUE(all.equal(p$scales$get_scales("fill")$limits, -rev(p$scales$get_scales("fill")$limits))) &&
        inherits(p$scales$get_scales("fill"), "ScaleContinuous") &&
        identical(toupper(p$scales$get_scales("fill")$palette(0.5)), "#FFFFFF"))
  lbl <- p$layers[[2]]
  check("cell text: the raw value in the axis-text size", inherits(lbl$geom, "GeomText") &&
        abs(lbl$aes_params$size - input$axis_text_font_size / ggplot2::.pt) < 1e-9 && nrow(p$data) == nrow(tiles))
  check("rows follow the sample order (first sample on top); columns are the wrapped metric labels",
        identical(levels(p$data$sample), rev(sample_levels(smp))) &&
        all(levels(p$data$metric) %in% env$pheno_metric_label(pm$metrics)))
  check("legend sits at the right (a tile figure)", identical(p$theme$legend.position, "right"))
  session$setInputs(pheno_values = FALSE)
  check("pheno_values off: no text layer", !any(vapply(generate_analysis_plot("phenotype_heatmap")$layers, function(l) inherits(l$geom, "GeomText"), logical(1))))
  session$setInputs(pheno_values = TRUE, pheno_scale = "range")
  pr <- generate_analysis_plot("phenotype_heatmap")
  check("range scale: viridis, legend title 'scaled', fill in 0..1",
        identical(pr$scales$get_scales("fill")$name, "scaled") && !is_placeholder_plot(pr) &&
        all(range(pr$data$value, na.rm = TRUE) %in% c(0, 1)))
  session$setInputs(pheno_scale = "none", pheno_metrics = c("lysis_time", "od_drop"))
  pn <- generate_analysis_plot("phenotype_heatmap")
  check("none scale with several metrics: raw values, generic 'value' title", identical(pn$scales$get_scales("fill")$name, "value"))
  session$setInputs(pheno_metrics = "auc")
  check("none scale with one metric: its unit (value x time) is the legend title",
        grepl("\u00d7 min", generate_analysis_plot("phenotype_heatmap")$scales$get_scales("fill")$name, fixed = TRUE))
  session$setInputs(pheno_scale = "z", pheno_metrics = c("lysis_time", "od_drop", "auc", "centroid_index"))
  # dropped metrics noted: no reference sample -> relative metrics are undefined
  session$setInputs(metrics_ref_sample = "", pheno_metrics = env$APP_DEFAULTS$pheno_metrics)
  pm2 <- pheno_current()
  check("without a reference sample infection_strength is dropped, the others stay",
        "infection_strength" %in% pm2$dropped && !"infection_strength" %in% pm2$metrics && length(pm2$metrics) == 7)
  note <- as.character(output$pheno_note$html)
  check("the dropped metrics are listed in a note", grepl("Infection strength", note, fixed = TRUE) && grepl("Left out", note, fixed = TRUE))
  check("the figure still renders without them", !is_placeholder_plot(generate_analysis_plot("phenotype_heatmap")))
  session$setInputs(pheno_metrics = character(0))
  check("no metric selected: the placeholder says so", is_placeholder_plot(generate_analysis_plot("phenotype_heatmap")))
  session$setInputs(pheno_metrics = env$APP_DEFAULTS$pheno_metrics, metrics_ref_sample = smp[1])
  # one sample: not available
  session$setInputs(selected_samples = smp[2])
  check("one sample: not available (placeholder), no error", is_placeholder_plot(generate_analysis_plot("phenotype_heatmap")))
  session$setInputs(selected_samples = smp)
  # not forced square: the panel is width x height as given
  session$setInputs(panel_size_mode = "fixed", panel_width_mm = 65, panel_height_mm = 40, panel_square = TRUE)
  dims <- panel_dims_mm("phenotype_heatmap")
  check("phenotype_heatmap is a non-curve kind: panel 65 x 40 mm even with 'square panel' on; fit_overlay stays square",
        identical(dims, c(65, 40)) && identical(panel_dims_mm("fit_overlay"), c(65, 65)))
  fgp <- fixed_figure(generate_analysis_plot("phenotype_heatmap"), "phenotype_heatmap")
  pcp <- fgp$layout[grepl("^panel", fgp$layout$name), ]
  check("fixed panel: the heatmap panel is 65 x 40 mm",
        abs(grid::convertWidth(fgp$widths[pcp$l[1]], "mm", valueOnly = TRUE) - 65) < 1e-6 &&
        abs(grid::convertHeight(fgp$heights[pcp$t[1]], "mm", valueOnly = TRUE) - 40) < 1e-6)
  session$setInputs(panel_size_mode = "fit")
})

cat("\n--- server: extraction tables ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1])
  check("no times typed: no table, a hint", is.null(od_at_times_summary_current()) && grepl("Type one or more times", as.character(output$extract_od_note$html), fixed = TRUE))
  session$setInputs(extract_times = "30, 60")
  s <- od_at_times_summary_current()
  check("extract_times '30, 60': 2 times x 3 samples = 6 rows, n = 3 each",
        nrow(s) == 6 && all(s$n == 3) && identical(names(s), c("time", "sample", "mean", "sd", "n", "sem", "ci95")) &&
        setequal(unique(s$time), c(30, 60)) && setequal(unique(s$sample), smp))
  rp <- prepare_replicate_data()
  v <- rp$value[rp$variable == "Uninfected" & rp$time == 30]
  check("the mean at a sampled time is the mean of the replicates", abs(s$mean[s$sample == "Uninfected" & s$time == 30] - mean(v)) < 1e-12)
  session$setInputs(extract_times = "37.5")
  s2 <- od_at_times_summary_current()
  y <- vapply(1:3, function(r) { g <- rp[rp$variable == "Uninfected" & rp$replicate == r, ]; approx(g$time, g$value, 37.5)$y }, 0)
  check("a time between readings is interpolated (default)", abs(s2$mean[s2$sample == "Uninfected"] - mean(y)) < 1e-12)
  session$setInputs(extract_method = "nearest")
  s3 <- od_at_times_summary_current()
  g1 <- rp[rp$variable == "Uninfected" & rp$replicate == 1, ]
  check("'nearest' takes the reading at the closest sampled time (37.5 -> 45 is 7.5 away, 30 is 7.5 away; either is a sampled value)",
        all(vapply(1:3, function(r) { g <- rp[rp$variable == "Uninfected" & rp$replicate == r, ]; any(abs(g$value - od_at_times_rep_current()$value[od_at_times_rep_current()$sample == "Uninfected"][r]) < 1e-12) }, logical(1))) && nrow(s3) == 3)
  session$setInputs(extract_method = "interpolate", extract_times = "30, 500, abc")
  s4 <- od_at_times_summary_current()
  check("a time outside the curves is NA (n = 0), never extrapolated; a word that is not a time is reported",
        all(s4$n[s4$time == 500] == 0) && all(is.na(s4$mean[s4$time == 500])) &&
        grepl("abc", as.character(output$extract_od_note$html), fixed = TRUE) && grepl("never extrapolated", as.character(output$extract_od_note$html), fixed = TRUE))
  session$setInputs(extract_times = "30, 60")
  check("the table renders and both downloads carry the data",
        grepl("Phage MOI 10", output$extract_od_table, fixed = TRUE) &&
        nrow(utils::read.csv(output$downloadExtractODCSV)) == 18 && nrow(utils::read.csv(output$downloadExtractODSummaryCSV)) == 6)
  # filters / windows are respected
  session$setInputs(enable_time_filter = TRUE, time_filter_range = c(0, 45))
  s5 <- od_at_times_summary_current()
  check("respects the time filter: 60 is outside the 0-45 window -> NA", all(s5$n[s5$time == 60] == 0) && all(s5$n[s5$time == 30] == 3))
  session$setInputs(enable_time_filter = FALSE)
  # time to threshold
  check("default threshold 0.5, up from the start: the lysing phage never reaches it except MOI 0.1? (finite or NA, one row per sample)",
        nrow(time_to_od_summary_current()) == 3)
  session$setInputs(extract_threshold = 0.15, extract_direction = "down", extract_after = "peak")
  tts <- time_to_od_summary_current(); ttr <- time_to_od_rep_current()
  check("time to 0.15 OD, down after the peak: finite for both phage samples (n = 3), NA for Uninfected (n = 0)",
        all(tts$n[tts$sample != "Uninfected"] == 3) && all(is.finite(tts$mean[tts$sample != "Uninfected"])) &&
        tts$n[tts$sample == "Uninfected"] == 0 && is.na(tts$mean[tts$sample == "Uninfected"]) && all(tts$n_curves == 3))
  check("per-replicate crossings: 9 rows, sample / replicate / time / threshold / direction",
        nrow(ttr) == 9 && all(c("sample", "replicate", "time", "threshold", "direction") %in% names(ttr)) &&
        all(ttr$direction == "down") && all(ttr$threshold == 0.15))
  check("the note names the sample that never crosses", grepl("Uninfected", as.character(output$extract_tt_note$html), fixed = TRUE) &&
        grepl("downwards", as.character(output$extract_tt_note$html), fixed = TRUE) && grepl("after its peak", as.character(output$extract_tt_note$html), fixed = TRUE))
  check("table and downloads", grepl("Phage MOI 0.1", output$extract_tt_table, fixed = TRUE) &&
        nrow(utils::read.csv(output$downloadExtractTTCSV)) == 9 && nrow(utils::read.csv(output$downloadExtractTTSummaryCSV)) == 3)
  session$setInputs(extract_direction = "up", extract_after = "start", extract_threshold = 0.5)
  tu <- time_to_od_summary_current()
  check("up from the start at 0.5: Uninfected crosses (grows to ~1.15), MOI 10 never does (peak 0.23)",
        tu$n[tu$sample == "Uninfected"] == 3 && tu$n[tu$sample == "Phage MOI 10"] == 0)
  session$setInputs(extract_threshold = NA)
  check("a cleared threshold box: no table, nothing configured", is.null(time_to_od_summary_current()) &&
        !"time_to_threshold_summary.csv" %in% names(extraction_tables_current()))
  session$setInputs(extract_threshold = 0.5, extract_times = "")
  check("extraction_tables_current: only the configured tables (threshold set, no times -> the two threshold tables)",
        identical(sort(names(extraction_tables_current())), c("time_to_threshold_per_replicate.csv", "time_to_threshold_summary.csv")))
  # per-sample window
  id1 <- env$safe_id(smp[1])
  session$setInputs(extract_times = "60")
  do.call(session$setInputs, setNames(list(30), paste0("tw_max_", id1)))
  s6 <- od_at_times_summary_current()
  check("respects a per-sample time window (Uninfected cut at 30: no value at 60)", s6$n[s6$sample == smp[1]] == 0 && s6$n[s6$sample == smp[2]] == 3)
})

cat("\n--- server: all-metrics comparison ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1])
  res <- all_metrics_current()
  check("all-metrics table has p_adj_bh_across, p_adj_holm, stars and the metric labels",
        !is.null(res$table) && all(c("metric", "metric_label", "p_adj_bh_across", "p_adj_holm", "stars", "hedges_g") %in% names(res$table)))
  check("many metrics (more than the 8 default heatmap ones), each sample vs the reference (at most 2 comparisons per metric; lysis onset, undefined for the Uninfected reference, falls back to the one pair)",
        length(unique(res$table$metric)) > 8 && all(table(res$table$metric) <= 2) && max(table(res$table$metric)) == 2 &&
        all(res$table$versus[res$table$metric == "max_od"] == smp[1]) && !"n_nonpositive_od" %in% res$table$metric)
  check("p_adj_bh_across is the BH adjustment over the whole table", isTRUE(all.equal(res$table$p_adj_bh_across, p.adjust(res$table$p, "BH"))))
  check("the rendered table has the column; the notes carry the engine's warnings",
        grepl("p_adj_bh_across", output$allmetrics_table, fixed = TRUE) && grepl("Welch t", as.character(output$allmetrics_notes$html), fixed = TRUE))
  check("the exploratory sentence is in the UI", grepl("exploratory", ui_html, fixed = TRUE) && grepl("p_adj_bh_across", ui_html, fixed = TRUE))
  csv <- utils::read.csv(output$downloadAllMetricsCSV)
  check("CSV download: all rows, unformatted", nrow(csv) == nrow(res$table) && "p_adj_bh_across" %in% names(csv))
  session$setInputs(stats_method = "nonparametric")
  check("follows the statistics method", all(all_metrics_current()$table$test == all_metrics_current()$table$test[1]) &&
        grepl("Mann", all_metrics_current()$table$test[1]) && grepl("Mann-Whitney", as.character(output$allmetrics_notes$html), fixed = TRUE))
  session$setInputs(stats_method = "welch", metrics_ref_sample = "")
  r0 <- all_metrics_current()
  check("no reference sample: all pairs (up to 3 per metric), relative metrics have no finite values and are left out",
        max(table(r0$table$metric)) == 3 && all(table(r0$table$metric) <= 3) && !"infection_strength" %in% r0$table$metric)
})

cat("\n--- server: reports, bundle, export all ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1],
                    methods_text = "", compose_panels = c("main", "metric_summary", "growth_rate", "normalized"),
                    analysis_metrics = c("lysis_time", "od_drop"), report_include_analysis = TRUE)
  n_img <- function(h) lengths(regmatches(h, gregexpr("<img ", h, fixed = TRUE)))
  no_b64 <- function(h) gsub("base64,[A-Za-z0-9+/=]+", "base64,", h)
  n_pages <- function(f) length(grepRaw("/Type /Page[^s]", readBin(f, "raw", file.size(f)), all = TRUE))
  if (env$has_base64enc && env$has_gridExtra) {
    meta <- report_meta(); meta$generated <- "2026-10-20 00:00:00"
    session$setInputs(extract_times = "")
    html <- html_report_string(meta)
    check("HTML report (analysis on): a 'Curve fits' section with the figure and the parameter table",
          grepl("<h2>Curve fits</h2>", html, fixed = TRUE) && grepl("<h3>Fitted growth curves</h3>", html, fixed = TRUE) &&
          grepl("<h3>Fitted parameters</h3>", html, fixed = TRUE) && grepl("<th>Lag (time units)</th>", html, fixed = TRUE) &&
          grepl("&#10003;|✓", html))
  check("the fit figure is an image (main, metric summary, growth rate, normalized, landmarks, dose-response, heatmap, composed = 8 + the fit figure)",
          n_img(html) == 9)
    check("the section comes after the analysis figures and before the replicate statistics",
          regexpr("<h3>Curve landmarks</h3>", html, fixed = TRUE) < regexpr("<h2>Curve fits</h2>", html, fixed = TRUE) &&
          regexpr("<h2>Curve fits</h2>", html, fixed = TRUE) < regexpr("<h2>Replicate statistics</h2>", html, fixed = TRUE))
    check("the phenotype heatmap is in the report, before the composed figure",
          grepl("<h3>Phenotype heatmap</h3>", html, fixed = TRUE) &&
          regexpr("<h3>Phenotype heatmap</h3>", html, fixed = TRUE) < regexpr("<h3>Composed figure</h3>", html, fixed = TRUE))
    check("the 'poorly determined' messages are listed in the report", grepl("poorly determined", html, fixed = TRUE))
    check("default threshold 0.5 is set: the Time to threshold table is in; no times typed: no OD-at-times table",
          grepl("<h3>Time to threshold</h3>", html, fixed = TRUE) && !grepl("<h3>OD at chosen times</h3>", html, fixed = TRUE))
    session$setInputs(extract_times = "30, 60")
    html2 <- html_report_string(meta)
    check("times typed: the OD at chosen times table appears (and the threshold table)",
          grepl("<h3>OD at chosen times</h3>", html2, fixed = TRUE) && grepl("<h3>Time to threshold</h3>", html2, fixed = TRUE) &&
          grepl("Extracted values", html2, fixed = TRUE))
    session$setInputs(extract_threshold = NA)
    html3 <- html_report_string(meta)
    check("a cleared threshold box leaves the threshold table out", !grepl("<h3>Time to threshold</h3>", html3, fixed = TRUE) && grepl("<h3>OD at chosen times</h3>", html3, fixed = TRUE))
    session$setInputs(extract_threshold = 0.5, extract_times = "")
    session$setInputs(fit_level = "replicate")
    html4 <- html_report_string(meta)
    check("replicate-level fits add the mean +/- SD table", grepl("Replicate fits: mean", html4, fixed = TRUE))
    session$setInputs(fit_level = "mean", report_include_analysis = FALSE)
    html_off <- html_report_string(meta)
    check("analysis off: no Curve fits, no heatmap, no extraction, one image",
          !grepl("Curve fits", html_off, fixed = TRUE) && !grepl("Phenotype heatmap", html_off, fixed = TRUE) &&
          !grepl("Extracted values", html_off, fixed = TRUE) && n_img(html_off) == 1)
    session$setInputs(report_include_analysis = TRUE, extract_times = "30, 60")
    pdf_on <- tempfile(fileext = ".pdf"); pdf_report_file(pdf_on, meta)
    session$setInputs(report_include_analysis = FALSE)
    pdf_off <- tempfile(fileext = ".pdf"); pdf_report_file(pdf_off, meta)
    check("PDF report builds with the fits, the heatmap and the extraction tables (more pages than without analysis)",
          identical(readBin(pdf_on, "raw", 4), charToRaw("%PDF")) && n_pages(pdf_on) >= n_pages(pdf_off) + 10)
    cat(sprintf("   -> PDF pages: on = %d, off = %d\n", n_pages(pdf_on), n_pages(pdf_off)))
    session$setInputs(report_include_analysis = TRUE, extract_times = "")
  } else skipit("report tests need base64enc and gridExtra")

  # figure bundle
  have_zip <- requireNamespace("zip", quietly = TRUE)
  ls_zip <- function(zf) if (have_zip) basename(zip::zip_list(zf)$filename) else
    basename(sub("^\\s*[0-9]+\\s+[0-9-]+\\s+[0-9:]+\\s+", "", system2("unzip", c("-l", zf), stdout = TRUE)))
  session$setInputs(extract_times = "30, 60")
  zb <- output$downloadFigureBundle
  zl <- ls_zip(zb)
  check("figure bundle: fits.csv and the four extraction CSVs next to the existing files",
        all(c("fits.csv", "od_at_times_summary.csv", "od_at_times_per_replicate.csv", "time_to_threshold_summary.csv",
              "time_to_threshold_per_replicate.csv", "curve_metrics.csv", "settings.json", "README.txt") %in% zl))
  exb <- tempfile("b_"); dir.create(exb); utils::unzip(zb, exdir = exb)
  fits_csv <- utils::read.csv(list.files(exb, "^fits\\.csv$", recursive = TRUE, full.names = TRUE))
  check("fits.csv is the whole fit table", nrow(fits_csv) == 9 && "lag_abs" %in% names(fits_csv))
  check("the README lists them", any(grepl("fits.csv", readLines(list.files(exb, "README.txt", recursive = TRUE, full.names = TRUE)), fixed = TRUE)))
  session$setInputs(extract_times = "", extract_threshold = NA)
  zl2 <- ls_zip(output$downloadFigureBundle)
  check("nothing configured: no extraction CSVs (fits.csv stays)", "fits.csv" %in% zl2 && !any(grepl("^(od_at_times|time_to_threshold)", zl2)))
  session$setInputs(extract_threshold = 0.5)
  # export all
  zf <- output$downloadAllFigures
  zla <- ls_zip(zf)
  check("export-all archive lists fit_overlay_*.svg / .png and phenotype_heatmap_*.svg / .png",
        any(grepl("^fit_overlay_.*\\.svg$", zla)) && any(grepl("^phenotype_heatmap_.*\\.svg$", zla)) &&
        any(grepl("^fit_overlay_.*\\.png$", zla)) && any(grepl("^phenotype_heatmap_.*\\.png$", zla)))
  check("export-all: no SKIPPED.txt for the demo", !"SKIPPED.txt" %in% zla)
  exa <- tempfile("a_"); dir.create(exa); utils::unzip(zf, exdir = exa)
  svg <- list.files(exa, "^phenotype_heatmap_.*\\.svg$", recursive = TRUE, full.names = TRUE)[1]
  check("the exported heatmap SVG is a real figure", length(readLines(svg)) > 50)
})

cat("\n--- server: settings round trip ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, fit_models = c("logistic", "baranyi"), fit_until = "all", pheno_scale = "range",
                    extract_times = "30, 60")
  sj <- settings_json_list()
  check("Save Settings carries the new global inputs",
        identical(sj$global_settings$fit_models, c("logistic", "baranyi")) && identical(sj$global_settings$fit_until, "all") &&
        identical(sj$global_settings$pheno_scale, "range") && identical(sj$global_settings$extract_times, "30, 60") &&
        identical(sj$global_settings$pheno_metrics, env$APP_DEFAULTS$pheno_metrics))
})

cat(sprintf("\n============ SUMMARY: %d passed / %d failed / %d skipped ============\n", pass, fail, skip))
if (fail > 0) quit(status = 1)
