# ============================================================================
# v2.18 UI-level additions: per-sample time windows, sample groups and facets, the
# "one panel per group" composer panel, the metric explorer figure, "Download all
# figures (.zip)".
# Run from the repository root:  Rscript tests/test_v218_features.R
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
# set_in(session, "id1", v1, "id2", v2, ...): session$setInputs() for input ids built at run time
set_in <- function(sess, ...) { a <- list(...); do.call(sess$setInputs, stats::setNames(a[c(FALSE, TRUE)], unlist(a[c(TRUE, FALSE)]))) }

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.18.0 or later", package_version(env$APP_VERSION) >= "2.18.0")

cat("\n--- defaults, legacy preset, choices ---\n")
check("facet_by_group: off in the journal look and the Legacy look",
      isFALSE(env$APP_DEFAULTS$facet_by_group) && isFALSE(env$legacy_preset_settings()$facet_by_group) &&
      isFALSE(env$journal_preset_settings(89, 65)$facet_by_group))
check("explorer inputs have APP_DEFAULTS entries",
      identical(env$APP_DEFAULTS$explorer_x, "lysis_time") && identical(env$APP_DEFAULTS$explorer_y, "od_drop") &&
      identical(env$APP_DEFAULTS$explorer_level, "Per replicate") && isFALSE(env$APP_DEFAULTS$explorer_labels) &&
      isFALSE(env$APP_DEFAULTS$explorer_logx) && isFALSE(env$APP_DEFAULTS$explorer_logy))
check("explorer is an analysis figure; the composer has 'one panel per group' after the main curve",
      "metric_explorer" %in% env$ANALYSIS_FIGURE_CHOICES &&
      identical(unname(env$COMPOSE_PANEL_CHOICES)[1:2], c("main", "main_by_group")) &&
      identical(names(env$COMPOSE_PANEL_CHOICES)[2], "Lysis curve, one panel per group"))
ui_html <- as.character(env$ui)
check("UI has the new inputs and buttons",
      all(vapply(c("time_windows_ui", "facet_by_group", "explorer_x", "explorer_y", "explorer_level", "explorer_labels",
                   "explorer_logx", "explorer_logy", "downloadAllFigures"),
                 function(id) grepl(paste0('id="', id, '"'), ui_html, fixed = TRUE), logical(1))))

cat("\n--- sample_group_map(): pure function ---\n")
gm <- env$sample_group_map(c("a", "b", "c"), c(a = "G1", b = "G1", c = "G2"))
check("two groups: map and level order", identical(as.vector(gm), c("G1", "G1", "G2")) && identical(attr(gm, "levels"), c("G1", "G2")))
gm2 <- env$sample_group_map(c("a", "b", "c"), c(a = "", b = "G2", c = "G1"))
check("empty group -> 'Other', placed last; named groups in first-appearance order",
      identical(as.vector(gm2), c("Other", "G2", "G1")) && identical(attr(gm2, "levels"), c("G2", "G1", "Other")))
check("one distinct group (or none) -> NULL for the facets; min_groups = 1 accepts one",
      is.null(env$sample_group_map(c("a", "b"), c(a = "G", b = "G"))) && is.null(env$sample_group_map(c("a", "b"), c(a = "", b = ""))) &&
      is.null(env$sample_group_map(c("a", "b"), c(a = "", b = ""), min_groups = 1L)) &&
      !is.null(env$sample_group_map(c("a", "b"), c(a = "G", b = ""), min_groups = 1L)) && is.null(env$sample_group_map("a", NULL)))

cat("\n--- server: time windows ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1])
  n_pts <- function(d, s) sum(d$variable == s)
  pd0 <- prepare_plot_data(); rp0 <- prepare_replicate_data()
  check("demo: 13 time points per sample before any window", all(table(pd0$variable) == 13))
  check("no window set: apply_time_windows_inputs() is the identity", nrow(apply_time_windows_inputs(pd0)) == nrow(pd0) && !nzchar(time_windows_text()))
  id1 <- env$safe_id(smp[2])
  set_in(session, paste0("tw_max_", id1), 60)
  pd1 <- prepare_plot_data(); rp1 <- prepare_replicate_data()
  check("tw_max on one sample: only that sample's points drop (to <= 60, i.e. 5 of 13); the others stay at 13",
        n_pts(pd1, smp[2]) == 5 && max(pd1$time[pd1$variable == smp[2]]) <= 60 &&
        all(vapply(smp[-2], function(s) n_pts(pd1, s) == 13, logical(1))))
  check("the replicate data follows the same window (5 time points x 3 replicates for that sample)",
        sum(rp1$variable == smp[2]) == 15 && all(vapply(smp[-2], function(s) sum(rp1$variable == s) == 39, logical(1))) &&
        all(c("time", "variable", "replicate", "value") %in% names(rp1)))
  check("both frames keep their columns", identical(names(pd1), names(pd0)) && identical(names(rp1), names(rp0)))
  set_in(session, paste0("tw_min_", id1), 30)
  pd2 <- prepare_plot_data()
  check("from + to: a closed window (30..60 = 3 points)", n_pts(pd2, smp[2]) == 3 && min(pd2$time[pd2$variable == smp[2]]) >= 30)
  set_in(session, paste0("tw_min_", id1), NA, paste0("tw_max_", id1), NA)
  check("blank (NA) boxes = no limit again", all(table(prepare_plot_data()$variable) == 13))
  # works with the global filter on as well (applied after it)
  session$setInputs(enable_time_filter = TRUE, time_filter_range = c(0, 120))
  set_in(session, paste0("tw_max_", id1), 30)
  pd3 <- prepare_plot_data()
  check("after the global filter: other samples 9 points (0..120), the windowed one 3 (0..30)",
        n_pts(pd3, smp[2]) == 3 && all(vapply(smp[-2], function(s) n_pts(pd3, s) == 9, logical(1))))
  check("provenance: report_meta()$filter_txt lists the window",
        grepl("per-sample windows:", report_meta()$filter_txt, fixed = TRUE) && grepl(smp[2], report_meta()$filter_txt, fixed = TRUE) &&
        grepl("up to 30", report_meta()$filter_txt, fixed = TRUE))
  check("the figure legend mentions the window", grepl("Per-sample time windows:", figure_legend_string(), fixed = TRUE))
  # save / load
  sj <- settings_json_list()
  check("Save Settings: the window is saved per sample (tw_max), blank limits are omitted",
        identical(sj$sample_aesthetics[[smp[2]]]$tw_max, 30) && is.null(sj$sample_aesthetics[[smp[2]]]$tw_min) &&
        is.null(sj$sample_aesthetics[[smp[1]]]$tw_max) && identical(sj$sample_aesthetics[[smp[1]]]$group, ""))
  check("per-sample inputs are not in global_settings (tw_ and group_ prefixes)",
        !any(grepl("^(tw_min_|tw_max_|group_)", names(sj$global_settings))))
  set_in(session, paste0("group_", id1), "G")
  check("the group is saved with legend_label's siblings", identical(settings_json_list()$sample_aesthetics[[smp[2]]]$group, "G"))
  session$setInputs(enable_time_filter = FALSE)
  # the sidebar table: one tw_min / tw_max pair per selected sample
  ui <- as.character(output$time_windows_ui$html)
  check("the per-sample table has a from / to pair for each selected sample",
        all(vapply(smp, function(s) grepl(paste0('id="tw_min_', env$safe_id(s), '"'), ui, fixed = TRUE) &&
                                    grepl(paste0('id="tw_max_', env$safe_id(s), '"'), ui, fixed = TRUE), logical(1))))
  # a loaded settings file seeds the table
  rv$imported_settings <- list(global = list(), samples = setNames(list(list(tw_min = 15, tw_max = 90, group = "X")), smp[1]))
  session$flushReact()
  ui2 <- as.character(output$time_windows_ui$html)
  check("imported per-sample windows are the table's values",
        grepl(sprintf('id="tw_min_%s"[^>]*value="15"', env$safe_id(smp[1])), ui2) &&
        grepl(sprintf('id="tw_max_%s"[^>]*value="90"', env$safe_id(smp[1])), ui2))
  rv$imported_settings <- NULL
})

cat("\n--- server: sample groups, facets, composer ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1])
  gid <- function(s) paste0("group_", env$safe_id(s))
  set_in(session, gid(smp[1]), "A", gid(smp[2]), "A", gid(smp[3]), "B")
  check("resolve_aesthetics() carries the groups", identical(unname(resolve_aesthetics(smp)$groups), c("A", "A", "B")))
  p0 <- generate_plot()
  check("facet_by_group off (default): no facets", inherits(p0$facet, "FacetNull"))
  session$setInputs(facet_by_group = TRUE)
  p1 <- generate_plot(); b1 <- ggplot_build(p1)
  check("facet_by_group on, groups A/A/B: a facet_wrap with two panels", inherits(p1$facet, "FacetWrap") && nrow(b1$layout$layout) == 2)
  check("the panels are the groups A and B (strip text), shared fixed scales",
        identical(as.character(b1$layout$layout$group), c("A", "B")) && isFALSE(p1$facet$params$free$x) && isFALSE(p1$facet$params$free$y))
  th <- p1$theme
  check("strips: no background, axis-title size",
        inherits(th$strip.background, "element_blank") && th$strip.text$size == input$axis_label_font_size)
  pts <- b1$data[[which(vapply(p1$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))[1]]]
  check("points: A holds two samples x 13, B one x 13", table(pts$PANEL)[["1"]] == 26 && table(pts$PANEL)[["2"]] == 13)
  # one group only -> no facets; an empty group -> Other
  set_in(session, gid(smp[3]), "A")
  check("a single distinct group: no facets", inherits(generate_plot()$facet, "FacetNull"))
  set_in(session, gid(smp[3]), "")
  check("A, A and an empty group: still one named group -> no facets", inherits(generate_plot()$facet, "FacetNull"))
  set_in(session, gid(smp[2]), "B", gid(smp[3]), "")
  p2 <- generate_plot(); b2 <- ggplot_build(p2)
  check("A, B and an empty group: three panels A, B, Other", nrow(b2$layout$layout) == 3 &&
        identical(as.character(b2$layout$layout$group), c("A", "B", "Other")))
  set_in(session, gid(smp[2]), "A", gid(smp[3]), "B")
  # fixed panel: the existing facet handling splits the panel area among the facets
  session$setInputs(panel_size_mode = "fixed", panel_width_mm = 60, panel_square = TRUE)
  g <- fixed_figure(generate_plot(), "main")
  panel_cells <- g$layout[grepl("^panel", g$layout$name), ]
  pw <- grid::convertWidth(g$widths[panel_cells$l[1]], "mm", valueOnly = TRUE)
  tot_w <- sum(vapply(unique(panel_cells$l), function(i) grid::convertWidth(g$widths[i], "mm", valueOnly = TRUE), 0))
  tot_h <- sum(vapply(unique(panel_cells$t), function(i) grid::convertHeight(g$heights[i], "mm", valueOnly = TRUE), 0))
  check("fixed panel + 2 facets: the 60 x 60 mm panel area is divided between them (2 x 30 x 60)",
        abs(tot_w - 60) < 1e-6 && abs(tot_h - 60) < 1e-6 && abs(pw - 30) < 1e-6 && is.numeric(attr(g, "size_mm")))
  check("legend stays outside with facets (inside legend positions fall back to right)",
        identical(generate_plot()$theme$legend.position, "right"))
  # composer
  session$setInputs(panel_size_mode = "fit", facet_by_group = FALSE, compose_panels = c("main", "main_by_group"))
  info <- compose_info()
  check("composer with main_by_group: main + one panel per group = 3 panels; types main, main_by_group",
        info$n == 3 && identical(info$types, c("main", "main_by_group")) && length(info$skipped) == 0 && inherits(info$plot, "patchwork"))
  session$setInputs(compose_panels = "main_by_group")
  info2 <- compose_info()
  check("main_by_group alone yields two panels (groups A and B)", info2$n == 2 && identical(info2$types, "main_by_group"))
  pl <- main_group_plots()
  check("each group panel is a single-group main figure titled with the group, in group order",
        identical(names(pl), c("main_by_group:A", "main_by_group:B")) && identical(pl[[1]]$labels$title, "A") &&
        identical(pl[[2]]$labels$title, "B") && inherits(pl[[1]]$facet, "FacetNull") &&
        identical(sort(unique(as.character(pl[[1]]$data$variable))), sort(smp[1:2])) && identical(unique(as.character(pl[[2]]$data$variable)), smp[3]))
  check("the composer caption of the reports can name the panel type", "Lysis curve, one panel per group" %in% names(env$COMPOSE_PANEL_CHOICES)[match(info2$types, env$COMPOSE_PANEL_CHOICES)])
  session$setInputs(panel_size_mode = "fixed", panel_width_mm = 65)
  info3 <- compose_info()
  check("fixed-panel composer: two group panels, each with the fixed 65 mm panel (figure wider than 2 x 65)",
        info3$n == 2 && info3$width_mm > 130)
  # no groups: unavailable, not an error
  set_in(session, gid(smp[1]), "", gid(smp[2]), "", gid(smp[3]), "")
  session$setInputs(compose_panels = c("main", "main_by_group"))
  i4 <- compose_info()
  check("no sample has a group: main_by_group is left out (listed as skipped), main remains",
        i4$n == 1 && "Lysis curve, one panel per group" %in% i4$skipped)
})

cat("\n--- server: metric explorer ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1],
                    analysis_type = "metric_explorer")
  p <- generate_analysis_plot("metric_explorer")
  check("explorer renders a real figure (not the placeholder) for the defaults",
        inherits(p, "ggplot") && !is_placeholder_plot(p) && !is.null(tryCatch(ggplot_build(p), error = function(e) NULL)))
  b <- ggplot_build(p)
  check("explorer: axes are the default metrics lysis onset (x) and OD drop (y), with unit labels",
        grepl("Lysis onset", paste(as.character(p$labels$x), collapse = " ")) && grepl("OD drop", paste(as.character(p$labels$y), collapse = " ")))
  lay <- vapply(p$layers, function(l) class(l$geom)[1], "")
  check("explorer: open replicate circles, error bars in both directions, mean points",
        sum(lay == "GeomPoint") == 2 && sum(lay == "GeomLinerange") == 2)
  mean_pts <- b$data[[which(lay == "GeomPoint")[2]]]
  check("explorer: one mean point per sample with a finite value for both metrics (the demo's Uninfected has no lysis onset)",
        nrow(mean_pts) == 2 && all(is.finite(mean_pts$x)))
  session$setInputs(explorer_level = "Per sample mean ± error")
  p_s <- generate_analysis_plot("metric_explorer")
  check("'Per sample mean +/- error': no replicate circles", sum(vapply(p_s$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))) == 1 && !is_placeholder_plot(p_s))
  session$setInputs(explorer_level = "Per replicate", explorer_labels = TRUE)
  p_l <- generate_analysis_plot("metric_explorer")
  txt <- p_l$layers[[which(vapply(p_l$layers, function(l) inherits(l$geom, "GeomText"), logical(1)))[1]]]
  check("point labels: a GeomText layer (not ggrepel) in the axis-text size",
        inherits(txt$geom, "GeomText") && abs(txt$aes_params$size - input$axis_text_font_size / ggplot2::.pt) < 1e-9)
  # log axis
  session$setInputs(explorer_labels = FALSE, explorer_x = "max_od", explorer_y = "auc", explorer_logy = TRUE)
  p_g <- generate_analysis_plot("metric_explorer"); bg <- ggplot_build(p_g)
  check("explorer with a log y axis renders a real figure with a log10 y scale",
        !is_placeholder_plot(p_g) && inherits(bg$layout$panel_scales_y[[1]]$trans, "transform") &&
        grepl("log", bg$layout$panel_scales_y[[1]]$trans$name))
  session$setInputs(explorer_logx = TRUE)
  check("log on both axes renders too", !is_placeholder_plot(generate_analysis_plot("metric_explorer")))
  # not available: a relative metric without a reference
  session$setInputs(explorer_logx = FALSE, explorer_logy = FALSE, explorer_x = "relative_growth", explorer_y = "od_drop", metrics_ref_sample = "")
  check("a metric that is NA for every sample gives the 'not available' placeholder, not an error",
        is_placeholder_plot(generate_analysis_plot("metric_explorer")))
  session$setInputs(metrics_ref_sample = smp[1], explorer_x = "lysis_time", explorer_y = "od_drop")
  # journal theme + fixed panel (square)
  do.call(session$setInputs, env$journal_preset_settings(89, 65, "a"))
  fg <- fixed_figure(generate_analysis_plot("metric_explorer"), "metric_explorer")
  sz <- attr(fg, "size_mm")
  pc <- fg$layout[grepl("^panel", fg$layout$name), ]
  pw <- grid::convertWidth(fg$widths[pc$l[1]], "mm", valueOnly = TRUE); ph <- grid::convertHeight(fg$heights[pc$t[1]], "mm", valueOnly = TRUE)
  check("journal look + fixed panel: the explorer's panel is square (65 x 65 mm)", abs(pw - 65) < 1e-6 && abs(ph - 65) < 1e-6 && length(sz) == 2)
  f <- tempfile(fileext = ".svg")
  quiet(ggsave(f, fg, device = env$export_device("svg"), width = sz[1] / 25.4, height = sz[2] / 25.4, units = "in", bg = "white"))
  svg <- readLines(f)
  check("journal look: the explorer SVG is Arial text, no textLength", any(grepl('font-family: "Arial"', svg)) && !any(grepl("textLength", svg)))
  # selectable through the analysis tab download and in the composer
  session$setInputs(analysis_type = "metric_explorer", panel_size_mode = "fit")
  check("analysis download name uses the type", grepl("^metric_explorer_metric_explorer_[0-9]+x[0-9]+mm\\.", analysis_download_name()))
  session$setInputs(compose_panels = c("main", "metric_explorer"))
  check("composer accepts the explorer panel", compose_info()$n == 2)
})

cat("\n--- server: Download all figures (.zip) ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1],
                    methods_text = "", compose_panels = c("main", "metric_summary", "growth_rate", "normalized"),
                    analysis_metrics = c("lysis_time", "od_drop"))
  zf <- output$downloadAllFigures
  check("all-figures archive name: all_figures_<W>x<H>mm.zip", grepl("^all_figures_[0-9]+x[0-9]+mm\\.zip$", basename(zf)))
  have_zip <- requireNamespace("zip", quietly = TRUE)
  zl <- if (have_zip) basename(zip::zip_list(zf)$filename) else
    basename(sub("^\\s*[0-9]+\\s+[0-9-]+\\s+[0-9:]+\\s+", "", system2("unzip", c("-l", zf), stdout = TRUE)))
  check("archive lists lysis_curve_*.svg and metric_summary_*.svg (and the PNGs)",
        any(grepl("^lysis_curve_.*\\.svg$", zl)) && any(grepl("^metric_summary_.*\\.svg$", zl)) &&
        any(grepl("^lysis_curve_.*\\.png$", zl)) && any(grepl("^metric_summary_.*\\.png$", zl)))
  check("archive: an SVG and a PNG for every analysis figure (the demo has a reference sample and MOIs in the names)",
        all(vapply(unname(env$ANALYSIS_FIGURE_CHOICES),
                   function(ty) any(grepl(paste0("^", ty, "_.*\\.svg$"), zl)) && any(grepl(paste0("^", ty, "_.*\\.png$"), zl)), logical(1))))
  check("no SKIPPED.txt when every figure could be drawn", !"SKIPPED.txt" %in% zl)
  check("archive: the composed figure, the legend and the methods text",
        any(grepl("^figure_composed_[0-9]+x[0-9]+mm\\.svg$", zl)) && any(grepl("^figure_composed_.*\\.png$", zl)) &&
        "figure_legend.txt" %in% zl && "methods.txt" %in% zl)
  ex <- tempfile("allfig_x_"); dir.create(ex); utils::unzip(zf, exdir = ex)
  fold <- list.dirs(ex, recursive = FALSE)
  check("archive top level is the all_figures_<W>x<H>mm folder", length(fold) == 1 && grepl("^all_figures_", basename(fold)))
  svg_main <- list.files(fold, "^lysis_curve_.*\\.svg$", full.names = TRUE)[1]
  main_name <- sub("\\.svg$", "", basename(svg_main))
  check("file names match the single download's sized name", identical(main_name, main_file_stem()))
  check("the SVG is a real figure", length(readLines(svg_main)) > 50)
  pn <- list.files(fold, "^lysis_curve_.*\\.png$", full.names = TRUE)[1]
  check("the PNG is a PNG", identical(readBin(pn, "raw", 4), as.raw(c(0x89, 0x50, 0x4e, 0x47))))
  check("methods.txt: drafted from the settings when the box is empty", length(readLines(file.path(fold, "methods.txt"), warn = FALSE)) >= 1 &&
        nzchar(paste(readLines(file.path(fold, "methods.txt"), warn = FALSE), collapse = "")))
  # a figure that is not available: no reference sample -> "normalized OD" is the placeholder
  session$setInputs(metrics_ref_sample = "", explorer_x = "relative_growth")
  zf3 <- output$downloadAllFigures
  zl3 <- if (have_zip) basename(zip::zip_list(zf3)$filename) else character(0)
  ex3 <- tempfile("allfig_s_"); dir.create(ex3); utils::unzip(zf3, exdir = ex3)
  sk <- list.files(ex3, "SKIPPED.txt", recursive = TRUE, full.names = TRUE)
  check("not-available figures are not written and are listed in SKIPPED.txt with the reason",
        length(sk) == 1 && !any(grepl("^normalized_", zl3)) && !any(grepl("^metric_explorer_", zl3)) &&
        any(grepl("normalized", readLines(sk), fixed = TRUE)) && any(grepl("reference", readLines(sk), fixed = TRUE)) &&
        any(grepl("metric_explorer", readLines(sk), fixed = TRUE)) && any(grepl("^metric_summary_.*svg$", zl3)))
  session$setInputs(metrics_ref_sample = smp[1], explorer_x = "lysis_time")
  # fit mode and a typed methods text
  session$setInputs(panel_size_mode = "fit", methods_text = "Cultures were grown in LB.")
  zf2 <- output$downloadAllFigures
  zl2 <- if (have_zip) basename(zip::zip_list(zf2)$filename) else character(0)
  if (have_zip) check("fit mode: also builds; sized names carry the export size", any(grepl("^lysis_curve_main_[0-9]+x[0-9]+mm\\.svg$", zl2)) && any(grepl("^metric_summary_.*\\.svg$", zl2)))
  ex2 <- tempfile("allfig_f_"); dir.create(ex2); utils::unzip(zf2, exdir = ex2)
  check("a typed methods text is written as is", identical(trimws(paste(readLines(list.files(ex2, "methods.txt", recursive = TRUE, full.names = TRUE), warn = FALSE), collapse = "\n")), "Cultures were grown in LB."))
})

cat(sprintf("\n============ SUMMARY: %d passed / %d failed / %d skipped ============\n", pass, fail, skip))
if (fail > 0) quit(status = 1)
