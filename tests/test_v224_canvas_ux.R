# ============================================================================
# v2.24.0: the canvas as a user handles it: right-click > Copy Image, the legend dragged as a live copy that lands where
# it is dropped, and a preview scale that does not follow the legend.
# Run from the repository root:  Rscript tests/test_v224_canvas_ux.R
# (The browser side is exercised with a real headless browser in the click-through runs; here: the page script's
#  contract and the server rules.)
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v224_canvas_ux.R")
Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_paper_style_here.json"))
pass <- 0L; fail <- 0L
check  <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.24.0 or later", package_version(env$APP_VERSION) >= "2.24.0")
html_of <- function(x) { rt <- htmltools::renderTags(x); gsub("[ \t\r\n]+", " ", paste(rt$head, rt$html)) }
full <- html_of(env$ui)
has <- function(...) all(vapply(c(...), function(s) grepl(s, full, fixed = TRUE), logical(1)))

cat("\n--- right-click > Copy Image ---\n")
check("the copy script lays a PNG of the figure over the inline SVG (img.kc-rc-img, made with the Copy figure pixels)",
      has("img.kc-rc-img", "function rcBuild()", "toPng(svg, dims)", "URL.createObjectURL(b)"))
check("the PNG takes the secondary button only (armed on mousedown button 2 / Ctrl+click on a Mac, disarmed after the menu)",
      has("ev.button === 2", "ev.ctrlKey && mac", "document.addEventListener('contextmenu'", "rcArm(false)"))
check("the overlay is invisible and ignores every other click (CSS: opacity 0, pointer-events none, over the figure)",
      grepl(".kc-canvas img.kc-rc-img { position:absolute; left:0; top:0; width:100%; height:100%; opacity:0; pointer-events:none;", full, fixed = TRUE))
check("it is rebuilt after each redraw of the figure and when the pointer comes over it", has("ev.name === 'od_plot_svg') { rcFor = null;", "setTimeout(rcBuild, 120)"))
check("the canvas tooltip names the three ways to copy", any(grepl("Right-click > Copy Image, the Copy figure button, or Cmd/Ctrl+C while hovering", readLines("Lysis_Curve_App_v28.R", warn = FALSE), fixed = TRUE)))

cat("\n--- the legend drag ---\n")
check("panel and legend box are read off the figure's own rectangles (kcPanelBox / kcLegendBox), the axis-line reading stays as the fallback",
      has("function kcPanelBox(svg, vb)", "function kcLegendBox(svg, texts, P)", "kcPanelBox(svg, vb) || kcPanelRect(svg, vb)"))
check("the legend is dragged as a live copy (a window onto a clone of the figure) while its old place is veiled",
      has("gh.className = 'kc-leg-ghost'", "svg.cloneNode(true)", "veil.className = 'kc-leg-veil'"))
check("inside the panel the copy is kept within the frame (2 % margin) and the drop is exact to 0.001",
      has("0.02 * Math.max(pw - lwI, 0)", "* 1000) / 1000"))
check("outside the panel a label names the side and says the figure grows there", has("hint.className = 'kc-leg-hint'", "(the figure grows on that side)"))
check("the copy stays until the redrawn figure arrives (no jump back), with a time limit when no redraw comes",
      has("if (kcGhostSvg && kcGhostSvg !== svg) kcDropGhost();", "}, 6000);"))
check("a click on the overlay without a drag does not close the rename editor it has just opened",
      has("ev.target.classList.contains('kc-leg-drag') || ev.target.classList.contains('kc-lab-drag'))) return;"))   # v2.25.0: the label handles too
check("a row legend dropped into the panel is anchored at the dragged centre with its column size estimated", has("var horiz = pos === 'top' || pos === 'bottom'"))
check("the drag still never calls preventDefault (it would cancel the sidebar's summary toggles on some browsers)",
      !grepl("preventDefault", sub("(?s).*(function kcLegDrag\\(\\).*?)// after the SVG is replaced.*", "\\1", full, perl = TRUE), fixed = TRUE))
check("CSS for the copy, the veil and the label", has(".kc-leg-ghost {", ".kc-leg-veil {", ".kc-leg-hint {", ".kc-leg-ghost.kc-out {"))

cat("\n--- the preview scale does not follow the legend ---\n")
res <- list()
testServer(app, {
  S <- session$setInputs
  ingest_data(env$make_demo_data("lysis"), "Demo"); do.call(S, env$APP_DEFAULTS)
  S(selected_samples = rv$od_vars, main_panel_width = 1095, sticky_viewport_height = 950, quick_edit_open = TRUE, preview_zoom_auto = TRUE)
  one <- function(pos) { S(legend_position = pos); sz <- export_size_in(); core <- main_core_size_in()
    list(sz = sz, core = core, z = preview_zoom_value(sz[1], sz[2], core), z_old = preview_zoom_value(sz[1], sz[2])) }
  for (p in c("inside-auto", "inside-top-left", "inside-custom", "none", "right", "left", "top", "bottom")) res[[p]] <<- one(p)
  S(legend_position = "right", main_panel_width = 520); sz <- export_size_in()
  res$narrow <<- list(sz = sz, z = preview_zoom_value(sz[1], sz[2], main_core_size_in()))
  S(main_panel_width = 1095, sticky_viewport_height = 520, legend_position = "bottom"); sz <- export_size_in()
  res$low <<- list(sz = sz, z = preview_zoom_value(sz[1], sz[2], main_core_size_in()))
  S(sticky_viewport_height = 950, panel_size_mode = "fit", legend_position = "right"); sz <- export_size_in()
  res$fit <<- list(sz = sz, core = main_core_size_in())
  S(panel_size_mode = "fixed", preview_zoom_auto = FALSE, preview_zoom = 1.5); sz <- export_size_in()
  res$manual <<- preview_zoom_value(sz[1], sz[2], main_core_size_in())
})
ins <- res[["inside-auto"]]
check("legend inside / hidden: the core size is the figure size", all(vapply(c("inside-auto", "inside-top-left", "inside-custom", "none"), function(p) isTRUE(all.equal(res[[p]]$core, res[[p]]$sz)), NA)))
check("legend right / left: the figure is wider, the core is the inside figure (plus the legend spacing, under 8 pt)",
      all(vapply(c("right", "left"), function(p) res[[p]]$sz[1] > ins$sz[1] + 0.5 && max(abs(res[[p]]$core - ins$sz)) < 8 / 72, NA)))
check("legend top / bottom: the figure is taller, the core is the inside figure (plus the legend spacing, under 8 pt)",
      all(vapply(c("top", "bottom"), function(p) res[[p]]$sz[2] > ins$sz[2] + 0.2 && max(abs(res[[p]]$core - ins$sz)) < 8 / 72, NA)))
zs <- vapply(c("inside-auto", "inside-top-left", "inside-custom", "none", "right", "left", "top", "bottom"), function(p) res[[p]]$z, 0)
check(sprintf("the zoom is the same for every legend position (%s)", paste(unique(zs), collapse = ", ")), length(unique(zs)) == 1L && zs[[1]] == ins$z_old)
check("(the old rule gave a smaller zoom with the legend below: the regression this guards)", res$bottom$z_old < res$bottom$z)
check("a figure too wide for the column is still scaled down to fit it", res$narrow$z < ins$z && res$narrow$sz[1] * 96 * res$narrow$z <= 520 - 24 + 1e-6)
check("a figure too tall for the window is still scaled down to fit it", res$low$z < ins$z && res$low$sz[2] * 96 * res$low$z <= 520 - 130 + 1e-6)
check("fit mode: the figure size does not depend on the legend, core = size", isTRUE(all.equal(res$fit$core, res$fit$sz)))
check("auto-fit off: the slider's zoom is used as before", identical(res$manual, 1.5))

cat("\n--- one preview scale on every tab ---\n")
zz <- list()
testServer(app, {
  S <- session$setInputs
  ingest_data(env$make_demo_data("lysis"), "Demo"); do.call(S, env$APP_DEFAULTS)
  S(selected_samples = rv$od_vars, main_panel_width = 1095, sticky_viewport_height = 950, quick_edit_open = TRUE, preview_zoom_auto = TRUE)
  ms <- export_size_in(); zz$main <<- preview_zoom_for("od_plot_img", ms)
  for (a in c("metric_summary", "growth_rate", "phases", "phenotype_heatmap")) { S(analysis_type = a); zz[[a]] <<- preview_zoom_for("analysis_img", analysis_size_in()) }
  zz$old_fill <<- preview_zoom_value(analysis_size_in()[1])
  zz$wide <<- preview_zoom_for("compose_img", c(12, 4)); zz$wide_fit <<- 12 * 96 * zz$wide <= 1095 - 24 + 1e-6
})
check(sprintf("the analysis figures are shown at the main figure's zoom (%s), not stretched to the column", zz$main),
      all(vapply(c("growth_rate", "phases", "phenotype_heatmap"), function(a) identical(zz[[a]], zz$main), NA)) &&
        zz$metric_summary <= zz$main)   # v2.27.3: the metric summary is two full panels wide; it is never shown larger than the main figure
check("(the old rule filled the column: a larger zoom than the main figure's)", zz$old_fill > zz$main)
check("a figure too wide for the column at that zoom is still shrunk to fit", zz$wide < zz$main && isTRUE(zz$wide_fit))

cat("\n--- one legend, whatever its title and labels; long legends wrap ---\n")
nkeys <- function(p) { g <- ggplot2::ggplotGrob(p); gb <- g$grobs[[which(grepl("guide-box", g$layout$name) & !vapply(g$grobs, inherits, NA, "zeroGrob"))[1]]]; sum(grepl("^guides", gb$layout$name)) }
lg <- list()
testServer(app, {
  S <- session$setInputs
  ingest_data(env$make_demo_data("lysis"), "Demo"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars)
  lg$plain <<- nkeys(generate_plot())
  S(legend_title = "Strain / treatment"); lg$title <<- nkeys(generate_plot())
  S(legend_title = "", legend_wrap_width = 8); lg$wrap <<- nkeys(generate_plot())
  S(legend_wrap_width = NA, legend_title = "T", legend_position = "bottom", error_display_mode = "shadow"); lg$shadow <<- nkeys(generate_plot())
  S(legend_title = "", error_display_mode = "bars", legend_position = "bottom")
  g0 <- main_fixed_info(); lg$short_over <<- is.null(env$legend_overflow(g0)); lg$short_h <<- attr(g0, "size_mm")[2]
  rv$display_names <- setNames(c("Uninfected control culture, no phage added", "Phage T4 wild type at MOI 10 (high)", "Phage T4 wild type at MOI 0.1 (low)"), rv$od_vars); session$flushReact()
  p <- generate_plot(); d <- panel_dims_mm("main")
  g1 <- env$fixed_panel_grob(p, d[1], d[2], wrap_legend = FALSE); g2 <- env$fixed_panel_grob(p, d[1], d[2])
  lg$raw_over <<- env$legend_overflow(g1)$by; lg$fixed_over <<- is.null(env$legend_overflow(g2)); lg$w <<- c(attr(g1, "size_mm")[1], attr(g2, "size_mm")[1]); lg$h <<- c(attr(g1, "size_mm")[2], attr(g2, "size_mm")[2])
  S(legend_position = "right"); lg$right_ok <<- is.null(env$legend_overflow(main_fixed_info()))
})
check("the default figure has one legend", identical(lg$plain, 1L))
check("a legend title keeps it one (the marker scales carry the same title; it used to draw a second, black list)", identical(lg$title, 1L))
check("wrapped labels keep it one", identical(lg$wrap, 1L))
check("a title with a legend below and a shadow band keeps it one", identical(lg$shadow, 1L))
check("a short legend below the panel fits: nothing to wrap", isTRUE(lg$short_over))
check(sprintf("long names below the panel: one row would overflow the figure (by %.0f mm) ...", lg$raw_over %||% 0), is.numeric(lg$raw_over) && lg$raw_over > 5)
check("... so the legend goes onto more rows: nothing is cut off, the figure keeps its width and gets taller", isTRUE(lg$fixed_over) && abs(lg$w[1] - lg$w[2]) < 0.01 && lg$h[2] > lg$h[1])
check("the same names beside the panel need no wrapping (the figure widens)", isTRUE(lg$right_ok))

cat("\n--- exports: one physical size in every format, plain names, a notice when there is nothing to write ---\n")
ex <- list(); notes <- character(0)
old_sn <- env$showNotification
env$showNotification <- function(ui, ..., type = "default") { notes <<- c(notes, paste0("[", type, "] ", as.character(ui))); invisible(NULL) }
testServer(app, {
  S <- session$setInputs
  ex$hint <<- as.character(output$plot_container$html)
  ingest_data(env$make_demo_data("lysis"), "Demo"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars)
  ex$hint_after <<- as.character(output$plot_container$html)
  ex$name <<- analysis_download_name()
  S(analysis_panel_label = "b"); ex$name_lab <<- analysis_download_name()
  S(export_format = "jpeg", export_dpi = 600); f <- output$downloadPlot; ex$jpeg <<- readBin(f, "raw", n = 18L)
  ex$jpeg_px <<- tryCatch({ o <- system2("sips", c("-g", "pixelWidth", "-g", "dpiWidth", shQuote(f)), stdout = TRUE); as.numeric(sub(".*: ", "", o[grepl("pixelWidth|dpiWidth", o)])) }, error = function(e) NULL, warning = function(w) NULL)
  ex$size_in <<- export_size_in()
  ex$boot <<- tryCatch({ output$downloadBootCSV; "file" }, error = function(e) class(e)[1])
})
env$showNotification <- old_sn
check("before any data the figure area says how to start", grepl("kc-start-hint", ex$hint, fixed = TRUE) && grepl("Load Demo Data", ex$hint, fixed = TRUE))
check("with data loaded the hint is gone", !grepl("kc-start-hint", ex$hint_after, fixed = TRUE))
check("an analysis download is named once (metric_summary_<size>.svg), with the panel label only when one is typed",
      grepl("^metric_summary_[0-9]+x[0-9]+mm\\.svg$", ex$name) && grepl("^metric_summary_b_[0-9]+x[0-9]+mm\\.svg$", ex$name_lab))
check("the JPEG starts with a JFIF header that records 600 dots per inch (unit 1, density 0x0258)",
      identical(ex$jpeg[1:4], as.raw(c(0xff, 0xd8, 0xff, 0xe0))) && identical(ex$jpeg[14:18], as.raw(c(1, 2, 0x58, 2, 0x58))))
if (!is.null(ex$jpeg_px) && length(ex$jpeg_px) == 2)
  check(sprintf("so it opens at the figure's size (%.0f px at %.0f dpi = %.1f mm, the SVG's %.1f mm)", ex$jpeg_px[1], ex$jpeg_px[2], ex$jpeg_px[1] / ex$jpeg_px[2] * 25.4, ex$size_in[1] * 25.4),
        ex$jpeg_px[2] == 600 && abs(ex$jpeg_px[1] / 600 - ex$size_in[1]) < 0.01)
check("jpeg_set_dpi leaves a file that is not a JFIF JPEG alone", { f <- tempfile(fileext = ".png"); writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47, rep(0, 20))), f); b <- readBin(f, "raw", 24); isFALSE(env$jpeg_set_dpi(f, 600)) && identical(readBin(f, "raw", 24), b) })
check("a download with nothing to write (the bootstrap table before a run) stops quietly AND tells the user",
      identical(ex$boot, "shiny.silent.error") && any(grepl("Nothing to download yet", notes, fixed = TRUE)))
check("the page asks for no favicon", grepl("rel=\"icon\" href=\"data:,\"", full, fixed = TRUE))

cat("\n--- v2.24.1: dashed keys, Clear & Reset, the settings round trip ---\n")
kw <- function(p) { w <- ggplot2::calc_element("legend.key.width", p$theme); if (is.null(w)) NA_real_ else grid::convertWidth(w, "mm", valueOnly = TRUE) }
r1 <- list(); notes <- character(0)
env$showNotification <- function(ui, ..., type = "default") { notes <<- c(notes, paste0("[", type, "] ", as.character(ui))); invisible(NULL) }
sfile <- tempfile(fileext = ".json")
testServer(app, {
  S <- session$setInputs
  ingest_data(env$make_demo_data("lysis"), "Demo"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars, export_format = "svg")
  ids <- vapply(rv$od_vars, env$safe_id, "")
  r1$kw_solid <<- kw(generate_plot()); r1$svg0 <<- readLines(output$downloadPlot)
  a <- list(); a[[paste0("line_type_", ids[2])]] <- "dashed"; do.call(S, a)
  r1$kw_dashed <<- kw(generate_plot())
  S(hide_lines = TRUE); r1$kw_hidden <<- kw(generate_plot()); S(hide_lines = FALSE)
  # a figure styled by hand (no settings file loaded): Clear & Reset asks, then resets
  S(plot_title = "By hand", legend_position = "right")
  r1$styled <<- readLines(output$downloadPlot)
  r1$doc <<- settings_json_list(); writeLines(jsonlite::toJSON(r1$doc, auto_unbox = TRUE, null = "null", na = "null"), sfile)
  S(clearSettings = 1); r1$after_click <<- identical(readLines(output$downloadPlot), r1$styled)
  S(clearSettings_confirm = 1); r1$status <<- rv$settings_status$msg; r1$imported_null <<- is.null(rv$imported_settings)
  # (the mock session does not move inputs: what the reset sent is replayed, as the paper-style tests do)
  do.call(S, env$APP_DEFAULTS); b <- list(); b[[paste0("line_type_", ids[2])]] <- "solid"; do.call(S, b)
  r1$reset <<- identical(readLines(output$downloadPlot), r1$svg0)
  notes <<- character(0)
  S(loadSettings = data.frame(name = "s.json", size = file.size(sfile), type = "application/json", datapath = sfile, stringsAsFactors = FALSE))
  r1$load_status <<- paste(rv$settings_status$msg, rv$settings_status$details %||% "")
})
check("a figure of solid lines keeps the standard legend key (the default figure is unchanged)", is.na(r1$kw_solid) || r1$kw_solid < 6)
check("a dashed line gives a 9 mm key, long enough to show the pattern beside the marker", isTRUE(all.equal(r1$kw_dashed, 9, tolerance = 1e-6)))
check("with the lines hidden the key stays standard", is.na(r1$kw_hidden) || r1$kw_hidden < 6)
check("Clear & Reset asks first: the click alone changes nothing", isTRUE(r1$after_click))
check("confirming resets a figure styled by hand (no settings file needed) and says so",
      grepl("Styling reset", r1$status, fixed = TRUE) && isTRUE(r1$imported_null) && isTRUE(r1$reset))
gs <- names(r1$doc$global_settings)
check("buttons and file pickers are not written into a settings file",
      !any(c("clearSettings", "clearSettings_confirm", "load_demo", "colour_map_file", "clear_colour_map", "reload_paper_style") %in% gs))
check("a settings file the app has just written loads back with no 'wrong type' warning (blank number boxes are blank, not errors)",
      !grepl("wrong type", r1$load_status, fixed = TRUE) && !any(grepl("wrong type", notes, fixed = TRUE)))
check("the page has the confirm button's modal wording and the night-mode rule for the Quick edit heading",
      any(grepl("Reset the figure styling?", readLines("Lysis_Curve_App_v28.R", warn = FALSE), fixed = TRUE)) && grepl("body.kc-night .kc-qe-title", full, fixed = TRUE))

cat("\n--- v2.24.2: the y axis follows the transform; Enter applies a table edit ---\n")
tr <- list()
testServer(app, {
  S <- session$setInputs
  ingest_data(env$make_demo_data("lysis"), "Demo"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars)
  one <- function(m, ctl = "") { S(transform_mode = m, transform_control = ctl); p <- generate_plot(); b <- ggplot2::ggplot_build(p)
    list(scale = sel1("y_scale_type"), exp = ylog_exp("y_log_max_exponent"), lab = paste(deparse(p$labels$y), collapse = ""), yr = b$layout$panel_params[[1]]$y.range, eff = transform_effective(),
         top = max(prepare_plot_data()$mean_value, na.rm = TRUE)) }
  tr$none <<- one("none"); tr$t0 <<- one("t0"); tr$ctl0 <<- one("control"); tr$ctl <<- one("control", rv$od_vars[1]); tr$pct <<- one("pct_max"); tr$ln <<- one("ln")
  tr$legend <<- figure_legend_string()
  S(transform_mode = "pct_max", y_axis_label = "My own label"); tr$own <<- paste(deparse(generate_plot()$labels$y), collapse = "")
})
check("no transform: the scale menu, the OD decades and the label are used as before", identical(tr$none$scale, "log") && isTRUE(all.equal(tr$none$exp, env$APP_DEFAULTS$y_log_max_exponent)) && identical(tr$none$lab, "A[550]"))
check("'% of own maximum' is drawn on a linear axis that reaches 100 (it was squashed against the top of the 0.01-10 log axis)",
      identical(tr$pct$scale, "linear") && tr$pct$top > 99 && tr$pct$yr[2] >= 100 && tr$pct$yr[1] <= 5)
check("its axis says so: A550 (% of maximum)", grepl("% of maximum", tr$pct$lab, fixed = TRUE))
check("ln(OD) is drawn on a linear axis (its values are negative) and labelled ln A550", identical(tr$ln$scale, "linear") && tr$ln$yr[1] < 0 && grepl("ln", tr$ln$lab, fixed = TRUE))
check("relative to t = 0: the log axis stays, with automatic ticks (the fixed OD decades are dropped) and a label that says so",
      identical(tr$t0$scale, "log") && is.na(tr$t0$exp) && grepl("relative to t = 0", tr$t0$lab, fixed = TRUE))
check("relative to a control: the label names the control", grepl("relative to Uninfected", tr$ctl$lab, fixed = TRUE) && is.na(tr$ctl$exp))
check("'control' with no control chosen changes nothing (effective transform none: the default axis and label)", identical(tr$ctl0$eff, "none") && identical(tr$ctl0$lab, "A[550]") && isTRUE(all.equal(tr$ctl0$exp, env$APP_DEFAULTS$y_log_max_exponent)))
check("the legend text names the transformed measure", grepl("measured as ln A550", tr$legend, fixed = TRUE))
check("a y label the user typed is left alone", identical(tr$own, "\"My own label\""))
check("Enter applies an edit in the data table (the editor only saved on blur)", has("ev.key === 'Enter'", "t.matches('#data_view_table td input')", "fixVal(t); t.blur();"))   # v2.28.3: the script also reads a decimal comma and cancels on Escape

cat(sprintf("\nSUMMARY: %d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1)
