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
      has("ev.target.classList.contains('kc-leg-drag')) return;"))
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

cat(sprintf("\nSUMMARY: %d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1)
