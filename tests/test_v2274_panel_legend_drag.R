# ============================================================================
# v2.27.4: every panel's legend is dragged on its own (a figure of panels with "A legend in each panel").
# Run from the repository root:  Rscript tests/test_v2274_panel_legend_drag.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v2274_panel_legend_drag.R")
Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_paper_style_here.json"))
pass <- 0L; fail <- 0L
check <- function(desc, cond) { if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") } else { fail <<- fail + 1L; cat("FAIL:", desc, "\n") } }
env <- new.env(); app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("APP_VERSION is 2.27.4 or later", package_version(env$APP_VERSION) >= "2.27.4")
check("a panel's legend can be 'where dragged' or on the left; its place is two panel settings (appended)",
      all(c("inside-custom", "left") %in% env$PANEL_CFG_LEGEND_CHOICES) && identical(tail(env$PANEL_CFG_FIELDS, 2), c("legend_x", "legend_y")))
rt <- htmltools::renderTags(env$ui); h <- paste(rt$head, rt$html)
check("the page has the per-panel drag (one handle per panel legend) and sends kc_panel_legend_drop",
      grepl("function kcPanelLegDrag", h, fixed = TRUE) && grepl("kc_panel_legend_drop", h, fixed = TRUE) && grepl("if (kcPanelLegDrag()) return;", h, fixed = TRUE))
pg <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3, s = c("a", "b", "c")), ggplot2::aes(x, y, colour = s)) + ggplot2::geom_point()
q <- env$apply_panel_cfg(pg, list(legend = "inside-custom", legend_x = 0.3, legend_y = 0.2))
th <- ggplot2::theme_get() + q$theme
check("apply_panel_cfg: 'where dragged' puts the key inside at legend_x / legend_y",
      identical(as.character(ggplot2::calc_element("legend.position", th)), "inside") && isTRUE(all.equal(as.numeric(ggplot2::calc_element("legend.position.inside", th)), c(0.3, 0.2))))
check("apply_panel_cfg: 'left' is an outside legend on the left", identical(env$apply_panel_cfg(pg, list(legend = "left"))$theme$legend.position, "left"))

t <- seq(0, 180, 15); set.seed(4); cols <- list(Time = t)
for (i in 1:3) for (r in 1:3) cols[[length(cols) + 1]] <- round(0.05 * exp(0.02 * t) * (1 - 0.1 * i) * exp(rnorm(length(t), 0, 0.02)), 4)
d <- as.data.frame(cols); names(d) <- c("Time", rep(c("Ctrl", "P1", "P2"), each = 3))
r <- new.env()
inside_at <- function(p) { th <- ggplot2::theme_get() + p$theme; list(pos = as.character(ggplot2::calc_element("legend.position", th))[1], at = as.numeric(ggplot2::calc_element("legend.position.inside", th))) }
testServer(app, {
  S <- session$setInputs
  ingest_data(d, "d.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars, export_format = "svg")
  S(facet_by_group = TRUE, facet_source = "each", facet_every = "Ctrl")
  lay <- panel_legend_layout(); r$names <- vapply(lay$panels, `[[`, "", "name")
  before <- lapply(main_group_plots(min_panels = 2L), inside_at)
  S(kc_panel_legend_drop = list(panel = "P2", inside = TRUE, fx = 0.4, fy = 0.65, n = 1)); session$flushReact()
  after <- lapply(main_group_plots(min_panels = 2L), inside_at)
  r$p2 <- after[[2]]; r$p1_same <- identical(after[[1]], before[[1]]); r$cfg <- rv$panel_cfg[["P2"]]
  S(kc_panel_legend_drop = list(panel = "P1", inside = FALSE, side = "left", n = 2)); session$flushReact()
  r$p1_left <- main_group_plots(min_panels = 2L)[[1]]$theme$legend.position; r$p2_kept <- inside_at(main_group_plots(min_panels = 2L)[[2]])
  S(kc_panel_legend_drop = list(panel = "Nope", inside = TRUE, fx = 0.1, fy = 0.1, n = 3)); session$flushReact()
  r$nope <- is.null(rv$panel_cfg[["Nope"]])
  S(kc_panel_legend_drop = "junk", n = 4); r$junk_ok <- TRUE
  doc <- settings_json_list(); r$saved <- doc$panel_settings[["P2"]]
  r$html <- as.character(output$od_plot_svg$html)
})
check("the page is told the panels in grid order", identical(r$names, c("P1", "P2")))
check("a drop inside panel P2 pins P2's legend there (inside, at 0.4 / 0.65)", identical(r$p2$pos, "inside") && isTRUE(all.equal(r$p2$at, c(0.4, 0.65))) &&
        identical(r$cfg$legend, "inside-custom") && identical(r$cfg$legend_x, 0.4) && identical(r$cfg$legend_y, 0.65))
check("panel P1 is not touched by P2's drag", isTRUE(r$p1_same))
check("a drop outside P1 on the left: P1's legend on its left, P2 keeps its own place", identical(r$p1_left, "left") && isTRUE(all.equal(r$p2_kept$at, c(0.4, 0.65))))
check("a drop for a panel that does not exist, or a malformed one, changes nothing", isTRUE(r$nope) && isTRUE(r$junk_ok))
check("Save Settings keeps the dragged place of each panel's legend", identical(r$saved$legend, "inside-custom") && identical(r$saved$legend_x, 0.4) && identical(r$saved$legend_y, 0.65))
check("the preview carries the panel list for the page", grepl('id="kc_panel_legend_data"', r$html, fixed = TRUE) && grepl('"name":"P2"', r$html, fixed = TRUE))
cat("\n--- the last error bar at the end of a typed x range ---\n")
t2 <- seq(0, 120, 10); set.seed(7); c2 <- list(Time = t2)
for (r in 1:3) c2[[length(c2) + 1]] <- round(0.25 * exp(0.02 * t2) * exp(rnorm(length(t2), 0, 0.2)), 4)
d2 <- as.data.frame(c2); names(d2) <- c("Time", rep("EV", 3))
e2 <- new.env()
testServer(app, {
  S <- session$setInputs
  ingest_data(d2, "d2.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars, custom_x_limits = TRUE, x_min = 0, x_max = 120, x_expand_right = 0)
  p <- generate_plot(); b <- ggplot2::ggplot_build(p); i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomErrorbar"), NA))[1]
  e2$last <- b$data[[i]][b$data[[i]]$x == 120, c("xmin", "xmax")]
  S(custom_x_limits = TRUE, x_min = 0, x_max = 100); p <- generate_plot(); b <- ggplot2::ggplot_build(p)
  e2$beyond <- sum(b$data[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), NA))[1]]]$x > 100, na.rm = TRUE)
})
check("x range 0-120 with a reading at 120: the last bar keeps its cap (cut at the frame), not dropped", nrow(e2$last) == 1 && is.finite(e2$last$xmax) && e2$last$xmax == 120 && e2$last$xmin < 120)
check("readings beyond the typed x range are still left out (0-100: nothing drawn past 100)", identical(e2$beyond, 0L))

cat("\n", pass, "passed,", fail, "failed\n")
if (fail > 0) quit(status = 1)
