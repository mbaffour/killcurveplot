# ============================================================================
# v2.27.2: a figure split into panels has every panel the size of a single figure's panel (the figure grows), a legend in
# each panel, and each panel's legend inside it, in its emptiest corner measured against the axis.
# Run from the repository root:  Rscript tests/test_v2272_panel_layout.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v2272_panel_layout.R")
Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_paper_style_here.json"))
pass <- 0L; fail <- 0L
check <- function(desc, cond) { if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") } else { fail <<- fail + 1L; cat("FAIL:", desc, "\n") } }
env <- new.env(); app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("APP_VERSION is 2.27.2 or later", package_version(env$APP_VERSION) >= "2.27.2")
check("defaults: each panel at the full panel size, a legend in each panel, listed first",
      identical(env$APP_DEFAULTS$facet_size_mode, "each") && identical(env$APP_DEFAULTS$facet_legends, "each") &&
        identical(unname(env$FACET_SIZE_CHOICES[1]), "each") && identical(unname(env$FACET_LEGEND_CHOICES[1]), "each"))
cc <- env$legend_corner_counts
y <- c(0.25, 0.4, 1, 2, 3, 3, 3); x <- seq(0, 360, length.out = 7)
box <- c(0.4, 0.2)
check("corner counts against the axis (0.01 to 10): the bottom corners are empty although the data range fills them",
      { a <- cc(x, y, TRUE, box); b <- cc(x, y, TRUE, box, yrange = c(0.01, 10)); a[["bottom-left"]] > 0 && b[["bottom-left"]] == 0 && b[["bottom-right"]] == 0 })
check("corner counts: without an axis range, exactly as before", identical(cc(x, y, TRUE, box), cc(x, y, TRUE, box, yrange = NULL)))

# a curve that fills every corner of its own data range (as a real growth / lysis pair does)
t <- seq(0, 360, 10); set.seed(2)
g <- function(t) 0.25 + 2.8 / (1 + exp(-0.05 * (t - 70)))
lys <- function(t) ifelse(t < 130, g(t) * 0.4, pmax(1.1 * exp(-0.006 * (t - 130)), 0.2))
cols <- list(Time = t)
for (s in list(g, lys, function(t) g(t) * 0.8)) for (r in 1:3) cols[[length(cols) + 1]] <- round(s(t) * exp(rnorm(length(t), 0, 0.02)), 4)
d <- as.data.frame(cols); names(d) <- c("Time", rep(c("Host", "Host + phage A", "Host + phage B"), each = 3))
r <- new.env()
testServer(app, {
  S <- session$setInputs
  ingest_data(d, "pair.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars, export_format = "svg")
  r$single_size <- attr(main_plot_obj(), "size_mm"); r$single_panel <- attr(main_plot_obj(), "panel_mm") %||% c(env$APP_DEFAULTS$panel_width_mm, env$APP_DEFAULTS$panel_height_mm)
  S(facet_by_group = TRUE, facet_source = "each", facet_every = "Host", facet_letters = "upper")
  g0 <- main_plot_obj(); r$cls <- class(g0)[1]; r$size <- attr(g0, "size_mm"); r$export <- export_size_in() * 25.4
  pl <- main_group_plots(min_panels = 2L); r$n <- length(pl)
  r$pos <- vapply(pl, function(p) as.character(ggplot2::calc_element("legend.position", ggplot2::theme_get() + p$theme))[1], "")
  # each panel's plotting area in the composed figure
  gg <- lapply(pl, function(p) env$fixed_panel_grob(p, env$APP_DEFAULTS$panel_width_mm, env$APP_DEFAULTS$panel_height_mm))
  r$panel_mm <- lapply(gg, function(x) attr(x, "panel_mm") %||% NA)
  # the single figure keeps its own rule (falls back to an outside legend when every corner holds data)
  S(facet_by_group = FALSE); r$single_pos <- as.character(ggplot2::calc_element("legend.position", ggplot2::theme_get() + generate_plot()$theme))[1]
  # one legend for all panels and the other sizes still work
  S(facet_by_group = TRUE, facet_legends = "shared"); r$shared_ok <- inherits(main_plot_obj(), c("gtable", "ggplot"))
  S(facet_size_mode = "fit_double"); r$fit_w <- attr(main_plot_obj(), "size_mm")[1]
})
check("split: two panels, composed as one figure", r$n == 2L && identical(r$cls, "gtable"))
check("every panel's legend is inside the panel (never pushed outside)", length(r$pos) == 2L && all(r$pos == "inside"))
check("the figure grows: about two single figures wide, and the export size follows", r$size[1] > 1.8 * 65 && isTRUE(all.equal(r$export, r$size, tolerance = 0.02)))
check("each panel is the single figure's panel size (65 x 65 mm)",
      all(vapply(r$panel_mm, function(p) length(p) == 2L && isTRUE(all.equal(unname(p), c(65, 65), tolerance = 1e-6)), NA)))
check("the single figure is unchanged: its legend rule is its own", length(r$single_pos) == 1L && nzchar(r$single_pos))
check("one legend for all panels, and Fit a double column (183 mm), still work", isTRUE(r$shared_ok) && abs(r$fit_w - 183) < 1)
cat("\n", pass, "passed,", fail, "failed\n")
if (fail > 0) quit(status = 1)
