# ============================================================================
# v2.27.3: the multi-panel agreement everywhere. Every figure with several panels has each panel the size of a single
# figure's panel (all equal; the figure grows) and its legend inside: the split lysis curve with one legend for all
# panels, Compare side by side, the faceted analysis figures (metric summary, replicate panels) and the Compose tab.
# Run from the repository root:  Rscript tests/test_v2273_panel_rule.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v2273_panel_rule.R")
Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_paper_style_here.json"))
pass <- 0L; fail <- 0L
check <- function(desc, cond) { if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") } else { fail <<- fail + 1L; cat("FAIL:", desc, "\n") } }
env <- new.env(); app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("APP_VERSION is 2.27.3 or later", package_version(env$APP_VERSION) >= "2.27.3")

cat("\n--- the corner of a block of panels ---\n")
fc <- env$facet_inside_corner
t <- seq(0, 100, 10)
up <- 0.05 * exp(0.04 * t); down <- 0.5 * exp(-0.03 * t)
# 2 panels in a row: A rises (its bottom-right and top-left are busy), B falls (top-left and bottom-right busy)
x <- c(t, t); y <- c(up, down); s <- rep("s", 22); pnl <- rep(c("A", "B"), each = 11)
# A rising: its top-left is free; with A's top-left also busy (a flat high line), B's top-right (B falls) is the free one
check("two panels: A rising leaves its own top-left free", identical(fc(x, y, s, pnl, c("A", "B"), ncol = 2, log_y = TRUE, box = c(0.4, 0.2)), "top-left"))
x2 <- c(t, t, t); y2 <- c(up, rep(max(up), 11), down); s2 <- rep(c("r", "h", "f"), each = 11); p2 <- c(rep("A", 22), rep("B", 11))
check("two panels: each corner is judged by the panel it belongs to (A full on top -> B's empty top-right)",
      identical(fc(x2, y2, s2, p2, c("A", "B"), ncol = 2, log_y = TRUE, box = c(0.4, 0.2)), "top-right"))
# 3 panels on a 2 x 2 grid: the bottom-right cell is empty, so the key goes there
full <- data.frame(x = c(0, 100, 0, 100), y = c(0.01, 10, 10, 0.01), s = c("a", "a", "b", "b"))   # two crossing lines: every corner busy
f3 <- do.call(rbind, lapply(c("A", "B", "C"), function(p) cbind(full, p = p)))
check("three panels on a 2 x 2 grid: every panel is full, so the empty cell (bottom-right) takes the key",
      identical(fc(f3$x, f3$y, paste(f3$p, f3$s), f3$p, c("A", "B", "C"), ncol = 2, log_y = TRUE, box = c(0.4, 0.2)), "bottom-right"))
check("corner counts with a shared x range: identical without one", identical(env$legend_corner_counts(t, up, TRUE, c(0.4, 0.2)), env$legend_corner_counts(t, up, TRUE, c(0.4, 0.2), xrange = NULL)))

cat("\n--- every multi-panel figure ---\n")
tt <- seq(0, 360, 10); set.seed(2)
g <- function(t) 0.25 + 2.8 / (1 + exp(-0.05 * (t - 70)))
lys <- function(t) ifelse(t < 130, g(t) * 0.4, pmax(1.1 * exp(-0.006 * (t - 130)), 0.2))
cols <- list(Time = tt)
for (f in list(g, lys, function(t) g(t) * 0.8)) for (r in 1:3) cols[[length(cols) + 1]] <- round(f(tt) * exp(rnorm(length(tt), 0, 0.02)), 4)
d <- as.data.frame(cols); names(d) <- c("Time", rep(c("Host", "Host + phage A", "Host + phage B"), each = 3))
fB <- file.path(tempdir(), "second.csv"); write.csv(d, fB, row.names = FALSE); l <- readLines(fB); l[1] <- paste(c("Time", rep(c("Host", "Host + phage A", "Host + phage B"), each = 3)), collapse = ","); writeLines(l, fB)
r <- new.env()
pos <- function(p) as.character(ggplot2::calc_element("legend.position", ggplot2::theme_get() + p$theme))[1]
testServer(app, {
  S <- session$setInputs
  ingest_data(d, "pair.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars, export_format = "svg")
  r$lp <- input$legend_position; r$single <- pos(generate_plot())
  S(facet_by_group = TRUE, facet_source = "each", facet_every = "Host", facet_legends = "shared")
  r$split_pos <- pos(generate_plot()); r$split_panel <- attr(main_plot_obj(), "panel_mm")
  S(legend_position = "inside-top-right"); r$split_tr <- pos(generate_plot()); S(legend_position = r$lp)
  S(facet_by_group = FALSE)
  compare_load(fB, "second.csv"); S(compare_mode = "facet")
  pc <- generate_compare_plot(); r$cmp_pos <- pos(pc); r$cmp_panel <- attr(compare_plot_obj(), "panel_mm")
  S(analysis_metrics = c("lysis_time", "od_drop"), analysis_type = "metric_summary")
  am <- generate_analysis_plot("metric_summary"); r$ms_each <- isTRUE(attr(am, "kc_panel_each")); r$ms_panel <- attr(analysis_plot_obj(), "panel_mm")
  ar <- generate_analysis_plot("replicate_facets"); r$rf_each <- isTRUE(attr(ar, "kc_panel_each")); r$rf_panel <- attr(fixed_figure(ar, "replicate_facets"), "panel_mm")
  # Compose: the main figure and an analysis figure with a key, each key inside its own panel
  r$main_alone <- pos(generate_plot())
  r$in_compose <- with_panels_inside(c(main = pos(generate_plot()), growth = pos(generate_analysis_plot("growth_rate"))))
  r$after <- pos(generate_plot())
  S(compose_panels = c("main", "growth_rate")); ci <- compose_info(); r$comp_n <- ci$n; r$comp_ok <- !is.null(ci$plot)
})
check("one legend for all panels (split): inside the block of panels, each panel 65 x 65 mm",
      identical(r$split_pos, "inside") && isTRUE(all.equal(unname(r$split_panel), c(65, 65))))
check("one legend for all panels: a chosen inside corner is kept (not moved outside)", identical(r$split_tr, "inside"))
check("Compare side by side: the legend inside, each file's panel 65 x 65 mm", identical(r$cmp_pos, "inside") && isTRUE(all.equal(unname(r$cmp_panel), c(65, 65))))
check("metric summary split by metric: each metric's panel at the full panel size", r$ms_each && isTRUE(all.equal(unname(r$ms_panel), c(65, 65))))
check("replicate panels: each sample's panel at the full panel size", r$rf_each && isTRUE(all.equal(unname(r$rf_panel), c(65, 65))))
check("Compose: every figure's key inside its own panel while composing", all(r$in_compose == "inside"))
check("Compose: the flag ends with the composition (the single figure keeps its own rule)", identical(r$after, r$main_alone))
check("Compose builds (main + growth rate)", r$comp_ok && r$comp_n == 2L)
cat("\n", pass, "passed,", fail, "failed\n")
if (fail > 0) quit(status = 1)
