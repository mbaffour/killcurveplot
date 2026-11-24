# ============================================================================
# v2.28.1: error bars that went missing.
#   1. A bar shorter than its marker, with a cap narrower than the marker, lay entirely under the marker. Such a bar now
#      gets caps that reach past the marker (decided at drawing time, in points); every other element is unchanged.
#   2. With "drop readings outside the axis" (y_oob = censor) a bar whose mean is on the axis but one end runs past a
#      limit was deleted whole; it now runs to the frame.
# Run from the repository root:  Rscript tests/test_v2281_error_bars.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v2281_error_bars.R")
Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_paper_style_here.json"))
pass <- 0L; fail <- 0L
check <- function(desc, cond) { if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") } else { fail <<- fail + 1L; cat("FAIL:", desc, "\n") } }
env <- new.env(); app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("APP_VERSION is 2.28.1 or later", package_version(env$APP_VERSION) >= "2.28.1")
check("the switch is appended to APP_DEFAULTS (after v2.28.0's panel_style_match), on by default",
      match("error_cap_past_marker", names(env$APP_DEFAULTS)) > match("panel_style_match", names(env$APP_DEFAULTS)) && isTRUE(env$APP_DEFAULTS$error_cap_past_marker))
pg0 <- ggplot2::ggplot(data.frame(time = 1:3, mean_value = 1:3, err_lo = 0.9 * (1:3), err_hi = 1.1 * (1:3), variable = "a", kc_mk = 2, kc_mv = 2)) +
  ggplot2::geom_errorbar(ggplot2::aes(time, ymin = err_lo, ymax = err_hi)) +
  ggplot2::layer(mapping = ggplot2::aes(x = time, ymin = err_lo, ymax = err_hi, kc_mk = kc_mk, kc_mv = kc_mv), stat = "identity", geom = env$GeomKcCapPastMarker, position = "identity") +
  ggplot2::geom_point(ggplot2::aes(time, mean_value, shape = variable))
geoms <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], ""))
check("a panel's 'Hide points' also drops the caps that reach past the marker; 'Hide error bars' drops both bar layers",
      !"GeomKcCapPastMarker" %in% geoms(env$apply_panel_cfg(pg0, list(hide_points = TRUE))) && "GeomErrorbar" %in% geoms(env$apply_panel_cfg(pg0, list(hide_points = TRUE))) &&
        identical(geoms(env$apply_panel_cfg(pg0, list(hide_errors = TRUE))), "GeomPoint"))
check("marker sizes: a solid circle is R's 0.375 x fontsize; open symbols and unfilled 21-25 hide nothing; a white-filled 21 does",
      identical(env$marker_half_extent(16), c(0.375, 0.375)) && identical(env$marker_half_extent(1), c(0, 0)) &&
        identical(env$marker_half_extent(21, NA), c(0, 0)) && identical(env$marker_half_extent(21, "white"), c(0.375, 0.375)) &&
        env$marker_half_extent(17)[2] > env$marker_half_extent(16)[2])

# A rising control and a falling curve whose replicates are tight everywhere (every bar shorter than the marker), and a
# falling curve with a wide last point (that bar is visible anyway).
t <- seq(0, 120, 10); set.seed(3)
fall <- 0.05 * exp(0.03 * pmin(t, 60)) * ifelse(t > 60, exp(-0.07 * (t - 60)), 1)
reps <- function(base, last, sd = 0.01) { m <- sapply(1:3, function(r) round(base * exp(rnorm(length(t), 0, sd)), 5)); m[length(t), ] <- last; m }
d <- data.frame(Time = t, reps(0.05 * exp(0.03 * t), c(0.31, 0.30, 0.32)), reps(fall * 1.3, c(0.02, 0.0205, 0.0195)), reps(fall, c(0.002, 0.004, 0.03)))
names(d) <- c("Time", rep(c("Ctrl", "Tight", "Wide"), each = 3))

svg_of <- function(p, sz) { f <- tempfile(fileext = ".svg"); ggplot2::ggsave(f, p, device = svglite::svglite, width = sz[1], height = sz[2], units = "in", bg = "white", limitsize = FALSE); readLines(f, warn = FALSE) }
lines_of <- function(s) { m <- regmatches(s, gregexpr("<line x1='[-0-9.]+' y1='[-0-9.]+' x2='[-0-9.]+' y2='[-0-9.]+'[^>]*>", s)); unlist(m) }
seg <- function(l) { v <- as.numeric(regmatches(l, gregexpr("[-0-9.]+(?=')", l, perl = TRUE))[[1]][1:4]); v }
has_layer <- function(p) any(vapply(p$layers, function(l) inherits(l$geom, "GeomKcCapPastMarker"), NA))
R <- new.env()
testServer(app, {
  S <- session$setInputs
  ingest_data(d, "d.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars, export_format = "svg")
  sz <- export_size_in()
  p_on <- main_plot_obj(); R$on <- svg_of(p_on, sz); R$layer_on <- has_layer(generate_plot())
  R$mk <- env$marker_extent_pt(sel_samples(), resolve_aesthetics(sel_samples()), size = env$APP_DEFAULTS$shape_size, stroke = env$APP_DEFAULTS$point_stroke)
  S(error_cap_past_marker = FALSE); R$off <- svg_of(main_plot_obj(), sz); R$layer_off <- has_layer(generate_plot())
  S(error_cap_past_marker = TRUE, show_points = FALSE); R$layer_nopts <- has_layer(generate_plot())
  S(show_points = TRUE, error_bar_style = "solid"); R$layer_solid <- has_layer(generate_plot())
  S(error_bar_style = "T", shape_filled_Tight = FALSE, shape_filled_Ctrl = FALSE, shape_filled_Wide = FALSE)
  R$layer_open <- has_layer(generate_plot())
  S(shape_filled_Tight = TRUE, shape_filled_Ctrl = TRUE, shape_filled_Wide = TRUE, error_bar_width = 12)   # caps already wider than the marker
  R$wide_cap <- svg_of(main_plot_obj(), sz)
  S(error_bar_width = env$APP_DEFAULTS$error_bar_width, shape_size = 3)                                     # a bigger marker hides more
  R$big <- svg_of(main_plot_obj(), sz)
  # settings and project files carry the switch
  S(shape_size = env$APP_DEFAULTS$shape_size, error_cap_past_marker = FALSE)
  R$settings <- settings_json_list()$global_settings$error_cap_past_marker
  pf <- tempfile(fileext = ".kcproj"); write_project_file(pf); R$proj <- fromJSON(pf, simplifyVector = FALSE)$settings$global_settings$error_cap_past_marker
})
added <- setdiff(lines_of(R$on), lines_of(R$off))
check("on (the default): the tight curve's bars get caps; the figure is otherwise identical (same file without the added caps)",
      length(added) > 0 && identical(R$on[!grepl("<line ", R$on)], R$off[!grepl("<line ", R$off)]) &&
        identical(R$on[grepl("<line ", R$on)] |> lines_of() |> setdiff(added), lines_of(R$off)))
w <- vapply(added, function(l) { v <- seg(l); abs(v[3] - v[1]) }, 0)
want <- 2 * (R$mk$kc_mk + env$MARKER_CAP_MARGIN_PT)
check("each added cap reaches past its marker on both sides (width = that marker's width + 2 x 1 pt)",
      all(vapply(w, function(x) any(abs(x - want) < 0.05), NA)) && all(R$mk$kc_mk > 0))
check("the extra layer is there when on; off, no points, a capless style or caps already wider than the marker add nothing",
      isTRUE(R$layer_on) && !isTRUE(R$layer_off) && !isTRUE(R$layer_nopts) && !isTRUE(R$layer_solid) &&
        length(setdiff(lines_of(R$wide_cap), lines_of(R$off))) == 0L)
check("open (white-filled) markers hide a bar too, so they get the caps as well", isTRUE(R$layer_open))
big_added <- setdiff(lines_of(R$big), lines_of(R$off)); wb <- vapply(big_added, function(l) { v <- seg(l); abs(v[3] - v[1]) }, 0)
check("a bigger marker: at least as many bars get caps, and the caps grow with the marker", length(big_added) >= length(added) && max(wb) > max(w) + 2)
# the wide bar at the last point of "Wide" is visible by itself: no cap is added at the last time point for it
check("only hidden bars get the caps: fewer caps than bars", length(added) / 2 < 3 * length(t))
check("Save Settings and the project file keep the switch", identical(R$settings, FALSE) && identical(R$proj, FALSE))

cat("\n--- 'drop readings outside the axis' keeps a bar whose mean is on the axis ---\n")
C <- new.env()
testServer(app, {
  S <- session$setInputs
  ingest_data(d, "d.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars, export_format = "svg")
  S(y_oob = "censor", custom_y_limits = TRUE, y_min = 0.004, y_max = 1)
  p <- generate_plot(); b <- suppressWarnings(ggplot2::ggplot_build(p))
  i <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomErrorbar") && !inherits(l$geom, "GeomKcCapPastMarker"), NA))[1]
  ld <- p$layers[[i]]$data; ed <- b$data[[i]]
  C$on_axis <- sum(ld$mean_value >= 0.004 & ld$mean_value <= 1); C$kept <- sum(is.finite(ed$ymin) & is.finite(ed$ymax))
  C$last_wide <- ed[ed$x == 120 & abs(ed$ymin - log10(0.004)) < 1e-9, , drop = FALSE]
  S(y_oob = "squish"); p2 <- generate_plot(); b2 <- suppressWarnings(ggplot2::ggplot_build(p2)); C$squish_kept <- sum(is.finite(b2$data[[i]]$ymin))
})
check("censor: every bar whose mean is on the axis is drawn (none deleted for an end past the limit)", C$kept == C$on_axis)
check("censor: the wide last bar runs down to the axis floor instead of vanishing", nrow(C$last_wide) == 1L)
check("squish draws the same bars as before", C$squish_kept >= C$on_axis)

cat("\n", pass, "passed,", fail, "failed\n")
if (fail > 0) quit(status = 1)
