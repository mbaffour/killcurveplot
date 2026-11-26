# ============================================================================
# v2.29.0: fixes from the 2026-11-25 review (two code reviews of v2.27.0 - v2.28.3) and the audit of the legend-key rule.
# Each block names the finding it guards (A = review of v2.27.x, B = review of v2.28.x, gap = rules audit).
# Run from the repository root:  Rscript tests/test_v229_review_fixes.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v229_review_fixes.R")
Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_paper_style_here.json"))
options(killcurve.themes_dir = file.path(tempdir(), paste0("kc_rf_", Sys.getpid())))
pass <- 0L; fail <- 0L
check <- function(desc, cond) { if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") } else { fail <<- fail + 1L; cat("FAIL:", desc, "\n") } }
env <- new.env(); app <- source("Lysis_Curve_App_v28.R", local = env)$value
calls <- list()
for (fn in c("updateSelectInput", "updateTextInput")) local({
  orig <- get(fn, envir = asNamespace("shiny")); nm <- fn
  assign(fn, function(session, inputId, ...) { calls[[length(calls) + 1L]] <<- list(id = inputId, args = list(...)); orig(session, inputId, ...) }, envir = env)
})
last_call <- function(id) { hit <- Filter(function(z) identical(z$id, id), calls); if (length(hit)) hit[[length(hit)]]$args else NULL }
mkd <- function(lst, t, sd = 0.03, seed = 2) { set.seed(seed); cols <- list(Time = t); nm <- character(0)
  for (n in names(lst)) for (r in 1:3) { cols[[length(cols) + 1]] <- round(lst[[n]] * exp(rnorm(length(t), 0, sd)), 5); nm <- c(nm, n) }
  d <- as.data.frame(cols); names(d) <- c("Time", nm); d }
layer_of <- function(p, cl, not = character(0)) which(vapply(p$layers, function(l) inherits(l$geom, cl) && !inherits(l$geom, not), NA))
klabs <- function(k) if (is.null(k)) character(0) else env$legend_key_labels(k)

cat("\n--- A1: a typed x range never draws a reading from beyond it ---\n")
t2 <- seq(0, 180, 2); d1 <- mkd(list(Ctrl = 0.05 * exp(0.02 * t2), Phage = 0.05 * exp(0.018 * t2)), t2)
X <- new.env()
testServer(app, {
  S <- session$setInputs
  ingest_data(d1, "dense.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars)
  S(custom_x_limits = TRUE, x_min = 0, x_max = 120, x_expand_right = 0, error_bar_width = 6)
  p <- generate_plot(); b <- suppressWarnings(ggplot2::ggplot_build(p))
  pt <- b$data[[layer_of(p, "GeomPoint")[1]]]; eb <- b$data[[layer_of(p, "GeomErrorbar", "GeomKcCapPastMarker")[1]]]
  X$at_edge <- sum(pt$x == 120, na.rm = TRUE); X$beyond <- sum(pt$x > 120, na.rm = TRUE); X$n_pts <- sum(is.finite(pt$x))
  X$last_cap <- eb[eb$x == 120, c("xmin", "xmax")]
  S(x_min = 61, x_max = 180); p <- generate_plot(); b <- suppressWarnings(ggplot2::ggplot_build(p))
  X$low_edge <- sum(b$data[[layer_of(p, "GeomPoint")[1]]]$x == 61, na.rm = TRUE)
})
check("0 to 120 with readings every 2 min and a wide cap: one marker per sample at 120, none moved onto the frame", X$at_edge == 2L && X$beyond == 0L && X$n_pts == 2L * 61L)
check("the last bar keeps its cap, cut at the frame (v2.27.4)", nrow(X$last_cap) == 2L && all(is.finite(X$last_cap$xmax)) && all(X$last_cap$xmax == 120))
check("a range that starts between readings draws nothing on its left edge", X$low_edge == 0L)

cat("\n--- B5 / P1: 'drop readings outside the axis' and error bars ---\n")
t <- seq(0, 120, 10); fall <- 0.05 * exp(0.03 * pmin(t, 60)) * ifelse(t > 60, exp(-0.07 * (t - 60)), 1)
d2 <- mkd(list(Ctrl = 0.05 * exp(0.03 * t), Wide = fall), t, sd = 0.05, seed = 3); d2[nrow(d2), 5:7] <- c(0.002, 0.004, 0.03)
Y <- new.env()
testServer(app, {
  S <- session$setInputs
  ingest_data(d2, "d2.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars)
  S(y_oob = "censor", custom_y_limits = TRUE, y_min = 0.015, y_max = 1)        # Wide's last mean (0.012) is below the floor, its bar top (0.021) above
  p <- generate_plot(); b <- suppressWarnings(ggplot2::ggplot_build(p))
  pt <- b$data[[layer_of(p, "GeomPoint")[1]]]; eb <- b$data[[layer_of(p, "GeomErrorbar", "GeomKcCapPastMarker")[1]]]
  pt <- pt[is.finite(pt$y), ]; eb <- eb[is.finite(eb$ymin) & is.finite(eb$ymax), ]
  key <- function(z) paste(z$x, z$colour)
  Y$bars_no_point <- sum(!key(eb) %in% key(pt)); Y$zero <- sum(eb$ymin == eb$ymax); Y$bars <- nrow(eb); Y$pts <- nrow(pt)
})
check("a bar is drawn only where its point is drawn (the dropped point's bar is dropped with it)", Y$bars_no_point == 0L && Y$bars > 0L && Y$bars <= Y$pts)
check("no bar of zero length on the frame", Y$zero == 0L)

cat("\n--- B2: 'lysis complete' needs the curve to fall below the level from above ---\n")
od_low <- c(0.02, 0.03, 0.045, 0.06, 0.075, 0.08, 0.0795, 0.079, 0.0792, 0.0788, 0.079, 0.0785, 0.078, 0.06, 0.04, 0.02, 0.01, 0.008, 0.007, 0.007, 0.007)
tl <- seq(0, 200, 10); od_hi <- 0.05 * exp(0.03 * pmin(tl, 100)) * ifelse(tl > 100, exp(-0.08 * (tl - 100)), 1)
d3 <- mkd(list(LowOD = od_low, Classic = od_hi), tl, sd = 0.002, seed = 5)
M <- new.env()
testServer(app, {
  S <- session$setInputs
  ingest_data(d3, "d3.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars)
  cm <- curve_metrics_current(); M$cm <- cm[, intersect(c("sample", "variable", "max_od", "lysis_time", "lysis_complete_time"), names(cm))]
})
row <- function(nm) M$cm[as.character(M$cm[[1]]) == nm, , drop = FALSE]
check("a curve that peaks below the level has no 'lysis complete' time (it was put before lysis began)", nrow(row("LowOD")) >= 1L && all(is.na(row("LowOD")$lysis_complete_time)))
check("a curve that falls from above the level keeps its time, after the onset", all(is.finite(row("Classic")$lysis_complete_time)) && all(row("Classic")$lysis_complete_time >= row("Classic")$lysis_time))

cat("\n--- B7: Position 'Dodge' and the caps that reach past the marker ---\n")
th <- seq(0, 2, 0.25); d4 <- mkd(list(A = 0.05 * exp(1.2 * th), B = 0.06 * exp(1.2 * th), C = 0.07 * exp(1.2 * th)), th, sd = 0.01, seed = 4)
D <- new.env()
caps_of <- function(g, sz) { f <- tempfile(fileext = ".svg"); ggplot2::ggsave(f, g, device = svglite::svglite, width = sz[1], height = sz[2], units = "in", bg = "white", limitsize = FALSE)
  s <- readLines(f, warn = FALSE); m <- unlist(regmatches(s, gregexpr("<line x1='[-0-9.]+' y1='[-0-9.]+' x2='[-0-9.]+' y2='[-0-9.]+'[^>]*>", s))); m }
testServer(app, {
  S <- session$setInputs
  ingest_data(d4, "hours.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars, export_format = "svg"); sz <- export_size_in()
  S(error_cap_past_marker = FALSE); off <- caps_of(main_plot_obj(), sz)
  S(error_cap_past_marker = TRUE); D$mid <- length(setdiff(caps_of(main_plot_obj(), sz), off))
  S(error_bar_position = "dodge", error_cap_past_marker = FALSE); offd <- caps_of(main_plot_obj(), sz)
  S(error_cap_past_marker = TRUE); D$dodge <- length(setdiff(caps_of(main_plot_obj(), sz), offd))
})
check("bars under their marker get the caps; bars that Dodge moved beside their marker do not (only the middle sample's stay under it)",
      D$mid > 0L && D$dodge > 0L && D$dodge < D$mid)

cat("\n--- A2, A3, A5: what a project brings back ---\n")
t5 <- seq(0, 120, 20); d5 <- mkd(list(Control = 0.05 * exp(0.02 * t5), `Phage A` = 0.05 * exp(0.018 * t5), `Phage B` = 0.05 * exp(0.016 * t5)), t5)
P <- new.env()
testServer(app, {
  S <- session$setInputs
  ingest_data(d5, "p.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars)
  # A2: a sample with no sidebar row takes the style the loaded file gave it
  rv$imported_settings <- list(global = list(), samples = list(`Phage B` = list(color = "#654321", shape = 15, shape_filled = FALSE, line_type = "dotted", group = "Phages", tw_min = 30, tw_max = 150)))
  a <- resolve_aesthetics(rv$od_vars); P$a2 <- list(col = a$colors[["Phage B"]], lt = a$line_types[["Phage B"]], shp = a$shapes[["Phage B"]], fil = a$filled_map[["Phage B"]], grp = a$groups[["Phage B"]])
  P$tw <- time_window_for("Phage B"); P$saved <- settings_json_list(all_samples = TRUE)$sample_aesthetics[["Phage B"]]
  P$other <- a$colors[["Control"]]
  S(color_selector_Phage_B = "custom", color_Phage_B = "#111111", line_type_Phage_B = "solid"); P$a2_widget <- resolve_aesthetics("Phage B")$colors[["Phage B"]]
  rv$imported_settings <- NULL
  # A3: one panel per sample, a renamed sample: its panel settings are saved once
  S(facet_by_group = TRUE, facet_source = "each", qe_name_Phage_A = "mltG phage"); session$flushReact()
  S(kc_panel_legend_drop = list(panel = "mltG phage", inside = TRUE, fx = 0.6, fy = 0.2, n = 1)); session$flushReact()
  do.call(S, setNames(list("right"), panel_cfg_id("mltG phage", "legend"))); session$flushReact()
  ps <- settings_json_list()$panel_settings; P$a3_names <- names(ps); P$a3_legend <- ps[["Phage A"]]$legend
  S(facet_by_group = FALSE)
  # A5: a project saved with nothing dragged opens with nothing dragged
  pf <- tempfile(fileext = ".kcproj"); write_project_file(pf); P$has_lp <- !is.null(fromJSON(pf, simplifyVector = FALSE)$settings$label_positions)
  rv$float_pos <- list(Control = c(0.42, 0.62)); rv$note_pos <- list(`note one` = c(0.7, 0.67))
  load_project(pf, "p.kcproj"); P$a5 <- c(length(rv$float_pos), length(rv$note_pos))
})
check("A2: no sidebar row yet: colour, line, marker, fill and group come from the loaded file", identical(toupper(P$a2$col), "#654321") && identical(P$a2$lt, "dotted") &&
        P$a2$shp == 15 && identical(P$a2$fil, FALSE) && identical(P$a2$grp, "Phages") && identical(unname(P$tw), c(30, 150)))
check("A2: and are saved again as loaded (they came back as the defaults)", identical(toupper(P$saved$color), "#654321") && identical(P$saved$line_type, "dotted") && identical(P$saved$group, "Phages") && P$saved$tw_min == 30)
check("A2: a sample the file did not style is untouched, and a widget wins once it exists", identical(P$other, "#000000") && identical(toupper(P$a2_widget), "#111111"))
check("A3: a renamed sample's panel is saved once, with what the panel shows now", identical(P$a3_names, "Phage A") && identical(P$a3_legend, "right"))
check("A5: dragged label places of the previous work do not stay after a project opens", !isTRUE(P$has_lp) && identical(P$a5, c(0L, 0L)))

cat("\n--- A7, B9, P2: the excluded times when the time unit changes ---\n")
t6 <- seq(0, 720, 10); d6 <- mkd(list(S1 = 0.05 * exp(0.004 * t6), S2 = 0.04 * exp(0.004 * t6)), t6)
U <- new.env()
testServer(app, {
  S <- session$setInputs
  ingest_data(d6, "long.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars, time_convert = "none", exclude_timepoints = "620"); session$flushReact()
  calls <<- list(); S(time_convert = "min_to_h"); session$flushReact(); U$to_h <- last_call("exclude_timepoints")$value
  S(exclude_timepoints = U$to_h); session$flushReact()
  U$excl_h <- as.numeric(env$parse_excluded_timepoints(U$to_h, rv$all_timepoints / 60)); U$note <- attr(env$parse_excluded_timepoints(U$to_h, rv$all_timepoints / 60), "unmatched")
  calls <<- list(); S(time_convert = "none"); session$flushReact(); U$back <- last_call("exclude_timepoints")$value
  # P2: a settings load is not a reason to leave the box alone (only a project restores the box with the unit)
  S(exclude_timepoints = "30"); rv$load_stamp <- Sys.time(); calls <<- list(); S(time_convert = "min_to_h"); session$flushReact(); U$p2 <- last_call("exclude_timepoints")$value
  # A7: the one unit change a project causes is left alone, however late it arrives
  S(exclude_timepoints = "1.5"); rv$unit_guard <- "none"; calls <<- list(); S(time_convert = "none"); session$flushReact(); U$a7 <- last_call("exclude_timepoints"); U$guard_after <- is.null(rv$unit_guard)
})
check("B9: 620 min becomes the time point in hours (no rounding notice), and comes back as 620", isTRUE(all.equal(as.numeric(U$to_h), 620 / 60, tolerance = 1e-9)) && isTRUE(all.equal(U$excl_h, 620 / 60)) && !length(U$note) && identical(U$back, "620"))
check("P2: a unit change right after a settings load still converts the box (30 min -> 0.5 h)", identical(U$p2, "0.5"))
check("A7: the unit change a project itself causes leaves the restored box alone, and only that one", is.null(U$a7) && isTRUE(U$guard_after))

cat("\n--- B8: a sample the user hid is not put back when it stops being the blank ---\n")
BL <- new.env()
testServer(app, {
  S <- session$setInputs
  ingest_data(d5, "b.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = c("Control", "Phage A")); session$flushReact()   # Phage B hidden by the user
  S(blank_mode = "sample", blank_sample = "Phage B"); session$flushReact()
  calls <<- list(); S(blank_mode = "constant"); session$flushReact(); BL$hidden_stays <- is.null(last_call("selected_samples"))
  S(selected_samples = rv$od_vars); session$flushReact(); calls <<- list()
  S(blank_mode = "sample", blank_sample = "Phage B"); session$flushReact(); BL$taken <- last_call("selected_samples")$selected
  S(selected_samples = c("Control", "Phage A")); session$flushReact(); calls <<- list()
  S(blank_mode = "constant"); session$flushReact(); BL$back <- last_call("selected_samples")$selected
})
check("hidden by the user: stays hidden; taken off by the app: comes back", isTRUE(BL$hidden_stays) && identical(BL$taken, c("Control", "Phage A")) && identical(BL$back, c("Control", "Phage A", "Phage B")))

cat("\n--- A8: the launcher's auto-stop ---\n")
ra <- paste(readLines("run_app.R", warn = FALSE), collapse = "\n")
check("the stop waits for 15 s after the LAST close (an older timer cannot stop the app)", grepl("last_close <<- Sys.time()", ra, fixed = TRUE) && grepl("difftime(Sys.time(), last_close", ra, fixed = TRUE))

cat("\n--- B1, B3, gaps 1 to 4: keys and legend text read from each figure ---\n")
t7 <- seq(0, 180, 15); grow <- function(t) 0.05 * exp(0.025 * t) / (1 + 0.05 * exp(0.025 * t) / 1.5); lyse <- function(t, t0) grow(pmin(t, t0)) * ifelse(t > t0, exp(-0.06 * (t - t0)), 1)
d7 <- mkd(list(Host = grow(t7), `Host + phage A` = lyse(t7, 90), `Host + phage B` = lyse(t7, 120), Mutant = grow(t7) * 0.8), t7)
d8 <- mkd(list(Host = grow(t7), `Only B` = lyse(t7, 100)), t7, seed = 9)
K <- new.env()
testServer(app, {
  S <- session$setInputs
  ingest_data(d7, "k.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars, export_format = "svg", qe_name_Host___phage_A = "phiA"); session$flushReact()
  ms <- generate_analysis_plot("metric_summary"); K$ms_own <- is.null(tryCatch(env$legend_key_grob(ms), error = function(e) NULL))
  K$ms_x <- as.character(ggplot2::ggplot_build(ms)$layout$panel_params[[1]]$x$get_labels()); K$ms_key <- klabs(legend_key_for(ms)); K$ms_txt <- figure_legend_text_for(ms)
  K$auc_key <- klabs(legend_key_for(generate_analysis_plot("auc"))); K$main_key <- klabs(legend_key_for(generate_plot()))
  K$rf <- figure_key_entries(generate_analysis_plot("replicate_facets"))
  K$fit_txt <- figure_legend_text_for(generate_analysis_plot("fit_overlay"))
  S(show_points = FALSE, show_replicate_points = TRUE); K$b6 <- figure_legend_text_for(generate_plot()); S(show_points = TRUE, show_replicate_points = FALSE)
  S(compose_panels = c("main", "metric_summary")); K$comp_key <- klabs(compose_legend_key()); K$comp_txt <- compose_legend_text()
  S(compose_panels = c("main", "growth_rate")); K$comp_same <- klabs(compose_legend_key())
  compare_load_frame(d8, "second.csv"); S(compare_mode = "facet", compare_samples_b = c("Host", "Only B")); session$flushReact()
  pl <- compare_facet_plots(); b <- suppressWarnings(ggplot2::ggplot_build(pl[[1]])); ld <- b$data[[layer_of(pl[[1]], "GeomLine")[1]]]
  K$b1_na <- sum(is.na(ld$y)); K$b1_rows <- unname(table(ld$group)); K$cmp_txt <- compare_legend_text()
  S(compare_mode = "overlay"); K$ov_txt <- compare_legend_text()
  S(tw_min_Mutant = 300, tw_max_Mutant = 100); session$flushReact()
  K$g4_txt <- figure_legend_string(); K$g4_df <- as.character(legend_samples_df()[[1]]); K$g4_key <- klabs(main_legend_key())
})
check("gap 1: the metric summary has no legend of its own; its key lists the samples IT draws, in its order, by display name",
      isTRUE(K$ms_own) && length(K$ms_key) >= 1L && setequal(K$ms_key, K$ms_x) && identical(K$ms_key, K$ms_x[K$ms_x %in% K$ms_key]) && !identical(sort(K$ms_key), sort(K$main_key)))
check("gap 1 / 3: its text names those samples with the open circles the figure draws", grepl("phiA (orange, open circles)", K$ms_txt, fixed = TRUE) && !grepl("Mutant", K$ms_txt, fixed = TRUE) && !grepl("filled", K$ms_txt, fixed = TRUE))
check("gap 1: AUC (no legend either) gets a key of its four samples", setequal(K$auc_key, c("Host", "phiA", "Host + phage B", "Mutant")))
check("gap 3: the replicate small multiples are described by their panels", is.data.frame(K$rf) && setequal(K$rf$label, c("Host", "phiA", "Host + phage B", "Mutant")) && identical(K$rf$colour[K$rf$label == "Host"], "#000000"))
check("gap 3: a renamed sample is not lost from an analysis figure's text", grepl("phiA", K$fit_txt, fixed = TRUE))
check("B6: no marker is named when the mean markers are not drawn (replicate points only)", grepl("Host (black)", K$b6, fixed = TRUE) && !grepl("circles|squares|triangles", K$b6))
check("gap 2: the Compose key is the composed panels' keys, each under its panel", any(grepl("^a ", K$comp_key)) && any(grepl("^b ", K$comp_key)) && sum(K$comp_key == "Mutant") == 1L && sum(K$comp_key == "phiA") == 2L)
check("gap 2: the Compose text describes each panel", grepl("a Lysis curve", K$comp_txt, fixed = TRUE) && grepl("open circles", K$comp_txt, fixed = TRUE) && grepl("filled squares", K$comp_txt, fixed = TRUE))
check("gap 2: panels that say the same thing give one key, not two", !is.null(K$comp_same) && length(K$comp_same) >= 4L)
check("B1: Compare side by side: every curve runs from its first reading (no empty row inside a line)", K$b1_na == 0L && all(K$b1_rows == length(t7)))
check("B3: its text names the second file's own sample, under that file's name", grepl("Only B", K$cmp_txt, fixed = TRUE) && grepl("Dataset B:", K$cmp_txt, fixed = TRUE))
check("B3: the overlay text says which file is dashed", grepl("Host (Dataset B) (black, filled circles, dashed line)", K$ov_txt, fixed = TRUE))
check("gap 4: a sample with no reading left is in neither the main text, the samples table nor the key",
      !grepl("Mutant", sub("\\..*$", "", sub("^.*Curves: ", "", K$g4_txt)), fixed = TRUE) && !("Mutant" %in% K$g4_df) && !("Mutant" %in% K$g4_key) && grepl("phiA", K$g4_txt, fixed = TRUE))

cat("\n", pass, "passed,", fail, "failed\n")
if (fail > 0) quit(status = 1)
