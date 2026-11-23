# ============================================================================
# v2.26.0: one dataset split into panels. Any number of panels (odd numbers too), a sample in every panel (the control)
# or in several ("A; B"), panel letters (A, B, C / a, b, c) with or without the names, panel size (shared, fitted to a
# journal column, or full size each), and "A legend in each panel": every panel its own figure with its own legend,
# title, axis titles and ranges.
# Run from the repository root:  Rscript tests/test_v226_panels.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v226_panels.R")
Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_paper_style_here.json"))
pass <- 0L; fail <- 0L
check  <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.26.0 or later", package_version(env$APP_VERSION) >= "2.26.0")
full <- { rt <- htmltools::renderTags(env$ui); gsub("[ \t\r\n]+", " ", paste(rt$head, rt$html)) }
has <- function(...) all(vapply(c(...), function(s) grepl(s, full, fixed = TRUE), logical(1)))

# twelve samples: a control and eleven phages, three replicates each
twelve <- local({
  t <- seq(0, 180, 15); set.seed(3); cols <- list(Time = t)
  grow <- function(t) 0.05 * exp(0.025 * t) / (1 + 0.05 * (exp(0.025 * t) - 1) / 1.2)
  nm <- c("Control", paste("Phage", 1:11))
  for (i in seq_along(nm)) { b <- grow(t); if (i > 1) { lt <- 60 + 8 * i; b <- pmax(ifelse(t > lt, b * exp(-0.04 * (t - lt)), b), 0.02) }
    for (r in 1:3) cols[[length(cols) + 1]] <- round(b * exp(rnorm(length(t), 0, 0.03)), 4) }
  d <- as.data.frame(cols); names(d) <- c("Time", rep(nm, each = 3)); d
})

cat("\n--- controls and defaults ---\n")
check("the Panels section: split box, 'Each sample its own panel', every-panel samples, letters, size, legends, names",
      has("Split the lysis curve into panels", "Each sample its own panel", "id=\"facet_every\"", "id=\"facet_letters\"", "id=\"facet_size_mode\"",
          "id=\"facet_legends\"", "A legend in each panel", "Panel names:", "Fit a double column (page width)"))
tl <- tail(names(env$APP_DEFAULTS), 4)
check("new defaults are appended at the end: letters none, size shared, one legend, 2 picked panels (the faceted figure is unchanged)",
      identical(tl, c("facet_letters", "facet_size_mode", "facet_legends", "facet_n_panels")) && identical(env$APP_DEFAULTS$facet_n_panels, 2) && identical(env$APP_DEFAULTS$facet_letters, "none") &&
        identical(env$APP_DEFAULTS$facet_size_mode, "share") && identical(env$APP_DEFAULTS$facet_legends, "shared"))

cat("\n--- pure helpers ---\n")
s3 <- c("a", "b", "c")
pm0 <- env$panel_membership(s3, c(a = "G1", b = "G1", c = "G2")); gm0 <- env$sample_group_map(s3, c(a = "G1", b = "G1", c = "G2"))
check("one panel per sample: the same panels and order as before (sample_group_map)",
      identical(pm0$levels, attr(gm0, "levels")) && identical(unlist(pm0$map), unclass(gm0)[s3] |> setNames(s3)))
pm1 <- env$panel_membership(c("C", "a", "b", "c"), c(C = "", a = "P1", b = "P1; P2", c = "P2"), every = "C")
check("a control in every panel and a sample in two panels ('P1; P2')",
      identical(pm1$levels, c("P1", "P2")) && identical(pm1$map$C, c("P1", "P2")) && identical(pm1$map$b, c("P1", "P2")) && identical(pm1$map$c, "P2"))
check("a sample with no panel goes to 'Other'; fewer than two panels is no split",
      identical(env$panel_membership(s3, c(a = "X", b = "Y", c = ""))$levels, c("X", "Y", "Other")) &&
        is.null(env$panel_membership(s3, c(a = "X", b = "X", c = "X"))) && is.null(env$panel_membership(s3, NULL)) &&
        is.null(env$panel_membership(s3, c(a = "X", b = "Y", c = "Z"), every = s3)))
dd <- data.frame(variable = c("C", "a", "b", "c"), v = 1:4)
ex <- env$panel_expand(dd, pm1)
check("panel_expand repeats a row once per panel, in order, with the panel as the facet variable",
      nrow(ex) == 6L && identical(as.character(ex$group), c("P1", "P2", "P1", "P1", "P2", "P2")) && identical(ex$v, c(1L, 1L, 2L, 3L, 3L, 4L)))
check("panel_expand of a plain assignment keeps the rows and their order", { e0 <- env$panel_expand(data.frame(variable = s3, v = 1:3), pm0); identical(e0$v, 1:3) && identical(as.character(e0$group), c("G1", "G1", "G2")) })
check("panel letters A..Z then AA, AB; lower case too", identical(env$panel_letter(c(1, 2, 26, 27, 28)), c("A", "B", "Z", "AA", "AB")) && identical(env$panel_letter(1:3, "lower"), c("a", "b", "c")))
check("part labels: 1, 2, 3 / i, ii, iii / A1, A2 (the figure's letter as the prefix, A when blank)",
      identical(env$panel_letter(1:3, "number"), c("1", "2", "3")) && identical(env$panel_letter(c(1, 4, 9), "roman"), c("i", "iv", "ix")) &&
        identical(env$panel_letter(1:2, "prefixed", "B"), c("B1", "B2")) && identical(env$panel_letter(1:2, "prefixed", ""), c("A1", "A2")))
check("strip text: bold letter then the name, or the letter alone with the names off",
      identical(unname(env$facet_strip_texts(c("x", "y"), "upper", TRUE)), c("**A**  x", "**B**  y")) &&
        identical(unname(env$facet_strip_texts(c("x", "y"), "lower", FALSE)), c("a", "b")) && identical(unname(env$facet_strip_texts(c("x", "y"), "none")), c("x", "y")))
fp <- env$fit_panel_size(183, 170, c(65, 65), c(30, 20), 3, 2)
check("fit_panel_size: (183 - 30) / 3 = 51 mm square panels; capped by the maximum height", isTRUE(all.equal(fp, c(51, 51))) &&
        isTRUE(all.equal(env$fit_panel_size(183, 100, c(65, 65), c(30, 20), 3, 2), c(40, 40))) && env$fit_panel_size(20, NA, c(65, 65), c(30, 20), 3, 2)[1] == 10)

cat("\n--- the split figure ---\n")
r <- list(); notes <- character(0)
env$showNotification <- function(ui, ..., type = "default") { notes <<- c(notes, paste0("[", type, "] ", as.character(ui))); invisible(NULL) }
sfile <- tempfile(fileext = ".json")
strip_text <- function(p) { g <- ggplot2::ggplotGrob(p); i <- grep("^strip", g$layout$name); i <- i[order(g$layout$t[i], g$layout$l[i])]; unlist(lapply(g$grobs[i], function(s) env$legend_key_labels(s))) }
testServer(app, {
  S <- session$setInputs
  ingest_data(twelve, "twelve"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars, export_format = "svg")
  S(facet_by_group = TRUE, facet_source = "each", facet_every = "Control")
  b <- ggplot2::ggplot_build(generate_plot()); r$n11 <<- nrow(b$layout$layout)
  r$ctrl_everywhere <<- { d1 <- b$data[[which(vapply(b$plot$layers, function(l) inherits(l$geom, "GeomLine"), NA))[1]]]
                          all(seq_len(11) %in% as.integer(d1$PANEL[toupper(substr(d1$colour, 1, 7)) == "#000000" & d1$x == 0])) }
  for (k in c(4, 6, 8)) { S(selected_samples = rv$od_vars[seq_len(k)]); r$odd[[as.character(k - 1)]] <<- nrow(ggplot2::ggplot_build(generate_plot())$layout$layout) }
  S(selected_samples = rv$od_vars[1:6])
  S(facet_letters = "upper"); r$strips_both <<- tryCatch(strip_text(generate_plot()), error = function(e) conditionMessage(e))
  S(facet_strip_style = "none"); r$strips_letters <<- tryCatch(strip_text(generate_plot()), error = function(e) conditionMessage(e))
  S(facet_letters = "none"); r$strips_none <<- tryCatch(strip_text(generate_plot()), error = function(e) conditionMessage(e))
  S(facet_strip_style = "plain", facet_letters = "upper")
  for (m in c("share", "fit_single", "fit_double", "each")) { S(facet_size_mode = m); r$size[[m]] <<- attr(main_plot_obj(), "size_mm") }
  r$export_mm <<- export_size_in() * 25.4
  r$legend <<- figure_legend_string()
  # one letter for the whole set (Panel label) with numbered parts; A1, A2 parts carry it instead
  S(facet_letters = "number", panel_label = "A"); g1 <- ggplot2::ggplotGrob(generate_plot())
  r$whole_num <<- any(grepl("^tag", g1$layout$name) & !vapply(g1$grobs, inherits, NA, "zeroGrob")); r$strips_num <<- strip_text(generate_plot())
  S(facet_letters = "prefixed", facet_strip_style = "none"); g2 <- ggplot2::ggplotGrob(generate_plot())
  r$whole_pref <<- any(grepl("^tag", g2$layout$name) & !vapply(g2$grobs, inherits, NA, "zeroGrob")); r$strips_pref <<- strip_text(generate_plot())
  r$legend_pref <<- figure_legend_string()
  S(facet_legends = "each", facet_letters = "lower", facet_strip_style = "plain", panel_label = "B")
  g3 <- main_plot_obj(); r$comp_whole <<- "whole-tag" %in% g3$layout$name; r$comp_tags <<- vapply(g3$grobs[grepl("^tag-", g3$layout$name)], function(x) x$label, "")
  r$comp_sub_tags <<- vapply(main_group_plots(min_panels = 2L), function(z) is.null(z$labels$tag), NA)
  S(facet_letters = "prefixed"); g4 <- main_plot_obj(); r$comp_whole_pref <<- "whole-tag" %in% g4$layout$name; r$comp_tags_pref <<- vapply(g4$grobs[grepl("^tag-", g4$layout$name)], function(x) x$label, "")
  S(facet_legends = "shared", facet_letters = "upper", panel_label = "")
  # a legend in each panel, each panel set on its own
  S(facet_legends = "each", facet_size_mode = "fit_double")
  S(panelcfg_Phage_1_title = "*mltG* knockout", panelcfg_Phage_1_legend = "bottom", panelcfg_Phage_2_ymin = 0.05, panelcfg_Phage_2_ylab = "OD_{600}",
    panelcfg_Phage_3_legend = "none", panelcfg_Phage_4_legend_title = "Strain", panelcfg_Phage_5_xmax = 120)
  g <- main_plot_obj(); r$each_class <<- class(g)[1]; r$each_size <<- attr(g, "size_mm"); r$each_export <<- export_size_in() * 25.4
  r$each_tags <<- sum(grepl("^tag-", g$layout$name)); r$each_cells <<- sum(grepl("^cell-", g$layout$name))
  pl <- main_group_plots(min_panels = 2L); r$each_n <<- length(pl)
  r$p1_title <<- deparse(pl[[1]]$labels$title); r$p1_legpos <<- ggplot2::calc_element("legend.position", ggplot2::theme_get() + pl[[1]]$theme)
  r$p3_legpos <<- pl[[3]]$theme$legend.position
  r$p2_ylim <<- pl[[2]]$coordinates$limits$y; r$p5_xlim <<- pl[[5]]$coordinates$limits$x
  r$p4_names <<- unique(vapply(Filter(function(sc) any(sc$aesthetics %in% c("colour", "shape")), pl[[4]]$scales$scales), function(sc) paste(as.character(sc$name), collapse = ""), ""))
  r$p1_samples <<- sort(unique(as.character(pl[[1]]$data$variable)))
  r$svg <<- { f <- output$downloadPlot; c(exists = file.exists(f), bytes = file.size(f)) }
  r$key <<- env$legend_key_labels(legend_key_for(generate_plot()))
  r$doc <<- settings_json_list(); writeLines(jsonlite::toJSON(r$doc, auto_unbox = TRUE, null = "null", na = "null"), sfile)
  S(clearSettings = 1, clearSettings_confirm = 1); r$reset <<- list(cfg = length(rv$panel_cfg), every = length(every_wanted()))
  notes <<- character(0)
  S(loadSettings = data.frame(name = "s.json", size = file.size(sfile), type = "application/json", datapath = sfile, stringsAsFactors = FALSE))
  r$load_status <<- paste(rv$settings_status$msg, rv$settings_status$details %||% ""); r$loaded_cfg <<- rv$panel_cfg; r$loaded_every <<- every_wanted()
  # the composer's "one panel per group" takes the same panels (here: typed boxes, the control in each)
  S(facet_legends = "shared", facet_source = "manual"); ids <- vapply(rv$od_vars, env$safe_id, "")
  a <- list(); pn <- c("", "early", "early", "late", "late", "late"); for (i in 1:6) a[[paste0("facet_of_", ids[i])]] <- pn[i]; do.call(S, a)
  cp <- main_group_plots(); r$comp <<- lapply(cp, function(z) sort(unique(as.character(z$data$variable))))
  r$comp_facets <<- vapply(cp, function(z) inherits(z$facet, "FacetNull"), NA)
})
check("each sample its own panel with the control in every one: 11 panels for 12 samples", identical(r$n11, 11L))
check("the control is drawn in all 11 panels", isTRUE(r$ctrl_everywhere))
check("odd numbers of panels work: 3, 5 and 7 panels", identical(unname(unlist(r$odd)), c(3L, 5L, 7L)))
check("letters with names: 'A  Phage 1' ... at the left of each panel", length(r$strips_both) == 5L && grepl('bold("A")', r$strips_both[1], fixed = TRUE) && grepl("Phage 1", r$strips_both[1], fixed = TRUE) && any(grepl("Phage 5", r$strips_both)))
check("names off, letters on: the strips read A, B, C, D, E", identical(r$strips_letters, c("A", "B", "C", "D", "E")))
check("names off, letters off: no strip text at all", length(r$strips_none) == 0L || all(!nzchar(r$strips_none)))
check("one letter for the whole set: the Panel label 'A' once, with the parts numbered 1..5 in the strips", isTRUE(r$whole_num) && all(grepl("^.*\\b[1-5]\\b", r$strips_num)) && length(r$strips_num) == 5L)
check("A1, A2 parts: the strips read A1..A5 and the whole-figure letter is not drawn a second time",
      identical(r$strips_pref, paste0("A", 1:5)) && !isTRUE(r$whole_pref) && grepl("Panels: (A1): Control, Phage 1; (A2): Control, Phage 2", r$legend_pref, fixed = TRUE))
check("a legend in each panel: the set's letter 'B' once at the top, the panels a..e, and no letter inside the sub-figures",
      isTRUE(r$comp_whole) && identical(r$comp_tags, letters[1:5]) && all(r$comp_sub_tags))
check("a legend in each panel with A1, A2 parts: B1..B5, no separate set letter", !isTRUE(r$comp_whole_pref) && identical(r$comp_tags_pref, paste0("B", 1:5)))
check("size: shared keeps the figure small; fit one column = 89 mm; fit double = 183 mm; full size each is wider still",
      abs(r$size$fit_single[1] - 89) < 0.6 && abs(r$size$fit_double[1] - 183) < 0.6 && r$size$each[1] > r$size$fit_double[1] && r$size$share[1] < r$size$fit_single[1] + 30)
check("the export size is the figure's size", isTRUE(all.equal(unname(r$export_mm), unname(r$size$each), tolerance = 1e-3)))
check("the legend text says what each panel holds, with the letters",
      grepl("Panels: (A) Phage 1: Control, Phage 1; (B) Phage 2: Control, Phage 2", r$legend, fixed = TRUE))
check("a legend in each panel: one figure of 5 cells with 5 letters, 183 mm wide, exported at that size",
      identical(r$each_class, "gtable") && r$each_cells == 5L && r$each_tags == 5L && abs(r$each_size[1] - 183) < 0.6 &&
        isTRUE(all.equal(unname(r$each_export), unname(r$each_size), tolerance = 1e-3)))
check("each panel figure holds its own samples only (control + its phage)", identical(r$p1_samples, c("Control", "Phage 1")) && r$each_n == 5L)
check("panel settings: title with markup, legend at the bottom, no legend, y from 0.05, x to 120, own y label",
      grepl("mltG", r$p1_title) && identical(r$p1_legpos, "bottom") && identical(r$p3_legpos, "none") &&
        isTRUE(all.equal(r$p2_ylim[1], 0.05)) && is.na(r$p2_ylim[2]) && is.na(r$p5_xlim[1]) && isTRUE(all.equal(r$p5_xlim[2], 120)))
check("panel legend title: every key scale of that panel is renamed", identical(r$p4_names, "Strain"))
check("the legend key still lists every sample once", identical(r$key[r$key != ""], c("Control", paste("Phage", 1:5))) || all(c("Control", paste("Phage", 1:5)) %in% r$key))
check("the per-panel figure exports as SVG", isTRUE(as.logical(r$svg[["exists"]])) && r$svg[["bytes"]] > 1000)
check("settings: every-panel samples and each panel's settings are saved as their own keys, never as plain inputs",
      identical(unlist(r$doc$panel_every), "Control") && identical(r$doc$panel_settings[["Phage 1"]]$legend, "bottom") &&
        isTRUE(all.equal(r$doc$panel_settings[["Phage 2"]]$ymin, 0.05)) && !any(grepl("^panelcfg_|^facet_every$", names(r$doc$global_settings))))
check("Clear & Reset clears them", identical(r$reset$cfg, 0L) && identical(r$reset$every, 0L))
check("loading the file brings them back with no 'wrong type' warning",
      identical(r$loaded_every, "Control") && identical(r$loaded_cfg[["Phage 1"]]$title, "*mltG* knockout") &&
        !grepl("wrong type", r$load_status, fixed = TRUE) && !any(grepl("wrong type", notes, fixed = TRUE)))
check("the composer's per-group panels use the same panels: typed names, the control in each, no facets inside",
      identical(unname(r$comp), list(c("Control", "Phage 1", "Phage 2"), c("Control", "Phage 3", "Phage 4", "Phage 5"))) && all(r$comp_facets))

check("the size bar of a split figure gives the panel count, one panel's size and the page-width fit",
      grepl("n_pan >= 2L) paste0(n_pan, \" panels, each\")", paste(readLines("Lysis_Curve_App_v28.R", warn = FALSE), collapse = "\n"), fixed = TRUE))

cat("\n--- picking the samples of each panel; what each panel shows; samples styled per panel ---\n")
q <- list(); notes <- character(0); sfile2 <- tempfile(fileext = ".json")
testServer(app, {
  S <- session$setInputs
  ingest_data(twelve, "twelve"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars, export_format = "svg")
  S(facet_by_group = TRUE, facet_source = "pick", facet_n_panels = 3, facet_letters = "upper")
  q$pick_ui <- tryCatch(paste(as.character(output$facet_pick_ui$html), collapse = ""), error = function(e) paste("ERROR", conditionMessage(e)))   # before anything is picked
  q$cfg_ui_err <- NULL
  S(panelpick_1_name = "Early lysis", panelpick_1_samples = c("Control", "Phage 1", "Phage 2"),
    panelpick_2_name = "Late lysis", panelpick_2_samples = c("Control", "Phage 9", "Phage 10", "Phage 11"),
    panelpick_3_name = "Both", panelpick_3_samples = c("Phage 1", "Phage 11"))
  pm <- panel_membership_now(sel_samples(), resolve_aesthetics(sel_samples())); q$levels <- pm$levels
  b <- ggplot2::ggplot_build(generate_plot()); q$n <- nrow(b$layout$layout)
  q$key <- env$legend_key_labels(main_legend_key()); q$legend <- figure_legend_string()
  S(facet_legends = "each", error_type = "sd", panelcfg_Late_lysis_hide_points = TRUE, panelcfg_Both_hide_errors = TRUE, panelcfg_Early_lysis_hide_lines = TRUE)
  q$cfg_ui <- tryCatch(paste(as.character(output$panel_custom_ui$html), collapse = ""), error = function(e) paste("ERROR", conditionMessage(e)))
  pl <- main_group_plots(min_panels = 2L); q$geoms <- lapply(pl, function(z) vapply(z$layers, function(l) class(l$geom)[1], ""))
  S(panelsmp_Late_lysis__Control_color = "grey60", panelsmp_Late_lysis__Control_line = "dashed", panelsmp_Late_lysis__Control_name = "Uninfected",
    panelsmp_Both__Phage_1_shape = "15", panelsmp_Both__Phage_1_filled = "open")
  pl <- main_group_plots(min_panels = 2L)
  q$late_labels <- unname(pl[[2]]$scales$get_scales("colour")$labels); q$late_cols <- unname(pl[[2]]$scales$get_scales("colour")$palette(1))
  q$early_ctrl <- unname(ggplot2::ggplot_build(pl[[1]])$plot$scales$get_scales("colour")$labels[1])
  q$both_shape <- { b3 <- ggplot2::ggplot_build(pl[[3]]); i <- which(vapply(pl[[3]]$layers, function(l) inherits(l$geom, "GeomPoint") && "shape" %in% names(l$mapping), NA))[1]
                     d3 <- b3$data[[i]]; unique(d3$shape[toupper(d3$colour) == toupper(unname(resolve_aesthetics(sel_samples())$colors["Phage 1"]))]) }
  q$key2 <- env$legend_key_labels(main_legend_key()); q$legend2 <- figure_legend_string()
  q$doc <- settings_json_list(); writeLines(jsonlite::toJSON(q$doc, auto_unbox = TRUE, null = "null", na = "null"), sfile2)
  S(clearSettings = 1, clearSettings_confirm = 1); q$reset <- c(length(rv$panel_picks), length(rv$panel_cfg))
  notes <<- character(0)
  S(loadSettings = data.frame(name = "s.json", size = file.size(sfile2), type = "application/json", datapath = sfile2, stringsAsFactors = FALSE))
  q$status <- paste(rv$settings_status$msg, rv$settings_status$details %||% ""); q$picks <- rv$panel_picks; q$cfg <- rv$panel_cfg
  q <<- q
})
check("pick mode: the picker shows a name box and a sample list for each of the 3 panels, before anything is picked",
      !startsWith(q$pick_ui, "ERROR") && all(vapply(1:3, function(k) grepl(paste0("panelpick_", k, "_samples"), q$pick_ui, fixed = TRUE), NA)))
check("each panel's settings block renders, with its samples and the hide boxes",
      !startsWith(q$cfg_ui, "ERROR") && grepl("panelcfg_Both_hide_errors", q$cfg_ui, fixed = TRUE) && grepl("panelsmp_Late_lysis__Control_color", q$cfg_ui, fixed = TRUE))
check("pick mode: three named panels in the order they were set up", identical(q$levels, c("Early lysis", "Late lysis", "Both")) && identical(q$n, 3L))
check("a sample picked for no panel is left off the figure, its key and the legend text",
      identical(sort(q$key), sort(c("Control", "Phage 1", "Phage 2", "Phage 9", "Phage 10", "Phage 11"))) && !grepl("Phage 5", q$legend, fixed = TRUE) &&
        grepl("(A) Early lysis: Control, Phage 1, Phage 2; (B) Late lysis: Control, Phage 9, Phage 10, Phage 11; (C) Both: Phage 1, Phage 11", q$legend, fixed = TRUE))
check("each panel shows what was set: A without lines, B without points, C without error bars",
      !("GeomLine" %in% q$geoms[[1]]) && "GeomPoint" %in% q$geoms[[1]] && !("GeomPoint" %in% q$geoms[[2]]) && "GeomLine" %in% q$geoms[[2]] &&
        !("GeomErrorbar" %in% q$geoms[[3]]) && "GeomErrorbar" %in% q$geoms[[2]])
check("a sample styled in one panel only: the control is 'Uninfected' in B and 'Control' in A",
      identical(q$late_labels[1], "Uninfected") && identical(q$early_ctrl, "Control"))
check("an open square for Phage 1 in panel C only", isTRUE(q$both_shape %in% c(0, 22)))
check("the legend key follows the panels: each panel's key under its letter and name",
      all(c("A Early lysis", "B Late lysis", "C Both", "Uninfected") %in% q$key2) && sum(q$key2 == "Control") == 1L)
check("the legend text describes a sample styled in a panel", grepl("Uninfected (grey, dashed line", q$legend2, fixed = TRUE) || grepl("Uninfected (", q$legend2, fixed = TRUE))
check("settings: the picks, the per-panel options and the per-panel sample styles are saved as their own keys",
      length(q$doc$panel_picks) == 3L && identical(q$doc$panel_picks[[2]]$name, "Late lysis") && isTRUE(q$doc$panel_settings[["Both"]]$hide_errors) &&
        identical(q$doc$panel_settings[["Late lysis"]]$samples$Control$line, "dashed") && !any(grepl("^panel(pick|smp|cfg)_", names(q$doc$global_settings))))
check("Clear & Reset clears the picks and the per-panel settings", identical(q$reset, c(0L, 0L)))
check("loading brings them back, no 'wrong type' warning",
      identical(q$picks[[3]]$samples, c("Phage 1", "Phage 11")) && isTRUE(q$cfg[["Both"]]$hide_errors) && identical(q$cfg[["Late lysis"]]$samples$Control$color, "grey60") &&
        !grepl("wrong type", q$status, fixed = TRUE) && !any(grepl("wrong type", notes, fixed = TRUE)))

cat("\n--- the illustrated tutorial ---\n")
tut <- readLines("docs/TUTORIAL.md", warn = FALSE, encoding = "UTF-8")
imgs <- unique(unlist(regmatches(tut, gregexpr("tutorial/[0-9a-z_]+\\.(jpg|png)", tut))))
check("docs/TUTORIAL.md shows at least 40 screenshots, every one present in docs/tutorial/",
      length(imgs) >= 40L && all(file.exists(file.path("docs", imgs))))
check("every screenshot in docs/tutorial/ is used by the tutorial", all(basename(list.files("docs/tutorial")) %in% basename(imgs)))
h <- env$render_help_doc("docs/TUTORIAL.md", list(), "docs", prefix = "help_TUTORIAL")
toc <- unique(regmatches(h, gregexpr('href="#help_TUTORIAL-[^"]+"', h))[[1]]); ids <- regmatches(h, gregexpr('id="help_TUTORIAL-[^"]+"', h))[[1]]
check("in the Help tab every contents link of the tutorial has its heading", length(toc) >= 20L && all(sub('^href="#', 'id="', toc) %in% ids))
check("the tutorial is the first guide of the Help tab (the labelling tutorial stays open by default)",
      identical(env$HELP_GUIDES[[1]]$file, "docs/TUTORIAL.md") && isTRUE(env$HELP_GUIDES[[2]]$open))

cat(sprintf("\nSUMMARY: %d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1)
