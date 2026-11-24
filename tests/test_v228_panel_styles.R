# ============================================================================
# v2.28.0: Sample styles across panels. The k-th sample of every panel can look like the k-th sample of the first panel
# (colour, marker, fill, line), with one thing changing by panel (line type, or filled / open markers) or nothing.
# Run from the repository root:  Rscript tests/test_v228_panel_styles.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v228_panel_styles.R")
Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_paper_style_here.json"))
pass <- 0L; fail <- 0L
check <- function(desc, cond) { if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") } else { fail <<- fail + 1L; cat("FAIL:", desc, "\n") } }
env <- new.env(); app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("APP_VERSION is 2.28.0 or later", package_version(env$APP_VERSION) >= "2.28.0")
check("the choice is appended at the end of APP_DEFAULTS (v2.28.1 appends after it) and is on by default (v2.28.2: matched, line type by panel)",
      { nm <- names(env$APP_DEFAULTS); all(nm[-seq_len(match("panel_style_match", nm))] %in% c("error_cap_past_marker", "analysis_key_on_figure", "fit_key_on_figure", "compose_key_on_figure", "compare_key_on_figure", "lysis_floor_od")) } && identical(env$APP_DEFAULTS$panel_style_match, "line") &&
        identical(unname(env$PANEL_STYLE_MATCH_CHOICES), c("line", "own", "fill", "same")))
rt <- htmltools::renderTags(env$ui); h <- paste(rt$head, rt$html)
check("the Panels section has the menu 'Sample styles across panels'", grepl('id="panel_style_match"', h, fixed = TRUE))

cat("\n--- who follows whom ---\n")
pm <- list(map = list(a1 = "A", a2 = "A", b1 = "B", b2 = "B", b3 = "B", c1 = "C", ctl = c("A", "B", "C")), levels = c("A", "B", "C"))
ld <- env$panel_style_leaders(c("ctl", "a1", "a2", "b1", "b2", "b3", "c1"), pm)
check("k-th sample of each panel follows the k-th of panel 1; a 3rd sample with no 3rd in panel 1 follows the first panel that has one (itself)",
      identical(unname(ld$leader[c("a1", "a2", "b1", "b2", "b3", "c1")]), c("a1", "a2", "a1", "a2", "b3", "a1")) &&
        identical(unname(ld$panel[c("a1", "b2", "c1")]), c(1L, 2L, 3L)))
check("a sample shown in every panel (the control) keeps its own style", !"ctl" %in% names(ld$leader))
check("one curve per panel (each sample its own panel, a control in every panel): nobody follows, each keeps its colour",
      length(env$panel_style_leaders(c("ctl", "p1", "p2", "p3"), list(map = list(ctl = c("A", "B", "C"), p1 = "A", p2 = "B", p3 = "C"), levels = c("A", "B", "C")))$leader) == 0)
check("one panel only: nobody follows", length(env$panel_style_leaders(c("x", "y"), list(map = list(x = "A", y = "A"), levels = "A"))$leader) == 0)

cat("\n--- the lysis curve, two panels of the same five strains (-/+ a drug) ---\n")
t <- seq(0, 180, 15); set.seed(11); strains <- c("EV", "S1", "S2", "S3", "S4"); nm <- c(strains, paste(strains, "+ drug"))
cols <- list(Time = t)
for (s in seq_along(nm)) for (r in 1:3) cols[[length(cols) + 1]] <- round(0.05 * exp(0.02 * t) * (1 - 0.05 * s) * exp(rnorm(length(t), 0, 0.03)), 4)
d <- as.data.frame(cols); names(d) <- c("Time", rep(nm, each = 3))
r <- new.env()
aes_of <- function(p) { b <- ggplot2::ggplot_build(p)
  pt <- b$data[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), NA))[1]]]
  ln <- b$data[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), NA))[1]]]
  list(col = sort(unique(toupper(pt$colour))), shape = sort(unique(pt$shape)), lt = unique(ln$linetype)) }
testServer(app, {
  S <- session$setInputs
  ingest_data(d, "d.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars, export_format = "svg")
  S(facet_by_group = TRUE, facet_source = "pick", facet_n_panels = 2,
    panelpick_1_name = "- drug", panelpick_1_samples = strains, panelpick_2_name = "+ drug", panelpick_2_samples = paste(strains, "+ drug"))
  r$def_panels <- lapply(main_group_plots(min_panels = 2L), aes_of)   # the default: matched, line type by panel
  S(panel_style_match = "own")
  own <- resolve_aesthetics(rv$od_vars)
  r$own_panels <- lapply(main_group_plots(min_panels = 2L), aes_of)
  r$own_differ <- !identical(own$colors[["EV"]], own$colors[["EV + drug"]])
  for (m in c("line", "fill", "same")) {
    S(panel_style_match = m)
    a <- resolve_aesthetics_drawn(rv$od_vars); pl <- main_group_plots(min_panels = 2L)
    r[[m]] <- list(a = a, p = lapply(pl, aes_of), txt = figure_legend_string(), df = legend_samples_df(),
                   own_after = resolve_aesthetics(rv$od_vars), saved = collect_sample_aesthetics(rv$od_vars))
  }
  S(selected_samples = c("EV", "S1", "EV + drug")); r$sub <- resolve_aesthetics_drawn(c("EV", "S1", "EV + drug"))
  S(selected_samples = rv$od_vars, panel_style_match = "line", facet_by_group = FALSE)
  r$off <- resolve_aesthetics_drawn(rv$od_vars); r$own2 <- resolve_aesthetics(rv$od_vars)
  r$doc <- settings_json_list()$global_settings$panel_style_match
})
check("the default (v2.28.2): both panels have the same colours, panel 2 dashed", identical(r$def_panels[[1]]$col, r$def_panels[[2]]$col) && identical(r$def_panels[[2]]$lt, "dashed"))
check("each sample its own style: the two panels use different colours", isTRUE(r$own_differ) && !identical(r$own_panels[[1]]$col, r$own_panels[[2]]$col))
for (m in c("line", "fill", "same")) {
  a <- r[[m]]$a
  check(paste0(m, ": every '+ drug' strain has the colour and marker of the same strain in panel 1"),
        all(vapply(strains, function(s) identical(a$colors[[s]], a$colors[[paste(s, "+ drug")]]) && identical(a$shapes[[s]], a$shapes[[paste(s, "+ drug")]]), NA)))
  check(paste0(m, ": both panels are drawn with the same five colours", if (m != "fill") " and markers" else " (markers open in panel 2)"),
        identical(r[[m]]$p[[1]]$col, r[[m]]$p[[2]]$col) && length(r[[m]]$p[[1]]$col) == 5 &&
          (identical(m, "fill") || identical(r[[m]]$p[[1]]$shape, r[[m]]$p[[2]]$shape)))
  check(paste0(m, ": each sample's own style is untouched (sidebar, saved settings)"),
        !identical(r[[m]]$own_after$colors[["EV"]], r[[m]]$own_after$colors[["EV + drug"]]) &&
          !identical(r[[m]]$saved[["EV + drug"]]$color, r[[m]]$saved[["EV"]]$color))
}
check("line: panel 1 solid, panel 2 dashed (only the line changes)",
      identical(r$line$p[[1]]$lt, "solid") && identical(r$line$p[[2]]$lt, "dashed") && all(!r$line$a$filled_map[c("EV + drug")] == FALSE))
check("fill: panel 1 filled, panel 2 open, lines as they were",
      isTRUE(r$fill$a$filled_map[["EV"]]) && identical(r$fill$a$filled_map[["EV + drug"]], FALSE) && identical(r$fill$p[[2]]$lt, "solid"))
check("same: the panels are drawn alike (lines too)", identical(r$same$p[[1]]$lt, r$same$p[[2]]$lt) && identical(r$same$a$line_types[["EV + drug"]], "solid"))
check("the legend text describes the curves as drawn ('EV + drug' black with a dashed line)",
      grepl("EV + drug (black, filled circles, dashed line)", r$line$txt, fixed = TRUE))
check("the samples-as-drawn table follows (same hex for EV and EV + drug)",
      { df <- r$line$df; hx <- df[[grep("hex|Hex", names(df))[1]]]; nmc <- df[[1]]; identical(hx[nmc == "EV"], hx[nmc == "EV + drug"]) })
check("a selection of some samples: still matched to panel 1", identical(r$sub$colors[["EV + drug"]], r$sub$colors[["EV"]]))
check("panels off: the choice does nothing", identical(r$off, r$own2))
check("Save Settings keeps the choice", identical(r$doc, "line"))

cat("\n", pass, "passed,", fail, "failed\n")
if (fail > 0) quit(status = 1)
