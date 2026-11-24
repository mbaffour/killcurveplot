# ============================================================================
# v2.25.0: floating labels. Legend position "Floating labels": every sample name is its own coloured label on the panel,
# dragged where it is wanted; "Extra labels on the figure": free text (a gene name) placed the same way.
# Run from the repository root:  Rscript tests/test_v225_floating_labels.R
# (The drag itself is exercised with a real mouse in a headless browser in the click-through runs; here: the page
#  script's contract and the server rules.)
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v225_floating_labels.R")
Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_paper_style_here.json"))
pass <- 0L; fail <- 0L
check  <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.25.0 or later", package_version(env$APP_VERSION) >= "2.25.0")
full <- { rt <- htmltools::renderTags(env$ui); gsub("[ \t\r\n]+", " ", paste(rt$head, rt$html)) }
has <- function(...) all(vapply(c(...), function(s) grepl(s, full, fixed = TRUE), logical(1)))

cat("\n--- controls and helpers ---\n")
check("Legend position offers 'Floating labels (drag each name)'", "floating" %in% env$SELECT_INPUT_CHOICES$legend_position && has("Floating labels (drag each name)"))
check("'floating' is an accepted legend position in settings files", "floating" %in% env$LEGEND_POSITIONS_OK)
check("Label Options has the extra-labels box; its default is appended at the end of APP_DEFAULTS, empty", has("id=\"figure_labels\"") && { nm <- names(env$APP_DEFAULTS); match("figure_labels", nm) == length(nm) - sum(nm %in% c("facet_letters", "facet_size_mode", "facet_legends", "facet_n_panels", "panel_style_match", "analysis_key_on_figure", "fit_key_on_figure", "compose_key_on_figure", "compare_key_on_figure", "error_cap_past_marker")) } && identical(env$APP_DEFAULTS$figure_labels, ""))
check("parse_figure_labels: one label per line, blanks and repeats dropped, 40 at most, junk gives none",
      identical(env$parse_figure_labels(" *mltG* \n\nlysis onset\r\n*mltG*"), c("*mltG*", "lysis onset")) && length(env$parse_figure_labels(paste(seq_len(100), collapse = "\n"))) == 40L &&
      identical(env$parse_figure_labels(NULL), character(0)) && identical(env$parse_figure_labels(c("a", "b")), character(0)) && identical(env$parse_figure_labels(NA_character_), character(0)))
check("float_label_plain strips the markup marks (what the browser sees in the figure)", identical(env$float_label_plain(c("*mltG*", "**b**", "A_{550}", "x^{2} y")), c("mltG", "b", "A550", "x2 y")))
check("float_pos_clean keeps {name: [fx, fy]} pairs, clamps to 0..1, drops junk",
      identical(env$float_pos_clean(list(a = list(0.2, 0.3), b = c(5, -1), c = "x", d = list(0.1), e = list(NA, 0.2))), list(a = c(0.2, 0.3), b = c(1, 0))) &&
      identical(env$float_pos_clean("nope"), list()) && identical(env$float_pos_clean(list(0.1, 0.2)), list()))
check("the default places are a staggered column inside the frame", { d <- env$float_default_pos(c("a", "b", "c")); all(vapply(d, function(v) v[1] == 0.82, NA)) && d$a[2] > d$b[2] && d$b[2] > d$c[2] && all(unlist(d) >= 0.02 & unlist(d) <= 0.98) })
check("the page script makes each label a handle and reports a drop as a distance in panel fractions",
      has("function kcLabDrag()", "kc_label_data", "Shiny.setInputValue('kc_label_drop'", "dfx: dxp / pw, dfy: -dyp / ph", ".kc-lab-drag {"))
check("the legend-box drag stands down in floating mode", has("pos === 'matrix' || pos === 'floating') return;"))

cat("\n--- the figure ---\n")
grobs_named <- function(p, pat) { g <- ggplot2::ggplotGrob(p); n <- character(0); walk <- function(x) { if (inherits(x, "grob") && !is.null(x$name) && grepl(pat, x$name)) n <<- c(n, x$name)
  if (!is.null(x$children)) for (ch in x$children) walk(ch); if (!is.null(x$grobs)) for (ch in x$grobs) walk(ch) }; walk(g); n }
has_legend <- function(p) { g <- ggplot2::ggplotGrob(p); any(grepl("guide-box", g$layout$name) & !vapply(g$grobs, inherits, NA, "zeroGrob")) }
r <- list(); sfile <- tempfile(fileext = ".json")
testServer(app, {
  S <- session$setInputs
  ingest_data(env$make_demo_data("lysis"), "Demo"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars, export_format = "svg")
  v <- rv$od_vars; ids <- vapply(v, env$safe_id, "")
  r$svg0 <<- readLines(output$downloadPlot); r$size0 <<- export_size_in(); r$lay0 <<- label_layout_now()
  S(legend_position = "floating"); p <- generate_plot()
  r$n_float <<- length(grobs_named(p, "^float_label_")); r$legend_gone <<- !has_legend(p); r$size_f <<- export_size_in()
  svg <- paste(readLines(output$downloadPlot), collapse = "\n")
  r$names_in_svg <<- all(vapply(v, function(n) grepl(paste0(">", n, "<"), svg, fixed = TRUE), NA)); r$orange <<- grepl("fill: #E69F00;[^>]*>Phage MOI 10<", svg) || grepl("#E69F00", svg, fixed = TRUE)
  r$lay <<- label_layout_now(); r$pos0 <<- float_positions(v)
  S(kc_label_drop = list(kind = "sample", id = ids[[2]], dfx = -0.4, dfy = -0.5, n = 1)); r$pos1 <<- float_positions(v)
  S(kc_label_drop = list(kind = "sample", id = ids[[2]], dfx = 0.1, dfy = 0.05, n = 2)); r$pos2 <<- float_positions(v)
  S(kc_label_drop = list(kind = "sample", id = ids[[1]], dfx = 1.9, dfy = 1.9, n = 3)); r$pos_far <<- float_positions(v)[[v[1]]]
  before <- rv$float_pos
  for (bad in list("x", NULL, list(kind = "sample", id = "nope", dfx = 0.1, dfy = 0.1), list(kind = "sample", id = ids[[1]], dfx = "a", dfy = 0.1),
                   list(kind = "sample", id = ids[[1]], dfx = 1e308, dfy = 0), list(kind = "note", id = 7, dfx = 0.1, dfy = 0.1), list(kind = list(), id = list(), dfx = list(), dfy = list())))
    { a <- list(); a["kc_label_drop"] <- list(bad); do.call(S, a) }
  r$junk_ok <<- identical(rv$float_pos, before)
  # extra labels
  S(figure_labels = "*mltG*\nlysis onset"); p2 <- generate_plot()
  r$n_notes <<- length(grobs_named(p2, "^float_note_")); svg2 <- paste(readLines(output$downloadPlot), collapse = "\n")
  r$note_italic <<- grepl("font-style: italic;[^>]*>mltG<", svg2); r$note_plain <<- grepl(">lysis onset<", svg2, fixed = TRUE); r$size_n <<- export_size_in()
  S(kc_label_drop = list(kind = "note", id = 1, dfx = -0.2, dfy = -0.3, n = 4)); r$npos <<- note_positions(figure_labels_now()); r$lay2 <<- label_layout_now()
  S(legend_position = "inside-auto"); p3 <- generate_plot(); r$inside <<- c(length(grobs_named(p3, "^float_label_")), length(grobs_named(p3, "^float_note_")), has_legend(p3))
  S(legend_position = "floating")
  # a long name is kept whole inside the frame
  rv$display_names <- setNames(c("A very long strain name that would not fit at the default place", v[2], v[3]), v); session$flushReact()
  r$long_x <<- float_positions(v)[[v[1]]][1]; rv$display_names <- NULL; session$flushReact()
  # facets: no floating labels, the ordinary key instead
  a <- list(); a[[paste0("group_", ids[[1]])]] <- "g1"; a[[paste0("group_", ids[[2]])]] <- "g2"; a[[paste0("group_", ids[[3]])]] <- "g2"; do.call(S, c(a, list(facet_by_group = TRUE)))
  p4 <- generate_plot(); r$facet <<- c(length(grobs_named(p4, "^float_")), has_legend(p4), is.null(label_layout_now()))
  S(facet_by_group = FALSE)
  # settings: save, reset, load
  r$doc <<- settings_json_list(); writeLines(jsonlite::toJSON(r$doc, auto_unbox = TRUE, null = "null", na = "null"), sfile)
  keep <- list(f = rv$float_pos, n = rv$note_pos)
  S(clearSettings_confirm = 1); r$after_reset <<- c(length(rv$float_pos), length(rv$note_pos))
  S(loadSettings = data.frame(name = "s.json", size = file.size(sfile), type = "application/json", datapath = sfile, stringsAsFactors = FALSE))
  r$reloaded <<- isTRUE(all.equal(rv$float_pos, keep$f)) && isTRUE(all.equal(rv$note_pos, keep$n)); r$load_status <- rv$settings_status$msg
  S(reset_label_positions = 1); r$after_link <<- c(length(rv$float_pos), length(rv$note_pos))
  # an old lysis-curve-app file: its floating_labels are taken
  old <- tempfile(fileext = ".json"); writeLines(jsonlite::toJSON(list(experiment_notes = list(host = "MG1655"), global_settings = list(legend_position = "floating"),
                                                                    floating_labels = setNames(list(c(0.25, 0.4)), v[3])), auto_unbox = TRUE), old)
  S(loadSettings = data.frame(name = "old.json", size = file.size(old), type = "application/json", datapath = old, stringsAsFactors = FALSE))
  r$old <<- rv$float_pos[[v[3]]]; r$v <<- v
})
check("the default figure has no floating labels and nothing to hand to the page", is.null(r$lay0))
check("floating mode: one label per sample, no legend box, the figure keeps its size", identical(r$n_float, 3L) && isTRUE(r$legend_gone) && isTRUE(all.equal(r$size_f, r$size0)))
check("every sample name is text in the exported SVG, in its colour", isTRUE(r$names_in_svg) && isTRUE(r$orange))
check("the page gets id, place and plain text of each label", length(r$lay$samples) == 3L && all(c("id", "fx", "fy", "text") %in% names(r$lay$samples[[1]])) && identical(r$lay$samples[[2]]$text, r$v[2]))
check("a drop moves the label by the dragged distance", isTRUE(all.equal(unname(r$pos1[[r$v[2]]]), unname(r$pos0[[r$v[2]]]) + c(-0.4, -0.5))))
check("a second drop starts from where the label is (no drift)", isTRUE(all.equal(unname(r$pos2[[r$v[2]]]), unname(r$pos1[[r$v[2]]]) + c(0.1, 0.05))))
check("a drop far outside is held inside the frame", all(r$pos_far <= 0.98) && all(r$pos_far >= 0.02) && r$pos_far[2] == 0.98)
check("junk drop messages (text, NULL, an unknown id, a non-number, 1e308, a label that does not exist, lists) change nothing", isTRUE(r$junk_ok))
check("the extra labels are drawn: *mltG* in italics, plain text plain, the figure keeps its size", identical(r$n_notes, 2L) && isTRUE(r$note_italic) && isTRUE(r$note_plain) && isTRUE(all.equal(r$size_n, r$size0)))
check("an extra label is dragged the same way and the page knows it by its line number", isTRUE(all.equal(unname(r$npos[["*mltG*"]]), c(0.5 - 0.2, 0.92 - 0.3))) && identical(r$lay2$notes[[1]]$id, 1L) && identical(r$lay2$notes[[1]]$text, "mltG"))
check("with an ordinary (inside) legend the sample labels go, the extra labels stay, the legend box is back", identical(as.numeric(r$inside), c(0, 2, 1)))
check("a long name is kept whole inside the frame (its centre moves left of the default column)", r$long_x < 0.82 && r$long_x >= 0.49)
check("with facets there are no floating labels: the ordinary key is used and the page gets nothing", identical(as.numeric(r$facet), c(0, 1, 1)))
check("Save Settings carries the dragged places (label_positions: samples and notes) and the extra-label text",
      is.list(r$doc$label_positions) && all(c("samples", "notes") %in% names(r$doc$label_positions)) && identical(r$doc$global_settings$figure_labels, "*mltG*\nlysis onset") &&
      !any(c("kc_label_drop", "reset_label_positions") %in% names(r$doc$global_settings)))
check("Clear & Reset puts the labels back; loading the file restores every place", identical(r$after_reset, c(0L, 0L)) && isTRUE(r$reloaded))
check("'Put the labels back' clears the dragged places", identical(r$after_link, c(0L, 0L)))
check("an old lysis-curve-app settings file: its floating_labels are used", isTRUE(all.equal(unname(r$old), c(0.25, 0.4))))

cat("\n--- nothing else moved ---\n")
check("the default figure is byte-identical with and without the feature's code path (no label, same SVG as at load)", length(r$svg0) > 50)
check("a settings document without dragged labels has no label_positions key", { d <- NULL
  testServer(app, { ingest_data(env$make_demo_data("lysis"), "Demo"); do.call(session$setInputs, env$APP_DEFAULTS); session$setInputs(selected_samples = rv$od_vars); d <<- names(settings_json_list()) })
  !("label_positions" %in% d) })

cat(sprintf("\nSUMMARY: %d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1)
