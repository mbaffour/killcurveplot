# ============================================================================
# v2.23.3: legend text + legend key in the Quick edit strip under the figure (Copy text button).
# Run from the repository root:  Rscript tests/test_v223_legend_drag.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v223_legend_drag.R")
pass <- 0L; fail <- 0L
check  <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.23.3 or later", package_version(env$APP_VERSION) >= "2.23.3")
html_of <- function(x) { rt <- htmltools::renderTags(x); gsub("> <", "><", gsub("[ \t\r\n]+", " ", paste(rt$head, rt$html))) }

cat("\n--- Part A: the Legend text tab ---\n")
full <- html_of(env$ui)
tab_start <- regexpr("data-value=\"Legend text\"", full, fixed = TRUE)
check("a main tab 'Legend text' exists, right after 'Lysis curve'",
      tab_start > 0 && regexpr("data-value=\"Lysis curve\"", full, fixed = TRUE) < tab_start &&
      !grepl("data-value=\"[^\"]+\"", substr(full, regexpr("data-value=\"Lysis curve\"", full, fixed = TRUE) + 20, tab_start - 1)))
tab <- substr(full, regexpr("class=\"kc-legtab\"", full, fixed = TRUE), regexpr("class=\"kc-legtab\"", full, fixed = TRUE) + 4000)
for (id in c("figure_legend_text", "legend_key_preview", "downloadLegendKey", "legend_samples_table"))
  check(sprintf("the tab holds %s", id), grepl(sprintf("id=\"%s\"", id), tab, fixed = TRUE))
check("the tab has Copy text and Copy key buttons and the four headings",
      grepl("kcCopyLegendText()", tab, fixed = TRUE) && grepl("kcCopyFigure(&#39;legend_key&#39;)", tab, fixed = TRUE) &&
      all(vapply(c("Figure legend draft", "Samples as drawn", "Legend key", "Experiment Notebook"), function(h) grepl(h, tab, fixed = TRUE), logical(1))))
count <- function(pat) lengths(regmatches(full, gregexpr(pat, full, fixed = TRUE)))
check("each output id appears exactly once in the whole page",
      count("id=\"figure_legend_text\"") == 1L && count("id=\"legend_key_preview\"") == 1L && count("id=\"downloadLegendKey\"") == 1L)
check("the sidebar keeps a one-line pointer to the tab", grepl("Legend text and key: the Legend text tab", full, fixed = TRUE))
check("the page script defines kcCopyLegendText (clipboard.writeText, falls back to selecting, reports kind legend_text)",
      grepl("window.kcCopyLegendText = function", full, fixed = TRUE) && grepl("clipboard.writeText", full, fixed = TRUE) &&
      grepl("kind: 'legend_text'", full, fixed = TRUE) && grepl("removeAllRanges", full, fixed = TRUE))

testServer(app, {
  ingest_data(env$make_demo_data("lysis"), "Demo")
  do.call(session$setInputs, env$APP_DEFAULTS); session$setInputs(selected_samples = rv$od_vars)
  d <- legend_samples_df()
  check("the samples table has one row per selected sample", !is.null(d) && nrow(d) == length(sel_samples()) && nrow(d) >= 3L)
  av <- resolve_aesthetics(sel_samples())
  check("each row carries the figure's own hex and its colour word", identical(d$hex, toupper(unname(av$colors[sel_samples()]))) &&
        identical(d$colour, env$colour_words(d$hex)))
  check("each row has a marker and a line description", all(nzchar(d$marker)) && all(nzchar(d$line)))
  html <- as.character(output$legend_samples_table$html)
  check("the rendered table has a swatch in that hex per row", all(vapply(d$hex, function(h) grepl(paste0("background:", h), html, fixed = TRUE), logical(1))) &&
        lengths(regmatches(html, gregexpr("kc-leg-sw", html, fixed = TRUE))) == nrow(d))
})

cat("\n--- Part B: drag the legend ---\n")
d <- env$APP_DEFAULTS
check("APP_DEFAULTS ends with legend_inside_x, legend_inside_y (0.98 each)",
      identical(tail(names(d), 2), c("legend_inside_x", "legend_inside_y")) && d$legend_inside_x == 0.98 && d$legend_inside_y == 0.98)
check("both are numeric boxes in the UI with 0..1 ranges",
      all(vapply(c("legend_inside_x", "legend_inside_y"), function(i) { r <- env$NUMERIC_INPUT_RANGES[[i]]; !is.null(r) && r[["min"]] == 0 && r[["max"]] == 1 }, logical(1))))
check("legend_position accepts inside-custom (LEGEND_POSITIONS_OK and the select's choices)",
      "inside-custom" %in% env$LEGEND_POSITIONS_OK && "inside-custom" %in% env$SELECT_INPUT_CHOICES$legend_position)
th <- env$inside_legend_theme("custom", 0.2, 0.3)
check("inside_legend_theme('custom') positions and justifies at (x, y); corners are unchanged",
      identical(th$legend.position, "inside") && identical(th$legend.position.inside, c(0.2, 0.3)) && identical(th$legend.justification.inside, c(0.2, 0.3)) &&
      identical(env$inside_legend_theme("top-right")$legend.position.inside, c(0.98, 0.98)))
src <- paste(readLines("Lysis_Curve_App_v28.R", warn = FALSE), collapse = "\n")
check("the page script has the drag code", grepl("kc-leg-drag", src, fixed = TRUE) && grepl("legend_inside_x", src, fixed = TRUE) && grepl("kcLegDrag", src, fixed = TRUE))
check("the panel finder falls back to the axis lines when the figure has no panel clip rect (the journal default)", grepl("the panel is read from the", src, fixed = TRUE) && grepl("querySelectorAll('polyline, line')", src, fixed = TRUE))
testServer(app, {
  ingest_data(env$make_demo_data("lysis"), "Demo")
  do.call(session$setInputs, env$APP_DEFAULTS); session$setInputs(selected_samples = rv$od_vars, export_format = "svg")
  session$setInputs(legend_position = "inside-custom", legend_inside_x = 0.2, legend_inside_y = 0.3)
  p <- generate_plot()
  check("the built plot has legend.position 'inside' at c(0.2, 0.3)",
        identical(p$theme$legend.position, "inside") && identical(as.numeric(p$theme$legend.position.inside), c(0.2, 0.3)))
  svg_of <- function(pl) { f <- tempfile(fileext = ".svg"); ggsave(f, pl, device = svglite::svglite, width = 3.5, height = 2.6, units = "in", bg = "white"); on.exit(unlink(f)); readLines(f, warn = FALSE) }
  a <- svg_of(p); b <- svg_of(generate_plot())
  check("preview and export use the same theme path (identical SVG)", identical(a, b) && length(a) > 10)
  for (bad in list(1e308, "abc", -1, 5)) {
    ok <- tryCatch({ session$setInputs(legend_inside_x = bad, legend_inside_y = bad); q <- generate_plot(); v <- q$theme$legend.position.inside
                     is.numeric(v) && all(is.finite(v)) && all(v >= 0 & v <= 1) }, error = function(e) FALSE)
    check(sprintf("legend_inside_x/y = %s is clamped into 0..1 and nothing errors", format(bad)), isTRUE(ok))
  }
  session$setInputs(legend_position = "inside-top-right")
  check("a corner position still gives the compact corner legend", identical(as.numeric(generate_plot()$theme$legend.position.inside), c(0.98, 0.98)))
})

got <- character(0); old <- env$showNotification
env$showNotification <- function(ui, ..., type = "default") { got <<- c(got, paste0("[", type, "] ", as.character(ui))); invisible(NULL) }
testServer(app, { session$setInputs(kc_copy_result = list(kind = "legend_text", ok = TRUE)) })
check("a legend_text result gives 'Legend text copied.'", identical(got, "[message] Legend text copied."))
got <- character(0)
testServer(app, { session$setInputs(kc_copy_result = list(kind = "legend_text", ok = FALSE, reason = "nope")) })
check("a failed legend_text copy is a warning that says why", length(got) == 1L && grepl("^\\[warning\\] Could not copy the text: nope", got))
env$showNotification <- old

cat(sprintf("\nSUMMARY: %d passed, %d failed\n", pass, fail))
if (fail > 0L) quit(status = 1)
