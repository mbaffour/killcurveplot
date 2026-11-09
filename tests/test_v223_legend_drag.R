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
        identical(d$colour, env$colour_word(d$hex)))
  check("each row has a marker and a line description", all(nzchar(d$marker)) && all(nzchar(d$line)))
  html <- as.character(output$legend_samples_table$html)
  check("the rendered table has a swatch in that hex per row", all(vapply(d$hex, function(h) grepl(paste0("background:", h), html, fixed = TRUE), logical(1))) &&
        lengths(regmatches(html, gregexpr("kc-leg-sw", html, fixed = TRUE))) == nrow(d))
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
