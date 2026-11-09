# ============================================================================
# v2.23.2 Copy figure: a "Copy figure" button on every figure (page script kcCopyFigure draws the preview to a PNG and
# writes it to the clipboard), Cmd/Ctrl+C over a preview, and the server notification that reports the result.
# Run from the repository root:  Rscript tests/test_v223_copy_figure.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v223_copy_figure.R")

pass <- 0L; fail <- 0L
check  <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.23.2 or later", package_version(env$APP_VERSION) >= "2.23.2")

with_notes <- function(fn) {
  got <- character(0); old <- env$showNotification
  env$showNotification <- function(ui, ..., type = "default") { got <<- c(got, paste0("[", type, "] ", as.character(ui))); invisible(NULL) }
  on.exit(env$showNotification <- old)
  val <- fn(); list(value = val, notes = got)
}

cat("\n--- UI: a Copy button for every figure, and the page script ---\n")
rt <- htmltools::renderTags(env$ui)
ui_html <- gsub("> <", "><", gsub("[ \t\r\n]+", " ", paste(rt$head, rt$html)))
for (k in c("main", "analysis", "fit", "compose", "compare", "legend_key"))
  check(sprintf("a button calls kcCopyFigure('%s')", k), grepl(sprintf("onclick=\"kcCopyFigure(&#39;%s&#39;)\"", k), ui_html, fixed = TRUE))
check("the buttons are plain btn btn-default buttons with the copy icon",
      grepl("<button class=\"btn btn-default\" type=\"button\"[^>]*onclick=\"kcCopyFigure\\(&#39;main&#39;\\)\"> ?<i class=\"far fa-copy", ui_html) &&
      grepl("Copy key", ui_html, fixed = TRUE) && grepl("Copy figure", ui_html, fixed = TRUE))
check("the page script defines kcCopyFigure and writes a PNG with clipboard.write",
      grepl("window.kcCopyFigure = function(kind)", ui_html, fixed = TRUE) && grepl("clipboard.write", ui_html, fixed = TRUE) &&
      grepl("image/png", ui_html, fixed = TRUE) && grepl("kc_copy_result", ui_html, fixed = TRUE))
check("the script finds the inline SVG and the preview images of every figure",
      all(vapply(c("#od_plot_svg svg", "#analysis_img img", "#fit_img img", "#compose_img img", "#compare_img img", "#legend_key_preview img"),
                 function(s) grepl(s, ui_html, fixed = TRUE), logical(1))))
check("Cmd/Ctrl+C while hovering a preview is wired (keydown, metaKey / ctrlKey, no text selected)",
      grepl("ev.metaKey || ev.ctrlKey", ui_html, fixed = TRUE) && grepl("getSelection", ui_html, fixed = TRUE) && grepl("mouseover", ui_html, fixed = TRUE))
check("the result input is not a saved setting", sum(grepl("kc_copy_result", readLines("Lysis_Curve_App_v28.R", warn = FALSE))) >= 5L &&
      length(grep("\"sidebar_jump\", \"kc_copy_result\"", readLines("Lysis_Curve_App_v28.R", warn = FALSE), fixed = TRUE)) == 2L)

cat("\n--- server: the result notification ---\n")
errs <- character(0)
r <- with_notes(function() testServer(app, {
  session$onUnhandledError(function(e) errs <<- c(errs, conditionMessage(e)))
  session$setInputs(kc_copy_result = list(kind = "main", ok = TRUE, w = 1200, h = 900))
}))
check("ok:TRUE gives one 'copied' message naming the size", length(r$notes) == 1L &&
      grepl("^\\[message\\] Figure copied to the clipboard \\(1200 x 900 px\\)\\. For print use Download Image\\.$", r$notes))
r <- with_notes(function() testServer(app, {
  session$onUnhandledError(function(e) errs <<- c(errs, conditionMessage(e)))
  session$setInputs(kc_copy_result = list(kind = "fit", ok = FALSE, reason = "x"))
}))
check("ok:FALSE gives one 'Could not copy' warning with the reason", length(r$notes) == 1L &&
      identical(r$notes, "[warning] Could not copy: x. Use Download Image instead."))
r <- with_notes(function() testServer(app, {
  session$onUnhandledError(function(e) errs <<- c(errs, conditionMessage(e)))
  session$setInputs(kc_copy_result = list(kind = "main", ok = FALSE))
}))
check("a failure without a reason still gives one warning", length(r$notes) == 1L && grepl("^\\[warning\\] Could not copy: unknown reason", r$notes))
r <- with_notes(function() testServer(app, {
  session$onUnhandledError(function(e) errs <<- c(errs, conditionMessage(e)))
  session$setInputs(kc_copy_result = list(kind = "main", ok = TRUE, w = "abc", h = list()))
}))
check("non-numeric sizes are shown as ? instead of failing", length(r$notes) == 1L && grepl("\\(\\? x \\? px\\)", r$notes))
r <- with_notes(function() testServer(app, {
  session$onUnhandledError(function(e) errs <<- c(errs, conditionMessage(e)))
  session$setInputs(kc_copy_result = 5)
  session$setInputs(kc_copy_result = "ok")
  session$setInputs(kc_copy_result = list(1, 2))
  session$setInputs(kc_copy_result = list(ok = c(TRUE, FALSE)))
  session$setInputs(kc_copy_result = list(ok = "yes"))
  session$setInputs(kc_copy_result = NULL)
}))
check("a scalar or malformed message is ignored: no notification, no unhandled error", length(r$notes) == 0L && length(errs) == 0L)

cat("\n--- the main preview container ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "")
  pc <- as.character(output$plot_container$html)
  check("the od_plot_svg container carries the Copy figure title",
        grepl("od_plot_svg", pc, fixed = TRUE) &&
        grepl("title=\"Use Copy figure (or Cmd/Ctrl+C while hovering) to copy; Download Image for a file.\"", pc, fixed = TRUE))
})

cat(sprintf("\nSUMMARY: %d passed, %d failed\n", pass, fail))
if (fail > 0L) quit(status = 1)
