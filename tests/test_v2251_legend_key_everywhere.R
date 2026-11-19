# ============================================================================
# v2.25.1: the legend key is in every archive and has its "Place the key on the figure" switch; a settings file saved
# after a visit to the Statistics tab loads back cleanly; many labels start in columns, not in a pile.
# Run from the repository root:  Rscript tests/test_v2251_legend_key_everywhere.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v2251_legend_key_everywhere.R")
Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_paper_style_here.json"))
pass <- 0L; fail <- 0L
check  <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.25.1 or later", package_version(env$APP_VERSION) >= "2.25.1")
full <- { rt <- htmltools::renderTags(env$ui); gsub("[ \t\r\n]+", " ", paste(rt$head, rt$html)) }
has <- function(...) all(vapply(c(...), function(s) grepl(s, full, fixed = TRUE), logical(1)))
src <- paste(readLines("Lysis_Curve_App_v28.R", warn = FALSE), collapse = "\n")
zip_names <- function(zf) if (requireNamespace("zip", quietly = TRUE)) basename(zip::zip_list(zf)$filename) else
  basename(sub("^\\s*[0-9]+\\s+[0-9-]+\\s+[0-9:]+\\s+", "", system2("unzip", c("-l", zf), stdout = TRUE)))

cat("\n--- the legend key in every archive ---\n")
check("the batch list starts with the main figure and its legend key ticked", identical(env$APP_DEFAULTS$batch_outputs, c("main", "legend_key")) &&
        grepl("value=\"legend_key\" checked", full, fixed = TRUE))
r <- list(); notes <- character(0); sent <- list()
env$showNotification <- function(ui, ..., type = "default") { notes <<- c(notes, paste0("[", type, "] ", as.character(ui))); invisible(NULL) }
env$updateSelectInput <- function(session, inputId, ...) { a <- list(...); if (identical(inputId, "legend_position")) sent$pos <<- c(sent$pos, a$selected); invisible(NULL) }
env$updateCheckboxInput <- function(session, inputId, ...) { a <- list(...); if (identical(inputId, "legend_key_on_figure")) sent$chk <<- c(sent$chk, a$value); invisible(NULL) }
sfile <- tempfile(fileext = ".json")
testServer(app, {
  S <- session$setInputs
  ingest_data(env$make_demo_data("lysis"), "Demo"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars, export_format = "svg", legend_key_on_figure = TRUE)
  r$svg0 <<- readLines(output$downloadPlot)
  r$all  <<- zip_names(output$downloadAllFigures)
  S(batch_format = "svg"); r$batch <<- zip_names(output$downloadBatchExport)
  # the matrix (grid) legend: the key is the grid itself
  ids <- vapply(rv$od_vars, env$safe_id, ""); m <- list(legend_position = "matrix", matrix_legend_title = "MOI")
  rws <- c("host", "phage", "phage"); cls <- c("low", "low", "high")
  for (i in seq_along(ids)) { m[[paste0("legrow_", ids[i])]] <- rws[i]; m[[paste0("legcol_", ids[i])]] <- cls[i] }
  do.call(S, m); pm <- generate_plot(); km <- legend_key_for(pm)
  r$mat_is_grid <<- !is.null(attr(pm, "matrix_legend")) && identical(km$layout$name[km$layout$name != "matrix-legend-box"], attr(pm, "matrix_legend")$grob$layout$name[attr(pm, "matrix_legend")$grob$layout$name != "matrix-legend-box"])
  r$mat_labels <<- env$legend_key_labels(km); r$mat_w <<- attr(km, "width_in"); r$mat_h <<- attr(km, "height_in")
  S(legend_position = "inside-auto"); r$key_after_matrix <<- env$legend_key_labels(legend_key_for(generate_plot()))
  # floating labels: no legend box on the figure, the key is still cut (and is the same key)
  r$key_right <<- env$legend_key_labels(legend_key_for(generate_plot()))
  S(legend_position = "floating"); r$key_float <<- env$legend_key_labels(legend_key_for(generate_plot())); r$all_float <<- zip_names(output$downloadAllFigures)
  r$chk_after_floating <<- utils::tail(sent$chk, 1)
  # the switch: on from a names-only position -> the last legend box position; off -> none
  S(legend_key_on_figure = FALSE); S(legend_key_on_figure = TRUE); r$pos_on <<- utils::tail(sent$pos, 1)
  S(legend_position = "right", legend_key_on_figure = TRUE); n0 <- length(sent$pos)
  S(legend_key_on_figure = FALSE); r$pos_off <<- utils::tail(sent$pos, 1); r$n_sent <<- length(sent$pos) - n0
  S(legend_position = "none"); r$chk_none <<- utils::tail(sent$chk, 1)
  S(legend_key_on_figure = TRUE); r$pos_back <<- utils::tail(sent$pos, 1)
  # the settings round trip after the Statistics tab was open (its table reports rows, sort and search as inputs)
  S(legend_position = "inside-auto", legend_key_on_figure = TRUE,
    stats_tab_summary_rows_selected = NULL, stats_tab_summary_rows_current = 1:25, stats_tab_summary_rows_all = 1:39,
    stats_tab_summary_state = list(time = 1, start = 0L, length = 25L, order = list(), search = list(search = "", smart = TRUE)),
    stats_tab_summary_search = "", stats_tab_summary_cell_clicked = list(), data_view_table_rows_all = 1:13)
  r$doc <<- settings_json_list(); writeLines(jsonlite::toJSON(r$doc, auto_unbox = TRUE, null = "null", na = "null"), sfile)
  notes <<- character(0)
  S(loadSettings = data.frame(name = "s.json", size = file.size(sfile), type = "application/json", datapath = sfile, stringsAsFactors = FALSE))
  r$load_status <<- paste(rv$settings_status$msg, rv$settings_status$details %||% "")
  # a file written before this version still carries the table state: skipped silently, not "wrong type"
  old <- r$doc; old$global_settings$stats_tab_summary_rows_all <- as.list(1:39); old$global_settings$stats_tab_summary_state <- list(time = 1, order = list())
  old$global_settings$stats_tab_summary_cells_selected <- list(); old$global_settings$legend_key_on_figure <- TRUE
  writeLines(jsonlite::toJSON(old, auto_unbox = TRUE, null = "null", na = "null"), sfile); notes <<- character(0)
  S(loadSettings = data.frame(name = "old.json", size = file.size(sfile), type = "application/json", datapath = sfile, stringsAsFactors = FALSE))
  r$old_status <<- paste(rv$settings_status$msg, rv$settings_status$details %||% ""); r$old_notes <<- notes
})
check("Download all figures: the archive has legend_key.svg and legend_key.png", all(c("legend_key.svg", "legend_key.png") %in% r$all))
check("the batch export with its default ticks has the legend key beside the main figure",
      "legend_key_main.svg" %in% r$batch && any(grepl("^lysis_curve_main.*\\.svg$", r$batch)))
check("with floating labels (no legend box on the figure) the key is still cut, with the same entries",
      length(r$key_float) >= 3 && identical(r$key_float, r$key_right) && all(c("legend_key.svg", "legend_key.png") %in% r$all_float))

check("a matrix (grid) legend: the key is the grid as drawn (corner title, column and row headers), not the plain key",
      isTRUE(r$mat_is_grid) && all(c("MOI", "low", "high", "host", "phage") %in% r$mat_labels) && r$mat_w > 0 && r$mat_h > 0)
check("with the matrix legend off again the key is the ordinary one", identical(r$key_after_matrix, r$key_right))

cat("\n--- one switch beside the key ---\n")
check("the Legend text tab has 'Place the key on the figure', ticked at the start (the default legend is on the figure)",
      has("id=\"legend_key_on_figure\"", "Place the key on the figure") && grepl("id=\"legend_key_on_figure\"[^>]*checked", full))
check("a names-only legend position (floating labels) unticks the switch", identical(r$chk_after_floating, FALSE))
check("ticking it from there puts back the last legend box position", identical(r$pos_on, "inside-auto"))
check("unticking it hides the legend (position 'None'), once", identical(r$pos_off, "none") && identical(r$n_sent, 1L))
check("choosing 'None' in the menu unticks the switch; ticking it again returns to where the legend was", identical(r$chk_none, FALSE) && identical(r$pos_back, "right"))
check("the switch is not a setting of its own (the legend position is)", !("legend_key_on_figure" %in% names(r$doc$global_settings)) && !("legend_key_on_figure" %in% names(env$APP_DEFAULTS)))

cat("\n--- settings: a table's own state is not a setting ---\n")
check("is_table_state_key knows both tables and nothing else",
      identical(env$is_table_state_key(c("stats_tab_summary_state", "data_view_table_rows_all", "legend_position", "stats_method")), c(TRUE, TRUE, FALSE, FALSE)))
check("nothing a table reports about itself is written into a settings file", !any(env$is_table_state_key(names(r$doc$global_settings))))
check("a file saved after the Statistics tab was open loads back with no 'wrong type' warning",
      !grepl("wrong type", r$load_status, fixed = TRUE) && !any(grepl("wrong type", notes, fixed = TRUE)))
check("an older file that carries the table state loads without the warning too",
      !grepl("wrong type", r$old_status, fixed = TRUE) && !any(grepl("wrong type", r$old_notes, fixed = TRUE)))

cat("\n--- many labels start in columns ---\n")
d9  <- env$float_default_pos(paste0("s", 1:9)); d36 <- env$float_default_pos(paste0("s", 1:36))
n40 <- env$float_default_pos(paste0("n", 1:40), x = 0.5, top = 0.92, step = 0.08)
uniq <- function(d) length(unique(vapply(d, function(v) paste(round(v, 4), collapse = ","), "")))
inside <- function(d) all(vapply(d, function(v) v[1] >= 0.05 && v[1] <= 0.95 && v[2] >= 0.1 && v[2] <= 0.95, NA))
check("up to nine sample names: one column at the right, as before", all(vapply(d9, function(v) v[1] == 0.82, NA)) && isTRUE(all.equal(d9$s9[2], 0.12)))
check("36 sample names: 36 different places, all inside the frame", uniq(d36) == 36L && inside(d36))
check("40 extra labels (the most allowed): 40 different places, all inside the frame", uniq(n40) == 40L && inside(n40))
check("the first extra-label column is where it was (centre, top down)", isTRUE(all.equal(n40$n1, c(0.5, 0.92))) && isTRUE(all.equal(n40$n2, c(0.5, 0.84))))

cat("\n--- facets: the labels say why they are gone ---\n")
fn <- list()
testServer(app, {
  S <- session$setInputs
  ingest_data(env$make_demo_data("lysis"), "Demo"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars, legend_position = "floating", figure_labels = "*mltG*")
  txt <- function(o) { v <- tryCatch(o, error = function(e) NULL); if (is.null(v)) "" else paste(as.character(v$html %||% v), collapse = " ") }
  fn$one_a <<- txt(output$float_facet_note); fn$one_b <<- txt(output$labels_facet_note)
  ids <- vapply(rv$od_vars, env$safe_id, ""); a <- list(facet_source = "manual", facet_by_group = TRUE); pn <- c("host", "phage", "phage")
  for (i in seq_along(ids)) a[[paste0("facet_of_", ids[i])]] <- pn[i]
  do.call(S, a); fn$fac_a <<- txt(output$float_facet_note); fn$fac_b <<- txt(output$labels_facet_note); fn$layout <<- is.null(label_layout_now())
  S(figure_labels = "", legend_position = "right"); fn$off_a <<- txt(output$float_facet_note); fn$off_b <<- txt(output$labels_facet_note)
})
check("one panel: no note under the floating-labels menu or the extra-labels box", !nzchar(fn$one_a) && !nzchar(fn$one_b))
check("one panel per group with floating labels: the note under the menu says the ordinary legend is shown",
      grepl("One panel per group is on", fn$fac_a, fixed = TRUE) && grepl("ordinary legend", fn$fac_a, fixed = TRUE) && isTRUE(fn$layout))
check("one panel per group with extra labels typed: the note under the box says they are not drawn", grepl("extra labels are not drawn", fn$fac_b, fixed = TRUE))
check("no labels in play: no note, facets or not", !nzchar(fn$off_a) && !nzchar(fn$off_b))

cat("\n--- nothing else moved ---\n")
check("RGB sliders are sent plain numbers (no named value in the message to the page)", !grepl("tryCatch(col2rgb(", src, fixed = TRUE) && grepl("tryCatch(unname(col2rgb(sel))", src, fixed = TRUE))
check("the default figure still exports", length(r$svg0) > 50)

cat(sprintf("\nSUMMARY: %d passed, %d failed\n", pass, fail))
if (fail > 0) quit(status = 1)
