# ============================================================================
# v2.27.0: the project file. One .kcproj holds the data (as loaded and as edited), the selection, exclusions, the time
# filter, every setting and sample style, a colour map and the Compare file; opening it restores all of that.
# Run from the repository root:  Rscript tests/test_v227_project.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v227_project.R")
Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_paper_style_here.json"))
pass <- 0L; fail <- 0L
check  <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("APP_VERSION is 2.27.0 or later", package_version(env$APP_VERSION) >= "2.27.0")
full <- { rt <- htmltools::renderTags(env$ui); gsub("[ \t\r\n]+", " ", paste(rt$head, rt$html)) }
check("the UI: Save project, Open project, and the file picker takes .kcproj with a Save link",
      all(vapply(c('id="saveProject"', 'id="loadProject"', 'id="saveProjectTop"', ".kcproj", "A project file (.kcproj) opens here too"), grepl, NA, x = full, fixed = TRUE)))

cat("\n--- tables in a project file ---\n")
d <- data.frame(Time = c(0, 15, 30), WT = c(0.05, NA, 0.2), WT = c(0.051, 0.1, Inf), Note = c("a", NA, "c"), Flag = c(TRUE, FALSE, NA),
                check.names = FALSE, stringsAsFactors = FALSE)
f <- tempfile(fileext = ".json"); write_json(env$frame_to_doc(d), f, auto_unbox = TRUE, digits = NA, na = "null", null = "null")
back <- env$doc_to_frame(fromJSON(f, simplifyVector = FALSE))
d2 <- d; d2[[3]][3] <- NA
check("a table survives: repeated column names, order, NA, text and logical columns (Inf is a missing reading)",
      identical(colnames(back), colnames(d)) && isTRUE(all.equal(back[[2]], d2[[2]])) && isTRUE(all.equal(back[[3]], d2[[3]])) &&
        identical(back$Note, d$Note) && identical(back$Flag, d$Flag) && nrow(back) == 3)
check("full precision: 0.1234567891234 comes back exactly",
      { x <- data.frame(t = 1:2, v = c(0.1234567891234, 1/3)); g <- tempfile(); write_json(env$frame_to_doc(x), g, auto_unbox = TRUE, digits = NA, na = "null")
        identical(env$doc_to_frame(fromJSON(g, simplifyVector = FALSE))$v, x$v) })
check("is_project_file: .kcproj, or a .json that carries kc_project; a settings file is not one",
      { g <- tempfile(fileext = ".json"); writeLines('{"kc_project":1,"data":{}}', g); h <- tempfile(fileext = ".json"); writeLines('{"version":"2.26","global_settings":{}}', h)
        env$is_project_file("x", "run.kcproj") && env$is_project_file(g, "p.json") && !env$is_project_file(h, "s.json") && !env$is_project_file("x", "data.csv") })

cat("\n--- save in one session, open in another ---\n")
mk <- function(nm, seed = 3) { t <- seq(0, 180, 15); set.seed(seed); cols <- list(Time = t)
  for (i in seq_along(nm)) for (r in 1:3) cols[[length(cols) + 1]] <- round(0.05 * exp(0.02 * t) * (1 - 0.04 * i) * exp(rnorm(length(t), 0, 0.03)), 4)
  x <- as.data.frame(cols); names(x) <- c("Time", rep(nm, each = 3)); x }
dat <- mk(c("Control", "Phage A", "Phage B", "Phage C"))
fB <- file.path(tempdir(), "runB.csv"); write.csv(mk(c("Control", "Phage A"), seed = 9), fB, row.names = FALSE)
l <- readLines(fB); l[1] <- paste(c("Time", rep(c("Control", "Phage A"), each = 3)), collapse = ","); writeLines(l, fB)
proj <- tempfile(fileext = ".kcproj")
A <- new.env()
testServer(app, {
  S <- session$setInputs
  ingest_data(dat, "my_run.csv"); do.call(S, env$APP_DEFAULTS)
  S(selected_samples = c("Control", "Phage A", "Phage C"), export_format = "svg")
  S(x_axis_label = "Time after infection (min)", plot_title = "Project test", error_type = "sd", color_palette = "set1")
  S(qe_name_Phage_A = "*mltG* phage")
  S(line_type_Phage_C = "dashed", shape_Phage_C = "8", color_selector_Phage_C = "custom", color_Phage_C = "#123456")
  apply_cell_edit(3, 2, "0.9")                                  # a Data-tab edit: row 3, first Control column
  S(excluded_reps = replicate_catalog_current()$key[2], exclude_timepoints = "165")
  S(enable_time_filter = TRUE, time_filter_range = c(15, 150))
  compare_load(fB, "runB.csv"); S(compare_samples_b = "Control")
  rv$colour_map <- c("Phage B" = "#AA0000"); rv$colour_map_source <- "map.csv"
  S(facet_by_group = TRUE, facet_source = "each", facet_legends = "each", panelcfg_Control_title = "Uninfected panel")
  session$flushReact()
  A$legend <- figure_legend_string(); A$data <- rv$data; A$loaded <- rv$data_loaded; A$tc <- rv$time_col; A$excl <- excluded_rep_keys()
  A$pd <- prepare_plot_data(); A$aes <- resolve_aesthetics(sel_samples())
  write_project_file(proj)
})
doc <- fromJSON(proj, simplifyVector = FALSE)
check("the project file: kc_project, the app version, data as loaded AND as edited, selection, settings, compare, colour map",
      identical(doc$kc_project, 1L) && identical(doc$app_version, env$APP_VERSION) && is.list(doc$data$loaded) && is.list(doc$data$current) &&
        identical(unlist(doc$selection$selected_samples), c("Control", "Phage A", "Phage C")) && is.list(doc$settings$global_settings) &&
        is.list(doc$compare$frame) && identical(doc$colour_map$map[["Phage B"]], "#AA0000"))
check("the project file: the time filter, excluded replicate and excluded time point are in it",
      identical(as.numeric(unlist(doc$selection$time_filter_range)), c(15, 150)) && identical(unlist(doc$selection$excluded_reps), A$excl) &&
        identical(doc$selection$exclude_timepoints, "165"))

calls <- list()
for (fn in c("updateTextInput", "updateNumericInput", "updateSelectInput", "updateSelectizeInput", "updateCheckboxInput", "updateRadioButtons", "updateSliderInput", "updateTextAreaInput", "updateCheckboxGroupInput")) local({
  orig <- get(fn, envir = asNamespace("shiny")); nm <- fn
  assign(fn, function(session, inputId, ...) { calls[[length(calls) + 1L]] <<- list(id = inputId, args = list(...)); orig(session, inputId, ...) }, envir = env)
})
B <- new.env()
testServer(app, {
  S <- session$setInputs
  ingest_data(mk(c("Other", "Sample")), "something_else.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars)
  # controls a browser always has (testServer only knows the inputs that were set): the loader applies known controls
  S(enable_time_filter = FALSE, facet_by_group = FALSE, facet_source = "group", facet_legends = "shared", panelcfg_Control_title = "")
  calls <<- list()
  ok <- load_project(proj, basename(proj)); session$flushReact()
  B$ok <- ok; B$data <- rv$data; B$loaded <- rv$data_loaded; B$tc <- rv$time_col; B$src <- rv$data_loaded_source
  B$cmp <- rv_cmp$ds$od_vars; B$cmap <- rv$colour_map; B$status <- rv$project_status
  B$imp <- rv$imported_settings; B$pending_excl <- rv$pending_excl; B$pending_tr <- rv$pending_trange; B$panel_cfg <- rv$panel_cfg
  # play the browser: every update the server sent becomes the input's value, and the lists drawn from the project
  # (sidebar rows, quick-edit names, exclusions, the slider) take the values they are drawn with
  for (c0 in calls) { v <- c0$args$value %||% c0$args$selected; if (!is.null(v)) do.call(S, setNames(list(v), c0$id)) }
  invisible(output$excluded_reps_ui); invisible(output$time_range_slider_ui)
  for (vn in rv$od_vars) { im <- rv$imported_settings$samples[[vn]]; vid <- env$safe_id(vn)
    a <- list(im$legend_label %||% vn, im$line_type %||% "solid", as.character(im$shape %||% 16), "custom", im$color %||% "#000000", isTRUE(im$shape_filled %||% TRUE))
    names(a) <- paste0(c("qe_name_", "line_type_", "shape_", "color_selector_", "color_", "shape_filled_"), vid); do.call(S, a) }
  S(excluded_reps = B$pending_excl %||% A$excl, time_filter_range = c(15, 150))
  session$flushReact()
  B$legend <- figure_legend_string(); B$pd <- prepare_plot_data(); B$aes <- resolve_aesthetics(sel_samples()); B$excl <- excluded_rep_keys()
  B$inp <- list(sel = input$selected_samples, etp = input$exclude_timepoints, tf = input$time_filter_range, etf = input$enable_time_filter)
  B$xlab <- input$x_axis_label; B$title <- input$plot_title; B$pal <- input$color_palette
})
check("open: the project loads (one call, no data upload)", isTRUE(B$ok) && grepl("opened", B$status))
check("open: the data as edited and as loaded, and the time column, are exactly as saved", identical(B$data, A$data) && identical(B$loaded, A$loaded) && identical(B$tc, A$tc))
check("open: the source name is the original file's, not the project's", identical(B$src, "my_run.csv"))
check("open: the Data-tab edit is there (0.9) and Revert still has the loaded value", B$data[[2]][3] == 0.9 && B$loaded[[2]][3] != 0.9)
check("open: the Compare file and its chosen samples", identical(B$cmp, c("Control", "Phage A")) &&
        any(vapply(calls, function(c0) identical(c0$id, "compare_samples_b") && identical(c0$args$selected, "Control"), NA)))
check("open: the colour map", identical(B$cmap, c("Phage B" = "#AA0000")))
check("open: global settings come back (axis label, title, palette)", identical(B$xlab, "Time after infection (min)") && identical(B$title, "Project test") && identical(B$pal, "set1"))
check("open: sample styles and names come back (Phage C dashed X #123456, Phage A renamed)",
      identical(B$imp$samples[["Phage C"]]$line_type, "dashed") && identical(toupper(B$imp$samples[["Phage C"]]$color), "#123456") &&
        identical(B$imp$samples[["Phage A"]]$legend_label, "*mltG* phage"))
check("open: the per-panel settings", identical(B$panel_cfg[["Control"]]$title, "Uninfected panel"))
check("open: the selected samples, the excluded replicate, the excluded time point and the time filter",
      any(vapply(calls, function(c0) identical(c0$id, "selected_samples") && identical(c0$args$selected, c("Control", "Phage A", "Phage C")), NA)) &&
        identical(B$excl, A$excl) && any(vapply(calls, function(c0) identical(c0$id, "exclude_timepoints") && identical(c0$args$value, "165"), NA)))
if (!isTRUE(all.equal(B$pd, A$pd))) { print(all.equal(B$pd, A$pd)); str(B$inp) }
if (!identical(B$legend, A$legend)) { cat("A:", A$legend, "\nB:", B$legend, "\n") }
check("open: the figure's data and the legend text are the same as when saved", isTRUE(all.equal(B$pd, A$pd)) && identical(B$legend, A$legend))
check("open: colours, markers, line types and names as drawn are the same", identical(B$aes$colors, A$aes$colors) && identical(B$aes$shapes, A$aes$shapes) &&
        identical(B$aes$line_types, A$aes$line_types) && identical(B$aes$leg_labels, A$aes$leg_labels))

cat("\n--- bad files ---\n")
testServer(app, {
  ingest_data(mk(c("Keep")), "keep.csv"); before <- rv$data
  bad <- tempfile(fileext = ".kcproj"); writeLines("not json", bad)
  set1 <- tempfile(fileext = ".kcproj"); writeLines('{"version":"2.26.1","global_settings":{}}', set1)
  nodata <- tempfile(fileext = ".kcproj"); writeLines('{"kc_project":1,"data":{}}', nodata)
  r <- c(load_project(bad, "bad.kcproj"), load_project(set1, "settings.kcproj"), load_project(nodata, "nodata.kcproj"))
  B$bad <- !any(r) && identical(rv$data, before)
  # the main file picker opens a project
  session$setInputs(file = list(name = "run.kcproj", datapath = proj, size = file.size(proj), type = "")); session$flushReact()
  B$picker <- identical(rv$data, A$data) && identical(rv$data_loaded_source, "my_run.csv")
})
check("a broken file, a settings file renamed .kcproj, a project with no data: refused, the data untouched", isTRUE(B$bad))
check("the main file picker opens a project file (data, edits and source name restored)", isTRUE(B$picker))

cat("\n--- odd files and real-world layouts ---\n")
shapes <- c('{"kc_project":1,"data":"x"}', '{"kc_project":1,"data":{"loaded":{"names":["Time","A"],"types":["num","num"],"columns":[[0,1],[0.1]]}}}',
            '[1,2,3]', '{"kc_project":1,"data":{"loaded":{"names":["Time","A","A"],"types":["num","num","num"],"columns":[[0,15,30],[0.1,0.2,0.3],[0.1,0.21,0.31]]}},"selection":"x","settings":[1],"colour_map":{"map":{"A":"not a colour","B":["#FF0000"]}}}')
B2 <- new.env()
testServer(app, {
  ingest_data(mk(c("Keep")), "keep.csv"); before <- rv$data
  r <- vapply(shapes, function(js) { f <- tempfile(fileext = ".kcproj"); writeLines(js, f); isTRUE(tryCatch(load_project(f, "odd.kcproj"), error = function(e) "ERR")) }, NA)
  B2$r <- r; B2$after <- rv$data; B2$cmap <- rv$colour_map; B2$before <- before
})
check("odd shapes (data not a table, ragged columns, an array, selection / settings / colour map of the wrong shape): no error",
      identical(unname(B2$r), c(FALSE, FALSE, FALSE, TRUE)))
check("odd shapes: the last one loads its data; a colour map entry that is not a colour is dropped", ncol(B2$after) == 3 && is.null(B2$cmap))

ing <- function(fx) file.path("tests", "fixtures", "ingest", fx)
for (fx in c("U01_user_file.csv", "H03_blocks_repeated_headers.csv", "L07_time_well_od.csv", "F13_datetime_dmy_dots.csv")) {
  if (!file.exists(ing(fx))) next
  R <- new.env()
  testServer(app, {
    session$setInputs(file = list(name = fx, datapath = ing(fx), size = file.size(ing(fx)), type = "")); session$flushReact()
    do.call(session$setInputs, env$APP_DEFAULTS); session$setInputs(selected_samples = rv$od_vars)
    R$pd <- prepare_plot_data(); R$vars <- rv$od_vars; R$long <- rv$is_long_format; R$blocks <- rv$n_blocks
    pf <- tempfile(fileext = ".kcproj"); write_project_file(pf); R$pf <- pf
  })
  Q <- new.env()
  testServer(app, {
    session$setInputs(file = list(name = "p.kcproj", datapath = R$pf, size = file.size(R$pf), type = "")); session$flushReact()
    do.call(session$setInputs, env$APP_DEFAULTS); session$setInputs(selected_samples = rv$od_vars)
    Q$pd <- prepare_plot_data(); Q$vars <- rv$od_vars; Q$long <- rv$is_long_format; Q$blocks <- rv$n_blocks
  })
  check(sprintf("%s through a project: same samples, layout (long / blocks) and plotted means", fx),
        identical(Q$vars, R$vars) && identical(Q$long, R$long) && identical(Q$blocks, R$blocks) && isTRUE(all.equal(Q$pd, R$pd)))
}

cat("\n--- a hidden sample keeps its style; a loaded panel source does not come back later ---\n")
H <- new.env()
testServer(app, {
  S <- session$setInputs
  ingest_data(mk(c("Control", "Phage A", "Phage B")), "h.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars)
  S(line_type_Phage_B = "dotted", color_selector_Phage_B = "custom", color_Phage_B = "#654321", qe_name_Phage_B = "Hidden one")
  S(selected_samples = c("Control", "Phage A"))
  pf <- tempfile(fileext = ".kcproj"); write_project_file(pf)
  d <- fromJSON(pf, simplifyVector = FALSE)
  H$hidden <- d$settings$sample_aesthetics[["Phage B"]]
  # the panel source: a settings file with the source already in use, then a hand-picked one survives a menu refresh
  S(facet_by_group = TRUE, facet_source = "each")
  sf <- tempfile(fileext = ".json"); write_json(list(version = "2.27.0", styling_schema = 222L, global_settings = list(facet_source = "each")), sf, auto_unbox = TRUE)
  load_settings_file(sf); session$flushReact()
  S(facet_source = "group"); session$flushReact()
  ingest_data(mk(c("Control", "Phage A", "Phage B"), seed = 4), "h2.csv", keep_state = TRUE); session$flushReact()
  H$pending <- facet_pending$v
})
check("a hidden sample's style and name are saved in the project", identical(H$hidden$line_type, "dotted") && identical(toupper(H$hidden$color), "#654321") && identical(H$hidden$legend_label, "Hidden one"))
check("a loaded panel source that was applied is not kept to come back over a later hand-picked one", is.null(H$pending))

cat("\n", pass, "passed,", fail, "failed\n")
if (fail > 0) quit(status = 1)
