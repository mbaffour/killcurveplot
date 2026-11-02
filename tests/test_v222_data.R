# ============================================================================
# v2.22 data tab and ingest: detection fixes (wide: every numeric column is a sample; long: Strain), the time-column
# override, the Data tab (view / edit / revert / download, pasted text, Excel), exclusions that really filter (OD range,
# replicates), blank sample / baseline and transforms, the replicate QC table, the n = 1 warning, Samples All / None,
# the extinction floor and t_stat inputs, and the settings round trip.
# Run from the repository root:  Rscript tests/test_v222_data.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root")

pass <- 0L; fail <- 0L; skip <- 0L
check <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
skipit <- function(desc) { skip <<- skip + 1L; cat("SKIP:", desc, "\n") }
near <- function(a, b, tol = 1e-9) length(a) == length(b) && all(abs(a - b) < tol)

env <- new.env()
app <- suppressWarnings(suppressMessages(source("Lysis_Curve_App_v28.R", local = env)$value))
check("app sources cleanly", inherits(app, "shiny.appobj"))

cat("\n--- defaults, UI ids, tab order ---\n")
nm <- names(env$APP_DEFAULTS)
new_keys <- c("use_od_filter", "od_min", "od_max", "blank_mode", "blank_sample", "blank_k", "transform_mode",
              "transform_control", "extinction_threshold", "t_stat_override")
# (later releases append their own defaults after these, so the check is: a contiguous run, in this order, at the end of the v2.22 block)
check("the new defaults are a contiguous block at the END of APP_DEFAULTS (later additions follow them)",
      { pos <- match(new_keys, nm); !anyNA(pos) && all(diff(pos) == 1) })
check("defaults reproduce the old behaviour (nothing excluded, constant blank, no transform)",
      isFALSE(env$APP_DEFAULTS$use_od_filter) && env$APP_DEFAULTS$blank_mode == "constant" &&
      env$APP_DEFAULTS$transform_mode == "none" && env$APP_DEFAULTS$extinction_threshold == 0.05 &&
      is.na(env$APP_DEFAULTS$t_stat_override))
ui_html <- as.character(env$ui)
ids <- c("time_col_choice", "sheet_ui", "blank_mode", "blank_od", "blank_sample", "blank_k", "transform_mode", "transform_control",
         "use_od_filter", "od_min", "od_max", "excluded_reps_ui", "excluded_reps_clear", "samples_all", "samples_none",
         "data_view_table", "data_edit_reset", "download_current_data_csv", "data_entry_text", "data_entry_sep",
         "data_entry_apply", "data_entry_clear", "extinction_threshold", "t_stat_override", "storms_window_readout",
         "rep_qc_samples", "rep_qc_replicates", "downloadRepQCCSV", "rep_qc_flag")
check("UI has every new control", all(vapply(ids, function(id) grepl(paste0('id="', id, '"'), ui_html, fixed = TRUE), logical(1))))
tabs <- regmatches(ui_html, gregexpr('data-value="[^"]+"', ui_html))[[1]]
tabs <- sub('data-value="(.*)"', "\\1", tabs)
check("main tab 'Data' comes right after 'Lysis curve'", match("Data", tabs) == match("Lysis curve", tabs) + 1L)
check("Data sub-tabs: View & edit and Enter data", all(c("View &amp; edit", "Enter data") %in% tabs) || all(c("View & edit", "Enter data") %in% tabs))
check("the file picker accepts Excel", grepl(".xlsx", ui_html, fixed = TRUE))
check("Statistics tab has the Replicate QC section", grepl("Replicate QC", ui_html, fixed = TRUE))
check("APP_VERSION untouched by this change", is.character(env$APP_VERSION))

cat("\n--- detection: wide format keeps every numeric column ---\n")
w <- data.frame(Time = c(0, 10, 20), Mod = c(.1, .2, .3), WT = c(.1, .25, .4))
ws <- env$wide_sample_columns(w, "Time")
check("Time, Mod, WT -> two samples (position based)", identical(ws$used, c("Mod", "WT")) && identical(ws$idx, 2:3))
w2 <- data.frame(Time = 1:3, Mod = 1:3 / 10, `T° 600` = 37, Temp = 37, `37 °C` = 37, Notes = c("a", "b", "c"), WT = 3:1 / 10, check.names = FALSE)
ws2 <- env$wide_sample_columns(w2, "Time")
check("temperature columns (T°, temp, °C) are dropped", identical(ws2$used, c("Mod", "WT")) &&
      all(c("T° 600", "Temp", "37 °C") %in% ws2$dropped$name[ws2$dropped$reason == "temperature column"]))
check("a text column is dropped as not numeric", identical(ws2$dropped$reason[ws2$dropped$name == "Notes"], "not numeric"))
w3 <- data.frame(Time = 1:3, OD = 1:3 / 10, Period = 3:1 / 10, Product = c(.2, .2, .3))
check("OD-named and other columns are all samples (no name filter)", length(env$wide_sample_columns(w3, "Time")$idx) == 3)
dd <- data.frame(Time = 1:3, A = 1:3, A = 3:1, B = c(1, 1, 2), check.names = FALSE)
check("duplicate names (replicate columns) are all kept", identical(env$wide_sample_columns(dd, "Time")$idx, 2:4))
check("long-format group names include strain / Strain", all(c("strain", "Strain") %in% env$LONG_GROUP_NAMES))

cat("\n--- pure helpers: pasted text, Excel names, catalog, keys ---\n")
p_tab <- env$parse_pasted_text("Time\tWT\tMut\n0\t0.05\t0.05\n15\t0.08\t0.07\n")
check("pasted tab text -> 2 rows x 3 columns, numeric", is.data.frame(p_tab) && identical(dim(p_tab), c(2L, 3L)) && is.numeric(p_tab$WT))
p_com <- env$parse_pasted_text("Time,WT,Mut\n0,0.05,0.05\n15,0.08,0.07")
check("pasted comma text parses", identical(dim(p_com), c(2L, 3L)) && near(p_com$Mut, c(.05, .07)))
p_sem <- env$parse_pasted_text("Time;WT\n0;0,05\n15;0,08", "auto")
check("pasted semicolon text with decimal commas parses", identical(dim(p_sem), c(2L, 2L)) && near(p_sem$WT, c(.05, .08)))
p_tr <- env$parse_pasted_text("Time\tWT\t\n0\t0.05\t\n15\t0.08\t\n\n\n", "tab")
check("empty trailing column and rows are dropped", identical(dim(p_tr), c(2L, 2L)))
check("blank text -> NULL", is.null(env$parse_pasted_text("   \n")) && is.null(env$parse_pasted_text("")))
check("is_excel_file", env$is_excel_file("a.XLSX") && env$is_excel_file("b.xls") && !env$is_excel_file("c.csv"))
cat_w <- env$replicate_catalog(data.frame(Time = 0:2), FALSE, "Time", col_map = list(A = 2:4, B = 5L), n_blocks = 2)
check("catalog, wide + 2 blocks: A has 6 replicates, B has 2; keys 'A #k'", sum(cat_w$sample == "A") == 6 && sum(cat_w$sample == "B") == 2 && "A #6" %in% cat_w$key)
lg <- data.frame(Time = rep(c(0, 10), each = 4), Strain = rep(c("X", "X", "Y", "Y"), 2), Well = rep(c("A1", "A2", "B1", "B2"), 2), OD = c(.1, .12, .2, .21, .3, .31, .4, .42))
cat_l <- env$replicate_catalog(lg, TRUE, "Time", "Strain", "Well")
check("catalog, long with a replicate column: 2 replicates per strain, labelled with the well", nrow(cat_l) == 4 && any(grepl("A2", cat_l$label)))
cat_l2 <- env$replicate_catalog(lg[c("Time", "Strain", "OD")], TRUE, "Time", "Strain", NULL)
check("catalog, long without a replicate column: nth row at each time", nrow(cat_l2) == 4)
pk <- env$parse_replicate_keys(c("WT #2", "My #1 sample #10", "junk"))
check("replicate keys parse from the right ('#' inside a name is fine)", identical(pk$sample, c("WT", "My #1 sample")) && identical(pk$id, c(2L, 10L)))

cat("\n--- transform_frame (pure) ---\n")
rf <- data.frame(time = rep(c(0, 10, 20), 3 * 2), variable = rep(c("blank", "S"), each = 9), replicate = rep(rep(1:3, each = 3), 2),
                 value = c(rep(c(.05, .06, .05), 3), rep(c(.1, .3, .5), 3) + rep(c(0, .01, .02), each = 3)))
tf <- env$transform_frame(rf, "sample", "blank")
check("blank sample: its rows are gone, S lowered by the blank's mean curve", !"blank" %in% tf$variable && nrow(tf) == 9 &&
      near(tf$value[tf$replicate == 1], c(.1 - .05, .3 - .06, .5 - .05)))
tk <- env$transform_frame(rf[rf$variable == "S", ], "first_k", blank_k = 2)
check("first-k baseline: each replicate's own first 2 readings are subtracted", near(tk$value[tk$replicate == 1][1:2], c(-.1, .1)))
t0 <- env$transform_frame(rf[rf$variable == "S", ], transform = "t0")
check("t0 normalisation: every replicate starts at 1", near(t0$value[t0$time == 0], rep(1, 3)))
tc <- env$transform_frame(rf, transform = "control", control = "blank")
check("control normalisation: the control becomes 1 and S = S / control", near(tc$value[tc$variable == "blank"], rep(1, 9)) &&
      near(tc$value[tc$variable == "S" & tc$replicate == 1], c(.1 / .05, .3 / .06, .5 / .05)))
tp <- env$transform_frame(rf, transform = "pct_max")
mx <- function(v) max(tapply(v$value, v$time, mean))
check("0-100 %: the maximum of each sample's mean curve is 100", near(mx(tp[tp$variable == "S", ]), 100) && near(mx(tp[tp$variable == "blank", ]), 100))
tl <- env$transform_frame(rf[rf$variable == "S", ], transform = "ln")
check("ln: values are the natural log", near(tl$value, log(rf$value[rf$variable == "S"])))
tn <- env$transform_frame(data.frame(time = 0:2, variable = "S", replicate = 1, value = c(0, .1, .2)), transform = "ln")
check("ln of 0 is dropped to NA with a note", is.na(tn$value[1]) && length(attr(tn, "curation_notes")) == 1)
tx <- env$transform_frame(rf, "sample", "nope")
check("a blank sample with no readings: no change, with a note", near(tx$value, rf$value) && length(attr(tx, "curation_notes")) == 1)
check("curation text names every active step", {
  s <- env$curation_description(list(od = c(0, 2), n_excluded = 2, excluded_text = "A: rep 1; B: rep 2", blank_mode = "sample",
                                      blank_sample = "media", transform = "ln"))
  grepl("outside OD 0 to 2", s) && grepl("2 replicates excluded", s) && grepl("'media'", s) && grepl("ln\\(OD\\)", s) })
check("curation text is empty when nothing is active", identical(env$curation_description(list()), ""))

cat("\n--- replicate_cv_table (pure) ---\n")
qd <- do.call(rbind, lapply(1:3, function(r) data.frame(time = c(0, 10, 20), variable = "S", replicate = r, value = c(1, 2, 3) * c(1, 1.1, 2)[r])))
qt <- env$replicate_cv_table(qd)
check("QC table shape: samples + replicates data frames with the documented columns",
      identical(names(qt$samples), c("sample", "n_replicates", "n_timepoints", "median_cv_pct", "max_cv_pct", "time_of_max_cv", "n_over_flag", "high_cv")) &&
      identical(names(qt$replicates), c("sample", "replicate", "n_points", "distance_from_median", "distance_pct", "most_distant", "excluded")) &&
      nrow(qt$samples) == 1 && nrow(qt$replicates) == 3)
cv_hand <- sd(c(1, 1.1, 2)) / mean(c(1, 1.1, 2)) * 100
check("CV % per time point: median = max here (CV is the same at every time) and equals the hand value", near(qt$samples$median_cv_pct, cv_hand, 1e-6) && near(qt$samples$max_cv_pct, cv_hand, 1e-6))
check("high-CV flag above 20 %", qt$samples$high_cv && !env$replicate_cv_table(qd, flag_pct = 90)$samples$high_cv)
check("the replicate furthest from the median curve is marked most_distant", qt$replicates$replicate[qt$replicates$most_distant] == 3 && sum(qt$replicates$most_distant) == 1)
check("distance from the median curve: replicate 2 (the median) has distance 0", near(qt$replicates$distance_from_median[qt$replicates$replicate == 2], 0))
check("empty input -> empty tables with the same columns", nrow(env$replicate_cv_table(NULL)$samples) == 0 && nrow(env$replicate_cv_table(qd[0, ])$replicates) == 0)
one <- env$replicate_cv_table(qd[qd$replicate == 1, ])
check("a single replicate: CV is NA, never flagged", is.na(one$samples$median_cv_pct) && !one$samples$high_cv)
csvf <- env$replicate_qc_csv_frame(qt)
check("QC csv frame: one row per replicate with the sample-level columns joined", nrow(csvf) == 3 && "median_cv_pct" %in% names(csvf))

cat("\n--- server: detection + time column override ---\n")
testServer(app, {
  summary_html <- function() { session$flushReact(); as.character(output$data_summary_ui$html) }
  do.call(session$setInputs, env$APP_DEFAULTS)
  ingest_data(data.frame(Time = c(0, 10, 20), Mod = c(.1, .2, .3), WT = c(.1, .25, .4), `T° 600` = 37, check.names = FALSE), "mod.csv")
  check("ingest: Time, Mod, WT, T° -> samples Mod and WT", identical(rv$od_vars, c("Mod", "WT")) && !rv$is_long_format)
  check("the dropped column is recorded with its reason", identical(rv$dropped_cols$name, "T° 600"))
  html <- summary_html()
  check("data summary lists the dropped column", grepl("Not used as samples: T° 600 (temperature column)", html, fixed = TRUE) && grepl("Columns used as samples: Mod, WT", html, fixed = TRUE))
  ingest_data(data.frame(Time = rep(c(0, 10, 20), each = 2), Strain = rep(c("A", "B"), 3), OD = c(.1, .2, .15, .3, .2, .5)), "strain.csv")
  check("Time, Strain, OD is long format with Strain as the sample column", rv$is_long_format && identical(rv$group_col, "Strain") && identical(rv$value_col, "OD") && identical(rv$od_vars, c("A", "B")))
  ingest_data(data.frame(Time = rep(c(0, 10, 20), each = 2), strain = rep(c("A", "B"), 3), OD = c(.1, .2, .15, .3, .2, .5), Rep = rep(1, 6)), "strain2.csv")
  check("lower-case 'strain' is recognised too", rv$is_long_format && identical(rv$group_col, "strain"))
  ingest_data(data.frame(Time = 1:3, Mod = 1:3 / 10, WT = 3:1 / 10, Extra = 1:3, Extra2 = c(5, 6, 7), Extra3 = c(1, 1, 2)), "six.csv")
  check("a six-column wide file with arbitrary names keeps all numeric samples", length(rv$od_vars) == 5)

  # time column override
  fl <- data.frame(Minutes = c(0, 5, 10, 15), Cycle = c(1, 2, 3, 4), A = c(.1, .2, .3, .4), B = c(.1, .1, .2, .2))
  ingest_data(fl, "cycles.csv")
  src0 <- rv$data_loaded
  check("auto-detected time column is the first monotone numeric one", identical(rv$time_col, "Minutes") && identical(rv$od_vars, c("Cycle", "A", "B")))
  session$setInputs(time_col_choice = "Minutes")
  check("choosing the already-chosen column changes nothing", identical(rv$time_col, "Minutes"))
  session$setInputs(time_col_choice = "Cycle")
  check("time column override re-runs the detection: Cycle is the time, Minutes a sample", identical(rv$time_col, "Cycle") && identical(rv$od_vars, c("Minutes", "A", "B")))
  check("the time points are the new column's values", identical(rv$all_timepoints, c(1, 2, 3, 4)))
  check("override works on the data in memory: the loaded copy and source name are untouched", identical(rv$data_loaded, src0) && identical(rv$data_source, "cycles.csv"))
  fl2 <- cbind(fl, Label = c("a", "b", "c", "d"))
  ingest_data(fl2, "label.csv")
  session$setInputs(time_col_choice = "Label")
  check("a column without usable numbers is refused; the time column stays", identical(rv$time_col, "Minutes"))
})

cat("\n--- server: Data tab edit / revert / download ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = "Uninfected")
  m0 <- curve_metrics_current(); pd0 <- prepare_plot_data()
  mo0 <- m0$max_od[m0$sample == "Phage MOI 10"]
  col <- which(colnames(rv$data) == "Phage MOI 10")[1]
  rw  <- which.max(rv$data[[col]])
  v_old <- rv$data[[col]][rw]
  session$setInputs(data_view_table_cell_edit = list(row = rw, col = col - 1L, value = "1.7"))
  check("a cell edit lands in the working data", near(rv$data[[col]][rw], 1.7))
  m1 <- curve_metrics_current()
  check("an edit refreshes the metrics (max OD of that sample changed)", m1$max_od[m1$sample == "Phage MOI 10"] != mo0)
  pd1 <- prepare_plot_data()
  check("an edit refreshes the plotted means", !isTRUE(all.equal(pd0$mean_value, pd1$mean_value)))
  check("the edit went through the ingest path: col_map and replicates intact", identical(unname(lengths(rv$col_map)), c(3L, 3L, 3L)) && identical(rv$od_vars, c("Uninfected", "Phage MOI 10", "Phage MOI 0.1")))
  check("sample selection survives an edit", identical(input$selected_samples, rv$od_vars))
  check("the loaded copy is unchanged by edits", !near(rv$data_loaded[[col]][rw], 1.7))
  # an empty cell is a missing reading: n shrinks at that time point
  rt <- rv$data[[rv$time_col]][rw]
  session$setInputs(data_view_table_cell_edit = list(row = rw, col = col - 1L, value = ""))
  check("emptying a cell stores NA", is.na(rv$data[[col]][rw]))
  pdn <- prepare_plot_data()
  check("per-point exclusion: n at that time point is 2 instead of 3", pdn$n[pdn$variable == "Phage MOI 10" & pdn$time == rt] == 2)
  # a non-number in a numeric column
  session$setInputs(data_view_table_cell_edit = list(row = 1, col = col - 1L, value = "abc"))
  check("non-numeric text in a numeric column becomes NA", is.na(rv$data[[col]][1]))
  # the no-DT form uses the same function
  check("apply_cell_edit rejects a cell that does not exist", isFALSE(apply_cell_edit(999, 1, "1")))
  apply_cell_edit(rw, col, as.character(v_old))
  check("apply_cell_edit (the no-DT form) sets a cell", near(rv$data[[col]][rw], v_old))
  # revert
  session$setInputs(data_view_table_cell_edit = list(row = rw, col = col - 1L, value = "2.2"))
  session$setInputs(data_edit_reset = 1)
  check("Revert restores the loaded data", isTRUE(all.equal(rv$data[[col]], rv$data_loaded[[col]])) && !near(rv$data[[col]][rw], 2.2))
  check("Revert restores the metrics", near(curve_metrics_current()$max_od[m0$sample == "Phage MOI 10"], mo0))
  # download
  session$setInputs(data_view_table_cell_edit = list(row = rw, col = col - 1L, value = "1.5"))
  f <- tryCatch(output$download_current_data_csv, error = function(e) NULL)
  dl <- if (is.character(f) && file.exists(f)) utils::read.csv(f, check.names = FALSE) else NULL
  check("Download current data: the CSV holds the edited value", !is.null(dl) && nrow(dl) == nrow(rv$data) && near(dl[[col]][rw], 1.5))
  check("the edited CSV loads back through the file reader", !is.null(dl) && identical(dim(env$read_data_file(f, "x.csv")), dim(rv$data)))
})

cat("\n--- server: pasted data and Excel ---\n")
testServer(app, {
  do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(data_entry_sep = "auto", data_entry_text = "Time\tWT\tWT\tMut\n0\t0.05\t0.06\t0.05\n15\t0.08\t0.09\t0.07\n30\t0.14\t0.15\t0.10\n")
  check("pasted text is parsed live", is.data.frame(entry_parsed()) && ncol(entry_parsed()) == 4)
  session$setInputs(data_entry_apply = 1)
  check("Apply ingests the pasted table: source, samples and replicate columns", identical(rv$data_source, "Pasted data") &&
        identical(rv$od_vars, c("WT", "Mut")) && identical(unname(lengths(rv$col_map)), c(2L, 1L)))
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem")
  check("pasted data flows into the plot data", nrow(prepare_plot_data()) == 6 && near(prepare_plot_data()$mean_value[2], mean(c(.05, .06))))
  session$setInputs(data_entry_text = "Time,A\n0,1")
  session$setInputs(data_entry_apply = 2)
  check("too little data is refused with a message, the loaded data stays", identical(rv$data_source, "Pasted data") && grepl("Nothing to apply", entry_msg()$text))
  session$setInputs(data_entry_text = "Time;A;B\n0;0,1;0,2\n10;0,2;0,3\n", data_entry_sep = "semicolon")
  session$setInputs(data_entry_apply = 3)
  check("a semicolon / decimal-comma paste applies", identical(rv$od_vars, c("A", "B")) && near(rv$data$A, c(.1, .2)))
  session$setInputs(data_entry_clear = 1)
  check("Clear empties the message", is.null(entry_msg()))
})
if (requireNamespace("writexl", quietly = TRUE) && requireNamespace("readxl", quietly = TRUE)) {
  xf <- tempfile(fileext = ".xlsx")
  x1 <- data.frame(Time = c(0, 15, 30), WT = c(.05, .08, .14), Mut = c(.05, .07, .1))
  x2 <- data.frame(Time = c(0, 15, 30), Other = c(.1, .2, .4))
  writexl::write_xlsx(list(Run1 = x1, Run2 = x2), xf)
  check("excel_sheet_names lists the sheets", identical(env$excel_sheet_names(xf), c("Run1", "Run2")))
  rd <- env$read_data_file(xf, "book.xlsx")
  check("read_data_file reads the first sheet of a workbook", identical(names(rd), c("Time", "WT", "Mut")) && near(rd$WT, x1$WT))
  check("read_data_file reads a named sheet", identical(names(env$read_data_file(xf, "book.xlsx", sheet = "Run2")), c("Time", "Other")))
  xd <- tempfile(fileext = ".xlsx")
  dupdf <- data.frame(Time = c(0, 15), a = c(.1, .2), b = c(.1, .3), c = c(.2, .2)); names(dupdf) <- c("Time", "WT", "WT", "Mut")
  writexl::write_xlsx(dupdf, xd)
  rdd <- env$read_data_file(xd, "dup.xlsx")
  check("duplicate headers (replicate columns) survive the Excel read", identical(colnames(rdd), c("Time", "WT", "WT", "Mut")))
  testServer(app, {
    do.call(session$setInputs, env$APP_DEFAULTS)
    session$setInputs(csv_locale = "auto", file = list(name = "book.xlsx", datapath = xf, size = 1, type = ""))
    check("uploading a workbook ingests its first sheet", identical(rv$od_vars, c("WT", "Mut")) && grepl("book.xlsx", rv$data_source, fixed = TRUE))
    check("a multi-sheet workbook offers the sheet list", identical(rv$xlsx_sheets, c("Run1", "Run2")))
    session$setInputs(sheet_choice = "Run2")
    check("picking another sheet loads it", identical(rv$od_vars, "Other") && grepl("Run2", rv$data_source, fixed = TRUE))
  })
} else {
  skipit("xlsx ingest tests (writexl / readxl not installed)")
}

cat("\n--- server: OD-range exclusion ---\n")
rep_data <- data.frame(Time = c(0, 10, 20, 30),
                       WT = c(.10, .20, .40, .80), WT = c(.11, .22, .44, .90), WT = c(.09, .18, .36, 2.9),
                       Mut = c(.10, .12, .14, .16), Mut = c(.10, .13, .15, .15), Mut = c(.10, .11, .13, .17), check.names = FALSE)
testServer(app, {
  summary_html <- function() { session$flushReact(); as.character(output$data_summary_ui$html) }
  do.call(session$setInputs, env$APP_DEFAULTS)
  ingest_data(rep_data, "reps.csv")
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = "Mut", show_replicate_points = FALSE)
  pd0 <- prepare_plot_data(); rp0 <- prepare_replicate_data()
  check("default path: WT at t = 30 is the mean of 3 readings including the 2.9", near(pd0$mean_value[pd0$variable == "WT" & pd0$time == 30], mean(c(.8, .9, 2.9))) && pd0$n[pd0$variable == "WT" & pd0$time == 30] == 3)
  # the fast path and the curated path agree when the curation does nothing
  alt <- plot_data_from_replicates(rp0)
  alt <- alt[order(alt$variable, alt$time), ]; ref <- as.data.frame(pd0[order(pd0$variable, pd0$time), ])
  check("the plot data equals the aggregate of the replicate data (both routes use the same numbers)", near(alt$mean_value, ref$mean_value) && near(alt$n, ref$n) && near(alt$sd_value, ref$sd_value))
  session$setInputs(use_od_filter = TRUE, od_min = 0, od_max = 1)
  pd1 <- prepare_plot_data(); rp1 <- prepare_replicate_data()
  check("OD filter: the 2.9 reading no longer enters the mean; n = 2 there", near(pd1$mean_value[pd1$variable == "WT" & pd1$time == 30], mean(c(.8, .9))) && pd1$n[pd1$variable == "WT" & pd1$time == 30] == 2)
  check("OD filter: the replicate frame loses that reading", nrow(rp1) == nrow(rp0) - 1 && all(rp1$value <= 1))
  check("OD filter: other time points are untouched", near(pd1$mean_value[pd1$time == 10], pd0$mean_value[pd0$time == 10]))
  check("OD filter shows in provenance and the methods draft", {
    pv <- report_meta()$settings_df; row <- pv$value[grepl("Data handling", pv$setting)]
    grepl("outside OD 0 to 1", row) && grepl("outside OD 0 to 1", draft_methods_text()) })
  check("OD filter shows in the data summary", grepl("outside OD 0 to 1", summary_html(), fixed = TRUE))
  check("OD filter shows in the figure legend text", grepl("outside OD 0 to 1", figure_legend_string(), fixed = TRUE))
  session$setInputs(od_min = 1, od_max = 0)
  check("a reversed range is swapped, not an error", !is.null(od_filter_range()) && od_filter_range()[1] == 0)
  session$setInputs(use_od_filter = FALSE)
  check("OD filter off again: the old numbers", near(prepare_plot_data()$mean_value, pd0$mean_value))
})

cat("\n--- server: replicate exclusion ---\n")
testServer(app, {
  summary_html <- function() { session$flushReact(); as.character(output$data_summary_ui$html) }
  do.call(session$setInputs, env$APP_DEFAULTS)
  ingest_data(rep_data, "reps.csv")
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = "Mut", show_replicate_points = FALSE)
  ct <- replicate_catalog_current()
  check("catalog: 3 replicates per sample, keys 'WT #1'..", nrow(ct) == 6 && all(c("WT #3", "Mut #1") %in% ct$key))
  pd0 <- prepare_plot_data(); m0 <- curve_metrics_current(); rm0 <- replicate_metrics_current()
  session$setInputs(excluded_reps = "WT #3")
  pd1 <- prepare_plot_data()
  check("excluding WT #3 changes the WT mean (the 2.9 is gone) and n = 2", near(pd1$mean_value[pd1$variable == "WT" & pd1$time == 30], mean(c(.8, .9))) && all(pd1$n[pd1$variable == "WT"] == 2))
  check("the other sample keeps n = 3", all(pd1$n[pd1$variable == "Mut"] == 3))
  rp1 <- prepare_replicate_data()
  check("the replicate frame has no WT replicate 3", !any(rp1$variable == "WT" & rp1$replicate == 3) && nrow(rp1) == 20)
  rx <- prepare_replicate_data(which = "excluded"); ra <- prepare_replicate_data(which = "all")
  check("which = 'excluded' returns only the excluded replicate", nrow(rx) == 4 && all(rx$variable == "WT" & rx$replicate == 3))
  check("which = 'all' has every row with an excluded flag", nrow(ra) == 24 && sum(ra$excluded) == 4)
  m1 <- curve_metrics_current()
  check("metrics follow the exclusion (WT max OD changed)", m1$max_od[m1$sample == "WT"] != m0$max_od[m0$sample == "WT"] && near(m1$max_od[m1$sample == "WT"], .85))
  rm1 <- replicate_metrics_current()
  check("per-replicate metrics follow the exclusion (WT has 2 replicates)", rm1$n_replicates[rm1$sample == "WT"] == 2 && rm0$n_replicates[rm0$sample == "WT"] == 3)
  check("statistics rows are built from the kept replicates only", { rows <- replicate_metric_rows_current(); sum(rows$sample == "WT") == 2 })
  check("the exclusion is in the data summary", grepl("1 replicate excluded", summary_html(), fixed = TRUE) && grepl("WT: rep 3", summary_html(), fixed = TRUE))
  check("the exclusion is in the figure legend text", grepl("1 replicate excluded (WT: rep 3)", figure_legend_string(), fixed = TRUE))
  check("the exclusion is in the report provenance", grepl("1 replicate excluded", report_meta()$settings_df$value[grepl("Data handling", report_meta()$settings_df$setting)], fixed = TRUE))
  check("the exclusion is in the methods draft", grepl("1 replicate excluded (WT: rep 3)", draft_methods_text(), fixed = TRUE))
  session$setInputs(show_replicate_points = TRUE)
  gp <- generate_plot()
  grey <- vapply(gp$layers, function(l) inherits(l$geom, "GeomPoint") && identical(l$aes_params$colour, "grey55") && identical(l$aes_params$shape, 1), logical(1))
  check("excluded replicates are drawn as hollow grey circles", sum(grey) == 1 && nrow(gp$layers[[which(grey)]]$data) == 4)
  check("the legend text mentions the hollow grey circles", grepl("Hollow grey circles", figure_legend_string(), fixed = TRUE))
  session$setInputs(show_replicate_points = FALSE)
  gp2 <- generate_plot()
  check("no grey layer without the replicate points", !any(vapply(gp2$layers, function(l) inherits(l$geom, "GeomPoint") && identical(l$aes_params$colour, "grey55"), logical(1))))
  # QC sees the excluded replicate
  qc <- rep_qc_current()
  check("replicate QC still includes the excluded replicate (flagged)", nrow(qc$replicates) == 6 && qc$replicates$excluded[qc$replicates$sample == "WT" & qc$replicates$replicate == 3])
  check("replicate QC singles out the 2.9 replicate as the most distant WT curve", qc$replicates$most_distant[qc$replicates$sample == "WT" & qc$replicates$replicate == 3])
  check("QC table has sample labels from the catalog", all(!is.na(qc$replicates$label)))
  # junk keys are ignored; restoring
  session$setInputs(excluded_reps = c("WT #3", "Ghost #1", "WT #9"))
  check("keys for replicates that do not exist are ignored", identical(excluded_rep_keys(), "WT #3"))
  session$setInputs(excluded_reps = character(0))
  check("restoring all replicates returns the original numbers", near(prepare_plot_data()$mean_value, pd0$mean_value) && near(curve_metrics_current()$max_od, m0$max_od))
  # all replicates of a sample
  session$setInputs(excluded_reps = c("WT #1", "WT #2", "WT #3"))
  check("excluding every replicate of a sample removes the sample from the plot data", !"WT" %in% prepare_plot_data()$variable && "Mut" %in% prepare_plot_data()$variable)
  session$setInputs(excluded_reps = character(0))
})
testServer(app, {   # stacked blocks and long format
  do.call(session$setInputs, env$APP_DEFAULTS)
  blocks <- data.frame(Time = c(0, 10, 0, 10), A = c(.1, .2, .12, .24), B = c(.1, .1, .1, .12))
  ingest_data(blocks, "blocks.csv")
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem")
  check("wide stacked blocks: 2 replicates per sample in the catalog", nrow(replicate_catalog_current()) == 4)
  session$setInputs(excluded_reps = "A #2")
  pdb <- prepare_plot_data()
  check("excluding block 2 of A: the means at t = 10 are block 1's", near(pdb$mean_value[pdb$variable == "A" & pdb$time == 10], .2) && pdb$n[pdb$variable == "A" & pdb$time == 10] == 1)
  session$setInputs(excluded_reps = character(0))
  lg <- data.frame(Time = rep(c(0, 10, 20), each = 4), Strain = rep(c("X", "X", "Y", "Y"), 3), Well = rep(c("A1", "A2", "B1", "B2"), 3),
                   OD = c(.1, .12, .2, .21, .3, .31, .4, .42, .5, .9, .6, .61))
  ingest_data(lg, "long.csv")
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem")
  check("long format with a Well column: 2 replicates per strain", nrow(replicate_catalog_current()) == 4 && identical(rv$replicate_col, "Well"))
  m_before <- prepare_plot_data()
  session$setInputs(excluded_reps = "X #2")
  mx <- prepare_plot_data()
  check("long format: excluding replicate 2 of X (well A2) leaves X's first well", near(mx$mean_value[mx$variable == "X" & mx$time == 20], .5) && mx$n[mx$variable == "X" & mx$time == 20] == 1)
  check("long format: Y is untouched", near(mx$mean_value[mx$variable == "Y"], m_before$mean_value[m_before$variable == "Y"]))
})

cat("\n--- server: blank sample / baseline and transforms ---\n")
bl <- data.frame(Time = c(0, 10, 20, 30), Media = c(.05, .05, .06, .06), Media = c(.05, .06, .06, .05),
                 S1 = c(.15, .25, .45, .85), S1 = c(.16, .27, .47, .87), S2 = c(.15, .17, .19, .21), check.names = FALSE)
testServer(app, {
  summary_html <- function() { session$flushReact(); as.character(output$data_summary_ui$html) }
  do.call(session$setInputs, env$APP_DEFAULTS)
  ingest_data(bl, "blank.csv")
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = "S2")
  raw <- prepare_plot_data()
  session$setInputs(blank_mode = "sample", blank_sample = "Media")
  pdb <- prepare_plot_data(); rpb <- prepare_replicate_data()
  media_mean <- c(.05, .055, .06, .055)
  check("blank sample: it is not a sample any more (plot data and replicate data)", !"Media" %in% pdb$variable && !"Media" %in% rpb$variable)
  check("blank sample: its mean curve is subtracted row-wise from S2", near(pdb$mean_value[pdb$variable == "S2"], c(.15, .17, .19, .21) - media_mean))
  check("blank sample: replicates keep their own spread (S1 replicates minus the same curve)", near(sort(rpb$value[rpb$variable == "S1" & rpb$time == 10]), sort(c(.25, .27) - .055)))
  check("blank sample: n is unchanged", all(pdb$n[pdb$variable == "S1"] == 2))
  check("blank sample: metrics see the corrected data", near(curve_metrics_current()$max_od[curve_metrics_current()$sample == "S2"], max(c(.15, .17, .19, .21) - media_mean)))
  check("blank sample: named in the summary, provenance, methods and legend",
        grepl("'Media'", summary_html()) && grepl("'Media'", figure_legend_string()) &&
        grepl("'Media'", draft_methods_text()) && any(grepl("'Media'", report_meta()$settings_df$value)))
  session$setInputs(blank_od = 0.5)
  check("the constant blank is ignored in the sample mode", near(prepare_plot_data()$mean_value, pdb$mean_value))
  session$setInputs(blank_mode = "constant")
  check("constant mode applies blank_od again", near(prepare_plot_data()$mean_value[prepare_plot_data()$variable == "S2"], c(.15, .17, .19, .21) - .5))
  session$setInputs(blank_od = 0)
  check("back to the defaults: the original numbers", near(prepare_plot_data()$mean_value, raw$mean_value))
  session$setInputs(blank_mode = "first_k", blank_k = 2)
  rpk <- prepare_replicate_data()
  check("first-k baseline: each replicate's first 2 readings average to zero", all(abs(tapply(rpk$value[rpk$time <= 10], list(rpk$variable[rpk$time <= 10], rpk$replicate[rpk$time <= 10]), mean)) < 1e-12, na.rm = TRUE))
  check("first-k baseline reaches the plot data", near(prepare_plot_data()$mean_value[prepare_plot_data()$variable == "S2" & prepare_plot_data()$time == 0], -.01))
  session$setInputs(blank_mode = "constant")
  # transforms
  session$setInputs(transform_mode = "t0")
  p <- prepare_plot_data()
  check("transform t0: every curve starts at 1", near(p$mean_value[p$time == 0], rep(1, 3)))
  check("transform t0: the metrics use it (max of S2 = 0.21 / 0.15)", near(curve_metrics_current()$max_od[curve_metrics_current()$sample == "S2"], .21 / .15))
  session$setInputs(transform_mode = "control", transform_control = "Media")
  p <- prepare_plot_data()
  check("transform control: the control curve is 1 and stays in the data", near(p$mean_value[p$variable == "Media"], rep(1, 4)))
  check("transform control: S2 / Media mean curve", near(p$mean_value[p$variable == "S2"], c(.15, .17, .19, .21) / media_mean))
  session$setInputs(transform_mode = "pct_max")
  p <- prepare_plot_data()
  check("transform 0-100 %: each sample's mean curve peaks at 100", near(tapply(p$mean_value, p$variable, max), c(Media = 100, S1 = 100, S2 = 100)))
  check("transform 0-100 %: replicate data is on the same scale", max(prepare_replicate_data()$value) > 99)
  session$setInputs(transform_mode = "ln")
  p <- prepare_replicate_data()
  check("transform ln: replicate values are log(OD)", near(sort(p$value[p$variable == "S2"]), sort(log(c(.15, .17, .19, .21)))))
  check("transform ln: plot means are means of the logs", near(prepare_plot_data()$mean_value[prepare_plot_data()$variable == "S2"], log(c(.15, .17, .19, .21))))
  check("transform shows in the summary, provenance, methods and legend", {
    grepl("ln\\(OD\\)", summary_html()) && grepl("ln\\(OD\\)", figure_legend_string()) &&
    grepl("ln\\(OD\\)", draft_methods_text()) && any(grepl("ln\\(OD\\)", report_meta()$settings_df$value)) })
  session$setInputs(transform_mode = "none")
  check("transform none: the original numbers", near(prepare_plot_data()$mean_value, raw$mean_value))
  # blank + transform + exclusion together
  session$setInputs(blank_mode = "sample", blank_sample = "Media", transform_mode = "t0", excluded_reps = "S1 #2")
  p <- prepare_plot_data()
  check("blank sample + t0 + an excluded replicate combine (S1 has n = 1, normalised to its own t0)", all(p$n[p$variable == "S1"] == 1) && near(p$mean_value[p$variable == "S1" & p$time == 0], 1))
  session$setInputs(blank_mode = "constant", transform_mode = "none", excluded_reps = character(0))
  # the blank sample is taken out of the selection (observer) and the figure builds
  session$setInputs(blank_mode = "sample", blank_sample = "Media", selected_samples = rv$od_vars)
  check("a figure builds with the blank selected (it is dropped from the data)", inherits(generate_plot(), "ggplot"))
  session$setInputs(blank_mode = "constant")
})

cat("\n--- server: n = 1 warning ---\n")
testServer(app, {
  do.call(session$setInputs, env$APP_DEFAULTS)
  ingest_data(data.frame(Time = c(0, 10, 20), A = c(.1, .2, .4), B = c(.1, .1, .2)), "single.csv")
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem")
  n1_state$shown <- FALSE          # (a rendered output may already have drawn the figure)
  invisible(generate_plot())
  check("error bars requested with n = 1 everywhere: the one-time warning fires", isTRUE(n1_state$shown))
  invisible(generate_plot())
  check("it stays 'shown' (one-time) on a second render", isTRUE(n1_state$shown))
  session$setInputs(error_type = "none")
  invisible(generate_plot())
  check("no error type: no new warning", isTRUE(n1_state$shown))
  ingest_data(rep_data, "reps.csv")
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem")
  invisible(generate_plot())
  check("with replicates the flag is reset", !isTRUE(n1_state$shown))
})

cat("\n--- server: Samples All / None ---\n")
testServer(app, {
  do.call(session$setInputs, env$APP_DEFAULTS)
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  check("All returns every sample", identical(select_samples("all"), rv$od_vars))
  check("None returns no sample", identical(select_samples("none"), character(0)))
  session$setInputs(samples_all = 1)
  session$setInputs(samples_none = 1)
  check("the buttons are wired (observers run without error)", TRUE)
})

cat("\n--- server: extinction floor and t_stat ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = "Uninfected")
  m0 <- curve_metrics_current()
  check("default extinction floor 0.05 and automatic t_stat reach the engine unchanged", isTRUE(all.equal(m0$extinction_time, env$compute_curve_metrics(prepare_plot_data(), ref_sample = "Uninfected")$extinction_time)))
  session$setInputs(extinction_threshold = 0.4)
  m1 <- curve_metrics_current()
  check("a different extinction floor changes the extinction time", !isTRUE(all.equal(m0$extinction_time, m1$extinction_time)))
  check("the engine agrees with the app at the new floor", isTRUE(all.equal(m1$extinction_time, env$compute_curve_metrics(prepare_plot_data(), ref_sample = "Uninfected", extinction_threshold = 0.4)$extinction_time)))
  session$setInputs(extinction_threshold = 0.05)
  txt0 <- paste(as.character(output$storms_window_readout$html), collapse = "")
  check("readout: auto", grepl("Storms integration window: 0 to t_stat (auto)", txt0, fixed = TRUE))
  session$setInputs(t_stat_override = 90)
  m2 <- curve_metrics_current()
  check("t_stat override changes the local virulence", !isTRUE(all.equal(m0$local_virulence, m2$local_virulence)))
  check("the engine agrees at the override", isTRUE(all.equal(m2$local_virulence, env$compute_curve_metrics(prepare_plot_data(), ref_sample = "Uninfected", t_stat_override = 90)$local_virulence)))
  txt1 <- paste(as.character(output$storms_window_readout$html), collapse = "")
  check("readout: override with the value", grepl("(override) = 90", txt1, fixed = TRUE))
  rm1 <- replicate_metrics_current()
  check("replicate metrics accept the same arguments", is.data.frame(rm1) && nrow(rm1) == 3)
  rr <- replicate_metric_rows_current()
  check("replicate metric rows carry local virulence at the override", all(is.finite(rr$local_virulence[rr$sample != "Uninfected"])))
  session$setInputs(t_stat_override = NA_real_)
  check("blank override = automatic again", isTRUE(all.equal(curve_metrics_current()$local_virulence, m0$local_virulence)))
})

cat("\n--- server: settings round trip ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = rv$od_vars, use_od_filter = TRUE, od_min = 0.02, od_max = 1.5, blank_mode = "first_k", blank_k = 4,
                    transform_mode = "pct_max", extinction_threshold = 0.1, t_stat_override = 120, excluded_reps = "Uninfected #2",
                    time_col_choice = "Time", data_entry_text = "x", data_view_table_rows_current = 1:3, rep_qc_flag = 25)
  sj <- settings_json_list()$global_settings
  check("the new inputs are saved", identical(sj$use_od_filter, TRUE) && near(sj$od_min, 0.02) && near(sj$od_max, 1.5) && identical(sj$blank_mode, "first_k") &&
        sj$blank_k == 4 && identical(sj$transform_mode, "pct_max") && near(sj$extinction_threshold, 0.1) && sj$t_stat_override == 120)
  check("per-file inputs are not saved (time column, exclusions, pasted text, DT state)",
        !any(c("time_col_choice", "excluded_reps", "data_entry_text", "data_view_table_rows_current", "samples_all", "rep_qc_flag") %in% names(sj)))
  tf <- tempfile(fileext = ".json")
  jsonlite::write_json(settings_json_list(), tf, pretty = TRUE, auto_unbox = TRUE)
  session$setInputs(loadSettings = list(name = "s.json", datapath = tf))
  check("loading applies the new global keys", grepl("global setting", rv$settings_status$msg) && as.integer(sub(" .*", "", rv$settings_status$msg)) >= 8)
  tf2 <- tempfile(fileext = ".json")
  writeLines('{"global_settings": {"use_od_filter": true, "od_min": 0.1, "blank_mode": "first_k", "time_col_choice": "Time", "excluded_reps": ["A #1"], "data_view_table_state": 1}}', tf2)
  session$setInputs(loadSettings = list(name = "s2.json", datapath = tf2))
  check("loading skips the per-file inputs and counts only the three look settings", grepl("^3 global setting", rv$settings_status$msg))
})

cat(sprintf("\n============ SUMMARY: %d passed / %d failed / %d skipped ============\n", pass, fail, skip))
if (fail > 0) quit(status = 1)
