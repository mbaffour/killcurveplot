# ============================================================================
# v2.17 UI-level additions: log-axis clipping (y_oob), QC flags, notebook
# downloads + legend text in the reports, figure bundle, threshold line,
# gradient palettes, empty-selection robustness.
# Run from the repository root:  Rscript tests/test_v217_ui.R
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
quiet <- function(expr) suppressWarnings(suppressMessages(expr))

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.17.0 or later", package_version(env$APP_VERSION) >= "2.17.0")

cat("\n--- defaults and the legacy preset ---\n")
check("y_oob: journal default squish, Legacy look censor",
      identical(env$APP_DEFAULTS$y_oob, "squish") && identical(env$legacy_preset_settings()$y_oob, "censor"))
check("threshold and QC defaults are in APP_DEFAULTS; threshold off in both looks",
      isFALSE(env$APP_DEFAULTS$enable_threshold) && isFALSE(env$legacy_preset_settings()$enable_threshold) &&
      env$APP_DEFAULTS$threshold_value == 0.1 && identical(env$APP_DEFAULTS$threshold_label, "") &&
      identical(env$APP_DEFAULTS$threshold_color, "grey40") && identical(env$APP_DEFAULTS$threshold_linetype, "dashed") &&
      env$APP_DEFAULTS$qc_od_ceiling == 2.5)
check("the three new notebook fields are in APP_DEFAULTS",
      all(c("nb_passage", "nb_inducer_conc", "nb_other_condition") %in% names(env$APP_DEFAULTS)))
ui_html <- as.character(env$ui)
check("UI has the new inputs and buttons",
      all(vapply(c("y_oob", "enable_threshold", "threshold_value", "threshold_label", "threshold_color", "threshold_linetype",
                   "qc_od_ceiling", "qc_flags_ui", "dash_qc", "nb_passage", "nb_inducer_conc", "nb_other_condition",
                   "download_notebook_txt", "download_notebook_csv", "downloadFigureBundle"),
                 function(id) grepl(paste0('id="', id, '"'), ui_html, fixed = TRUE), logical(1))))
check("Dashboard hint points to the Statistics tab", grepl("see the Statistics tab", ui_html, fixed = TRUE) && !grepl("Stats & Reports → CSV", ui_html, fixed = TRUE))

cat("\n--- gradient palettes ---\n")
b6 <- env$get_palette_colors("blues", 6); r6 <- env$get_palette_colors("blue_red", 6)
check("blues: 6 distinct colours, first black", length(unique(b6)) == 6 && b6[1] == "#000000")
check("blue_red: 6 distinct colours, first black, ends blue and red", length(unique(r6)) == 6 && r6[1] == "#000000" && r6[2] == "#2166AC" && r6[6] == "#B2182B")
check("palette with one sample is black, not an error", identical(env$get_palette_colors("blues", 1), "#000000"))
check("older palettes unchanged", length(env$get_palette_colors("viridis", 4)) == 4 && is.null(env$get_palette_colors("custom", 3)))

cat("\n--- qc_flags(): pure function ---\n")
tt <- seq(0, 60, by = 5)
mk_rep <- function(v, times = tt, s = "S") do.call(rbind, lapply(1:3, function(r)
  data.frame(time = times, variable = s, replicate = r, value = v, stringsAsFactors = FALSE)))
mk_pd <- function(v, times = tt, s = "S")
  data.frame(time = times, variable = s, mean_value = v, stringsAsFactors = FALSE)
good <- 0.05 * exp(0.05 * tt)
q0 <- env$qc_flags(mk_rep(good), mk_pd(good), "S")
check("clean growth curve: zero rows with the three columns", nrow(q0) == 0 && identical(names(q0), c("sample", "flag", "detail")))
v31 <- good; v31[5] <- 3.1
q1 <- env$qc_flags(mk_rep(v31), mk_pd(v31), NULL)
check("a 3.1 reading flags CEILING for that sample", "CEILING" %in% q1$flag && q1$sample[q1$flag == "CEILING"] == "S")
check("the ceiling is adjustable", nrow(env$qc_flags(mk_rep(good), mk_pd(good), NULL, ceiling = 0.5)) > 0 &&
      "CEILING" %in% env$qc_flags(mk_rep(good), mk_pd(good), NULL, ceiling = 0.5)$flag)
vneg <- good; vneg[3] <- -0.01
check("a reading <= 0 flags NEGATIVE", "NEGATIVE" %in% env$qc_flags(mk_rep(vneg), mk_pd(vneg), NULL)$flag)
miss <- tt[-6]; gm <- 0.05 * exp(0.05 * miss)
qg <- env$qc_flags(mk_rep(gm, miss), mk_pd(gm, miss), NULL)
check("a missing time step flags GAP", "GAP" %in% qg$flag)
miss2 <- tt[-c(5, 6)]; gm2 <- 0.05 * exp(0.05 * miss2)
check("two missing steps flag GAP too", "GAP" %in% env$qc_flags(mk_rep(gm2, miss2), mk_pd(gm2, miss2), NULL)$flag)
flat <- rep(0.5, length(tt)) + c(0, 0.1, 0.2, 0.1, 0.2, rep(0.15, length(tt) - 5))
check("a flat reference flags FLAT_REFERENCE; only for the reference sample",
      "FLAT_REFERENCE" %in% env$qc_flags(mk_rep(flat), mk_pd(flat), "S")$flag &&
      !"FLAT_REFERENCE" %in% env$qc_flags(mk_rep(flat), mk_pd(flat), "other")$flag &&
      !"FLAT_REFERENCE" %in% env$qc_flags(mk_rep(good), mk_pd(good), "S")$flag)
check("NULL / empty inputs give zero rows", nrow(env$qc_flags(NULL, NULL, NULL)) == 0 && nrow(env$qc_flags(mk_rep(good)[0, ], mk_pd(good)[0, ], "S")) == 0)

cat("\n--- server: y_oob, threshold, QC flags, notebook, reports, bundle ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = "Uninfected")

  # -- y_oob --
  session$setInputs(y_log_min_exponent = -1, y_oob = "squish")
  pts <- function() { b <- suppressWarnings(ggplot_build(generate_plot()))
    i <- which(vapply(b$plot$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))[1]; b$data[[i]] }
  ps <- pts()
  check("squish: all 13 points of every sample are kept, the floor ones at y = -1 (log10 of 0.1)",
        all(table(ps$group[is.finite(ps$y)]) == 13) && sum(abs(ps$y + 1) < 1e-9) >= 10 && min(ps$y, na.rm = TRUE) >= -1 - 1e-9)
  session$setInputs(y_oob = "censor")
  pc <- pts()
  check("censor (legacy): points below the floor are dropped", sum(is.finite(pc$y)) < sum(is.finite(ps$y)) && any(table(pc$group[is.finite(pc$y)]) < 13))
  session$setInputs(y_oob = "squish", y_log_min_exponent = -2)

  # -- threshold --
  has_hline <- function() any(vapply(generate_plot()$layers, function(l) inherits(l$geom, "GeomHline"), logical(1)))
  check("no threshold line by default", !has_hline())
  session$setInputs(enable_threshold = TRUE, threshold_value = 0.3, threshold_label = "lysis threshold")
  check("enable_threshold adds a GeomHline", has_hline())
  gp <- generate_plot()
  hl <- gp$layers[[which(vapply(gp$layers, function(l) inherits(l$geom, "GeomHline"), logical(1)))[1]]]
  check("threshold line at the value, with the chosen type and colour",
        identical(hl$data$yintercept, 0.3) && identical(hl$aes_params$linetype, "dashed") && identical(hl$aes_params$colour, "grey40"))
  check("threshold label is a text layer at the right end", any(vapply(gp$layers, function(l) inherits(l$geom, "GeomText") &&
        identical(l$data$label, "lysis threshold"), logical(1))))
  session$setInputs(threshold_label = "")
  check("blank label: no text layer", !any(vapply(generate_plot()$layers, function(l) inherits(l$geom, "GeomText"), logical(1))))
  session$setInputs(enable_threshold = FALSE)
  check("threshold off again: no GeomHline", !has_hline())

  # -- QC flags --
  check("the demo flags nothing", nrow(qc_current()) == 0)
  check("the demo QC list renders nothing", is.null(output$qc_flags_ui$html) || !nzchar(as.character(output$qc_flags_ui$html)) || !grepl("Data quality", as.character(output$qc_flags_ui$html)))
  session$setInputs(qc_od_ceiling = 0.2)
  q <- qc_current()
  check("a lower ceiling flags CEILING for the grown samples", "CEILING" %in% q$flag && nrow(q) >= 1)
  check("QC list renders the flags", grepl("CEILING", as.character(output$qc_flags_ui$html)) && grepl("CEILING", as.character(output$dash_qc$html)))
  session$setInputs(qc_od_ceiling = 2.5)

  # -- QC through a fixture with a 3.1 reading and a missing time step --
  fx <- data.frame(Time = c(0, 5, 10, 20, 25, 30), Ctl = c(0.05, 0.1, 0.2, 3.1, 0.6, 0.9),
                   Ctl = c(0.05, 0.11, 0.2, 3.0, 0.6, 0.9), Ctl = c(0.05, 0.1, 0.21, 3.2, 0.6, 0.9), check.names = FALSE)
  ingest_data(fx, "fixture.csv")
  session$setInputs(selected_samples = rv$od_vars, metrics_ref_sample = "")
  qf <- qc_current()
  check("fixture: CEILING and GAP raised through the server pipeline", all(c("CEILING", "GAP") %in% qf$flag))
  check("fixture: the flags reach the HTML report (QC flags section)", { h <- html_report_string(); grepl("<h2>QC flags</h2>", h, fixed = TRUE) && grepl("CEILING", h, fixed = TRUE) })
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  session$setInputs(selected_samples = rv$od_vars, metrics_ref_sample = "Uninfected")

  # -- notebook fields, legend and downloads --
  session$setInputs(nb_exp_id = "EXP 7/a", nb_host_strain = "E. coli MG1655", nb_phage_plasmid = "N4", nb_moi = "10",
                    nb_passage = "P12", nb_inducer = "IPTG", nb_inducer_conc = "0.5 mM", nb_other_condition = "shaking, 37 C",
                    nb_custom_key1 = "Plate ID", nb_custom_val1 = "PL-42", nb_observations = "Lysis at 40 min.",
                    methods_text = "Cultures were grown in LB.")
  leg <- figure_legend_string()
  check("figure_legend_string() gives the paragraph; the box shows the same", nzchar(leg) && identical(output$figure_legend_text, leg))
  txt_file <- output$download_notebook_txt
  tl <- readLines(txt_file, warn = FALSE)
  check("notebook .txt: filled fields as 'key: value' lines", "Experiment ID: EXP 7/a" %in% tl && "Passage #: P12" %in% tl &&
        "Inducer concentration: 0.5 mM" %in% tl && "Other condition: shaking, 37 C" %in% tl && "Plate ID: PL-42" %in% tl)
  check("notebook .txt: legend text and methods text", any(grepl(substr(leg, 1, 40), tl, fixed = TRUE)) && "FIGURE LEGEND" %in% tl &&
        "METHODS" %in% tl && "Cultures were grown in LB." %in% tl)
  check("notebook .txt: empty fields are not listed", !any(grepl("^Project:", tl)) && !any(grepl("^Tags:", tl)))
  check("notebook .txt file name carries the safe experiment id", grepl("^notebook_EXP_7_a_[0-9]{8}\\.txt$", basename(txt_file)))
  csv_file <- output$download_notebook_csv
  cdf <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE)
  check("notebook .csv: one data row, one column per filled field, custom key as a column name",
        nrow(cdf) == 1 && all(c("Experiment ID", "Passage #", "Inducer concentration", "Other condition", "Plate ID", "MOI") %in% names(cdf)) &&
        cdf[["Plate ID"]] == "PL-42" && cdf[["Passage #"]] == "P12" && !"Project" %in% names(cdf))
  check("notebook .csv: the column count equals the filled-field count", ncol(cdf) == length(notebook_filled()))

  html <- html_report_string()
  check("HTML report: 'Figure legend' paragraph right under the main figure",
        grepl("alt='Lysis curve figure'/>\\s*<h3>Figure legend</h3><p class='legend'>", html) && grepl(substr(leg, 1, 40), html, fixed = TRUE))
  check("HTML report: QC flags section after provenance (no flags on the demo)",
        grepl("<h2>QC flags</h2><p>No data quality flags", html, fixed = TRUE) &&
        regexpr("<h2>QC flags</h2>", html, fixed = TRUE) > regexpr("<h2>Data provenance</h2>", html, fixed = TRUE) &&
        regexpr("<h2>QC flags</h2>", html, fixed = TRUE) < regexpr("<h2>Experiment notebook</h2>", html, fixed = TRUE))
  check("HTML report notebook table lists the three new fields", grepl("Passage #", html, fixed = TRUE) && grepl("Inducer concentration", html, fixed = TRUE) && grepl("Other condition", html, fixed = TRUE))
  pdf_f <- tempfile(fileext = ".pdf")
  ok_pdf <- tryCatch({ quiet(pdf_report_file(pdf_f)); file.exists(pdf_f) && file.size(pdf_f) > 5000 }, error = function(e) { cat("   pdf error:", conditionMessage(e), "\n"); FALSE })
  check("PDF report builds with the legend and QC line", ok_pdf)
  if (ok_pdf && nzchar(Sys.which("pdftotext"))) {
    ptxt <- paste(system2("pdftotext", c(pdf_f, "-"), stdout = TRUE), collapse = "\n")
    check("PDF report text has the 'Figure legend' heading and the QC line", grepl("Figure legend", ptxt, fixed = TRUE) && grepl("QC flags: none", ptxt, fixed = TRUE))
  } else skipit("pdftotext not available: PDF text check")
  pdf_f2 <- tempfile(fileext = ".pdf")
  session$setInputs(panel_size_mode = "fit")
  check("PDF report builds in fit mode (ggplot figure page with legend)",
        tryCatch({ quiet(pdf_report_file(pdf_f2)); file.size(pdf_f2) > 5000 }, error = function(e) FALSE))
  session$setInputs(panel_size_mode = "fixed")
  qcf <- tempfile(fileext = ".pdf"); session$setInputs(qc_od_ceiling = 0.2)
  check("PDF report builds with a QC flags page", tryCatch({ quiet(pdf_report_file(qcf)); file.size(qcf) > 5000 }, error = function(e) FALSE))
  session$setInputs(qc_od_ceiling = 2.5)

  # -- figure bundle --
  zf <- output$downloadFigureBundle
  check("bundle: file name figure_bundle_<W>x<H>mm.zip", grepl("^figure_bundle_[0-9]+x[0-9]+mm\\.zip$", basename(zf)))
  have_zip <- requireNamespace("zip", quietly = TRUE)
  if (have_zip) {
    zl <- basename(zip::zip_list(zf)$filename)
  } else {
    zl <- basename(sub("^\\s*[0-9]+\\s+[0-9-]+\\s+[0-9:]+\\s+", "", system2("unzip", c("-l", zf), stdout = TRUE)))
  }
  check("bundle: settings.json and source_data_tidy.csv", all(c("settings.json", "source_data_tidy.csv") %in% zl))
  check("bundle: figures, tables, legend, methods and README",
        all(c("figure_main.svg", "figure_main.pdf", "summary_statistics.csv", "curve_metrics.csv", "figure_legend.txt",
              "methods.txt", "README.txt") %in% zl))
  check("bundle: composed figure SVG when >= 2 panels are selected (default composer selection)", "figure_composed.svg" %in% zl)
  ex <- tempfile("bundle_x_"); dir.create(ex)
  utils::unzip(zf, exdir = ex)
  fold <- list.dirs(ex, recursive = FALSE)
  src <- read.csv(file.path(fold, "source_data_tidy.csv"))
  check("bundle: tidy source data has time, sample, replicate, value", identical(names(src), c("time", "sample", "replicate", "value")) && nrow(src) == nrow(prepare_replicate_data()))
  sj <- jsonlite::fromJSON(file.path(fold, "settings.json"), simplifyVector = FALSE)
  check("bundle: settings.json is the Save Settings document", identical(sj$version, env$APP_VERSION) && !is.null(sj$global_settings$y_oob) && !is.null(sj$sample_aesthetics))
  rd <- readLines(file.path(fold, "README.txt"), warn = FALSE)
  check("bundle: README names the version, data source, size and fonts",
        any(grepl(env$APP_VERSION, rd, fixed = TRUE)) && any(grepl("Demo data", rd, fixed = TRUE)) && any(grepl("Figure size: ", rd)) && any(grepl("Fonts: Arial", rd)))
  check("bundle: legend file equals the legend text", identical(trimws(paste(readLines(file.path(fold, "figure_legend.txt"), warn = FALSE), collapse = "\n")), leg))
  session$setInputs(compose_panels = "main", methods_text = "")
  zf2 <- output$downloadFigureBundle
  zl2 <- if (have_zip) basename(zip::zip_list(zf2)$filename) else character(0)
  if (have_zip) check("bundle: no composed SVG with one panel, no methods.txt when the methods box is empty",
                      !"figure_composed.svg" %in% zl2 && !"methods.txt" %in% zl2)
  check("archive kind resolves to zip or tar", env$bundle_archive_kind() %in% c("zip", "tar"))
})

cat("\n--- selected samples that match nothing loaded (no errors, zero-row frames) ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = "no such sample", exclude_timepoints = "", metrics_ref_sample = "")
  rp <- tryCatch(prepare_replicate_data(), error = function(e) e)
  pp <- tryCatch(prepare_plot_data(), error = function(e) e)
  check("prepare_replicate_data(): zero rows, the four columns, no error",
        is.data.frame(rp) && nrow(rp) == 0 && identical(names(rp), c("time", "variable", "replicate", "value")))
  check("prepare_plot_data(): zero rows with its usual columns, no error",
        is.data.frame(pp) && nrow(pp) == 0 &&
        identical(names(pp), c("time", "variable", "mean_value", "sd_value", "n", "sem_value", "ci95_value")))
  st <- tryCatch(analysis_stats_current(), error = function(e) e)
  check("analysis_stats_current(): the empty structure, no error",
        is.list(st) && !inherits(st, "error") && is.null(st$pairwise) && is.null(st$omnibus) && is.null(st$posthoc))
  ok_out <- function(id) tryCatch({ output[[id]]; TRUE }, error = function(e) inherits(e, "shiny.silent.error"))
  check("Statistics-tab outputs tolerate it (value or a silent req)",
        all(vapply(c("stats_tab_summary", "stats_tab_metrics", "stats_tab_repmetrics", "stats_tab_notes",
                     "stats_tab_pairwise", "stats_tab_omnibus", "stats_tab_posthoc", "dash_metrics_table"), ok_out, logical(1))))
  check("QC and legend tolerate it", is.null(qc_current()) || nrow(qc_current()) == 0)
  check("legend string is empty (not an error)", identical(figure_legend_string(), ""))
  # long format too
  lg <- data.frame(Time = rep(c(0, 10, 20), each = 2), Group = "A", Val = c(.1, .1, .2, .2, .4, .4), stringsAsFactors = FALSE)
  ingest_data(lg, "long.csv")
  session$setInputs(selected_samples = "zzz")
  check("long format: zero-row frames as well", tryCatch(nrow(prepare_replicate_data()) == 0 && nrow(prepare_plot_data()) == 0, error = function(e) FALSE))
})

cat(sprintf("\n============ SUMMARY: %d passed / %d failed / %d skipped ============\n", pass, fail, skip))
if (fail > 0) quit(status = 1)
