# ============================================================================
# v2.20 UI integration of the v2.20 engine: the curve phases figure and table, the bootstrap intervals (Statistics tab,
# metric summary error source), the separation-from-the-reference figure and table, the report / bundle / export-all
# additions.
# Run from the repository root:  Rscript tests/test_v220_integration.R
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

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.20.0 or later", package_version(env$APP_VERSION) >= "2.20.0")

cat("\n--- defaults, choices, UI ---\n")
d <- env$APP_DEFAULTS
check("new inputs have APP_DEFAULTS entries",
      identical(d$boot_metrics, c("lysis_time", "od_drop", "mu_max", "auc", "infection_strength")) && d$boot_n == 1000 && d$boot_seed == 1 &&
      identical(d$metric_error_source, "replicates") && identical(d$diff_mode, "difference") && isTRUE(d$diff_mark))
check("neither the journal preset nor the Legacy look touch them",
      !any(grepl("^(boot_|diff_|metric_error)", names(env$legacy_preset_settings()))) &&
      !any(grepl("^(boot_|diff_|metric_error)", names(env$journal_preset_settings(89, 65)))))
check("phases and difference are analysis figures and composer panels",
      all(c("phases", "difference") %in% env$ANALYSIS_FIGURE_CHOICES) && all(c("phases", "difference") %in% env$COMPOSE_PANEL_CHOICES))
ui_html <- as.character(env$ui)
check("UI: every new input and output id",
      all(vapply(c("boot_metrics", "boot_n", "boot_seed", "boot_run", "boot_table", "boot_note", "downloadBootCSV",
                   "metric_error_source", "diff_mode", "diff_mark", "phases_table", "phases_note", "downloadPhasesCSV",
                   "analysis_separation_table", "stats_tab_separation_table"),
                 function(id) grepl(paste0('id="', id, '"'), ui_html, fixed = TRUE), logical(1))))
check("UI: the three Statistics sections, before the Methods text",
      all(vapply(c("Curve phases", "Bootstrap intervals", "First separation from reference"), function(x) grepl(x, ui_html, fixed = TRUE), logical(1))) &&
      regexpr("Bootstrap intervals", ui_html, fixed = TRUE) < regexpr("Methods text (editable)", ui_html, fixed = TRUE))
check("UI: the bootstrap interval is described as a percentile interval from replicate resampling, degenerate at n = 3",
      grepl("percentile", ui_html, fixed = TRUE) && grepl("n = 3", ui_html, fixed = TRUE))

cat("\n--- pure helpers ---\n")
check("phase palette: the seven phases, fixed",
      identical(names(env$PHASE_COLORS), env$PHASE_LEVELS) && length(env$PHASE_LEVELS) == 7 && !anyDuplicated(env$PHASE_COLORS) &&
      env$PHASE_BAND_ALPHA == 0.12)
l1 <- env$phase_layout(0, 1, 1)
check("phase_layout: one sample = bands over the whole panel, no strips", is.null(l1$strips) && l1$limits[1] < 0 && l1$limits[2] > 1)
l3 <- env$phase_layout(0, 1, 3)
P <- diff(l3$limits)
check("phase_layout: 3 strips of 6 % of the panel at the top, curves below with a gap",
      nrow(l3$strips) == 3 && isTRUE(all.equal(l3$strips$ymax[1], l3$limits[2])) &&
      isTRUE(all.equal(l3$strips$ymax - l3$strips$ymin, rep(0.06 * P, 3))) &&
      isTRUE(all.equal(l3$strips$ymax[2:3], l3$strips$ymin[1:2])) && l3$strips$ymin[3] > 1)
check("phase_layout: the curves fill the rest (data range = 1 - 4 % - 3 % - 3 x 6 % of the panel)",
      isTRUE(all.equal(1 / P, 1 - 0.04 - 0.03 - 0.18)))
ph_tab <- data.frame(sample = c("A", "A"), phase = c("lag", "lysis"), t_start = c(0, 30.123), t_end = c(30.123, 100),
                     od_start = c(0.05, 0.123456), od_end = c(0.123456, 0.0198), duration = c(30.123, 69.877), stringsAsFactors = FALSE)
dt <- env$phases_display_table(ph_tab)
check("phases_display_table: the documented columns, 3 significant digits",
      identical(names(dt), c("Sample", "Phase", "Start", "End", "Duration", "OD start", "OD end")) && dt$Start[2] == "30.1" && dt$`OD start`[2] == "0.123")
check("phases_display_table: NULL for nothing", is.null(env$phases_display_table(NULL)))
check("BOOT_METRIC_NAMES: lysis time, OD drop, mu_max, AUC yes; the reference-relative infection strength no",
      all(c("lysis_time", "od_drop", "mu_max", "auc") %in% env$BOOT_METRIC_NAMES) && !"infection_strength" %in% env$BOOT_METRIC_NAMES)
sd_tab <- env$separation_display_table(data.frame(sample = c("U", "P", "Q"), first_separation_time = c(NA, 75, NA)), ref = "U", tunit = "min")
check("separation_display_table: the reference is left out, 'none' when never reached, the unit in the header",
      identical(sd_tab$Sample, c("P", "Q")) && identical(sd_tab[[2]], c("75", "none")) && names(sd_tab)[2] == "First separation (min)")

cat("\n--- server: curve phases ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1])
  p <- generate_analysis_plot("phases")
  check("phases renders a real figure (not the placeholder)",
        inherits(p, "ggplot") && !is_placeholder_plot(p) && !is.null(tryCatch(ggplot_build(p), error = function(e) NULL)))
  ph <- phases_current()
  check("the phases table has rows for every sample", setequal(unique(ph$sample), smp) && all(table(ph$sample) >= 2))
  check("the table is in sample order and each sample's phases are in time order",
        identical(unique(ph$sample), sample_levels(smp)) && all(tapply(ph$t_start, ph$sample, function(x) !is.unsorted(x))))
  lay <- vapply(p$layers, function(l) class(l$geom)[1], "")
  b <- ggplot_build(p)
  rect_d <- b$data[[which(lay == "GeomRect")]]
  check("one rectangle per phase of every sample, translucent strips",
        nrow(rect_d) == nrow(ph) && all(rect_d$alpha > 0.12))
  yl <- b$layout$panel_params[[1]]$y.range
  check("several samples: the strips are stacked at the top, 6 % of the panel each, one row per sample",
        length(unique(round(rect_d$ymax, 8))) == 3 && isTRUE(all.equal(max(rect_d$ymax), yl[2], tolerance = 1e-6)) &&
        isTRUE(all.equal(rect_d$ymax - rect_d$ymin, rep(0.06 * diff(yl), nrow(rect_d)), tolerance = 1e-6)))
  fills <- unique(rect_d$fill)
  check("the fill follows the fixed phase palette",
        all(substr(fills, 1, 7) %in% toupper(substr(unname(env$PHASE_COLORS), 1, 7)) | substr(fills, 1, 7) %in% unname(env$PHASE_COLORS)))
  check("no text over the strips: one 1.5 mm square (shape 15, size 2, stroke 0) per sample in its colour, and the curves are drawn",
        !"GeomText" %in% lay && sum(lay == "GeomPoint") == 1 && nrow(b$data[[which(lay == "GeomPoint")]]) == 3 &&
        all(b$data[[which(lay == "GeomPoint")]]$shape == 15) && all(b$data[[which(lay == "GeomPoint")]]$size == 2) &&
        sum(lay == "GeomLine") == 1)
  check("the phase legend title explains the strip order, over two lines",
        identical(p$scales$get_scales("fill")$name, "Phase (strips top\u2192bottom\nin legend order)"))
  check("the phase key sits outside right", identical(p$theme$legend.position, "right"))
  check("the curves lie below the strips (the highest mean value is under the lowest strip edge)",
        max(b$data[[which(lay == "GeomLine")]]$y) < min(rect_d$ymin))
  # one sample: full-height bands
  session$setInputs(selected_samples = smp[3])
  p1 <- generate_analysis_plot("phases"); lay1 <- vapply(p1$layers, function(l) class(l$geom)[1], ""); b1 <- ggplot_build(p1)
  r1 <- b1$data[[which(lay1 == "GeomRect")]]
  check("one sample: full-height bands (every rectangle spans the whole panel), alpha 0.12, no strip marks, plain legend title",
        !is_placeholder_plot(p1) && length(unique(r1$ymin)) == 1 && length(unique(r1$ymax)) == 1 && all(abs(r1$alpha - 0.12) < 1e-9) &&
        !"GeomText" %in% lay1 && !"GeomPoint" %in% lay1 && identical(p1$scales$get_scales("fill")$name, "Phase"))
  session$setInputs(selected_samples = smp)
  # log axis
  session$setInputs(y_scale_type = "log")
  pl <- generate_analysis_plot("phases")
  check("log y axis: still a real figure on a log10 scale",
        !is_placeholder_plot(pl) && grepl("log", ggplot_build(pl)$layout$panel_scales_y[[1]]$trans$name))
  session$setInputs(y_scale_type = "linear")
  pl2 <- generate_analysis_plot("phases")
  check("linear y axis renders too", !is_placeholder_plot(pl2) && !grepl("log", ggplot_build(pl2)$layout$panel_scales_y[[1]]$trans$name))
  # follows the metrics inputs
  n_before <- nrow(phases_current())
  session$setInputs(metrics_lysis_threshold_pct = 40)
  ph40 <- phases_current()
  check("the lysis threshold input moves the lysis edge (as it moves lysis_time)",
        ph40$t_start[ph40$sample == "Phage MOI 10" & ph40$phase == "lysis"] >
        ph$t_start[ph$sample == "Phage MOI 10" & ph$phase == "lysis"])
  session$setInputs(metrics_lysis_threshold_pct = 5)
  # table output, CSV, notes
  check("the table renders with 3 significant digits", grepl("Duration", output$phases_table, fixed = TRUE) && grepl("exponential", output$phases_table, fixed = TRUE))
  csv <- utils::read.csv(output$downloadPhasesCSV)
  check("CSV: the unformatted table (duration columns included)",
        nrow(csv) == nrow(phases_current()) && all(c("sample", "phase", "t_start", "t_end", "od_start", "od_end", "duration") %in% names(csv)))
  check("the note says where the edges come from", grepl("curve metrics", as.character(output$phases_note$html), fixed = TRUE))
  # fixed panel mode
  session$setInputs(panel_size_mode = "fixed", panel_width_mm = 65, panel_height_mm = 65)
  g <- analysis_fixed_info()
  session$setInputs(analysis_type = "phases")
  gf <- analysis_fixed_info()
  check("fixed panel mode: a gtable at a computed size", inherits(gf, "gtable") && length(attr(gf, "size_mm")) == 2)
  session$setInputs(panel_size_mode = "fit")
})

cat("\n--- server: bootstrap ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1],
                    analysis_metrics = c("lysis_time", "od_drop", "mu_max"))
  check("nothing is cached until the button is pressed", is.null(boot_cache()) && is.null(boot_valid()))
  check("the note invites to run it", grepl("Run bootstrap", as.character(output$boot_note$html), fixed = TRUE))
  # the metric summary with the bootstrap source before any run: falls back (and says so)
  session$setInputs(metric_error_source = "bootstrap")
  p0 <- generate_analysis_plot("metric_summary")
  check("bootstrap source with no result: falls back to the replicate spread, still a real figure", !is_placeholder_plot(p0))
  rep_err <- ggplot_build(p0)$data[[1]]
  session$setInputs(metric_error_source = "replicates")
  # the button path
  session$setInputs(boot_n = 1000, boot_seed = 1, boot_metrics = env$APP_DEFAULTS$boot_metrics)
  { run_bootstrap_now(); session$flushReact() }
  bt <- boot_valid()
  check("run_bootstrap_now() caches a table: sample, metric, estimate, CI, n replicates, n ok draws",
        !is.null(bt) && all(c("sample", "metric", "estimate", "ci_low", "ci_high", "n_replicates", "n_boot_ok") %in% names(bt)))
  ph_bt <- bt[bt$metric == "lysis_time" & grepl("^Phage", bt$sample), ]
  check("the phage samples' lysis_time have finite CIs that contain the estimate",
        nrow(ph_bt) == 2 && all(is.finite(ph_bt$ci_low) & is.finite(ph_bt$ci_high)) &&
        all(ph_bt$ci_low <= ph_bt$estimate + 1e-9 & ph_bt$estimate <= ph_bt$ci_high + 1e-9) && all(ph_bt$n_boot_ok == 1000))
  check("the uninfected control never lyses: lysis_time is NA with 0 ok draws",
        all(is.na(bt$ci_low[bt$metric == "lysis_time" & bt$sample == smp[1]])) && all(bt$n_boot_ok[bt$metric == "lysis_time" & bt$sample == smp[1]] == 0))
  check("infection_strength (reference-relative) is not bootstrapped; the note says so",
        !"infection_strength" %in% bt$metric && identical(boot_cache()$dropped, "infection_strength") &&
        grepl("Not bootstrapped", as.character(output$boot_note$html), fixed = TRUE))
  check("the table renders and the CSV is the unformatted table",
        grepl("CI low", output$boot_table, fixed = TRUE) && grepl("lysis_time|Lysis onset", output$boot_table) &&
        nrow(utils::read.csv(output$downloadBootCSV)) == nrow(bt))
  # reproducible
  b_a <- boot_cache()$table; { run_bootstrap_now(); session$flushReact() }
  check("same seed, same result", isTRUE(all.equal(b_a, boot_cache()$table)))
  session$setInputs(boot_seed = 2); { run_bootstrap_now(); session$flushReact() }
  check("another seed gives other draws (the bootstrap means move; a percentile of 10 distinct resamples can coincide)",
        !isTRUE(all.equal(b_a$boot_mean, boot_cache()$table$boot_mean)))
  session$setInputs(boot_seed = 1, boot_n = 50); { run_bootstrap_now(); session$flushReact() }
  check("boot_n is clamped to 200..5000", boot_cache()$n_boot == 200L)
  session$setInputs(boot_n = 99999); { run_bootstrap_now(); session$flushReact() }
  check("... and to 5000 at the top", boot_cache()$n_boot == 5000L)
  session$setInputs(boot_n = 300, boot_metrics = "lysis_time"); { run_bootstrap_now(); session$flushReact() }
  check("a single metric: only that metric is cached", identical(unique(boot_cache()$table$metric), "lysis_time"))
  session$setInputs(boot_metrics = "infection_strength"); before <- boot_cache()
  { run_bootstrap_now(); session$flushReact() }
  check("only an unsupported metric: nothing is run, the earlier result stays", identical(boot_cache()$time, before$time) && identical(boot_cache()$metrics, "lysis_time"))
  # through the real button input
  session$setInputs(boot_n = 200, boot_seed = 3, boot_metrics = c("lysis_time", "od_drop"))
  session$setInputs(boot_run = 1)
  check("the Run bootstrap input triggers the run (seed 3, 200 draws, two metrics)",
        boot_cache()$seed == 3L && boot_cache()$n_boot == 200L && setequal(boot_cache()$metrics, c("lysis_time", "od_drop")))
  # the metric summary with bootstrap whiskers
  session$setInputs(boot_n = 1000, boot_seed = 1, boot_metrics = c("lysis_time", "od_drop", "mu_max"))
  { run_bootstrap_now(); session$flushReact() }
  session$setInputs(metric_error_source = "bootstrap")
  pb <- generate_analysis_plot("metric_summary")
  check("metric summary with metric_error_source = bootstrap renders", inherits(pb, "ggplot") && !is_placeholder_plot(pb) && !is.null(tryCatch(ggplot_build(pb), error = function(e) NULL)))
  bb <- ggplot_build(pb)
  lay <- vapply(pb$layers, function(l) class(l$geom)[1], "")
  eb <- bb$data[[which(lay == "GeomErrorbar")[1]]]; cb <- bb$data[[which(lay == "GeomCrossbar")[1]]]
  btl <- boot_valid(); lt <- btl[btl$metric == "lysis_time" & grepl("^Phage", btl$sample), ]
  # panel 1 (lysis time): whiskers are the CI, the bar the estimate
  p1 <- eb[eb$PANEL == 1 & is.finite(eb$ymin), ]; c1 <- cb[cb$PANEL == 1, ]
  check("the whiskers are the bootstrap CI and the bar the mean-curve estimate (lysis time panel)",
        nrow(p1) == 2 && isTRUE(all.equal(sort(p1$ymin), sort(lt$ci_low), tolerance = 1e-9)) &&
        isTRUE(all.equal(sort(p1$ymax), sort(lt$ci_high), tolerance = 1e-9)) &&
        isTRUE(all.equal(sort(c1$y[is.finite(c1$y)]), sort(lt$estimate), tolerance = 1e-9)))
  check("the replicate spread differs from the bootstrap whiskers (the figure really changed)",
        !isTRUE(all.equal(sort(rep_err$ymin), sort(eb$ymin[eb$PANEL == 1])) ))
  # a metric the cache does not hold keeps the replicate spread
  session$setInputs(analysis_metrics = c("lysis_time", "auc", "max_od"))
  pm <- generate_analysis_plot("metric_summary")
  bm <- ggplot_build(pm); lm <- vapply(pm$layers, function(l) class(l$geom)[1], "")
  ebm <- bm$data[[which(lm == "GeomErrorbar")[1]]]
  check("a metric the cached bootstrap does not hold (max OD) still gets whiskers (replicate spread)",
        !is_placeholder_plot(pm) && any(ebm$PANEL == 3 & is.finite(ebm$ymin)))
  session$setInputs(analysis_metrics = c("lysis_time", "od_drop", "mu_max"))
  # stale: the data / filters change
  check("the cache is valid for the data it was run on", !is.null(boot_valid()))
  session$setInputs(selected_samples = smp[2:3])
  check("a different sample selection invalidates it (a notice, no figure / report use)",
        is.null(boot_valid()) && !is.null(boot_cache()) && grepl("Out of date", as.character(output$boot_note$html), fixed = TRUE))
  session$setInputs(selected_samples = smp)
  check("... and it is valid again for the original selection", !is.null(boot_valid()))
  session$setInputs(enable_time_filter = TRUE, time_filter_range = c(0, 120))
  check("a time filter invalidates it", is.null(boot_valid()))
  session$setInputs(enable_time_filter = FALSE)
  check("removing the filter restores it", !is.null(boot_valid()))
  session$setInputs(metrics_smooth_window = 7)
  check("a metric setting (smoothing window) invalidates it", is.null(boot_valid()))
  session$setInputs(metrics_smooth_window = 5)
  # methods sentence
  check("the drafted methods mention the bootstrap once a current one exists",
        grepl("bootstrap", draft_methods_text(), fixed = TRUE))
  session$setInputs(metrics_smooth_window = 7)
  check("... and not when it is out of date", !grepl("bootstrap", draft_methods_text(), fixed = TRUE))
  session$setInputs(metrics_smooth_window = 5)
})

cat("\n--- server: separation from the reference ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1])
  p <- generate_analysis_plot("difference")
  check("difference renders a real figure with a reference",
        inherits(p, "ggplot") && !is_placeholder_plot(p) && !is.null(tryCatch(ggplot_build(p), error = function(e) NULL)))
  lay <- vapply(p$layers, function(l) class(l$geom)[1], ""); b <- ggplot_build(p)
  check("a dashed zero line, one curve per non-reference sample (the reference is the zero line)",
        "GeomHline" %in% lay && identical(p$layers[[which(lay == "GeomHline")]]$aes_params$linetype, "dashed") &&
        length(unique(b$data[[which(lay == "GeomLine")]]$group)) == 2)
  cd <- difference_current()
  check("the line is curve_difference(): mean minus the reference mean",
        isTRUE(all.equal(sort(b$data[[which(lay == "GeomLine")]]$y), sort(cd$diff[cd$sample != smp[1] & is.finite(cd$diff)]))) &&
        all(c("p_adj_holm", "stars") %in% names(cd)))
  mk <- b$data[[which(lay == "GeomPoint")[2]]]
  check("marks at the times with p_adj_holm < 0.05 only",
        nrow(mk) == sum(cd$p_adj_holm < 0.05, na.rm = TRUE) && nrow(mk) > 0)
  session$setInputs(diff_mark = FALSE)
  lay2 <- vapply(generate_analysis_plot("difference")$layers, function(l) class(l$geom)[1], "")
  check("diff_mark = FALSE: no marks", sum(lay2 == "GeomPoint") == 1)
  session$setInputs(diff_mark = TRUE, diff_mode = "log2_ratio")
  pl <- generate_analysis_plot("difference")
  cd2 <- difference_current()
  check("log2 ratio mode renders; the test is on log2 readings (the separation time is in the same family)",
        !is_placeholder_plot(pl) && isTRUE(all.equal(cd2$diff[cd2$sample == "Phage MOI 10" & cd2$time == 0],
                                                      log2(cd2$mean_value[cd2$sample == "Phage MOI 10" & cd2$time == 0] / cd2$ref_mean[cd2$sample == "Phage MOI 10" & cd2$time == 0]))))
  check("the y label names the ratio", grepl("log2", paste(as.character(pl$labels$y), collapse = " "), fixed = TRUE))
  session$setInputs(diff_mode = "difference")
  # the separation table
  fs <- separation_current()
  check("first_separation_time: the two phage samples have finite times, the reference NA",
        all(is.finite(fs$first_separation_time[grepl("^Phage", fs$sample)])) && nrow(fs) == 3 && is.na(fs$first_separation_time[fs$sample == smp[1]]))
  check("the higher MOI separates no later than the lower one (75 vs 120 min in the demo)",
        fs$first_separation_time[fs$sample == "Phage MOI 10"] <= fs$first_separation_time[fs$sample == "Phage MOI 0.1"])
  tb <- as.character(output$analysis_separation_table)
  check("the table under the figure lists the two phage samples (not the reference)",
        grepl("Phage MOI 10", tb, fixed = TRUE) && grepl("Phage MOI 0.1", tb, fixed = TRUE) && !grepl("Uninfected", tb, fixed = TRUE) &&
        grepl("First separation", tb, fixed = TRUE))
  check("the Statistics tab's Replicate comparison section has the same table",
        identical(as.character(output$stats_tab_separation_table), tb))
  # no reference
  session$setInputs(metrics_ref_sample = "")
  pn <- generate_analysis_plot("difference")
  check("without a reference: the placeholder with the instruction", is_placeholder_plot(pn) && grepl("reference", pn$layers[[1]]$aes_params$label, fixed = TRUE))
  check("without a reference: no separation table, a note instead",
        is.null(separation_current()) && grepl("reference", as.character(output$analysis_separation_note$html), fixed = TRUE))
  session$setInputs(metrics_ref_sample = smp[1])
})

cat("\n--- server: reports, bundle, export all ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1],
                    methods_text = "", compose_panels = c("main", "metric_summary"),
                    analysis_metrics = c("lysis_time", "od_drop"), report_include_analysis = TRUE,
                    boot_metrics = c("lysis_time", "od_drop"), boot_n = 200, boot_seed = 1, extract_threshold = NA)
  n_img <- function(h) lengths(regmatches(h, gregexpr("<img ", h, fixed = TRUE)))
  n_pages <- function(f) length(grepRaw("/Type /Page[^s]", readBin(f, "raw", file.size(f)), all = TRUE))
  if (env$has_base64enc && env$has_gridExtra) {
    meta <- report_meta(); meta$generated <- "2026-10-20 00:00:00"
    html <- html_report_string(meta)
    check("HTML report: a 'Curve phases' figure with the phases table right after it",
          grepl("<h3>Curve phases</h3>", html, fixed = TRUE) && grepl("<th>OD start</th>", html, fixed = TRUE) &&
          regexpr("<h3>Curve phases</h3>", html, fixed = TRUE) < regexpr("<th>OD start</th>", html, fixed = TRUE) &&
          regexpr("<th>OD start</th>", html, fixed = TRUE) < regexpr("<h3>Separation from the reference</h3>", html, fixed = TRUE))
    check("the phases section comes after the landmarks figure",
          regexpr("<h3>Curve landmarks</h3>", html, fixed = TRUE) < regexpr("<h3>Curve phases</h3>", html, fixed = TRUE))
    check("HTML report (reference set): the 'Separation' figure and the first-separation table",
          grepl("Separation from the reference", html, fixed = TRUE) && grepl("First separation from the reference", html, fixed = TRUE) &&
          grepl("First separation (min)", html, fixed = TRUE))
    check("no bootstrap run: no 'Bootstrap intervals' section", !grepl("Bootstrap intervals", html, fixed = TRUE))
    n_with <- n_img(html)
    { run_bootstrap_now(); session$flushReact() }
    html_b <- html_report_string(meta)
    check("a current bootstrap is a section of the report, with the CI columns",
          grepl("<h2>Bootstrap intervals</h2>", html_b, fixed = TRUE) && grepl("<th>CI low</th>", html_b, fixed = TRUE) &&
          regexpr("<h2>Replicate statistics</h2>", html_b, fixed = TRUE) < regexpr("<h2>Bootstrap intervals</h2>", html_b, fixed = TRUE))
    session$setInputs(metrics_ref_sample = "")
    html_n <- html_report_string(meta)
    check("no reference: no Separation figure or table (and the phases stay)",
          !grepl("Separation from the reference", html_n, fixed = TRUE) && grepl("<h3>Curve phases</h3>", html_n, fixed = TRUE) &&
          n_img(html_n) == n_with - 1L)   # the separation figure is the only image less (the normalized figure becomes the replicate overlay)
    session$setInputs(metrics_ref_sample = smp[1], report_include_analysis = FALSE)
    html_off <- html_report_string(meta)
    check("analysis off: none of the three", !grepl("Curve phases|Separation|Bootstrap intervals", html_off) && n_img(html_off) == 1)
    session$setInputs(report_include_analysis = TRUE)
    pdf_on <- tempfile(fileext = ".pdf"); pdf_report_file(pdf_on, meta)
    session$setInputs(report_include_analysis = FALSE)
    pdf_off <- tempfile(fileext = ".pdf"); pdf_report_file(pdf_off, meta)
    check("PDF report builds with the new pages (phases, separation, bootstrap)",
          identical(readBin(pdf_on, "raw", 4), charToRaw("%PDF")) && n_pages(pdf_on) >= n_pages(pdf_off) + 8)
    session$setInputs(report_include_analysis = TRUE)
  } else skipit("report tests need base64enc and gridExtra")

  have_zip <- requireNamespace("zip", quietly = TRUE)
  ls_zip <- function(zf) if (have_zip) basename(zip::zip_list(zf)$filename) else
    basename(sub("^\\s*[0-9]+\\s+[0-9-]+\\s+[0-9:]+\\s+", "", system2("unzip", c("-l", zf), stdout = TRUE)))
  { run_bootstrap_now(); session$flushReact() }
  zb <- output$downloadFigureBundle
  zl <- ls_zip(zb)
  check("figure bundle: phases.csv and bootstrap.csv", all(c("phases.csv", "bootstrap.csv", "curve_metrics.csv") %in% zl))
  exb <- tempfile("b_"); dir.create(exb); utils::unzip(zb, exdir = exb)
  pcsv <- utils::read.csv(list.files(exb, "^phases\\.csv$", recursive = TRUE, full.names = TRUE))
  check("phases.csv has the duration columns for every sample", all(c("duration", "t_start", "t_end") %in% names(pcsv)) && setequal(unique(pcsv$sample), smp))
  check("the README lists them", any(grepl("phases.csv", readLines(list.files(exb, "README.txt", recursive = TRUE, full.names = TRUE)), fixed = TRUE)))
  session$setInputs(metrics_smooth_window = 7)
  zl2 <- ls_zip(output$downloadFigureBundle)
  check("an out-of-date bootstrap is left out of the bundle (phases.csv stays)", "phases.csv" %in% zl2 && !"bootstrap.csv" %in% zl2)
  session$setInputs(metrics_smooth_window = 5)
  zf <- output$downloadAllFigures
  zla <- ls_zip(zf)
  check("export-all lists phases_*.svg / .png and difference_*.svg / .png",
        any(grepl("^phases_.*\\.svg$", zla)) && any(grepl("^difference_.*\\.svg$", zla)) &&
        any(grepl("^phases_.*\\.png$", zla)) && any(grepl("^difference_.*\\.png$", zla)))
  check("export-all: no SKIPPED.txt for the demo with a reference", !"SKIPPED.txt" %in% zla)
  session$setInputs(metrics_ref_sample = "")
  zlb <- ls_zip(output$downloadAllFigures)
  check("export-all without a reference: phases yes, difference skipped (listed in SKIPPED.txt)",
        any(grepl("^phases_.*\\.svg$", zlb)) && !any(grepl("^difference_", zlb)) && "SKIPPED.txt" %in% zlb)
  session$setInputs(metrics_ref_sample = smp[1])

  # composer
  session$setInputs(compose_panels = c("phases", "difference", "metric_summary"), compose_ncol = "2")
  ci <- compose_info()
  check("the composer takes both new panels (nothing skipped)", ci$n == 3 && length(ci$skipped) == 0 && all(c("phases", "difference") %in% ci$types))
})

cat("\n--- server: settings round trip ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, boot_metrics = c("lysis_time", "mu_max"), boot_n = 400, boot_seed = 7,
                    metric_error_source = "bootstrap", diff_mode = "log2_ratio", diff_mark = FALSE)
  sj <- settings_json_list()
  check("Save Settings carries the new global inputs",
        identical(sj$global_settings$boot_metrics, c("lysis_time", "mu_max")) && sj$global_settings$boot_n == 400 &&
        identical(sj$global_settings$metric_error_source, "bootstrap") && identical(sj$global_settings$diff_mode, "log2_ratio") &&
        identical(sj$global_settings$diff_mark, FALSE))
  check("... but not the Run bootstrap button", !"boot_run" %in% names(sj$global_settings))
})

cat(sprintf("\n============ SUMMARY: %d passed / %d failed / %d skipped ============\n", pass, fail, skip))
if (fail > 0) quit(status = 1)
