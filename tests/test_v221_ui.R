# ============================================================================
# v2.21 navigation and renaming: sticky figure, accordion sidebar with a jump menu, the quick-edit strip (name,
# colour, show per sample; axis labels and legend position), and sample display names carried into every table,
# CSV, report and sentence (the file's column name stays the key).
# Run from the repository root:  Rscript tests/test_v221_ui.R
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
check("APP_VERSION is 2.21.0 or later", package_version(env$APP_VERSION) >= "2.21.0")

cat("\n--- defaults ---\n")
d <- env$APP_DEFAULTS
check("sticky_figure and quick_edit_open are in APP_DEFAULTS, on", isTRUE(d$sticky_figure) && isTRUE(d$quick_edit_open))
nm <- names(env$LEGACY_BASE)
check("they sit immediately after demo_scenario (so a merge with new defaults at the end stays clean)",
      identical(nm[match("demo_scenario", nm) + 1:2], c("sticky_figure", "quick_edit_open")))
check("neither the journal preset nor the Legacy look touches them",
      !any(c("sticky_figure", "quick_edit_open") %in% names(env$legacy_preset_settings())) &&
      !any(c("sticky_figure", "quick_edit_open") %in% names(env$journal_preset_settings(89, 65))))

cat("\n--- UI: sticky container, accordion, jump menu, quick-edit strip ---\n")
rt <- htmltools::renderTags(env$ui)
ui_html <- gsub("> <", "><", gsub("[ \t\r\n]+", " ", paste(rt$head, rt$html)))   # head (CSS, scripts) + body, one line, single spaces
check("kc_main_class: sticky adds kc-sticky, off drops it",
      identical(env$kc_main_class(TRUE), "kc-main kc-sticky") && identical(env$kc_main_class(FALSE), "kc-main"))
check("the main panel keeps id main_panel and starts with the sticky class",
      grepl('<div id="main_panel" class="kc-main kc-sticky">', ui_html, fixed = TRUE))
check("the sticky checkbox is the first control of the sidebar, checked",
      grepl('<input id="sticky_figure" type="checkbox" class="shiny-input-checkbox" checked="checked"/>', ui_html, fixed = TRUE) &&
      regexpr('id="sticky_figure"', ui_html, fixed = TRUE) < regexpr('id="file"', ui_html, fixed = TRUE))
check("the sticky CSS: sticky, top 0, flex-start, max-height 100vh, own scrolling",
      grepl("position:sticky; top:0; align-self:flex-start; max-height:100vh; overflow-y:auto", ui_html, fixed = TRUE))
check("the layout is a flex row (sidebar 25 %), stacked and not sticky on narrow screens",
      grepl(".kc-layout { display:flex;", ui_html, fixed = TRUE) && grepl("flex:0 0 25%", ui_html, fixed = TRUE) &&
      grepl("@media (max-width: 767px)", ui_html, fixed = TRUE) && grepl(".kc-main.kc-sticky { position:static;", ui_html, fixed = TRUE))
check("the toggle is client-side: the page script flips kc-sticky on shiny:inputchanged for sticky_figure",
      grepl("ev.name === 'sticky_figure'", ui_html, fixed = TRUE) && grepl("classList.toggle('kc-sticky'", ui_html, fixed = TRUE))

secs <- regmatches(ui_html, gregexpr('<details class="kc-section" id="sec_[a-z_]+"><summary>[^<]*</summary>', ui_html))[[1]]
sec_titles <- sub('.*<summary>(.*)</summary>', "\\1", secs)
sec_ids    <- sub('.*id="sec_([a-z_]+)".*', "\\1", secs)
check("every sidebar section (13 + the v2.22 Panels section) is an accordion section, one per SIDEBAR_SECTIONS entry, in order",
      length(secs) == length(env$SIDEBAR_SECTIONS) && length(secs) == 14 && identical(sec_ids, unname(env$SIDEBAR_SECTIONS)) &&
      identical(sec_titles, htmltools::htmlEscape(names(env$SIDEBAR_SECTIONS))))
check("every sidebar <summary> is an accordion section (none left behind as a plain <details>)",
      length(gregexpr("<summary>", sub('(?s)<div id="main_panel".*', "", ui_html, perl = TRUE), fixed = TRUE)[[1]]) == 14)
check("the Statistics tab's sections stay independent (no kc-section class)",
      grepl('<details class="stats-section">', ui_html, fixed = TRUE) && !grepl('class="stats-section kc-section"', ui_html, fixed = TRUE))
blk  <- regmatches(ui_html, regexpr('<select[^>]*id="sidebar_jump".*?</select>', ui_html))
opts <- regmatches(blk, gregexpr('<option value="[^"]*"[^>]*>[^<]*</option>', blk))[[1]]
opt_val <- sub('<option value="([^"]*)".*', "\\1", opts); opt_lab <- sub('.*>([^<]*)</option>', "\\1", opts)
check("the jump menu lists every section title, with the section's id key as the value",
      length(blk) == 1 && identical(opt_lab[-1], sec_titles) && identical(opt_val[-1], sec_ids) && identical(opt_val[1], ""))
check("the jump menu is a native select (keyboard friendly) with a Collapse all button",
      !grepl("selectized", blk, fixed = TRUE) && grepl('class="btn btn-link btn-xs kc-collapse-all"', ui_html, fixed = TRUE))
check("accordion script: closes the other sections when one opens (toggle event, native keyboard behaviour kept)",
      grepl("addEventListener('toggle'", ui_html, fixed = TRUE) && grepl("closeOthers(d)", ui_html, fixed = TRUE) &&
      grepl("scrollIntoView({block: 'start'})", ui_html, fixed = TRUE) &&
      !grepl("preventDefault", sub("(?s).*?(function sections\\(\\).*?closeOthers\\(d\\).*?\\}\\)\\(\\);).*", "\\1", ui_html, perl = TRUE), fixed = TRUE))   # v2.22: the markup buttons' script (a separate block) uses preventDefault on purpose
check("no section is open at the start", !grepl("<details class=\"kc-section\"[^>]* open", ui_html))
check("the three moved widgets are static inputs in the Lysis curve tab, after the figure",
      all(vapply(c("x_axis_label", "y_axis_label", "legend_position", "quick_edit_open", "quick_edit_rows"),
                 function(id) grepl(paste0('id="', id, '"'), ui_html, fixed = TRUE), logical(1))) &&
      regexpr('id="od_plot"|id="plot_container"', ui_html) < regexpr('id="x_axis_label"', ui_html, fixed = TRUE) &&
      regexpr('id="x_axis_label"', ui_html, fixed = TRUE) > regexpr('id="main_panel"', ui_html, fixed = TRUE) &&
      lengths(regmatches(ui_html, gregexpr('id="x_axis_label"', ui_html, fixed = TRUE))) == 1)
check("the sidebar points to the strip where the labels and the legend position used to be",
      grepl("The X- and Y-axis labels are in the Quick edit strip", ui_html, fixed = TRUE) &&
      grepl("The legend position is in the Quick edit strip", ui_html, fixed = TRUE))
check("the strip collapses with a Hide quick edit button",
      grepl('id="kc_qe_toggle"', ui_html, fixed = TRUE) && grepl("Hide quick edit", ui_html, fixed = TRUE) &&
      grepl("input.quick_edit_open == true", ui_html, fixed = TRUE))
check("Variable Styling no longer carries the Legend Label box", !grepl("legend_label_", ui_html, fixed = TRUE))

cat("\n--- pure helpers ---\n")
mp <- c(A = "Wild type", B = "B", C = " ", D = "Mutant 1")
check("display_name_of: renamed, kept, blank and unknown names", identical(env$display_name_of(c("A", "B", "C", "Z"), mp), c("Wild type", "B", "C", "Z")))
check("display_name_of: no map = identity", identical(env$display_name_of(c("A", "B"), NULL), c("A", "B")))
df <- data.frame(sample = c("A", "D"), versus = c("D", "A"), x = 1:2, stringsAsFactors = FALSE)
w  <- env$with_display(df, mp)
check("with_display: display name in place, raw name right after it, per column",
      identical(names(w), c("sample", "sample_raw", "versus", "versus_raw", "x")) &&
      identical(w$sample, c("Wild type", "Mutant 1")) && identical(w$sample_raw, c("A", "D")) &&
      identical(w$versus, c("Mutant 1", "Wild type")) && identical(w$versus_raw, c("D", "A")))
check("with_display: twice is the same as once; drop_raw_columns restores the original columns",
      identical(env$with_display(w, mp), w) && identical(names(env$drop_raw_columns(w)), c("sample", "versus", "x")))
check("with_display: a table without the column, NULL and a 0-row table pass through",
      identical(env$with_display(data.frame(y = 1), mp), data.frame(y = 1)) && is.null(env$with_display(NULL, mp)) &&
      identical(names(env$with_display(df[0, ], mp)), c("sample", "sample_raw", "versus", "versus_raw", "x")))
check("with_display: a *_display column is dropped (curve_metrics_current)",
      identical(names(env$with_display(data.frame(sample = "A", sample_display = "Wild type", y = 1), mp, "sample")), c("sample", "sample_raw", "y")))
check("relabel_text: whole names only, renamed samples only, longest first",
      identical(env$relabel_text("No test possible: A vs D; B and Abc.", mp), "No test possible: Wild type vs Mutant 1; B and Abc.") &&
      identical(env$relabel_text("x", NULL), "x") &&
      identical(env$relabel_text("A vs AB", c(A = "One", AB = "Two")), "One vs Two"))
check("relabel_text: names with regex characters and a backslash in the display name",
      identical(env$relabel_text("x (3) y", c("(3)" = "three")), "x three y") &&
      identical(env$relabel_text("a.b", c("a.b" = "C:\\x")), "C:\\x"))
check("relabel_comparison: Tukey 'A-B' labels", identical(env$relabel_comparison(c("D-A", "B-A", "Q-A"), mp), c("Mutant 1-Wild type", "B-Wild type", "Q-A")))
check("display_mapping_text: 'raw -> display' where renamed, the name where not",
      identical(env$display_mapping_text(c("A", "B"), mp), "A \u2192 Wild type, B"))
check("toggle_sample_selection: hide, show (plot order of the loaded samples), idempotent",
      identical(env$toggle_sample_selection(c("A", "B", "D"), c("A", "B", "C", "D"), "B", FALSE), c("A", "D")) &&
      identical(env$toggle_sample_selection(c("A", "D"), c("A", "B", "C", "D"), "B", TRUE), c("A", "B", "D")) &&
      identical(env$toggle_sample_selection(c("A", "D"), c("A", "B", "C", "D"), "D", TRUE), c("A", "D")) &&
      identical(env$toggle_sample_selection(NULL, c("A", "B"), "B", TRUE), "B"))
check("moi_map_with_display: the raw name wins; the display name fills a gap",
      identical(unname(env$moi_map_with_display(c("Phage MOI 1", "S1", "S2"), "", c(`Phage MOI 1` = "x MOI 5", S1 = "Phage MOI 10", S2 = "S2"))), c(1, 10, NA)))

cat("\n--- server: renaming ---\n")
testServer(app, {
  si <- function(vals, ...) do.call(session$setInputs, c(vals, list(...)))
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars                                         # Uninfected, Phage MOI 0.1, Phage MOI 10 (file order)
  ref <- smp[grepl("^Uninfected", smp)][1]
  mid <- smp[grepl("0\\.1$", smp)][1]
  hi  <- smp[grepl("MOI 10$", smp)][1]
  sid <- env$safe_id
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = ref,
                    extract_times = "30, 60", extract_threshold = 0.2)
  check("before any rename: no display names differ from the raw names", all(rv$display_names == names(rv$display_names)) || is.null(rv$display_names))
  m0 <- curve_metrics_current()
  check("before any rename: curve_metrics_current() is exactly the engine table (no extra column)",
        !"sample_display" %in% names(m0) && "sample" %in% names(m0))

  si(setNames(list("Wild type"), paste0("qe_name_", sid(ref))))
  check("the quick-edit name box fills rv$display_names (raw name -> display name)",
        identical(unname(rv$display_names[ref]), "Wild type") && identical(unname(rv$display_names[hi]), hi))
  m1 <- curve_metrics_current()
  check("curve_metrics_current(): `sample` stays the raw column name, `sample_display` carries the new name",
        "sample_display" %in% names(m1) && identical(m1$sample, m0$sample) && identical(m1$sample_display[m1$sample == ref], "Wild type") &&
        identical(m1$sample_display[m1$sample == hi], hi))
  pm1 <- present(m1, "sample")
  check("present(): the display name in `sample`, the raw name right next to it in `sample_raw`",
        identical(names(pm1)[1:2], c("sample", "sample_raw")) && "Wild type" %in% pm1$sample && ref %in% pm1$sample_raw && !ref %in% pm1$sample)
  check("the raw name is still the key: selected_samples, col_map, the plotted data",
        identical(input$selected_samples, smp) && ref %in% names(rv$col_map) && !"Wild type" %in% names(rv$col_map) &&
        ref %in% prepare_plot_data()$variable && !"Wild type" %in% prepare_plot_data()$variable)

  av <- resolve_aesthetics(smp)
  check("figure legends: the legend labels are the display names", identical(unname(av$leg_labels[ref]), "Wild type") && identical(unname(av$leg_labels[hi]), hi))
  p <- generate_plot()
  lab <- tryCatch(ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")$get_labels(), error = function(e) character(0))
  check("the drawn plot's colour scale carries the display name", "Wild type" %in% lab && !ref %in% lab)
  lg <- figure_legend_string()
  check("the figure legend text names the sample as the user named it", grepl("Wild type", lg, fixed = TRUE) && !grepl(ref, lg, fixed = TRUE))
  mt <- draft_methods_text()
  check("the methods text names the samples and the reference by display name",
        grepl("Wild type", mt, fixed = TRUE) && !grepl(ref, mt, fixed = TRUE))

  # CSV files
  csv_of <- function(f) { tf <- tempfile(fileext = ".csv"); f(tf); read.csv(tf, check.names = FALSE, stringsAsFactors = FALSE) }
  sc <- csv_of(stats_csv_content)
  check("summary statistics CSV: sample (display) and sample_raw (file's column name) side by side",
        identical(names(sc)[1:3], c("time", "sample", "sample_raw")) && "Wild type" %in% sc$sample && ref %in% sc$sample_raw &&
        all(sc$sample_raw[sc$sample == "Wild type"] == ref))
  check("stats_table_export() itself keeps the old columns and the raw names",
        identical(colnames(stats_table_export()), c("time", "sample", "mean", "sd", "n", "sem", "ci95_half_width")) && ref %in% stats_table_export()$sample)
  mc <- csv_of(metrics_csv_content)
  check("curve metrics CSV: sample = display, sample_raw = raw, no sample_display left over",
        identical(names(mc)[1:2], c("sample", "sample_raw")) && !"sample_display" %in% names(mc) && "Wild type" %in% mc$sample && ref %in% mc$sample_raw)
  rc <- csv_of(repmetrics_csv_content)
  check("per-replicate metrics CSV: sample + sample_raw", identical(names(rc)[1:2], c("sample", "sample_raw")) && "Wild type" %in% rc$sample)
  ac <- readLines({ tf <- tempfile(fileext = ".csv"); analysis_stats_csv_content(tf); tf })
  check("replicate statistics CSV (one rectangular long table): the reference in both forms, in info rows",
        any(grepl("reference: Wild type", ac, fixed = TRUE)) && any(grepl(paste0("reference_raw: ", ref), ac, fixed = TRUE)))
  ex <- extraction_tables_current()
  check("extraction tables: sample + sample_raw in the summary and the per-replicate table",
        length(ex) == 4 && all(vapply(ex, function(x) identical(names(x)[names(x) %in% c("sample", "sample_raw")], c("sample", "sample_raw")), logical(1))) &&
        "Wild type" %in% ex[["od_at_times_summary.csv"]]$sample)
  ph <- present(phases_current(), "sample")
  check("phases: sample + sample_raw", identical(names(ph)[1:2], c("sample", "sample_raw")) && "Wild type" %in% ph$sample && ref %in% ph$sample_raw)
  fr <- present(fit_table_current(), "sample")
  check("fit table: sample + sample_raw", identical(names(fr)[1:2], c("sample", "sample_raw")) && "Wild type" %in% fr$sample)
  am <- present(all_metrics_current()$table)
  check("all-metrics comparison: sample, sample_raw, versus, versus_raw", all(c("sample_raw", "versus_raw") %in% names(am)) && any(am$versus == "Wild type"))
  run_bootstrap_now()
  bt <- present(boot_valid(), "sample")
  check("bootstrap: sample + sample_raw", !is.null(bt) && identical(names(bt)[1:2], c("sample", "sample_raw")) && "Wild type" %in% bt$sample)
  sp <- env$map_name_column(env$separation_display_table(separation_current(), ref, "min"), "Sample", rv$display_names)
  check("separation table: the reference row is left out by its raw name, the others show display names",
        !is.null(sp) && !"Wild type" %in% sp$Sample && all(sp$Sample %in% smp))

  # on-screen tables: display names, no *_raw columns
  stv <- stats_pairwise_build()
  check("replicate statistics on screen: no sample_raw column", !any(grepl("_raw$", names(stv))))
  st_d <- analysis_stats_display()
  check("analysis_stats_display(): raw names kept in sample_raw / versus_raw", all(c("sample_raw", "versus_raw") %in% names(st_d$pairwise)))
  check("Statistics tab tables carry the display name", "Wild type" %in% stats_tab_summary_df()$sample && !"sample_raw" %in% names(stats_tab_summary_df()))

  # settings
  sj <- settings_json_list()
  check("settings file: the saved legend_label is the display name, keyed by the raw name",
        identical(sj$sample_aesthetics[[ref]]$legend_label, "Wild type") && identical(sj$sample_aesthetics[[hi]]$legend_label, hi))
  check("settings file: UI-only inputs (jump menu, quick-edit boxes) are not saved; the sticky toggle is",
        !any(grepl("^(sidebar_jump|qe_)", names(sj$global_settings))) && "sticky_figure" %in% names(sj$global_settings) &&
        "quick_edit_open" %in% names(sj$global_settings))

  # report provenance
  if (env$has_base64enc) {
    session$setInputs(report_include_analysis = FALSE)
    meta <- report_meta(); meta$generated <- "2026-11-01 00:00:00"
    html <- html_report_string(meta)
    check("HTML report provenance lists the mapping 'raw column name -> display name'",
          grepl(paste0(ref, " \u2192 Wild type"), html, fixed = TRUE))
    check("HTML report tables use the display name and have no *_raw columns",
          grepl("<td>Wild type</td>", html, fixed = TRUE) && !grepl("_raw", html, fixed = TRUE))
  } else skipit("HTML report provenance (base64enc missing)")

  # every analysis figure still builds after a rename (colours are keyed by the raw name, labels show the display name)
  if (TRUE) {
    fig_ok <- function() vapply(unname(env$ANALYSIS_FIGURE_CHOICES), function(ty)
      tryCatch(!is_placeholder_plot(generate_analysis_plot(ty)), error = function(e) FALSE), logical(1))
    ok_renamed <- fig_ok()
    si(setNames(list(""), paste0("qe_name_", sid(ref))))
    ok_plain <- fig_ok()
    si(setNames(list("Wild type"), paste0("qe_name_", sid(ref))))
    check("every analysis figure that renders with the file's names renders after a rename (and vice versa)",
          identical(ok_renamed, ok_plain) && sum(ok_renamed) >= 8)
    ph <- generate_analysis_plot("phenotype_heatmap")
    check("phenotype heatmap rows and the metric-summary legend use the display name",
          "Wild type" %in% levels(ph$data$sample) && !ref %in% levels(ph$data$sample))
    lab_ms <- tryCatch(ggplot2::ggplot_build(generate_analysis_plot("replicate_overlay"))$plot$scales$get_scales("colour")$get_labels(), error = function(e) character(0))
    check("replicate overlay: colour key shows the display name", "Wild type" %in% lab_ms && !ref %in% lab_ms)
  }
  if (env$has_base64enc) {
    session$setInputs(report_include_analysis = TRUE)
    html_a <- html_report_string(meta)
    n_raw <- lengths(regmatches(html_a, gregexpr(ref, html_a, fixed = TRUE)))
    n_map <- lengths(regmatches(html_a, gregexpr(paste0(ref, " \u2192 Wild type"), html_a, fixed = TRUE)))
    check("HTML report with the analysis section: the raw name appears only in the 'raw -> display' provenance lines",
          n_raw >= 1 && n_raw == n_map && grepl("Wild type", html_a, fixed = TRUE))
    session$setInputs(report_include_analysis = FALSE)
  }

  # name edge cases
  si(setNames(list("   "), paste0("qe_name_", sid(ref))))
  check("a blank name box means the raw name again", identical(unname(rv$display_names[ref]), ref) && !"sample_display" %in% names(curve_metrics_current()))
  si(setNames(list("Wild type"), paste0("qe_name_", sid(ref))))

  # MOI from the display name
  si(setNames(list("Control MOI 5"), paste0("qe_name_", sid(ref))), analysis_type = "dose_response")
  mm <- moi_map(smp, "")
  check("MOI: raw names are parsed first; a display name only fills a sample whose raw name has none",
        isTRUE(unname(mm[mid]) == 0.1) && isTRUE(unname(mm[ref]) == 5))
  si(setNames(list("Wild type"), paste0("qe_name_", sid(ref))))
})

cat("\n--- server: quick-edit colour and show ---\n")
testServer(app, {
  si <- function(vals, ...) do.call(session$setInputs, c(vals, list(...)))
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars; sid <- env$safe_id
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1])
  def_col <- resolve_aesthetics(smp)$colors
  s2 <- smp[2]
  si(setNames(list("#112233"), paste0("qe_color_", sid(s2))))
  check("a colour picked in the strip becomes the sample's colour (and only that sample's)",
        identical(unname(resolve_aesthetics(smp)$colors[s2]), "#112233") && identical(unname(resolve_aesthetics(smp)$colors[smp[1]]), unname(def_col[smp[1]])))
  check("the strip's colour is what Save Settings writes", identical(settings_json_list()$sample_aesthetics[[s2]]$color, "#112233"))
  si(setNames(list("custom", "#FF0000"), c(paste0("color_selector_", sid(s2)), paste0("color_", sid(s2)))))
  check("a different colour chosen in the sidebar wins again (one source of truth, the later edit)",
        identical(unname(resolve_aesthetics(smp)$colors[s2]), "#FF0000") && is.null(rv$qe_colors[[s2]]))
  si(setNames(list("#00ff00"), paste0("qe_color_", sid(s2))))
  check("and the strip can override the sidebar colour again", identical(unname(resolve_aesthetics(smp)$colors[s2]), "#00FF00"))

  check("show: with no strip box touched, the selection is the select box's",
        setequal(quick_edit_selection(), smp))
  si(setNames(list(FALSE), paste0("qe_show_", sid(smp[3]))))
  check("show unticked: the sample leaves the selection", identical(quick_edit_selection(), smp[1:2]))
  si(setNames(list(TRUE), paste0("qe_show_", sid(smp[3]))))
  check("show ticked: the sample is back, in the plot order", identical(quick_edit_selection(), smp))
  si(setNames(list(FALSE, FALSE), paste0("qe_show_", sid(smp[1:2]))))
  check("several boxes at once", identical(quick_edit_selection(), smp[3]))
})

cat("\n--- v2.22.1: Help tab (the docs/ guides rendered in the app) ---\n")
tabs <- sub('data-value="(.*)"', "\\1", regmatches(ui_html, gregexpr('data-value="[^"]+"', ui_html))[[1]])
check("the Help tab is the last main tab, after Curve fitting",
      "Help" %in% tabs && match("Help", tabs) > match("Curve fitting", tabs))
check("it opens with the labelling summary", grepl("How to label a figure", ui_html, fixed = TRUE) &&
      grepl("Sample names", ui_html, fixed = TRUE) && grepl("Panel letters", ui_html, fixed = TRUE))
check("the guides are a server output, so no document text sits in the static page",
      grepl('id="help_guides"', ui_html, fixed = TRUE) && !grepl("kc-help-doc", ui_html, fixed = TRUE))
hh <- gsub("[ \t\r\n]+", " ", htmltools::renderTags(env$help_guides_ui())$html)
testServer(app, {
  session$setInputs(main_tabs = "Help")
  check("output help_guides renders the same sections", identical(gsub("[ \t\r\n]+", " ", output$help_guides$html), hh))
})
check("the labelling tutorial is rendered from docs/LABELLING_HOWTO.md and open by default",
      grepl('<details class="kc-help-doc" id="help_LABELLING_HOWTO" open>', hh, fixed = TRUE) &&
      grepl("ten-minute walkthrough", hh, fixed = TRUE) && grepl("Common mistakes", hh, fixed = TRUE))
check("the user guide, figure standards, metrics reference, README and changelog are sections too (closed)",
      all(vapply(c("help_USER_GUIDE", "help_FIGURE_STANDARDS", "help_METRICS_REFERENCE", "help_README", "help_CHANGELOG"),
                 function(id) grepl(paste0('<details class="kc-help-doc" id="', id, '">'), hh, fixed = TRUE), logical(1))))
check("tables of the tutorial are rendered as HTML tables (commonmark GitHub extension)",
      !requireNamespace("commonmark", quietly = TRUE) || grepl("<th>Symptom</th>", hh, fixed = TRUE))
check("images under docs/ are served from the kc_docs resource path",
      grepl('src="kc_docs/gallery/main.png"', hh, fixed = TRUE) && !grepl('src="gallery/', hh, fixed = TRUE))
check("a link from one guide to another jumps to that guide's section; the README link too",
      grepl('href="#help_USER_GUIDE"', hh, fixed = TRUE) && grepl('href="#help_README"', hh, fixed = TRUE) &&
      !grepl('href="kc_docs/USER_GUIDE.md"', hh, fixed = TRUE))
check("the guide's own h1 is dropped (the section summary carries the title)",
      !grepl("<h1>How to label a figure</h1>", hh, fixed = TRUE))
check("a missing guide is skipped, not an error", !grepl("Could not read", hh, fixed = TRUE))
old_opt <- options(killcurve.docs_dir = tempfile("nodocs_")); hh_none <- htmltools::renderTags(env$help_guides_ui())$html; options(old_opt)
check("without a docs/ folder the tab says so instead of failing", grepl("was not found next to the app", hh_none, fixed = TRUE))
old_opt <- options(killcurve.docs_dir = "/tmp/x"); dd_opt <- env$docs_dir_path(); options(old_opt)
check("docs_dir_path(): the option wins, else docs/ under the working directory",
      identical(dd_opt, "/tmp/x") && identical(env$docs_dir_path(), file.path(getwd(), "docs")))
tmpd <- tempfile("help_"); dir.create(file.path(tmpd, "docs"), recursive = TRUE)
writeLines(c("# T", "", "See [the guide](USER_GUIDE.md#x) and ![i](gallery/p.png).", "", "| a | b |", "|---|---|", "| 1 | 2 |"),
           file.path(tmpd, "docs", "LABELLING_HOWTO.md"))
h <- env$render_help_doc(file.path(tmpd, "docs", "LABELLING_HOWTO.md"), list("USER_GUIDE.md" = "help_USER_GUIDE"), "docs")
check("render_help_doc: anchors (with a fragment) rewritten, images prefixed, h1 dropped",
      grepl('href="#help_USER_GUIDE"', h, fixed = TRUE) && grepl('src="kc_docs/gallery/p.png"', h, fixed = TRUE) &&
      !grepl("<h1>", h, fixed = TRUE))

cat(sprintf("\n============ SUMMARY: %d passed / %d failed / %d skipped ============\n", pass, fail, skip))
if (fail > 0) quit(status = 1)
