# ============================================================================
# v2.22: the Compare tab, the "N4 paper look" preset, old settings import (N4 flat JSON, lysis-curve-app structure),
# project themes, notes on the figure and notes exports, batch export options, parity frames for PowerPoint / GIF,
# Welch + Wilcoxon side by side, night mode.
# Run from the repository root:  Rscript tests/test_v222_compare.R
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
check("APP_VERSION is 2.21.0 or later (this suite does not bump it)", package_version(env$APP_VERSION) >= "2.21.0")
seed_defaults <- function(session) do.call(session$setInputs, env$APP_DEFAULTS)
out_dir <- tempfile("v222_"); dir.create(out_dir)
of <- function(x) file.path(out_dir, x)
near <- function(a, b, tol = 0.25) all(abs(a - b) <= tol)

# ── defaults and UI ─────────────────────────────────────────────────────────
cat("\n--- defaults, choices, UI ---\n")
d <- env$APP_DEFAULTS
nm <- names(env$LEGACY_BASE)
new_keys <- c("compare_label_a", "compare_label_b", "compare_mode", "compare_overlay_style", "compare_alpha_b", "compare_common_only",
              "theme_autoapply", "notes_show_caption", "notes_caption_fields", "notes_caption_size", "notes_pptx_slide",
              "batch_outputs", "batch_format", "batch_dpi", "batch_size_mode", "batch_width_mm", "batch_height_mm", "night_mode")
check("the v2.22 defaults are the LAST entries of the default list (after lysed_threshold), in order",
      identical(tail(nm, length(new_keys) + 2)[1:2], c("lysed_mode", "lysed_threshold")) && identical(tail(nm, length(new_keys)), new_keys))
check("journal startup defaults are unchanged (89 x 65 mm, SVG, Arial 6-8 pt, inside-auto legend, Okabe-Ito)",
      d$export_units == "mm" && d$export_width == 89 && d$export_height == 65 && d$export_format == "svg" && d$font_family == "Arial" &&
      d$axis_text_font_size == 6 && d$axis_label_font_size == 7 && d$legend_position == "inside-auto" && d$color_palette == "colorblind" &&
      d$panel_size_mode == "fixed" && d$y_oob == "squish")
check("neither the journal preset nor the Legacy look carries a v2.22 key",
      !any(new_keys %in% names(env$legacy_preset_settings())) && !any(new_keys %in% names(env$journal_preset_settings(89, 65))))
check("legacy_preset_settings() still covers exactly the journal preset's inputs",
      setequal(names(env$legacy_preset_settings()), names(env$journal_preset_settings(89, 65))))
check("v2.22 defaults: compare overlay by line type, notes caption off, batch = main figure only, night mode off",
      identical(d$compare_mode, "overlay") && identical(d$compare_overlay_style, "linetype") && isFALSE(d$notes_show_caption) &&
      identical(d$batch_outputs, "main") && isFALSE(d$night_mode) && isTRUE(d$theme_autoapply))
check("the Compare figure is a composer panel choice",
      "compare" %in% env$COMPOSE_PANEL_CHOICES && !("compare" %in% env$ANALYSIS_FIGURE_CHOICES))
rt <- htmltools::renderTags(env$ui)
ui_html <- gsub("> <", "><", gsub("[ \t\r\n]+", " ", paste(rt$head, rt$html)))
has_id <- function(id) grepl(paste0('id="', id, '"'), ui_html, fixed = TRUE)
check("UI: the Compare tab comes after Compose figure and has its inputs and outputs",
      regexpr('data-value="Compose figure"', ui_html, fixed = TRUE) < regexpr('data-value="Compare"', ui_html, fixed = TRUE) &&
      all(vapply(c("compare_file", "compare_label_a", "compare_label_b", "compare_mode", "compare_overlay_style", "compare_samples_b",
                   "compare_common_only", "compare_status", "compare_container", "downloadCompare"), has_id, logical(1))))
check("UI: the N4 paper look button sits beside the Legacy look button",
      has_id("apply_n4_look") && has_id("apply_legacy_look") &&
      abs(regexpr('id="apply_n4_look"', ui_html, fixed = TRUE) - regexpr('id="apply_legacy_look"', ui_html, fixed = TRUE)) < 600)
check("UI: project themes, notes caption, notes downloads, batch export and night mode controls exist",
      all(vapply(c("active_theme", "theme_autoapply", "theme_name", "save_theme", "apply_theme", "delete_theme", "notes_show_caption",
                   "notes_caption_fields", "notes_caption_size", "notes_pptx_slide", "download_notes_pdf", "download_notes_html",
                   "download_notes_json", "download_notes_png", "batch_outputs", "batch_format", "batch_dpi", "batch_size_mode",
                   "downloadBatchExport", "night_mode"), has_id, logical(1))))
check("UI: stats_method offers Welch + Wilcoxon side by side (value both)",
      grepl('value="both"', ui_html, fixed = TRUE) && grepl("Welch + Wilcoxon side by side", ui_html, fixed = TRUE))
check("UI: night mode is a page class (kcNight handler + body.kc-night CSS), not a figure setting",
      grepl("kcNight", ui_html, fixed = TRUE) && grepl("body.kc-night", ui_html, fixed = TRUE))
check("batch export offers the main figure, every analysis figure, the composed and compare figures, CSVs and notes",
      all(c("main", paste0("analysis:", env$ANALYSIS_FIGURE_CHOICES), "composed", "compare", "csv_summary", "csv_replicate_stats",
            "csv_metrics", "csv_raw_wide", "notes") %in% unlist(env$batch_output_choices())))

# ── N4 paper look ───────────────────────────────────────────────────────────
cat("\n--- N4 paper look ---\n")
n4 <- env$n4_preset_settings()
check("n4_preset_settings(): no duplicate keys", !anyDuplicated(names(n4)))
check("N4 text: axis text 20, axis labels 20, title 20; stroke 1.5; line 1; point size 3",
      n4$axis_text_font_size == 20 && n4$axis_label_font_size == 20 && n4$title_font_size == 20 && n4$point_stroke == 1.5 &&
      n4$line_thickness == 1 && n4$shape_size == 3)
check("N4 end labels: 20 ggplot-mm = 57 pt here; legend text 11.2 pt (theme_prism 0.8 x 14, read off the old app)",
      n4$label_font_size == 57 && n4$legend_font_size == 11.2)
check("N4: no error caption, y values below the floor squished, legend on the right, alphabetical legend order",
      isFALSE(n4$show_error_caption) && n4$y_oob == "squish" && n4$legend_position == "right" && n4$legend_order == "alpha")
check("N4 export: PDF, 10 x 8 in, 300 dpi, pixel canvas of 800 px, panel size mode fit",
      n4$export_format == "pdf" && n4$export_units == "in" && n4$export_width == 10 && n4$export_height == 8 && n4$export_dpi == 300 &&
      n4$preview_mode == "canvas" && n4$plot_width == 800 && n4$panel_size_mode == "fit")
check("N4 drawing switches: theme spacing 0, plain axis labels, no log floor, absolute caps, sample-coloured bars, bars not ribbon",
      n4$theme_base_pt == 0 && isFALSE(n4$axis_subscripts) && isFALSE(n4$error_log_floor) && isFALSE(n4$error_cap_relative) &&
      n4$error_bar_color == "sample" && n4$error_display_mode == "bars" && n4$y_tick_label_style == "legacy")
check("N4 ticks: x every 30, log y 0.01 to 10", n4$x_tick_interval == 30 && n4$y_log_min_exponent == -2 && n4$y_log_max_exponent == 0)
check("N4 preset covers every journal-preset input (so it can replace the journal look completely)",
      all(names(env$journal_preset_settings(89, 65)) %in% names(n4)))
check("the N4 preset differs from the Legacy look where the N4 app did (axis text 20 vs 16, stroke 1.5 vs 0.5, caption off vs on)",
      env$legacy_preset_settings()$axis_text_font_size == 16 && env$legacy_preset_settings()$point_stroke == 0.5 &&
      isTRUE(env$legacy_preset_settings()$show_error_caption))

# ── Old settings files: pure translation ───────────────────────────────────
cat("\n--- old settings import: translation ---\n")
n4_json  <- jsonlite::fromJSON("tests/v222_n4_settings.json", simplifyVector = FALSE)
lca_json <- jsonlite::fromJSON("tests/v222_lca_settings.json", simplifyVector = FALSE)
ids <- unique(c(names(env$APP_DEFAULTS), "x_tick_interval", "x_min", "x_max", "y_min", "y_max", "nb_date", "plot_width",
                "region_count", "marker_count", "aspect_ratio"))
check("fixture: the N4 file is the flat input list with every scalar written as a one-element array",
      is.list(n4_json$x_scale_type) && length(n4_json$x_scale_type) == 1 && is.null(n4_json$global_settings))
check("format detection: n4, lca, current",
      identical(env$legacy_settings_kind(n4_json), "n4") && identical(env$legacy_settings_kind(lca_json), "lca") &&
      identical(env$legacy_settings_kind(list(global_settings = list(a = 1), sample_aesthetics = list())), "current"))
check("settings_unbox: [x] -> x, [a, b] -> c(a, b), [] -> NULL, nested object -> NULL",
      identical(env$settings_unbox(list(5)), 5) && identical(env$settings_unbox(list("a", "b")), c("a", "b")) &&
      is.null(env$settings_unbox(list())) && is.null(env$settings_unbox(list(name = list("x")))))
tr <- env$translate_old_settings(n4_json, c("WT", "Mutant", "Third"), ids)
g <- tr$s$global_settings
check("N4: matching input ids are kept and unboxed (scale, error type, fonts, labels, point size)",
      g$x_scale_type == "linear" && g$y_scale_type == "log" && g$error_type == "sd" && g$title_font_size == 22 &&
      g$x_axis_label == "Time (min)" && g$shape_size == 3.5 && isTRUE(g$custom_y_limits) && g$y_min == 0.02)
check("N4: x_major_step -> x_tick_interval", g$x_tick_interval == 60 && is.null(g$x_major_step))
check("N4: font 'helvetica' -> Helvetica", identical(g$font_family, "Helvetica"))
check("N4: inch sizes stay inches and the units are switched to inches (10 x 8 in stays 7 x 5 in)",
      identical(g$export_units, "in") && g$export_width == 7 && g$export_height == 5 && g$export_dpi == 600)
check("N4: controls without a counterpart are reported as ignored with a reason (minor interval, y ticks, line_type, OD filter ...)",
      all(c("x_minor_step", "y_major_step", "y_minor_step", "line_type", "error_cap_type", "use_od_filter", "od_min", "od_max",
            "plot_height", "exclude_times", "time_col_choice") %in% names(tr$ignored)) && all(nzchar(tr$ignored)))
check("N4: nothing leaks the positional widgets or the file input into the globals",
      !any(grepl("^(color_selector|shape|line_type|legend_label|exclude_var|use_rgb|hex_color)_[0-9]+$", names(g))) && is.null(g$file))
sa <- tr$s$sample_aesthetics
check("N4: positional keys land on the loaded samples in order (1 -> WT, 2 -> Mutant, 3 -> Third)",
      identical(names(sa), c("WT", "Mutant", "Third")) && sa$WT$color == "#000000" && sa$Mutant$color == "#E41A1C" && sa$Third$color == "#12AB34")
check("N4: custom colour comes from color_i, a named one from color_selector_i; shape and line type are mapped",
      sa$Third$color == "#12AB34" && sa$WT$shape == 16 && sa$Mutant$line_type == "dashed" && sa$Third$line_type == "dotted")
check("N4: the old open shapes become the closed shape with Filled off (0 -> square, 2 -> triangle)",
      sa$Mutant$shape == 15 && isFALSE(sa$Mutant$shape_filled) && sa$Third$shape == 17 && isFALSE(sa$Third$shape_filled) && is.null(sa$WT$shape_filled))
check("N4: a legend label becomes the display name; a blank one is not applied",
      sa$WT$legend_label == "N4 wild type" && sa$Mutant$legend_label == "Uninfected" && is.null(sa$Third$legend_label))
check("N4: exclude_var_3 = TRUE hides the third sample", identical(tr$unselect, "Third") && isTRUE(sa$Third$hidden))
check("N4: the count of sample settings is 14 (4 + 5 + 5)", tr$n_sample_settings == 14)
tr2 <- env$translate_old_settings(n4_json, c("WT", "Mutant"), ids)
check("N4: a different sample count warns and maps by position (the third key has no sample)",
      any(grepl("styles 3 samples but 2 are loaded", tr2$notes)) && identical(names(tr2$s$sample_aesthetics), c("WT", "Mutant")) &&
      "sample 3" %in% names(tr2$ignored))
tr3 <- env$translate_old_settings(n4_json, NULL, ids)
check("N4: with no data loaded the per-sample styles wait (positional list returned, none applied yet)",
      length(tr3$s$sample_aesthetics) == 0 && identical(names(tr3$positional), c("1", "2", "3")) && any(grepl("next data file", tr3$notes)))
check("N4: with known_ids given, a key that is not an input id is ignored, not applied",
      "x_tick_interval" %in% names(env$translate_old_settings(n4_json, c("a", "b", "c"), ids)$s$global_settings) &&
      is.null(env$translate_old_settings(n4_json, c("a", "b", "c"), setdiff(ids, "x_tick_interval"))$s$global_settings$x_tick_interval))
tl <- env$translate_old_settings(lca_json, c("Uninfected", "Phage MOI 10"), ids)
gl <- tl$s$global_settings
check("lysis-curve-app: global notes_* ids become nb_* (id, experimenter, host, MOI, observations, type)",
      gl$nb_exp_id == "EXP-042" && gl$nb_experimenter == "A. Researcher" && gl$nb_host_strain == "E. coli MG1655" &&
      gl$nb_moi == "10" && gl$nb_observations == "Clear lysis after 90 min" && gl$nb_exp_type == "Lysis Curve")
check("lysis-curve-app: notes_extra_cond -> nb_other_condition; the experiment_notes block fills the rest (project, plate key / value)",
      gl$nb_other_condition == "37 C shaking" && gl$nb_project == "Lysis kinetics" && gl$nb_custom_key1 == "Plate" && gl$nb_custom_val1 == "P7" &&
      gl$nb_next_steps == "Repeat at MOI 1")
check("lysis-curve-app: the notes caption inputs keep their ids (notes_show_caption, fields, size)",
      isTRUE(gl$notes_show_caption) && identical(gl$notes_caption_fields, c("exp_id", "host_strain", "moi")) && gl$notes_caption_size == 7)
check("lysis-curve-app: legend_label -> display name; shape / filled / line type kept",
      tl$s$sample_aesthetics$Uninfected$legend_label == "No phage" && tl$s$sample_aesthetics$`Phage MOI 10`$line_type == "dashed" &&
      isFALSE(tl$s$sample_aesthetics$`Phage MOI 10`$shape_filled))
check("lysis-curve-app: floating labels, panel and matrix-legend keys are ignored with a reason",
      all(c("floating_labels", "Uninfected$panel", "Uninfected$leg_row", "Phage MOI 10$leg_col") %in% names(tl$ignored)))
check("lysis-curve-app: axis frame, exact panel, legend title, facets and the unsupported legend / error modes are ignored",
      all(c("axis_frame", "exact_panel", "legend_title", "enable_facets", "legend_position", "error_display_mode") %in% names(tl$ignored)) &&
      is.null(gl$legend_position) && is.null(gl$error_display_mode))
check("lysis-curve-app: palette 'publication' (Okabe-Ito there) becomes 'colorblind', with a note",
      identical(gl$color_palette, "colorblind") && any(grepl("publication", tl$notes)))
check("a current-format file passes through untouched (kind current, same structure)",
      identical(env$translate_old_settings(list(global_settings = list(a = 1), sample_aesthetics = list()), NULL, ids)$kind, "current"))
check("the import message: 'N global, M sample settings applied, K ignored'",
      identical(env$legacy_import_message("n4", 47, 14, 18), "N4 app settings imported: 47 global, 14 sample settings applied, 18 ignored.") &&
      grepl("^Lysis Curve App settings imported: 3 global, 4 sample settings applied, 5 ignored\\.$", env$legacy_import_message("lca", 3, 4, 5)))

# ── Old settings import through the loader ─────────────────────────────────
testServer(app, {
  cat("\n--- old settings import: through the loader ---\n")
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  seed_defaults(session)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", x_tick_interval = NA, x_min = 0, x_max = 100,
                    y_min = 0.01, y_max = 2, nb_date = Sys.Date(), plot_width = 820, region_count = 1, marker_count = 1)
  smp <- rv$od_vars
  session$setInputs(loadSettings = list(name = "n4.json", datapath = file.path(getwd(), "tests/v222_n4_settings.json"), size = 1, type = "application/json"))
  li <- rv$last_import
  check("loader: the N4 file is recognised and the status says 'N global, M sample settings applied, K ignored'",
        identical(li$kind, "n4") && grepl("^N4 app settings imported: [0-9]+ global, 14 sample settings applied, [0-9]+ ignored\\.$", rv$settings_status$msg) &&
        identical(rv$settings_status$msg, li$message) && identical(rv$settings_status$type, "ok"))
  check("loader: the counts in the record add up (global applied > 30, ignored >= the 11 known)", li$n_global > 30 && li$n_ignored >= 11)
  check("loader: positional legend labels become display names of the first three samples in order",
        identical(unname(rv$display_names[smp]), c("N4 wild type", "Uninfected", "Phage MOI 0.1")))
  check("loader: styles are kept for the samples (colour of sample 3 is the custom hex)",
        identical(rv$imported_settings$samples[[smp[3]]]$color, "#12AB34") && identical(rv$imported_settings$samples[[smp[2]]]$shape, 15))
  check("loader: the sample the old file hid is reported in the record", identical(li$unselect, smp[3]))
  check("loader: inch sizes mark the units as inches (rv$units_seen)", identical(rv$units_seen, "in"))
  check("loader: the status details list the ignored controls and the notes",
        grepl("x_minor_step", rv$settings_status$details) && grepl("inches", rv$settings_status$details))
  session$setInputs(loadSettings = list(name = "lca.json", datapath = file.path(getwd(), "tests/v222_lca_settings.json"), size = 2, type = "application/json"))
  li <- rv$last_import
  check("loader: the lysis-curve-app file is recognised; legend labels become display names; floating labels are ignored",
        identical(li$kind, "lca") && identical(unname(rv$display_names["Uninfected"]), "No phage") && identical(unname(rv$display_names["Phage MOI 10"]), "MOI 10") &&
        "floating_labels" %in% li$ignored)
  check("loader: its notes are carried as nb_* settings (count in the record, no 'not a control' for nb_exp_id)",
        li$n_global >= 25 && !("nb_exp_id" %in% li$ignored))
  # a current-format file still gets the old message
  f <- of("current.json")
  jsonlite::write_json(list(version = "2.21.0", global_settings = list(line_thickness = 0.5), sample_aesthetics = list()), f, auto_unbox = TRUE)
  session$setInputs(loadSettings = list(name = "c.json", datapath = f, size = 3, type = "application/json"))
  check("loader: a current-format file keeps the usual status text", grepl("global setting\\(s\\) applied", rv$settings_status$msg))
  # a file loaded before any data: the positional styles wait for the data
  rv$od_vars <- NULL
  session$setInputs(loadSettings = list(name = "n4b.json", datapath = file.path(getwd(), "tests/v222_n4_settings.json"), size = 4, type = "application/json"))
  check("loader: with no data loaded the positional styles are queued (rv$pending_positional)", length(rv$pending_positional) == 3)
})

# ── Project themes ──────────────────────────────────────────────────────────
cat("\n--- project themes ---\n")
th_dir <- tempfile("themes"); options(killcurve.themes_dir = th_dir)
check("themes folder: the option wins, the folder is created on the first save",
      identical(env$themes_dir_path(), th_dir) && !dir.exists(th_dir) && length(env$theme_list()) == 0)
check("theme_key: spaces and symbols become underscores, nothing left for an empty name",
      identical(env$theme_key(" My lab / theme 1 "), "My_lab_theme_1") && !nzchar(env$theme_key("  ")))
aes_map <- list(WT = list(color = "#000000", shape = 16, shape_filled = TRUE, line_type = "solid", legend_label = "Wild type", group = "x"),
                Mut = list(color = "#FF0000", shape = 17, shape_filled = FALSE, line_type = "dashed", legend_label = "Mutant"))
k <- env$theme_write("Lab theme", aes_map, qe_colors = list(WT = "#123456", Other = "#AAAAAA"), display_names = c(WT = "Wild type", Mut = "Mutant"))
check("theme_write: one JSON file named after the key; list and read back",
      identical(k, "Lab_theme") && file.exists(file.path(th_dir, "Lab_theme.json")) && identical(env$theme_list(), "Lab_theme"))
rd <- env$theme_read("Lab_theme")
check("theme_read: styles, quick-edit colours (only of samples in the theme) and display names round-trip; non-style keys (group) are not stored",
      identical(names(rd$sample_aesthetics), c("WT", "Mut")) && rd$sample_aesthetics$Mut$line_type == "dashed" && isFALSE(rd$sample_aesthetics$Mut$shape_filled) &&
      identical(rd$qe_colors$WT, "#123456") && is.null(rd$qe_colors$Other) && is.null(rd$sample_aesthetics$WT$group) && identical(rd$display_names$Mut, "Mutant"))
mt <- env$theme_for_samples(rd, c("WT", "New"))
check("theme_for_samples: only the samples whose names match", identical(mt$matched, "WT") && identical(names(mt$sample_aesthetics), "WT"))
check("a missing or unreadable theme reads as NULL", is.null(env$theme_read("nope")) && {
  writeLines("{not json", file.path(th_dir, "Broken.json")); is.null(env$theme_read("Broken")) })
check("theme_delete: removes the file; a second delete is FALSE", isTRUE(env$theme_delete("Broken")) && !isTRUE(env$theme_delete("Broken")))
env$theme_delete("Lab_theme")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  seed_defaults(session)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "")
  smp <- rv$od_vars
  session$setInputs(qe_name_Uninfected = "Control", qe_name_Phage_MOI_10 = "Phage 10", qe_name_Phage_MOI_0_1 = "Phage 0.1")
  rv$qe_colors <- list(Uninfected = "#112233")
  session$setInputs(color_selector_Phage_MOI_10 = "custom", color_Phage_MOI_10 = "#445566", line_type_Phage_MOI_10 = "dashed", shape_Phage_MOI_10 = "17")
  before <- resolve_aesthetics(smp)
  key <- theme_save_current("Round trip")
  check("themes: save the current styles of all loaded samples (file in the themes folder)", identical(key, "Round_trip") && "Round_trip" %in% env$theme_list())
  js <- jsonlite::fromJSON(file.path(th_dir, "Round_trip.json"), simplifyVector = FALSE)
  check("themes: the file holds colour (the quick-edit colour wins), shape, filled, line type, display name per sample",
        identical(js$sample_aesthetics$Uninfected$color, "#112233") && js$sample_aesthetics$`Phage MOI 10`$line_type == "dashed" &&
        js$sample_aesthetics$`Phage MOI 10`$legend_label == "Phage 10" && identical(js$qe_colors$Uninfected, "#112233") && identical(js$display_names$Uninfected, "Control"))
  # change everything, then apply the theme
  rv$qe_colors <- list(); rv$display_names <- setNames(smp, smp)
  session$setInputs(color_selector_Phage_MOI_10 = "#000000", line_type_Phage_MOI_10 = "solid")
  check("themes: after the change the styles differ", !identical(resolve_aesthetics(smp)$colors[["Uninfected"]], "#112233") && identical(unname(dname(smp[1])), smp[1]))
  n <- theme_apply_named("Round_trip")
  after <- resolve_aesthetics(smp)
  check("themes: apply styles every matching sample (3 of 3)", identical(n, 3L))
  check("themes: the quick-edit colour is restored in rv$qe_colors and in the resolved colours",
        identical(rv$qe_colors$Uninfected, "#112233") && identical(after$colors[["Uninfected"]], "#112233"))
  check("themes: the display names are restored (rv$display_names, so legends and tables)",
        identical(unname(rv$display_names[smp]), c("Control", "Phage 10", "Phage 0.1")) && identical(unname(after$leg_labels[smp]), c("Control", "Phage 10", "Phage 0.1")))
  check("themes: applying to samples that do not match changes nothing", identical(theme_apply_named("Round_trip", c("Nothing", "Matches")), 0L))
  check("themes: an unknown theme name gives NA (and no error)", is.na(theme_apply_named("does_not_exist")))
  # auto-apply on load
  rv$qe_colors <- list(); rv$display_names <- NULL; rv$imported_settings <- NULL
  session$setInputs(active_theme = "Round_trip", theme_autoapply = TRUE)
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  check("themes: auto-apply on load styles the new samples whose names match (theme_autoapply_on_load)",
        identical(rv$qe_colors$Uninfected, "#112233") && identical(unname(rv$display_names["Uninfected"]), "Control"))
  rv$qe_colors <- list(); rv$display_names <- NULL
  session$setInputs(theme_autoapply = FALSE)
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  check("themes: with auto-apply off a load leaves the styles alone", length(rv$qe_colors) == 0)
  other <- env$make_demo_data(); names(other)[-1] <- rep(c("S one", "S two", "S three"), each = 3)
  session$setInputs(theme_autoapply = TRUE)
  ingest_data(other, "other")
  check("themes: a file whose sample names do not match the theme is not styled by it", length(rv$qe_colors) == 0)
  session$setInputs(selected_samples = rv$od_vars)
  gs <- settings_json_list()$global_settings
  check("settings files carry the v2.22 settings but never the UI-only or data inputs",
        "theme_autoapply" %in% names(gs) && "batch_outputs" %in% names(gs) && !("night_mode" %in% names(gs)) && !("compare_file" %in% names(gs)) &&
        !("apply_n4_look" %in% names(gs)))
})

# ── Compare ─────────────────────────────────────────────────────────────────
cat("\n--- Compare: a second dataset ---\n")
demo <- env$make_demo_data()
second <- demo; second[-1] <- second[-1] * 1.2; second$Extra <- second[[2]] * 0.5
f2 <- of("second.csv"); write.csv(second, f2, row.names = FALSE)
long_b <- do.call(rbind, lapply(names(second)[-1][!duplicated(names(second)[-1])], function(s)
  data.frame(Time = second$Time, Sample = s, OD = rowMeans(second[names(second) == s, drop = FALSE]))))
f3 <- of("second_long.csv"); write.csv(long_b, f3, row.names = FALSE)
has_readxl <- requireNamespace("readxl", quietly = TRUE) && requireNamespace("writexl", quietly = TRUE)
testServer(app, {
  ingest_data(demo, "Demo data (built-in)")
  seed_defaults(session)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", panel_size_mode = "fit",
                    compare_samples_b = NULL, compare_label_a = "Run 1", compare_label_b = "Run 2")
  check("compare: without a second dataset the figure is the 'Figure not available' placeholder (so the composer skips it)",
        is_placeholder_plot(generate_compare_plot()) && grepl("second dataset", placeholder_reason(generate_compare_plot())))
  # the same ingest path
  fr_w <- ingest_frame(demo, "demo")
  check("ingest_frame (wide) agrees with ingest_data: time column, samples, column map, format, time points",
        identical(fr_w$time_col, rv$time_col) && identical(fr_w$od_vars, rv$od_vars) && identical(fr_w$col_map, rv$col_map) &&
        identical(fr_w$is_long_format, rv$is_long_format) && identical(fr_w$all_timepoints, rv$all_timepoints))
  main_od <- rv$od_vars; main_cm <- rv$col_map
  ingest_data(long_b, "long"); lg <- list(tc = rv$time_col, ov = rv$od_vars, gc = rv$group_col, vc = rv$value_col, il = rv$is_long_format, rc = rv$replicate_col)
  fr_l <- ingest_frame(long_b, "long")
  check("ingest_frame (long) agrees with ingest_data: group column, value column, samples",
        identical(fr_l$group_col, lg$gc) && identical(fr_l$value_col, lg$vc) && identical(fr_l$od_vars, lg$ov) && isTRUE(fr_l$is_long_format) &&
        identical(fr_l$replicate_col, lg$rc))
  ingest_data(demo, "Demo data (built-in)")
  session$setInputs(selected_samples = rv$od_vars)
  tiny <- data.frame(Time = c(0, 10, 20, 0, 10, 20), A = c(0.1, 0.2, 0.3, 0.12, 0.22, 0.31))
  fr_b <- ingest_frame(data.frame(Time = c(0, 10, 20, 30, 0, 10, 20, 30), WT = c(.1, .2, .3, .4, .11, .21, .31, .41)), "blocks")
  check("ingest_frame: stacked time blocks (the time column restarts) are detected as replicates", identical(fr_b$n_blocks, 2L) && !is.null(fr_b$block_id))
  # load B through the file input
  session$setInputs(compare_file = list(name = "second.csv", datapath = f2, size = file.size(f2), type = "text/csv"))
  check("compare: the file input ingests the second dataset (samples, wide, source name) without touching the main data",
        identical(rv_cmp$ds$od_vars, c("Uninfected", "Phage MOI 10", "Phage MOI 0.1", "Extra")) && !rv_cmp$ds$is_long_format &&
        identical(rv_cmp$ds$data_source, "second.csv") && identical(rv$od_vars, main_od) && identical(rv$col_map, main_cm) && is.null(rv_cmp$error))
  pdB <- compare_scope(rv_cmp$ds)$prepare_plot_data(samples = c("Uninfected", "Extra"))
  pdA <- prepare_plot_data(samples = c("Uninfected", "Extra"))
  check("compare: dataset B goes through prepare_plot_data (mean / SD / n / SEM / CI per time and sample); the main data has no 'Extra'",
        all(c("time", "variable", "mean_value", "sd_value", "n", "sem_value", "ci95_value") %in% names(pdB)) && all(pdB$n[pdB$variable == "Uninfected"] == 3) &&
        nrow(pdA[pdA$variable == "Extra", ]) == 0)
  check("compare: B's values are 1.2 x A's for the same sample (the second file's own numbers)",
        isTRUE(all.equal(pdB$mean_value[pdB$variable == "Uninfected"], 1.2 * prepare_plot_data(samples = "Uninfected")$mean_value, tolerance = 1e-8)))
  session$setInputs(compare_mode = "overlay", compare_overlay_style = "linetype", compare_alpha_b = 0.5, compare_common_only = FALSE)
  po <- generate_compare_plot()
  check("compare overlay: a real ggplot built by build_plot (line, point and error-bar layers)",
        inherits(po, "ggplot") && !is_placeholder_plot(po) && length(po$layers) >= 3 &&
        any(vapply(po$layers, function(l) inherits(l$geom, "GeomErrorbar"), logical(1))) && any(vapply(po$layers, function(l) inherits(l$geom, "GeomLine"), logical(1))))
  keys <- unique(as.character(po$data$variable))
  check("compare overlay: composite keys A||sample and B||sample (7 = 3 shared samples in both + Extra in B)",
        length(keys) == 7 && all(c("A||Uninfected", "B||Uninfected", "B||Extra") %in% keys) && !("A||Extra" %in% keys))
  lab <- ggplot2::ggplot_build(po)$plot$scales$get_scales("colour")$get_labels()
  check("compare overlay: legend entries are display name + dataset label",
        all(c("Uninfected (Run 1)", "Uninfected (Run 2)", "Extra (Run 2)") %in% lab))
  bscale <- function(p, aes) ggplot2::ggplot_build(p)$plot$scales$get_scales(aes)    # the scales are trained by the build
  smap   <- function(p, aes, keys) unname(bscale(p, aes)$map(keys))
  check("compare overlay: dataset B is dashed, dataset A keeps the sample's line type",
        identical(smap(po, "linetype", "B||Uninfected"), "dashed") && identical(smap(po, "linetype", "A||Uninfected"), "solid"))
  check("compare overlay: colours are by sample (A and B of one sample share the colour)",
        identical(smap(po, "colour", "A||Uninfected"), smap(po, "colour", "B||Uninfected")) && !identical(smap(po, "colour", "A||Uninfected"), smap(po, "colour", "A||Phage MOI 10")))
  session$setInputs(qe_name_Uninfected = "Control", qe_name_Phage_MOI_10 = "Phage 10", qe_name_Phage_MOI_0_1 = "Phage 0.1")
  lab <- bscale(generate_compare_plot(), "colour")$get_labels()
  check("compare overlay: display names from the quick-edit strip apply (Control (Run 1), Phage 10 (Run 2))",
        all(c("Control (Run 1)", "Control (Run 2)", "Phage 10 (Run 2)") %in% lab) && !("Uninfected (Run 1)" %in% lab))
  session$setInputs(compare_overlay_style = "alpha", compare_alpha_b = 0.4)
  pal <- generate_compare_plot()
  cB <- smap(pal, "colour", "B||Uninfected"); cA <- smap(pal, "colour", "A||Uninfected")
  check("compare overlay by transparency: B is the same colour with an alpha channel (40 % = 66 hex), the line type is the same",
        nchar(cB) == 9 && endsWith(cB, "66") && nchar(cA) == 7 && identical(substr(cB, 1, 7), cA) &&
        identical(smap(pal, "linetype", "B||Uninfected"), smap(pal, "linetype", "A||Uninfected")))
  session$setInputs(compare_overlay_style = "linetype", compare_mode = "facet")
  pf <- generate_compare_plot()
  check("compare facet: two panels (one per dataset) with the dataset labels, shared scales", compare_panel_count(pf) == 2 &&
        identical(levels(pf$data$dataset), c("Run 1", "Run 2")) && inherits(pf$facet, "FacetWrap"))
  check("compare facet: colours by sample with ONE legend entry per sample (display names)",
        identical(sort(bscale(pf, "colour")$get_labels()), sort(c("Control", "Phage 10", "Phage 0.1", "Extra"))))
  ggsave(of("compare_facet.png"), pf, width = 6, height = 3, dpi = 60)
  check("compare facet: renders to a PNG", file.exists(of("compare_facet.png")) && file.size(of("compare_facet.png")) > 1000)
  session$setInputs(show_replicate_points = TRUE)
  pfr <- generate_compare_plot()
  is_rep <- function(l) inherits(l$geom, "GeomPoint") && identical(l$aes_params$shape, 1) && isFALSE(l$show.legend)
  check("compare: the replicate-point layer (main data only) is not drawn in either layout, so it cannot sit in both panels",
        !any(vapply(pfr$layers, is_rep, logical(1))) && any(vapply(generate_plot()$layers, is_rep, logical(1))))
  session$setInputs(show_replicate_points = FALSE)
  # fixed panel size per panel
  session$setInputs(panel_size_mode = "fixed", panel_width_mm = 60, panel_height_mm = 60, panel_square = TRUE)
  gF <- compare_fixed_info(); szF <- attr(gF, "size_mm")
  pcols <- gF$layout[grepl("^panel", gF$layout$name), ]
  wmm <- vapply(sort(unique(pcols$l)), function(i) as.numeric(gF$widths[i]), 0)
  check("compare facet in fixed mode: every panel keeps the fixed 60 x 60 mm panel (two panels, figure wider than one panel)",
        length(wmm) == 2 && near(wmm, 60, 1e-6) && szF[1] > 2 * 60 && szF[1] < 2 * 60 + 40)
  session$setInputs(compare_mode = "overlay")
  szO <- attr(compare_fixed_info(), "size_mm")
  check("compare overlay in fixed mode: one 60 x 60 mm panel; the figure is narrower than the facet one", szO[1] < szF[1] - 40 && szO[2] > 60)
  check("compare_size_in follows the fixed figure", near(compare_size_in() * 25.4, szO, 0.01))
  session$setInputs(panel_size_mode = "fit", export_units = "mm", export_width = 89, export_height = 65, export_format = "svg")
  # composer
  session$setInputs(compose_panels = c("main", "compare"), compose_ncol = "2", compose_width = "double", compose_width_mm = 183,
                    compose_panel_height_mm = 60, compose_tag_size = 8, compose_tag_case = "a", compose_tags = TRUE, journal_family = "nature")
  ci <- compose_info()
  check("compose: the Compare figure is a panel of the composed figure (two panels, nothing skipped)",
        ci$n == 2 && !is.null(ci$plot) && length(ci$skipped) == 0 && "compare" %in% ci$types)
  # one-time load of a long-format second file
  session$setInputs(compare_file = list(name = "second_long.csv", datapath = f3, size = file.size(f3), type = "text/csv"))
  check("compare: a long-format second file is ingested with its own structure", isTRUE(rv_cmp$ds$is_long_format) && identical(rv_cmp$ds$group_col, "Sample"))
  pl <- generate_compare_plot()
  check("compare: and drawn (overlay) like a wide one", inherits(pl, "ggplot") && !is_placeholder_plot(pl))
  session$setInputs(compare_samples_b = c("Uninfected", "Extra"))
  ss <- compare_sample_sets()
  check("compare: the second dataset's sample picker restricts B only; common-only intersects both",
        identical(ss$B, c("Uninfected", "Extra")) && length(ss$A) == 3 && {
          session$setInputs(compare_common_only = TRUE); s2 <- compare_sample_sets(); session$setInputs(compare_common_only = FALSE)
          identical(s2$A, "Uninfected") && identical(s2$B, "Uninfected") })
  check("compare_labels: blank -> default labels; equal labels are made different",
        identical(unname(env$compare_labels("", "")), c("Dataset A", "Dataset B")) && identical(unname(env$compare_labels("X", "X")), c("X", "X (2)")))
  # a broken file does not break the tab
  bad <- of("bad.csv"); writeLines("", bad)
  session$setInputs(compare_file = list(name = "bad.csv", datapath = bad, size = 0, type = "text/csv"))
  check("compare: an unreadable second file leaves no dataset and an error message (no crash)", is.null(rv_cmp$ds) && is.character(rv_cmp$error))
  if (has_readxl) {
    xf <- of("second.xlsx"); writexl::write_xlsx(second[, !duplicated(names(second))], xf)
    session$setInputs(compare_file = list(name = "second.xlsx", datapath = xf, size = file.size(xf), type = "application/vnd.ms-excel"))
    check("compare: an Excel file is read when readxl is installed", !is.null(rv_cmp$ds) && "Extra" %in% rv_cmp$ds$od_vars)
  } else skipit("compare: Excel input (readxl / writexl not installed)")
})

# ── Notes ───────────────────────────────────────────────────────────────────
cat("\n--- notes: caption, exports, technical-replicate caveat ---\n")
notes <- list(exp_id = "EXP-1", date = "2026-01-02", host_strain = "E. coli", phage_plasmid = "N4", moi = "10", experimenter = "",
              observations = "Clear lysis.\nSecond line.", custom_key1 = "Plate", custom_val1 = "P7", temperature = "37")
check("build_notes_caption: ticked fields in order, 'Label: value' joined by two spaces + bar; empty fields are skipped",
      identical(env$build_notes_caption(notes, c("exp_id", "experimenter", "host_strain", "moi")), "ID: EXP-1  |  Host: E. coli  |  MOI: 10"))
check("build_notes_caption: nothing ticked or nothing filled gives ''",
      identical(env$build_notes_caption(notes, character(0)), "") && identical(env$build_notes_caption(notes, "experimenter"), "") && identical(env$build_notes_caption(NULL, "exp_id"), ""))
check("the caption field list offers the 12 caption fields of the lysis-curve-app", length(env$NOTES_CAPTION_FIELDS) == 12 && all(env$NOTES_CAPTION_FIELDS %in% names(env$NOTES_CAPTION_LABELS)))
secs <- env$notes_sections(notes)
check("notes_sections: titled sections of the filled rows only (identity, biology, conditions, custom, notes)",
      identical(vapply(secs, `[[`, "", "title"), c("Experiment identity", "Biological parameters", "Conditions", "Custom fields", "Notes")) &&
      !any(vapply(secs, function(s) any(vapply(s$rows, function(r) r[[1]] == "Experimenter", logical(1))), logical(1))))
html_f <- of("notes.html"); env$write_notes_html(html_f, notes, caveat = "A caveat.")
h <- paste(readLines(html_f, warn = FALSE), collapse = "\n")
check("notes HTML: title with the experiment id, a row per filled field, the line break kept, the caveat",
      grepl("Experiment notes – EXP-1", h, fixed = TRUE) && grepl("<th>Host strain(s)</th><td>E. coli</td>", h, fixed = TRUE) &&
      grepl("Clear lysis.<br>Second line.", h, fixed = TRUE) && grepl("A caveat.", h, fixed = TRUE))
json_f <- of("notes.json"); env$write_notes_json(json_f, notes)
jj <- jsonlite::fromJSON(json_f)
check("notes JSON: every notebook field by its short key, plus the export time and app version",
      jj$exp_id == "EXP-1" && jj$host_strain == "E. coli" && jj$experimenter == "" && !is.null(jj$exported_at) && identical(jj$app_version, env$APP_VERSION))
pdf_f <- of("notes.pdf"); png_f <- of("notes.png")
quiet(env$write_notes_pdf(pdf_f, notes)); quiet(env$write_notes_png(png_f, notes))
check("notes PDF: a PDF file", file.exists(pdf_f) && identical(readBin(pdf_f, "raw", 4), charToRaw("%PDF")))
check("notes PNG (2550 x 3300 px, 300 dpi): a PNG file", file.exists(png_f) && identical(readBin(png_f, "raw", 4), as.raw(c(0x89, 0x50, 0x4e, 0x47))) && file.size(png_f) > 5000)
check("notes slide text: sections and 'Label: value' lines, newlines flattened",
      grepl("EXPERIMENT IDENTITY", env$notes_slide_text(notes), fixed = TRUE) && grepl("Observations / results: Clear lysis. Second line.", env$notes_slide_text(notes), fixed = TRUE))
check("technical_rep_caveat_for: well / well_id / technical_replicate columns only",
      !is.null(env$technical_rep_caveat_for("Well")) && !is.null(env$technical_rep_caveat_for("technical_replicate")) &&
      is.null(env$technical_rep_caveat_for("replicate")) && is.null(env$technical_rep_caveat_for(NULL)) && grepl("'well'", env$technical_rep_caveat_for("well")))
check("with_caveat_column: adds a caveat column to a stats table, leaves it alone when there is no caveat or no rows",
      identical(names(env$with_caveat_column(data.frame(a = 1:2), "x")), c("a", "caveat")) && identical(names(env$with_caveat_column(data.frame(a = 1:2), NULL)), "a") &&
      nrow(env$with_caveat_column(data.frame(a = integer(0)), "x")) == 0)
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  seed_defaults(session)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", panel_size_mode = "fit",
                    nb_exp_id = "EXP-1", nb_host_strain = "E. coli", nb_moi = "10", nb_phage_plasmid = "N4", nb_date = as.Date("2026-01-02"),
                    notes_show_caption = FALSE, notes_caption_fields = c("exp_id", "host_strain", "moi"), notes_caption_size = 6,
                    metrics_ref_sample = "Uninfected", analysis_type = "metric_summary", analysis_metrics = c("lysis_time", "od_drop"))
  check("figure notes caption: off by default - the figure has no caption beyond the error caption",
        !grepl("EXP-1", paste(generate_plot()$labels$caption, collapse = "")))
  session$setInputs(notes_show_caption = TRUE, show_error_caption = TRUE)
  cap <- generate_plot()$labels$caption
  check("figure notes caption: ticked, the figure carries the notebook fields (after the error caption)",
        grepl("ID: EXP-1", cap, fixed = TRUE) && grepl("Host: E. coli", cap, fixed = TRUE) && grepl("MOI: 10", cap, fixed = TRUE) && grepl("Error bars", cap, fixed = TRUE))
  check("figure notes caption: renders", { f <- of("capt.png"); ggsave(f, generate_plot(), width = 4, height = 3, dpi = 60); file.exists(f) })
  session$setInputs(notes_caption_size = 9)
  check("figure notes caption: the size input sets the caption text size", identical(generate_plot()$theme$plot.caption$size, 9))
  check("notes_current: the notebook inputs under their short keys", identical(notes_current()$host_strain, "E. coli") && identical(notes_current()$date, "2026-01-02"))
  # caveat
  check("no technical replicate column: no caveat", is.null(technical_rep_caveat()))
  rv$replicate_col <- "well"
  check("technical replicate column 'well': the caveat text is set", grepl("TECHNICAL", technical_rep_caveat(), fixed = TRUE))
  html <- as.character(stats_notes_ui(list(warnings = character(0), pairwise = NULL)))
  check("stats banner: the caveat is the first thing in the statistics notes", grepl("TECHNICAL", html, fixed = TRUE) && regexpr("TECHNICAL", html) < regexpr("No comparison possible", html))
  f <- of("stats.csv"); analysis_stats_csv_content(f)
  txt <- readLines(f, warn = FALSE)
  hdr <- txt[grep("^\"sample\"", txt)[1]]
  check("stats CSV export: a caveat column on every table (pairwise / omnibus / posthoc)", length(hdr) == 1 && grepl("caveat", hdr, fixed = TRUE) && any(grepl("TECHNICAL", txt, fixed = TRUE)))
  rv$replicate_col <- NULL
  analysis_stats_csv_content(f); check("stats CSV export: no caveat column without a technical replicate column", !any(grepl("caveat", readLines(f, warn = FALSE), fixed = TRUE)))
})

# ── Welch + Wilcoxon side by side ─────────────────────────────────────────
cat("\n--- stats: Welch + Wilcoxon side by side ---\n")
set.seed(3)
rm_syn <- data.frame(sample = rep(c("A", "B", "C"), each = 5), lysis_time = c(rnorm(5, 60, 3), rnorm(5, 80, 3), rnorm(5, 100, 3)), stringsAsFactors = FALSE)
r_w <- env$compare_replicate_metric(rm_syn, "lysis_time", method = "welch")
r_n <- env$compare_replicate_metric(rm_syn, "lysis_time", method = "nonparametric")
r_b <- env$compare_replicate_metric_both(env$compare_replicate_metric, rm_syn, "lysis_time")
check("both: Welch's table plus p_welch, p_wilcoxon, p_adj_bh_welch, p_adj_bh_wilcoxon",
      all(c("p_welch", "p_wilcoxon", "p_adj_bh_welch", "p_adj_bh_wilcoxon") %in% names(r_b$pairwise)) && all(names(r_w$pairwise) %in% names(r_b$pairwise)))
check("both: p is Welch's p; p_wilcoxon is the Mann-Whitney p of the same pair",
      isTRUE(all.equal(r_b$pairwise$p, r_w$pairwise$p)) && isTRUE(all.equal(r_b$pairwise$p_welch, r_w$pairwise$p)) &&
      isTRUE(all.equal(r_b$pairwise$p_wilcoxon, r_n$pairwise$p[match(paste(r_b$pairwise$sample, r_b$pairwise$versus), paste(r_n$pairwise$sample, r_n$pairwise$versus))])))
check("both: Benjamini-Hochberg within the metric across the pairs, for each test",
      isTRUE(all.equal(r_b$pairwise$p_adj_bh_welch, p.adjust(r_w$pairwise$p, "BH"))) && isTRUE(all.equal(r_b$pairwise$p_adj_bh_wilcoxon, p.adjust(r_b$pairwise$p_wilcoxon, "BH"))))
check("both: the Holm adjustment and stars of the Welch result stay", isTRUE(all.equal(r_b$pairwise$p_adj_holm, r_w$pairwise$p_adj_holm)) && identical(r_b$pairwise$stars, r_w$pairwise$stars))
check("both: ANOVA and Kruskal-Wallis omnibus rows for three groups", nrow(r_b$omnibus) == 2 && identical(r_b$omnibus$test, c("One-way ANOVA", "Kruskal-Wallis")) && !is.null(r_b$posthoc))
check("both: the test column names both tests", all(r_b$pairwise$test == "Welch t + Mann-Whitney") && identical(env$stats_test_label("both"), "Welch t + Mann-Whitney"))
check("stats_base_method: 'both' -> welch; the others unchanged", identical(env$stats_base_method("both"), "welch") && identical(env$stats_base_method("nonparametric"), "nonparametric"))
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  seed_defaults(session)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = "")
  sb <- compare_replicate_metric(rm_syn, "lysis_time", method = "both")
  check("server: compare_replicate_metric(method = 'both') is handled (the shadowing wrapper); 'welch' still goes to the original",
        "p_wilcoxon" %in% names(sb$pairwise) && !("p_wilcoxon" %in% names(compare_replicate_metric(rm_syn, "lysis_time", method = "welch")$pairwise)))
  am <- compare_all_metrics(env$compute_replicate_metrics(prepare_replicate_data()), metrics = "lysis_time", method = "both")
  check("server: compare_all_metrics(method = 'both') runs the single-test (Welch) path instead of erroring", is.data.frame(am$table) && all(am$table$test == "Welch t"))
  session$setInputs(stats_method = "both", analysis_type = "metric_summary", analysis_metrics = c("lysis_time", "od_drop"))
  st <- analysis_stats_display()
  check("server: the Analysis tab's replicate statistics table shows both tests when 'both' is chosen",
        !is.null(st$pairwise) && "p_wilcoxon" %in% names(st$pairwise))
  m <- draft_methods_text()
  check("methods text: names both tests and the BH adjustment when 'both' is chosen",
        grepl("Welch's t-test (unequal variances) and the Mann–Whitney U test side by side", m, fixed = TRUE) && grepl("Benjamini", m, fixed = TRUE) && grepl("Kruskal", m, fixed = TRUE))
  session$setInputs(stats_method = "welch")
  check("methods text: the Welch default sentence is unchanged", grepl("Welch's t-test (unequal variances); p values were adjusted", draft_methods_text(), fixed = TRUE))
})

# ── Batch export ────────────────────────────────────────────────────────────
cat("\n--- batch export ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  seed_defaults(session)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", panel_size_mode = "fit",
                    export_units = "mm", export_width = 89, export_height = 65, metrics_ref_sample = "Uninfected",
                    analysis_type = "metric_summary", analysis_metrics = c("lysis_time", "od_drop"), nb_exp_id = "EXP-9", nb_date = Sys.Date())
  d1 <- file.path(out_dir, "b1"); dir.create(d1)
  res <- batch_export_build(d1, c("main", "csv_summary", "csv_raw_wide"), "png", 80, NULL)
  files <- sort(list.files(d1))
  check("batch: exactly the chosen outputs are written (main figure, summary CSV, raw wide CSV), in the chosen format",
        identical(files, sort(c(res$files))) && length(files) == 3 && sum(grepl("\\.png$", files)) == 1 && all(c("summary_statistics.csv", "raw_data_wide.csv") %in% files) &&
        length(res$skipped) == 0)
  png1 <- file.path(d1, grep("\\.png$", files, value = TRUE))
  check("batch: the PNG is a PNG", identical(readBin(png1, "raw", 4), as.raw(c(0x89, 0x50, 0x4e, 0x47))))
  raw <- read.csv(file.path(d1, "raw_data_wide.csv"), check.names = FALSE)
  check("batch: the raw wide data CSV is the loaded wide table (time + every replicate column)", ncol(raw) == 10 && identical(names(raw)[1], "Time") && nrow(raw) == nrow(rv$data))
  d2 <- file.path(out_dir, "b2"); dir.create(d2)
  res2 <- batch_export_build(d2, c("analysis:metric_summary", "compare", "notes", "csv_metrics", "csv_replicate_stats"), "svg", 300, NULL)
  f2 <- sort(list.files(d2))
  check("batch: an analysis figure as SVG, the notes as HTML + JSON, the metrics CSV and the replicate statistics CSVs are written",
        any(grepl("^metric_summary.*\\.svg$", f2)) && all(c("notes.html", "notes.json", "curve_metrics.csv") %in% f2) && any(grepl("^replicate_statistics_pairwise", f2)))
  check("batch: an output that cannot be made (compare without a second dataset) is skipped and listed in SKIPPED.txt, not written",
        !any(grepl("^compare", f2)) && "SKIPPED.txt" %in% f2 && any(grepl("Compare figure", res2$skipped)) && any(grepl("Compare figure", readLines(file.path(d2, "SKIPPED.txt"), warn = FALSE))))
  d3 <- file.path(out_dir, "b3"); dir.create(d3)
  res3 <- batch_export_build(d3, "main", "svg", 300, c(120, 50))
  svg <- readLines(list.files(d3, full.names = TRUE)[1], warn = FALSE, n = 3)
  check("batch: a custom size applies to the single figure (120 x 50 mm = 4.72 x 1.97 in in the SVG header)",
        any(grepl("width='340.1", svg, fixed = TRUE)) || any(grepl("340.15", svg, fixed = TRUE)) || any(grepl("4.72", svg, fixed = TRUE)) || any(grepl("viewBox='0 0 340", svg)))
  d4 <- file.path(out_dir, "b4"); dir.create(d4)
  fixed_res <- {
    session$setInputs(panel_size_mode = "fixed", panel_width_mm = 50, panel_height_mm = 50, panel_square = TRUE)
    batch_export_build(d4, "main", "pdf", 300, NULL)
  }
  check("batch: in fixed-panel mode the figure is written at the computed size (size tag in the file name), as a PDF",
        length(fixed_res$files) == 1 && grepl("\\.pdf$", fixed_res$files) && grepl("[0-9]+x[0-9]+mm", fixed_res$files))
  check("batch: the batch size reader: 'current' -> NULL, custom -> (w, h) in mm clipped to the limit",
        { session$setInputs(batch_size_mode = "current"); is.null(batch_size_now()) } && {
          session$setInputs(batch_size_mode = "custom", batch_width_mm = 100, batch_height_mm = 80); identical(batch_size_now(), c(100, 80)) })
  # the archive
  tmp <- tempfile("arch"); dir.create(tmp); folder <- "lysis_export"; dir.create(file.path(tmp, folder))
  file.copy(file.path(d1, files), file.path(tmp, folder))
  arch <- file.path(out_dir, paste0("batch.", env$bundle_archive_kind()))
  env$write_bundle_archive(arch, tmp, folder)
  inside <- if (identical(env$bundle_archive_kind(), "zip")) utils::unzip(arch, list = TRUE)$Name else utils::untar(arch, list = TRUE)
  inside <- sub("^lysis_export/", "", inside[!grepl("/$", inside)])
  check("batch archive: contains the chosen files only (nothing else, inside one folder)", identical(sort(inside), files))
  check("raw_wide_from_replicates: time + one column per sample / replicate (long-format data)",
        { rp <- data.frame(time = rep(c(0, 10), 4), variable = rep(c("A", "B"), each = 4), replicate = rep(rep(1:2, each = 2), 2), value = 1:8)
          w <- env$raw_wide_from_replicates(rp); identical(names(w), c("time", "A_rep1", "A_rep2", "B_rep1", "B_rep2")) && nrow(w) == 2 })
})

# ── Parity frames ───────────────────────────────────────────────────────────
cat("\n--- parity frames (PowerPoint slides / GIF frames) ---\n")
panel_left_mm <- function(g) {
  pos <- env$gtable_panel_row(g)
  f <- tempfile(fileext = ".svg"); svglite::svglite(f, width = 8, height = 8); on.exit({ dev.off(); unlink(f) })
  if (pos$l > 1) grid::convertWidth(sum(g$widths[seq_len(pos$l - 1)]), "mm", valueOnly = TRUE) else 0
}
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  seed_defaults(session)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", panel_size_mode = "fit")
  samps <- rv$od_vars
  for (lp in c("inside-auto", "right")) {
    session$setInputs(legend_position = lp)
    fr <- presentation_frames(samps, canvas_w_in = 8, canvas_h_in = 6)
    sz <- vapply(fr$grobs, function(g) attr(g, "size_mm"), numeric(2))
    check(sprintf("parity (%s legend): %d frames, one per added sample, all gtables", lp, length(samps)), length(fr$grobs) == length(samps) && all(vapply(fr$grobs, inherits, TRUE, "gtable")))
    check(sprintf("parity (%s legend): every frame has the same figure size (width and height equal to 0.25 mm)", lp), near(sz[1, ], sz[1, 1]) && near(sz[2, ], sz[2, 1]))
    left <- vapply(fr$grobs, panel_left_mm, 0)
    check(sprintf("parity (%s legend): the panel sits at the same distance from the left edge in every frame", lp), near(left, left[1], 0.25))
    pr <- env$gtable_panel_row(fr$grobs[[1]])
    ph <- vapply(fr$grobs, function(g) as.numeric(g$heights[env$gtable_panel_row(g)$t]), 0)
    check(sprintf("parity (%s legend): the panel height is pinned to the same value in every frame (%.1f mm)", lp, ph[1]), near(ph, ph[1], 1e-6))
    check(sprintf("parity (%s legend): the reported size is the final frame's", lp), near(c(fr$w_mm, fr$h_mm), sz[, length(samps)]))
  }
  # all-frames axes: the first frame (one sample) has the same x / y range as the last
  fr <- presentation_frames(samps, canvas_w_in = 8, canvas_h_in = 6)
  # fit into a smaller box: the panel shrinks, still identical in every frame
  fr_fit <- presentation_frames(samps, canvas_w_in = 8, canvas_h_in = 6, fit_w_in = 3.2, fit_h_in = 2.4)
  szf <- vapply(fr_fit$grobs, function(g) attr(g, "size_mm"), numeric(2))
  check("parity: a box the figure must fit (a slide) shrinks the panel (same in every frame) until the figure fits",
        isTRUE(fr_fit$fitted) && fr_fit$w_in <= 3.2 + 1e-6 && fr_fit$h_in <= 2.4 + 1e-6 && near(szf[1, ], szf[1, 1]) && fr_fit$panel_w_mm < fr$panel_w_mm)
  check("parity: without a box the panel is 62 % x 70 % of the canvas", near(c(fr$panel_w_mm, fr$panel_h_mm), c(0.62 * 8 * 25.4, 0.7 * 6 * 25.4), 1e-6))
  session$setInputs(panel_size_mode = "fixed", panel_width_mm = 70, panel_height_mm = 55, panel_square = FALSE)
  frx <- presentation_frames(samps, canvas_w_in = 8, canvas_h_in = 6)
  check("parity: in fixed-panel mode the frames use the fixed panel size of the main figure", near(c(frx$panel_w_mm, frx$panel_h_mm), c(70, 55), 1e-6))
})
check("expand_for_tall_legend: a legend taller than its panel adds pad rows; the panel height stays",
      {
        pl <- ggplot(data.frame(x = rep(1:3, 22), y = rnorm(66), g = factor(rep(sprintf("strain %02d", 1:22), each = 3))), aes(x, y, colour = g)) + geom_line() +
          theme(legend.position = "right")
        g0 <- env$fixed_panel_grob(pl, 60, 30)
        pad <- env$legend_pad_pt(g0)
        g1 <- env$expand_for_tall_legend(g0)
        ph1 <- as.numeric(g1$heights[env$gtable_panel_row(g1)$t])
        pad > 0 && attr(g1, "size_mm")[2] > attr(g0, "size_mm")[2] + 5 && near(ph1, 30, 1e-6) && env$legend_pad_pt(g1) <= 0.5
      })
check("expand_for_tall_legend: a legend that fits is left alone", {
  pl <- ggplot(data.frame(x = 1:3, y = 1:3, g = "a"), aes(x, y, colour = g)) + geom_line() + theme(legend.position = "right")
  g0 <- env$fixed_panel_grob(pl, 60, 60); identical(env$legend_pad_pt(g0), 0) && identical(dim(env$expand_for_tall_legend(g0)), dim(g0))
})
check("gtable_legend_col: the right legend's column lies to the right of the panel", {
  pl <- ggplot(data.frame(x = 1:3, y = 1:3, g = "a"), aes(x, y, colour = g)) + geom_line()
  g <- env$fixed_panel_grob(pl + theme(legend.position = "right"), 50, 50)
  lc <- env$gtable_legend_col(g); is.numeric(lc) && lc > env$gtable_panel_row(g)$l
})

# ── Download handlers: the compare figure in the all-figures archive and the new downloads exist ──
cat("\n--- downloads ---\n")
testServer(app, {
  ingest_data(demo, "Demo data (built-in)")
  seed_defaults(session)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", panel_size_mode = "fit",
                    compare_label_a = "A1", compare_label_b = "B1", compare_mode = "overlay", compare_overlay_style = "linetype", compare_alpha_b = .5)
  session$setInputs(compare_file = list(name = "second.csv", datapath = f2, size = file.size(f2), type = "text/csv"))
  path <- output$downloadCompare
  check("download: the Compare figure (SVG) is a real file; the name carries both labels and the size",
        is.character(path) && file.exists(path) && file.size(path) > 2000 && grepl("<svg", paste(readLines(path, n = 5, warn = FALSE), collapse = " ")))
  check("download: compare_file_stem names both datasets and the size", grepl("^compare_A1_vs_B1_[0-9]+x[0-9]+mm$", compare_file_stem()))
  session$setInputs(nb_exp_id = "EXP-5", nb_host_strain = "E. coli", nb_date = Sys.Date(), batch_outputs = c("main", "csv_summary"), batch_format = "png",
                    batch_dpi = 72, batch_size_mode = "current", metrics_ref_sample = "Uninfected", analysis_type = "metric_summary",
                    analysis_metrics = c("lysis_time", "od_drop"))
  nh <- output$download_notes_html
  check("download: the notes page (HTML) is a file that names the experiment", file.exists(nh) && grepl("EXP-5", paste(readLines(nh, warn = FALSE), collapse = " "), fixed = TRUE))
  nj <- output$download_notes_json
  check("download: the notes JSON parses", file.exists(nj) && identical(jsonlite::fromJSON(nj)$exp_id, "EXP-5"))
  np <- output$download_notes_pdf
  check("download: the notes PDF is a PDF", file.exists(np) && identical(readBin(np, "raw", 4), charToRaw("%PDF")))
  bz <- output$downloadBatchExport
  inside <- if (identical(env$bundle_archive_kind(), "zip")) utils::unzip(bz, list = TRUE)$Name else utils::untar(bz, list = TRUE)
  inside <- sub("^lysis_export/", "", inside[!grepl("/$", inside)])
  check("download: the batch archive holds exactly the ticked outputs (main figure as PNG + the summary CSV)",
        length(inside) == 2 && sum(grepl("^lysis_curve_main.*\\.png$", inside)) == 1 && "summary_statistics.csv" %in% inside)
  session$setInputs(batch_outputs = c("main", "compare"))
  bz2 <- output$downloadBatchExport
  inside2 <- if (identical(env$bundle_archive_kind(), "zip")) utils::unzip(bz2, list = TRUE)$Name else utils::untar(bz2, list = TRUE)
  check("download: the all-figures archive includes the compare figure when a second dataset is loaded",
        { af <- output$downloadAllFigures
          nms <- if (identical(env$bundle_archive_kind(), "zip")) utils::unzip(af, list = TRUE)$Name else utils::untar(af, list = TRUE)
          any(grepl("compare_A1_vs_B1_.*\\.svg$", nms)) && any(grepl("compare_A1_vs_B1_.*\\.png$", nms)) })
  check("download: the batch archive with a compare figure ticked contains it", any(grepl("compare_A1_vs_B1_.*\\.png$", inside2)))
  if (has_officer_rvg <- requireNamespace("officer", quietly = TRUE) && requireNamespace("rvg", quietly = TRUE)) {
    session$setInputs(legend_position = "right", notes_pptx_slide = TRUE, gif_fps = 1, gif_width = 600, gif_height = 400)
    pp <- output$downloadPPTX; prs <- officer::read_pptx(pp)
    check("PPTX: one slide per sample, the final slide and the notes slide (3 + 1 + 1)", length(prs) == 5)
    ns <- officer::slide_summary(prs, 5)
    check("PPTX: the notes slide carries the notebook fields", any(grepl("EXP-5", ns$text, fixed = TRUE)) && any(grepl("Experiment Notes", ns$text, fixed = TRUE)))
    session$setInputs(notes_pptx_slide = FALSE)
    check("PPTX: no notes slide when it is not ticked (3 + 1 slides)", length(officer::read_pptx(output$downloadPPTX)) == 4)
  } else skipit("PPTX download (officer / rvg not installed)")
  if (requireNamespace("gifski", quietly = TRUE)) {
    gf <- output$downloadGIF
    check("GIF: a GIF built from the parity frames", file.exists(gf) && identical(rawToChar(readBin(gf, "raw", 6)), "GIF89a"))
  } else skipit("GIF download (gifski not installed)")
})

cat(sprintf("\n============ SUMMARY: %d passed / %d failed / %d skipped ============\n", pass, fail, skip))
if (fail > 0) quit(status = 1)
