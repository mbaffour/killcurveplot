# Lysis Curve OD Visualization App  v2.11
# Michael Baffour Awuah / Ramsey Lab
#
# v2.17.0 -- Log-axis clipping, QC flags, notebook exports, figure bundle, threshold line, gradients:
#   LOG-AXIS CLIPPING -- y_oob: a reading below the log-axis floor is drawn ON the floor (squish, the default
#                        and the journal preset) instead of being deleted (censor, the Legacy look).
#   QC FLAGS          -- qc_flags(): CEILING / NEGATIVE / GAP / FLAT_REFERENCE; sidebar list, Dashboard and a
#                        "QC flags" section in the HTML and PDF reports. Input qc_od_ceiling.
#   NOTES / LEGEND    -- the Figure Legend Text is a paragraph under the main figure in the reports;
#                        notebook .txt and one-row .csv downloads; notebook fields Passage #, Inducer
#                        concentration, Other condition.
#   FIGURE BUNDLE     -- one .zip: figure (SVG + PDF), composed SVG, settings.json, tidy source data,
#                        summary statistics, curve metrics, legend, methods, README.
#   THRESHOLD LINE    -- horizontal OD line with an optional label (Time Point Markers section).
#   GRADIENTS         -- "Blues" and "Blue to red" dose-gradient palettes (control black) for MOI series.
#   ROBUSTNESS        -- samples that match no loaded column give zero-row frames, not an error.
#
# v2.16.0 -- Subscript axis labels and a Statistics tab:
#   SUBSCRIPTS      -- label_with_subscripts(): a y label with a wavelength token (A550, OD600,
#                      "A550 relative to Uninfected", "log10 A550") becomes plotmath A[550] / OD[600];
#                      labels without a token stay character. Checkbox axis_subscripts (default on;
#                      the Legacy look turns it off, so the v2.9 output stays byte-identical). Applied
#                      to the main figure, the analysis figures' y labels, the heatmap key and the
#                      dose-response axis; facet strips, the Figure Legend Text and report captions
#                      stay plain text.
#   STATISTICS TAB  -- new main-panel tab after Dashboard: summary statistics (DT table with search
#                      when DT is installed), curve metrics + definitions, per-replicate metrics,
#                      the replicate comparison (same builders as the Analysis tab), each with a CSV
#                      button, and an editable Methods text (draft_methods_text() fills it from the
#                      current settings). A non-empty Methods text is a "Methods" section in the HTML
#                      and PDF reports and downloads as .txt.
#
# v2.15.0 -- Square panels: "fixed panel size" is the new standard (panel_size_mode):
#   the plotting panel is exactly panel_width_mm x panel_height_mm (65 x 65 mm by default) and the
#   figure is sized AROUND it (axes, labels and an outside legend make it wider), via
#   fixed_panel_grob(). The size bar, previews, file names, exports, reports and the composer
#   all use the computed figure size. "fit" (the Legacy look) keeps the old behaviour: the panel
#   fills export_width x export_height.
#
# v2.14.1 -- Hardening: one raster guard (safe_raster_dpi, 40 megapixel cap) on every raster
#   render, size/zoom clamps, tick-count guards, render and download errors shown instead of
#   fatal (with a hint for memory errors), data-size warnings, memory line in the report.
#
# v2.14.0 -- Journal standard by default (a deliberate change of default output):
#   STARTUP STATE     -- APP_DEFAULTS = journal_preset_settings(89, 65, "a"): Nature single
#                        column, Arial 6-8 pt, SVG, true-size preview. The pre-v2.14 look is
#                        legacy_preset_settings() (button "Legacy look"); the byte-identical
#                        test against v2.9 applies it. Widgets start from APP_DEFAULTS.
#   TRUE-SIZE PREVIEW -- zoom fits the main panel (input$main_panel_width, rounded down to
#                        0.25); canvas mode keeps the export aspect ratio.
#   SIZE BAR          -- always-visible line above the tabs with the live export size,
#                        journal column, fonts and line widths; warns when not journal-standard.
#   FILE NAMES        -- lysis_curve_<label>_<W>x<H>mm.svg etc., no timestamp.
#   PDF REPORT        -- opens the font-embedding device (Arial is now the default font).
#
# v2.13.0 -- Multi-panel figure composer (opt-in; default main figure unchanged):
#   COMPOSE FIGURE    -- New main-panel tab: pick the main lysis curve and any
#                        analysis figures, a column count, a journal column width
#                        and a panel height; the app lays them out with patchwork
#                        (a, b, c tags, free() panels so outside legends do not
#                        squeeze their neighbours), previews it at true size and
#                        exports it (compose_figure(), shared with
#                        tools/render_gallery.R). Panels whose figure is not
#                        available are skipped, not composed. The composed figure
#                        is also the last figure of the HTML / PDF reports.
#
# v2.12.4 -- Review round 4: dashboard prints "—" (no "± SD") when a mean-curve
#   metric is NA; adjustable lysis-onset threshold (metrics_lysis_threshold_pct,
#   default 5 %); the reports always carry the curve-landmarks figure and the
#   dose-response figure when the selection has >= 2 distinct positive MOIs.
#
# v2.12.3 -- Review round 3 (opt-in; default main figure unchanged):
#   Inside legends fall back to "right" above 4 entries, or when the emptiest
#   corner box still holds 2+ points. Peak mode "first_local" now needs a > 10 %
#   decline (4 % plateau noise no longer counts as lysis).
#
# v2.12.2 -- Review round (all opt-in; the default main figure is unchanged):
#   LEGEND AUTO CORNER -- legend position "Inside: emptiest corner (auto)" puts
#                         the key in the quadrant with the fewest plotted points
#                         (auto_legend_corner); > 4 entries fall back to "right".
#                         Inside keys are more compact. The journal preset uses it.
#   LABELS             -- shorter metric facet titles, 45-degree x labels for 5+
#                         samples, plain-number MOI axis labels.
#   DOSE-RESPONSE      -- one line + end label per phage (MOI stripped from the
#                         sample label); a single MOI level shows a message.
#   PEAK MODE          -- metrics_peak_mode "first_local": lysis metrics anchored
#                         on the first peak followed by a > 10 % decline, for
#                         regrowth / resistance curves (default "global").
#
# v2.11 -- Analysis figures, replicate statistics, dashboard:
#   SHARED THEME       -- figure_theme_layers(): the main figure's fonts, line
#                         weights, legend and spacing (journal preset included)
#                         as one list applied to every figure the app draws.
#   ANALYSIS FIGURES   -- New main-panel tab: metric summary (per-replicate
#                         points + mean +/- SD, faceted by metric, optional
#                         significance marks vs the reference), specific growth
#                         rate mu(t) vs time (same rolling regression as mu_max),
#                         normalized OD (vs reference or t0), replicate overlay,
#                         MOI dose-response, OD heatmap. Same export devices and
#                         size as the main figure; own panel label.
#   STATISTICS         -- compare_replicate_metric(): Welch t (or Mann-Whitney)
#                         per pair / vs reference, Holm-adjusted p, Hedges' g,
#                         95% CI; one-way ANOVA + Tukey (or Kruskal-Wallis) for
#                         >= 3 groups; explicit low-n warnings. CSV export.
#   DASHBOARD          -- Tab with data cards, key metrics per sample, four
#                         small-multiple figures, statistics, notebook summary.
#   MOI PARSING        -- parse_moi()/moi_map(): MOI read from sample names
#                         ("Phage MOI 10") or a 'sample = MOI' override box.
#
# v2.10 -- Journal-standard figure export (see docs/FIGURE_STANDARDS.md):
#   JOURNAL PRESET     -- One-click "Apply journal preset": sets the figure to a
#                         real column width (Nature 89/120/183 mm, Cell 85/114/174,
#                         ASM 84/175, PLOS 132, Science 55/120/183, or custom mm),
#                         Arial 6-8 pt text, 0.5-0.75 pt lines, Okabe-Ito colours,
#                         legend inside the panel, bold 8 pt panel label (a, b...).
#                         Opt-in: default output is unchanged.
#   EXPORT DEVICES     -- PDF now embeds fonts (quartz on macOS, cairo elsewhere;
#                         grDevices::pdf never embedded and crashed on "Arial").
#                         SVG is written with svglite(fix_text_size = FALSE) so
#                         text stays freely editable in Inkscape (no textLength).
#                         PNG/TIFF/JPEG use ragg when installed; TIFF keeps LZW.
#   MM UNITS + WYSIWYG -- Export size can be given in mm; "True export size"
#                         preview renders the on-screen plot at the exact
#                         physical size (x zoom) so what you see is what
#                         Inkscape opens.
#   FIGURE LEGEND TEXT -- Auto-drafted legend paragraph (measure, n, error type,
#                         MOI/host/phage/medium from the notebook) to paste into
#                         the manuscript, so the artwork itself stays clean.
#   LAYOUT INPUTS      -- Legend position (incl. inside corners), legend border,
#                         legend key size, axis/tick line width (pt), tick length
#                         (mm), panel label + size, explicit Arial/Helvetica fonts.
#   PALETTE WARNING    -- Colorblind/Publication palettes warn when > 8 samples
#                         are loaded (colours would repeat).
#
# v2.8 -- Accuracy fixes + stats/reporting features:
#   REPLICATE FIX      -- CRITICAL: wide-format files with replicate columns
#                         sharing the same name (the documented workflow) were
#                         silently collapsed to only the FIRST column of each
#                         name, so means used n=1 and error bars disappeared.
#                         Columns are now resolved by position (col_map), and
#                         every replicate column contributes to mean/SD/SEM/CI.
#   SETTINGS SAVE FIX  -- 'shape_size' and 'color_palette' were misclassified
#                         as per-sample inputs (prefix collision with 'shape_'
#                         / 'color_') and silently dropped from saved settings
#                         files. They are now saved and restored correctly.
#   RGB->HEX FIX       -- rgb_to_hex() no longer appends a redundant 'FF'
#                         alpha channel when alpha = 1, so preset color
#                         matching works after using the RGB sliders.
#   DATA SUMMARY       -- After loading a CSV the sidebar shows what was
#                         detected: format, time column, samples, and the
#                         number of replicate columns per sample.
#   DEMO DATA          -- One-click demo dataset (3 samples x 3 replicates)
#                         for trying the app without a file.
#   STATS EXPORT       -- Download the summary statistics table (time, sample,
#                         mean, SD, n, SEM, 95% CI) as CSV.
#   CURVE METRICS      -- Per-sample metrics table (initial/final/min/max OD,
#                         AUC, time to half-of-max after peak, steepest decline
#                         rate, max specific growth rate, doubling time) with
#                         CSV export. All metrics computed from the plotted
#                         mean curve (i.e. after time filtering) with
#                         transparent, documented formulas.
#   HTML REPORT        -- One-click single-file HTML report: figure (embedded),
#                         stats + metrics tables, metric definitions, data
#                         provenance, key settings, and R session info.
#   PDF REPORT         -- Same content as a multi-page PDF (no LaTeX/pandoc
#                         needed; uses gridExtra).
#   TIFF LZW           -- TIFF export now uses lossless LZW compression
#                         (journal requirement).
#
# v2.7 -- Bug fixes:
#   SHADOW FIX         -- Shadow/ribbon error display no longer crashes when
#                         points are also enabled. The fill scale conflict
#                         between geom_ribbon (scale_fill_manual) and
#                         geom_point (scale_fill_identity) is resolved by
#                         using a unified scale_fill_manual when shadow mode
#                         is active.
#   NA ERROR FIX       -- Single-replicate samples (where sd() returns NA)
#                         no longer crash error bars or ribbons. NA sd values
#                         are replaced with 0 before computing error bounds.
#   OPERATOR ORDER FIX -- The %||% (null-coalescing) operator is now defined
#                         before its first use in renderUI.
#   ERROR GUARD        -- Error column values are explicitly guarded against
#                         NA before computing err_lo / err_hi bounds.
#
# v2.6 -- Stability & performance improvements for large replicate counts:
#   OBSERVER LEAK FIX  -- Color observeEvent handlers are registered at most
#                         once per sample ID (tracked in registered_color_obs).
#   POINT LOOP -> O(1) -- build_plot() no longer adds one geom_point() layer
#                         per sample; shape/fill are vectorised into the data
#                         frame and a single geom_point() call covers all.
#   PIVOT GUARD        -- prepare_plot_data() pre-selects only the requested
#                         sample columns before pivot_longer().
#   VECTORISED FILTER  -- apply_time_filters() uses outer() matrix comparison
#                         instead of per-row vapply for time-point exclusions.
#   DEBOUNCE           -- Main plot render waits 400 ms after the last input
#                         change before rebuilding, preventing render pile-up.
#   CRASH GUARD        -- build_plot() wrapped in tryCatch(); errors display
#                         as an in-plot message rather than crashing the session.
#   PAGINATION         -- Variable Styling panel shows at most 20 samples at a
#                         time with Prev/Next navigation; keeps the DOM small.on App  v2.5
# Michael Baffour Awuah / Ramsey Lab
#
# v2.5 changes:
#   SETTINGS  – Save/load now stores BOTH per-sample aesthetics (color, shape,
#               linetype keyed by sample name) AND all global visual settings.
#               On import: matched samples get their saved aesthetics applied
#               automatically; a status panel shows exactly which names matched
#               and which didn't.  "Clear Imported" fully revokes everything and
#               restores defaults — per-sample overrides included.
#
#   GIF       – Rebuilt without gganimate. Each frame is a real ggplot rendered
#               to PNG via ragg/png, then stitched with gifski. Full fidelity:
#               same theme, axes, error bars, shapes, colors as the main plot.
#               Each frame adds one more line (cumulative reveal).
#
#   PPTX      – Each slide is a pixel-perfect render of build_plot() with the
#               cumulative subset + grey-out of non-active samples. Final slide
#               shows all samples in full color. Slide dimensions driven by the
#               export width/height inputs.

# ── Packages ──────────────────────────────────────────────────────────────────
library(shiny)
library(tidyverse)
library(ggpubr)
library(scales)
library(ggrepel)
library(ggprism)
library(svglite)
library(jsonlite)

has_officer   <- requireNamespace("officer",   quietly = TRUE)
has_rvg       <- requireNamespace("rvg",       quietly = TRUE)
has_gifski    <- requireNamespace("gifski",    quietly = TRUE)
has_base64enc <- requireNamespace("base64enc", quietly = TRUE)
has_gridExtra <- requireNamespace("gridExtra", quietly = TRUE)
has_ragg      <- requireNamespace("ragg",      quietly = TRUE)
has_patchwork <- requireNamespace("patchwork", quietly = TRUE)
has_DT        <- requireNamespace("DT",        quietly = TRUE)   # v2.16: searchable summary-statistics table

# Single source of truth for the app version. Used in the HTML report footer
# and the saved-settings JSON so a bundle/session can be tied back to the code
# that produced it (audit KC-8: the settings JSON used to hardcode "2.5").
APP_VERSION <- "2.17.0"

# ── Helpers ───────────────────────────────────────────────────────────────────

safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

normalize_hex_color <- function(hex) {
  if (is.null(hex) || is.na(hex) || nchar(trimws(hex)) == 0) return("#000000")
  hex <- trimws(hex)
  if (!grepl("^#", hex)) hex <- paste0("#", hex)
  if (grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", hex)) return(toupper(hex))
  "#000000"
}

rgb_to_hex <- function(r, g, b, a = 1) {
  rr <- min(max(as.integer(r), 0), 255) / 255
  gg <- min(max(as.integer(g), 0), 255) / 255
  bb <- min(max(as.integer(b), 0), 255) / 255
  aa <- min(max(a, 0), 1)
  # Only emit an 8-digit hex when alpha is actually < 1; a trailing "FF"
  # breaks equality checks against the 6-digit preset colors.
  if (aa >= 1) rgb(rr, gg, bb) else rgb(rr, gg, bb, aa)
}

# KC-11: read.csv() with no arguments assumes the US/UK locale (comma field
# separator, "." decimal mark). A European export ("1,5E+08" values with
# ";" field separators) then parses every numeric column to character, and
# those get silently dropped later by the numeric coercion + !is.na(cfu)-
# style filters -- the user sees missing data with no clue why. Sniff the
# separator from the header line (a semicolon-delimited file has no commas
# in its header) unless the user picked a locale explicitly.
detect_csv_locale <- function(path) {
  first_line <- tryCatch(readLines(path, n = 1, warn = FALSE), error = function(e) "")
  if (length(first_line) == 0) return("us")
  if (grepl(";", first_line, fixed = TRUE)) "eu" else "us"
}

# Ingest hardening (ported from the companion app's read_csv_smart(),
# coerce_sentinel_columns(), parse_time_column(), infer_wide_replicates() and
# detect_replicate_column(); see docs/PORTED_FROM_LYSIS_CURVE_APP.md).

# Field counts of the non-blank lines of a file, WITH their raw line numbers
# (read.csv's `skip` counts raw lines, blank ones included). Counts come from
# strsplit(), which drops trailing empty fields: a preamble line padded with
# commas out to the table width ("Software Version,3.08,,,,") therefore still
# counts as narrow, which is what the preamble test needs.
csv_line_field_counts <- function(path, sep, n = 500) {
  raw <- tryCatch(readLines(path, n = n, warn = FALSE), error = function(e) character(0))
  keep <- which(nzchar(trimws(raw)))
  list(line = keep, text = raw[keep],
       fc = vapply(raw[keep], function(l) length(strsplit(l, sep, fixed = TRUE)[[1]]),
                   integer(1), USE.NAMES = FALSE))
}

# Raw-line index (0-based, i.e. read.csv's `skip`) of the table header when the
# file starts with a reader preamble: the first non-blank line is NARROWER than
# the widest field count that occurs at least twice. 0 for an ordinary file.
csv_preamble_skip <- function(path, sep = ",") {
  z <- csv_line_field_counts(path, sep)
  if (length(z$fc) < 3) return(0L)
  tab <- table(z$fc[z$fc > 1])
  rep_counts <- as.integer(names(tab)[tab >= 2])
  if (!length(rep_counts)) return(0L)
  modal <- max(rep_counts)
  if (z$fc[1] >= modal) return(0L)
  as.integer(z$line[which(z$fc == modal)[1]] - 1L)
}

# Delimiter / preamble sniffing. Tries comma, semicolon and tab; the table is
# the block of lines with the LARGEST field count that occurs at least twice
# (a reader preamble is a handful of narrow lines above a wide table, so a modal
# count would sometimes pick the preamble); everything above its first line is
# skipped. The plain read.csv() is tried first and kept whenever it produces
# something sane (>= 2 columns, rows, and no preamble), so ordinary files take
# exactly the path they always did. Errors are worded for a bench scientist.
# Deviation from the companion app: `skip` is computed on RAW line numbers (the
# original filtered blank lines out first, so a blank line inside a preamble
# made read.csv skip too few lines).
read_csv_smart <- function(path) {
  plain <- tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
                    error = function(e) NULL)
  if (!is.null(plain) && ncol(plain) >= 2 && nrow(plain) > 0 && csv_preamble_skip(path, ",") == 0L)
    return(list(data = plain, sep = ",", dec = ".", skip = 0L, sniffed = FALSE))

  raw <- tryCatch(readLines(path, n = 500, warn = FALSE), error = function(e) character(0))
  keep  <- which(nzchar(trimws(raw)))
  lines <- raw[keep]
  if (length(lines) == 0) stop("The file is empty.", call. = FALSE)

  nf <- function(sep) vapply(lines, function(l)
    length(strsplit(l, sep, fixed = TRUE)[[1]]), integer(1), USE.NAMES = FALSE)

  # European Excel writes 0,05 for 0.05. With a comma separator that is
  # unparseable by construction -- the header has k fields and every data row
  # has more -- so diagnose it instead of guessing.
  fc_comma <- nf(",")
  if (length(fc_comma) >= 2 && fc_comma[1] >= 2 &&
      all(fc_comma[-1] > fc_comma[1]) &&
      any(grepl("[0-9],[0-9]", lines[-1])))
    stop("The header has ", fc_comma[1], " comma-separated fields but the data ",
         "rows have more. This looks like a European CSV written with decimal ",
         "commas (0,05 rather than 0.05); re-export it with a semicolon ",
         "separator or with decimal points.", call. = FALSE)

  best <- NULL
  for (sep in c(",", ";", "\t")) {
    fc  <- nf(sep)
    tab <- table(fc[fc > 1])
    if (!length(tab)) next
    rep_counts <- as.integer(names(tab)[tab >= 2])
    modal <- if (length(rep_counts)) max(rep_counts)
             else as.integer(names(tab)[which.max(tab)])
    if (modal <= 1) next
    score <- sum(fc == modal) * modal
    if (is.null(best) || score > best$score)
      best <- list(sep = sep, modal = modal, fc = fc, score = score)
  }
  if (is.null(best))
    stop("Could not find a consistent column separator (comma, semicolon and ",
         "tab were all tried). Check that the file is a plain-text table.",
         call. = FALSE)

  skip <- as.integer(keep[which(best$fc == best$modal)[1]] - 1L)   # raw lines above the header

  # decimal comma only makes sense when the separator is not a comma
  dec  <- "."
  body <- lines[best$fc == best$modal][-1]
  if (length(body) && best$sep != "," &&
      sum(grepl("[0-9],[0-9]", body)) >= 0.5 * length(body))
    dec <- ","

  data <- tryCatch(
    read.csv(path, sep = best$sep, dec = dec, skip = skip,
             stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e)
      stop("Could not read the file even after detecting a '",
           if (best$sep == "\t") "tab" else best$sep, "' separator and ",
           skip, " header line(s) to skip: ", conditionMessage(e), call. = FALSE))
  list(data = data, sep = best$sep, dec = dec, skip = skip, sniffed = TRUE)
}

# The app's reader. The locale-aware attempt (KC-11: semicolon/decimal-comma
# files) is tried first and kept whenever it yields >= 2 columns, rows and no
# reader preamble, so those files parse exactly as before; otherwise (a parse
# error, a single column because the wrong locale was picked, or a preamble above
# the header) the separator/preamble sniffer read_csv_smart() takes over.
read_csv_locale_aware <- function(path, locale = "auto") {
  loc <- if (identical(locale, "auto")) detect_csv_locale(path) else locale
  sep <- if (identical(loc, "eu")) ";" else ","
  dec <- if (identical(loc, "eu")) "," else "."
  plain <- tryCatch(
    read.csv(path, sep = sep, dec = dec, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) tryCatch(
      read.csv(path, sep = sep, dec = dec, stringsAsFactors = FALSE, check.names = TRUE),
      error = function(e2) NULL))
  if (!is.null(plain) && ncol(plain) >= 2 && nrow(plain) > 0 && csv_preamble_skip(path, sep) == 0L)
    return(plain)
  smart <- tryCatch(read_csv_smart(path), error = function(e) e)
  if (inherits(smart, "error")) {
    if (!is.null(plain)) return(plain)       # nothing better than the locale-aware parse
    stop(smart)
  }
  smart$data
}

# Strings a reader writes into a well that went out of range (Gen5 OVRFLW,
# i-control OVER, SoftMax Pro #SAT) make read.csv() type the whole column as
# character, and detect_od_column_idx()'s is.numeric() fallback then dropped that
# entire sample without a word. Coerce a mostly-numeric character column to
# numeric, every unparseable cell becoming NA and being reported, so one bad well
# costs one time point instead of the whole curve. min_numeric_frac keeps genuine
# text columns (notes, sample labels) out; skip_cols protects the time column and
# any group / replicate column. Returns list(data, report) where report is NULL or
# a data.frame(sample, time, value) of the cells set to NA.
coerce_sentinel_columns <- function(data, skip_cols = character(0),
                                    time_values = NULL,
                                    min_numeric_frac = 0.5) {
  report <- list()
  if (is.null(data) || ncol(data) == 0) return(list(data = data, report = NULL))
  for (j in seq_along(data)) {
    nm  <- colnames(data)[j]
    if (nm %in% skip_cols) next
    col <- data[[j]]
    if (is.numeric(col) || !(is.character(col) || is.factor(col))) next
    txt <- trimws(as.character(col))
    txt[is.na(txt)] <- ""
    num <- suppressWarnings(as.numeric(txt))
    nonblank <- nzchar(txt)
    if (!any(nonblank)) next
    if (sum(is.finite(num) & nonblank) / sum(nonblank) < min_numeric_frac) next
    bad <- which(nonblank & !is.finite(num))
    if (length(bad))
      report[[length(report) + 1]] <- data.frame(
        sample = nm,
        time   = if (!is.null(time_values) && length(time_values) == nrow(data))
                   as.character(time_values[bad]) else as.character(bad),
        value  = txt[bad], stringsAsFactors = FALSE)
    data[[j]] <- num
  }
  list(data = data,
       report = if (length(report)) do.call(rbind, report) else NULL)
}

# BioTek Gen5 and Tecan i-control write elapsed time as 0:15:00, and an Excel
# round-trip turns the column into a full datetime. Both are non-numeric, so the
# is.finite() row filter used to delete EVERY row. Returns list(values, converted,
# unit): values in MINUTES when converted (clock durations H:MM[:SS[.s]], or
# date-times as minutes from the earliest timestamp), otherwise the numeric
# values as they were.
parse_time_column <- function(x) {
  txt  <- trimws(as.character(x))
  txt[is.na(txt)] <- ""
  keep <- nzchar(txt)
  num  <- suppressWarnings(as.numeric(txt))
  if (any(keep) && all(is.finite(num[keep])))
    return(list(values = num, converted = FALSE, unit = NA_character_))

  out <- rep(NA_real_, length(txt))

  # 1. clock durations H:MM / H:MM:SS (optionally with fractional seconds)
  is_clock <- grepl("^[0-9]{1,5}:[0-9]{1,2}(:[0-9]{1,2}([.][0-9]+)?)?$", txt)
  if (any(is_clock)) {
    out[is_clock] <- vapply(strsplit(txt[is_clock], ":", fixed = TRUE), function(p) {
      p <- suppressWarnings(as.numeric(p))
      if (length(p) == 2) p[1] * 60 + p[2] else p[1] * 60 + p[2] + p[3] / 60
    }, numeric(1))
    out[is.finite(num)] <- num[is.finite(num)]
    return(list(values = out, converted = TRUE, unit = "minutes"))
  }

  # 2. datetimes -> minutes elapsed from the earliest timestamp
  fmts <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M:%S",
            "%Y-%m-%dT%H:%M:%S", "%d/%m/%Y %H:%M:%S", "%m/%d/%Y %H:%M:%S",
            "%m/%d/%Y %H:%M")
  for (f in fmts) {
    pt <- suppressWarnings(as.POSIXct(txt, format = f, tz = "UTC"))
    if (sum(!is.na(pt)) >= max(2, 0.8 * sum(keep)))
      return(list(values = as.numeric(difftime(pt, min(pt, na.rm = TRUE), units = "mins")),
                  converted = TRUE, unit = "minutes"))
  }

  # 3. whatever was already numeric; the caller decides what to do with the rest
  out[is.finite(num)] <- num[is.finite(num)]
  list(values = out, converted = FALSE, unit = NA_character_)
}

# Replicates in WIDE format from repeated (stacked) time blocks: the time counter
# restarts for each replicate block. Returns list(reps = number of blocks,
# block_id = per-row block number); reps/block_id NULL when the time column is
# unreadable.
infer_wide_replicates <- function(data, time_col) {
  if (is.null(data) || nrow(data) == 0) return(list(reps = NULL, block_id = NULL))
  t_vec <- suppressWarnings(as.numeric(as.character(data[[time_col]])))
  if (all(is.na(t_vec))) return(list(reps = NULL, block_id = NULL))
  resets <- which(diff(t_vec) < 0)
  if (length(resets) == 0) return(list(reps = 1L, block_id = rep(1L, nrow(data))))
  block_starts <- c(1L, resets + 1L)
  block_ends   <- c(resets, nrow(data))
  block_id <- integer(nrow(data))
  for (i in seq_along(block_starts)) block_id[block_starts[i]:block_ends[i]] <- i
  list(reps = length(block_starts), block_id = block_id)
}

# A replicate / well column in LONG format (never the time, group or value column).
detect_replicate_column <- function(data, time_col, group_col, value_col) {
  if (is.null(data) || nrow(data) == 0) return(NULL)
  ignore_cols <- c(time_col, group_col, value_col)
  names_lower <- tolower(colnames(data))
  rep_hits <- names_lower %in% c("replicate", "rep", "rep_id", "replicate_id",
                                 "well", "well_id", "wellid",
                                 "technical_replicate", "biological_replicate", "repeat")
  rep_cols <- setdiff(colnames(data)[rep_hits], ignore_cols)
  if (length(rep_cols) > 0) return(rep_cols[1])
  NULL
}

parse_excluded_timepoints <- function(text, all_timepoints) {
  if (is.null(text) || nchar(trimws(text)) == 0) return(numeric(0))
  parts <- strsplit(trimws(text), "[,; ]+")[[1]]
  vals  <- suppressWarnings(as.numeric(parts))
  vals  <- vals[!is.na(vals)]
  if (length(vals) == 0 || is.null(all_timepoints)) return(numeric(0))
  matched <- sapply(vals, function(v) {
    diffs <- abs(all_timepoints - v)
    tol   <- max(abs(all_timepoints)) * 0.001 + 0.001
    if (min(diffs) <= tol) all_timepoints[which.min(diffs)] else NA_real_
  })
  unique(matched[!is.na(matched)])
}

# ── Curve metrics ─────────────────────────────────────────────────────────────
# All metrics are computed from the per-sample MEAN curve exactly as plotted
# (i.e. after time filtering / point exclusion), sorted by time.
#
# The growth/lysis engine (mu_max, lag_phase, stat_phase_dur, lysis_time,
# lysis_rate, od_drop, residual_od, recovery_slope, and the reference-based
# infection metrics) is a port of calculate_growth_metrics() /
# calculate_infection_metrics() from the companion analysis app used for the
# N4 lysis paper ("Lysis Curve Ap 26.03.13.R"), verified against it by a
# parity test suite. The rolling regression is implemented in base R (no zoo
# dependency) with identical centered-window semantics.
#
# Two DELIBERATE corrections vs the reference implementation:
#  1. Infection metrics: the reference uses ifelse() with a length-1
#     condition, which silently recycles the FIRST sample's relative value
#     to every sample. Here each sample gets its own correct ratio.
#  2. mu_max requires a rolling slope > 1e-10 (reference: > 0), so an exactly
#     flat curve is not reported as growing with doubling time ~ 1e17 due to
#     floating-point noise in the regression.

# Centered rolling slope of y ~ t over consecutive index windows of `win`
# points; replicates zoo::rollapply(..., align = "center", fill = NA).
rolling_slope <- function(t, y, win) {
  n   <- length(t)
  out <- rep(NA_real_, n)
  if (win > n || win < 2) return(out)
  offset <- floor((win - 1) / 2)   # zoo places even-width windows left of center
  for (s in seq_len(n - win + 1)) {
    idx <- s:(s + win - 1)
    tt  <- t[idx]; yy <- y[idx]
    out[s + offset] <- if (diff(range(tt)) == 0) 0 else
      unname(coef(lm(yy ~ tt))[2])
  }
  out
}

# KC-3: simple centered moving average, used ONLY to locate the peak (see
# peak_smooth_window below) -- not to alter any reported OD value. Edge
# points use a smaller, still-centered window rather than padding with NA,
# so every point gets a value. w = 1 (the default) is a no-op: half = 0
# makes every window just the point itself, returning y unchanged.
moving_average <- function(y, w) {
  n <- length(y)
  if (!is.finite(w) || w <= 1 || n < 2) return(y)
  half <- (w - 1) %/% 2
  vapply(seq_len(n), function(i) {
    lo <- max(1, i - half); hi <- min(n, i + half)
    mean(y[lo:hi], na.rm = TRUE)
  }, numeric(1))
}

# LC-3 (ported from curve_noise_sd() in "Lysis Curve Ap 26.03.13.R"): robust
# point-to-point noise SD from second differences. For white noise the second
# difference y[i] - 2y[i+1] + y[i+2] has variance 6*sigma^2, so
# sigma = median(|d2|) * 1.4826 / sqrt(6). The MEDIAN is what makes it usable:
# real curvature (growth shoulder, lysis kink) shows up in a handful of large
# second differences and cannot inflate the noise floor. Returns 0 when there
# are fewer than 5 finite points (too little data to estimate anything).
curve_noise_sd <- function(y) {
  y <- y[is.finite(y)]
  n <- length(y)
  if (n < 5) return(0)
  d2 <- y[1:(n - 2)] - 2 * y[2:(n - 1)] + y[3:n]
  s  <- stats::median(abs(d2)) * 1.4826 / sqrt(6)
  if (!is.finite(s) || s < 0) return(0)
  s
}

# LC-3: number of consecutive post-peak points that must sit below the lysis
# detection threshold before a decline counts as lysis. One point below a fixed
# 5 % of peak scored a clean 4 %-CV uninfected control as lysing in 39 % of
# simulated runs (and the app's own demo Uninfected replicates).
LYSIS_SUSTAIN_PTS <- 2L

# LC-17: start/end indices of the longest contiguous TRUE run in `flag`
# (first one on ties), or NULL when there is none. Used for stat_phase_dur.
longest_true_run <- function(flag) {
  flag <- !is.na(flag) & flag
  if (!any(flag)) return(NULL)
  r <- rle(flag)
  ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1L
  k <- which(r$values)
  k <- k[which.max(r$lengths[k])]
  c(start = starts[k], end = ends[k])
}

# ── New metrics (ported from the companion app, step 2) ──────────────────────
# 3-point centered moving mean with the RAW value kept at the two edges (and
# wherever the mean is not finite) -- the smoothing calculate_growth_metrics()
# uses to find the first prominent peak. Deliberately NOT moving_average(y, 3),
# whose edge windows shrink instead of falling back to the raw point.
smooth3_raw_edges <- function(y) {
  n <- length(y)
  if (n < 3) return(y)
  sm <- rep(NA_real_, n)
  sm[2:(n - 1)] <- (y[1:(n - 2)] + y[2:(n - 1)] + y[3:n]) / 3
  ifelse(is.finite(sm), sm, y)
}

# Port of first_prominent_peak_idx(): index of the EARLIEST local maximum of
# `sm` whose topographic prominence reaches `prom_min`, or NA when there is
# none. A regrowing culture's global maximum can be its SECOND peak, so
# anything anchored on which.max() misses the first one.
first_prominent_peak_idx <- function(sm, prom_min) {
  n <- length(sm)
  if (n < 3 || !is.finite(prom_min)) return(NA_integer_)
  cand <- which(sm[2:(n - 1)] >  sm[1:(n - 2)] &
                sm[2:(n - 1)] >= sm[3:n]) + 1L
  for (pk in cand) {
    p_val <- sm[pk]
    # walk out both ways until a higher point (or the curve end) is met; the
    # prominence is the drop to the shallower of the two saddles
    li <- pk
    while (li > 1 && sm[li - 1] <= p_val) li <- li - 1
    ri <- pk
    while (ri < n && sm[ri + 1] <= p_val) ri <- ri + 1
    left_min  <- min(sm[li:pk], na.rm = TRUE)
    right_min <- min(sm[pk:ri], na.rm = TRUE)
    prom <- p_val - max(left_min, right_min)
    if (is.finite(prom) && prom >= prom_min) return(pk)
  }
  NA_integer_
}

# Port of the centroid block: EXACT trapezoid-strip centroids of the area
# under the curve (each consecutive pair of points is a trapezoid whose own
# centroid is known in closed form; the curve centroid is the area-weighted
# mean of the strip centroids). Returns c(x, y); NA, NA for < 2 points or a
# non-positive total area.
curve_centroid <- function(t_vec, od_vec) {
  n <- length(t_vec)
  if (n < 2) return(c(x = NA_real_, y = NA_real_))
  h_s <- diff(t_vec)
  a_s <- head(od_vec, -1)
  b_s <- tail(od_vec, -1)
  A_s <- h_s * (a_s + b_s) / 2
  ab  <- a_s + b_s
  xbar_s <- ifelse(ab == 0, head(t_vec, -1) + h_s / 2,
                   head(t_vec, -1) + h_s * (a_s + 2 * b_s) / (3 * ab))
  ybar_s <- ifelse(ab == 0, 0, (a_s^2 + a_s * b_s + b_s^2) / (3 * ab))
  A_tot  <- sum(A_s, na.rm = TRUE)
  if (!is.finite(A_tot) || A_tot <= 0) return(c(x = NA_real_, y = NA_real_))
  c(x = sum(xbar_s * A_s, na.rm = TRUE) / A_tot,
    y = sum(ybar_s * A_s, na.rm = TRUE) / A_tot)
}

# Port of the extinction block: first time the curve falls below the floor
# `thr` (default 0.05 OD, as in the reference), linearly interpolated between
# the last point at/above it and the first below. A curve that STARTS below the
# floor reports its first time point. NA when it never falls below.
curve_extinction_time <- function(t_vec, od_vec, thr = 0.05) {
  n <- length(t_vec)
  thr <- suppressWarnings(as.numeric(thr))
  if (n < 1 || length(thr) != 1 || !is.finite(thr)) return(NA_real_)
  if (isTRUE(od_vec[1] < thr)) return(t_vec[1])
  if (n < 2) return(NA_real_)
  cross <- which(od_vec[-1] < thr & od_vec[-n] >= thr)
  if (length(cross) == 0) return(NA_real_)
  j  <- cross[1]
  y0 <- od_vec[j]; y1 <- od_vec[j + 1]; x0 <- t_vec[j]; x1 <- t_vec[j + 1]
  if (y0 == y1) x1 else x0 + (x1 - x0) * (y0 - thr) / (y0 - y1)
}

# KC-4: the full per-curve metrics engine, factored out into its own function
# so the EXACT SAME formulas run on a per-replicate curve
# (compute_replicate_metrics(), below) as on the mean curve
# (compute_curve_metrics(), below) -- not a re-implementation that could
# silently drift from it. Returns one row of metrics for a single
# (t_vec, od_vec) curve.
#
# peak_smooth_window: OFF by default (1 = no smoothing). At the app's own demo
# noise level (~4%), which(max)/lysis-onset detection on the raw curve can
# move by a whole timepoint between otherwise-identical noise draws (see
# AUDIT-2026-09.md, KC-3), which cascades into lysis_time/lysis_rate/od_drop/
# residual_od/lysis_efficiency/recovery_ratio/t_half_max_decline, all anchored
# on the peak index. When > 1, the peak (and only the peak) is located on a
# centered moving average of the curve instead of the raw curve; every
# reported OD value still comes from the raw data at that index -- smoothing
# only decides WHERE the peak is, never WHAT its value is.
#
# lysis_threshold (v2.12.4): the decline from the peak, as a fraction of the
# peak, that counts as lysis (default 0.05 = 5 %, the paper's definition).
#
# peak_mode (v2.12.2): "global" (default, unchanged) takes the global maximum as
# the peak. "first_local" is for regrowth / resistance curves, where the global
# maximum is the late regrowth and every lysis metric would be NA: the peak is
# the first point that is the running maximum AND is followed by a decline of
# more than 10 % (v2.12.3; was 5 %, which 4 % replicate noise on a plateau can
# trigger) before the curve rises above it again (see first_local_peak_index).
PEAK_DECLINE_FRACTION <- 0.10
first_local_peak_index <- function(peak_vec) {
  n <- length(peak_vec)
  run_max <- -Inf
  for (i in seq_len(n - 1)) {
    v <- peak_vec[i]
    if (!is.finite(v) || v < run_max) next      # not a running maximum
    run_max <- v
    later <- peak_vec[(i + 1):n]
    j <- which(later < (1 - PEAK_DECLINE_FRACTION) * v)[1]   # first decline of more than 10 %
    # The rule as first specified is "some later point is below the threshold". Taken
    # literally it also accepts every point on the way UP (a later lysis trough
    # is lower than the inoculum, so t = 0 would be "the peak"). Requiring that
    # the curve has not exceeded v by the time it first declines keeps the rule
    # identical for a real peak and rejects the rising points.
    if (!is.na(j) && !any(later[seq_len(j)] > v, na.rm = TRUE)) return(i)
  }
  which.max(peak_vec)
}
compute_one_curve_metrics <- function(t_vec, od_vec, sample_name,
                                      smooth_window = 5, peak_smooth_window = 1,
                                      peak_mode = c("global", "first_local"),
                                      lysis_threshold = 0.05,
                                      extinction_threshold = 0.05) {
    peak_mode <- match.arg(peak_mode)
    n_pts  <- length(t_vec)
    peak_vec <- if (peak_smooth_window > 1) moving_average(od_vec, peak_smooth_window) else od_vec
    max_idx <- if (peak_mode == "first_local") first_local_peak_index(peak_vec) else which.max(peak_vec)
    min_idx <- which.min(od_vec)
    initial_od <- od_vec[1]
    max_od     <- od_vec[max_idx]
    final_od   <- od_vec[n_pts]

    auc <- if (n_pts >= 2)
      sum(diff(t_vec) * (head(od_vec, -1) + tail(od_vec, -1)) / 2) else NA_real_

    # ── mu_max + lag phase via centered rolling regression on log(OD) ────────
    # (paper method: window of `smooth_window` points; lag = first time the
    # rolling slope reaches 10% of mu_max)
    mu_max <- NA_real_; lag_phase <- NA_real_
    win <- min(smooth_window, n_pts)
    # KC-2: a non-positive OD has no logarithm. The old clamp,
    # log(pmax(od_vec, 1e-9)), mapped e.g. a -0.002 blank-subtraction
    # artefact (routine after plate-reader blanking) onto log(1e-9) = -20.7
    # -- about twenty log units below every real reading -- so the rolling
    # window containing it fit an enormous artificial slope that always won
    # max(), inflating mu_max ~2.8x on a representative test curve and
    # understating doubling time to match. Treat non-positive OD as missing
    # for this log-scale metric instead of clamping it: any window that
    # touches one gets no slope at all, and the count is reported below so
    # an affected sample is easy to spot.
    n_nonpositive_od <- sum(is.finite(od_vec) & od_vec <= 0)
    if (n_pts >= win && win >= 3) {
      pos_od <- is.finite(od_vec) & od_vec > 0
      log_od <- rep(NA_real_, n_pts)
      log_od[pos_od] <- log(od_vec[pos_od])
      roll_slopes <- tryCatch(rolling_slope(t_vec, log_od, win),
                              error = function(e) rep(NA_real_, n_pts))
      if (n_nonpositive_od > 0) {
        offset <- floor((win - 1) / 2)
        for (s in seq_len(n_pts - win + 1)) {
          idx <- s:(s + win - 1)
          if (any(!pos_od[idx])) roll_slopes[s + offset] <- NA_real_
        }
      }
      # deviation from the reference engine: require slopes > 1e-10 (not > 0)
      # so floating-point noise on a flat curve is not reported as growth
      # (the reference can emit mu_max ~ 1e-18 -> doubling time ~ 1e17)
      valid <- which(is.finite(roll_slopes) & roll_slopes > 1e-10)
      if (length(valid) > 0) {
        mu_max    <- max(roll_slopes[valid], na.rm = TRUE)
        threshold <- 0.10 * mu_max
        lag_idx   <- which(roll_slopes >= threshold & is.finite(roll_slopes))
        lag_phase <- if (length(lag_idx) > 0) t_vec[lag_idx[1]] else NA_real_
      }
    }
    doubling_time <- if (is.finite(mu_max) && mu_max > 0) log(2) / mu_max else NA_real_

    # ── Stationary phase duration: time spent within 95% of max OD ───────────
    # LC-17: the LONGEST CONTIGUOUS run of points >= 95 % of the peak, not the
    # span from the first to the last such point. The span ran straight
    # through a lysis trough (or any dip) between two separate near-peak
    # episodes -- the opposite of "time spent near the maximum". A single
    # near-peak point gives no duration (NA), as before.
    stat_phase_dur <- NA_real_
    if (max_idx > 1) {
      run <- longest_true_run(is.finite(od_vec) & od_vec >= 0.95 * max_od)
      if (!is.null(run) && run[["end"]] > run[["start"]])
        stat_phase_dur <- t_vec[run[["end"]]] - t_vec[run[["start"]]]
    }

    # ── Lysis detection ──────────────────────────────────────────────────────
    # Detection and reporting are deliberately separate (LC-3 / LC-9; ported
    # from calculate_growth_metrics() in the companion app).
    #   DETECT (LC-3): the post-peak decline must exceed
    #     max(lysis_threshold * peak, 3 * curve_noise_sd(od))
    #     for LYSIS_SUSTAIN_PTS consecutive points. A single point 5 % below
    #     a noisy peak is noise, not lysis.
    #   REPORT (LC-9): once lysis is called, lysis_time is the LINEARLY
    #     INTERPOLATED time at which the curve first falls lysis_threshold *
    #     peak below the peak (the noise term is NOT in the interpolation, so
    #     the number does not move with the curve's noise). The old value was
    #     the first sampled point already past the threshold -- late by up to
    #     one acquisition interval and not comparable across sampling rates.
    # rate  = linear-regression slope from peak to the post-peak trough
    lysis_time <- NA_real_; lysis_rate <- NA_real_
    od_drop <- NA_real_; residual_od <- NA_real_; recovery_slope <- NA_real_
    noise_sd <- curve_noise_sd(od_vec)
    if (max_idx < n_pts) {
      post_peak  <- (max_idx + 1):n_pts
      detect_thr <- max(lysis_threshold * max_od, 3 * noise_sd)
      report_thr <- lysis_threshold * max_od
      od_decline <- max_od - od_vec[post_peak]
      below      <- is.finite(od_decline) & od_decline > detect_thr
      need       <- as.integer(LYSIS_SUSTAIN_PTS)
      onset      <- NA_integer_
      if (length(below) >= need) {
        runs <- vapply(seq_len(length(below) - need + 1L),
                       function(i) all(below[i:(i + need - 1L)]), logical(1))
        if (any(runs)) onset <- which(runs)[1]
      }
      # A decline of more than half the peak is lysis even when the series ends
      # right after it (e.g. a sample read only until it lysed): the sustained-
      # run rule guards against noise, not against an unambiguous crash.
      if (is.na(onset)) {
        big <- is.finite(od_decline) & od_decline > 0.5 * max_od
        if (any(big)) onset <- which(big)[1]
      }
      if (!is.na(onset)) {
        # the report level is crossed at or before the sustained run starts
        cross   <- which(is.finite(od_decline) & od_decline > report_thr &
                         seq_along(od_decline) <= onset)
        abs_idx <- post_peak[if (length(cross)) cross[1] else onset]
        level   <- max_od - report_thr
        y0 <- od_vec[abs_idx - 1L]; y1 <- od_vec[abs_idx]
        x0 <- t_vec[abs_idx - 1L];  x1 <- t_vec[abs_idx]
        lysis_time <- if (is.finite(y0) && is.finite(y1) && y0 > level && y0 > y1)
                        x0 + (x1 - x0) * (y0 - level) / (y0 - y1)
                      else t_vec[abs_idx]
        trough_idx <- max_idx + which.min(od_vec[post_peak])
        if (trough_idx > max_idx) {
          lw <- max_idx:trough_idx
          if (length(lw) >= 2)
            lysis_rate <- tryCatch(
              unname(coef(lm(od_vec[lw] ~ t_vec[lw]))[2]),
              error = function(e) NA_real_)
        }
        post_min    <- min(od_vec[post_peak], na.rm = TRUE)
        od_drop     <- max_od - post_min
        residual_od <- post_min
        if (trough_idx < n_pts) {
          rw <- trough_idx:n_pts
          if (length(rw) >= 3) {
            rec <- tryCatch(unname(coef(lm(od_vec[rw] ~ t_vec[rw]))[2]),
                            error = function(e) NA_real_)
            recovery_slope <- if (is.finite(rec) && rec > 0) rec else NA_real_
          }
        }
      }
    }

    # ── Additional transparent metrics ───────────────────────────────────────
    half <- max_od * 0.5
    t50  <- NA_real_
    if (max_idx < n_pts) {
      at <- t_vec[max_idx:n_pts]; ay <- od_vec[max_idx:n_pts]
      below <- which(ay < half)
      if (length(below)) {
        j <- below[1]
        if (j == 1) t50 <- at[1]
        else {
          t1 <- at[j-1]; t2 <- at[j]; y1 <- ay[j-1]; y2 <- ay[j]
          t50 <- if (y2 == y1) t2 else t1 + (half - y1) * (t2 - t1) / (y2 - y1)
        }
      }
    }
    dec_rate <- NA_real_; dec_int <- NA_character_
    if (n_pts >= 2) {
      dt <- diff(t_vec)
      slopes <- ifelse(dt > 0, diff(od_vec) / dt, NA_real_)
      if (any(is.finite(slopes))) {
        k <- which.min(ifelse(is.finite(slopes), slopes, Inf))
        dec_rate <- slopes[k]
        dec_int  <- paste0(t_vec[k], "–", t_vec[k+1])
      }
    }

    # ── First prominent peak, extinction time, area centroid (companion-app port) ──
    # first_peak_od / t_first_peak: the earliest local maximum of the lightly
    # (3-point) smoothed curve whose prominence is >= 5 % of that curve's range
    # (Blazanin et al., PNAS 2025). The definition is deliberately the plain
    # published one (no noise term; AUDIT LC-18). With no prominent peak (or
    # < 3 points) it falls back to the GLOBAL maximum, as the reference does,
    # whatever peak_mode is. The reported OD is the raw value at that index.
    fp_idx <- NA_integer_
    if (n_pts >= 3 && all(is.finite(od_vec))) {
      sm_vec   <- smooth3_raw_edges(od_vec)
      sm_range <- diff(range(sm_vec))
      fp_idx   <- tryCatch(first_prominent_peak_idx(sm_vec, 0.05 * sm_range),
                           error = function(e) NA_integer_)
    }
    if (is.na(fp_idx)) fp_idx <- which.max(od_vec)
    first_peak_od <- od_vec[fp_idx]
    t_first_peak  <- t_vec[fp_idx]
    extinction_time <- curve_extinction_time(t_vec, od_vec, extinction_threshold)
    cen <- curve_centroid(t_vec, od_vec)

    data.frame(
      sample          = sample_name,
      n_timepoints    = n_pts,
      t_start         = t_vec[1],
      t_end           = t_vec[n_pts],
      initial_od      = initial_od,
      max_od          = max_od,
      time_max_od     = t_vec[max_idx],
      # KC-10: named global_min_od (not min_od) because on a lysis curve
      # "minimum OD" invites the other reading -- the post-lysis floor,
      # which is residual_od, not this. This is genuinely the curve's
      # global minimum, usually the inoculum at t_start.
      global_min_od      = od_vec[min_idx],
      time_global_min_od = t_vec[min_idx],
      final_od        = final_od,
      auc             = auc,
      mu_max          = mu_max,
      n_nonpositive_od = n_nonpositive_od,
      doubling_time   = doubling_time,
      lag_phase       = lag_phase,
      stat_phase_dur  = stat_phase_dur,
      lysis_time      = lysis_time,
      lysis_rate      = lysis_rate,
      od_drop         = od_drop,
      residual_od     = residual_od,
      recovery_slope  = recovery_slope,
      lysis_efficiency = if (max_od > 0 && is.finite(od_drop)) od_drop / max_od else NA_real_,
      recovery_ratio  = if (max_od > 0) final_od / max_od else NA_real_,
      fold_change_final_vs_initial = if (initial_od != 0) final_od / initial_od else NA_real_,
      pct_drop_from_max = if (max_od != 0) (max_od - final_od) / max_od * 100 else NA_real_,
      t_half_max_decline        = t50,
      steepest_decline_rate     = dec_rate,
      steepest_decline_interval = dec_int,
      first_peak_od   = first_peak_od,
      t_first_peak    = t_first_peak,
      extinction_time = extinction_time,
      centroid_x      = unname(cen[["x"]]),
      centroid_y      = unname(cen[["y"]]),
      stringsAsFactors = FALSE
    )
}

# ref_sample: optional name of an uninfected/reference sample; when given (and
# present in the data) the five reference-normalised infection metrics are added.
compute_curve_metrics <- function(pd, smooth_window = 5, ref_sample = NULL,
                                  peak_smooth_window = 1, peak_mode = "global",
                                  lysis_threshold = 0.05, extinction_threshold = 0.05) {
  if (is.null(pd) || nrow(pd) == 0) return(NULL)
  pd <- pd[order(pd$variable, pd$time), , drop = FALSE]
  out <- lapply(split(pd, pd$variable, drop = TRUE), function(g) {
    compute_one_curve_metrics(g$time, g$mean_value, g$variable[1],
                              smooth_window, peak_smooth_window, peak_mode, lysis_threshold,
                              extinction_threshold)
  })
  m <- do.call(rbind, c(out, list(make.row.names = FALSE)))

  # ── Reference-normalised infection metrics (paper method) ──────────────────
  if (!is.null(ref_sample) && nzchar(ref_sample) && ref_sample %in% m$sample) {
    ref <- m[m$sample == ref_sample, , drop = FALSE]
    ref_auc    <- mean(ref$auc,    na.rm = TRUE)
    ref_mu_max <- mean(ref$mu_max, na.rm = TRUE)
    ref_max_od <- mean(ref$max_od, na.rm = TRUE)
    m$relative_growth    <- if (is.finite(ref_auc) && ref_auc > 0)
      m$auc / ref_auc else NA_real_
    m$infection_strength <- if (is.finite(ref_auc) && ref_auc > 0)
      1 - (m$auc / ref_auc) else NA_real_
    m$relative_mu_max    <- if (is.finite(ref_mu_max) && ref_mu_max > 0)
      m$mu_max / ref_mu_max else NA_real_
    m$relative_max_od    <- if (is.finite(ref_max_od) && ref_max_od > 0)
      m$max_od / ref_max_od else NA_real_
    m$lysis_onset_delta  <- m$lysis_time - m$lag_phase
    m$centroid_index     <- centroid_index_vs_ref(m$centroid_x, m$centroid_y, ref$centroid_x, ref$centroid_y)
    lv <- calculate_local_virulence(pd, ref_sample, smooth_window)
    m$local_virulence    <- if (is.null(lv)) NA_real_ else lv$local_virulence[match(m$sample, lv$sample)]
  }
  m
}

# Centroid Index (Hosseini et al. 2024; port of calculate_infection_metrics()):
# 1 - (cx * cy) / (ref_cx * ref_cy). The denominator is the MEAN of the reference
# curves' cx*cy products, not the product of the column means: with one reference
# curve the two coincide, with replicates only this form makes the reference's own
# index average to exactly 0 (E[XY] != E[X]E[Y]). NA when the reference product is
# not a positive finite number.
centroid_index_vs_ref <- function(cx, cy, ref_cx, ref_cy) {
  ref_prod <- mean(ref_cx * ref_cy, na.rm = TRUE)
  if (!is.finite(ref_prod) || ref_prod <= 0) return(rep(NA_real_, length(cx)))
  1 - (cx * cy) / ref_prod
}

# Storms local virulence (PHAGE 1(1):27-36, 2020), port of
# calculate_local_virulence(): the "virulence index" area ratio
# 1 - A_sample / A_reference, integrated only up to the onset of stationary phase
# (t_stat) in the REFERENCE culture -- unlike infection_strength, which integrates
# the whole run. t_stat defaults to the first time AFTER the reference's mu_max
# window at which the reference's centered rolling ln-slope falls below 10 % of
# mu_max (the run's last time when there is none); `t_stat_override` replaces it.
# Operates on time / variable / mean_value[, replicate] data so it also works per
# replicate; the reference curve is the mean over the reference's replicates at each
# time. Returns sample[, replicate], local_virulence, t_stat_used; NULL when the
# reference is absent. The reference sample's own value is 0.
calculate_local_virulence <- function(pd, ref_sample, smooth_window = 5,
                                      t_stat_override = NULL) {
  if (is.null(pd) || nrow(pd) == 0 || is.null(ref_sample) || !nzchar(ref_sample)) return(NULL)
  if (!ref_sample %in% pd$variable) return(NULL)
  has_rep <- "replicate" %in% names(pd)

  rg <- pd[pd$variable == ref_sample & is.finite(pd$time) & is.finite(pd$mean_value), , drop = FALSE]
  if (nrow(rg) == 0) return(NULL)
  t_ref  <- sort(unique(rg$time))
  od_ref <- vapply(t_ref, function(tt) mean(rg$mean_value[rg$time == tt]), numeric(1))
  n_ref  <- length(t_ref)
  if (n_ref < 2) return(NULL)

  t_stat <- t_ref[n_ref]
  win <- min(smooth_window, n_ref)
  if (n_ref >= win && win >= 3) {
    # same non-positive-OD rule as mu_max: a window containing a reading <= 0 has no slope
    pos_ref <- od_ref > 0
    log_od  <- rep(NA_real_, n_ref); log_od[pos_ref] <- log(od_ref[pos_ref])
    roll <- tryCatch(rolling_slope(t_ref, log_od, win), error = function(e) rep(NA_real_, n_ref))
    off <- floor((win - 1) / 2)
    for (s in seq_len(n_ref - win + 1)) {
      idx <- s:(s + win - 1)
      if (any(!pos_ref[idx])) roll[s + off] <- NA_real_
    }
    valid <- which(is.finite(roll) & roll > 1e-10)
    if (length(valid) > 0) {
      mu_max_ref <- max(roll[valid])
      mu_idx     <- valid[which.max(roll[valid])]
      after <- which(seq_len(n_ref) > mu_idx & is.finite(roll) & roll < 0.10 * mu_max_ref)
      if (length(after) > 0) t_stat <- t_ref[after[1]]
    }
  }
  if (!is.null(t_stat_override)) {
    ov <- suppressWarnings(as.numeric(t_stat_override))
    if (length(ov) == 1 && is.finite(ov)) t_stat <- ov
  }

  # trapezoid integral up to t_cut, interpolating a point AT t_cut
  area_to <- function(tt, yy, t_cut) {
    ok <- is.finite(tt) & is.finite(yy)
    tt <- tt[ok]; yy <- yy[ok]
    o  <- order(tt); tt <- tt[o]; yy <- yy[o]
    if (length(tt) < 2) return(NA_real_)
    keep <- which(tt <= t_cut)
    if (length(keep) == 0) return(NA_real_)
    k  <- max(keep)
    tk <- tt[seq_len(k)]; yk <- yy[seq_len(k)]
    if (k < length(tt) && tt[k] < t_cut) {
      frac <- (t_cut - tt[k]) / (tt[k + 1] - tt[k])
      tk <- c(tk, t_cut)
      yk <- c(yk, yy[k] + frac * (yy[k + 1] - yy[k]))
    }
    if (length(tk) < 2) return(NA_real_)
    sum(diff(tk) * (head(yk, -1) + tail(yk, -1)) / 2)
  }
  A0 <- area_to(t_ref, od_ref, t_stat)

  key <- if (has_rep) interaction(pd$variable, pd$replicate, drop = TRUE, lex.order = TRUE)
         else factor(pd$variable)
  rows <- lapply(split(seq_len(nrow(pd)), key, drop = TRUE), function(ix) {
    d  <- pd[ix, , drop = FALSE]
    Ai <- area_to(d$time, d$mean_value, t_stat)
    r  <- data.frame(sample = d$variable[1],
                     local_virulence = if (is.finite(A0) && A0 > 0 && is.finite(Ai)) 1 - Ai / A0 else NA_real_,
                     t_stat_used = t_stat, stringsAsFactors = FALSE)
    if (has_rep) r$replicate <- d$replicate[1]
    r
  })
  res <- do.call(rbind, c(rows, list(make.row.names = FALSE)))
  res[, if (has_rep) c("sample", "replicate", "local_virulence", "t_stat_used")
        else c("sample", "local_virulence", "t_stat_used"), drop = FALSE]
}

# KC-4: compute_curve_metrics() above runs the metrics engine ONCE per
# sample, on the already-averaged mean curve -- so every one of its 27+
# metrics is a single number with no replicate-to-replicate uncertainty and
# no path to "is this difference real". This is additive alongside it, not
# a replacement: it runs compute_one_curve_metrics() once per INDIVIDUAL
# replicate curve, then reports mean +/- SD across replicates for every
# metric. The two are NOT the same number -- the mean of per-replicate
# metrics is generally not equal to the metric of the mean curve, because
# most of these metrics (mu_max, lysis_time, ...) are nonlinear functions
# of the curve -- so both are kept, clearly labelled, rather than one being
# presented as if it were the other.
#
# rep_pd: long, UN-averaged data with columns time, variable, replicate,
# value (see prepare_replicate_data() in the server). "replicate" is an
# arbitrary but stable per-sample index -- for wide files it is the
# replicate column's position (matching rv$col_map); for long files with no
# explicit replicate-ID column it is the row's occurrence order within its
# (sample, time) group, which assumes same-replicate rows appear in a
# consistent relative order across time points.
# v2.11: the per-replicate rows (one metrics row per replicate curve) are
# factored out so the analysis figures and the statistics can use them
# directly. With ref_sample, the reference-normalised metrics are added per
# replicate, each replicate divided by the reference sample's replicate MEAN
# (same definitions as compute_curve_metrics).
compute_replicate_metric_rows <- function(rep_pd, smooth_window = 5, peak_smooth_window = 1,
                                          ref_sample = NULL, peak_mode = "global",
                                          lysis_threshold = 0.05) {
  if (is.null(rep_pd) || nrow(rep_pd) == 0) return(NULL)
  rep_pd <- rep_pd[is.finite(rep_pd$value), , drop = FALSE]
  if (nrow(rep_pd) == 0) return(NULL)
  rep_pd <- rep_pd[order(rep_pd$variable, rep_pd$replicate, rep_pd$time), , drop = FALSE]
  rep_key <- interaction(rep_pd$variable, rep_pd$replicate, drop = TRUE)

  per_rep <- lapply(split(rep_pd, rep_key, drop = TRUE), function(g) {
    row <- compute_one_curve_metrics(g$time, g$value, g$variable[1],
                                     smooth_window, peak_smooth_window, peak_mode, lysis_threshold)
    row$replicate <- g$replicate[1]
    row
  })
  rm <- do.call(rbind, c(per_rep, list(make.row.names = FALSE)))
  if (!is.null(ref_sample) && nzchar(ref_sample) && ref_sample %in% rm$sample) {
    ref     <- rm[rm$sample == ref_sample, , drop = FALSE]
    ref_auc <- mean(ref$auc,    na.rm = TRUE)
    ref_mu  <- mean(ref$mu_max, na.rm = TRUE)
    ref_max <- mean(ref$max_od, na.rm = TRUE)
    rm$relative_growth    <- if (is.finite(ref_auc) && ref_auc > 0) rm$auc / ref_auc        else NA_real_
    rm$infection_strength <- if (is.finite(ref_auc) && ref_auc > 0) 1 - rm$auc / ref_auc    else NA_real_
    rm$relative_mu_max    <- if (is.finite(ref_mu)  && ref_mu  > 0) rm$mu_max / ref_mu      else NA_real_
    rm$relative_max_od    <- if (is.finite(ref_max) && ref_max > 0) rm$max_od / ref_max     else NA_real_
    # centroid index: per replicate vs the reference's mean cx*cy product
    rm$centroid_index     <- centroid_index_vs_ref(rm$centroid_x, rm$centroid_y, ref$centroid_x, ref$centroid_y)
    # local virulence: each replicate's area to t_stat vs the reference's replicate-mean curve
    lv <- calculate_local_virulence(
      data.frame(time = rep_pd$time, variable = rep_pd$variable, replicate = rep_pd$replicate,
                 mean_value = rep_pd$value, stringsAsFactors = FALSE),
      ref_sample, smooth_window)
    rm$local_virulence    <- if (is.null(lv)) NA_real_ else
      lv$local_virulence[match(paste(rm$sample, rm$replicate), paste(lv$sample, lv$replicate))]
  }
  rm
}

compute_replicate_metrics <- function(rep_pd, smooth_window = 5, peak_smooth_window = 1,
                                      peak_mode = "global", lysis_threshold = 0.05) {
  rm <- compute_replicate_metric_rows(rep_pd, smooth_window, peak_smooth_window,
                                      peak_mode = peak_mode, lysis_threshold = lysis_threshold)
  if (is.null(rm)) return(NULL)

  safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  safe_sd   <- function(x) if (sum(!is.na(x)) < 2) NA_real_ else sd(x, na.rm = TRUE)

  metric_cols <- setdiff(names(rm)[vapply(rm, is.numeric, logical(1))],
                         c("replicate"))
  out <- lapply(split(rm, rm$sample, drop = TRUE), function(g) {
    row <- data.frame(sample = g$sample[1], n_replicates = nrow(g),
                      stringsAsFactors = FALSE)
    for (cc in metric_cols) {
      row[[paste0(cc, "_mean")]] <- safe_mean(g[[cc]])
      row[[paste0(cc, "_sd")]]   <- safe_sd(g[[cc]])
    }
    row
  })
  do.call(rbind, c(out, list(make.row.names = FALSE)))
}

METRIC_DEFINITIONS <- c(
  n_timepoints    = "Number of time points in the (filtered) mean curve",
  t_start         = "First time point plotted",
  t_end           = "Last time point plotted",
  initial_od      = "Mean value at the first time point",
  max_od          = "Maximum (peak) of the mean curve or, with peak mode 'first_local', the first peak followed by a > 10 % decline",
  time_max_od     = "Time at which the peak occurs (global maximum or, with peak mode 'first_local', the first peak followed by a > 10 % decline)",
  global_min_od      = "Global minimum of the mean curve (usually the inoculum, near t_start) -- NOT the post-lysis floor; see residual_od for that",
  time_global_min_od = "Time at which the global minimum occurs",
  final_od        = "Mean value at the last time point",
  auc             = "Area under the mean curve (trapezoidal rule)",
  mu_max          = "Maximum specific growth rate: largest centered rolling-window regression slope of ln(mean) vs time (window = smoothing setting, default 5 points); NA if the curve never grows",
  n_nonpositive_od = "Count of time points with OD <= 0 (e.g. a blank-subtraction artefact). Any mu_max rolling window touching one of these is excluded rather than clamped, so mu_max/doubling_time/lag_phase may be NA or based on fewer windows when this is > 0",
  doubling_time   = "ln(2) / mu_max; NA when there is no growth phase",
  lag_phase       = "First time at which the rolling ln-slope reaches 10% of mu_max",
  stat_phase_dur  = "Duration of the longest contiguous run of points within 95% of the peak (separate near-peak episodes are not joined across a dip)",
  lysis_time      = "Lysis onset: linearly interpolated time at which the mean falls the lysis threshold (adjustable, default 5%) of the peak below the peak. Called only when the post-peak decline exceeds max(threshold x peak, 3 x the curve's noise SD) for 2 consecutive points, so noise on an uninfected plateau is not scored as lysis; NA otherwise",
  lysis_rate      = "Linear-regression slope of the mean from the peak to the post-peak trough (negative = decline)",
  od_drop         = "Peak minus post-peak minimum (absolute decline)",
  residual_od     = "Post-peak minimum (residual OD after lysis) -- this is the post-lysis floor; see global_min_od for the curve's overall minimum",
  recovery_slope  = "Positive linear-regression slope from the post-peak trough to the end (regrowth / burst proxy); NA if no recovery",
  lysis_efficiency = "od_drop / max_od (fraction of the peak lost to lysis)",
  recovery_ratio  = "final_od / max_od",
  fold_change_final_vs_initial = "final_od / initial_od (NA if initial is 0)",
  pct_drop_from_max = "(max_od - final_od) / max_od x 100",
  t_half_max_decline = "First time after the peak at which the mean falls below 50% of the peak (linear interpolation); NA if never",
  steepest_decline_rate = "Most negative slope between consecutive mean points",
  steepest_decline_interval = "Time interval over which the steepest decline occurs",
  first_peak_od   = "OD at the first prominent peak: the earliest local maximum of the 3-point smoothed curve whose prominence is at least 5% of the curve's range (Blazanin et al., PNAS 2025); the global maximum when there is no prominent peak. Unlike max_od it is not displaced by regrowth that overtakes the first peak",
  t_first_peak    = "Time of the first prominent peak (see first_peak_od)",
  extinction_time = "First time the mean falls below the extinction floor (0.05 OD), linearly interpolated between the last point at or above it and the first below; the first time point if the curve starts below the floor; NA if it never falls below (Blazanin et al., PNAS 2025)",
  centroid_x      = "Time coordinate of the centroid of the area under the mean curve (exact trapezoid-strip centroid)",
  centroid_y      = "OD coordinate of the centroid of the area under the mean curve (exact trapezoid-strip centroid)",
  relative_growth    = "auc / auc(reference sample)",
  infection_strength = "1 - auc / auc(reference sample)",
  relative_mu_max    = "mu_max / mu_max(reference sample)",
  relative_max_od    = "max_od / max_od(reference sample)",
  lysis_onset_delta  = "lysis_time - lag_phase",
  centroid_index     = "1 - (centroid_x * centroid_y) / (centroid_x * centroid_y of the reference sample); Centroid Index, Hosseini et al. 2024 (0 for the reference itself)",
  local_virulence    = "Storms local virulence: 1 - area under the mean curve / area under the reference curve, both integrated only up to the onset of stationary phase in the reference (first time after its fastest growth that its rolling ln-slope falls below 10% of mu_max; the last time if none); 0 for the reference itself (Storms et al., PHAGE 2020)"
)

# ── Report helpers ────────────────────────────────────────────────────────────

fmt_sig <- function(x, digits = 5) {
  if (is.null(x)) return("")
  vapply(x, function(v) {
    if (is.na(v)) return("NA")
    if (is.numeric(v)) format(signif(v, digits), trim = TRUE, scientific = FALSE)
    else as.character(v)
  }, character(1))
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  gsub(">", "&gt;",  x, fixed = TRUE)
}

html_table <- function(df, digits = 5, class = NULL) {
  if (is.null(df) || nrow(df) == 0) return("<p><em>No data.</em></p>")
  head_row <- paste0("<th>", html_escape(colnames(df)), "</th>", collapse = "")
  body <- vapply(seq_len(nrow(df)), function(i) {
    cells <- vapply(seq_len(ncol(df)), function(j) {
      v <- df[i, j]
      txt <- if (is.numeric(v)) fmt_sig(v, digits) else html_escape(as.character(v))
      if (is.na(df[i, j])) txt <- "NA"
      txt <- gsub("\n", "<br/>", txt, fixed = TRUE)
      paste0("<td>", txt, "</td>")
    }, character(1))
    paste0("<tr>", paste0(cells, collapse = ""), "</tr>")
  }, character(1))
  cls <- if (is.null(class)) "" else paste0(" class='", class, "'")
  paste0("<div class='tblwrap'><table", cls, "><thead><tr>", head_row,
         "</tr></thead><tbody>",
         paste0(body, collapse = ""), "</tbody></table></div>")
}

# ── v2.17: data quality flags ────────────────────────────────────────────────
# Cheap sanity checks, one row (sample, flag, detail) per problem; zero rows when the data are clean.
#   CEILING         a replicate reading above the OD ceiling (plate-reader linearity is lost)
#   NEGATIVE        a replicate reading <= 0 (blank over-subtraction; no logarithm)
#   GAP             a time step at least 2x the median step of that sample (a missing reading)
#   FLAT_REFERENCE  the reference sample's max OD is below 2x its initial OD (it barely grew, so every
#                   ratio to it is unreliable)
# rep_pd: prepare_replicate_data() (time, variable, replicate, value); pd: prepare_plot_data() (the mean
# curves). Ported from the companion app's qc_flags(), reduced to the checks that matter for a plotter.
# "At least 2x" (not "more than"): one missing reading in a regular series makes exactly 2x the median step.
qc_flags <- function(rep_pd, pd, ref_sample = NULL, ceiling = 2.5) {
  empty <- data.frame(sample = character(0), flag = character(0), detail = character(0),
                      stringsAsFactors = FALSE)
  ceiling <- suppressWarnings(as.numeric(ceiling))
  if (length(ceiling) != 1 || !is.finite(ceiling)) ceiling <- 2.5
  rows <- list()
  add <- function(s, f, d)
    rows[[length(rows) + 1L]] <<- data.frame(sample = s, flag = f, detail = d, stringsAsFactors = FALSE)
  num <- function(x) signif(x, 4)

  if (!is.null(rep_pd) && nrow(rep_pd) > 0 && all(c("variable", "value") %in% names(rep_pd))) {
    for (s in unique(as.character(rep_pd$variable))) {
      v <- rep_pd$value[as.character(rep_pd$variable) == s]
      v <- v[is.finite(v)]
      if (!length(v)) next
      if (any(v > ceiling))
        add(s, "CEILING", paste0(sum(v > ceiling), " reading(s) above OD ", format(ceiling, trim = TRUE),
                                 " (max ", num(max(v)), "): plate-reader linearity is lost at high OD; dilute or calibrate"))
      if (any(v <= 0))
        add(s, "NEGATIVE", paste0(sum(v <= 0), " reading(s) <= 0 (min ", num(min(v)),
                                  "): blank over-subtraction? They cannot be drawn on a log axis"))
    }
  }

  if (!is.null(pd) && nrow(pd) > 0 && all(c("time", "variable", "mean_value") %in% names(pd))) {
    for (s in unique(as.character(pd$variable))) {
      d  <- pd[as.character(pd$variable) == s & is.finite(pd$time) & is.finite(pd$mean_value), , drop = FALSE]
      if (nrow(d) == 0) next
      d  <- d[order(d$time), , drop = FALSE]
      tt <- sort(unique(d$time))
      if (length(tt) >= 3) {
        dts <- diff(tt); med <- stats::median(dts)
        if (is.finite(med) && med > 0) {
          gi <- which(dts >= 2 * med * (1 - 1e-9))
          if (length(gi))
            add(s, "GAP", paste0("time step(s) of at least 2x the median step (", num(med), ") after t = ",
                                 paste(num(tt[gi]), collapse = ", "), ": a reading is missing or was excluded"))
        }
      }
      if (!is.null(ref_sample) && length(ref_sample) == 1 && !is.na(ref_sample) && nzchar(ref_sample) &&
          identical(s, as.character(ref_sample))) {
        init <- d$mean_value[1]; mx <- max(d$mean_value)
        if (is.finite(init) && init > 0 && mx < 2 * init)
          add(s, "FLAT_REFERENCE", paste0("the reference barely grew (max OD ", num(mx), " < 2 x initial OD ",
                                          num(init), "): ratios to it (normalized OD, infection strength) are unreliable"))
      }
    }
  }
  if (!length(rows)) empty else do.call(rbind, rows)
}

# ── v2.11.1: analysis figures + replicate statistics in the reports ──────────
# Both report builders take an optional `analysis` list (NULL = omitted, and
# the output is byte-identical to v2.11.0):
#   figures      list of list(type, title, caption, plot | b64, error)
#   stats        the compare_replicate_metric() result
#   metric_label human-readable name of the tested metric
#   ref_sample   "" for all-pairs comparisons
#   method       "welch" | "nonparametric"
# Same formatting as the app's on-screen statistics tables (3 significant
# figures, an em dash for non-finite values).
fmt_stats_df <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  for (cc in names(df)) if (is.numeric(df[[cc]]))
    df[[cc]] <- ifelse(is.finite(df[[cc]]), formatC(signif(df[[cc]], 3), format = "g"), "—")
  df
}

STATS_DEFINITIONS <- paste0(
  "Welch's t-test compares two sample means without assuming equal variances. ",
  "p values across the pairwise comparisons are Holm-adjusted (step-down Bonferroni; ",
  "controls the family-wise error rate). Hedges' g is the standardized mean difference ",
  "(difference divided by the pooled SD) with a small-sample bias correction; |g| of 0.2, ",
  "0.5 and 0.8 are conventionally small, medium and large. The 95% CI is the confidence ",
  "interval of the difference in means from the Welch test. With three or more groups, ",
  "one-way ANOVA tests whether any group mean differs and Tukey's HSD then compares every ",
  "pair with family-wise error control. The nonparametric alternatives are the ",
  "Mann-Whitney U (Wilcoxon rank-sum) test for pairs and the Kruskal-Wallis test for three ",
  "or more groups; they compare distributions by rank and do not assume normality. ",
  "Stars: *** p < 0.001, ** p < 0.01, * p < 0.05, ns = not significant. With three ",
  "biological replicates power is low: a non-significant result is not evidence of no difference.")

# How the summary-statistics columns are computed (the Statistics tab; same wording as the README).
STATS_N_NOTE <- paste0("Per sample and time point: mean, SD, n, SEM = SD / \u221an, and the 95% CI half-width ",
                       "(t\u2080.\u2089\u2087\u2085,\u2099\u208b\u2081 \u00d7 SEM). n counts only non-missing replicate values; ",
                       "with a single replicate SD, SEM and CI are NA.")

# One-sentence description of what was tested and how.
stats_lead_text <- function(analysis) {
  st <- analysis$stats
  if (is.null(st$pairwise))
    return("No comparison possible: the statistics need at least two samples with 2+ replicate values for this metric.")
  ref <- analysis$ref_sample %||% ""
  paste0("Metric: ", analysis$metric_label %||% st$metric, ". ",
         if (nzchar(ref) && all(st$pairwise$versus == ref)) paste0("Each sample vs reference '", ref, "'. ")
         else "All pairs of samples. ",
         if (identical(analysis$method, "nonparametric"))
           "Mann-Whitney U per pair, Holm-adjusted; Kruskal-Wallis across 3+ groups."
         else "Welch's t-test per pair, Holm-adjusted; one-way ANOVA + Tukey HSD across 3+ groups.")
}

report_analysis_html <- function(analysis) {
  figs <- analysis$figures %||% list()
  cap_style <- "font-size:0.9em;color:#495057;margin-top:6px;"
  fig_html <- vapply(figs, function(f) {
    body <- if (!is.null(f$b64))
      paste0("<img class='figure' src='data:image/png;base64,", f$b64,
             "' alt='", html_escape(f$title), "'/>")
    else paste0("<p><em>Figure not available: ", html_escape(f$error %||% "unknown reason"), "</em></p>")
    paste0("<h3>", html_escape(f$title), "</h3>", body,
           if (nzchar(f$caption %||% "")) paste0("<p style='", cap_style, "'>", html_escape(f$caption), "</p>") else "")
  }, character(1))
  st <- analysis$stats
  warn_html <- paste0(vapply(st$warnings %||% character(0), function(w)
    paste0("<p style='background:#fff4e6;border:1px solid #ffd8a8;border-radius:6px;padding:8px 12px;'>",
           html_escape(w), "</p>"), character(1)), collapse = "")
  tbl <- function(df, title) if (is.null(df) || nrow(df) == 0) "" else
    paste0("<h3>", title, "</h3>", html_table(fmt_stats_df(df)))
  paste0(
    "<h2>Analysis figures</h2>\n",
    "<p>Rendered at the export size with the main figure's theme; each is the figure the ",
    "Analysis figures tab shows for the current samples, filtering and settings.</p>\n",
    paste0(fig_html, collapse = "\n"),
    "\n<h2>Replicate statistics</h2>\n",
    "<p>", html_escape(stats_lead_text(analysis)), "</p>\n",
    warn_html,
    tbl(st$pairwise, "Pairwise comparisons"),
    tbl(st$omnibus,  "Omnibus test"),
    tbl(st$posthoc,  "Post-hoc comparisons (Tukey HSD)"),
    "<h3>Definitions</h3>\n<p style='font-size:0.9em;'>", html_escape(STATS_DEFINITIONS), "</p>\n")
}

# Builds the entire self-contained HTML report as one string.
# meta: named list with title, author, notes, generated, data_file, format,
#       time_col, samples_txt, filter_txt, error_txt, settings_df
# analysis: optional (v2.11.1), see report_analysis_html(); NULL omits it.
build_html_report <- function(meta, img_b64, stats_df, metrics_df, analysis = NULL) {
  analysis_html <- if (is.null(analysis)) "" else report_analysis_html(analysis)
  def_keys <- if (!is.null(metrics_df))
    intersect(names(METRIC_DEFINITIONS), colnames(metrics_df))
  else names(METRIC_DEFINITIONS)
  defs_df <- data.frame(metric = def_keys,
                        definition = unname(METRIC_DEFINITIONS[def_keys]),
                        stringsAsFactors = FALSE)
  si <- utils::sessionInfo()
  pkg_names <- sort(names(si$otherPkgs %||% list()))
  pkg_txt <- paste(vapply(pkg_names, function(p)
    paste0(p, " ", as.character(si$otherPkgs[[p]]$Version)), character(1)),
    collapse = ", ")

  notes_html <- if (nzchar(trimws(meta$notes %||% "")))
    paste0("<h2>Notes</h2><p class='notes'>",
           gsub("\n", "<br/>", html_escape(meta$notes)), "</p>") else ""

  notebook_html <- if (!is.null(meta$notebook_df))
    paste0("<h2>Experiment notebook</h2>",
           html_table(meta$notebook_df, class = "wrap prov")) else ""

  # v2.16: the editable methods paragraph (Statistics tab), only when there is text; paragraphs split on blank lines
  methods_html <- if (nzchar(trimws(meta$methods %||% "")))
    paste0("<h2>Methods</h2>",
           paste0("<p class='methods'>", gsub("\n", "<br/>",
                    html_escape(trimws(strsplit(meta$methods, "\n[ \t]*\n")[[1]])), fixed = TRUE), "</p>", collapse = "")) else ""

  # v2.17: QC flags (after provenance) and the figure legend (right under the main figure); both are
  # omitted when the meta list does not carry them
  qc_html <- if (is.null(meta$qc_df)) "" else
    paste0("<h2>QC flags</h2>",
           if (nrow(meta$qc_df) == 0) "<p>No data quality flags were raised (readings above the OD ceiling, readings <= 0, time-step gaps, a flat reference).</p>"
           else html_table(meta$qc_df, class = "wrap"))
  legend_html <- if (nzchar(trimws(meta$legend %||% "")))
    paste0("<h3>Figure legend</h3><p class='legend'>", gsub("\n", "<br/>", html_escape(trimws(meta$legend))), "</p>") else ""

  font_pt <- meta$font_pt %||% 14

  paste0(
"<!DOCTYPE html><html><head><meta charset='utf-8'>
<title>", html_escape(meta$title), "</title>
<style>
  html { background: #ffffff; }
  body { font-family: -apple-system, 'Segoe UI', Helvetica, Arial, sans-serif;
         max-width: 1000px; margin: 24px auto; padding: 0 16px;
         color: #212529; background: #ffffff; font-size: ", font_pt, "px; }
  h1 { border-bottom: 3px solid #343a40; padding-bottom: 8px; margin-bottom: 4px; }
  h2 { color: #343a40; border-bottom: 1px solid #dee2e6; padding-bottom: 4px; margin-top: 32px; }
  .meta { color: #6c757d; font-size: 0.9em; margin-bottom: 20px; }
  .tblwrap { overflow-x: auto; }
  table { border-collapse: collapse; font-size: 0.85em; margin: 12px 0; }
  th, td { border: 1px solid #dee2e6; padding: 5px 9px; text-align: left; white-space: nowrap; }
  th { background: #f1f3f5; }
  tr:nth-child(even) td { background: #fafbfc; }
  img.figure { max-width: 100%; height: auto; border: 1px solid #dee2e6; }
  .notes { background: #fff9db; border: 1px solid #ffe066; border-radius: 6px; padding: 12px; }
  .methods { text-align: justify; line-height: 1.5; }
  .legend { text-align: justify; line-height: 1.45; font-size: 0.92em; color: #343a40; }
  .prov td:first-child { font-weight: bold; background: #f1f3f5; }
  table.wrap td { white-space: normal; }
  footer { margin-top: 40px; color: #adb5bd; font-size: 0.8em; border-top: 1px solid #dee2e6; padding-top: 8px; }
  @media print { body { max-width: none; } h2 { page-break-after: avoid; } .tblwrap { overflow-x: visible; } }
</style></head><body>
<h1>", html_escape(meta$title), "</h1>
<div class='meta'>",
  if (nzchar(meta$author %||% "")) paste0(html_escape(meta$author), " &middot; ") else "",
  "Generated ", html_escape(meta$generated), "</div>",
notes_html,
"<h2>Data provenance</h2>
<div class='tblwrap'><table class='prov'>
<tr><td>Data file</td><td>",        html_escape(meta$data_file),   "</td></tr>
<tr><td>Detected format</td><td>",  html_escape(meta$format),      "</td></tr>
<tr><td>Time column</td><td>",      html_escape(meta$time_col),    "</td></tr>
<tr><td>Samples plotted</td><td>",  html_escape(meta$samples_txt), "</td></tr>
<tr><td>Time filtering</td><td>",   html_escape(meta$filter_txt),  "</td></tr>
<tr><td>Error display</td><td>",    html_escape(meta$error_txt),   "</td></tr>
</table></div>",
qc_html,
notebook_html,
methods_html,
"<h2>Figure</h2>
<img class='figure' src='data:image/png;base64,", img_b64, "' alt='Lysis curve figure'/>
", legend_html, analysis_html, "<h2>Summary statistics</h2>
<p>Per sample and time point, computed from the currently loaded (and filtered) data.
SEM = SD / &radic;n; 95% CI = t<sub>0.975, n-1</sub> &times; SEM.</p>",
html_table(stats_df),
"<h2>Curve metrics</h2>
<p>Computed from each sample's mean curve as plotted.</p>",
html_table(metrics_df),
"<h3>Metric definitions</h3>",
html_table(defs_df),
"<h2>Key visual settings</h2>",
html_table(meta$settings_df),
"<h2>Reproducibility</h2>
<p>", html_escape(paste0(si$R.version$version.string, "; platform ", si$platform)), "</p>
<p style='font-size:0.85em;color:#6c757d;'>Attached packages: ", html_escape(pkg_txt), "</p>
<footer>Lysis Curve OD Visualization App v", APP_VERSION, " &middot; Michael Baffour Awuah / Ramsey Lab</footer>
</body></html>")
}

# Builds a multi-page PDF report (no LaTeX/pandoc; base pdf device + gridExtra).
# Page 1: title + provenance. Page 2: figure. Later pages: stats + metrics tables.
# analysis (v2.11.1, optional): one page per analysis figure right after the
# main figure, then a replicate-statistics text page and paginated tables.
build_pdf_report <- function(file, meta, plot_obj, stats_df, metrics_df,
                             width = 10, height = 8, analysis = NULL) {
  stopifnot(requireNamespace("gridExtra", quietly = TRUE))
  # v2.14: the figure fonts are Arial by default, which grDevices::pdf() (base-14 PostScript fonts
  # only) cannot draw; open the same font-embedding device the image export uses.
  pdf_dev <- export_device("pdf")
  if (is.function(pdf_dev)) pdf_dev(file, width = width, height = height)
  else grDevices::pdf(file, width = width, height = height, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)

  fscale <- (meta$font_pt %||% 14) / 14   # report font size control
  tt  <- gridExtra::ttheme_default(
    core    = list(fg_params = list(cex = 0.55 * fscale)),
    colhead = list(fg_params = list(cex = 0.6 * fscale)))

  # ── Page 1: title + provenance ──
  grid::grid.newpage()
  grid::grid.text(meta$title, y = 0.90, gp = grid::gpar(fontsize = 22 * fscale, fontface = "bold"))
  sub <- paste0(if (nzchar(meta$author %||% "")) paste0(meta$author, "  •  ") else "",
                "Generated ", meta$generated)
  grid::grid.text(sub, y = 0.84, gp = grid::gpar(fontsize = 11 * fscale, col = "grey40"))
  prov <- c(paste0("Data file: ",       meta$data_file),
            paste0("Detected format: ", meta$format),
            paste0("Time column: ",     meta$time_col),
            paste0("Samples: ",         meta$samples_txt),
            paste0("Time filtering: ",  meta$filter_txt),
            paste0("Error display: ",   meta$error_txt),
            if (!is.null(meta$qc_df))   # v2.17
              paste0("QC flags: ", if (nrow(meta$qc_df) == 0) "none" else paste0(nrow(meta$qc_df), " (listed on the next page)")))
  grid::grid.text(paste(prov, collapse = "\n"), y = 0.68, just = "top",
                  gp = grid::gpar(fontsize = 11 * fscale, lineheight = 1.4))
  if (nzchar(trimws(meta$notes %||% ""))) {
    grid::grid.text(paste0("Notes:\n", meta$notes), y = 0.40, just = "top",
                    gp = grid::gpar(fontsize = 10 * fscale, lineheight = 1.3, col = "grey20"))
  }

  # ── QC flags page (v2.17; only when a flag was raised) ──
  if (!is.null(meta$qc_df) && nrow(meta$qc_df) > 0) {
    q <- meta$qc_df
    lines <- unlist(lapply(seq_len(nrow(q)), function(i)
      c(strwrap(paste0(q$flag[i], "  ", q$sample[i], ": ", q$detail[i]), 105, exdent = 4), "")), use.names = FALSE)
    grid::grid.newpage()
    grid::grid.text("QC flags", y = 0.97, gp = grid::gpar(fontsize = 13 * fscale, fontface = "bold"))
    grid::grid.text(paste(lines, collapse = "\n"), x = 0.04, y = 0.92, just = c("left", "top"),
                    gp = grid::gpar(fontsize = 9.5 * fscale, lineheight = 1.3))
  }

  # ── Experiment notebook page (only when fields were filled in) ──
  if (!is.null(meta$notebook_df)) {
    nb <- meta$notebook_df
    nb$value <- vapply(nb$value, function(s)
      paste(strwrap(s, 60), collapse = "\n"), character(1))
    grid::grid.newpage()
    grid::grid.text("Experiment notebook", y = 0.97,
                    gp = grid::gpar(fontsize = 13 * fscale, fontface = "bold"))
    g <- gridExtra::tableGrob(nb, rows = NULL, theme = tt)
    gw <- grid::convertWidth(sum(g$widths), "npc", valueOnly = TRUE)
    vp <- if (gw > 0.98) grid::viewport(y = 0.5, width = 0.98 / gw) else grid::viewport(y = 0.5)
    grid::pushViewport(vp); grid::grid.draw(g); grid::popViewport()
  }

  # ── Methods pages (v2.16; only when the Statistics tab's text box is non-empty) ──
  if (nzchar(trimws(meta$methods %||% ""))) {
    paras <- trimws(strsplit(meta$methods, "\n[ \t]*\n")[[1]])
    wrapped <- unlist(lapply(paras, function(s) c(strwrap(s, 100), "")), use.names = FALSE)
    per_page <- 46L
    for (p0 in seq(1L, length(wrapped), by = per_page)) {
      grid::grid.newpage()
      grid::grid.text("Methods", y = 0.97, gp = grid::gpar(fontsize = 13 * fscale, fontface = "bold"))
      grid::grid.text(paste(wrapped[p0:min(p0 + per_page - 1L, length(wrapped))], collapse = "\n"), x = 0.04, y = 0.92,
                      just = c("left", "top"), gp = grid::gpar(fontsize = 9.5 * fscale, lineheight = 1.3))
    }
  }

  # ── Figure page ──
  # v2.17: the figure legend sits right under the figure (a continuation page when the figure leaves no room)
  legend_txt <- trimws(meta$legend %||% "")
  has_legend <- nzchar(legend_txt)
  pg_in <- grDevices::dev.size("in")
  legend_wrapped <- if (has_legend)
    paste(c("Figure legend", strwrap(legend_txt, max(40, floor(pg_in[1] * 0.92 * 15)))), collapse = "\n") else ""
  draw_legend <- function(y) grid::grid.text(legend_wrapped, x = 0.04, y = y, just = c("left", "top"),
                                             gp = grid::gpar(fontsize = 9 * fscale, lineheight = 1.3, col = "grey20"))
  if (inherits(plot_obj, "gtable") && !is.null(attr(plot_obj, "size_mm"))) {   # v2.15: fixed-panel figure at its size
    grid::grid.newpage()
    sz <- attr(plot_obj, "size_mm")
    if (has_legend) {
      fig_h_npc <- sz[2] / 25.4 / pg_in[2]
      grid::pushViewport(grid::viewport(y = 0.97, just = "top", width = grid::unit(sz[1], "mm"), height = grid::unit(sz[2], "mm")))
      grid::grid.draw(plot_obj); grid::popViewport()
      leg_y <- 0.97 - fig_h_npc - 0.03
      n_lines <- length(strsplit(legend_wrapped, "\n")[[1]])
      if (leg_y - n_lines * 9 * fscale * 1.3 / 72 / pg_in[2] > 0.02) draw_legend(leg_y)
      else { grid::grid.newpage(); draw_legend(0.95) }
    } else {
      grid::pushViewport(grid::viewport(width = grid::unit(sz[1], "mm"), height = grid::unit(sz[2], "mm")))
      grid::grid.draw(plot_obj); grid::popViewport()
    }
  } else if (has_legend) {
    grid::grid.newpage()
    print(plot_obj, newpage = FALSE, vp = grid::viewport(y = 1, height = 0.8, just = "top"))
    draw_legend(0.17)
  } else print(plot_obj)

  # ── Analysis figure pages (v2.11.1): title, figure, caption; one per page ──
  text_page <- function(title, paragraphs, wrap = 95) {
    grid::grid.newpage()
    grid::grid.text(title, y = 0.97, gp = grid::gpar(fontsize = 13 * fscale, fontface = "bold"))
    body <- paste(vapply(paragraphs, function(s) paste(strwrap(s, wrap), collapse = "\n"), character(1)),
                  collapse = "\n\n")
    grid::grid.text(body, x = 0.04, y = 0.92, just = c("left", "top"),
                    gp = grid::gpar(fontsize = 9.5 * fscale, lineheight = 1.3))
  }
  for (f in analysis$figures %||% list()) {
    drawn <- FALSE
    if (!is.null(f$plot)) {
      drawn <- tryCatch({
        grid::grid.newpage()
        grid::grid.text(f$title, y = 0.97, gp = grid::gpar(fontsize = 13 * fscale, fontface = "bold"))
        vp <- if (inherits(f$plot, "gtable") && !is.null(attr(f$plot, "size_mm"))) {
          # fixed-panel figures are absolute-size: drawn at their physical size, centred in the page area
          grid::viewport(y = 0.53, width = grid::unit(attr(f$plot, "size_mm")[1], "mm"),
                         height = grid::unit(attr(f$plot, "size_mm")[2], "mm"))
        } else if (is.null(f$aspect)) grid::viewport(y = 0.53, height = 0.82) else {
          # a composed figure keeps its own aspect ratio inside the page area
          pg <- grDevices::dev.size("in"); bw <- 0.95 * pg[1]; bh <- 0.82 * pg[2]
          if (f$aspect * bw <= bh) grid::viewport(y = 0.53, width = grid::unit(bw, "in"), height = grid::unit(f$aspect * bw, "in"))
          else grid::viewport(y = 0.53, width = grid::unit(bh / f$aspect, "in"), height = grid::unit(bh, "in"))
        }
        grid::pushViewport(vp)
        if (inherits(f$plot, "gtable")) grid::grid.draw(f$plot) else print(f$plot, newpage = FALSE)
        grid::popViewport()
        if (nzchar(f$caption %||% ""))
          grid::grid.text(paste(strwrap(f$caption, 110), collapse = "\n"), y = 0.08, just = "top",
                          gp = grid::gpar(fontsize = 9 * fscale, lineheight = 1.25, col = "grey20"))
        TRUE
      }, error = function(e) { f$error <<- conditionMessage(e); FALSE })
    }
    if (!drawn) text_page(f$title, paste0("Figure not available: ", f$error %||% "unknown reason"))
  }

  # ── Stats table pages (chunked) ──
  fmt_df <- function(df) {
    df2 <- df
    for (j in seq_along(df2)) if (is.numeric(df2[[j]])) df2[[j]] <- fmt_sig(df2[[j]])
    df2
  }
  page_table <- function(df, title, rows_per_page = 28) {
    df <- fmt_df(df)
    n  <- nrow(df)
    starts <- seq(1, n, by = rows_per_page)
    for (s in starts) {
      chunk <- df[s:min(s + rows_per_page - 1, n), , drop = FALSE]
      grid::grid.newpage()
      grid::grid.text(paste0(title,
                             if (length(starts) > 1)
                               paste0("  (rows ", s, "–", min(s + rows_per_page - 1, n),
                                      " of ", n, ")") else ""),
                      y = 0.97, gp = grid::gpar(fontsize = 13 * fscale, fontface = "bold"))
      g <- gridExtra::tableGrob(chunk, rows = NULL, theme = tt)
      # scale down if wider than page
      gw <- grid::convertWidth(sum(g$widths), "npc", valueOnly = TRUE)
      vp <- if (gw > 0.98) grid::viewport(y = 0.5, width = 0.98 / gw) else grid::viewport(y = 0.5)
      grid::pushViewport(vp); grid::grid.draw(g); grid::popViewport()
    }
  }
  # ── Replicate statistics pages (v2.11.1): same place as in the HTML report,
  #    between the figures and the summary-statistics table ──
  if (!is.null(analysis)) {
    st <- analysis$stats
    text_page("Replicate statistics",
              c(stats_lead_text(analysis), st$warnings %||% character(0),
                paste0("Definitions. ", STATS_DEFINITIONS)))
    if (!is.null(st$pairwise)) page_table(fmt_stats_df(st$pairwise), "Replicate statistics: pairwise comparisons")
    if (!is.null(st$omnibus))  page_table(fmt_stats_df(st$omnibus),  "Replicate statistics: omnibus test")
    if (!is.null(st$posthoc))  page_table(fmt_stats_df(st$posthoc),  "Replicate statistics: post-hoc (Tukey HSD)")
  }
  page_table(stats_df,   "Summary statistics (mean / SD / n / SEM / 95% CI)")
  if (!is.null(metrics_df)) {
    # metrics are wide; page through blocks of columns, repeating the sample col
    other  <- setdiff(seq_len(ncol(metrics_df)), 1)
    blocks <- split(other, ceiling(seq_along(other) / 7))
    for (bi in seq_along(blocks)) {
      mb <- metrics_df[, c(1, blocks[[bi]]), drop = FALSE]
      page_table(mb, paste0("Curve metrics (", bi, " of ", length(blocks), ")"))
    }
    defs_df <- data.frame(metric = intersect(names(METRIC_DEFINITIONS), colnames(metrics_df)),
                          stringsAsFactors = FALSE)
    defs_df$definition <- unname(METRIC_DEFINITIONS[defs_df$metric])
    defs_df$definition <- vapply(defs_df$definition, function(s)
      paste(strwrap(s, 70), collapse = "\n"), character(1))
    page_table(defs_df, "Metric definitions", rows_per_page = 14)
  }
  invisible(file)
}

# ── Demo datasets (used by the "Load Demo Data" button) ──────────────────────
# v2.12.1: `scenario` picks one of five built-in examples so every figure has a
# meaningful demo. All share one style: logistic growth, exponential lysis,
# ~4% multiplicative noise, 3 replicates as duplicated column names.
# "lysis" (the default) is the original v2.8 dataset, unchanged.
DEMO_SCENARIOS <- c("lysis", "moi_series", "resistance", "hours", "two_phages")
make_demo_data <- function(scenario = "lysis") {
  scenario <- match.arg(scenario, DEMO_SCENARIOS)
  set.seed(42)
  grow  <- function(t, k = 0.028, y0 = 0.05, cap = 1.4)
    cap / (1 + ((cap - y0) / y0) * exp(-k * t))
  # exponential decline after t_lysis; `resid` (optional) is the fraction of the
  # pre-lysis OD that survives, i.e. an incomplete lysis
  lyse  <- function(t, t_lysis, y_pre, rate = 0.06, resid = NULL) {
    lo <- if (is.null(resid)) 0.02 else pmax(resid * y_pre(t_lysis), 0.02)
    ifelse(t < t_lysis, y_pre(t), pmax(y_pre(t_lysis) * exp(-rate * (t - t_lysis)), lo))
  }
  noisy <- function(y) pmax(y * (1 + rnorm(length(y), 0, 0.04)), 0.01)
  wt    <- function(t) grow(t)
  # build with duplicate column names (replicates), as documented
  build <- function(times, defs) {
    m <- do.call(cbind, lapply(defs, function(f) replicate(3, noisy(f(times)))))
    colnames(m) <- rep(names(defs), each = 3)
    cbind(data.frame(Time = times, check.names = FALSE), as.data.frame(m, check.names = FALSE))
  }

  switch(scenario,
    lysis = {
      # higher MOI infects more cells at once -> earlier lysis (ph1 lyses at t=60)
      build(seq(0, 180, by = 15), list(
        "Uninfected"    = wt,
        "Phage MOI 10"  = function(t) lyse(t, 60,  wt),
        "Phage MOI 0.1" = function(t) lyse(t, 105, wt)))
    },
    moi_series = {
      # lysis onset falls and lysis rate rises with log10(MOI)
      onset <- c(150, 120, 90, 60, 45)
      rate  <- c(0.03, 0.04, 0.05, 0.06, 0.08)
      moi   <- c("0.01", "0.1", "1", "10", "100")
      defs  <- list("Uninfected" = wt)
      for (i in seq_along(moi)) local({
        j <- i
        defs[[paste("Phage MOI", moi[j])]] <<- function(t) lyse(t, onset[j], wt, rate[j])
      })
      build(seq(0, 240, by = 15), defs)
    },
    resistance = {
      # both infected samples lyse at 90 min; the second regrows from 150 min
      # (resistant mutants) towards ~60% of the uninfected plateau
      y_cap <- 0.6 * 1.4
      lysed <- function(t) lyse(t, 90, wt, 0.05)
      regrow <- function(t) {
        y0 <- lysed(150)
        ifelse(t < 150, lysed(t),
               y_cap / (1 + ((y_cap - y0) / y0) * exp(-0.035 * (t - 150))))
      }
      build(seq(0, 360, by = 15), list(
        "Uninfected"              = wt,
        "Phage MOI 1"             = lysed,
        "Phage MOI 1 + resistant" = regrow))
    },
    hours = {
      # the "lysis" shapes on a 0-8 h axis (1 h = 22.5 min of the original)
      to_min <- 22.5
      build(seq(0, 8, by = 0.5), list(
        "Control"       = function(t) wt(t * to_min),
        "Phage A MOI 5" = function(t) lyse(t * to_min, 60,  wt),
        "Phage B MOI 5" = function(t) lyse(t * to_min, 105, wt)))
    },
    two_phages = {
      # phage B lyses later and less completely than phage A
      build(seq(0, 240, by = 15), list(
        "Uninfected"     = wt,
        "Phage A MOI 1"  = function(t) lyse(t, 90,  wt, 0.05),
        "Phage B MOI 1"  = function(t) lyse(t, 150, wt, 0.025, resid = 0.4),
        "Phage A MOI 10" = function(t) lyse(t, 60,  wt, 0.06),
        "Phage B MOI 10" = function(t) lyse(t, 105, wt, 0.04,  resid = 0.25)))
    })
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── v2.10: figure-standard helpers ───────────────────────────────────────────
# ggplot2 `linewidth` is not in pt: stroke(pt) = linewidth * .pt * 0.75
# (2.134 pt per unit, measured against svglite output). Journals specify line
# weights in pt, so the new UI controls take pt and convert here.
pt2lw <- function(pt) pt / (ggplot2::.pt * 0.75)
lw2pt <- function(lw) lw * ggplot2::.pt * 0.75

# Column widths in mm. Nature values are from its official figure guide; the
# others are the journals' author instructions as quoted by secondary sources
# (docs/FIGURE_STANDARDS.md lists them) -- confirm on the journal page before
# submission. max_h = maximum figure height the journal allows (mm).
JOURNAL_PRESETS <- list(
  nature  = list(label = "Nature family (89 / 120 / 183 mm)",      single = 89,  onehalf = 120, double = 183, max_h = 170),
  cell    = list(label = "Cell Press (85 / 114 / 174 mm)",         single = 85,  onehalf = 114, double = 174, max_h = 230),
  asm     = list(label = "ASM journals (84 / 175 mm; <= 7 x 9 in)", single = 84, onehalf = NA,  double = 175, max_h = 228),
  plos    = list(label = "PLOS (text column 132 mm; max 190 mm)",  single = 132, onehalf = NA,  double = 190, max_h = 225),
  science = list(label = "Science (55 / 120 / 183 mm)",            single = 55,  onehalf = 120, double = 183, max_h = 230),
  custom  = list(label = "Custom width (mm)",                      single = NA,  onehalf = NA,  double = NA,  max_h = NA)
)
PANEL_GUTTER_MM <- 5   # gap assumed between two side-by-side panels

journal_width_mm <- function(family, columns, custom_mm = 89) {
  family <- family %||% "nature"
  jp     <- JOURNAL_PRESETS[[family]]
  if (is.null(jp) || identical(family, "custom")) return(custom_mm)
  switch(columns %||% "single",
         single      = jp$single,
         onehalf     = jp$onehalf,
         double      = jp$double,
         half_double = (jp$double - PANEL_GUTTER_MM) / 2,
         jp$single)
}

# ── v2.12.2: inside legends ──────────────────────────────────────────────────
# Theme for a legend anchored just inside a panel corner. Compact key (tight
# vertical spacing and margin) so it covers as little of the panel as possible.
inside_legend_theme <- function(corner = "top-left") {
  just <- switch(corner,
                 "top-left" = c(0, 1), "top-right" = c(1, 1),
                 "bottom-left" = c(0, 0), "bottom-right" = c(1, 0), c(1, 1))
  pos  <- ifelse(just == 1, 0.98, 0.02)
  theme(legend.position = "inside",
        legend.position.inside = pos, legend.justification.inside = just,
        legend.key.spacing.y = unit(1, "pt"), legend.margin = margin(1, 3, 1, 3))
}

# The panel corner with the fewest plotted points: the panel is split into
# quadrants at the midpoint of the x range and of the y range (log10 scale when
# the y axis is log), points are counted per quadrant, and the emptiest corner
# wins; ties go top-left, then top-right, then bottom-left, then bottom-right.
#
# box = NULL (default) is exactly the quadrant rule above. box = c(w, h) counts
# instead the points inside a corner box covering the fraction w of the x range
# and h of the y range at each corner, i.e. roughly the area a legend occupies;
# the figures use it because a whole quadrant can look empty while the thin band
# of data along its edge is exactly where a legend box lands.
auto_legend_corner <- function(x, y, log_y = FALSE, box = NULL) {
  n <- legend_corner_counts(x, y, log_y, box)
  names(n)[which.min(n)]
}
# Points along the polylines, k per segment, so a steep line that crosses a
# corner box between two sparse data points still counts as being there.
# x, y and g (series id) are parallel vectors; y is interpolated as given.
densify_xy <- function(x, y, g, k = 8) {
  parts <- lapply(split(seq_along(x), g, drop = TRUE), function(ix) {
    ix <- ix[is.finite(x[ix]) & is.finite(y[ix])]
    ix <- ix[order(x[ix])]
    if (length(ix) < 2) return(data.frame(x = x[ix], y = y[ix]))
    t <- seq(0, 1, length.out = k + 1)[-(k + 1)]
    data.frame(x = c(unlist(lapply(seq_len(length(ix) - 1), function(i) x[ix[i]] + t * (x[ix[i + 1]] - x[ix[i]]))), x[ix[length(ix)]]),
               y = c(unlist(lapply(seq_len(length(ix) - 1), function(i) y[ix[i]] + t * (y[ix[i + 1]] - y[ix[i]]))), y[ix[length(ix)]]))
  })
  do.call(rbind, c(parts, list(make.row.names = FALSE)))
}
# Points per corner (named, in tie-break order); all zero for no finite points.
legend_corner_counts <- function(x, y, log_y = FALSE, box = NULL) {
  corners <- c("top-left", "top-right", "bottom-left", "bottom-right")
  ok <- is.finite(x) & is.finite(y)
  if (isTRUE(log_y)) ok <- ok & y > 0
  x <- x[ok]; y <- y[ok]
  if (!length(x)) return(setNames(rep(0L, 4), corners))
  if (isTRUE(log_y)) y <- log10(y)
  if (is.null(box)) {
    right <- x > mean(range(x)); top <- y > mean(range(y))
    n <- c(sum(!right & top), sum(right & top), sum(!right & !top), sum(right & !top))
  } else {
    u <- if (diff(range(x)) > 0) (x - min(x)) / diff(range(x)) else rep(0.5, length(x))
    v <- if (diff(range(y)) > 0) (y - min(y)) / diff(range(y)) else rep(0.5, length(y))
    l <- u <= box[1]; r <- u >= 1 - box[1]; t <- v >= 1 - box[2]; b <- v <= box[2]
    n <- c(sum(l & t), sum(r & t), sum(l & b), sum(r & b))
  }
  setNames(as.integer(n), corners)
}
# More legend entries than this and an inside key is taller than the empty
# corner of any panel: "inside-auto" falls back to an outside (right) legend.
# It also falls back when even the emptiest corner box holds this many points
# or more: a key that overlaps data is worse than an outside key.
INSIDE_LEGEND_MAX_POINTS <- 2
INSIDE_LEGEND_MAX_ENTRIES <- 4
# Corner box a legend occupies, as fractions of the panel (x, y): about 9 % of
# the height per entry, and 22 % + 1.7 % per label character of the width
# (measured on 89 x 60 mm journal panels at 6 pt).
inside_legend_box <- function(n_entries, label_chars = 12)
  c(min(0.7, 0.22 + 0.017 * label_chars), min(0.9, 0.09 * n_entries + 0.02))

# Everything a journal preset pushes into the UI. Each is an ordinary input
# afterwards, so Save Settings captures it and the user can still adjust it.
# Text 6-8 pt (Nature: 5-7 pt max, Cell: 6-8 pt), data lines 0.75 pt, axes and
# error bars 0.5 pt (Nature: 0.25-1 pt), Okabe-Ito palette (Nature's own
# recommendation), legend inside the panel, SVG for Inkscape, 600 dpi rasters.
journal_preset_settings <- function(width_mm, height_mm, panel_label = "", x_range = NULL) {
  s <- list(
    export_units = "mm", export_width = width_mm, export_height = height_mm,
    export_format = "svg", export_dpi = 600,
    preview_mode = "wysiwyg", preview_zoom = 2,
    font_family = "Arial",
    title_font_size = 8, axis_label_font_size = 7, axis_text_font_size = 6,
    legend_font_size = 6, label_font_size = 6,
    bold_title = FALSE, italic_axis_labels = FALSE,
    line_thickness = 0.35,        # 0.75 pt data lines
    shape_size = 1.2, point_stroke = 0.25,
    error_bar_thickness = 0.25,   # ~0.5 pt error bars
    axis_line_pt = 0.5, tick_length_mm = 1.2,
    theme_base_pt = 7, y_tick_label_style = "trim",
    legend_position = "inside-auto", legend_box_border = FALSE, legend_key_mm = 4,
    legend_order = "selected",   # control first, as listed in the file; same order in every panel
    panel_label = panel_label, panel_label_size = 8,
    color_palette = "colorblind",
    show_major_gridlines = FALSE, show_minor_gridlines = FALSE,
    show_error_caption = FALSE,   # the Figure Legend Text box carries measure + n instead
    custom_aspect_ratio = FALSE,
    # v2.15: the panel itself has the fixed size; the figure grows around it
    panel_size_mode = "fixed", panel_width_mm = 65, panel_height_mm = 65, panel_square = TRUE,
    axis_subscripts = TRUE,       # v2.16: A550 -> A[550] in axis labels (the Legacy look keeps plain text)
    y_oob = "squish",             # v2.17: a reading below the log-axis floor is drawn on the floor, not deleted
    enable_threshold = FALSE      # v2.17: no threshold line (the Legacy look and the preset both clear it)
  )
  # v2.12: caps as a fraction of the x range, so the look is the same whether
  # time is in minutes or hours (x_range kept for callers; no longer needed)
  s$error_cap_relative <- TRUE
  s$error_bar_width    <- 1.5
  s$error_bar_color    <- "sample"
  s
}

# ── v2.10: export devices ────────────────────────────────────────────────────
# One resolver so every export path agrees on which device writes a format.
#  pdf  : journals want fonts EMBEDDED. grDevices::pdf() never embeds (it only
#         names the base-14 Helvetica) and errors on families such as "Arial"
#         that are not in its PostScript font database. macOS: quartz(type =
#         "pdf") embeds any installed font; elsewhere cairo_pdf; last resort pdf.
#  svg  : svglite with fix_text_size = FALSE. The default adds textLength= to
#         every <text>, which makes Inkscape stretch/squash glyphs the moment a
#         label is edited. Without it, text reflows naturally.
#  png/tiff/jpeg : ragg when installed (crisper text, consistent font lookup).
quartz_pdf_device <- function(filename, width, height, bg = "white", pointsize = 12, ...) {
  grDevices::quartz(type = "pdf", file = filename, width = width, height = height,
                    bg = bg, pointsize = pointsize)
}
svglite_inkscape_device <- function(filename, width, height, bg = "white", ...) {
  svglite::svglite(filename, width = width, height = height, bg = bg, fix_text_size = FALSE)
}
export_device <- function(format) {
  switch(format,
         pdf  = if (isTRUE(capabilities("aqua"))) quartz_pdf_device
                else if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else "pdf",
         svg  = svglite_inkscape_device,
         png  = if (has_ragg) ragg::agg_png  else "png",
         tiff = if (has_ragg) ragg::agg_tiff else "tiff",
         jpeg = if (has_ragg) ragg::agg_jpeg else "jpeg",
         format)
}

# ── v2.14.1: guards against oversized allocations ────────────────────────────
# Largest dpi <= `dpi` for which width_in x height_in x dpi^2 stays within max_pixels (floored to
# an integer, never below 36). attr(, "capped") is TRUE when the dpi was reduced. A size in the
# wrong unit (89 in instead of 89 mm) at 600 dpi would otherwise ask for ~2 gigapixels.
safe_raster_dpi <- function(width_in, height_in, dpi, max_pixels = 40e6) {
  out <- dpi; capped <- FALSE
  area <- width_in * height_in
  if (is.finite(area) && area > 0 && is.finite(dpi) && area * dpi^2 > max_pixels) {
    out <- max(36, floor(sqrt(max_pixels / area))); capped <- TRUE
  }
  attr(out, "capped") <- capped
  out
}
RASTER_MAX_PIXELS <- 40e6
SIZE_MAX_IN <- 60                       # each export dimension, inches (1524 mm)
TICK_MAX_BREAKS <- 500                  # more breaks than this -> ggplot's default breaks
# A readable message for a failed render; memory errors get a hint.
friendly_error_message <- function(e) {
  msg <- conditionMessage(e)
  if (grepl("vector memory limit|cannot allocate (vector|memory)|memory limit", msg, ignore.case = TRUE))
    msg <- paste0(msg, " Reduce DPI, export size, zoom or the number of points.")
  msg
}
# The placeholder figure every failed plot render shows.
plot_error_figure <- function(msg)
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = paste("Plot error:", msg),
             color = "red", size = 4, hjust = 0.5) +
    theme_void()

# ── v2.15: fixed panel size ──────────────────────────────────────────────────
# The size, in mm, of a gtable whose panel cells are absolute. Everything left (axis text, titles,
# legends) is measured on a scratch SVG device so font metrics resolve; the panels are absolute
# already, so the result does not depend on the device size.
figure_size_mm <- function(g) {
  measure <- function(open_dev) {
    old <- grDevices::dev.cur()
    open_dev()
    on.exit({ grDevices::dev.off(); if (old > 1) tryCatch(grDevices::dev.set(old), error = function(e) NULL) }, add = TRUE)
    c(grid::convertWidth(sum(g$widths), "mm", valueOnly = TRUE),
      grid::convertHeight(sum(g$heights), "mm", valueOnly = TRUE))
  }
  f <- tempfile(fileext = ".svg"); fp <- tempfile(fileext = ".png")
  on.exit(unlink(c(f, fp)), add = TRUE)
  sizes <- list(
    if (requireNamespace("svglite", quietly = TRUE))
      tryCatch(measure(function() svglite::svglite(f, width = 400 / 25.4, height = 400 / 25.4)), error = function(e) NULL),
    if (requireNamespace("ragg", quietly = TRUE))
      tryCatch(measure(function() ragg::agg_png(fp, width = 400, height = 400, units = "mm", res = 72)), error = function(e) NULL))
  sizes <- Filter(Negate(is.null), sizes)
  if (!length(sizes)) sizes <- list(measure(function() grDevices::pdf(NULL, width = 400 / 25.4, height = 400 / 25.4)))
  # text metrics differ by a fraction of a mm between devices; the larger of the two fits both
  c(max(vapply(sizes, `[`, 0, 1)), max(vapply(sizes, `[`, 0, 2)))
}
# A ggplot as a gtable whose panel area is exactly panel_w_mm x panel_h_mm (split evenly over the
# panel columns / rows of a facet figure), with attr(, "size_mm") = the whole figure's size. Only
# the panels' null units are replaced; every other null unit is zeroed so the size is absolute.
fixed_panel_grob <- function(p, panel_w_mm, panel_h_mm) {
  g   <- ggplot2::ggplotGrob(p)
  lay <- g$layout[grepl("^panel", g$layout$name), , drop = FALSE]
  cols <- sort(unique(unlist(Map(seq, lay$l, lay$r))))
  rows <- sort(unique(unlist(Map(seq, lay$t, lay$b))))
  cols <- cols[grid::unitType(g$widths)[cols]  == "null"]
  rows <- rows[grid::unitType(g$heights)[rows] == "null"]
  for (i in cols) g$widths[i]  <- grid::unit(panel_w_mm / length(cols), "mm")
  for (i in rows) g$heights[i] <- grid::unit(panel_h_mm / length(rows), "mm")
  for (i in which(grid::unitType(g$widths)  == "null")) g$widths[i]  <- grid::unit(0, "mm")
  for (i in which(grid::unitType(g$heights) == "null")) g$heights[i] <- grid::unit(0, "mm")
  g$respect <- FALSE
  attr(g, "size_mm") <- figure_size_mm(g)
  g
}
# Panels laid out as an ncol-column grid of fixed-panel gtables (each placed top-left in a cell as
# wide / tall as the largest one), with a, b, c tags at each cell's top-left corner.
compose_fixed_figure <- function(items, ncol = 2, tag_size = 8, font = "Arial", tag_case = "a", tags_on = TRUE) {
  n <- length(items); ncol <- max(1, min(ncol, n)); nrow <- ceiling(n / ncol)
  sz <- vapply(items, function(g) attr(g, "size_mm"), numeric(2))
  cw <- max(sz[1, ]); ch <- max(sz[2, ])
  master <- gtable::gtable(widths = grid::unit(rep(cw, ncol), "mm"), heights = grid::unit(rep(ch, nrow), "mm"))
  tags <- if (identical(tag_case, "A")) LETTERS else letters
  for (i in seq_len(n)) {
    r <- (i - 1) %/% ncol + 1; cc <- (i - 1) %% ncol + 1
    cell <- grid::gTree(children = grid::gList(items[[i]]),
                        vp = grid::viewport(x = 0, y = 1, width = grid::unit(sz[1, i], "mm"),
                                            height = grid::unit(sz[2, i], "mm"), just = c("left", "top")))
    master <- gtable::gtable_add_grob(master, cell, t = r, l = cc, name = paste0("cell-", i), clip = "off")
    if (!isTRUE(tags_on)) next   # v2.16: "Add panel letters" off
    tag <- grid::textGrob(tags[i], x = grid::unit(0.5, "mm"), y = grid::unit(1, "npc") - grid::unit(0.5, "mm"),
                          hjust = 0, vjust = 1,
                          gp = grid::gpar(fontsize = tag_size, fontface = "bold", fontfamily = font))
    master <- gtable::gtable_add_grob(master, tag, t = r, l = cc, name = paste0("tag-", i), clip = "off", z = Inf)
  }
  attr(master, "size_mm") <- c(cw * ncol, ch * nrow)
  master
}
# The fit indicator of the size bar: which column layouts a figure of width W (mm) fits in the
# chosen journal family ("single column", "two across double column", "three across ...").
figure_fit_text <- function(w_mm, jp) {
  single <- jp$single %||% NA; double <- jp$double %||% NA
  if (!is.finite(single) && !is.finite(double)) return(NULL)
  ok <- c(if (is.finite(single) && w_mm <= single + 0.05) "single column",
          if (is.finite(double) && 2 * w_mm + 5 <= double + 0.05) "two across double column",
          if (is.finite(double) && 3 * w_mm + 10 <= double + 0.05) "three across double column")
  list(fits = length(ok) > 0,
       text = if (length(ok)) paste0("fits: ", paste(ok, collapse = " \u00b7 "))
              else sprintf("exceeds %s mm column", if (is.finite(single)) single else double))
}

# ── v2.11: analysis figures, replicate statistics, MOI ───────────────────────
ANALYSIS_METRIC_LABELS <- c(
  lysis_time         = "Lysis onset",
  od_drop            = "OD drop",
  lysis_efficiency   = "Lysis efficiency",
  lysis_rate         = "Lysis rate",
  t_half_max_decline = "Time to half peak",
  residual_od        = "Residual OD after lysis",
  recovery_slope     = "Recovery slope",
  mu_max             = "μmax (ln OD per time)",
  doubling_time      = "Doubling time",
  lag_phase          = "Lag phase",
  max_od             = "Peak OD",
  time_max_od        = "Time of peak",
  auc                = "AUC",
  final_od           = "Final OD",
  relative_growth    = "Relative growth",
  infection_strength = "Infection strength",
  relative_mu_max    = "Relative μmax",
  relative_max_od    = "Relative peak OD",
  first_peak_od      = "First-peak OD",
  t_first_peak       = "Time of first peak",
  extinction_time    = "Extinction time",
  centroid_x         = "Centroid time",
  centroid_y         = "Centroid OD",
  centroid_index     = "Centroid index",
  local_virulence    = "Local virulence"
)
# The analysis figures, with the labels the Analysis tab and the figure composer both use.
ANALYSIS_FIGURE_CHOICES <- c("Metric summary (replicates, mean \u00b1 SD)" = "metric_summary",
                             "Specific growth rate vs time"              = "growth_rate",
                             "Normalized OD vs time"                     = "normalized",
                             "Replicate overlay"                         = "replicate_overlay",
                             "Curve landmarks (metrics drawn on the curves)" = "landmarks",
                             "MOI dose-response"                         = "dose_response",
                             "OD heatmap (samples \u00d7 time)"           = "heatmap")
COMPOSE_PANEL_CHOICES <- c("Lysis curve (main)" = "main", ANALYSIS_FIGURE_CHOICES)
metric_label <- function(m) { l <- unname(ANALYSIS_METRIC_LABELS[m]); ifelse(is.na(l), m, l) }
# Axis / facet label with units, from the x-axis time unit and the y-axis label.
metric_axis_label <- function(m, tunit = "min", ylab = "OD") {
  base <- metric_label(m)
  if (m %in% c("lysis_time", "t_half_max_decline", "doubling_time", "lag_phase", "time_max_od",
               "t_first_peak", "extinction_time", "centroid_x"))
    paste0(base, " (", tunit, ")")
  else if (m == "mu_max") paste0("\u03bcmax (", tunit, "\u207b\u00b9)")
  else if (m %in% c("lysis_rate", "recovery_slope"))
    paste0(base, " (", ylab, " per ", tunit, ")")
  else if (m %in% c("od_drop", "residual_od", "max_od", "final_od", "first_peak_od", "centroid_y"))
    paste0(base, " (", ylab, ")")
  else if (m == "auc") paste0(base, " (", ylab, " \u00d7 ", tunit, ")")
  else base
}

# v2.16: typeset the wavelength in an absorbance label as a subscript (A550 -> A[550], OD600 ->
# OD[600]). A label with such a token ("A550", "OD600", "A550 relative to Uninfected", "log10 A550",
# "OD drop (A550)") becomes a plotmath call: the token is letters[digits] and the rest stays quoted
# text, joined with "~" (a space) where the text had whitespace and "*" (no space) where it did
# not. Labels without a token come back unchanged (character), so output without one is identical.
label_with_subscripts <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(x)
  m <- gregexpr("(?<![A-Za-z0-9])(OD|A)([0-9]+)(?![A-Za-z0-9])", x, perl = TRUE)[[1]]
  if (m[1] == -1L) return(x)
  # raw segments in order: text between tokens (whitespace kept) and the tokens themselves
  raw <- character(0); is_tok <- logical(0); pos <- 1L
  for (i in seq_along(m)) {
    if (as.integer(m[i]) > pos) { raw <- c(raw, substr(x, pos, m[i] - 1L)); is_tok <- c(is_tok, FALSE) }
    raw <- c(raw, substr(x, m[i], m[i] + attr(m, "match.length")[i] - 1L)); is_tok <- c(is_tok, TRUE)
    pos <- as.integer(m[i]) + attr(m, "match.length")[i]
  }
  if (pos <= nchar(x)) { raw <- c(raw, substr(x, pos, nchar(x))); is_tok <- c(is_tok, FALSE) }
  keep <- is_tok | nzchar(trimws(raw))                        # whitespace-only text between two tokens only spaces them
  part <- function(i) {
    if (!is_tok[i]) return(trimws(raw[i]))
    base <- sub("[0-9]+$", "", raw[i]); dig <- sub("^[A-Za-z]+", "", raw[i])
    call("[", as.name(base), if (identical(as.character(as.numeric(dig)), dig)) as.numeric(dig) else dig)
  }
  idx <- which(keep)
  out <- part(idx[1])
  for (k in seq_along(idx)[-1]) {
    i0 <- idx[k - 1L]; i1 <- idx[k]
    between <- paste0(substr(raw[i0], nchar(raw[i0]), nchar(raw[i0])),            # last char before, first after,
                      if (i1 - i0 > 1L) raw[(i0 + 1L):(i1 - 1L)] else "",         # and any dropped whitespace text
                      substr(raw[i1], 1L, 1L))
    out <- call(if (grepl("\\s", between)) "~" else "*", out, part(i1))
  }
  out
}

sig_stars <- function(p)
  ifelse(is.na(p), "", ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "ns"))))

time_unit_from_label <- function(xlab) {
  x <- tolower(xlab %||% "")
  if (grepl("min", x)) "min"
  else if (grepl("hour|\\(h\\)|\\bhr", x)) "h"
  else if (grepl("sec", x)) "s"
  else "time unit"
}

# MOI from sample names: "Phage MOI 10", "MOI=0.1", "moi_1e-2", "MOI: 5" ...
parse_moi <- function(x) {
  m <- regmatches(x, regexec("(?i)MOI[[:space:]_:=]*([0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?)", x, perl = TRUE))
  vapply(m, function(v) if (length(v) >= 2) suppressWarnings(as.numeric(v[2])) else NA_real_, numeric(1))
}
# override text: one "sample = MOI" per line or separated by ";"
moi_map <- function(samples, override = "") {
  moi <- setNames(parse_moi(samples), samples)
  ov  <- trimws(override %||% "")
  if (nzchar(ov)) {
    parts <- trimws(unlist(strsplit(ov, "[;\n]+")))
    for (p in parts[nzchar(parts)]) {
      kv <- trimws(unlist(strsplit(p, "=", fixed = TRUE)))
      if (length(kv) == 2 && kv[1] %in% samples) moi[kv[1]] <- suppressWarnings(as.numeric(kv[2]))
    }
  }
  moi
}

# Specific growth rate mu(t) = d ln(OD)/dt per replicate, by the SAME centered
# rolling regression (window = the mu_max smoothing window) and the same
# non-positive-OD exclusion as the mu_max metric, then mean +/- SD across
# replicates at each time point. Lysis shows as a negative rate.
compute_growth_rate_curves <- function(rep_pd, window = 5) {
  if (is.null(rep_pd) || nrow(rep_pd) == 0) return(NULL)
  rep_pd <- rep_pd[is.finite(rep_pd$value), , drop = FALSE]
  if (nrow(rep_pd) == 0) return(NULL)
  rep_pd <- rep_pd[order(rep_pd$variable, rep_pd$replicate, rep_pd$time), , drop = FALSE]
  key <- interaction(rep_pd$variable, rep_pd$replicate, drop = TRUE)
  pieces <- lapply(split(rep_pd, key, drop = TRUE), function(g) {
    n <- nrow(g); win <- min(window, n)
    mu <- rep(NA_real_, n)
    if (n >= win && win >= 3) {
      pos <- g$value > 0
      lo  <- rep(NA_real_, n); lo[pos] <- log(g$value[pos])
      mu  <- tryCatch(rolling_slope(g$time, lo, win), error = function(e) rep(NA_real_, n))
      if (any(!pos)) {
        off <- floor((win - 1) / 2)
        for (s in seq_len(n - win + 1)) if (any(!pos[s:(s + win - 1)])) mu[s + off] <- NA_real_
      }
    }
    data.frame(time = g$time, variable = g$variable[1], replicate = g$replicate[1], mu = mu,
               stringsAsFactors = FALSE)
  })
  d <- do.call(rbind, c(pieces, list(make.row.names = FALSE)))
  d <- d[is.finite(d$mu), , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  out <- lapply(split(d, list(d$variable, d$time), drop = TRUE), function(g)
    data.frame(time = g$time[1], variable = g$variable[1], n = nrow(g), mu_mean = mean(g$mu),
               mu_sd = if (nrow(g) >= 2) sd(g$mu) else NA_real_, stringsAsFactors = FALSE))
  out <- do.call(rbind, c(out, list(make.row.names = FALSE)))
  out[order(out$variable, out$time), , drop = FALSE]
}

# OD relative to the uninfected reference at each time point (mean curves),
# or to each sample's own first value ("t0"). The reference-normalised curve
# is the infection-strength metric (1 - AUC ratio) drawn out over time.
compute_normalized_curves <- function(pd, ref_sample = NULL, mode = c("reference", "t0")) {
  mode <- match.arg(mode)
  if (is.null(pd) || nrow(pd) == 0) return(NULL)
  pd <- pd[order(pd$variable, pd$time), , drop = FALSE]
  if (mode == "reference") {
    if (is.null(ref_sample) || !nzchar(ref_sample) || !ref_sample %in% pd$variable) return(NULL)
    ref <- pd[pd$variable == ref_sample, c("time", "mean_value")]
    names(ref)[2] <- "ref_value"
    m <- merge(pd, ref, by = "time")
    m$ratio <- ifelse(is.finite(m$ref_value) & m$ref_value > 0, m$mean_value / m$ref_value, NA_real_)
  } else {
    m <- pd
    m$ratio <- ave(m$mean_value, m$variable,
                   FUN = function(v) if (is.finite(v[1]) && v[1] > 0) v / v[1] else rep(NA_real_, length(v)))
  }
  m <- m[is.finite(m$ratio), c("time", "variable", "ratio"), drop = FALSE]
  m[order(m$variable, m$time), , drop = FALSE]
}

# ── Replicate statistics ─────────────────────────────────────────────────────
# Compares one per-replicate metric between samples. With a reference sample
# each sample is tested against it; otherwise all pairs. Welch's t-test
# (unequal variances) or Mann-Whitney; Holm-adjusted p across the comparisons;
# Hedges' g (bias-corrected standardized mean difference); the 95% CI of the
# difference from the t-test. Omnibus for >= 3 groups: one-way ANOVA + Tukey
# HSD, or Kruskal-Wallis. Groups with fewer than 2 finite values are excluded.
# Any n < 3 is flagged: three biological replicates give little power, so a
# non-significant result is not evidence of no difference.
compare_replicate_metric <- function(rm, metric, ref_sample = NULL,
                                     method = c("welch", "nonparametric")) {
  method <- match.arg(method)
  empty  <- list(pairwise = NULL, omnibus = NULL, posthoc = NULL, warnings = character(0), metric = metric)
  if (is.null(rm) || !metric %in% names(rm)) return(empty)
  d <- rm[is.finite(rm[[metric]]), c("sample", metric), drop = FALSE]
  names(d)[2] <- "value"
  ns      <- table(d$sample)
  dropped <- names(ns)[ns < 2]
  keep    <- names(ns)[ns >= 2]
  warns   <- character(0)
  if (length(dropped))
    warns <- c(warns, paste0("Excluded (fewer than 2 finite values): ", paste(dropped, collapse = ", "), "."))
  if (length(keep) && any(ns[keep] < 3))
    warns <- c(warns, "Some groups have n < 3: low power -- a non-significant result is not evidence of no difference.")
  if (length(keep) < 2) {
    empty$warnings <- c(warns, "Need at least two samples with 2+ replicate values for this metric.")
    return(empty)
  }
  d <- d[d$sample %in% keep, , drop = FALSE]
  has_ref <- !is.null(ref_sample) && nzchar(ref_sample)
  if (has_ref && !ref_sample %in% keep)
    warns <- c(warns, paste0("Reference '", ref_sample, "' has no usable values for this metric ",
                             "(e.g. an uninfected control never lyses), so all pairs are compared instead."))
  pairs <- if (has_ref && ref_sample %in% keep)
    lapply(setdiff(keep, ref_sample), function(s) c(s, ref_sample))
  else combn(keep, 2, simplify = FALSE)
  hedges_g <- function(a, b) {
    na <- length(a); nb <- length(b)
    sp <- sqrt(((na - 1) * var(a) + (nb - 1) * var(b)) / (na + nb - 2))
    if (!is.finite(sp) || sp == 0) return(NA_real_)
    (mean(a) - mean(b)) / sp * (1 - 3 / (4 * (na + nb) - 9))
  }
  rows <- lapply(pairs, function(pr) {
    a  <- d$value[d$sample == pr[1]]; b <- d$value[d$sample == pr[2]]
    tt <- tryCatch(if (method == "welch") t.test(a, b) else wilcox.test(a, b, exact = FALSE),
                   error = function(e) NULL)
    ci <- if (!is.null(tt) && !is.null(tt$conf.int)) tt$conf.int else c(NA_real_, NA_real_)
    data.frame(sample = pr[1], versus = pr[2], n = length(a), n_versus = length(b),
               mean = mean(a), mean_versus = mean(b), difference = mean(a) - mean(b),
               ci_low = ci[1], ci_high = ci[2], hedges_g = hedges_g(a, b),
               test = if (method == "welch") "Welch t" else "Mann-Whitney",
               p = if (is.null(tt)) NA_real_ else unname(tt$p.value),
               stringsAsFactors = FALSE)
  })
  pw <- do.call(rbind, c(rows, list(make.row.names = FALSE)))
  if (any(is.na(pw$p)))   # t.test() refuses constant data (e.g. identical lysis onsets at a coarse sampling interval)
    warns <- c(warns, paste0("No test possible (values constant within both groups): ",
                             paste(paste(pw$sample[is.na(pw$p)], "vs", pw$versus[is.na(pw$p)]), collapse = "; "), "."))
  pw$p_adj_holm <- p.adjust(pw$p, method = "holm")
  pw$stars      <- sig_stars(pw$p_adj_holm)
  om <- NULL; ph <- NULL
  if (length(keep) >= 3) {
    d$sample <- factor(d$sample)
    if (method == "welch") {
      fit <- tryCatch(aov(value ~ sample, data = d), error = function(e) NULL)
      if (!is.null(fit)) {
        s  <- summary(fit)[[1]]
        om <- data.frame(test = "One-way ANOVA", statistic = s[["F value"]][1],
                         df1 = s[["Df"]][1], df2 = s[["Df"]][2], p = s[["Pr(>F)"]][1],
                         stringsAsFactors = FALSE)
        tk <- tryCatch(TukeyHSD(fit)$sample, error = function(e) NULL)
        if (!is.null(tk)) {
          ph <- data.frame(comparison = rownames(tk), difference = tk[, "diff"],
                           ci_low = tk[, "lwr"], ci_high = tk[, "upr"], p_adj_tukey = tk[, "p adj"],
                           row.names = NULL, stringsAsFactors = FALSE)
          ph$stars <- sig_stars(ph$p_adj_tukey)
        }
      }
    } else {
      kw <- tryCatch(kruskal.test(value ~ sample, data = d), error = function(e) NULL)
      if (!is.null(kw))
        om <- data.frame(test = "Kruskal-Wallis", statistic = unname(kw$statistic),
                         df1 = unname(kw$parameter), df2 = NA_real_, p = kw$p.value,
                         stringsAsFactors = FALSE)
    }
  }
  list(pairwise = pw, omnibus = om, posthoc = ph, warnings = warns, metric = metric)
}

# ── App defaults (v2.10: single source of truth) ─────────────────────────────
# Used by Clear/Reset Settings and by the test suites to seed a server session
# with the same values the UI widgets start with.
# v2.14.0: the app now STARTS in the journal standard (Nature single column, 89 x 65 mm,
# Arial 6-8 pt, SVG). LEGACY_BASE is the complete pre-v2.14 default set (the v2.9 look:
# 10 x 8 in, 20 pt, PDF, canvas preview); legacy_preset_settings() is the part of it the
# journal preset touches, offered as the "Legacy look" button. Every non-visual default
# (metrics, notebook, analysis, compose, ...) is unchanged.
LEGACY_BASE <- list(
  x_scale_type          = "linear",  y_scale_type           = "log",
  use_advanced_ticks    = TRUE,       y_log_min_exponent     = -2,
  y_log_max_exponent    = 0,          custom_x_limits        = FALSE,
  custom_y_limits       = FALSE,      x_expand_left          = 0,
  x_expand_right        = 0.05,       y_expand_bottom        = 0,
  y_expand_top          = 0.05,       x_axis_label           = "Time (minutes)",
  demo_scenario         = "lysis",
  y_axis_label          = "A550",     plot_title             = "",
  plot_subtitle         = "",         show_major_gridlines   = FALSE,
  show_minor_gridlines  = FALSE,      font_family            = "sans",
  title_font_size       = 20,         axis_label_font_size   = 20,
  axis_text_font_size   = 16,         bold_title             = TRUE,
  italic_axis_labels    = FALSE,      axis_text_angle        = "0",
  enable_highlighting   = FALSE,      enable_time_markers    = FALSE,
  color_palette         = "custom",   line_thickness         = 1,
  show_points           = TRUE,       shape_size             = 3,
  point_stroke          = 0.5,        show_end_labels        = FALSE,
  label_font_size       = 12,         label_bold             = TRUE,
  label_offset          = 3.5,        error_type             = "sem",
  error_display_mode    = "bars",     error_multiplier       = 1,
  asymmetric_error      = FALSE,      error_bar_style        = "T",
  error_bar_width       = 1,          error_bar_thickness    = 0.8,
  error_bar_position    = "middle",   shadow_alpha           = 0.2,
  show_error_caption    = TRUE,       # KC-6: default figure states the error measure + n
  custom_aspect_ratio   = FALSE,      aspect_ratio           = 1,
  export_format         = "pdf",      export_width           = 10,
  export_height         = 8,          export_dpi             = 300,
  plot_width            = 820,
  gif_fps               = 1,          gif_width              = 800,
  gif_height            = 600,        x_extra_ticks          = "",
  report_title          = "Lysis Curve Analysis Report",
  report_author         = "",         report_notes           = "",
  metrics_smooth_window = 5,          legend_font_size       = 14,
  metrics_peak_smooth_window = 1,     # KC-3: off by default
  preview_zoom_auto = TRUE,           # v2.14: zoom chosen to fit the main panel width
  # v2.13.0 figure composer
  compose_panels = c("main", "metric_summary", "growth_rate", "normalized"),
  compose_ncol = "2", compose_width = "double", compose_width_mm = 183,
  compose_panel_height_mm = 60, compose_tag_size = 8, compose_tag_case = "a", compose_tags = TRUE,   # v2.16: panel letters in the composer (explicit request, on)
  metrics_lysis_threshold_pct = 5,    # v2.12.4: decline from peak that counts as lysis (%)
  metrics_peak_mode = "global",       # v2.12.2: "first_local" for regrowth / resistance curves
  blank_od               = 0,         # KC-7: off by default
  report_font_size      = 14,
  report_include_analysis = TRUE,   # v2.11.1: analysis figures + replicate statistics in reports
  # v2.10 journal-figure inputs
  export_units = "in", preview_mode = "canvas", preview_zoom = 2,
  axis_line_pt = 1.07, tick_length_mm = 1.5, legend_position = "right",
  legend_box_border = TRUE, legend_key_mm = 0, panel_label = "", panel_label_size = 8,
  panel_size_mode = "fit", panel_width_mm = 65, panel_height_mm = 65, panel_square = TRUE,   # v2.15
  theme_base_pt = 0, y_tick_label_style = "legacy",
  axis_subscripts = FALSE,            # v2.16: journal preset TRUE; plain "A550" in the Legacy look
  methods_text = "", methods_replace = FALSE,   # v2.16 Statistics tab: editable methods paragraph
  y_oob = "censor",                   # v2.17: journal preset "squish" (clip to the axis floor); Legacy look drops them
  enable_threshold = FALSE, threshold_value = 0.1, threshold_label = "",   # v2.17: horizontal OD threshold line
  threshold_color = "grey40", threshold_linetype = "dashed",
  qc_od_ceiling = 2.5,                # v2.17: QC flag CEILING above this OD
  # v2.11 analysis / dashboard inputs
  analysis_type = "metric_summary", analysis_metrics = c("lysis_time", "od_drop"),
  analysis_dose_metric = "lysis_time", analysis_norm_mode = "reference",
  analysis_panel_label = "", stats_method = "welch", stats_mark = TRUE, moi_override = "",
  # v2.12 error display
  error_bar_color = "sample", show_replicate_points = FALSE, error_log_floor = TRUE,
  error_cap_relative = FALSE,
  legend_order = "alpha",
  journal_family = "nature", journal_columns = "single", journal_custom_width_mm = 89,
  journal_height_mm = 65, journal_panel_label = "",
  nb_exp_id = "", nb_experimenter = "", nb_project = "", nb_institution = "",
  nb_exp_type = "Phage Infection", nb_host_strain = "", nb_phage_plasmid = "",
  nb_moi = "", nb_replicate = "", nb_media = "", nb_temperature = "",
  nb_time_inf = "", nb_inducer = "", nb_observations = "", nb_issues = "",
  nb_next_steps = "", nb_tags = "",
  nb_passage = "", nb_inducer_conc = "", nb_other_condition = "",   # v2.17
  nb_custom_key1 = "", nb_custom_val1 = "", nb_custom_key2 = "", nb_custom_val2 = "",
  nb_custom_key3 = "", nb_custom_val3 = ""
)

# The inputs the journal preset sets, at their legacy values.
legacy_preset_settings <- function() LEGACY_BASE[names(journal_preset_settings(89, 65))]
APP_DEFAULTS <- modifyList(LEGACY_BASE, journal_preset_settings(89, 65))   # v2.16: no panel letter unless the user sets one
# ── Figure bundle archive (v2.17) ─────────────────────────────────────────────
# "zip" when the zip package or a zip binary is available, else "tar" (utils::tar, always there).
bundle_archive_kind <- function()
  if (requireNamespace("zip", quietly = TRUE) || nzchar(Sys.which("zip"))) "zip" else "tar"
# Archive the folder `folder` found in `root` into `archive` (the folder is the top level of the archive).
write_bundle_archive <- function(archive, root, folder, kind = bundle_archive_kind()) {
  archive <- normalizePath(archive, mustWork = FALSE)
  if (identical(kind, "zip") && requireNamespace("zip", quietly = TRUE)) {
    zip::zip(archive, files = folder, root = root)
  } else if (identical(kind, "zip")) {
    owd <- setwd(root); on.exit(setwd(owd), add = TRUE)
    utils::zip(archive, files = folder, flags = "-r9Xq")
  } else {
    owd <- setwd(root); on.exit(setwd(owd), add = TRUE)
    utils::tar(archive, files = folder, compression = "none")
  }
  invisible(archive)
}

# ── Palettes ──────────────────────────────────────────────────────────────────
# n colours for a named palette. v2.17: "blues" and "blue_red" are dose gradients for MOI series:
# the first sample (usually the uninfected control) is black and the n - 1 others run along the
# ramp from low to high dose (light to dark blue, or blue to red).
get_palette_colors <- function(pal, n) {
  gradient <- function(ends) {
    if (n <= 1) return(rep_len("#000000", n))
    c("#000000", grDevices::colorRampPalette(ends)(n - 1))
  }
  switch(pal,
         viridis     = scales::viridis_pal(option = "D")(n),
         plasma      = scales::viridis_pal(option = "C")(n),
         colorblind  = rep_len(c("#000000","#E69F00","#56B4E9","#009E73",
                                 "#F0E442","#0072B2","#D55E00","#CC79A7"), n),
         publication = rep_len(c("#E41A1C","#377EB8","#4DAF4A","#984EA3",
                                 "#FF7F00","#FFFF33","#A65628","#F781BF"), n),
         rainbow     = rainbow(n),
         gray        = gray.colors(n, start = 0.15, end = 0.85),
         blues       = gradient(c("#c6dbef", "#08306b")),
         blue_red    = gradient(c("#2166ac", "#b2182b")),
         NULL
  )
}

# ── v2.18 engine: growth fits, phenotype matrix, all-metrics statistics, extraction ──
# Pure functions (no Shiny state): the UI calls them. See docs/FITS_AND_EXTRACTION.md.

# ── Growth-model fits (Zwietering 1990 / Baranyi-Roberts 1994) ───────────────
# Every fit is made on the CHANGE from the first reading: y(t) = ln(od(t) / od(t0)) (log_transform =
# TRUE) or od(t) - od(t0), against tt = t - t0, so the fitted function is f(tt) - f(0) and
# od(t) = od(t0) * exp(f(tt) - f(0)). For Baranyi f(0) = 0 already. A is therefore the asymptote of f
# (in ln-OD units, or OD units without the log), mu_max the maximum slope of f (per time unit; a
# specific growth rate on the log scale) and lag = lambda a DURATION from the first time point.
gm_logistic <- function(tt, A, mu, lag)       # modified logistic, Zwietering 1990
  A / (1 + exp(4 * mu / A * (lag - tt) + 2)) - A / (1 + exp(4 * mu / A * lag + 2))
gm_gompertz <- function(tt, A, mu, lag)       # modified Gompertz, Zwietering 1990
  A * exp(-exp(mu * exp(1) / A * (lag - tt) + 1)) - A * exp(-exp(mu * exp(1) / A * lag + 1))
gm_baranyi  <- function(tt, A, mu, lag) {     # Baranyi-Roberts, y0 = 0, h0 = mu * lag, ymax = A
  h0 <- mu * lag
  At <- tt + log(exp(-mu * tt) + exp(-h0) - exp(-mu * tt - h0)) / mu      # adjustment function A(t)
  mu * At - log1p(exp(mu * At - A) - exp(-A))
}
GROWTH_MODEL_FUNS <- list(logistic = gm_logistic, gompertz = gm_gompertz, baranyi = gm_baranyi)

# One output row. t0 / log_transform / until are extra columns predict_growth_model() needs.
gm_fit_row <- function(model, converged = FALSE, n_points = NA_integer_, A = NA_real_, mu_max = NA_real_,
                       lag = NA_real_, y0 = NA_real_, rss = NA_real_, aic = NA_real_, bic = NA_real_,
                       r2 = NA_real_, se = c(NA_real_, NA_real_, NA_real_), message = NA_character_,
                       t0 = NA_real_, log_transform = TRUE, until = "peak") {
  data.frame(model = model, converged = converged, n_points = as.integer(n_points), A = A, mu_max = mu_max,
             lag = lag, y0 = y0, rss = rss, aic = aic, bic = bic, r2 = r2,
             se_A = se[1], se_mu_max = se[2], se_lag = se[3], message = as.character(message),
             t0 = t0, log_transform = log_transform, until = until, stringsAsFactors = FALSE)
}

# Fit one or more growth models to a single curve; never errors (a failed model returns
# converged = FALSE, NA parameters and the reason in `message`).
#   until = "peak": only the points up to and including the maximum of od are used, so a lysis
#                   curve is never fitted through its collapse; "all": every point.
#   Starting values: mu_max = largest centered rolling ln-slope (rolling_slope, window 5), lag = the
#   engine's rule (first time that slope reaches 10 % of mu_max) or the tangent intercept, A = max(y).
fit_growth_models <- function(t, od, models = c("logistic", "gompertz", "baranyi"),
                              until = c("peak", "all"), log_transform = TRUE) {
  until  <- match.arg(until)
  models <- match.arg(models, names(GROWTH_MODEL_FUNS), several.ok = TRUE)
  fail_all <- function(msg, n = 0L, t0 = NA_real_, y0 = NA_real_)
    do.call(rbind, lapply(models, function(m)
      gm_fit_row(m, n_points = n, message = msg, y0 = y0, t0 = t0, log_transform = log_transform, until = until)))
  ok <- is.finite(t) & is.finite(od)
  t <- as.numeric(t)[ok]; od <- as.numeric(od)[ok]
  if (length(t) == 0) return(fail_all("no finite data"))
  o <- order(t); t <- t[o]; od <- od[o]
  note <- NA_character_
  if (log_transform) {
    pos <- od > 0
    if (!any(pos)) return(fail_all("no positive OD readings (cannot take the logarithm)"))
    if (any(!pos)) {
      note <- sprintf("%d non-positive reading(s) dropped before the log transform", sum(!pos))
      t <- t[pos]; od <- od[pos]
    }
  }
  if (length(od) > 0 && diff(range(od)) == 0)
    return(fail_all("no growth (flat curve)", length(od), t[1], od[1]))
  if (until == "peak") { k <- which.max(od); t <- t[seq_len(k)]; od <- od[seq_len(k)] }
  n <- length(t); od0 <- od[1]; t0 <- t[1]
  if (n < 4)
    return(fail_all(sprintf("too few points (%d; need at least 4)%s", n,
                            if (until == "peak") " up to the peak" else ""), n, t0, od0))
  if (n < 6) note <- paste(stats::na.omit(c(note, sprintf("only %d points: parameters are poorly determined", n))), collapse = "; ")
  y  <- if (log_transform) log(od / od0) else od - od0
  tt <- t - t0
  if (!is.finite(max(y)) || max(y) <= 1e-8 * max(1, abs(od0)))
    return(fail_all("no growth up to the peak (flat or declining curve)", n, t0, od0))

  # starting values
  win <- min(5L, n)
  rs  <- if (win >= 3) tryCatch(rolling_slope(tt, y, win), error = function(e) rep(NA_real_, n)) else rep(NA_real_, n)
  if (!any(is.finite(rs) & rs > 0)) rs <- c(NA_real_, diff(y) / diff(tt))   # very short curve: consecutive slopes
  rs_ok <- which(is.finite(rs) & rs > 0)
  if (length(rs_ok) == 0) return(fail_all("no positive growth slope", n, t0, od0))
  kmax  <- rs_ok[which.max(rs[rs_ok])]
  mu0   <- rs[kmax]
  lag_rule <- tt[which(rs >= 0.10 * mu0 & is.finite(rs))[1]]
  lag_tan  <- min(max(tt[kmax] - y[kmax] / mu0, 0), max(tt))
  lag_starts <- unique(pmin(pmax(c(lag_rule, lag_tan), 0), max(tt)))
  ymax <- max(y)
  A_ub <- 5 * ymax
  starts <- unlist(lapply(c(1, 1.5), function(f) lapply(lag_starts, function(l)
    c(A = f * ymax, mu = mu0, lag = l))), recursive = FALSE)
  lower <- c(A = ymax * 1e-3, mu = mu0 * 1e-3, lag = 0)
  upper <- c(A = A_ub, mu = mu0 * 20, lag = max(tt))
  dat   <- data.frame(tt = tt, y = y)
  sst   <- sum((y - mean(y))^2)
  ctl   <- stats::nls.control(maxiter = 500, scaleOffset = 1e-6 * max(sum(y^2), 1e-12), warnOnly = FALSE)

  rows <- lapply(models, function(m) {
    form <- stats::as.formula(paste0("y ~ gm_", m, "(tt, A, mu, lag)"))
    best <- NULL; best_rss <- Inf; last_err <- "did not converge"
    for (st in starts) {
      for (alg in c("port", "default")) {
        st_c <- if (alg == "port") pmin(pmax(st, lower), upper) else st
        fit <- tryCatch(suppressWarnings(
          if (alg == "port") stats::nls(form, data = dat, start = as.list(st_c), algorithm = "port",
                                        lower = lower, upper = upper, control = ctl)
          else stats::nls(form, data = dat, start = as.list(st_c), control = ctl)),
          error = function(e) { last_err <<- conditionMessage(e); NULL })
        if (is.null(fit)) next
        cf <- stats::coef(fit)
        if (!all(is.finite(cf)) || cf[["A"]] <= 0 || cf[["mu"]] <= 0 || cf[["lag"]] < -1e-8 * max(1, max(tt))) next
        r <- sum(stats::resid(fit)^2)
        if (is.finite(r) && r < best_rss) { best <- fit; best_rss <- r }
        break                                  # port first; the default algorithm only if port failed
      }
    }
    if (is.null(best))
      return(gm_fit_row(m, n_points = n, y0 = od0, t0 = t0, log_transform = log_transform, until = until,
                        message = paste0("did not converge: ", gsub("\\s+", " ", last_err))))
    cf  <- stats::coef(best)
    se  <- tryCatch(suppressWarnings(unname(summary(best)$coefficients[, "Std. Error"])),
                    error = function(e) rep(NA_real_, 3))
    if (length(se) != 3) se <- rep(NA_real_, 3)
    k_par <- 4L                                           # A, mu_max, lag + the residual variance
    aic <- n * (log(2 * pi) + 1 + log(max(best_rss, 1e-30) / n)) + 2 * k_par
    bic <- n * (log(2 * pi) + 1 + log(max(best_rss, 1e-30) / n)) + log(n) * k_par
    msg <- note
    if (cf[["A"]] >= 0.99 * A_ub)
      msg <- paste(stats::na.omit(c(msg, "asymptote at its upper bound (no plateau reached in the fitted points)")), collapse = "; ")
    gm_fit_row(m, TRUE, n, A = unname(cf["A"]), mu_max = unname(cf["mu"]), lag = unname(cf["lag"]), y0 = od0,
               rss = best_rss, aic = aic, bic = bic, r2 = if (sst > 0) 1 - best_rss / sst else NA_real_,
               se = se, message = msg, t0 = t0, log_transform = log_transform, until = until)
  })
  do.call(rbind, c(rows, list(make.row.names = FALSE)))
}

# Predicted OD of one fit row (as returned by fit_growth_models) at times t, starting from baseline
# od0 (use fit_row$y0 for the fitted curve itself). Back-transformed (od0 * exp(.)) when the fit was
# made on the ln scale; all NA for a model that did not converge.
predict_growth_model <- function(fit_row, t, od0) {
  fit_row <- fit_row[1, , drop = FALSE]
  if (!isTRUE(fit_row$converged)) return(rep(NA_real_, length(t)))
  fun <- GROWTH_MODEL_FUNS[[as.character(fit_row$model)]]
  t0  <- if ("t0" %in% names(fit_row) && is.finite(fit_row$t0)) fit_row$t0 else 0
  lg  <- if ("log_transform" %in% names(fit_row)) isTRUE(fit_row$log_transform) else TRUE
  f   <- fun(t - t0, fit_row$A, fit_row$mu_max, fit_row$lag)
  if (lg) od0 * exp(f) else od0 + f
}

# Stacked fits per sample from the app's mean-curve frame (time, variable, mean_value).
fit_growth_models_by_sample <- function(pd, ...) {
  if (is.null(pd) || nrow(pd) == 0) return(NULL)
  out <- lapply(unique(as.character(pd$variable)), function(s) {
    g <- pd[as.character(pd$variable) == s, , drop = FALSE]
    cbind(data.frame(sample = s, stringsAsFactors = FALSE), fit_growth_models(g$time, g$mean_value, ...))
  })
  do.call(rbind, c(out, list(make.row.names = FALSE)))
}

# Stacked fits per sample and replicate from the replicate frame (time, variable, replicate, value).
fit_growth_models_by_replicate <- function(rep_pd, ...) {
  if (is.null(rep_pd) || nrow(rep_pd) == 0) return(NULL)
  key <- unique(data.frame(sample = as.character(rep_pd$variable), replicate = rep_pd$replicate,
                           stringsAsFactors = FALSE))
  out <- lapply(seq_len(nrow(key)), function(i) {
    g <- rep_pd[as.character(rep_pd$variable) == key$sample[i] & rep_pd$replicate == key$replicate[i], , drop = FALSE]
    cbind(data.frame(sample = key$sample[i], replicate = key$replicate[i], stringsAsFactors = FALSE),
          fit_growth_models(g$time, g$value, ...))
  })
  do.call(rbind, c(out, list(make.row.names = FALSE)))
}

# The converged model with the lowest AIC, one row per sample (and per replicate when the table has a
# replicate column). A group with no converged model keeps one row with model = NA and the
# parameters NA.
best_model <- function(fit_table) {
  if (is.null(fit_table) || nrow(fit_table) == 0) return(fit_table)
  keys <- intersect(c("sample", "replicate"), names(fit_table))
  key_str <- if (length(keys)) do.call(paste, c(lapply(fit_table[keys], as.character), sep = "\r")) else rep("all", nrow(fit_table))
  rows <- lapply(unique(key_str), function(k) {
    g  <- fit_table[key_str == k, , drop = FALSE]
    ok <- which(g$converged %in% TRUE & !is.na(g$aic))
    if (length(ok)) return(g[ok[which.min(g$aic[ok])], , drop = FALSE])
    g <- g[1, , drop = FALSE]
    keep <- c(keys, "n_points", "message", "t0", "log_transform", "until", "y0")
    for (cc in setdiff(names(g), keep)) g[[cc]] <- if (is.character(g[[cc]])) NA_character_ else NA
    g$converged <- FALSE
    g
  })
  do.call(rbind, c(rows, list(make.row.names = FALSE)))
}

# ── Phenotype matrix (metrics x samples heatmap) ─────────────────────────────
# Columns of a metrics table that are bookkeeping, not phenotypes.
PHENOTYPE_NON_METRICS <- c("sample", "n_timepoints", "t_start", "t_end", "replicate", "n_replicates")

# Numeric columns of a metrics table that are candidate metrics (bookkeeping columns removed).
numeric_metric_columns <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(character(0))
  setdiff(names(df)[vapply(df, is.numeric, logical(1))], PHENOTYPE_NON_METRICS)
}

# samples x metrics matrix of a sample-level metrics table (compute_curve_metrics output; a replicate
# table is averaged per sample). scale = "z": each column centred on its mean and divided by its SD
# across samples; "range": each column mapped to 0-1 (min -> 0, max -> 1); "none": as is. A metric that
# is all NA or constant across samples (SD 0, so z and range are undefined), or that is not a numeric
# column of metrics_df, is not returned and is listed in `dropped` (by its column name).
phenotype_matrix <- function(metrics_df, metrics = NULL, scale = c("z", "range", "none")) {
  scale <- match.arg(scale)
  empty <- list(matrix = matrix(numeric(0), 0, 0), raw = matrix(numeric(0), 0, 0), dropped = character(0),
                metrics = character(0))
  if (is.null(metrics_df) || nrow(metrics_df) == 0 || !"sample" %in% names(metrics_df)) return(empty)
  cand <- if (is.null(metrics)) numeric_metric_columns(metrics_df) else as.character(metrics)
  usable <- cand[cand %in% names(metrics_df) & vapply(cand, function(m) is.numeric(metrics_df[[m]]), logical(1))]
  dropped <- setdiff(cand, usable)
  samples <- unique(as.character(metrics_df$sample))
  raw <- vapply(usable, function(m)
    vapply(samples, function(s) {
      v <- metrics_df[[m]][as.character(metrics_df$sample) == s]
      v <- v[is.finite(v)]
      if (length(v)) mean(v) else NA_real_
    }, numeric(1)), numeric(length(samples)))
  raw <- matrix(raw, nrow = length(samples), ncol = length(usable), dimnames = list(samples, usable))
  varies <- vapply(seq_len(ncol(raw)), function(j) {
    v <- raw[, j]; v <- v[is.finite(v)]
    length(v) >= 2 && diff(range(v)) > 0
  }, logical(1))
  dropped <- c(dropped, colnames(raw)[!varies])
  raw <- raw[, varies, drop = FALSE]
  kept <- colnames(raw)
  mat <- switch(scale,
    z     = apply(raw, 2, function(v) (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE)),
    range = apply(raw, 2, function(v) (v - min(v, na.rm = TRUE)) / (max(v, na.rm = TRUE) - min(v, na.rm = TRUE))),
    none  = raw)
  mat <- matrix(mat, nrow = nrow(raw), ncol = ncol(raw))
  lab <- if (length(kept)) as.character(metric_label(kept)) else character(0)
  dimnames(mat) <- list(rownames(raw), lab)
  dimnames(raw) <- list(rownames(raw), lab)
  list(matrix = mat, raw = raw, dropped = dropped, metrics = kept)
}

# ── All-metrics statistics ───────────────────────────────────────────────────
# compare_replicate_metric() for every metric (default: every numeric column of the replicate metrics
# table except sample / n_timepoints / t_start / t_end / replicate), stacked into one long table.
# p_adj_holm is the per-metric Holm adjustment from compare_replicate_metric (so it is identical to the
# Analysis tab); p_adj_bh_across is Benjamini-Hochberg over ALL tests in the table (every metric x
# comparison), an EXPLORATORY false-discovery control for scanning many metrics at once.
# stars come from p_adj_holm.
compare_all_metrics <- function(rm, metrics = NULL, ref_sample = NULL, method = c("welch", "nonparametric")) {
  method <- match.arg(method)
  cols <- c("metric", "metric_label", "sample", "versus", "n", "n_versus", "mean", "mean_versus", "difference",
            "hedges_g", "p", "p_adj_holm", "p_adj_bh_across", "stars", "ci_low", "ci_high", "test")
  empty <- data.frame(metric = character(0), metric_label = character(0), sample = character(0),
                      versus = character(0), n = integer(0), n_versus = integer(0), mean = numeric(0),
                      mean_versus = numeric(0), difference = numeric(0), hedges_g = numeric(0), p = numeric(0),
                      p_adj_holm = numeric(0), p_adj_bh_across = numeric(0), stars = character(0),
                      ci_low = numeric(0), ci_high = numeric(0), test = character(0), stringsAsFactors = FALSE)
  if (is.null(rm) || nrow(rm) == 0) return(list(table = empty, warnings = "No replicate metrics to compare."))
  if (is.null(metrics)) metrics <- numeric_metric_columns(rm)
  metrics <- metrics[metrics %in% names(rm)]
  warns <- character(0); parts <- list()
  for (m in metrics) {
    res <- compare_replicate_metric(rm, m, ref_sample = ref_sample, method = method)
    lab <- as.character(metric_label(m))
    if (length(res$warnings)) warns <- c(warns, paste0(lab, ": ", res$warnings))
    pw <- res$pairwise
    if (is.null(pw) || nrow(pw) == 0) next
    parts[[length(parts) + 1L]] <- cbind(data.frame(metric = m, metric_label = lab, stringsAsFactors = FALSE), pw)
  }
  if (!length(parts)) return(list(table = empty, warnings = warns))
  tab <- do.call(rbind, c(parts, list(make.row.names = FALSE)))
  tab$p_adj_bh_across <- p.adjust(tab$p, method = "BH")
  list(table = tab[, cols], warnings = warns)
}

# ── end v2.18 engine ──

# ── Optional UI widgets — must be pre-computed before fluidPage() ─────────────

pptx_ui <- if (has_officer && has_rvg) {
  downloadButton("downloadPPTX", "Download PowerPoint (.pptx)",
                 style = "background:#C0392B;color:white;border:none;width:100%;")
} else {
  p(style = "color:#999;font-size:.85em;",
    "Install 'officer' + 'rvg' to enable PPTX export.")
}

gif_ui <- if (has_gifski) {
  downloadButton("downloadGIF", "Download Animated GIF",
                 style = "background:#27AE60;color:white;border:none;width:100%;")
} else {
  p(style = "color:#999;font-size:.85em;",
    "Install 'gifski' to enable GIF export.")
}

html_report_ui <- if (has_base64enc) {
  downloadButton("downloadHTMLReport", "HTML Report (single file)",
                 style = "background:#2C3E50;color:white;border:none;width:100%;")
} else {
  p(style = "color:#999;font-size:.85em;",
    "Install 'base64enc' to enable the HTML report.")
}

pdf_report_ui <- if (has_gridExtra) {
  downloadButton("downloadPDFReport", "PDF Report (multi-page)",
                 style = "background:#8E44AD;color:white;border:none;width:100%;margin-top:6px;")
} else {
  p(style = "color:#999;font-size:.85em;",
    "Install 'gridExtra' to enable the PDF report.")
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("Optical Density (OD) Time-Series Visualization"),
  
  tags$head(
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('updateColorPreview', function(msg) {
         var el = document.getElementById(msg.id);
         if (el) el.style.backgroundColor = msg.color;
       });"
    )),
    # v2.14: the main panel's pixel width, so true-size previews can pick the zoom that fits it
    tags$script(HTML(
      "(function() {
         var timer = null;
         function report() {
           var el = document.getElementById('main_panel');
           if (el && window.Shiny && Shiny.setInputValue)
             Shiny.setInputValue('main_panel_width', Math.round(el.clientWidth));
         }
         window.addEventListener('resize', function() { clearTimeout(timer); timer = setTimeout(report, 200); });
         if (window.jQuery) jQuery(document).on('shiny:connected', report);
       })();"
    )),
    tags$style(HTML("
      .fig-bar { background:#f1f3f5; border:1px solid #dee2e6; border-radius:3px; padding:4px 10px;
                 font-size:.85em; margin-bottom:8px; color:#495057; }
      .fig-bar.warn { background:#fff3cd; border-color:#ffe08a; color:#856404; }
      .fig-bar .btn { margin-left:10px; padding:0 8px; font-size:.95em; }
      .panel-section {
        background:#f8f9fa; padding:12px; border-radius:5px;
        margin:10px 0; border:1px solid #e9ecef;
      }
      .panel-title { margin-top:0; margin-bottom:10px; font-weight:bold; color:#495057; }
      details > summary {
        cursor:pointer; font-weight:bold; padding:6px 10px;
        background:#e9ecef; border-radius:3px; list-style:none; user-select:none;
      }
      details > summary::-webkit-details-marker { display:none; }
      details[open] > summary::before { content:'\\25BC  '; font-size:.75em; }
      details > summary::before       { content:'\\25B6  '; font-size:.75em; }
      details { margin-bottom:6px; }
      .color-preview {
        display:inline-block; width:28px; height:28px;
        border:1px solid #ccc; border-radius:4px; flex-shrink:0;
      }
      .settings-status { padding:8px 12px; border-radius:4px; font-size:.85em; margin-top:6px; }
      #figure_legend_text { white-space: pre-wrap; font-size: .82em; line-height: 1.35; }
      .analysis-controls { background:#f8f9fa; border:1px solid #e9ecef; border-radius:6px; padding:10px 12px 0; margin-bottom:10px; }
      .dash-cards { display:flex; flex-wrap:wrap; gap:10px; }
      .dash-card  { flex:1 1 150px; background:#f8f9fa; border:1px solid #e9ecef; border-radius:6px; padding:10px 12px; }
      .dash-card-title { font-size:.75em; color:#6c757d; text-transform:uppercase; letter-spacing:.04em; }
      .dash-card-value { font-size:1.35em; font-weight:bold; color:#2C3E50; margin:2px 0; }
      .dash-card-sub   { font-size:.78em; color:#6c757d; }
      .dash-empty { color:#6c757d; padding:30px; text-align:center; }
      .stats-note { background:#fff3cd; border:1px solid #ffe69c; border-radius:4px; padding:6px 8px; margin:4px 0 8px; font-size:.8em; color:#664d03; }
      .settings-status.ok   { background:#d4edda; color:#155724; border:1px solid #c3e6cb; }
      .settings-status.warn { background:#fff3cd; color:#856404; border:1px solid #ffeeba; }
      .match-list { margin:4px 0 0 0; padding-left:16px; }
      .excl-preview {
        background:#fff3cd; color:#856404; padding:6px 10px;
        border-radius:4px; font-size:.82em; margin-top:4px;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 
                 fileInput("file", "Choose CSV File", accept = c("text/csv", ".csv")),
                 selectInput("csv_locale", "CSV format:",
                             choices = c("Auto-detect"                              = "auto",
                                        "US/UK (comma-separated, . decimal)"        = "us",
                                        "European (semicolon-separated, , decimal)" = "eu"),
                             selected = "auto"),
                 numericInput("blank_od", "Subtract blank/media OD (optional):", 0, step = 0.001),
                 p(style = "font-size:.78em;color:#777;margin-top:-8px;",
                   "0 = off (default). This app assumes pre-blanked input; if your CSV still has ",
                   "a media/blank OD baked in, enter it here — it's subtracted from every ",
                   "reading before plotting, statistics, and curve metrics, and noted in the ",
                   "report provenance. Small negative values after subtraction are expected and ",
                   "handled correctly (see the μmax non-positive-OD note above)."),
                 div(style = "display:flex;gap:6px;align-items:flex-start;margin:-8px 0 10px 0;",
                     div(class = "demo-pick", style = "flex:1 1 55%;min-width:0;font-size:.85em;",
                         selectInput("demo_scenario", NULL, width = "100%",
                                     choices = c("Lysis curve (3 samples)"      = "lysis",
                                                 "MOI series (dose-response)"   = "moi_series",
                                                 "Resistant regrowth"           = "resistance",
                                                 "Time in hours"                = "hours",
                                                 "Two phages x two MOIs"        = "two_phages"),
                                     selected = "lysis"),
                         tags$style(".demo-pick .form-group{margin-bottom:0;}")),
                     actionButton("load_demo", "Load Demo Data", icon = icon("flask"),
                                  style = "flex:1 1 45%;font-size:.85em;")),
                 uiOutput("data_summary_ui"),
                 uiOutput("qc_flags_ui"),

                 # ── Settings ─────────────────────────────────────────────────────────────
                 div(class = "panel-section",
                     h4("Save / Load Settings", class = "panel-title"),
                     p(style = "font-size:.82em;color:#555;margin-bottom:8px;",
                       "Saves all visual settings: axis scales, labels, error bars, fonts, line/point options, ",
                       "AND per-sample aesthetics (color, shape, linetype) keyed by sample name. ",
                       "Loading restores all those settings — but never touches your data, ",
                       "sample selection, or time filter."),
                     fluidRow(
                       column(6, downloadButton("saveSettings",  "Save Settings",  style = "width:100%")),
                       column(6, actionButton("clearSettings", "Clear & Reset",
                                              icon  = icon("times"),
                                              style = "width:100%;background:#dc3545;color:white;border:none;"))
                     ),
                     br(),
                     fileInput("loadSettings", "Load Settings File",
                               accept = c("application/json", ".json")),
                     uiOutput("settings_status_ui")
                 ),
                 
                 # ── Axis Settings ─────────────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Axis Settings"),
                   div(class = "panel-section",
                       h4("Axis Scales", class = "panel-title"),
                       selectInput("x_scale_type", "X-axis Scale:",
                                   choices  = c("Linear" = "linear", "Logarithmic" = "log",
                                                "Square Root" = "sqrt", "Reverse" = "reverse"),
                                   selected = "linear"),
                       selectInput("y_scale_type", "Y-axis Scale:",
                                   choices  = c("Linear" = "linear", "Logarithmic" = "log",
                                                "Square Root" = "sqrt", "Reverse" = "reverse"),
                                   selected = "log"),
                       checkboxInput("use_advanced_ticks", "Advanced Tick Customization", value = TRUE),
                       conditionalPanel(
                         condition = "input.use_advanced_ticks == true && input.y_scale_type == 'log'",
                         h5("Y-axis Log Tick Range"),
                         fluidRow(
                           column(6, numericInput("y_log_min_exponent", "Min Exponent:", value = -2)),
                           column(6, numericInput("y_log_max_exponent", "Max Exponent:", value =  0))
                         )
                       ),
                       conditionalPanel(
                         condition = "input.use_advanced_ticks == true && input.x_scale_type == 'linear'",
                         h5("X-axis Tick Control", style = "margin:10px 0 4px;"),
                         fluidRow(
                           column(6,
                                  numericInput("x_tick_interval", "Major Interval:",
                                               value = NA, min = 0.001, step = 1)
                           ),
                           column(6,
                                  p(style = "font-size:.8em;color:#777;margin-top:28px;",
                                    "Leave blank = auto")
                           )
                         ),
                         textInput("x_extra_ticks", "Extra tick positions:",
                                   value = "", placeholder = "e.g. 270, 450"),
                         p(style = "font-size:.8em;color:#777;margin-top:-6px;",
                           "Values always shown regardless of interval. Comma-separated.")
                       ),
                       checkboxInput("custom_x_limits", "Custom X-axis limits", value = FALSE),
                       conditionalPanel(condition = "input.custom_x_limits == true",
                                        fluidRow(
                                          column(6, numericInput("x_min", "X-min:", value = 0)),
                                          column(6, numericInput("x_max", "X-max:", value = 100))
                                        )
                       ),
                       checkboxInput("custom_y_limits", "Custom Y-axis limits", value = FALSE),
                       conditionalPanel(condition = "input.custom_y_limits == true",
                                        fluidRow(
                                          column(6, numericInput("y_min", "Y-min:", value = 0.01)),
                                          column(6, numericInput("y_max", "Y-max:", value = 2))
                                        )
                       ),
                       selectInput("y_oob", "Readings below the axis floor:",
                                   choices = c("Clip to the floor (recommended)" = "squish",
                                               "Drop (legacy)"                   = "censor"),
                                   selected = APP_DEFAULTS$y_oob),
                       p(style = "font-size:.8em;color:#777;margin-top:-6px;",
                         "A post-lysis reading below the log-axis floor is drawn ON the floor instead of ",
                         "being deleted (the old behaviour, 'Drop'), so the curve and its points stay visible."),
                       sliderInput("x_expand_left",   "X Left Expand:",   0, 0.1, 0,    0.01),
                       sliderInput("x_expand_right",  "X Right Expand:",  0, 0.1, 0.05, 0.01),
                       sliderInput("y_expand_bottom", "Y Bottom Expand:", 0, 0.1, 0,    0.01),
                       sliderInput("y_expand_top",    "Y Top Expand:",    0, 0.1, 0.05, 0.01)
                   ),
                   div(class = "panel-section",
                       h4("Labels & Formatting", class = "panel-title"),
                       textInput("x_axis_label",  "X-axis Label:",  "Time (minutes)"),
                       textInput("y_axis_label",  "Y-axis Label:",  "A550"),
                       checkboxInput("axis_subscripts", "Typeset wavelength as subscript (A550 -> A\u2085\u2085\u2080)",
                                     APP_DEFAULTS$axis_subscripts),
                       textInput("plot_title",    "Plot Title:",    ""),
                       textInput("plot_subtitle", "Plot Subtitle:", ""),
                       h5("Gridlines", style = "margin-top:12px;"),
                       checkboxInput("show_major_gridlines", "Show Major Gridlines", APP_DEFAULTS$show_major_gridlines),
                       checkboxInput("show_minor_gridlines", "Show Minor Gridlines", APP_DEFAULTS$show_minor_gridlines),
                       conditionalPanel(condition = "input.show_major_gridlines == true",
                                        selectInput("major_gridline_color", "Major Gridline Color:",
                                                    choices  = c("Light Gray" = "#E6E6E6", "Medium Gray" = "#CCCCCC",
                                                                 "Dark Gray"  = "#999999", "Light Blue"  = "#DEEBF7",
                                                                 "Custom"     = "custom"),
                                                    selected = "#E6E6E6"),
                                        conditionalPanel(condition = "input.major_gridline_color == 'custom'",
                                                         textInput("major_gridline_color_custom", "HEX:", "#E6E6E6")),
                                        sliderInput("major_gridline_size", "Width:", 0.1, 1, 0.5, 0.1)
                       ),
                       conditionalPanel(condition = "input.show_minor_gridlines == true",
                                        selectInput("minor_gridline_color", "Minor Gridline Color:",
                                                    choices  = c("Light Gray" = "#F2F2F2", "Medium Gray" = "#E6E6E6",
                                                                 "Dark Gray"  = "#CCCCCC", "Light Blue"  = "#EFF6FB",
                                                                 "Custom"     = "custom"),
                                                    selected = "#F2F2F2"),
                                        conditionalPanel(condition = "input.minor_gridline_color == 'custom'",
                                                         textInput("minor_gridline_color_custom", "HEX:", "#F2F2F2")),
                                        sliderInput("minor_gridline_size", "Width:", 0.1, 1, 0.3, 0.1)
                       ),
                       h5("Font Settings", style = "margin-top:12px;"),
                       selectInput("font_family", "Font Family:",
                                   choices  = c("Sans Serif (Arial/Helvetica)" = "sans",
                                                "Arial"                        = "Arial",
                                                "Helvetica"                    = "Helvetica",
                                                "Serif (Times New Roman)"      = "serif",
                                                "Monospace (Courier New)"      = "mono",
                                                "Helvetica Neue"               = "Helvetica Neue",
                                                "Garamond"                     = "Garamond",
                                                "Palatino"                     = "Palatino"),
                                   selected = APP_DEFAULTS$font_family),
                       numericInput("title_font_size",      "Title Font Size:",  APP_DEFAULTS$title_font_size, 5, 48),
                       numericInput("axis_label_font_size", "Axis Label Size:",  APP_DEFAULTS$axis_label_font_size, 5, 36),
                       numericInput("axis_text_font_size",  "Axis Text Size:",   APP_DEFAULTS$axis_text_font_size, 5, 32),
                       numericInput("legend_font_size",     "Legend Font Size:", APP_DEFAULTS$legend_font_size, 5, 32),
                       checkboxInput("bold_title",         "Bold Title",         APP_DEFAULTS$bold_title),
                       checkboxInput("italic_axis_labels", "Italic Axis Labels", APP_DEFAULTS$italic_axis_labels),
                       selectInput("axis_text_angle", "X-axis Text Angle:",
                                   choices  = c("Horizontal (0\u00b0)" = "0",
                                                "Angled (45\u00b0)"    = "45",
                                                "Vertical (90\u00b0)"  = "90"),
                                   selected = "0")
                   ),
                   div(class = "panel-section",
                       h4("Figure Layout", class = "panel-title"),
                       p(style = "font-size:.8em;color:#777;margin-top:-4px;",
                         "Line widths in pt and lengths in mm, as journals specify them. ",
                         "The app starts in the journal standard; the Legacy look button restores the pre-v2.14 look."),
                       selectInput("legend_position", "Legend Position:",
                                   choices = c("Right (outside)"      = "right",
                                               "Top (outside)"        = "top",
                                               "Bottom (outside)"     = "bottom",
                                               "Left (outside)"       = "left",
                                               "Inside: top-left"     = "inside-top-left",
                                               "Inside: top-right"    = "inside-top-right",
                                               "Inside: bottom-left"  = "inside-bottom-left",
                                               "Inside: bottom-right" = "inside-bottom-right",
                                               "Inside: emptiest corner (auto)" = "inside-auto",
                                               "None"                 = "none"),
                                   selected = APP_DEFAULTS$legend_position),
                       checkboxInput("legend_box_border", "Legend box border", APP_DEFAULTS$legend_box_border),
                       selectInput("legend_order", "Sample order (legend, panels, facets):",
                                   choices = c("Alphabetical (legacy)" = "alpha",
                                               "As selected / as in the file" = "selected"),
                                   selected = APP_DEFAULTS$legend_order),
                       numericInput("legend_key_mm", "Legend key size (mm; 0 = auto):", APP_DEFAULTS$legend_key_mm, 0, 20, 0.5),
                       fluidRow(
                         column(6, numericInput("axis_line_pt",   "Axis / tick line (pt):", APP_DEFAULTS$axis_line_pt, 0.1, 4, 0.05)),
                         column(6, numericInput("tick_length_mm", "Tick length (mm):",     APP_DEFAULTS$tick_length_mm,  0,  10, 0.1))
                       ),
                       fluidRow(
                         column(6, numericInput("theme_base_pt", "Spacing base size (pt; 0 = theme default):", APP_DEFAULTS$theme_base_pt, 0, 30, 0.5)),
                         column(6, selectInput("y_tick_label_style", "Y tick labels:",
                                               choices = c("As is (0.10)"     = "legacy",
                                                           "Trim zeros (0.1)" = "trim",
                                                           "Powers of ten"    = "log10"),
                                               selected = APP_DEFAULTS$y_tick_label_style))
                       ),
                       p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                         "Spacing base size scales margins, tick/label gaps and legend spacing; ",
                         "set it near your text size (7 pt) for journal figures."),
                       fluidRow(
                         column(6, textInput("panel_label", "Panel label:", APP_DEFAULTS$panel_label, placeholder = "a")),
                         column(6, numericInput("panel_label_size", "Panel label (pt):", APP_DEFAULTS$panel_label_size, 5, 20, 0.5))
                       ),
                       # v2.15: the panel (the plotting area) has a fixed size and the figure grows around it
                       h5("Panel size", style = "margin-top:12px;"),
                       radioButtons("panel_size_mode", NULL,
                                    choices = c("Fixed panel size: the figure grows around it (axes, labels, outside legend)" = "fixed",
                                                "Fit the export size (legacy: the panel fills width x height)" = "fit"),
                                    selected = APP_DEFAULTS$panel_size_mode),
                       conditionalPanel(condition = "input.panel_size_mode == 'fixed'",
                                        fluidRow(
                                          column(6, numericInput("panel_width_mm", "Panel width (mm):", APP_DEFAULTS$panel_width_mm, 10, 400, 1)),
                                          column(6, conditionalPanel(condition = "input.panel_square == false",
                                                                     numericInput("panel_height_mm", "Panel height (mm):", APP_DEFAULTS$panel_height_mm, 10, 400, 1)))
                                        ),
                                        checkboxInput("panel_square", "Square panel (curve figures: height = width)", APP_DEFAULTS$panel_square),
                                        p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                                          "The size is that of the plotting area itself; the figure is sized around it. ",
                                          "The heatmap and the metric summary use the width and height above (the square ",
                                          "option is for the curve figures).")
                       )
                   )
                 ),
                 
                 # ── Time Point Filtering ──────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Time Point Filtering"),
                   div(class = "panel-section",
                       h4("Restrict Displayed Time Range", class = "panel-title"),
                       p(style = "font-size:.85em;color:#555;",
                         "Filters data before plotting. All statistics, error bars, and exports reflect the filtered data."),
                       checkboxInput("enable_time_filter", "Enable Time Filtering", value = FALSE),
                       conditionalPanel(condition = "input.enable_time_filter == true",
                                        h5("Time Range", style = "margin:8px 0 4px;"),
                                        uiOutput("time_range_slider_ui"),
                                        hr(style = "margin:10px 0;"),
                                        h5("Exclude Specific Time Points", style = "margin:8px 0 4px;"),
                                        p(style = "font-size:.82em;color:#666;", "Comma-separated values to remove (e.g. 0, 5, 180)."),
                                        textInput("exclude_timepoints", label = NULL, value = "", placeholder = "e.g. 0, 5, 180"),
                                        uiOutput("excluded_timepoints_preview")
                       )
                   )
                 ),
                 
                 # ── Region Highlighting ───────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Region Highlighting"),
                   div(class = "panel-section",
                       checkboxInput("enable_highlighting", "Enable Region Highlighting", FALSE),
                       conditionalPanel(condition = "input.enable_highlighting == true",
                                        numericInput("region_count", "Number of Regions:", 1, 1, 5),
                                        uiOutput("region_settings")
                       )
                   )
                 ),
                 
                 # ── Time Point Markers ────────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Time Point Markers"),
                   div(class = "panel-section",
                       checkboxInput("enable_time_markers", "Enable Time Markers", FALSE),
                       conditionalPanel(condition = "input.enable_time_markers == true",
                                        numericInput("marker_count", "Number of Markers:", 1, 1, 10),
                                        uiOutput("time_marker_settings")
                       ),
                       hr(),
                       checkboxInput("enable_threshold", "Show a horizontal threshold line (OD)", APP_DEFAULTS$enable_threshold),
                       conditionalPanel(condition = "input.enable_threshold == true",
                                        numericInput("threshold_value", "Threshold (OD):", APP_DEFAULTS$threshold_value, step = 0.01),
                                        textInput("threshold_label", "Label (blank = none):", APP_DEFAULTS$threshold_label),
                                        fluidRow(
                                          column(6, selectInput("threshold_color", "Colour:",
                                                                choices = c("Grey" = "grey40", "Black" = "black", "Red" = "firebrick",
                                                                            "Blue" = "steelblue4", "Green" = "darkgreen"),
                                                                selected = APP_DEFAULTS$threshold_color)),
                                          column(6, selectInput("threshold_linetype", "Line type:",
                                                                choices = c("Dashed" = "dashed", "Dotted" = "dotted", "Solid" = "solid",
                                                                            "Dot-dash" = "dotdash"),
                                                                selected = APP_DEFAULTS$threshold_linetype))
                                        ),
                                        p(style = "font-size:.8em;color:#777;margin-top:-6px;",
                                          "Drawn at the axis-line weight; the label sits at the right end in the axis-text size.")
                       )
                   )
                 ),
                 
                 # ── Color Palettes ────────────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Color Palettes"),
                   div(class = "panel-section",
                       selectInput("color_palette", "Color Palette:",
                                   choices  = c("Custom"              = "custom",
                                                "Viridis"             = "viridis",
                                                "Plasma"              = "plasma",
                                                "Colorblind-friendly" = "colorblind",
                                                "Publication"         = "publication",
                                                "Rainbow"             = "rainbow",
                                                "Grayscale"           = "gray",
                                                "Blues (dose gradient)"        = "blues",
                                                "Blue to red (dose gradient)"  = "blue_red"),
                                   selected = APP_DEFAULTS$color_palette),
                       htmlOutput("palette_preview")
                   )
                 ),
                 
                 # ── Line & Point Settings ─────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Line & Point Settings"),
                   div(class = "panel-section",
                       h4("Line Options", class = "panel-title"),
                       sliderInput("line_thickness", "Line Thickness (1 = 2.1 pt):", 0.1, 3, APP_DEFAULTS$line_thickness, 0.05)
                   ),
                   div(class = "panel-section",
                       h4("Point Options", class = "panel-title"),
                       checkboxInput("show_points", "Show Points", TRUE),
                       conditionalPanel(condition = "input.show_points == true",
                                        sliderInput("shape_size",   "Point Size:",   0.5, 8, APP_DEFAULTS$shape_size,   0.1),
                                        sliderInput("point_stroke", "Stroke Width:", 0,   2, APP_DEFAULTS$point_stroke, 0.05)
                       )
                   )
                 ),
                 
                 # ── Label Options ─────────────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Label Options"),
                   div(class = "panel-section",
                       checkboxInput("show_end_labels", "Show End-of-Line Labels", FALSE),
                       conditionalPanel(condition = "input.show_end_labels == true",
                                        numericInput("label_font_size", "Label Font Size (pt):", APP_DEFAULTS$label_font_size, 3, 36),
                                        checkboxInput("label_bold",  "Bold Labels",              TRUE),
                                        numericInput("label_offset", "Label Offset (% x-axis):", 3.5, 0, 20)
                       )
                   )
                 ),
                 
                 # ── Error Bars / Shadow ───────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Error Bars / Shadow"),
                   div(class = "panel-section",
                       selectInput("error_type", "Error Type:",
                                   choices  = c("None" = "none", "SD" = "sd",
                                                "SEM"  = "sem",  "95% CI" = "ci95"),
                                   selected = "sem"),
                       conditionalPanel(condition = "input.error_type != 'none'",
                                        selectInput("error_display_mode", "Display Mode:",
                                                    choices  = c("Error Bars" = "bars", "Shadow/Ribbon" = "shadow"),
                                                    selected = "bars"),
                                        numericInput("error_multiplier", "Error Multiplier:", 1, 0.1, 5, 0.1),
                                        checkboxInput("asymmetric_error", "Floor errors at zero", FALSE),
                                        selectInput("error_bar_color", "Error colour:",
                                                    choices = c("Sample colour" = "sample", "Black" = "black", "Grey" = "grey50"),
                                                    selected = APP_DEFAULTS$error_bar_color),
                                        checkboxInput("show_replicate_points",
                                                      "Show individual replicate values (open circles behind the mean)", FALSE),
                                        checkboxInput("error_log_floor",
                                                      "On a log axis, draw bars that cross zero down to the axis minimum", TRUE),
                                        p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                                          "Off, such a bar is dropped silently (the old behaviour). ",
                                          "The lower whisker is then shown as a bar reaching the axis floor."),
                                        conditionalPanel(condition = "input.error_display_mode == 'bars'",
                                                         selectInput("error_bar_style", "Bar Style:",
                                                                     choices  = c("T-shaped"     = "T",
                                                                                  "Solid lines"  = "solid",
                                                                                  "Dashed lines" = "dashed"),
                                                                     selected = "T"),
                                                         sliderInput("error_bar_width",     "Cap width:", 0.01, 50, APP_DEFAULTS$error_bar_width, 0.01),
                                                         checkboxInput("error_cap_relative", "Cap width is % of the x range (same look at any time scale)", APP_DEFAULTS$error_cap_relative),
                                                         sliderInput("error_bar_thickness", "Bar Thickness (1 = 2.1 pt):", 0.1,  2, APP_DEFAULTS$error_bar_thickness, 0.05),
                                                         selectInput("error_bar_position", "Position:",
                                                                     choices  = c("Middle" = "middle", "Dodge" = "dodge"),
                                                                     selected = "middle")
                                        ),
                                        conditionalPanel(condition = "input.error_display_mode == 'shadow'",
                                                         sliderInput("shadow_alpha", "Ribbon Alpha:", 0.05, 0.6, 0.2, 0.05)
                                        ),
                                        checkboxInput("show_error_caption",
                                                      "Annotate error type + n in caption",
                                                      value = APP_DEFAULTS$show_error_caption),
                                        p(style = "font-size:.8em;color:#777;margin-top:-6px;",
                                          "Adds a figure caption stating the error measure (SD / SEM / 95% CI) ",
                                          "and the replicate count — on by default (KC-6) so a default SEM-bar ",
                                          "figure never leaves the app unlabelled; recommended for publication figures.")
                       )
                   )
                 ),
                 
                 # ── Journal Figure Preset (v2.10) ─────────────────────────────────────────
                 tags$details(
                   tags$summary("Journal Figure Preset"),
                   div(class = "panel-section",
                       h4("Target Journal & Width", class = "panel-title"),
                       p(style = "font-size:.8em;color:#777;margin-top:-4px;",
                         "Sets the export size to a real column width, text to Arial 6-8 pt, lines to ",
                         "0.5-0.75 pt, the Okabe-Ito colour-blind-safe palette, and the legend inside the ",
                         "panel. Everything it sets stays an ordinary control you can adjust. ",
                         "Export as SVG, edit in Inkscape, never rescale. See docs/FIGURE_STANDARDS.md."),
                       selectInput("journal_family", "Journal:",
                                   choices  = setNames(names(JOURNAL_PRESETS),
                                                       vapply(JOURNAL_PRESETS, `[[`, "", "label")),
                                   selected = "nature"),
                       selectInput("journal_columns", "Width:",
                                   choices  = c("Single column"                               = "single",
                                                "1.5 columns"                                 = "onehalf",
                                                "Double column (full width)"                  = "double",
                                                "Half of a double column (one panel of two)" = "half_double"),
                                   selected = "single"),
                       conditionalPanel(condition = "input.journal_family == 'custom'",
                                        numericInput("journal_custom_width_mm", "Width (mm):", 89, 20, 300, 1)),
                       numericInput("journal_height_mm", "Height (mm):", 65, 20, 250, 1),
                       textInput("journal_panel_label", "Panel label (optional):", "", placeholder = "a"),
                       uiOutput("journal_preset_summary"),
                       actionButton("apply_journal_preset", "Apply journal preset",
                                    style = "background:#2C3E50;color:white;border:none;width:100%;"),
                       actionButton("apply_legacy_look", "Legacy look (v2.9 defaults)",
                                    style = "background:#f8f9fa;color:#495057;border:1px solid #ced4da;width:100%;margin-top:6px;"),
                       p(style = "font-size:.78em;color:#777;margin-top:4px;",
                         "The app starts in the journal standard (Nature single column, 89 x 65 mm). ",
                         "Legacy look restores the pre-v2.14 start: 10 x 8 in, 20 pt text, PDF, pixel canvas."),
                       p(style = "font-size:.78em;color:#777;margin-top:6px;",
                         "Use Save Settings afterwards to keep the result as a reusable style file.")
                   ),
                   div(class = "panel-section",
                       h4("Figure Legend Text", class = "panel-title"),
                       p(style = "font-size:.8em;color:#777;margin-top:-4px;",
                         "Drafted from the current plot and the Experiment Notebook: measure, replicate n, ",
                         "error type, MOI / host / phage / medium. Copy into the manuscript and edit."),
                       verbatimTextOutput("figure_legend_text", placeholder = TRUE)
                   )
                 ),
                 
                 # ── Plot Dimensions & Export ──────────────────────────────────────────────
                 tags$details(
                   tags$summary("Plot Dimensions & Export"),
                   div(class = "panel-section",
                       h4("On-screen Preview", class = "panel-title"),
                       radioButtons("preview_mode", NULL,
                                    choices  = c("Screen canvas (pixels)"                      = "canvas",
                                                 "True export size (what Inkscape will open)" = "wysiwyg"),
                                    selected = APP_DEFAULTS$preview_mode),
                       conditionalPanel(condition = "input.preview_mode == 'canvas'",
                                        numericInput("plot_width", "Width (px):", 820, 400, 2000),
                                        p(style = "font-size:.8em;color:#777;margin-top:-6px;",
                                          "The height follows the export size, so the figure keeps its proportions.")
                       ),
                       conditionalPanel(condition = "input.preview_mode == 'wysiwyg'",
                                        checkboxInput("preview_zoom_auto", "Fit the preview to the panel width",
                                                      APP_DEFAULTS$preview_zoom_auto),
                                        conditionalPanel(condition = "input.preview_zoom_auto == false",
                                          sliderInput("preview_zoom", "Zoom (x physical size):", 0.5, 4, APP_DEFAULTS$preview_zoom, 0.25)),
                                        p(style = "font-size:.8em;color:#777;margin-top:-6px;",
                                          "Rendered at the export width/height below through the same ",
                                          "pipeline as the download; text and lines appear at their true pt size.")
                       ),
                       checkboxInput("custom_aspect_ratio", "Custom Aspect Ratio", APP_DEFAULTS$custom_aspect_ratio),
                       conditionalPanel(condition = "input.custom_aspect_ratio == true",
                                        numericInput("aspect_ratio", "Width/Height:", 1, 0.3, 4, 0.1)
                       ),
                       h4("Image Export", class = "panel-title"),
                       selectInput("export_format", "Format:",
                                   choices  = c("PDF (fonts embedded)" = "pdf",
                                                "SVG (Inkscape-ready)"  = "svg",
                                                "PNG"                   = "png",
                                                "TIFF (LZW)"            = "tiff",
                                                "JPEG"                  = "jpeg"),
                                   selected = APP_DEFAULTS$export_format),
                       radioButtons("export_units", "Size units:",
                                    choices = c("inches" = "in", "mm" = "mm"),
                                    selected = APP_DEFAULTS$export_units, inline = TRUE),
                       conditionalPanel(condition = "input.panel_size_mode != 'fixed'",
                         fluidRow(
                           column(6, numericInput("export_width",  paste0("Width (", APP_DEFAULTS$export_units, "):"),  APP_DEFAULTS$export_width, 0.5, 400)),
                           column(6, numericInput("export_height", paste0("Height (", APP_DEFAULTS$export_units, "):"), APP_DEFAULTS$export_height,  0.5, 400))
                         )),
                       uiOutput("export_size_note"),
                       numericInput("export_dpi", "DPI (raster formats):", APP_DEFAULTS$export_dpi, 72, 1200),
                       downloadButton("downloadPlot", "Download Image"),
                       br(), br(),
                       downloadButton("downloadFigureBundle", "Download figure bundle (.zip)", style = "width:100%;"),
                       p(style = "font-size:.78em;color:#777;margin-top:4px;",
                         "Everything a figure submission needs in one file: the figure (SVG + PDF) at the current size, ",
                         "the composed figure (SVG, if two or more panels are selected), settings.json, the tidy source data, ",
                         "summary statistics, curve metrics, the legend text, the methods text and a README."),
                       hr(),
                       h4("PowerPoint Export", class = "panel-title"),
                       p(style = "font-size:.85em;color:#666;",
                         "Cumulative build: slide 1 shows sample 1, slide 2 adds sample 2, etc. Each slide title shows the newly-added sample name. Final slide = all samples, identical to the main plot."),
                       pptx_ui,
                       hr(),
                       h4("Animated GIF Export", class = "panel-title"),
                       p(style = "font-size:.85em;color:#666;",
                         "Cumulative build: each frame adds one more line. No grey masks — lines already drawn stay fully visible. Axes, theme, and error bars match the main plot exactly."),
                       numericInput("gif_fps",    "Frames per second:", 1,   0.2, 10,   0.2),
                       numericInput("gif_width",  "GIF Width (px):",    800, 300, 2000, 50),
                       numericInput("gif_height", "GIF Height (px):",   600, 200, 1500, 50),
                       gif_ui
                   )
                 ),

                 # ── Experiment Notebook ───────────────────────────────────────────────────
                 # Structured lab-notebook record (modeled on the companion app's
                 # Experiment Notes tab). Everything filled in here is embedded in
                 # the HTML / PDF reports; empty fields are skipped.
                 tags$details(
                   tags$summary("Experiment Notebook"),
                   div(class = "panel-section",
                       h4("Experiment Identity", class = "panel-title"),
                       textInput("nb_exp_id",       "Experiment ID / Name:", ""),
                       fluidRow(
                         column(6, dateInput("nb_date", "Date:", value = Sys.Date())),
                         column(6, textInput("nb_experimenter", "Experimenter:", ""))
                       ),
                       fluidRow(
                         column(6, textInput("nb_project",     "Project:", "")),
                         column(6, textInput("nb_institution", "Lab / Institution:", ""))
                       )
                   ),
                   div(class = "panel-section",
                       h4("Biology & Conditions", class = "panel-title"),
                       selectInput("nb_exp_type", "Experiment Type:",
                                   choices = c("Phage Infection", "Plasmid Induction",
                                               "Growth Curve", "Lysis Curve",
                                               "Dose-Response", "Other"),
                                   selected = "Phage Infection"),
                       fluidRow(
                         column(6, textInput("nb_host_strain",   "Host Strain(s):", "")),
                         column(6, textInput("nb_phage_plasmid", "Phage / Plasmid:", ""))
                       ),
                       fluidRow(
                         column(6, textInput("nb_moi",       "MOI:", "")),
                         column(6, textInput("nb_replicate", "Replicate / Batch:", ""))
                       ),
                       fluidRow(
                         column(6, textInput("nb_passage",        "Passage #:", "")),
                         column(6, textInput("nb_inducer_conc",   "Inducer concentration:", ""))
                       ),
                       fluidRow(
                         column(6, textInput("nb_media",       "Growth Medium:", "")),
                         column(6, textInput("nb_temperature", "Temperature (°C):", ""))
                       ),
                       fluidRow(
                         column(6, textInput("nb_time_inf", "Time of Infection/Induction:", "")),
                         column(6, textInput("nb_inducer",  "Inducer:", ""))
                       ),
                       textInput("nb_other_condition", "Other condition:", "")
                   ),
                   div(class = "panel-section",
                       h4("Notes", class = "panel-title"),
                       textAreaInput("nb_observations", "Observations / Results:", "", rows = 3),
                       textAreaInput("nb_issues",       "Issues / Anomalies:",     "", rows = 2),
                       textAreaInput("nb_next_steps",   "Next Steps:",             "", rows = 2),
                       textInput("nb_tags", "Tags (comma-separated):", "",
                                 placeholder = "e.g. pilot, MG1655, N4, low-MOI"),
                       h5("Custom Fields", style = "margin-top:10px;"),
                       fluidRow(
                         column(5, textInput("nb_custom_key1", "Key:",   "", placeholder = "Field")),
                         column(7, textInput("nb_custom_val1", "Value:", ""))
                       ),
                       fluidRow(
                         column(5, textInput("nb_custom_key2", "Key:",   "", placeholder = "Field")),
                         column(7, textInput("nb_custom_val2", "Value:", ""))
                       ),
                       fluidRow(
                         column(5, textInput("nb_custom_key3", "Key:",   "", placeholder = "Field")),
                         column(7, textInput("nb_custom_val3", "Value:", ""))
                       )
                   ),
                   div(class = "panel-section",
                       h4("Notebook Downloads", class = "panel-title"),
                       downloadButton("download_notebook_txt", "Download notebook (.txt)", style = "width:100%;"),
                       br(), br(),
                       downloadButton("download_notebook_csv", "Download notebook row (.csv)", style = "width:100%;"),
                       p(style = "font-size:.78em;color:#777;margin-top:4px;",
                         "The .txt lists every filled notebook field (key: value per line) followed by the figure legend ",
                         "text and the methods text. The .csv is ONE row with one column per filled field (custom keys ",
                         "become column names): append it to your master experiment spreadsheet.")
                   )
                 ),

                 # ── Stats & Reports ───────────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Stats & Reports"),
                   div(class = "panel-section",
                       h4("Statistics Export", class = "panel-title"),
                       p(style = "font-size:.82em;color:#555;",
                         "Tables reflect the current sample selection and time filtering — ",
                         "exactly what the plot shows."),
                       downloadButton("downloadStatsCSV", "Summary Statistics (CSV)",
                                      style = "width:100%;"),
                       br(), br(),
                       downloadButton("downloadMetricsCSV", "Curve Metrics (CSV)",
                                      style = "width:100%;"),
                       p(style = "font-size:.78em;color:#777;margin-top:4px;",
                         "Per-sample growth & lysis metrics (same engine as the lysis-paper ",
                         "companion app): μmax, doubling time, lag phase, stationary ",
                         "duration, lysis onset/rate, OD drop, residual OD, recovery slope, ",
                         "AUC, lysis efficiency, and more. Computed from the MEAN curve — a ",
                         "single number per sample, with no replicate-to-replicate uncertainty."),
                       br(),
                       downloadButton("downloadReplicateMetricsCSV",
                                      "Per-Replicate Metrics, mean ± SD (CSV)",
                                      style = "width:100%;"),
                       p(style = "font-size:.78em;color:#777;margin-top:4px;",
                         "KC-4: the SAME metrics engine run once per individual replicate curve, ",
                         "then reported as mean ± SD across replicates per sample — additive ",
                         "alongside Curve Metrics above, not a replacement for it. These two are ",
                         "not the same number: most of these metrics are nonlinear functions of ",
                         "the curve, so the mean of the per-replicate metrics is generally not ",
                         "equal to the metric of the mean curve. For long-format files with no ",
                         "explicit replicate-ID column, replicate identity is inferred from row ",
                         "order within each (sample, time) group."),
                       numericInput("metrics_smooth_window",
                                    "μmax smoothing window (points):", 5, 3, 15, 1),
                       uiOutput("nonpositive_od_warning_ui"),
                       numericInput("qc_od_ceiling", "QC: flag readings above OD:", APP_DEFAULTS$qc_od_ceiling, 0.1, 10, 0.1),
                       uiOutput("qc_flags_ui_metrics"),
                       numericInput("metrics_peak_smooth_window",
                                    "Peak-detection smoothing window (points, 1 = off):",
                                    1, 1, 15, 2),
                       p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                         "OFF by default. At typical replicate noise, which point counts as ",
                         "“the peak” can shift by a whole time point between runs — ",
                         "and lysis_time, lysis_rate, od_drop, residual_od, lysis_efficiency, ",
                         "recovery_ratio and t_half_max_decline are all anchored on it. Raising ",
                         "this locates the peak on a centered moving average (a simple, ",
                         "documented smoother) instead of the raw curve; every reported OD ",
                         "value still comes from the raw data — smoothing only decides ",
                         "WHERE the peak is."),
                       selectInput("metrics_peak_mode", "Peak used for lysis metrics:",
                                   choices = c("Global maximum (default)" = "global",
                                               "First peak before a > 10 % decline (regrowth / resistance curves)" = "first_local"),
                                   selected = "global"),
                       p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                         "A curve that lyses and then regrows above its first peak has its global ",
                         "maximum at the end, so lysis onset, OD drop and time to half peak come out NA. ",
                         "“First peak” anchors them on the first peak that is followed by a ",
                         "decline of more than 10 % instead (falls back to the global maximum if there is none)."),
                       numericInput("metrics_lysis_threshold_pct",
                                    "Lysis onset: decline from peak that counts as lysis (%):",
                                    5, 1, 50, 1),
                       p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                         "Default 5 % (the paper's definition). A larger value ignores small dips ",
                         "after the peak; it changes lysis onset, lysis rate, OD drop, residual OD, ",
                         "lysis efficiency and recovery slope."),
                       selectInput("metrics_ref_sample",
                                   "Reference sample (infection metrics):",
                                   choices = c("None" = "")),
                       p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                         "Pick your uninfected control to add relative growth, infection ",
                         "strength, relative μmax, relative max OD, and lysis-onset Δ."),
                       hr(),
                       h4("Analysis Report", class = "panel-title"),
                       p(style = "font-size:.82em;color:#555;",
                         "One self-contained file: figure, summary statistics, curve metrics ",
                         "with definitions, data provenance, key settings, and session info."),
                       textInput("report_title",  "Report Title:", "Lysis Curve Analysis Report"),
                       textInput("report_author", "Author / Lab:", ""),
                       textAreaInput("report_notes", "Summary / notes (optional):", "",
                                     rows = 3, resize = "vertical"),
                       numericInput("report_font_size", "Report base font size (pt):", 14, 9, 24),
                       p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                         "Scales all report text and tables (HTML and PDF). ",
                         "Everything in the Experiment Notebook section is included too."),
                       checkboxInput("report_include_analysis",
                                     "Include analysis figures and statistics", TRUE),
                       p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                         "Adds the metric summary, specific growth rate, normalized OD (or ",
                         "replicate overlay without a reference sample) and, when any sample ",
                         "has an MOI, the dose-response figure, plus the replicate statistics ",
                         "tables from the Analysis figures tab. Off: the report is figure, ",
                         "tables and provenance only."),
                       html_report_ui,
                       pdf_report_ui
                   )
                 ),

                 # ── Variable Styling ──────────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Variable Styling"),
                   div(class = "panel-section",
                       h4("Sample Selection", class = "panel-title"),
                       selectInput("selected_samples", "Select Samples:",
                                   choices = character(0), multiple = TRUE),
                       uiOutput("var_settings")
                   )
                 )
                 
    ), # end sidebarPanel
    
    mainPanel(width = 9, id = "main_panel",
      uiOutput("figure_standard_bar"),
      tabsetPanel(id = "main_tabs",
        tabPanel("Lysis curve", br(), uiOutput("plot_container")),
        # ── Analysis figures (v2.11) ──────────────────────────────────────────
        tabPanel("Analysis figures", br(),
          div(class = "analysis-controls",
            fluidRow(
              column(3,
                selectInput("analysis_type", "Figure:",
                            choices = ANALYSIS_FIGURE_CHOICES,
                            selected = "metric_summary"),
                textInput("analysis_panel_label", "Panel label:", APP_DEFAULTS$analysis_panel_label, placeholder = "b")
              ),
              column(3,
                conditionalPanel("input.analysis_type == 'metric_summary'",
                  selectInput("analysis_metrics", "Metrics (one facet each):",
                              choices  = setNames(names(ANALYSIS_METRIC_LABELS), ANALYSIS_METRIC_LABELS),
                              selected = c("lysis_time", "od_drop"), multiple = TRUE),
                  p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                    "Open circles: one per replicate; bar: mean; whiskers: \u00b1 SD. ",
                    "Relative metrics need a reference sample (Stats & Reports).")),
                conditionalPanel("input.analysis_type == 'dose_response'",
                  selectInput("analysis_dose_metric", "Metric vs MOI:",
                              choices  = setNames(names(ANALYSIS_METRIC_LABELS), ANALYSIS_METRIC_LABELS),
                              selected = "lysis_time"),
                  textAreaInput("moi_override", "MOI per sample (if not in the name):", "",
                                rows = 2, placeholder = "Phage A = 10; Phage B = 0.1"),
                  p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                    "MOI is read from names like 'Phage MOI 10'. Samples without a positive MOI ",
                    "(e.g. the uninfected control) are left out; the x axis is log10.")),
                conditionalPanel("input.analysis_type == 'normalized'",
                  radioButtons("analysis_norm_mode", "Relative to:",
                               c("Reference sample" = "reference", "Own t = 0 value" = "t0"),
                               selected = "reference", inline = TRUE),
                  p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                    "Reference mode divides each mean curve by the uninfected control at every ",
                    "time point: the infection-strength metric drawn out over time.")),
                conditionalPanel("input.analysis_type == 'growth_rate'",
                  p(style = "font-size:.78em;color:#777;",
                    "\u03bc(t) = d ln(OD)/dt per replicate by the same rolling regression as ",
                    "\u03bcmax (window under Stats & Reports), then mean \u00b1 SD (band). ",
                    "Lysis shows as a negative rate.")),
                conditionalPanel("input.analysis_type == 'replicate_overlay'",
                  p(style = "font-size:.78em;color:#777;",
                    "Every replicate curve (thin) with the mean (bold): the variance the error ",
                    "bars summarise.")),
                conditionalPanel("input.analysis_type == 'landmarks'",
                  p(style = "font-size:.78em;color:#777;",
                    "Every number in the Curve Metrics CSV, placed where it was measured: lag end, ",
                    "\u03bcmax window centre, peak, lysis onset, half of peak, trough. Use it to check ",
                    "the metrics (and the smoothing windows) before you report them.")),
                conditionalPanel("input.analysis_type == 'heatmap'",
                  p(style = "font-size:.78em;color:#777;",
                    "Mean OD per sample and time point; for screens with many samples."))
              ),
              column(3,
                selectInput("stats_method", "Statistics:",
                            choices = c("Welch t / ANOVA + Tukey"        = "welch",
                                        "Mann-Whitney / Kruskal-Wallis"  = "nonparametric"),
                            selected = "welch"),
                checkboxInput("stats_mark", "Mark significance vs reference on the metric summary", TRUE),
                p(style = "font-size:.78em;color:#777;margin-top:-6px;",
                  "Tested on the per-replicate metric (first selected metric, or the dose-response ",
                  "metric) against the reference sample, Holm-adjusted; all pairs when no reference is set.")
              ),
              column(3,
                p(style = "font-size:.82em;color:#555;",
                  "Same fonts, line weights, legend, colours and export size as the main figure ",
                  "(journal preset included), so panels assemble without rescaling."),
                downloadButton("downloadAnalysisPlot", "Download figure", style = "width:100%;"),
                br(), br(),
                downloadButton("downloadAnalysisStatsCSV", "Download statistics (CSV)", style = "width:100%;")
              )
            )
          ),
          uiOutput("analysis_container"),
          h4("Replicate statistics", style = "margin-top:18px;"),
          uiOutput("analysis_stats_notes"),
          tableOutput("analysis_stats_table"),
          tableOutput("analysis_stats_omnibus"),
          tableOutput("analysis_stats_posthoc")
        ),
        # ── Compose figure (v2.13) ────────────────────────────────────────────
        tabPanel("Compose figure", br(),
          div(class = "analysis-controls",
            fluidRow(
              column(4,
                checkboxGroupInput("compose_panels", "Panels (composed in this order):",
                                   choices = COMPOSE_PANEL_CHOICES,
                                   selected = c("main", "metric_summary", "growth_rate", "normalized"))),
              column(4,
                radioButtons("compose_ncol", "Columns:", choices = c("1" = "1", "2" = "2"),
                             selected = "2", inline = TRUE),
                conditionalPanel("input.panel_size_mode != 'fixed'",
                selectInput("compose_width", "Figure width:",
                            choices = c("Double column of Nature family (183 mm)" = "double",
                                        "Single column of Nature family (89 mm)"  = "single",
                                        "Custom" = "custom"),
                            selected = "double"),
                conditionalPanel("input.compose_width == 'custom'",
                                 numericInput("compose_width_mm", "Width (mm):", 183, 20, 300, 1)),
                numericInput("compose_panel_height_mm", "Height of each panel row (mm):", 60, 20, 250, 1)),
                conditionalPanel("input.panel_size_mode == 'fixed'",
                  p(style = "font-size:.8em;color:#777;", "Fixed panel size: every panel keeps its panel size and the composed figure is as wide and tall as the grid of cells needs."))),
              column(4,
                checkboxInput("compose_tags", "Add panel letters", APP_DEFAULTS$compose_tags),
                numericInput("compose_tag_size", "Panel label size (pt):", 8, 4, 20, 0.5),
                radioButtons("compose_tag_case", "Panel labels:",
                             choices = c("a, b, c" = "a", "A, B, C" = "A"), selected = "a", inline = TRUE),
                downloadButton("downloadComposedFigure", "Download composed figure",
                               style = "background:#2C3E50;color:white;border:none;width:100%;"))),
            uiOutput("compose_note"),
            p(style = "font-size:.8em;color:#777;",
              "Panels are rendered at their native single-column size, so text stays 6\u20138 pt ",
              "(use Apply journal preset first). Tick the figures, pick the columns and the width, ",
              "and download: the complete figure with a, b, c labels, without assembling it in Inkscape. ",
              "Export format and DPI come from the Export settings; a panel whose figure is not ",
              "available is left out.")),
          uiOutput("compose_container")),
        # ── Dashboard (v2.11) ─────────────────────────────────────────────────
        tabPanel("Dashboard", br(),
          uiOutput("dash_cards"),
          uiOutput("dash_qc"),
          h4("Key metrics per sample", style = "margin-top:16px;"),
          p(style = "font-size:.8em;color:#777;margin-top:-4px;",
            "From the mean curve; lysis onset and OD drop also show \u00b1 SD across replicates. ",
            "Full tables, per-replicate metrics and the replicate statistics: see the Statistics tab."),
          tableOutput("dash_metrics_table"),
          uiOutput("dash_size_note"),
          fluidRow(
            column(6, h5("Lysis curve"),           plotOutput("dash_fig_main",    height = "280px")),
            column(6, h5("Specific growth rate"),  plotOutput("dash_fig_rate",    height = "280px"))
          ),
          fluidRow(
            column(6, h5(textOutput("dash_fig_norm_title", inline = TRUE)),
                      plotOutput("dash_fig_norm",    height = "280px")),
            column(6, h5("Metric summary"),        plotOutput("dash_fig_metrics", height = "280px"))
          ),
          h4("Statistics", style = "margin-top:16px;"),
          uiOutput("dash_notes"),
          tableOutput("dash_stats"),
          h4("Experiment", style = "margin-top:16px;"),
          uiOutput("dash_notebook")
        ),
        # ── Statistics (v2.16): the descriptive statistics, reviewable without the sidebar ──
        tabPanel("Statistics", br(),
          p(style = "font-size:.85em;color:#555;",
            "Everything the reports and CSV downloads contain, in one place: review it here, ",
            "download each table, and edit the Methods paragraph that goes into the reports."),
          tags$details(open = NA, class = "stats-section",
            tags$summary(tags$b("Summary statistics")),
            div(class = "panel-section",
              uiOutput("stats_tab_summary_note"),
              if (has_DT) DT::DTOutput("stats_tab_summary") else div(style = "overflow-x:auto;", tableOutput("stats_tab_summary")),
              p(style = "font-size:.78em;color:#777;margin-top:6px;", STATS_N_NOTE),
              downloadButton("downloadStatsCSV_tab", "Summary statistics (CSV)"))),
          tags$details(class = "stats-section",
            tags$summary(tags$b("Curve metrics (mean curve)")),
            div(class = "panel-section",
              div(style = "overflow-x:auto;", tableOutput("stats_tab_metrics")),
              h5("Metric definitions"),
              tableOutput("stats_tab_metric_defs"),
              downloadButton("downloadMetricsCSV_tab", "Curve metrics (CSV)"))),
          tags$details(class = "stats-section",
            tags$summary(tags$b("Per-replicate metrics (mean \u00b1 SD)")),
            div(class = "panel-section",
              div(style = "overflow-x:auto;", tableOutput("stats_tab_repmetrics")),
              downloadButton("downloadReplicateMetricsCSV_tab", "Per-replicate metrics (CSV)"))),
          tags$details(class = "stats-section",
            tags$summary(tags$b("Replicate comparison")),
            div(class = "panel-section",
              p(style = "font-size:.78em;color:#777;",
                "The same tables as the Analysis figures tab: the test method, the reference sample and ",
                "the metric are set there (Statistics, and the figure's metric) and under Stats & Reports."),
              uiOutput("stats_tab_notes"),
              tableOutput("stats_tab_pairwise"),
              tableOutput("stats_tab_omnibus"),
              tableOutput("stats_tab_posthoc"),
              downloadButton("downloadAnalysisStatsCSV_tab", "Replicate statistics (CSV)"))),
          tags$details(class = "stats-section",
            tags$summary(tags$b("Methods text (editable)")),
            div(class = "panel-section",
              p(style = "font-size:.78em;color:#777;",
                "Draft a methods paragraph from the current settings, then edit it freely. A non-empty ",
                "text is included in the HTML and PDF reports as a Methods section, before the statistics."),
              textAreaInput("methods_text", NULL, value = APP_DEFAULTS$methods_text, rows = 6, width = "100%",
                            placeholder = "Click 'Draft from current settings', or write your own."),
              fluidRow(
                column(4, actionButton("methods_draft", "Draft from current settings")),
                column(4, checkboxInput("methods_replace", "replace existing text", APP_DEFAULTS$methods_replace)),
                column(4, downloadButton("downloadMethodsTxt", "Download methods (.txt)")))))
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # v2.14.1: every download in this server fails with a readable message (and a notification)
  # instead of an opaque error; memory errors get a hint. Shadows shiny::downloadHandler locally.
  guarded_content <- function(fn) function(file) {
    tryCatch(fn(file), error = function(e) {
      if (inherits(e, "shiny.silent.error")) stop(e)
      msg <- friendly_error_message(e)
      guard_notify("download_error", paste("Download failed:", msg), "error")
      stop(msg, call. = FALSE)
    })
  }
  downloadHandler <- function(filename, content, contentType = NULL, outputArgs = list())
    shiny::downloadHandler(filename = filename, content = guarded_content(content),
                           contentType = contentType, outputArgs = outputArgs)
  
  rv <- reactiveValues(
    data              = NULL,
    data_source       = NULL,   # file name (or "Demo data") for provenance
    time_col          = NULL,
    od_vars           = NULL,
    od_vars_raw       = NULL,
    col_map           = NULL,   # wide format: sample name -> integer column indices
    group_col         = NULL,
    value_col         = NULL,
    is_long_format    = FALSE,
    palette_colors    = NULL,
    all_timepoints    = NULL,
    # imported_settings is a list with two named sublists:
    #   $global  — named list of scalar values for global inputs
    #   $samples — named list keyed by SAMPLE NAME, each a list of per-sample aesthetics
    imported_settings = NULL,
    settings_status   = NULL,
    style_page        = 1L      # current page in Variable Styling panel (pagination)
  )
  
  # v2.16: y-axis-derived labels (main figure, analysis figures, dose-response, heatmap key) go through
  # this so the wavelength can be typeset as a subscript. Plain text when the box is off (and in the
  # Figure Legend Text, the facet strips and the report captions, which never use it).
  axis_y_label <- function(txt)
    if (isTRUE(input$axis_subscripts)) label_with_subscripts(txt) else txt
  
  # v2.10: warn when the chosen font family is not installed -- every device
  # would silently substitute another face, and the SVG would name a font the
  # Inkscape machine may not have either.
  observeEvent(input$font_family, {
    fam <- input$font_family
    if (is.null(fam) || fam %in% c("sans", "serif", "mono")) return()
    hit <- tryCatch(basename(systemfonts::match_fonts(fam)$path), error = function(e) "")
    if (!grepl(gsub(" ", "", fam), gsub(" ", "", hit), ignore.case = TRUE))
      showNotification(paste0("Font '", fam, "' is not installed on this computer; exports will ",
                              "substitute ", sub("\\.[A-Za-z]+$", "", hit), ". Install it, or choose ",
                              "Arial / Helvetica (accepted by every journal)."),
                       type = "warning", duration = 10)
  }, ignoreInit = TRUE)
  
  observeEvent(input$color_palette, {
    rv$palette_colors <- if (input$color_palette != "custom" && !is.null(rv$od_vars))
      get_palette_colors(input$color_palette, length(rv$od_vars)) else NULL
    # v2.10: these two palettes have 8 distinct colours; beyond that they repeat.
    if (input$color_palette %in% c("colorblind", "publication") &&
        !is.null(rv$od_vars) && length(rv$od_vars) > 8)
      showNotification(paste0("The '", input$color_palette, "' palette has 8 distinct colours but ",
                              length(rv$od_vars), " samples are loaded, so colours will repeat. ",
                              "Use Viridis, or set colours per sample."),
                       type = "warning", duration = 10)
  })
  
  output$palette_preview <- renderUI({
    if (input$color_palette == "custom" || is.null(rv$od_vars)) return(NULL)
    cols <- get_palette_colors(input$color_palette, min(8, length(rv$od_vars)))
    div(p("Preview:"),
        div(style = "margin-top:6px;",
            lapply(cols, function(col)
              div(style = paste0("display:inline-block;width:28px;height:20px;",
                                 "background:", col, ";margin-right:4px;",
                                 "border:1px solid #ccc;")))))
  })
  
  # ── Time filter UI ───────────────────────────────────────────────────────────
  output$time_range_slider_ui <- renderUI({
    tp <- rv$all_timepoints
    if (is.null(tp) || length(tp) < 2)
      return(p(style = "color:#999;font-size:.85em;", "Load a data file to enable the range slider."))
    t_min  <- min(tp); t_max <- max(tp)
    t_step <- max(min(diff(tp)), 0.001)
    sliderInput("time_filter_range", label = NULL,
                min = t_min, max = t_max, value = c(t_min, t_max),
                step = t_step, width = "100%")
  })
  
  output$excluded_timepoints_preview <- renderUI({
    if (is.null(rv$all_timepoints)) return(NULL)
    excl <- parse_excluded_timepoints(input$exclude_timepoints, rv$all_timepoints)
    if (length(excl) == 0) return(NULL)
    div(class = "excl-preview",
        paste0("Will exclude ", length(excl), " point(s): ", paste(sort(excl), collapse = ", ")))
  })
  
  # ── Data loading ─────────────────────────────────────────────────────────────
  is_long_format_detect <- function(data) {
    if (nrow(data) == 0) return(FALSE)
    potential <- c("variable","Variable","condition","Condition","treatment","Treatment",
                   "sample","Sample","group","Group")
    if (any(potential %in% colnames(data))) return(TRUE)
    if (ncol(data) <= 4) {
      # Only NON-NUMERIC columns can be grouping candidates: a numeric OD
      # column with repeated values (e.g. a flat/constant control) must not
      # flip a small wide file into long format.
      nn <- !vapply(data, is.numeric, logical(1))
      if (!any(nn)) return(FALSE)
      vd <- sapply(data[nn], function(x) length(unique(x)) / nrow(data))
      return(any(vd < 0.3 & vd > 0))
    }
    FALSE
  }
  
  detect_time_column <- function(data) {
    hits <- grep("^(time|Time|TIME|t|T)$", colnames(data), value = TRUE)
    if (length(hits)) return(hits[1])
    hits2 <- grep("time|Time", colnames(data), value = TRUE)
    if (length(hits2)) return(hits2[1])
    for (nm in colnames(data)) {
      v <- suppressWarnings(as.numeric(data[[nm]]))
      if (!any(is.na(v)) && length(v) > 1 && all(diff(v) >= 0)) return(nm)
    }
    colnames(data)[1]
  }
  
  detect_od_columns <- function(data, exclude_cols) {
    cols <- setdiff(colnames(data), exclude_cols)
    hits <- grep("od|OD|absorbance|Absorbance|value|Value", cols,
                 ignore.case = TRUE, value = TRUE)
    if (length(hits)) return(hits)
    cols[sapply(data[cols], is.numeric)]
  }

  # Wide format: detect OD columns BY POSITION so replicate columns that share
  # a name (the documented replicate workflow) are all kept. Returns integer
  # indices into colnames(data).
  detect_od_column_idx <- function(data, time_col) {
    cn       <- colnames(data)
    time_idx <- which(cn == time_col)
    cand     <- setdiff(seq_along(cn), time_idx)
    hits     <- cand[grepl("od|absorbance|value", cn[cand], ignore.case = TRUE)]
    if (length(hits)) return(hits)
    cand[vapply(cand, function(i) is.numeric(data[[i]]), logical(1))]
  }

  # Shared ingestion for both file upload and demo data
  ingest_data <- function(data, source_name) {
    rv$time_col       <- detect_time_column(data)
    rv$col_map        <- NULL
    rv$replicate_col  <- NULL    # long format: replicate / well column, when the file has one
    rv$n_blocks       <- NULL    # wide format: stacked time blocks (replicates), when > 1
    rv$block_id       <- NULL
    notes <- character(0)

    # Elapsed time as 0:15:00 (Gen5 / i-control) or a full date-time: convert to minutes
    # so the numeric time handling downstream (filters, axis range) just works.
    pt <- parse_time_column(data[[rv$time_col]])
    if (isTRUE(pt$converted)) {
      data[[rv$time_col]] <- pt$values
      notes <- c(notes, paste0("Time column '", rv$time_col, "' was written as clock time / date-time; converted to minutes."))
    }

    # A well that went out of range (OVER / OVRFLW / #SAT) makes read.csv type the whole
    # column as text: keep the column, turn just those cells into NA. The time column and
    # any group / replicate column are left alone.
    group_names <- c("variable","Variable","condition","Condition","treatment","Treatment",
                     "sample","Sample","group","Group")
    rep_names   <- colnames(data)[tolower(colnames(data)) %in%
                     c("replicate","rep","rep_id","replicate_id","well","well_id","wellid",
                       "technical_replicate","biological_replicate","repeat")]
    cs <- coerce_sentinel_columns(data, skip_cols = c(rv$time_col, group_names, rep_names),
                                  time_values = data[[rv$time_col]])
    data <- cs$data
    if (!is.null(cs$report))
      notes <- c(notes, paste0(nrow(cs$report), " non-numeric reading(s) (",
                               paste(utils::head(unique(cs$report$value), 3), collapse = ", "),
                               ") were treated as missing; affected column(s): ",
                               paste(unique(cs$report$sample), collapse = ", "), "."))
    rv$sentinel_report <- cs$report

    rv$is_long_format <- is_long_format_detect(data)

    if (rv$is_long_format) {
      potential <- group_names
      found     <- potential[potential %in% colnames(data)]
      rep_col0  <- detect_replicate_column(data, rv$time_col, NULL, NULL)
      group_col <- if (length(found) > 0) found[1] else {
        dv    <- sapply(data, function(x) length(unique(x)) / nrow(data))
        cands <- setdiff(names(which(dv < 0.3 & dv > 0)), c(rv$time_col, rep_col0))
        if (length(cands)) cands[1] else setdiff(colnames(data), c(rv$time_col, rep_col0))[1]
      }
      # the replicate column must never be taken for the value column (a numeric "rep"
      # column would win the is.numeric() fallback)
      rep_col <- detect_replicate_column(data, rv$time_col, group_col, NULL)
      vc <- detect_od_columns(data, c(rv$time_col, group_col, rep_col))
      if (length(vc) > 1) vc <- vc[1]
      rep_col <- detect_replicate_column(data, rv$time_col, group_col, vc)
      rv$group_col     <- group_col
      rv$value_col     <- vc
      rv$replicate_col <- rep_col
      rv$od_vars       <- as.character(unique(data[[group_col]]))
      rv$od_vars_raw   <- rv$od_vars
    } else {
      # Index-based detection keeps ALL replicate columns, including
      # duplicates of the same name (v2.8 REPLICATE FIX).
      idx <- detect_od_column_idx(data, rv$time_col)
      cn  <- colnames(data)
      oc  <- unique(cn[idx])
      rv$col_map     <- lapply(setNames(oc, oc), function(nm) idx[cn[idx] == nm])
      rv$od_vars     <- oc
      rv$od_vars_raw <- oc
      # Stacked replicate blocks (the time counter restarts for each block): every block is
      # one more replicate of every sample at each time point. prepare_plot_data() already
      # pools rows that share a time stamp; the block id lets prepare_replicate_data() keep
      # the blocks apart as separate replicate curves.
      wb <- infer_wide_replicates(data, rv$time_col)
      if (!is.null(wb$reps) && wb$reps > 1) {
        rv$n_blocks <- wb$reps
        rv$block_id <- wb$block_id
        notes <- c(notes, paste0("Detected ", wb$reps, " stacked time blocks (the time column restarts); ",
                                 "each block is treated as one replicate."))
      }
    }
    rv$data        <- data
    rv$data_source <- source_name
    # Tell the user what was repaired on load (best effort: no notification channel in tests)
    for (nt in notes)
      tryCatch(showNotification(nt, type = "message", duration = 12), error = function(e) NULL)

    tr <- range(suppressWarnings(as.numeric(as.character(data[[rv$time_col]]))), na.rm = TRUE)
    updateNumericInput(session, "x_min", value = tr[1])
    updateNumericInput(session, "x_max", value = tr[2])
    updateSelectInput(session, "selected_samples", choices = rv$od_vars, selected = rv$od_vars)
    updateSelectInput(session, "metrics_ref_sample",
                      choices = c("None" = "", setNames(rv$od_vars, rv$od_vars)),
                      selected = "")

    all_t <- sort(unique(suppressWarnings(as.numeric(as.character(data[[rv$time_col]])))))
    rv$all_timepoints <- all_t[is.finite(all_t)]
    updateTextInput(session, "exclude_timepoints", value = "")

    if (!is.null(input$color_palette) && input$color_palette != "custom")
      rv$palette_colors <- get_palette_colors(input$color_palette, length(rv$od_vars))

    # After loading data, auto-apply any imported per-sample aesthetics for matching names
    apply_imported_sample_aesthetics()
  }

  observeEvent(input$file, {
    req(input$file)
    data <- read_csv_locale_aware(input$file$datapath, input$csv_locale %||% "auto")
    ingest_data(data, input$file$name)
  })

  observeEvent(input$load_demo, {
    sc <- input$demo_scenario %||% "lysis"
    if (!sc %in% DEMO_SCENARIOS) sc <- "lysis"
    d  <- make_demo_data(sc)
    ingest_data(d, if (sc == "lysis") "Demo data (built-in)" else paste0("Demo data (built-in: ", sc, ")"))
    updateTextInput(session, "x_axis_label",
                    value = if (sc == "hours") "Time (hours)" else "Time (minutes)")
    # reference sample = the uninfected / control sample (sent after ingest's own
    # choices update, so it wins)
    smp <- setdiff(colnames(d), "Time")
    ref <- unique(smp[grepl("^(uninfected|control)", smp, ignore.case = TRUE)])
    updateSelectInput(session, "metrics_ref_sample",
                      choices = c("None" = "", setNames(unique(smp), unique(smp))),
                      selected = if (length(ref)) ref[1] else "")
  })

  # ── Data summary (what the app detected) ─────────────────────────────────────
  # v2.14.1: how big the loaded data is (time points, samples, replicates per sample)
  data_size_info <- function() {
    ntp <- length(rv$all_timepoints); ns <- length(rv$od_vars)
    nrep <- if (isTRUE(rv$is_long_format)) max(1, round(nrow(rv$data) / max(1, ntp * ns)))
            else max(1, lengths(rv$col_map)) * max(1, rv$n_blocks %||% 1)
    list(ntp = ntp, ns = ns, nrep = nrep, cells = as.numeric(ntp) * ns * nrep)
  }
  DATA_WARN_TIMEPOINTS <- 5000; DATA_WARN_SAMPLES <- 200; DASHBOARD_MAX_CELLS <- 2e6
  dashboard_too_big <- function() {
    if (is.null(rv$data)) return(FALSE)
    isTRUE(data_size_info()$cells > DASHBOARD_MAX_CELLS)
  }
  output$dash_size_note <- renderUI({
    if (!dashboard_too_big()) return(NULL)
    i <- data_size_info()
    div(class = "settings-status warn", style = "margin:8px 0;",
        sprintf("The dashboard figures are skipped for data this large (%s time points x %s samples x %s replicates = %s values, limit %s). Use the Analysis figures tab or reduce the selection / time window.",
                i$ntp, i$ns, i$nrep, format(i$cells, big.mark = ",", scientific = FALSE),
                format(DASHBOARD_MAX_CELLS, big.mark = ",", scientific = FALSE)))
  })
  output$data_summary_ui <- renderUI({
    req(rv$data)
    rep_txt <- if (rv$is_long_format) {
      cnts <- table(rv$data[[rv$group_col]])
      ntp  <- length(rv$all_timepoints)
      per  <- if (ntp > 0) round(as.numeric(cnts) / ntp, 1) else NA
      paste0(names(cnts), " (~", per, " rep/time)", collapse = ", ")
    } else {
      paste0(names(rv$col_map), " (", lengths(rv$col_map),
             ifelse(lengths(rv$col_map) > 1, " replicate cols)", " col)"),
             collapse = ", ")
    }
    di <- data_size_info()
    big <- if (di$ntp > DATA_WARN_TIMEPOINTS || di$ns > DATA_WARN_SAMPLES)
      div(class = "settings-status warn", style = "margin-top:6px;",
          sprintf("Large data set (%s time points, %s samples): analysis figures and statistics may be slow.", di$ntp, di$ns))
    div(style = "margin-bottom:10px;",
        div(class = "settings-status ok",
            HTML(paste0(
              "<b>", html_escape(rv$data_source %||% "data"), "</b><br/>",
              "Format: <b>", if (rv$is_long_format) "long" else "wide", "</b> &middot; ",
              "Time column: <b>", html_escape(rv$time_col), "</b> &middot; ",
              length(rv$all_timepoints), " time points<br/>",
              "Samples: ", html_escape(rep_txt)))),
        big)
  })
  
  # ── Settings: Save ───────────────────────────────────────────────────────────
  # Saves global visual settings AND per-sample aesthetics keyed by sample name.
  # Explicitly excluded: file path, loaded CSV, sample selection, time filter range —
  # anything that belongs to the data rather than the visualization.
  # v2.17: the settings document as a list, shared by the Save Settings download and the figure bundle
  settings_json_list <- function() {
      all_inputs <- reactiveValuesToList(input)
      
      # Inputs that belong to the data/session — never saved
      data_inputs     <- c("file", "loadSettings", "selected_samples",
                           "time_filter_range", "exclude_timepoints",
                           "apply_journal_preset", "apply_journal_preset_bar", "apply_legacy_look",
                           "main_panel_width")
      # Per-sample dynamic input prefixes — handled separately below
      sample_prefixes <- c("line_type_","shape_","shape_filled_","color_selector_",
                           "color_","use_rgb_","red_","green_","blue_","alpha_",
                           "use_hex_","hex_color_","legend_label_",
                           "apply_rgb_","apply_hex_")
      # v2.8 SETTINGS SAVE FIX: these GLOBAL inputs collide with per-sample
      # prefixes ("shape_", "color_") and were silently dropped from saved files
      global_exceptions <- c("shape_size", "color_palette")
      is_sample_inp <- function(nm) !(nm %in% global_exceptions) &&
        any(vapply(sample_prefixes, startsWith, logical(1), x = nm))
      
      # Global visual settings: everything that isn't data-specific or per-sample
      global_settings <- all_inputs[vapply(names(all_inputs), function(nm)
        !(nm %in% data_inputs) && !is_sample_inp(nm), logical(1))]
      
      # Per-sample aesthetics keyed by sample name
      sample_aesthetics <- list()
      samps <- isolate(input$selected_samples)
      if (!is.null(samps)) {
        for (vn in samps) {
          vid     <- safe_id(vn)
          leg     <- input[[paste0("legend_label_", vid)]]
          col_sel <- input[[paste0("color_selector_", vid)]]
          color   <- if (!is.null(col_sel) && col_sel == "custom")
            normalize_hex_color(input[[paste0("color_", vid)]])
          else if (!is.null(col_sel)) col_sel
          else "#000000"
          sample_aesthetics[[vn]] <- list(
            color        = color,
            shape        = input[[paste0("shape_",        vid)]],
            shape_filled = input[[paste0("shape_filled_", vid)]],
            line_type    = input[[paste0("line_type_",    vid)]],
            legend_label = if (!is.null(leg) && nchar(leg) > 0) leg else vn
          )
        }
      }
      
      list(
        version           = APP_VERSION,
        saved_at          = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        global_settings   = global_settings,
        sample_aesthetics = sample_aesthetics
      )
  }
  output$saveSettings <- downloadHandler(
    filename = function()
      paste0("plot_settings_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"),
    content = function(file) write_json(settings_json_list(), file, pretty = TRUE, auto_unbox = TRUE)
  )
  
  # ── Settings: Load ───────────────────────────────────────────────────────────
  # Applies global visual settings AND per-sample aesthetics from the file.
  # Never touches: file input, sample selection, time filter range.
  observeEvent(input$loadSettings, {
    req(input$loadSettings)
    s <- tryCatch(
      fromJSON(input$loadSettings$datapath, simplifyVector = FALSE),
      error = function(e) {
        rv$settings_status <- list(type = "warn",
                                   msg = paste("Could not parse settings file:", e$message), details = NULL)
        NULL
      }
    )
    if (is.null(s)) return()
    
    # Support both v2.5 structured format and legacy flat format
    global     <- if (!is.null(s$global_settings))   s$global_settings   else list()
    sample_aes <- if (!is.null(s$sample_aesthetics)) s$sample_aesthetics
    else if (!is.null(s$samples))       s$samples
    else list()
    
    rv$imported_settings <- list(global = global, samples = sample_aes)
    
    # ── Apply global visual settings ──────────────────────────────────────────
    # Skip anything data-specific or per-sample — only visual controls
    data_inputs     <- c("file","loadSettings","selected_samples",
                         "time_filter_range","exclude_timepoints",
                         "apply_journal_preset", "apply_journal_preset_bar", "apply_legacy_look",
                         "main_panel_width")
    sample_prefixes <- c("line_type_","shape_","shape_filled_","color_selector_",
                         "color_","use_rgb_","red_","green_","blue_","alpha_",
                         "use_hex_","hex_color_","legend_label_","apply_rgb_","apply_hex_")
    global_exceptions <- c("shape_size", "color_palette")   # see save handler note
    is_sample_inp <- function(nm) !(nm %in% global_exceptions) &&
      any(vapply(sample_prefixes, startsWith, logical(1), x = nm))
    
    # v2.10: a file saved in mm must not be re-converted by the units observer;
    # a pre-2.10 file has no export_units and its sizes are inches, so the
    # radio is switched back to inches before the sizes arrive.
    if (is.character(global$export_units)) {
      rv$units_seen <- global$export_units
    } else {
      rv$units_seen <- "in"
      updateRadioButtons(session, "export_units", selected = "in")
    }
    n_global <- 0L
    for (nm in names(global)) {
      if (nm %in% data_inputs || is_sample_inp(nm)) next
      if (!(nm %in% names(input)))                  next
      tryCatch({
        val <- global[[nm]]
        if (is.character(val) && (length(val) > 1 || nm == "compose_panels")) {
          updateCheckboxGroupInput(session, nm, selected = val)     # v2.13: checkbox groups
        } else if (is.logical(val) && length(val) == 1) {
          updateCheckboxInput(session, nm, value = val)
        } else if (is.numeric(val) && length(val) == 1) {
          updateNumericInput(session, nm, value = val)
        } else if (is.character(val) && length(val) == 1) {
          updateSelectInput(session, nm, selected = val)
          updateTextInput(session,   nm, value    = val)
          if (startsWith(nm, "compose_")) updateRadioButtons(session, nm, selected = val)
        }
        n_global <- n_global + 1L
      }, error = function(e) invisible(NULL))
    }
    
    # ── Apply per-sample aesthetics for any matching names ────────────────────
    apply_imported_sample_aesthetics()
    
    cur_samps   <- isolate(input$selected_samples)
    saved_names <- names(sample_aes)
    matched     <- intersect(cur_samps,  saved_names)
    unmatched   <- setdiff(saved_names,  cur_samps)
    
    sample_msg <- if (length(saved_names) == 0) "No sample styles in file."
    else paste0(length(matched), "/", length(saved_names), " sample style(s) applied.")
    
    details_html <- if (length(saved_names) > 0) {
      match_lines   <- if (length(matched)   > 0)
        paste0("<li style='color:#155724'>&#10003; ", matched, "</li>", collapse = "")
      else ""
      unmatch_lines <- if (length(unmatched) > 0)
        paste0("<li style='color:#856404'>&#9675; ", unmatched,
               " <em>(not in current data)</em></li>", collapse = "")
      else ""
      paste0("<ul class='match-list'>", match_lines, unmatch_lines, "</ul>")
    } else NULL
    
    rv$settings_status <- list(
      type    = "ok",
      msg     = paste0(n_global, " global setting(s) applied. ", sample_msg),
      details = details_html
    )
  })
  
  # Applies imported per-sample aesthetics to any matching currently-loaded samples.
  # Called both after loadSettings and after a new CSV is loaded.
  apply_imported_sample_aesthetics <- function() {
    imp <- rv$imported_settings
    if (is.null(imp) || is.null(imp$samples) || length(imp$samples) == 0) return()
    cur_samps <- isolate(input$selected_samples)
    if (is.null(cur_samps) || length(cur_samps) == 0) return()
    for (vn in cur_samps) {
      if (!vn %in% names(imp$samples)) next
      aes   <- imp$samples[[vn]]
      vid   <- safe_id(vn)
      if (!is.null(aes$color)) {
        updateSelectInput(session, paste0("color_selector_", vid), selected = "custom")
        updateTextInput(session,   paste0("color_",          vid), value   = aes$color)
        updateTextInput(session,   paste0("hex_color_",      vid), value   = aes$color)
      }
      if (!is.null(aes$shape))
        updateSelectInput(session, paste0("shape_", vid), selected = as.character(aes$shape))
      if (!is.null(aes$shape_filled))
        updateCheckboxInput(session, paste0("shape_filled_", vid), value = aes$shape_filled)
      if (!is.null(aes$line_type))
        updateSelectInput(session, paste0("line_type_", vid), selected = aes$line_type)
      if (!is.null(aes$legend_label))
        updateTextInput(session, paste0("legend_label_", vid), value = aes$legend_label)
    }
  }
  
  # ── Settings: Clear ──────────────────────────────────────────────────────────
  # Revokes all imported settings: resets global visual settings to app defaults
  # AND restores per-sample aesthetics to position-based defaults.
  # Never touches: data, sample selection, time filter.
  observeEvent(input$clearSettings, {
    if (is.null(rv$imported_settings)) {
      rv$settings_status <- list(type = "warn", msg = "No imported settings to clear.", details = NULL)
      return()
    }
    
    # ── Reset global visual settings to app defaults ──────────────────────────
    apply_settings(APP_DEFAULTS)   # v2.14: the journal standard (also moves rv$units_seen)
    
    # ── Reset per-sample aesthetics to position-based defaults ───────────────
    cur_samps  <- isolate(input$selected_samples)
    def_colors <- c("#000000","#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00",
                    "#A65628","#F781BF","#999999","#66C2A5","#FC8D62","#8DA0CB")
    shape_opts <- c(16, 15, 17, 18, 25, 4, 8, 23, 3, 10)
    if (!is.null(cur_samps)) {
      for (i in seq_along(cur_samps)) {
        vn        <- cur_samps[i]; vid <- safe_id(vn)
        def_col   <- def_colors[(i - 1) %% length(def_colors) + 1]
        def_shape <- as.character(shape_opts[(i - 1) %% length(shape_opts) + 1])
        tryCatch({
          updateSelectInput(session,   paste0("color_selector_", vid), selected = "custom")
          updateTextInput(session,     paste0("color_",          vid), value    = def_col)
          updateTextInput(session,     paste0("hex_color_",      vid), value    = def_col)
          updateSelectInput(session,   paste0("shape_",          vid), selected = def_shape)
          updateCheckboxInput(session, paste0("shape_filled_",   vid), value    = TRUE)
          updateSelectInput(session,   paste0("line_type_",      vid), selected = "solid")
          updateTextInput(session,     paste0("legend_label_",   vid), value    = vn)
        }, error = function(e) invisible(NULL))
      }
    }
    
    rv$imported_settings <- NULL
    rv$settings_status   <- list(
      type    = "ok",
      msg     = "All imported settings cleared. Global and per-sample settings restored to defaults.",
      details = NULL
    )
  })
  
  output$settings_status_ui <- renderUI({
    if (is.null(rv$settings_status)) return(NULL)
    cls <- if (rv$settings_status$type == "ok") "settings-status ok" else "settings-status warn"
    tagList(
      div(class = cls, rv$settings_status$msg),
      if (!is.null(rv$settings_status$details))
        HTML(paste0('<div style="font-size:.82em;margin-top:4px;">',
                    rv$settings_status$details, '</div>'))
    )
  })
  
  # Null-coalescing operator (must be defined before first use in renderUI below)
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # ── Dynamic UI: var_settings (paginated for large sample counts) ─────────────
  # When there are more than PAGE_SIZE samples, render only one page at a time.
  # This keeps the DOM small and prevents Shiny from serialising hundreds of inputs.
  PAGE_SIZE <- 20L
  
  # Reset to page 1 whenever the sample selection changes
  observeEvent(input$selected_samples, { rv$style_page <- 1L }, ignoreNULL = FALSE)
  observeEvent(input$style_prev_page,  { rv$style_page <- max(1L, rv$style_page - 1L) })
  observeEvent(input$style_next_page,  {
    n_pages <- ceiling(length(input$selected_samples) / PAGE_SIZE)
    rv$style_page <- min(n_pages, rv$style_page + 1L)
  })
  
  output$var_settings <- renderUI({
    req(rv$od_vars, input$selected_samples)
    all_samps <- input$selected_samples
    n_total   <- length(all_samps)
    n_pages   <- ceiling(n_total / PAGE_SIZE)
    page      <- rv$style_page
    idx_start <- (page - 1L) * PAGE_SIZE + 1L
    idx_end   <- min(page * PAGE_SIZE, n_total)
    page_samps <- all_samps[idx_start:idx_end]
    
    shape_opts <- c("Circle" = 16, "Square" = 15, "Triangle Up" = 17,
                    "Diamond" = 18, "Triangle Down" = 25, "Cross" = 4,
                    "X" = 8, "Open Circle" = 21, "Open Square" = 22,
                    "Open Triangle" = 24, "Plus" = 3, "Star" = 10)
    default_colors <- c("#000000","#E41A1C","#377EB8","#4DAF4A",
                        "#984EA3","#FF7F00","#A65628","#F781BF",
                        "#999999","#66C2A5","#FC8D62","#8DA0CB")
    named_colors   <- c("#000000","#E41A1C","#377EB8","#4DAF4A",
                        "#984EA3","#FF7F00","#FFFF33","#A65628",
                        "#F781BF","#999999","#009E73")
    
    # Pagination nav bar (only shown when needed)
    nav_bar <- if (n_pages > 1L) {
      div(style = "display:flex;align-items:center;justify-content:space-between;margin-bottom:10px;",
          actionButton("style_prev_page", "\u25C4 Prev",
                       style = "font-size:.8em;padding:3px 8px;",
                       disabled = if (page <= 1L) "disabled" else NULL),
          span(style = "font-size:.85em;color:#555;",
               paste0("Page ", page, " of ", n_pages,
                      " (samples ", idx_start, "\u2013", idx_end, " of ", n_total, ")")),
          actionButton("style_next_page", "Next \u25BA",
                       style = "font-size:.8em;padding:3px 8px;",
                       disabled = if (page >= n_pages) "disabled" else NULL)
      )
    } else NULL
    
    sample_widgets <- lapply(seq_along(page_samps), function(j) {
      i   <- idx_start + j - 1L   # global index across all samples
      vn  <- page_samps[j]
      vid <- safe_id(vn)
      shape_def <- as.character(shape_opts[1 + (i - 1L) %% length(shape_opts)])
      
      imp_aes <- rv$imported_settings$samples[[vn]]
      
      if (!is.null(imp_aes)) {
        color_def <- imp_aes$color %||% default_colors[1]
        sel_def   <- "custom"
      } else if (!is.null(rv$palette_colors) && input$color_palette != "custom") {
        idx       <- match(vn, rv$od_vars)
        color_def <- if (!is.na(idx)) rv$palette_colors[idx] else default_colors[1]
        sel_def   <- "custom"
      } else {
        color_def <- default_colors[1 + (i - 1L) %% length(default_colors)]
        sel_def   <- if (color_def %in% named_colors) color_def else "custom"
      }
      if (is.na(color_def) || is.null(color_def)) color_def <- "#000000"
      
      default_shape  <- if (!is.null(imp_aes$shape))       as.character(imp_aes$shape)    else shape_def
      default_lt     <- if (!is.null(imp_aes$line_type))    imp_aes$line_type              else "solid"
      default_filled <- if (!is.null(imp_aes$shape_filled)) imp_aes$shape_filled           else TRUE
      default_leg    <- if (!is.null(imp_aes$legend_label)) imp_aes$legend_label           else vn
      
      div(
        style = "margin-bottom:12px;padding:10px;border:1px solid #ddd;border-radius:6px;background:#fafafa;",
        h4(vn, style = "margin:0 0 8px 0;font-size:1em;color:#333;"),
        selectInput(paste0("line_type_", vid), "Line Type:",
                    choices  = c("Solid" = "solid", "Dashed" = "dashed",
                                 "Dotted" = "dotted", "DotDash" = "dotdash",
                                 "LongDash" = "longdash", "TwoDash" = "twodash"),
                    selected = default_lt),
        h5("Shape", style = "margin:8px 0 4px;"),
        fluidRow(
          column(6, selectInput(paste0("shape_", vid), "Shape:",
                                choices = shape_opts, selected = default_shape)),
          column(6, checkboxInput(paste0("shape_filled_", vid), "Filled", default_filled))
        ),
        div(class = "panel-section",
            h5("Color", class = "panel-title"),
            selectInput(paste0("color_selector_", vid), "Pick Color:",
                        choices  = c("Black" = "#000000", "Red"    = "#E41A1C",
                                     "Blue"  = "#377EB8", "Green"  = "#4DAF4A",
                                     "Purple"= "#984EA3", "Orange" = "#FF7F00",
                                     "Yellow"= "#FFFF33", "Brown"  = "#A65628",
                                     "Pink"  = "#F781BF", "Gray"   = "#999999",
                                     "Teal"  = "#009E73", "Custom" = "custom"),
                        selected = sel_def),
            conditionalPanel(
              condition = paste0("input['color_selector_", vid, "'] == 'custom'"),
              div(style = "display:flex;align-items:center;gap:8px;",
                  textInput(paste0("color_", vid), "HEX:", value = color_def),
                  div(id    = paste0("color_preview_", vid),
                      class = "color-preview",
                      style = paste0("background-color:", color_def, ";"))
              )
            ),
            checkboxInput(paste0("use_rgb_", vid), "RGB Sliders", FALSE),
            conditionalPanel(
              condition = paste0("input['use_rgb_", vid, "'] == true"),
              sliderInput(paste0("red_",   vid), "R:", 0, 255, 0, 1),
              sliderInput(paste0("green_", vid), "G:", 0, 255, 0, 1),
              sliderInput(paste0("blue_",  vid), "B:", 0, 255, 0, 1),
              sliderInput(paste0("alpha_", vid), "A:", 0,   1, 1, 0.01),
              actionButton(paste0("apply_rgb_", vid), "Apply RGB",
                           style = "margin-top:4px;font-size:.85em;")
            ),
            checkboxInput(paste0("use_hex_", vid), "Direct HEX Entry", FALSE),
            conditionalPanel(
              condition = paste0("input['use_hex_", vid, "'] == true"),
              div(style = "display:flex;align-items:center;gap:8px;",
                  textInput(paste0("hex_color_", vid), "HEX Code:", value = color_def),
                  actionButton(paste0("apply_hex_", vid), "Apply",
                               style = "font-size:.85em;")
              )
            )
        ),
        textInput(paste0("legend_label_", vid), "Legend Label:", value = default_leg)
      )
    })
    
    tagList(nav_bar, do.call(tagList, sample_widgets))
  })
  
  # ── Color reactivity ─────────────────────────────────────────────────────────
  # Observer registry: track which sample IDs have already had observers registered
  # so we never register the same observeEvent twice (prevents accumulation / leak).
  registered_color_obs <- character(0)
  
  observe({
    req(input$selected_samples, rv$data)
    new_samps <- setdiff(safe_id(input$selected_samples), registered_color_obs)
    if (length(new_samps) == 0) return()
    
    for (vn in input$selected_samples[safe_id(input$selected_samples) %in% new_samps]) {
      local({
        vi <- safe_id(vn)
        registered_color_obs <<- c(registered_color_obs, vi)
        
        observeEvent(input[[paste0("color_selector_", vi)]], {
          sel <- input[[paste0("color_selector_", vi)]]
          if (!is.null(sel) && sel != "custom") {
            rc <- tryCatch(col2rgb(sel), error = function(e) NULL)
            if (!is.null(rc)) {
              updateSliderInput(session, paste0("red_",   vi), value = rc[1, 1])
              updateSliderInput(session, paste0("green_", vi), value = rc[2, 1])
              updateSliderInput(session, paste0("blue_",  vi), value = rc[3, 1])
              updateTextInput(session,   paste0("color_",     vi), value = sel)
              updateTextInput(session,   paste0("hex_color_", vi), value = sel)
            }
          }
        }, ignoreInit = TRUE)
        
        observeEvent(input[[paste0("color_", vi)]], {
          col <- normalize_hex_color(input[[paste0("color_", vi)]])
          session$sendCustomMessage("updateColorPreview",
                                    list(id = paste0("color_preview_", vi), color = col))
          rc <- tryCatch(col2rgb(col), error = function(e) NULL)
          if (!is.null(rc)) {
            updateSliderInput(session, paste0("red_",   vi), value = rc[1, 1])
            updateSliderInput(session, paste0("green_", vi), value = rc[2, 1])
            updateSliderInput(session, paste0("blue_",  vi), value = rc[3, 1])
          }
        }, ignoreInit = TRUE)
        
        observeEvent(input[[paste0("apply_rgb_", vi)]], {
          hex <- rgb_to_hex(input[[paste0("red_",   vi)]],
                            input[[paste0("green_", vi)]],
                            input[[paste0("blue_",  vi)]],
                            input[[paste0("alpha_", vi)]])
          updateSelectInput(session, paste0("color_selector_", vi), selected = "custom")
          updateTextInput(session,   paste0("color_",     vi), value = hex)
          updateTextInput(session,   paste0("hex_color_", vi), value = hex)
        })
        
        observeEvent(input[[paste0("apply_hex_", vi)]], {
          hex <- normalize_hex_color(input[[paste0("hex_color_", vi)]])
          updateSelectInput(session, paste0("color_selector_", vi), selected = "custom")
          updateTextInput(session, paste0("color_", vi), value = hex)
          rc <- tryCatch(col2rgb(hex), error = function(e) NULL)
          if (!is.null(rc)) {
            updateSliderInput(session, paste0("red_",   vi), value = rc[1, 1])
            updateSliderInput(session, paste0("green_", vi), value = rc[2, 1])
            updateSliderInput(session, paste0("blue_",  vi), value = rc[3, 1])
          }
        })
      })
    }
  })
  
  # ── Dynamic UI: region / marker settings ────────────────────────────────────
  output$region_settings <- renderUI({
    req(input$region_count, input$enable_highlighting)
    do.call(tagList, lapply(seq_len(input$region_count), function(i) {
      div(style = "border:1px solid #ddd;padding:10px;margin-bottom:8px;border-radius:5px;",
          h5(paste("Region", i)),
          fluidRow(
            column(6, numericInput(paste0("region_x_min_", i), "X min:", 0)),
            column(6, numericInput(paste0("region_x_max_", i), "X max:", 60))
          ),
          fluidRow(
            column(6, numericInput(paste0("region_y_min_", i), "Y min:", 0.01)),
            column(6, numericInput(paste0("region_y_max_", i), "Y max:", 1))
          ),
          selectInput(paste0("region_color_", i), "Fill Color:",
                      choices  = c("Light Gray"   = "#ebebeb", "Light Blue"  = "#e6f3ff",
                                   "Light Red"    = "#ffebeb", "Light Green" = "#ebffeb",
                                   "Light Yellow" = "#ffffeb", "Custom"      = "custom"),
                      selected = "#ebebeb"),
          conditionalPanel(
            condition = paste0("input['region_color_", i, "'] == 'custom'"),
            textInput(paste0("region_color_custom_", i), "HEX:", "#ebebeb")
          ),
          sliderInput(paste0("region_alpha_", i), "Transparency:", 0, 1, 0.3, 0.05)
      )
    }))
  })
  
  output$time_marker_settings <- renderUI({
    req(input$marker_count, input$enable_time_markers)
    do.call(tagList, lapply(seq_len(input$marker_count), function(i) {
      div(style = "border:1px solid #ddd;padding:10px;margin-bottom:8px;border-radius:5px;",
          h5(paste("Marker", i)),
          numericInput(paste0("marker_time_", i), "Time Point:", 30),
          selectInput(paste0("marker_line_type_", i), "Line Type:",
                      choices  = c("Dashed" = "dashed", "Solid" = "solid",
                                   "Dotted" = "dotted", "DotDash" = "dotdash",
                                   "LongDash" = "longdash"),
                      selected = "dashed"),
          selectInput(paste0("marker_color_", i), "Color:",
                      choices  = c("Black" = "#000000", "Red"   = "#E41A1C",
                                   "Blue"  = "#377EB8", "Green" = "#4DAF4A",
                                   "Gray"  = "#999999", "Custom" = "custom"),
                      selected = "#000000"),
          conditionalPanel(
            condition = paste0("input['marker_color_", i, "'] == 'custom'"),
            textInput(paste0("marker_color_custom_", i), "HEX:", "#000000")
          ),
          sliderInput(paste0("marker_size_", i), "Width:", 0.1, 3, 1, 0.1),
          checkboxInput(paste0("marker_label_", i), "Add Label", FALSE),
          conditionalPanel(
            condition = paste0("input['marker_label_", i, "'] == true"),
            textInput(paste0("marker_text_", i),           "Label:",    paste0("t=", 30)),
            numericInput(paste0("marker_label_size_", i),  "Size:",     4, 2, 12),
            selectInput(paste0("marker_label_position_", i), "Position:",
                        choices = c("Top" = "top", "Middle" = "middle", "Bottom" = "bottom"),
                        selected = "top"),
            numericInput(paste0("marker_label_hjust_", i), "H-Offset:", 0, -5, 5, 0.1)
          )
      )
    }))
  })
  
  # ── Time filtering ───────────────────────────────────────────────────────────
  apply_time_filters <- function(d, time_col) {
    d[[time_col]] <- suppressWarnings(as.numeric(as.character(d[[time_col]])))
    if (!isTRUE(input$enable_time_filter)) return(d)
    rng <- input$time_filter_range
    if (!is.null(rng) && length(rng) == 2) {
      t_vec <- d[[time_col]]
      d <- d[!is.na(t_vec) & t_vec >= rng[1] & t_vec <= rng[2], , drop = FALSE]
    }
    excl <- parse_excluded_timepoints(input$exclude_timepoints, rv$all_timepoints)
    if (length(excl) > 0) {
      t_vec <- d[[time_col]]
      tol   <- max(abs(rv$all_timepoints), na.rm = TRUE) * 0.001 + 0.001
      # Vectorised: matrix of abs differences, any column within tol means exclude
      hit   <- rowSums(abs(outer(t_vec, excl, "-")) <= tol) == 0L
      d     <- d[hit, , drop = FALSE]
    }
    d
  }
  
  # ── Data prep ────────────────────────────────────────────────────────────────
  # Zero-row frames with the usual columns: what prepare_plot_data() / prepare_replicate_data()
  # return when the selected samples match nothing that is loaded (for instance for a moment when a
  # new file replaces the old one), so every consumer sees "no data" instead of an error.
  empty_plot_data <- function()
    tibble::tibble(time = numeric(0), variable = character(0), mean_value = numeric(0),
                   sd_value = numeric(0), n = integer(0), sem_value = numeric(0), ci95_value = numeric(0))
  empty_replicate_data <- function()
    data.frame(time = numeric(0), variable = character(0), replicate = integer(0), value = numeric(0),
               stringsAsFactors = FALSE)
  prepare_plot_data <- function(samples = NULL) {
    req(rv$data, rv$time_col, input$selected_samples)
    samps <- if (!is.null(samples)) samples else input$selected_samples
    if (rv$is_long_format) {
      req(rv$group_col, rv$value_col)
      d  <- apply_time_filters(rv$data, rv$time_col)
      # Pre-filter rows to only requested samples before any grouping
      d  <- d[d[[rv$group_col]] %in% samps, , drop = FALSE]
      if (nrow(d) == 0) return(empty_plot_data())
      pd <- d %>%
        group_by(across(all_of(c(rv$time_col, rv$group_col)))) %>%
        summarise(mean_value = mean(.data[[rv$value_col]], na.rm = TRUE),
                  sd_value   = sd(  .data[[rv$value_col]], na.rm = TRUE),
                  # count only the non-NA values that actually enter the mean —
                  # counting NA rows would silently understate SEM / CI
                  n          = sum(!is.na(.data[[rv$value_col]])), .groups = "drop") %>%
        # KC-1: SD/SEM/95% CI are UNDEFINED at n < 2, not zero. sd() of a
        # single value already returns NA; the old code overwrote that NA
        # with 0, which reads as a perfectly precise measurement in the
        # exported CSV and report tables. Keep NA and let the plot layer
        # decide whether/how to draw it (see the error-bar block below).
        mutate(sem_value  = ifelse(n >= 2, sd_value / sqrt(n), NA_real_),
               ci95_value = ifelse(n >= 2, qt(0.975, df = pmax(n - 1, 1)) * sem_value, NA_real_))
      names(pd)[names(pd) == rv$group_col] <- "variable"
      names(pd)[names(pd) == rv$time_col]  <- "time"
    } else {
      d    <- apply_time_filters(rv$data, rv$time_col)
      meas <- intersect(rv$od_vars_raw, samps)
      if (length(meas) == 0) return(empty_plot_data())
      # v2.8 REPLICATE FIX: resolve sample columns BY POSITION via rv$col_map
      # so replicate columns sharing one name all contribute. Name-based
      # subsetting (d[, meas]) silently picked only the FIRST column per name.
      t_vec  <- d[[rv$time_col]]
      pieces <- lapply(meas, function(vn) {
        idxs <- rv$col_map[[vn]]
        if (is.null(idxs)) idxs <- which(colnames(d) == vn)
        vals <- unlist(lapply(idxs, function(i)
          suppressWarnings(as.numeric(as.character(d[[i]])))), use.names = FALSE)
        data.frame(time     = rep(t_vec, times = length(idxs)),
                   variable = vn, value = vals, stringsAsFactors = FALSE)
      })
      pd <- bind_rows(pieces) %>%
        group_by(time, variable) %>%
        summarise(mean_value = mean(value, na.rm = TRUE),
                  sd_value   = sd(  value, na.rm = TRUE),
                  n          = sum(!is.na(value)), .groups = "drop") %>%
        # KC-1: see the long-format branch above — NA (undefined), not 0.
        mutate(sem_value  = ifelse(n >= 2, sd_value / sqrt(n), NA_real_),
               ci95_value = ifelse(n >= 2, qt(0.975, df = pmax(n - 1, 1)) * sem_value, NA_real_))
    }
    # KC-7: optional blank/media OD subtraction. A constant offset applied
    # equally to every reading commutes with averaging, so subtracting it
    # here (once, from the already-aggregated mean) is exactly equivalent
    # to subtracting it from every raw value before averaging -- sd/n are
    # unaffected either way (SD is shift-invariant).
    blank <- input$blank_od %||% 0
    if (is.numeric(blank) && !is.na(blank) && blank != 0) pd$mean_value <- pd$mean_value - blank
    pd %>% filter(is.finite(time) & is.finite(mean_value))
  }

  # KC-4: the un-averaged counterpart of prepare_plot_data() -- one row per
  # (time, sample, replicate), never collapsed to a mean. Feeds
  # compute_replicate_metrics() so per-replicate metrics can be computed
  # with mean +/- SD reported across replicates, alongside (not instead of)
  # the mean-curve metrics prepare_plot_data()/compute_curve_metrics() give.
  prepare_replicate_data <- function(samples = NULL) {
    req(rv$data, rv$time_col, input$selected_samples)
    samps <- if (!is.null(samples)) samples else input$selected_samples
    if (rv$is_long_format) {
      req(rv$group_col, rv$value_col)
      d <- apply_time_filters(rv$data, rv$time_col)
      d <- d[d[[rv$group_col]] %in% samps, , drop = FALSE]
      if (nrow(d) == 0) return(empty_replicate_data())
      d <- d[order(d[[rv$group_col]], d[[rv$time_col]]), , drop = FALSE]
      # Long files have no explicit replicate-ID column: the nth row within
      # each (sample, time) cell is treated as "replicate n". This assumes
      # same-replicate rows appear in a consistent relative order across
      # time points (true whenever each time point's rows were entered/
      # exported in the same replicate order, the common case).
      rep_idx <- ave(seq_len(nrow(d)),
                     d[[rv$group_col]], d[[rv$time_col]], FUN = seq_along)
      # When the file has a replicate / well column, that column is the replicate identity
      # (dense rank of its values within each sample) instead of the occurrence order.
      if (!is.null(rv$replicate_col) && rv$replicate_col %in% colnames(d)) {
        rc <- d[[rv$replicate_col]]
        rep_idx <- ave(seq_len(nrow(d)), d[[rv$group_col]],
                       FUN = function(ix) match(rc[ix], sort(unique(rc[ix]))))
      }
      out <- data.frame(time      = d[[rv$time_col]],
                        variable  = d[[rv$group_col]],
                        replicate = rep_idx,
                        value     = suppressWarnings(as.numeric(as.character(d[[rv$value_col]]))),
                        stringsAsFactors = FALSE)
    } else {
      d    <- apply_time_filters(rv$data, rv$time_col)
      meas <- intersect(rv$od_vars_raw, samps)
      if (length(meas) == 0) return(empty_replicate_data())
      t_vec <- d[[rv$time_col]]
      # Stacked time blocks (rv$block_id, set at ingest) are replicates too: the replicate
      # index of (column r, block b) is (b - 1) * ncols + r, so no replicate curve ever
      # holds the same time point twice. rows are matched by their original row names,
      # which the time filters preserve.
      blk <- if (!is.null(rv$block_id)) rv$block_id[suppressWarnings(as.integer(rownames(d)))] else NULL
      if (!is.null(blk) && (length(blk) != nrow(d) || anyNA(blk))) blk <- NULL
      pieces <- lapply(meas, function(vn) {
        idxs <- rv$col_map[[vn]]
        if (is.null(idxs)) idxs <- which(colnames(d) == vn)
        bind_rows(lapply(seq_along(idxs), function(r) {
          vals <- suppressWarnings(as.numeric(as.character(d[[idxs[r]]])))
          data.frame(time      = t_vec, variable = vn,
                     replicate = if (is.null(blk)) r else (blk - 1L) * length(idxs) + r,
                     value     = vals, stringsAsFactors = FALSE)
        }))
      })
      out <- bind_rows(pieces)
      if (nrow(out) == 0) return(empty_replicate_data())
    }
    # KC-7: see prepare_plot_data() above -- same constant blank offset,
    # applied per replicate value here since this frame is un-averaged.
    blank <- input$blank_od %||% 0
    if (is.numeric(blank) && !is.na(blank) && blank != 0) out$value <- out$value - blank
    out %>% filter(is.finite(time) & is.finite(value))
  }

  # ── Aesthetics resolver ──────────────────────────────────────────────────────
  resolve_aesthetics <- function(samples) {
    n          <- length(samples)
    shapes     <- setNames(numeric(n),   samples)
    colors     <- setNames(character(n), samples)
    line_types <- setNames(character(n), samples)
    leg_labels <- setNames(character(n), samples)
    filled_map <- setNames(logical(n),   samples)
    def_colors <- c("#000000","#E41A1C","#377EB8","#4DAF4A",
                    "#984EA3","#FF7F00","#A65628","#F781BF",
                    "#999999","#66C2A5","#FC8D62","#8DA0CB")
    def_shapes <- c(16, 15, 17, 18, 25, 4, 8, 23, 3, 10)
    
    for (i in seq_len(n)) {
      vn  <- samples[i]; vid <- safe_id(vn)
      si  <- input[[paste0("shape_", vid)]]
      shapes[i] <- if (!is.null(si)) as.numeric(si)
      else def_shapes[(i - 1) %% length(def_shapes) + 1]
      
      sel <- input[[paste0("color_selector_", vid)]]
      if (!is.null(sel) && sel == "custom")
        colors[i] <- normalize_hex_color(input[[paste0("color_", vid)]])
      else if (!is.null(sel))
        colors[i] <- sel
      else if (!is.null(rv$palette_colors)) {
        idx       <- match(vn, rv$od_vars)
        colors[i] <- if (!is.na(idx)) rv$palette_colors[idx]
        else def_colors[(i - 1) %% length(def_colors) + 1]
      } else
        colors[i] <- def_colors[(i - 1) %% length(def_colors) + 1]
      
      lt <- input[[paste0("line_type_", vid)]]
      line_types[i] <- if (!is.null(lt)) lt else "solid"
      
      ll <- input[[paste0("legend_label_", vid)]]
      leg_labels[i] <- if (!is.null(ll) && nchar(ll) > 0) ll else vn
      
      fi <- input[[paste0("shape_filled_", vid)]]
      filled_map[i] <- if (!is.null(fi)) fi else TRUE
    }
    list(shapes = shapes, colors = colors, line_types = line_types,
         leg_labels = leg_labels, filled_map = filled_map)
  }
  
  # ── Shared figure theme (v2.11) ─────────────────────────────────────────────
  # Everything that makes the main lysis curve look the way it does, as a list
  # of theme layers, so the analysis figures and the dashboard panels use the
  # same fonts, line weights, legend, spacing and journal preset. Adding the
  # list to a ggplot applies the layers in order; the main figure's output is
  # byte-identical to v2.10 (test suite).
  # v2.12.2: xy = list(x, y) of the plotted points, n_entries = legend entries,
  # log_y = y axis is log. Only used by legend_position "inside-auto".
  figure_theme_layers <- function(xy = NULL, n_entries = 0, log_y = FALSE, label_chars = 12) {
    tf      <- if (isTRUE(input$bold_title))        "bold"   else "plain"
    af      <- if (isTRUE(input$italic_axis_labels)) "italic" else "plain"
    # v2.10: theme spacing (margins, gaps) follows the theme's base_size, which
    # defaults to 14 (prism) / 12 (pubr) and looks oversized next to 6-7 pt text.
    # 0 keeps each theme's own default, so existing figures do not change.
    bs      <- input$theme_base_pt %||% 0
    base_t  <- if (isTRUE(input$use_advanced_ticks)) {
      if (is.numeric(bs) && is.finite(bs) && bs > 0) theme_prism(border = TRUE, base_size = bs)
      else theme_prism(border = TRUE)
    } else {
      if (is.numeric(bs) && is.finite(bs) && bs > 0) theme_pubr(base_size = bs) else theme_pubr()
    }
    maj_col <- if (!is.null(input$major_gridline_color) && input$major_gridline_color == "custom")
      input$major_gridline_color_custom else input$major_gridline_color
    min_col <- if (!is.null(input$minor_gridline_color) && input$minor_gridline_color == "custom")
      input$minor_gridline_color_custom else input$minor_gridline_color
    x_ang   <- as.numeric(input$axis_text_angle %||% "0")
    
    # v2.10 figure-layout inputs. Defaults equal the previous hard-coded values
    # (1.07 pt == linewidth 0.5; 1.5 mm == 0.15 cm; legend right, grey border).
    axis_lw  <- pt2lw(input$axis_line_pt %||% 1.07)
    tick_cm  <- (input$tick_length_mm %||% 1.5) / 10
    leg_pos  <- input$legend_position %||% "right"
    leg_bord <- if (isTRUE(input$legend_box_border %||% TRUE)) "gray80" else NA
    leg_key  <- input$legend_key_mm %||% 0
    
    layers <- list(base_t, theme(
      text             = element_text(family = input$font_family),
      plot.title       = element_text(size = input$title_font_size,       face = tf, hjust = 0.5),
      plot.caption     = element_text(size = max(8, input$axis_text_font_size * 0.7),
                                      hjust = 1, color = "grey30"),
      plot.tag         = element_text(size = input$panel_label_size %||% 8, face = "bold"),
      axis.title       = element_text(size = input$axis_label_font_size,  face = af),
      axis.text        = element_text(size = input$axis_text_font_size),
      axis.text.x      = element_text(angle = x_ang,
                                      hjust = if (x_ang > 0) 1 else 0.5,
                                      vjust = if (x_ang > 0) 1 else 0.5),
      panel.grid.major = if (isTRUE(input$show_major_gridlines))
        element_line(color = maj_col, linewidth = input$major_gridline_size) else element_blank(),
      panel.grid.minor = if (isTRUE(input$show_minor_gridlines))
        element_line(color = min_col, linewidth = input$minor_gridline_size) else element_blank(),
      axis.ticks        = element_line(color = "black", linewidth = axis_lw),
      axis.ticks.length = unit(tick_cm, "cm"),
      legend.text       = element_text(size = input$legend_font_size %||% 14),
      legend.title      = element_text(size = input$legend_font_size %||% 14),
      legend.background = element_rect(fill = "white", color = leg_bord),
      legend.key        = element_rect(fill = NA),
      panel.border      = element_rect(color = "black", fill = NA, linewidth = axis_lw)
    ))
    # Legend placement: outside positions as before; "inside-*" anchors the key
    # just inside a panel corner (journal panels rarely spend column width on
    # an outside key).
    if (identical(leg_pos, "inside-auto")) {
      # emptiest corner of the plotted data; too many entries, or a corner that
      # still holds data -> outside right
      if (!is.null(xy) && !is.null(xy$g)) {      # count along the lines, not just at the data points
        yy <- if (log_y) log10(ifelse(xy$y > 0, xy$y, NA_real_)) else xy$y
        dd <- densify_xy(xy$x, yy, xy$g, k = 8)
        xy <- list(x = dd$x, y = if (log_y) 10^dd$y else dd$y, per_point = 8)
      }
      cnt <- if (is.null(xy)) c("top-left" = 0L)
             else legend_corner_counts(xy$x, xy$y, log_y, box = inside_legend_box(n_entries, label_chars))
      if (!is.null(xy$per_point)) cnt <- cnt / xy$per_point   # interpolated points -> data-point equivalents
      if (n_entries > INSIDE_LEGEND_MAX_ENTRIES || min(cnt) >= INSIDE_LEGEND_MAX_POINTS) {
        layers <- c(layers, list(theme(legend.position = "right")))
      } else {
        layers <- c(layers, list(inside_legend_theme(names(cnt)[which.min(cnt)])))
      }
    } else if (startsWith(leg_pos, "inside-")) {
      layers <- c(layers, list(inside_legend_theme(sub("^inside-", "", leg_pos))))
    } else {
      layers <- c(layers, list(theme(legend.position = leg_pos)))
    }
    if (!isTRUE(input$use_advanced_ticks))          # theme_pubr draws axis lines
      layers <- c(layers, list(theme(axis.line = element_line(linewidth = axis_lw))))
    if (is.numeric(leg_key) && is.finite(leg_key) && leg_key > 0)
      layers <- c(layers, list(theme(legend.key.size = unit(leg_key, "mm"))))
    layers
  }
  
  # v2.17: what happens to a reading outside the y limits (below the log-axis floor in particular)
  y_oob_fn <- function()
    if (identical(input$y_oob %||% "squish", "censor")) scales::censor else scales::squish

  # v2.10: tick-label style for log axes. "legacy" is ggplot2's default (0.10).
  y_tick_labels <- function() {
    switch(input$y_tick_label_style %||% "legacy",
           trim  = scales::label_number(drop0trailing = TRUE, big.mark = ""),
           log10 = scales::label_log(),
           waiver())
  }
  
  # v2.12: one sample order for every figure. "alpha" is ggplot2's default
  # factor order (what the main figure always did); "selected" keeps the
  # selection / file order so the control comes first and every panel agrees.
  sample_levels <- function(samples) {
    if (identical(input$legend_order, "selected")) samples else sort(samples)
  }
  
  # ── Core plot builder ────────────────────────────────────────────────────────
  # highlight_samples: if non-NULL, all other samples are greyed out (not removed)
  build_plot <- function(plot_data, samples, aes_vals, highlight_samples = NULL) {
    shapes     <- aes_vals$shapes;     colors     <- aes_vals$colors
    line_types <- aes_vals$line_types; leg_labels <- aes_vals$leg_labels
    filled_map <- aes_vals$filled_map; n_vars     <- length(samples)
    
    if (identical(input$legend_order, "selected"))
      plot_data$variable <- factor(as.character(plot_data$variable), levels = samples)
    
    # Grey out non-highlighted samples while keeping axis scale stable
    display_colors <- colors
    if (!is.null(highlight_samples))
      for (i in seq_len(n_vars))
        if (!samples[i] %in% highlight_samples) display_colors[i] <- "#DDDDDD"
    
    p <- ggplot(plot_data,
                aes(x = time, y = mean_value, group = variable,
                    color = variable, shape = variable, linetype = variable))
    
    # Region highlighting
    if (isTRUE(input$enable_highlighting) && !is.null(input$region_count)) {
      for (i in seq_len(input$region_count)) {
        x1 <- input[[paste0("region_x_min_", i)]]; x2 <- input[[paste0("region_x_max_", i)]]
        y1 <- input[[paste0("region_y_min_", i)]]; y2 <- input[[paste0("region_y_max_", i)]]
        rc  <- input[[paste0("region_color_", i)]]
        col <- if (!is.null(rc) && rc == "custom") input[[paste0("region_color_custom_", i)]] else rc
        alp <- input[[paste0("region_alpha_", i)]]
        if (!is.null(x1) && !is.null(col) && !is.null(alp))
          p <- p + annotate("rect", xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=col, alpha=alp)
      }
    }
    
    # Time markers
    if (isTRUE(input$enable_time_markers) && !is.null(input$marker_count)) {
      yr <- range(plot_data$mean_value, na.rm = TRUE)
      if (input$y_scale_type == "log") yr[1] <- max(1e-9, yr[1])
      for (i in seq_len(input$marker_count)) {
        tp <- input[[paste0("marker_time_", i)]]
        lt <- input[[paste0("marker_line_type_", i)]]
        mc <- input[[paste0("marker_color_", i)]]
        lc <- if (!is.null(mc) && mc == "custom") input[[paste0("marker_color_custom_", i)]] else mc
        ls <- input[[paste0("marker_size_", i)]]
        if (!is.null(tp) && !is.null(lc))
          p <- p + geom_vline(xintercept = tp, linetype = lt, color = lc, linewidth = ls)
        if (isTRUE(input[[paste0("marker_label_", i)]])) {
          lbl  <- input[[paste0("marker_text_",         i)]]
          lsz  <- input[[paste0("marker_label_size_",   i)]]
          lpos <- input[[paste0("marker_label_position_",i)]]
          lhj  <- input[[paste0("marker_label_hjust_",  i)]]; if (is.null(lhj)) lhj <- 0
          yp <- switch(lpos,
                       top    = if (input$y_scale_type == "log") 10^(log10(yr[2]) - diff(log10(yr)) * 0.08) else yr[1] + diff(yr) * 0.92,
                       bottom = if (input$y_scale_type == "log") 10^(log10(yr[1]) + diff(log10(yr)) * 0.08) else yr[1] + diff(yr) * 0.08,
                       middle = if (input$y_scale_type == "log") 10^(mean(log10(yr))) else mean(yr))
          p <- p + annotate("text", x = tp + lhj, y = yp, label = lbl, size = lsz, color = lc)
        }
      }
    }
    
    # v2.17: horizontal OD threshold line (ported from the companion app), with an optional label at
    # the right end in the axis-text size. Not drawn at or below 0 on a log axis.
    thr <- suppressWarnings(as.numeric(input$threshold_value))
    if (isTRUE(input$enable_threshold) && length(thr) == 1 && is.finite(thr) &&
        !(identical(input$y_scale_type, "log") && thr <= 0)) {
      thr_col <- input$threshold_color %||% "grey40"
      p <- p + geom_hline(yintercept = thr, linetype = input$threshold_linetype %||% "dashed",
                          color = thr_col, linewidth = pt2lw(input$axis_line_pt %||% 0.5))
      thr_lbl <- trimws(input$threshold_label %||% "")
      if (nzchar(thr_lbl) && any(is.finite(plot_data$time)))
        p <- p + geom_text(data = data.frame(x = max(plot_data$time, na.rm = TRUE), y = thr, label = thr_lbl),
                           aes(x = x, y = y, label = label), inherit.aes = FALSE, hjust = 1, vjust = -0.4,
                           color = thr_col, size = (input$axis_text_font_size %||% 8) / 2.835,
                           family = if (nzchar(input$font_family %||% "")) input$font_family else "")
    }

    # Error bars / shadow
    # Track whether shadow/ribbon mode is used so we can unify the fill scale
    # with the points layer later (ggplot2 only allows ONE scale per aesthetic).
    using_shadow <- FALSE
    
    if (input$error_type != "none") {
      ec     <- switch(input$error_type, sd="sd_value", sem="sem_value", ci95="ci95_value")
      em     <- input$error_multiplier
      # KC-1: an NA error value means "undefined" (n < 2 replicates at that
      # point), not zero. Leave it NA — pmax()/arithmetic below propagate it
      # to err_lo/err_hi automatically — and draw the error-bar/ribbon layer
      # from a subset that excludes those rows, rather than drawing a false
      # zero-height bar for a single replicate.
      err_vals <- plot_data[[ec]]

      # v2.12: on a log axis a lower bound <= 0 cannot be drawn. The old code
      # clamped it to 1e-9, which the scale limits then censored, so the whole
      # bar vanished silently. Now (default) it is floored at the axis minimum,
      # so the bar visibly runs to the floor.
      lo_val <- if (input$y_scale_type == "log") {
        if (isTRUE(input$error_log_floor %||% TRUE)) {
          y_floor <- if (isTRUE(input$custom_y_limits)) input$y_min
                     else if (isTRUE(input$use_advanced_ticks)) 10^(input$y_log_min_exponent %||% -2)
                     else min(plot_data$mean_value[plot_data$mean_value > 0], na.rm = TRUE) / 2
          pmax(plot_data$mean_value - err_vals * em, y_floor)
        } else pmax(plot_data$mean_value - err_vals * em, 1e-9)
      }
      else if (isTRUE(input$asymmetric_error))
        pmax(plot_data$mean_value - err_vals * em, 0)
      else
        plot_data$mean_value - err_vals * em
      plot_data$err_lo <- lo_val
      plot_data$err_hi <- plot_data$mean_value + err_vals * em
      eb_data <- plot_data[!is.na(plot_data$err_lo) & !is.na(plot_data$err_hi), , drop = FALSE]

      dm <- if (!is.null(input$error_display_mode)) input$error_display_mode else "bars"
      if (dm == "shadow") {
        using_shadow <- TRUE
        p <- p +
          geom_ribbon(data = eb_data,
                      aes(x = time, ymin = err_lo, ymax = err_hi,
                          group = variable, fill = variable),
                      alpha = input$shadow_alpha, color = NA, inherit.aes = FALSE)
        # NOTE: scale_fill_manual is added AFTER the points section to unify
        # the fill scale for both ribbon and points (if points are enabled).
      } else {
        pos <- if (!is.null(input$error_bar_position) && input$error_bar_position == "dodge")
          position_dodge(0.2) else position_identity()
        blt <- if (!is.null(input$error_bar_style) && input$error_bar_style == "dashed") "dashed" else "solid"
        # v2.12: cap width as % of the x range, and a fixed bar colour (black /
        # grey bars are the journal convention when the lines carry the colour)
        cap_w <- if (isTRUE(input$error_cap_relative))
          diff(range(plot_data$time, na.rm = TRUE)) * input$error_bar_width / 100 else input$error_bar_width
        eb_col <- input$error_bar_color %||% "sample"
        eb_aes <- if (eb_col == "sample") aes(x = time, ymin = err_lo, ymax = err_hi, color = variable)
                  else aes(x = time, ymin = err_lo, ymax = err_hi, group = variable)
        eb_fixed <- if (eb_col == "sample") list() else list(color = eb_col)
        if (!is.null(input$error_bar_style) && input$error_bar_style == "T") {
          p <- p + do.call(geom_errorbar, c(list(data = eb_data, mapping = eb_aes,
                                 width = cap_w, linewidth = input$error_bar_thickness,
                                 linetype = blt, position = pos, inherit.aes = FALSE), eb_fixed))
        } else {
          p <- p + do.call(geom_linerange, c(list(data = eb_data, mapping = eb_aes,
                                  linewidth = input$error_bar_thickness,
                                  linetype = blt, position = pos, inherit.aes = FALSE), eb_fixed))
        }
      }
    }
    
    # v2.12: individual replicate values behind the mean (mean +/- error with
    # the raw observations is what reviewers ask for at n = 3)
    if (isTRUE(input$show_replicate_points)) {
      rp <- tryCatch(prepare_replicate_data(samples = samples), error = function(e) NULL)
      if (!is.null(rp) && nrow(rp) > 0) {
        if (input$y_scale_type == "log") rp <- rp[rp$value > 0, , drop = FALSE]
        jit <- diff(range(plot_data$time, na.rm = TRUE)) * 0.006
        p <- p + geom_point(data = rp, aes(x = time, y = value, color = variable),
                            shape = 1, size = input$shape_size * 0.5, alpha = 0.55,
                            stroke = max(0.2, input$point_stroke * 0.8),
                            position = position_jitter(width = jit, height = 0, seed = 1),
                            inherit.aes = FALSE, show.legend = FALSE)
      }
    }
    
    p <- p + geom_line(aes(group = variable), linewidth = input$line_thickness)
    
    if (input$show_points) {
      # Build lookup tables of shape and fill per variable.
      shape_map <- integer(n_vars)
      fill_map  <- character(n_vars)
      for (i in seq_len(n_vars)) {
        pt_s <- as.integer(shapes[i])
        if (!filled_map[i]) {
          pt_s         <- switch(as.character(pt_s),
                                 "16" = 21L, "15" = 22L, "17" = 24L, "18" = 23L, pt_s)
          fill_map[i]  <- "white"
        } else {
          fill_map[i]  <- display_colors[i]
        }
        shape_map[i] <- pt_s
      }
      names(shape_map) <- samples
      names(fill_map)  <- samples
      
      if (using_shadow) {
        # When shadow/ribbon is active, both ribbon and points share the 'fill'
        # aesthetic mapped to 'variable'.  We use a single scale_fill_manual that
        # maps each variable to its display color.  For open (unfilled) shapes the
        # fill is overridden to white via `override.aes` — open shapes use stroke-
        # based variants (21/22/24/23) whose interior respects the fill scale, so
        # we simply accept the fill color from the scale for filled shapes.
        # 
        # Trade-off: open shapes will show the ribbon color fill instead of white
        # inside the point.  This is acceptable because the alternative (two
        # conflicting fill scales) crashes ggplot2 entirely.  Users who need white-
        # filled open shapes can switch error display to "bars" mode.
        p <- p + geom_point(
          data        = plot_data,
          aes(x = time, y = mean_value, color = variable, shape = variable,
              fill = variable),
          size        = input$shape_size,
          stroke      = input$point_stroke,
          inherit.aes = FALSE
        ) +
          scale_shape_manual(values = shape_map, labels = leg_labels,
                             name = NULL, guide = "none") +
          scale_fill_manual(values = setNames(display_colors, samples),
                            labels = leg_labels, guide = "none")
      } else {
        # No shadow: use scale_fill_identity for per-point fill control
        pt_data          <- plot_data
        pt_data$pt_fill  <- fill_map[pt_data$variable]
        
        p <- p + geom_point(
          data        = pt_data,
          aes(x = time, y = mean_value, color = variable, shape = variable,
              fill = pt_fill),
          size        = input$shape_size,
          stroke      = input$point_stroke,
          inherit.aes = FALSE
        ) +
          scale_shape_manual(values = shape_map, labels = leg_labels,
                             name = NULL, guide = "none") +
          scale_fill_identity()
      }
    } else if (using_shadow) {
      # Points disabled but shadow is active: still need the fill scale for ribbon
      p <- p + scale_fill_manual(values = setNames(display_colors, samples),
                                  labels = leg_labels, guide = "none")
    }
    
    if (input$show_end_labels) {
      mt  <- max(plot_data$time, na.rm = TRUE)
      off <- mt * (input$label_offset / 100)
      ep  <- plot_data %>% group_by(variable) %>% filter(time == max(time)) %>% ungroup()
      p   <- p + geom_text_repel(data = ep,
                                 aes(label = variable, color = variable, x = Inf, y = mean_value),
                                 direction = "y", xlim = c(mt + off, Inf),
                                 min.segment.length = Inf, hjust = 0,
                                 size = input$label_font_size / 2.835,
                                 fontface = if (input$label_bold) "bold" else "plain")
    }
    
    p <- p +
      scale_color_manual(   values = setNames(display_colors, samples), labels = leg_labels, name = NULL) +
      scale_linetype_manual(values = line_types,                         labels = leg_labels, name = NULL)
    
    # When points are disabled the shape scale wasn't added inside the points block;
    # add it now for a consistent legend.
    if (!isTRUE(input$show_points))
      p <- p + scale_shape_manual(values = shapes, labels = leg_labels, name = NULL)
    
    p <- p +
      guides(shape = guide_legend(override.aes = list(alpha = 1)),
             color = guide_legend(override.aes = list(alpha = 1)))
    
    x_exp  <- expansion(mult = c(input$x_expand_left,   input$x_expand_right))
    y_exp  <- expansion(mult = c(input$y_expand_bottom, input$y_expand_top))
    x_lims <- if (isTRUE(input$custom_x_limits)) c(input$x_min, input$x_max) else NULL
    y_lims <- if (isTRUE(input$custom_y_limits)) c(input$y_min, input$y_max) else NULL
    
    if (input$x_scale_type == "log") {
      p <- p + scale_x_log10(limits = x_lims, expand = x_exp)
    } else if (input$x_scale_type == "sqrt") {
      p <- p + scale_x_sqrt(limits = x_lims, expand = x_exp)
    } else if (input$x_scale_type == "reverse") {
      p <- p + scale_x_reverse(limits = x_lims, expand = x_exp)
    } else if (isTRUE(input$use_advanced_ticks)) {
      # Determine the upper bound for tick generation
      mt2 <- if (isTRUE(input$custom_x_limits)) input$x_max
      else max(plot_data$time, na.rm = TRUE)
      x0  <- if (isTRUE(input$custom_x_limits)) input$x_min else 0
      
      # Parse any user-specified extra tick positions
      extra_ticks <- tryCatch({
        raw <- trimws(input$x_extra_ticks)
        if (nchar(raw) == 0) numeric(0)
        else {
          vals <- suppressWarnings(as.numeric(strsplit(raw, "[,;[:space:]]+")[[1]]))
          vals[is.finite(vals)]
        }
      }, error = function(e) numeric(0))
      
      # Determine major interval: manual if provided, otherwise auto
      manual_interval <- input$x_tick_interval
      use_manual <- !is.null(manual_interval) &&
        !is.na(manual_interval) &&
        is.numeric(manual_interval) &&
        manual_interval > 0
      
      # v2.14.1: count the breaks BEFORE building them: an interval of 0.0001 over a 6-hour axis
      # (or a huge time range) would otherwise allocate millions of break positions.
      iv_est   <- if (use_manual) manual_interval else if (mt2 <= 60) 10 else if (mt2 <= 120) 30 else 60
      minor_st <- if (use_manual) manual_interval / 2 else if (mt2 <= 60) 5 else if (mt2 <= 120) 10 else 30
      n_major_est <- (mt2 - x0) / iv_est + 1; n_minor_est <- (mt2 - x0) / minor_st + 1
      ticks_ok <- all(is.finite(c(n_major_est, n_minor_est))) && max(n_major_est, n_minor_est) <= TICK_MAX_BREAKS
      if (!ticks_ok) {
        guard_notify("tick_guard", sprintf("The x tick interval would draw about %s ticks; ggplot's default breaks are used instead.",
                                           format(round(max(n_major_est, n_minor_est)), big.mark = ",", scientific = FALSE)))
        mjb <- NULL; mnb <- NULL
      } else if (use_manual) {
        iv  <- manual_interval
        mjb <- seq(x0, ceiling((mt2 - x0) / iv) * iv + x0, by = iv)
        # Always include the actual data endpoint if it isn't already a break
        if (!isTRUE(input$custom_x_limits) && !mt2 %in% mjb)
          mjb <- sort(unique(c(mjb, mt2)))
        mnb <- seq(x0, max(mjb), by = iv / 2)
      } else {
        # Auto interval (original logic)
        if (mt2 <= 60)       { iv <- 10;  mjb <- seq(0, ceiling(mt2/10)*10,  10);  mnb <- seq(0, ceiling(mt2/10)*10,  5) }
        else if (mt2 <= 120) { iv <- 30;  mjb <- seq(0, ceiling(mt2/30)*30,  30);  mnb <- seq(0, ceiling(mt2/30)*30, 10) }
        else                 { iv <- 60;  mjb <- seq(0, ceiling(mt2/60)*60,  60);  mnb <- seq(0, ceiling(mt2/60)*60, 30) }
        # Always include the actual data endpoint even in auto mode
        if (!isTRUE(input$custom_x_limits) && !mt2 %in% mjb)
          mjb <- sort(unique(c(mjb, mt2)))
      }
      
      # Union in any extra positions the user explicitly requested
      if (!is.null(mjb) && length(extra_ticks) > 0)
        mjb <- sort(unique(c(mjb, extra_ticks)))
      
      p <- p + if (is.null(mjb)) scale_x_continuous(limits = x_lims, expand = x_exp, guide = guide_prism_minor())
               else scale_x_continuous(limits = x_lims, expand = x_exp, breaks = mjb,
                                       minor_breaks = mnb, guide = guide_prism_minor())
    } else {
      p <- p + scale_x_continuous(limits = x_lims, expand = x_exp)
    }
    
    y_oob <- y_oob_fn()   # v2.17: readings outside the axis limits are clipped to them ("squish") or dropped ("censor")
    if (input$y_scale_type == "log") {
      y_labs <- y_tick_labels()
      if (isTRUE(input$use_advanced_ticks)) {
        minE <- input$y_log_min_exponent; maxE <- input$y_log_max_exponent
        if (is.null(y_lims)) y_lims <- c(10^minE, if (maxE >= 0) 10^(maxE+1) else 10^maxE)
        if (!is.finite(maxE - minE) || maxE < minE || maxE - minE > 20) {
          # v2.14.1: a nonsensical or enormous exponent range: default minor breaks
          guard_notify("tick_guard", "The log axis exponent range is too wide (or reversed) for minor ticks; default breaks are used.")
          p <- p + scale_y_log10(limits = y_lims, expand = y_exp, labels = y_labs, guide = guide_prism_minor(), oob = y_oob)
        } else {
        ymn <- c(rep(1:9, maxE-minE+1) * 10^rep(minE:maxE, each=9))
        if (maxE >= 0) ymn <- c(ymn, 10^(maxE+1))
        p <- p + scale_y_log10(limits = y_lims, expand = y_exp, minor_breaks = ymn,
                               labels = y_labs, guide = guide_prism_minor(), oob = y_oob)
        }
      } else {
        p <- p + scale_y_log10(limits = y_lims, expand = y_exp, labels = y_labs, oob = y_oob)
      }
    } else if (input$y_scale_type == "sqrt") {
      p <- p + scale_y_sqrt(limits = y_lims, expand = y_exp, oob = y_oob)
    } else if (input$y_scale_type == "reverse") {
      p <- p + scale_y_reverse(limits = y_lims, expand = y_exp, oob = y_oob)
    } else {
      p <- p + scale_y_continuous(limits = y_lims, expand = y_exp, oob = y_oob)
    }
    
    bs     <- input$theme_base_pt %||% 0
    pl_lab <- trimws(input$panel_label %||% "")
    p <- p + figure_theme_layers(xy = list(x = plot_data$time, y = plot_data$mean_value, g = plot_data$variable),
                                 n_entries = n_vars, log_y = identical(input$y_scale_type, "log"),
                                 label_chars = max(nchar(unname(leg_labels)), 1))
    
    if (isTRUE(input$custom_aspect_ratio))
      p <- p + theme(aspect.ratio = 1 / input$aspect_ratio)

    # Optional publication caption describing the error measure and replicate count.
    # Purely additive: NULL (unchanged output) unless the user opts in AND error bars
    # are being drawn. Reads n from plot_data, which is populated in prepare_plot_data().
    err_caption <- NULL
    if (isTRUE(input$show_error_caption) && input$error_type != "none" &&
        !is.null(plot_data$n) && length(plot_data$n) > 0) {
      err_lab <- switch(input$error_type,
                        sd   = "SD",
                        sem  = "SEM",
                        ci95 = "95% CI",
                        input$error_type)
      n_rng <- range(plot_data$n, na.rm = TRUE)
      n_txt <- if (is.finite(n_rng[1]) && n_rng[1] == n_rng[2])
        paste0("n = ", n_rng[1])
      else
        paste0("n = ", n_rng[1], "–", n_rng[2])
      mult_txt <- if (!is.null(input$error_multiplier) &&
                      is.numeric(input$error_multiplier) &&
                      input$error_multiplier != 1)
        paste0(input$error_multiplier, "× ") else ""
      err_caption <- paste0("Error bars: ", mult_txt, "mean ± ", err_lab,
                            " (", n_txt, ").")
    }

    p + coord_cartesian(clip = "off") +
      labs(x        = input$x_axis_label,
           y        = axis_y_label(input$y_axis_label),
           # v2.10: with journal spacing (theme_base_pt > 0) a blank title is NULL,
           # so no empty title row (~5 mm) is reserved above the panel. Legacy
           # spacing keeps "" so pre-2.10 figures reproduce exactly.
           title    = if (nchar(trimws(input$plot_title %||% "")) > 0) input$plot_title
                      else if (is.numeric(bs) && is.finite(bs) && bs > 0) NULL else "",
           subtitle = if (nchar(trimws(input$plot_subtitle)) > 0) input$plot_subtitle else NULL,
           caption  = err_caption,
           tag      = if (nzchar(pl_lab)) pl_lab else NULL)
  }
  
  generate_plot <- function() {
    req(rv$data, input$selected_samples)
    tryCatch(
      build_plot(prepare_plot_data(), input$selected_samples,
                 resolve_aesthetics(input$selected_samples)),
      error = function(e) {
        # Return a blank ggplot with the error message rather than crashing the session
        plot_error_figure(friendly_error_message(e))
      }
    )
  }
  
  # Debounced reactive: coalesces rapid input changes (slider drags, typing) into
  # a single plot rebuild after 400 ms of inactivity — prevents render pile-up.
  plot_inputs_debounced <- debounce(reactive({
    list(
      selected_samples    = input$selected_samples,
      error_type          = input$error_type,
      error_display_mode  = input$error_display_mode,
      error_multiplier    = input$error_multiplier,
      line_thickness      = input$line_thickness,
      show_points         = input$show_points,
      shape_size          = input$shape_size,
      x_scale_type        = input$x_scale_type,
      y_scale_type        = input$y_scale_type,
      custom_x_limits     = input$custom_x_limits,
      custom_y_limits     = input$custom_y_limits,
      x_min = input$x_min, x_max = input$x_max,
      y_min = input$y_min, y_max = input$y_max,
      enable_time_filter  = input$enable_time_filter,
      time_filter_range   = input$time_filter_range,
      exclude_timepoints  = input$exclude_timepoints
    )
  }), millis = 400)
  
  # ── Export size (v2.10): one source of truth, in inches ─────────────────────
  # Every export path (image, reports, PPTX, true-size preview) reads this, so a
  # journal preset given in mm cannot drift from what the reports embed.
  # v2.14.1: a notification that replaces its predecessor with the same id (so a render that
  # repeats does not stack banners); never lets a notification failure break a render.
  guard_notify <- function(id, msg, type = "warning")
    tryCatch(showNotification(msg, id = id, type = type, duration = 10), error = function(e) invisible(NULL))
  # v2.14.1: every raster render goes through this: the requested dpi, reduced when the pixel
  # count would exceed 40 megapixels (with a notification saying so).
  raster_dpi <- function(width_in, height_in, dpi) {
    if (!is.numeric(dpi) || length(dpi) != 1 || !is.finite(dpi) || dpi <= 0) dpi <- 96
    r <- safe_raster_dpi(width_in, height_in, dpi, RASTER_MAX_PIXELS)
    if (isTRUE(attr(r, "capped")))
      guard_notify("raster_cap", sprintf("Raster reduced to %s dpi to stay within memory (%s x %s px).",
                                         r, round(width_in * r), round(height_in * r)))
    as.numeric(r)
  }
  # the dpi for an image download: raster formats are capped, vector formats ignore it
  export_dpi_for <- function(fmt, width_in, height_in) {
    d <- input$export_dpi
    if (fmt %in% c("png", "tiff", "jpeg")) raster_dpi(width_in, height_in, d)
    else if (is.numeric(d) && length(d) == 1 && is.finite(d)) d else 300
  }
  # ── Fixed panel size (v2.15) ────────────────────────────────────────────────
  fixed_mode <- function() identical(input$panel_size_mode, "fixed")
  CURVE_KINDS <- c("main", "growth_rate", "normalized", "replicate_overlay", "landmarks", "dose_response")
  # Panel (plotting area) size in mm for a figure kind: curve figures are square when
  # panel_square is on; the metric summary and heatmap use width x height as given.
  panel_dims_mm <- function(kind = "main") {
    cl <- function(x, d) if (is.numeric(x) && length(x) == 1 && is.finite(x)) min(max(x, 10), 400) else d
    w  <- cl(input$panel_width_mm, 65)
    h  <- cl(input$panel_height_mm, w)
    if (kind %in% CURVE_KINDS && isTRUE(input$panel_square %||% TRUE)) h <- w
    c(w, h)
  }
  fixed_figure <- function(p, kind = "main") {
    d <- panel_dims_mm(kind)
    fixed_panel_grob(p, d[1], d[2])
  }
  main_fixed_info    <- reactive(fixed_figure(generate_plot(), "main"))
  analysis_fixed_info <- reactive(fixed_figure(generate_analysis_plot(input$analysis_type %||% "metric_summary"),
                                               input$analysis_type %||% "metric_summary"))
  # what the main / analysis figure is drawn as: the fixed-panel gtable, or the plain ggplot (fit mode)
  main_plot_obj     <- function() if (fixed_mode()) main_fixed_info() else generate_plot()
  analysis_plot_obj <- function() if (fixed_mode()) analysis_fixed_info()
                                  else generate_analysis_plot(input$analysis_type %||% "metric_summary")
  size_from_grob <- function(g, kind) {
    sz <- tryCatch(attr(g, "size_mm"), error = function(e) NULL)
    if (is.null(sz)) { d <- panel_dims_mm(kind); sz <- d + c(13, 12) }
    sz / 25.4
  }
  # the figure size when a ggplot cannot be built yet (no data): the panel plus a typical margin
  fixed_fallback_in <- function(kind = "main") (panel_dims_mm(kind) + c(13, 12)) / 25.4
  analysis_size_in <- function() {
    if (!fixed_mode()) return(export_size_in())
    tryCatch(size_from_grob(analysis_fixed_info(), input$analysis_type %||% "metric_summary"),
             error = function(e) { if (inherits(e, "shiny.silent.error")) fixed_fallback_in("metric_summary") else stop(e) })
  }
  export_size_in <- function() {
    if (fixed_mode()) {
      sz <- tryCatch(size_from_grob(main_fixed_info(), "main"),
                     error = function(e) { if (inherits(e, "shiny.silent.error")) fixed_fallback_in("main") else stop(e) })
      mx <- SIZE_MAX_IN
      if (any(sz > mx)) { guard_notify("size_clamp", sprintf("Figure size limited to %s in per side.", mx)); sz <- pmin(sz, mx) }
      return(sz)
    }
    mm <- identical(input$export_units, "mm")
    w <- input$export_width  %||% 10
    h <- input$export_height %||% 8
    # an emptied or nonsensical box falls back to the standard size; huge values are clamped
    if (!is.numeric(w) || length(w) != 1 || !is.finite(w) || w <= 0) w <- if (mm) 89 else 89 / 25.4
    if (!is.numeric(h) || length(h) != 1 || !is.finite(h) || h <= 0) h <- if (mm) 65 else 65 / 25.4
    mx <- if (mm) SIZE_MAX_IN * 25.4 else SIZE_MAX_IN
    if (w > mx || h > mx) {
      guard_notify("size_clamp", sprintf("Export size limited to %s x %s: each side is at most %s %s.",
                                         round(min(w, mx), 1), round(min(h, mx), 1), round(mx), if (mm) "mm" else "in"))
      w <- min(w, mx); h <- min(h, mx)
    }
    if (mm) c(w, h) / 25.4 else c(w, h)
  }
  
  # Switching units converts the current numbers so the physical size is kept
  # (10 in -> 254 mm). A preset or a settings file sets units + numbers
  # together and pre-marks the unit it sends (rv$units_seen), so those values
  # are not converted a second time.
  rv$units_seen <- APP_DEFAULTS$export_units
  # One update loop for everything that pushes a settings list into the UI: the journal
  # preset, the legacy look and Clear & Reset. A value's type picks the widget update
  # (radio / select / text updates on a mismatched widget are no-ops).
  apply_settings <- function(vals) {
    if (is.character(vals$export_units)) rv$units_seen <- vals$export_units   # see observeEvent(input$export_units)
    for (nm in names(vals)) {
      val <- vals[[nm]]
      tryCatch({
        if (is.logical(val))      updateCheckboxInput(session, nm, value = val)
        else if (is.numeric(val)) updateNumericInput( session, nm, value = val)
        else if (is.character(val) && length(val) > 1) updateCheckboxGroupInput(session, nm, selected = val)
        else { updateSelectInput(session,   nm, selected = val)
               updateRadioButtons(session, nm, selected = val)
               updateTextInput(session,    nm, value    = val) }
      }, error = function(e) invisible(NULL))
    }
    if (is.character(vals$export_units)) {
      u <- vals$export_units
      updateNumericInput(session, "export_width",  label = paste0("Width (",  u, "):"))
      updateNumericInput(session, "export_height", label = paste0("Height (", u, "):"))
    }
    invisible(NULL)
  }
  observeEvent(input$export_units, {
    u <- input$export_units
    if (!identical(u, rv$units_seen)) {
      w <- isolate(input$export_width); h <- isolate(input$export_height)
      f  <- if (identical(u, "mm")) 25.4 else 1 / 25.4
      dg <- if (identical(u, "mm")) 1 else 3     # 89 mm -> 3.504 in -> 89.0 mm, no drift
      if (is.numeric(w) && is.finite(w)) updateNumericInput(session, "export_width",  value = round(w * f, dg))
      if (is.numeric(h) && is.finite(h)) updateNumericInput(session, "export_height", value = round(h * f, dg))
      rv$units_seen <- u
    }
    updateNumericInput(session, "export_width",  label = paste0("Width (",  u, "):"))
    updateNumericInput(session, "export_height", label = paste0("Height (", u, "):"))
  }, ignoreInit = TRUE)
  
  output$export_size_note <- renderUI({
    sz <- export_size_in()
    if (!all(is.finite(sz))) return(NULL)
    mm <- round(sz * 25.4, 1)
    if (fixed_mode()) {
      pd <- panel_dims_mm("main")
      return(p(style = "font-size:.8em;color:#777;margin-top:-6px;",
               sprintf("Figure size follows the panel (%s x %s mm): %s x %s mm (%s x %s in). PDF/SVG are vector; DPI applies to PNG/TIFF/JPEG.",
                       pd[1], pd[2], mm[1], mm[2], round(sz[1], 2), round(sz[2], 2))))
    }
    p(style = "font-size:.8em;color:#777;margin-top:-6px;",
      sprintf("= %s x %s mm (%s x %s in). PDF/SVG are vector; DPI applies to PNG/TIFF/JPEG.",
              mm[1], mm[2], round(sz[1], 2), round(sz[2], 2)))
  })
  
  # Preview helpers (v2.11): the main figure and the analysis figure share the
  # pixel-canvas / true-export-size preview mechanics.
  # size_fun: figure size in inches (the export size unless the figure has its own,
  # as the composed figure does); canvas mode keeps its width and scales the height.
  # v2.14: the zoom that makes the figure fit the main panel (rounded down to 0.25, 0.5-4), or the
  # slider's value when auto-fit is off or the page width is not known (tests, first render).
  preview_zoom_value <- function(width_in) {
    pw <- input$main_panel_width
    if (isTRUE(input$preview_zoom_auto %||% TRUE) && is.numeric(pw) && length(pw) == 1 && is.finite(pw) &&
        pw > 0 && is.finite(width_in) && width_in > 0) {
      pw <- min(max(pw, 200), 8000)                      # v2.14.1: a sane page width
      min(max(floor((pw - 24) / (width_in * 96) * 4) / 4, 0.5), 4)
    } else {
      z <- input$preview_zoom %||% 2
      if (!is.numeric(z) || length(z) != 1 || !is.finite(z)) z <- 2
      min(max(z, 0.5), 4)
    }
  }
  preview_container <- function(plot_id, img_id, size_fun = export_size_in) {
    sz <- size_fun()
    if (identical(input$preview_mode, "wysiwyg")) {
      z <- preview_zoom_value(sz[1])
      imageOutput(img_id,
                  width  = paste0(round(sz[1] * 96 * z), "px"),
                  height = paste0(round(sz[2] * 96 * z), "px"))
    } else {
      # canvas mode: the pixel width is the user's, the height keeps the figure's aspect ratio
      pwid <- min(max(input$plot_width %||% 820, 200), 4000)   # v2.14.1: a sane canvas width
      plotOutput(plot_id,
                 height = paste0(round(pwid * sz[2] / sz[1]), "px"),
                 width  = paste0(pwid,  "px"))
    }
  }
  # True-size preview (v2.10): the figure goes through the same ggsave path as
  # the download, at the export's physical size, and is shown at `zoom` x.
  # Text and line weights therefore appear at their real size relative to the
  # figure -- what Inkscape will open.
  register_preview <- function(plot_id, img_id, plot_fun, size_fun = export_size_in) {
    output[[plot_id]] <- renderPlot({
      plot_inputs_debounced()   # take debounced dependency
      # v2.14.1: a failing render shows the error figure instead of ending the session
      obj <- tryCatch(plot_fun(), error = function(e) {
        if (inherits(e, "shiny.silent.error")) stop(e)
        plot_error_figure(friendly_error_message(e))
      })
      # a fixed-panel figure is a gtable: draw it (print() of a gtable only prints its layout)
      if (inherits(obj, "gtable")) { grid::grid.newpage(); grid::grid.draw(obj); invisible(NULL) } else obj
    }, res = 96)
    output[[img_id]] <- renderImage({
      plot_inputs_debounced()
      sz <- size_fun(); z <- preview_zoom_value(sz[1])
      pr <- session$clientData$pixelratio %||% 1
      pr <- if (is.numeric(pr) && length(pr) == 1 && is.finite(pr)) min(max(pr, 1), 3) else 1
      f  <- tempfile(fileext = ".png")
      dpi <- raster_dpi(sz[1], sz[2], 96 * z * pr)
      tryCatch(
        ggsave(f, plot = plot_fun(), device = export_device("png"),
               width = sz[1], height = sz[2], units = "in", dpi = dpi, bg = "white", limitsize = FALSE),
        error = function(e) {
          if (inherits(e, "shiny.silent.error")) stop(e)
          msg <- friendly_error_message(e)
          guard_notify("render_error", paste("Preview failed:", msg), "error")
          # the placeholder figure, written small, in place of the failed preview
          ggsave(f, plot = plot_error_figure(msg), device = export_device("png"),
                 width = min(sz[1], 6), height = min(sz[2], 4), units = "in", dpi = 96, bg = "white")
        })
      list(src = f, contentType = "image/png",
           width = round(sz[1] * 96 * z), height = round(sz[2] * 96 * z),
           alt = "Figure preview at export size")
    }, deleteFile = TRUE)
  }
  # v2.14: stable, sized file names (no timestamp) so panels can be assembled from them:
  # lysis_curve_<label>_<W>x<H><unit>.<ext>, <type>_<label>_<W>x<H><unit>.<ext>, figure_composed_<W>x<H>mm.<ext>
  file_label <- function(x, fallback) {
    x <- gsub("[^A-Za-z0-9._-]+", "_", trimws(x %||% ""))
    if (nzchar(x)) x else fallback
  }
  export_size_tag <- function(sz_in = NULL) {
    if (fixed_mode() || !is.null(sz_in)) {     # fixed panels: the computed figure size, always in mm
      mm <- (sz_in %||% export_size_in()) * 25.4
      return(sprintf("%dx%dmm", as.integer(round(mm[1])), as.integer(round(mm[2]))))
    }
    u <- input$export_units %||% "mm"; w <- input$export_width %||% 89; h <- input$export_height %||% 65
    if (identical(u, "mm")) sprintf("%dx%dmm", as.integer(round(w)), as.integer(round(h)))
    else sprintf("%.1fx%.1fin", w, h)
  }
  main_download_name <- function()
    paste0("lysis_curve_", file_label(input$panel_label, "main"), "_", export_size_tag(), ".", input$export_format %||% "svg")
  analysis_download_name <- function() {
    ty <- input$analysis_type %||% "figure"
    paste0(ty, "_", file_label(input$analysis_panel_label, ty), "_", export_size_tag(if (fixed_mode()) analysis_size_in()), ".", input$export_format %||% "svg")
  }
  composed_download_name <- function() {
    sz <- tryCatch({ i <- compose_info(); c(i$width_mm, i$height_mm) }, error = function(e) NULL)
    paste0("figure_composed", if (!is.null(sz)) sprintf("_%dx%dmm", as.integer(round(sz[1])), as.integer(round(sz[2]))),
           ".", input$export_format %||% "svg")
  }

  # ── Figure size bar (v2.14) ──────────────────────────────────────────────────
  # One line above the tabs, always visible: what the export will be, from the live inputs, and a
  # warning (with a one-click fix) when the state is not journal-standard.
  output$figure_standard_bar <- renderUI({
    fx <- fixed_mode()
    if (fx) {                       # v2.15: the figure size is the computed one, in mm
      szm <- export_size_in() * 25.4
      u <- "mm"; w <- szm[1]; h <- szm[2]
    } else {
      u  <- input$export_units %||% "mm"
      w  <- input$export_width; h <- input$export_height
    }
    if (!is.numeric(w) || !is.numeric(h) || !is.finite(w) || !is.finite(h)) return(NULL)
    w_mm <- if (identical(u, "mm")) w else w * 25.4
    names_short <- function(jp) sub(" family$", "", sub(" \\(.*$", "", jp$label))
    fams <- JOURNAL_PRESETS[setdiff(names(JOURNAL_PRESETS), "custom")]
    fam  <- input$journal_family %||% "nature"
    fams <- fams[unique(c(if (fam %in% names(fams)) fam, names(fams)))]   # the chosen family first
    jname <- NULL
    col_names <- c(single = "single column", onehalf = "1.5 column", double = "double column")
    for (jp in fams) {
      for (key in names(col_names)) {
        val <- jp[[key]]
        if (is.finite(val %||% NA) && abs(w_mm - val) < 0.6) { jname <- paste(names_short(jp), col_names[[key]]); break }
      }
      if (!is.null(jname)) break
    }
    if (is.null(jname)) jname <- "custom size"
    size_txt <- if (identical(u, "mm")) sprintf("%s \u00d7 %s mm", round(w), round(h))
                else sprintf("%s \u00d7 %s in", round(w, 1), round(h, 1))
    fnt  <- input$font_family %||% "sans"
    al   <- input$axis_label_font_size %||% 7; at <- input$axis_text_font_size %||% 6
    dl   <- lw2pt(input$line_thickness %||% 0.35); xl <- input$axis_line_pt %||% 0.5
    g    <- function(x) format(round(x, 2), nsmall = 0)
    fit <- NULL
    if (fx) {
      pd  <- panel_dims_mm("main")
      jp0 <- JOURNAL_PRESETS[[fam]]
      fit <- if (!is.null(jp0)) figure_fit_text(w_mm, jp0)
      fit_txt <- if (is.null(fit)) NULL
                 else if (fit$fits) paste0(fit$text, " (", names_short(jp0), ")")
                 else paste0(fit$text, " (", names_short(jp0), "): smaller panel or legend inside")
      txt <- sprintf("Panel %s \u00d7 %s mm \u00b7 figure %s \u00d7 %s mm%s \u00b7 %s %s / %s pt \u00b7 lines %s / %s pt \u00b7 %s",
                     round(pd[1], 1), round(pd[2], 1), round(w), round(h),
                     if (!is.null(fit_txt)) paste0(" \u00b7 ", fit_txt) else "",
                     fnt, g(al), g(at), g(dl), g(xl), toupper(input$export_format %||% "svg"))
    } else
    txt  <- sprintf("Export: %s \u00b7 %s \u00b7 %s %s / %s pt \u00b7 lines %s / %s pt \u00b7 %s",
                    size_txt, jname, fnt, g(al), g(at), g(dl), g(xl), toupper(input$export_format %||% "svg"))
    standard <- (fx || identical(u, "mm")) && al <= 8 && al >= 5 && at <= 8 && at >= 5 &&
      fnt %in% c("Arial", "Helvetica", "sans") && dl <= 1.5
    comp <- NULL
    if (identical(input$main_tabs, "Compose figure") && has_patchwork) {
      i <- tryCatch(compose_info(), error = function(e) NULL)
      if (!is.null(i)) comp <- sprintf("  |  Composed: %s \u00d7 %s mm, %s column%s", round(i$width_mm), round(i$height_mm),
                                       i$ncol, if (i$ncol == 1) "" else "s")
    }
    too_wide <- fx && !is.null(fit) && !fit$fits
    if (standard && !too_wide) div(class = "fig-bar", txt, comp)
    else if (standard) div(class = "fig-bar warn", txt, comp)
    else div(class = "fig-bar warn", txt, comp, " \u2014 not journal-standard \u2014 ",
             actionButton("apply_journal_preset_bar", "Apply journal preset", class = "btn btn-default btn-xs"))
  })

  output$plot_container <- renderUI(preview_container("od_plot", "od_plot_img"))
  register_preview("od_plot", "od_plot_img", main_plot_obj)
  
  output$downloadPlot <- downloadHandler(
    filename = function() main_download_name(),
    content  = function(file) {
      sz   <- export_size_in()
      fmt  <- input$export_format
      args <- list(filename = file, plot = main_plot_obj(), device = export_device(fmt),
                   width = sz[1], height = sz[2],
                   units = "in", dpi = export_dpi_for(fmt, sz[1], sz[2]), bg = "white", limitsize = FALSE)
      # Journals require lossless compression for TIFF; the arg is only valid
      # for the tiff device so it must be added conditionally.
      if (identical(fmt, "tiff")) args$compression <- "lzw"
      do.call(ggsave, args)
    }
  )
  
  # ── Journal preset (v2.10) ───────────────────────────────────────────────────
  journal_target_width <- reactive({
    journal_width_mm(input$journal_family, input$journal_columns,
                     input$journal_custom_width_mm %||% 89)
  })
  
  output$journal_preset_summary <- renderUI({
    w  <- journal_target_width(); h <- input$journal_height_mm %||% 65
    if (!is.numeric(w) || !is.finite(w))
      return(p(style = "font-size:.85em;color:#856404;margin:6px 0;",
               "This journal publishes no 1.5-column width; choose single or double."))
    if (!is.numeric(h) || !is.finite(h)) return(NULL)
    jp   <- JOURNAL_PRESETS[[input$journal_family %||% "nature"]]
    warn <- if (!is.null(jp$max_h) && is.finite(jp$max_h) && h > jp$max_h)
      sprintf(" Height exceeds this journal's maximum (%s mm).", jp$max_h) else ""
    p(style = "font-size:.85em;margin:6px 0;",
      strong(sprintf("Figure: %s x %s mm", round(w, 1), h)),
      sprintf(" (%s x %s in).", round(w / 25.4, 2), round(h / 25.4, 2)),
      if (nzchar(warn)) span(style = "color:#856404;", warn))
  })
  
  apply_journal_preset_now <- function() {
    w <- journal_target_width(); h <- input$journal_height_mm %||% 65
    if (!is.numeric(w) || !is.finite(w) || !is.numeric(h) || !is.finite(h)) return()
    x_range <- tryCatch({
      pd <- prepare_plot_data(); diff(range(pd$time, na.rm = TRUE))
    }, error = function(e) NULL)
    apply_settings(journal_preset_settings(w, h, input$journal_panel_label %||% "", x_range))
    showNotification(sprintf(paste0("Journal preset applied: %s x %s mm, Arial 6-8 pt, ",
                                    "SVG export. Preview switched to true export size."),
                             round(w, 1), h),
                     type = "message", duration = 8)
  }
  observeEvent(input$apply_journal_preset,     apply_journal_preset_now())
  observeEvent(input$apply_journal_preset_bar, apply_journal_preset_now())   # the size bar's button
  # the pre-v2.14 look (10 x 8 in, 20 pt, PDF, canvas preview), as one click
  observeEvent(input$apply_legacy_look, {
    apply_settings(legacy_preset_settings())
    showNotification("Legacy look applied (the v2.9 defaults: 10 x 8 in, 20 pt, PDF).", type = "message", duration = 6)
  })
  
  # ── Figure legend text (v2.10) ──────────────────────────────────────────────
  # Journals put the measure, n and the error definition in the legend, not on
  # the artwork. Drafted from the plot state + Experiment Notebook; edit freely.
  # v2.17: one function, shared by the box, the reports, the notebook download and the figure bundle.
  # Returns the drafted paragraph, or "" when there is nothing to describe yet.
  figure_legend_string <- function() {
    if (is.null(rv$data) || is.null(input$selected_samples)) return("")
    pd <- tryCatch(prepare_plot_data(), error = function(e) NULL)
    if (is.null(pd) || nrow(pd) == 0) return("")
    aes_v <- resolve_aesthetics(input$selected_samples)
    nb    <- function(id) { v <- input[[id]]
                            if (is.null(v) || !nzchar(trimws(as.character(v)))) NULL else trimws(as.character(v)) }
    pl    <- nb("panel_label"); host <- nb("nb_host_strain"); phage <- nb("nb_phage_plasmid")
    moi   <- nb("nb_moi");      med  <- nb("nb_media");       temp  <- nb("nb_temperature")
    ylab  <- nb("y_axis_label") %||% "optical density"
    s1 <- paste0(if (!is.null(pl)) paste0("(", pl, ") ") else "",
                 "Growth and lysis of ", host %||% "the host strain",
                 if (!is.null(phage)) paste0(" infected with ", phage) else "",
                 if (!is.null(moi))   paste0(" (MOI ", moi, ")") else "",
                 ", measured as ", ylab, " over time",
                 if (!is.null(med))   paste0(" in ", med) else "",
                 if (!is.null(temp))  paste0(" at ", temp, " °C") else "", ".")
    s2 <- paste0("Curves: ", paste(unname(aes_v$leg_labels[input$selected_samples]), collapse = "; "), ".")
    n_rng <- suppressWarnings(range(pd$n, na.rm = TRUE))
    n_txt <- if (all(is.finite(n_rng)) && n_rng[1] == n_rng[2]) paste0("n = ", n_rng[1])
             else if (all(is.finite(n_rng))) paste0("n = ", n_rng[1], "–", n_rng[2])
             else "n not available"
    err  <- switch(input$error_type %||% "none",
                   sd = "s.d.", sem = "s.e.m.", ci95 = "the 95% confidence interval", NULL)
    mult <- input$error_multiplier %||% 1
    s3 <- paste0(if (isTRUE(input$show_points)) "Symbols and lines" else "Lines",
                 " show the mean of ", n_txt, " replicates",
                 if (!is.null(nb("nb_replicate"))) paste0(" (", nb("nb_replicate"), ")") else "",
                 if (!is.null(err)) paste0(
                   "; ", if (identical(input$error_display_mode, "shadow")) "shaded bands" else "error bars",
                   " indicate ", if (is.numeric(mult) && mult != 1) paste0(mult, " × ") else "", err,
                   if (any(pd$n < 2, na.rm = TRUE)) " (omitted where n = 1)" else "") else "")
    s3 <- paste0(sub("\\.$", "", s3), ".")   # "s.e.m." already ends with a period
    if (isTRUE(input$show_replicate_points)) s3 <- paste(s3, "Open circles show the individual replicate values.")
    s4 <- if (identical(input$y_scale_type, "log")) "The y axis is log10-scaled." else NULL
    blank <- input$blank_od %||% 0
    s5 <- if (is.numeric(blank) && is.finite(blank) && blank != 0)
      sprintf("Readings are blank-corrected (−%s).", blank) else NULL
    s6 <- if (isTRUE(input$enable_time_filter)) "Only the displayed time window was analysed." else NULL
    paste(c(s1, s2, s3, s4, s5, s6), collapse = " ")
  }
  output$figure_legend_text <- renderText({
    txt <- figure_legend_string()
    if (nzchar(txt)) txt else "Load data and select samples to draft a legend."
  })

  # ── Analysis figures (v2.11) ─────────────────────────────────────────────────
  # v2.12.4: lysis-onset threshold as a fraction (input is a percentage; default 5 %)
  lysis_threshold_current <- function() {
    p <- input$metrics_lysis_threshold_pct %||% 5
    if (!is.numeric(p) || !is.finite(p)) p <- 5
    min(max(p, 1), 50) / 100
  }
  replicate_metric_rows_current <- function() {
    compute_replicate_metric_rows(prepare_replicate_data(),
                                  smooth_window      = input$metrics_smooth_window %||% 5,
                                  peak_smooth_window = input$metrics_peak_smooth_window %||% 1,
                                  ref_sample         = input$metrics_ref_sample,
                                  peak_mode          = input$metrics_peak_mode %||% "global",
                                  lysis_threshold    = lysis_threshold_current())
  }
  analysis_stat_metric <- function() {
    if (identical(input$analysis_type, "dose_response")) input$analysis_dose_metric %||% "lysis_time"
    else (input$analysis_metrics %||% "lysis_time")[1]
  }
  analysis_stats_current <- function() {
    compare_replicate_metric(replicate_metric_rows_current(), analysis_stat_metric(),
                             ref_sample = input$metrics_ref_sample,
                             method     = input$stats_method %||% "welch")
  }
  
  # The analysis figures show the SAME error measure as the main figure
  # (Error Bars panel): SD, SEM or 95% CI of the per-replicate values; none
  # draws no whiskers. Returns a function(values) -> half-width.
  error_half_width <- function() {
    et <- input$error_type %||% "sem"; em <- input$error_multiplier %||% 1
    function(v) {
      v <- v[is.finite(v)]; n <- length(v)
      if (et == "none" || n < 2) return(NA_real_)
      s <- sd(v)
      em * switch(et, sd = s, sem = s / sqrt(n), ci95 = qt(0.975, df = n - 1) * s / sqrt(n), s)
    }
  }
  
  # One builder for every analysis figure. Each returns a ggplot that then gets
  # figure_theme_layers(), so it matches the main figure exactly.
  build_analysis_plot <- function(type) {
    dose_no_legend <- FALSE
    leg_xy <- NULL      # plotted x/y of this figure, for legend_position "inside-auto"
    leg_log_y <- FALSE
    ehw    <- error_half_width()
    eb_col <- input$error_bar_color %||% "sample"
    samps  <- input$selected_samples
    aes_v  <- resolve_aesthetics(samps)
    cols   <- setNames(aes_v$colors, samps); labs_v <- aes_v$leg_labels
    lts    <- aes_v$line_types;              shp    <- aes_v$shapes
    tunit  <- time_unit_from_label(input$x_axis_label)
    lw     <- input$line_thickness %||% 1
    ptsz   <- input$shape_size %||% 3
    tag    <- trimws(input$analysis_panel_label %||% "")
    ref    <- input$metrics_ref_sample %||% ""
    base_scales <- list(scale_color_manual(   values = cols, labels = labs_v, name = NULL),
                        scale_fill_manual(    values = cols, labels = labs_v, name = NULL),
                        scale_linetype_manual(values = lts,  labels = labs_v, name = NULL))
    p <- switch(type,
      metric_summary = {
        rm   <- replicate_metric_rows_current()
        mets <- intersect(input$analysis_metrics %||% c("lysis_time", "od_drop"), names(rm))
        if (is.null(rm) || !length(mets)) stop("No per-replicate metrics for the selected samples.")
        long <- do.call(rbind, lapply(mets, function(m)
          data.frame(sample = rm$sample, replicate = rm$replicate, metric = m, value = rm[[m]],
                     stringsAsFactors = FALSE)))
        long <- long[is.finite(long$value), , drop = FALSE]
        if (nrow(long) == 0) stop("The selected metrics are NA for every replicate (a relative metric needs a reference sample).")
        long$metric <- factor(long$metric, levels = mets,
                              labels = vapply(mets, metric_axis_label, "", tunit = tunit, ylab = input$y_axis_label %||% "OD"))
        long$sample <- factor(long$sample, levels = sample_levels(samps), labels = unname(labs_v[sample_levels(samps)]))
        x_guide <- if (as.numeric(input$axis_text_angle %||% "0") != 0) waiver()
          else if (nlevels(long$sample) >= 5 || max(nchar(levels(long$sample))) > 16)
            guide_axis(angle = 45)      # 5+ samples (or names too long for two rows in a 44 mm facet): angle, not stagger
          else if (max(nchar(levels(long$sample))) > 6) guide_axis(n.dodge = 2)   # 3-4 long names: two staggered rows
          else waiver()
        summ <- long %>% group_by(metric, sample) %>%
          summarise(mean = mean(value), sd = ehw(value), n = n(), .groups = "drop")
        g <- ggplot(long, aes(x = sample, y = value))
        if (eb_col == "sample")
          g <- g + geom_errorbar(data = summ, aes(x = sample, ymin = mean - sd, ymax = mean + sd, color = sample),
                                 width = 0.3, linewidth = pt2lw(0.5), inherit.aes = FALSE, na.rm = TRUE)
        else
          g <- g + geom_errorbar(data = summ, aes(x = sample, ymin = mean - sd, ymax = mean + sd),
                                 color = eb_col, width = 0.3, linewidth = pt2lw(0.5), inherit.aes = FALSE, na.rm = TRUE)
        g <- g +
          geom_crossbar(data = summ, aes(x = sample, y = mean, ymin = mean, ymax = mean, color = sample),
                        width = 0.5, linewidth = pt2lw(0.75), inherit.aes = FALSE) +
          geom_point(aes(color = sample), shape = 21, fill = "white", size = ptsz * 0.6,
                     stroke = pt2lw(0.5), position = position_jitter(width = 0.12, height = 0, seed = 1)) +
          facet_wrap(~ metric, scales = "free_y", labeller = label_wrap_gen(width = 18)) +
          scale_color_manual(values = setNames(unname(cols), unname(labs_v[names(cols)])), guide = "none") +
          scale_x_discrete(guide = x_guide) +
          scale_y_continuous(expand = expansion(mult = c(0.08, 0.18))) +   # headroom for marks
          labs(x = NULL, y = NULL)
        if (isTRUE(input$stats_mark) && nzchar(ref) && ref %in% rm$sample) {
          marks <- do.call(rbind, lapply(mets, function(m) {
            st <- compare_replicate_metric(rm, m, ref_sample = ref, method = input$stats_method %||% "welch")
            if (is.null(st$pairwise)) return(NULL)
            lab_m <- metric_axis_label(m, tunit = tunit, ylab = input$y_axis_label %||% "OD")
            top <- long[long$metric == lab_m, , drop = FALSE] %>% group_by(sample) %>%
              summarise(y = max(value), .groups = "drop")
            pwd <- st$pairwise[st$pairwise$versus == ref, , drop = FALSE]   # never show all-pairs results as "vs reference"
            if (nrow(pwd) == 0) return(NULL)
            pwd$sample <- factor(unname(labs_v[pwd$sample]), levels = levels(long$sample))
            d <- merge(pwd[, c("sample", "stars")], top, by = "sample")
            d$metric <- factor(lab_m, levels = levels(long$metric)); d
          }))
          if (!is.null(marks) && nrow(marks))
            g <- g + geom_text(data = marks, aes(x = sample, y = y, label = stars), vjust = -0.6,
                               size = (input$legend_font_size %||% 14) / ggplot2::.pt, inherit.aes = FALSE)
        }
        g
      },
      growth_rate = {
        gr <- compute_growth_rate_curves(prepare_replicate_data(), window = input$metrics_smooth_window %||% 5)
        if (is.null(gr)) stop("Growth rate needs at least 3 positive OD points per curve.")
        gr$variable <- factor(gr$variable, levels = sample_levels(samps))
        # band = the main figure's error measure (compute_growth_rate_curves gives SD and n)
        et <- input$error_type %||% "sem"; em <- input$error_multiplier %||% 1
        gr$mu_sd <- if (et == "none") NA_real_ else em * switch(et,
          sd = gr$mu_sd, sem = gr$mu_sd / sqrt(gr$n),
          ci95 = qt(0.975, df = pmax(gr$n - 1, 1)) * gr$mu_sd / sqrt(gr$n), gr$mu_sd)
        leg_xy <- list(x = gr$time, y = gr$mu_mean, g = gr$variable)
        g <- ggplot(gr, aes(x = time, y = mu_mean, color = variable, linetype = variable, group = variable)) +
          geom_hline(yintercept = 0, linewidth = pt2lw(0.5), color = "grey50", linetype = "dashed")
        if (any(is.finite(gr$mu_sd)))
          g <- g + geom_ribbon(aes(ymin = mu_mean - mu_sd, ymax = mu_mean + mu_sd, fill = variable),
                               alpha = 0.15, color = NA, linetype = "solid", na.rm = TRUE)
        g + geom_line(linewidth = lw) + base_scales +
          labs(x = input$x_axis_label,
               y = paste0("Specific growth rate, μ (", tunit, "⁻¹)"))
      },
      normalized = {
        mode <- input$analysis_norm_mode %||% "reference"
        nd   <- compute_normalized_curves(prepare_plot_data(), ref, mode)
        if (is.null(nd)) stop(if (mode == "reference")
          "Pick the uninfected reference sample under Stats & Reports first." else "No data.")
        nd$variable <- factor(nd$variable, levels = sample_levels(samps))
        ref_lab <- if (mode == "reference") unname(labs_v[ref]) else "t = 0"
        leg_xy <- list(x = nd$time, y = nd$ratio, g = nd$variable)
        ggplot(nd, aes(x = time, y = ratio, color = variable, linetype = variable,
                       shape = variable, group = variable)) +
          geom_hline(yintercept = 1, linewidth = pt2lw(0.5), color = "grey50", linetype = "dashed") +
          geom_line(linewidth = lw) + geom_point(size = ptsz * 0.7) +
          base_scales + scale_shape_manual(values = shp, labels = labs_v, name = NULL) +
          labs(x = input$x_axis_label, y = axis_y_label(paste0(input$y_axis_label, " relative to ", ref_lab)))
      },
      replicate_overlay = {
        rp <- prepare_replicate_data(); pd <- prepare_plot_data()
        rp$variable <- factor(rp$variable, levels = sample_levels(samps)); pd$variable <- factor(pd$variable, levels = sample_levels(samps))
        rp$curve <- interaction(rp$variable, rp$replicate)
        leg_xy <- list(x = rp$time, y = rp$value, g = rp$curve); leg_log_y <- identical(input$y_scale_type, "log")
        g <- ggplot() +
          geom_line(data = rp, aes(x = time, y = value, color = variable, group = curve),
                    linewidth = lw * 0.5, alpha = 0.45) +
          geom_line(data = pd, aes(x = time, y = mean_value, color = variable, linetype = variable,
                                   group = variable), linewidth = lw * 1.3) +
          base_scales + labs(x = input$x_axis_label, y = axis_y_label(input$y_axis_label))
        if (identical(input$y_scale_type, "log")) g + scale_y_log10(labels = y_tick_labels(), oob = y_oob_fn()) else g
      },
      dose_response = {
        rm     <- replicate_metric_rows_current()
        metric <- input$analysis_dose_metric %||% "lysis_time"
        if (is.null(rm) || !metric %in% names(rm)) stop("Metric not available for these samples.")
        moi    <- moi_map(samps, input$moi_override)
        rm$moi <- unname(moi[rm$sample])
        d <- rm[is.finite(rm$moi) & rm$moi > 0 & is.finite(rm[[metric]]),
                c("sample", "replicate", "moi", metric), drop = FALSE]
        names(d)[4] <- "value"
        if (nrow(d) == 0) stop("No sample has a positive MOI for this metric. Put the MOI in the sample name ('Phage MOI 10') or in the MOI box as 'sample = MOI'.")
        if (length(unique(d$moi)) < 2)
          stop("Dose-response needs at least two MOI levels; this selection has one.")
        summ <- d %>% group_by(sample, moi) %>%
          summarise(mean = mean(value), sd = ehw(value), .groups = "drop")
        d$sample <- factor(d$sample, levels = sample_levels(samps)); summ$sample <- factor(summ$sample, levels = sample_levels(samps))
        dose_no_legend <- all(grepl("MOI", unname(labs_v[unique(as.character(d$sample))]), ignore.case = TRUE))
        # One series per phage: the legend label with its MOI token removed
        # ("Phage A MOI 10" -> "Phage A"); one connecting line per series.
        smp_lab <- unname(labs_v[as.character(summ$sample)])
        series  <- trimws(gsub("(?i)\\s*MOI[[:space:]_:=]*[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?", "", smp_lab, perl = TRUE))
        series[!nzchar(series)] <- "Phage"
        summ$series <- series
        summ <- summ[order(summ$series, summ$moi), , drop = FALSE]
        series_levels <- unique(summ$series[order(match(as.character(summ$sample), samps))])
        # series colour = colour of the series' first sample (selection order)
        series_col <- vapply(series_levels, function(sr) {
          smp <- samps[samps %in% as.character(summ$sample[summ$series == sr])][1]
          unname(cols[smp]) }, "")
        leg_xy <- list(x = log10(summ$moi), y = summ$mean)
        g <- ggplot(summ, aes(x = moi, y = mean))
        for (sr in series_levels) {
          ss <- summ[summ$series == sr, , drop = FALSE]
          g <- g + geom_line(data = ss, aes(x = moi, y = mean, group = series), colour = series_col[[sr]],
                             linewidth = pt2lw(0.5), inherit.aes = FALSE)
          if (length(series_levels) > 1)   # end label: readable without a key
            g <- g + geom_text(data = ss[which.max(ss$moi), , drop = FALSE], aes(x = moi, y = mean),
                               label = paste0("  ", sr), colour = series_col[[sr]], hjust = 0,
                               size = (input$legend_font_size %||% 14) / ggplot2::.pt, inherit.aes = FALSE)
        }
        g <- g +
          geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, color = sample),
                        width = 0, linewidth = pt2lw(0.5), na.rm = TRUE) +
          geom_point(data = d, aes(x = moi, y = value, color = sample), shape = 21, fill = "white",
                     size = ptsz * 0.6, stroke = pt2lw(0.5)) +
          geom_point(aes(color = sample, shape = sample), size = ptsz) +
          base_scales + scale_shape_manual(values = shp, labels = labs_v, name = NULL) +
          scale_x_log10(labels = scales::label_number(drop0trailing = TRUE, big.mark = ""),
                        expand = if (length(series_levels) > 1) expansion(mult = c(0.05, 0.35)) else waiver()) +
          labs(x = "MOI", y = axis_y_label(metric_axis_label(metric, tunit = tunit, ylab = input$y_axis_label %||% "OD")))
        g
      },
      landmarks = {
        # The computed metrics drawn where they come from on each mean curve:
        # lag end, the mu_max window centre, peak, lysis onset, half of peak,
        # trough. Makes every number in the Curve Metrics CSV auditable by eye.
        pd <- prepare_plot_data(); m <- curve_metrics_current()
        if (is.null(m) || nrow(m) == 0) stop("No metrics available.")
        win <- input$metrics_smooth_window %||% 5
        lm_df <- do.call(rbind, lapply(seq_len(nrow(m)), function(i) {
          s <- m$sample[i]; g <- pd[pd$variable == s, , drop = FALSE]; g <- g[order(g$time), ]
          od_at <- function(t) if (is.finite(t) && nrow(g)) g$mean_value[which.min(abs(g$time - t))] else NA_real_
          # mu_max window centre: same rolling regression as the metric
          t_mu <- NA_real_
          if (nrow(g) >= max(3, min(win, nrow(g)))) {
            pos <- g$mean_value > 0; lo <- rep(NA_real_, nrow(g)); lo[pos] <- log(g$mean_value[pos])
            sl  <- tryCatch(rolling_slope(g$time, lo, min(win, nrow(g))), error = function(e) NULL)
            if (!is.null(sl) && any(is.finite(sl) & sl > 1e-10)) t_mu <- g$time[which.max(ifelse(is.finite(sl), sl, -Inf))]
          }
          pk <- if (is.finite(m$time_max_od[i])) which.min(abs(g$time - m$time_max_od[i])) else which.max(g$mean_value)   # same peak as the metrics (peak mode)
          t_trough <- if (pk < nrow(g)) g$time[pk + which.min(g$mean_value[(pk + 1):nrow(g)])] else NA_real_
          data.frame(variable = s,
                     landmark = c("Lag end", "\u03bcmax", "Peak", "Lysis onset", "Half of peak", "Trough"),
                     time = c(m$lag_phase[i], t_mu, m$time_max_od[i], m$lysis_time[i], m$t_half_max_decline[i], t_trough),
                     od   = c(od_at(m$lag_phase[i]), od_at(t_mu), m$max_od[i], od_at(m$lysis_time[i]),
                              if (is.finite(m$t_half_max_decline[i])) m$max_od[i] / 2 else NA_real_, od_at(t_trough)),
                     stringsAsFactors = FALSE)
        }))
        lm_df <- lm_df[is.finite(lm_df$time) & is.finite(lm_df$od), , drop = FALSE]
        lm_df$landmark <- factor(lm_df$landmark, levels = c("Lag end", "\u03bcmax", "Peak", "Lysis onset", "Half of peak", "Trough"))
        lm_df$variable <- factor(lm_df$variable, levels = sample_levels(samps)); pd$variable <- factor(pd$variable, levels = sample_levels(samps))
        if (identical(input$y_scale_type, "log")) lm_df <- lm_df[lm_df$od > 0, , drop = FALSE]
        g <- ggplot() +
          geom_line(data = pd, aes(x = time, y = mean_value, color = variable, linetype = variable, group = variable),
                    linewidth = lw) +
          geom_point(data = lm_df, aes(x = time, y = od, shape = landmark, fill = variable, color = variable),
                    size = ptsz * 1.1, stroke = pt2lw(0.6)) +
          base_scales +
          scale_shape_manual(values = c("Lag end" = 1, "\u03bcmax" = 2, "Peak" = 16, "Lysis onset" = 25,
                                        "Half of peak" = 4, "Trough" = 15),
                             name = NULL, drop = TRUE) +
          guides(fill = "none", color = guide_legend(order = 1, override.aes = list(shape = NA)),
                 linetype = guide_legend(order = 1),
                 shape = guide_legend(order = 2, override.aes = list(color = "black", fill = "grey60"))) +
          labs(x = input$x_axis_label, y = axis_y_label(input$y_axis_label))
        if (identical(input$y_scale_type, "log")) g + scale_y_log10(labels = y_tick_labels(), oob = y_oob_fn()) else g
      },
      heatmap = {
        pd <- prepare_plot_data()
        pd$variable <- factor(pd$variable, levels = rev(sample_levels(samps)), labels = rev(unname(labs_v[sample_levels(samps)])))
        logit <- identical(input$y_scale_type, "log")
        pd$fillv <- if (logit) log10(pmax(pd$mean_value, 1e-6)) else pd$mean_value
        ggplot(pd, aes(x = time, y = variable, fill = fillv)) + geom_tile() +
          scale_fill_viridis_c(name = axis_y_label(if (logit) paste0("log10 ", input$y_axis_label) else input$y_axis_label)) +
          labs(x = input$x_axis_label, y = NULL)
      },
      stop("Unknown analysis figure type"))
    p <- p + figure_theme_layers(xy = leg_xy, n_entries = length(samps), log_y = leg_log_y,
                                 label_chars = max(nchar(unname(labs_v)), 1))
    if (type == "metric_summary")   # facet strips: axis-title size, no grey box (theme default is 0.8 x base)
      p <- p + theme(legend.position = "none",
                     strip.text = element_text(size = input$axis_label_font_size, face = "plain"),
                     strip.background = element_blank())
    if (type == "heatmap")        p <- p + theme(legend.position = "right")
    if (type == "dose_response" && isTRUE(dose_no_legend))
      p <- p + theme(legend.position = "none")   # x axis already says it; the key would hide points
    if (type == "landmarks")   # nine legend entries: outside, never over the curves
      p <- p + theme(legend.position = "right", legend.background = element_rect(fill = NA, color = NA))
    if (isTRUE(input$custom_aspect_ratio)) p <- p + theme(aspect.ratio = 1 / input$aspect_ratio)
    p + coord_cartesian(clip = "off") + labs(tag = if (nzchar(tag)) tag else NULL)
  }
  
  generate_analysis_plot <- function(type = input$analysis_type %||% "metric_summary") {
    req(rv$data, input$selected_samples)
    tryCatch(build_analysis_plot(type), error = function(e) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste(strwrap(paste("Figure not available:", conditionMessage(e)), width = 40), collapse = "\n"),
                 color = "grey30", size = 2.6, hjust = 0.5) +
        theme_void()
    })
  }
  output$analysis_container <- renderUI(preview_container("analysis_plot", "analysis_img", analysis_size_in))
  register_preview("analysis_plot", "analysis_img", analysis_plot_obj, analysis_size_in)
  
  output$downloadAnalysisPlot <- downloadHandler(
    filename = function() analysis_download_name(),
    content  = function(file) {
      sz   <- analysis_size_in(); fmt <- input$export_format
      args <- list(filename = file, plot = analysis_plot_obj(), device = export_device(fmt),
                   width = sz[1], height = sz[2], units = "in", dpi = export_dpi_for(fmt, sz[1], sz[2]), bg = "white", limitsize = FALSE)
      if (identical(fmt, "tiff")) args$compression <- "lzw"
      do.call(ggsave, args)
    }
  )
  
  # ── Figure composer (v2.13) ──────────────────────────────────────────────────
  # compose_figure() is the one place that lays panels out: tools/render_gallery.R
  # calls it too, so the gallery, the app and the reports agree. Each panel's own
  # tag is removed, panels are free()d from alignment (an outside legend would
  # otherwise squeeze the panels beside it), and patchwork adds a, b, c tags in the
  # journal's panel-label style.
  compose_figure <- function(panels, ncol = 2, tag_size = 8, font = "Arial", tag_case = "a", tags_on = TRUE) {
    if (!has_patchwork) stop("The figure composer needs the 'patchwork' package: install.packages('patchwork')")
    panels <- lapply(panels, function(p) patchwork::free(p + labs(tag = NULL)))
    w <- patchwork::wrap_plots(panels, ncol = ncol)
    if (!isTRUE(tags_on)) return(w)    # v2.16: "Add panel letters" off: no a, b, c
    w + patchwork::plot_annotation(tag_levels = tag_case) &
      theme(plot.tag = element_text(size = tag_size, face = "bold", family = font))
  }
  is_placeholder_plot <- function(p) {
    lab <- tryCatch(p$layers[[1]]$aes_params$label, error = function(e) NULL)
    is.character(lab) && length(lab) == 1 && grepl("^(Plot error|Figure not available)", lab)
  }
  compose_width_mm_value <- function() {
    jp  <- JOURNAL_PRESETS[[input$journal_family %||% "nature"]]
    w   <- switch(input$compose_width %||% "double", double = jp$double, single = jp$single,
                  custom = input$compose_width_mm %||% 183)
    if (!is.numeric(w) || !is.finite(w)) w <- input$compose_width_mm %||% 183
    w
  }
  # Choice labels follow the journal family chosen under Journal Figure Preset.
  observeEvent(input$journal_family, {
    jp  <- JOURNAL_PRESETS[[input$journal_family %||% "nature"]]
    nm  <- sub(" \\(.*$", "", jp$label)
    ch  <- c(if (is.finite(jp$double %||% NA)) setNames("double", sprintf("Double column of %s (%s mm)", nm, jp$double)),
             if (is.finite(jp$single %||% NA)) setNames("single", sprintf("Single column of %s (%s mm)", nm, jp$single)),
             "Custom" = "custom")
    sel <- isolate(input$compose_width %||% "double")
    updateSelectInput(session, "compose_width", choices = ch,
                      selected = if (sel %in% ch) sel else ch[[1]])
  }, ignoreInit = TRUE)

  # The composed figure and its geometry, built once per input change and shared by
  # the preview, the note, the download and the reports. Panels whose figure is
  # "not available" (the placeholder) are skipped and listed in $skipped.
  compose_info <- reactive({
    req(rv$data, input$selected_samples)
    sel   <- input$compose_panels %||% character(0)
    types <- unname(COMPOSE_PANEL_CHOICES[COMPOSE_PANEL_CHOICES %in% sel])   # composed in checkbox order
    built <- lapply(setNames(types, types), function(ty) {
      p <- tryCatch(if (identical(ty, "main")) generate_plot() else generate_analysis_plot(ty),
                    error = function(e) NULL)
      if (is.null(p) || is_placeholder_plot(p)) NULL else p
    })
    ok      <- !vapply(built, is.null, logical(1))
    skipped <- names(COMPOSE_PANEL_CHOICES)[match(types[!ok], COMPOSE_PANEL_CHOICES)]
    plots   <- built[ok]; n <- length(plots)
    ncol    <- min(as.integer(input$compose_ncol %||% "2"), max(n, 1L))
    rows    <- if (n) ceiling(n / ncol) else 0L
    w_mm    <- compose_width_mm_value()
    h_mm    <- max(rows, 1L) * (input$compose_panel_height_mm %||% 60)
    fig <- if (n >= 1 && fixed_mode()) {
      # v2.15: every panel keeps its fixed panel size; the grid is as wide / tall as its cells need
      items <- lapply(names(plots), function(ty) fixed_figure(plots[[ty]] + labs(tag = NULL), ty))
      g <- compose_fixed_figure(items, ncol = ncol, tag_size = input$compose_tag_size %||% 8,
                                font = input$font_family %||% "Arial", tag_case = input$compose_tag_case %||% "a",
                                tags_on = isTRUE(input$compose_tags %||% TRUE))
      w_mm <- attr(g, "size_mm")[1]; h_mm <- attr(g, "size_mm")[2]
      g
    } else if (n >= 1)
      compose_figure(plots, ncol = ncol, tag_size = input$compose_tag_size %||% 8,
                     font = input$font_family %||% "Arial", tag_case = input$compose_tag_case %||% "a",
                     tags_on = isTRUE(input$compose_tags %||% TRUE))
    else NULL
    list(plot = fig, n = n, ncol = ncol, rows = rows, skipped = skipped,
         types = names(plots), width_mm = w_mm, height_mm = h_mm)
  })
  compose_size_in <- function() { i <- compose_info(); c(i$width_mm, i$height_mm) / 25.4 }
  compose_plot_current <- function() {
    i <- compose_info()
    if (is.null(i$plot))
      ggplot() + annotate("text", x = 0.5, y = 0.5, size = 3.5, color = "grey30",
                          label = "Tick at least one panel (no selected figure is available).") + theme_void()
    else i$plot
  }
  output$compose_container <- renderUI(preview_container("compose_plot", "compose_img", compose_size_in))
  register_preview("compose_plot", "compose_img", compose_plot_current, compose_size_in)

  output$compose_note <- renderUI({
    if (!has_patchwork)
      return(p(style = "font-size:.85em;color:#a94442;", "The figure composer needs the 'patchwork' package."))
    i <- compose_info()
    jp <- JOURNAL_PRESETS[[input$journal_family %||% "nature"]]
    warn <- if (!is.null(jp$max_h) && is.finite(jp$max_h) && i$height_mm > jp$max_h)
      sprintf(" Height exceeds this journal's maximum (%s mm).", jp$max_h) else ""
    p(style = "font-size:.85em;margin:6px 0;",
      strong(sprintf("Figure: %s x %s mm", round(i$width_mm, 1), round(i$height_mm, 1))),
      sprintf(" (%s panel%s in %s row%s of %s column%s).", i$n, if (i$n == 1) "" else "s",
              i$rows, if (i$rows == 1) "" else "s", i$ncol, if (i$ncol == 1) "" else "s"),
      if (length(i$skipped))
        span(style = "color:#856404;", " Left out (not available for this selection): ",
             paste(i$skipped, collapse = "; "), "."),
      if (nzchar(warn)) span(style = "color:#856404;", warn))
  })

  output$downloadComposedFigure <- downloadHandler(
    filename = function() composed_download_name(),
    content  = function(file) {
      i <- compose_info()
      if (is.null(i$plot)) stop("No panel to compose: tick at least one available figure.")
      if (length(i$skipped))
        tryCatch(showNotification(paste0("Left out of the composed figure (not available): ",
                                         paste(i$skipped, collapse = "; ")),
                                  type = "warning", duration = 10), error = function(e) invisible(NULL))
      fmt  <- input$export_format
      args <- list(filename = file, plot = i$plot, device = export_device(fmt),
                   width = i$width_mm, height = i$height_mm, units = "mm",
                   dpi = export_dpi_for(fmt, i$width_mm / 25.4, i$height_mm / 25.4), bg = "white", limitsize = FALSE)
      if (identical(fmt, "tiff")) args$compression <- "lzw"
      do.call(ggsave, args)
    }
  )

  # ── Statistics tables ───────────────────────────────────────────────────────
  fmt_stats_table <- fmt_stats_df   # same formatting in the app and the reports
  stats_notes_ui <- function(st) {
    notes <- st$warnings
    if (!is.null(st$pairwise))
      notes <- c(paste0("Metric: ", metric_label(st$metric), ". ",
                        if (nzchar(input$metrics_ref_sample %||% "")) paste0("Each sample vs reference '", input$metrics_ref_sample, "'. ") else "All pairs. ",
                        "p values Holm-adjusted; Hedges' g = standardized mean difference; ",
                        "95% CI of the difference (Welch)."), notes)
    else notes <- c("No comparison possible yet: load data with 2+ replicates per sample.", notes)
    div(lapply(notes, function(n) div(class = "stats-note", n)))
  }
  # v2.16: one builder per table, used by the Analysis tab and the Statistics tab (own output ids)
  stats_notes_build    <- function() { req(rv$data); stats_notes_ui(analysis_stats_current()) }
  stats_pairwise_build <- function() { req(rv$data); fmt_stats_table(analysis_stats_current()$pairwise) }
  stats_omnibus_build  <- function() { req(rv$data); fmt_stats_table(analysis_stats_current()$omnibus) }
  stats_posthoc_build  <- function() { req(rv$data); fmt_stats_table(analysis_stats_current()$posthoc) }
  output$analysis_stats_notes   <- renderUI(stats_notes_build())
  output$analysis_stats_table   <- renderTable(stats_pairwise_build(), striped = TRUE, spacing = "xs")
  output$analysis_stats_omnibus <- renderTable(stats_omnibus_build(),  striped = TRUE, spacing = "xs")
  output$analysis_stats_posthoc <- renderTable(stats_posthoc_build(),  striped = TRUE, spacing = "xs")
  analysis_stats_csv_name <- function() paste0("replicate_stats_", analysis_stat_metric(), "_",
                                               format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  analysis_stats_csv_content <- function(file) {
    st <- analysis_stats_current()
    con <- file(file, open = "w", encoding = "UTF-8"); on.exit(close(con))
    writeLines(paste0("# metric,", st$metric, " ; method,", input$stats_method %||% "welch",
                      " ; reference,", input$metrics_ref_sample %||% ""), con)
    for (w in st$warnings) writeLines(paste0("# ", w), con)
    if (!is.null(st$pairwise)) { writeLines("# pairwise", con); write.csv(st$pairwise, con, row.names = FALSE) }
    if (!is.null(st$omnibus))  { writeLines("# omnibus",  con); write.csv(st$omnibus,  con, row.names = FALSE) }
    if (!is.null(st$posthoc))  { writeLines("# posthoc (Tukey HSD)", con); write.csv(st$posthoc, con, row.names = FALSE) }
  }
  output$downloadAnalysisStatsCSV     <- downloadHandler(filename = analysis_stats_csv_name, content = analysis_stats_csv_content)
  output$downloadAnalysisStatsCSV_tab <- downloadHandler(filename = analysis_stats_csv_name, content = analysis_stats_csv_content)
  
  # ── Dashboard (v2.11) ───────────────────────────────────────────────────────
  output$dash_cards <- renderUI({
    if (is.null(rv$data) || is.null(input$selected_samples))
      return(div(class = "dash-empty", "Load a CSV or the demo data to populate the dashboard."))
    pd <- tryCatch(prepare_plot_data(), error = function(e) NULL)
    if (is.null(pd) || nrow(pd) == 0) return(NULL)
    n_rng <- range(pd$n, na.rm = TRUE); t_rng <- range(pd$time, na.rm = TRUE)
    sz <- export_size_in()
    card <- function(title, value, sub = NULL)
      div(class = "dash-card", div(class = "dash-card-title", title),
          div(class = "dash-card-value", value), if (!is.null(sub)) div(class = "dash-card-sub", sub))
    div(class = "dash-cards",
        card("Samples", length(input$selected_samples), rv$data_source %||% ""),
        card("Replicates", if (n_rng[1] == n_rng[2]) n_rng[1] else paste0(n_rng[1], "–", n_rng[2]),
             "per sample and time point"),
        card("Time points", length(unique(pd$time)),
             paste0(t_rng[1], " – ", t_rng[2], " ", time_unit_from_label(input$x_axis_label))),
        card("Error measure", toupper(input$error_type %||% "none"),
             if (identical(input$error_display_mode, "shadow")) "shaded band" else "error bars"),
        card("Reference", if (nzchar(input$metrics_ref_sample %||% "")) input$metrics_ref_sample else "none set",
             "for infection metrics and statistics"),
        card("Figure", sprintf("%s × %s mm", round(sz[1] * 25.4), round(sz[2] * 25.4)),
             paste0(input$font_family, " ", input$axis_label_font_size, "/", input$axis_text_font_size,
                    " pt, ", toupper(input$export_format %||% "pdf"))))
  })
  
  output$dash_metrics_table <- renderTable({
    req(rv$data, input$selected_samples)
    m  <- tryCatch(curve_metrics_current(),     error = function(e) NULL); req(m)
    rm <- tryCatch(replicate_metrics_current(), error = function(e) NULL)
    cols <- c(sample = "Sample", max_od = "Peak OD", time_max_od = "t (peak)", mu_max = "μmax",
              doubling_time = "Doubling", lag_phase = "Lag", lysis_time = "Lysis onset",
              od_drop = "OD drop", lysis_efficiency = "Lysis eff.", residual_od = "Residual OD",
              recovery_slope = "Recovery", infection_strength = "Infection strength")
    cols <- cols[names(cols) %in% names(m)]
    out  <- m[, names(cols), drop = FALSE]
    f3   <- function(x) ifelse(is.finite(x), formatC(signif(x, 3), format = "g"), "—")
    for (cc in setdiff(names(out), "sample")) out[[cc]] <- f3(out[[cc]])
    if (!is.null(rm)) {
      idx <- match(m$sample, rm$sample)
      for (cc in intersect(c("lysis_time", "od_drop"), names(out))) {
        sdv <- rm[[paste0(cc, "_sd")]][idx]
        # "—" alone when the mean-curve value is NA; "value ± sd" only for a finite value
        out[[cc]] <- ifelse(is.finite(sdv) & is.finite(m[[cc]]),
                            paste0(out[[cc]], " ± ", formatC(signif(sdv, 2), format = "g")), out[[cc]])
      }
    }
    names(out) <- unname(cols)
    out
  }, striped = TRUE, spacing = "xs")
  
  # Dashboard panels are a working view, not an export: they keep the user's
  # colours, line weights and legend placement but use fixed screen-sized text,
  # so they read the same whether the figure is set to 20 pt or to 6 pt.
  dash_theme <- theme(
    text         = element_text(size = 10),  plot.title   = element_text(size = 11),
    axis.title   = element_text(size = 10),  axis.text    = element_text(size = 8),
    legend.text  = element_text(size = 8),   legend.title = element_text(size = 8),
    strip.text   = element_text(size = 9),   plot.tag     = element_blank(),
    legend.key.size = unit(4, "mm"))
  dash_plot <- function(fun) renderPlot({
    req(rv$data, input$selected_samples)
    if (dashboard_too_big()) { plot.new(); return(invisible(NULL)) }   # see dash_size_note
    p <- tryCatch(fun(), error = function(e) NULL)
    if (is.null(p)) { plot.new(); return(invisible(NULL)) }
    tryCatch(print(p + labs(tag = NULL, caption = NULL) + dash_theme),
             error = function(e) {
               if (inherits(e, "shiny.silent.error")) stop(e)
               plot.new(); text(0.5, 0.5, paste(strwrap(friendly_error_message(e), 40), collapse = "\n"), col = "red", cex = 0.8)
             })
  }, res = 96, bg = "white")
  output$dash_fig_main    <- dash_plot(function() generate_plot())
  output$dash_fig_rate    <- dash_plot(function() generate_analysis_plot("growth_rate"))
  output$dash_fig_norm    <- dash_plot(function()
    if (nzchar(input$metrics_ref_sample %||% "")) generate_analysis_plot("normalized")
    else generate_analysis_plot("replicate_overlay"))
  output$dash_fig_norm_title <- renderText(
    if (nzchar(input$metrics_ref_sample %||% "")) "Normalized OD (vs reference)" else "Replicate overlay (set a reference for normalized OD)")
  output$dash_fig_metrics <- dash_plot(function() generate_analysis_plot("metric_summary"))
  output$dash_notes <- renderUI({ req(rv$data); stats_notes_ui(analysis_stats_current()) })
  output$dash_stats <- renderTable({ req(rv$data); fmt_stats_table(analysis_stats_current()$pairwise) },
                                   striped = TRUE, spacing = "xs")
  output$dash_notebook <- renderUI({
    fields <- c(nb_exp_id = "Experiment", nb_experimenter = "Experimenter", nb_project = "Project",
                nb_exp_type = "Type", nb_host_strain = "Host", nb_phage_plasmid = "Phage / plasmid",
                nb_moi = "MOI", nb_replicate = "Replicates", nb_passage = "Passage #", nb_media = "Medium",
                nb_temperature = "Temperature (°C)", nb_time_inf = "Time of infection",
                nb_inducer_conc = "Inducer concentration", nb_other_condition = "Other condition",
                nb_observations = "Observations")
    vals <- vapply(names(fields), function(id) { v <- input[[id]]; if (is.null(v)) "" else trimws(as.character(v)) }, "")
    keep <- nzchar(vals)
    if (!any(keep)) return(p(style = "color:#6c757d;font-size:.85em;",
                             "Fill in the Experiment Notebook (sidebar) and it is summarised here and in the reports."))
    tags$table(class = "table table-condensed", style = "width:auto;font-size:.85em;",
               lapply(which(keep), function(i) tags$tr(tags$th(fields[i]), tags$td(vals[i]))))
  })

  # ── Stats & Reports ──────────────────────────────────────────────────────────

  # Summary stats table with reader-friendly column names
  stats_table_export <- function() {
    pd <- prepare_plot_data()
    data.frame(time      = pd$time,
               sample    = pd$variable,
               mean      = pd$mean_value,
               sd        = pd$sd_value,
               n         = pd$n,
               sem       = pd$sem_value,
               ci95_half_width = pd$ci95_value,
               stringsAsFactors = FALSE)
  }

  # v2.16: each CSV has one filename / content function, shared by the sidebar button and the
  # Statistics tab button (a Shiny output id can only be bound once, so each has a second id).
  stats_csv_name   <- function() paste0("summary_stats_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  stats_csv_content <- function(file) {
    req(rv$data, input$selected_samples)
    write.csv(stats_table_export(), file, row.names = FALSE)
  }
  output$downloadStatsCSV     <- downloadHandler(filename = stats_csv_name, content = stats_csv_content)
  output$downloadStatsCSV_tab <- downloadHandler(filename = stats_csv_name, content = stats_csv_content)

  curve_metrics_current <- function() {
    compute_curve_metrics(prepare_plot_data(),
                          smooth_window       = input$metrics_smooth_window %||% 5,
                          ref_sample          = input$metrics_ref_sample,
                          peak_smooth_window  = input$metrics_peak_smooth_window %||% 1,
                          peak_mode           = input$metrics_peak_mode %||% "global",
                          lysis_threshold     = lysis_threshold_current())
  }

  # KC-2: warn when any sample has non-positive OD readings, since those
  # points are excluded from the mu_max/doubling_time/lag_phase windows
  # (see compute_curve_metrics) rather than silently clamped.
  output$nonpositive_od_warning_ui <- renderUI({
    m <- tryCatch(curve_metrics_current(), error = function(e) NULL)
    if (is.null(m) || !"n_nonpositive_od" %in% colnames(m)) return(NULL)
    bad <- m[m$n_nonpositive_od > 0, c("sample", "n_nonpositive_od"), drop = FALSE]
    if (nrow(bad) == 0) return(NULL)
    txt <- paste0(bad$sample, " (", bad$n_nonpositive_od, ")", collapse = ", ")
    div(style = "background:#fff3cd;border:1px solid #ffe69c;border-radius:4px;
                 padding:6px 8px;margin:4px 0 8px;font-size:.78em;color:#664d03;",
        HTML(paste0("<b>Non-positive OD detected</b> (samples, count of points): ", txt,
                    ". Those time points are excluded from the &mu;max/doubling-time/",
                    "lag-phase windows rather than clamped &mdash; a common cause is a ",
                    "small negative reading after blank subtraction.")))
  })

  # v2.17: data quality flags (CEILING / NEGATIVE / GAP / FLAT_REFERENCE) for the current selection:
  # a compact list in the sidebar, on the Dashboard and in the reports. NULL when they cannot be computed.
  qc_current <- function() {
    if (is.null(rv$data) || !length(input$selected_samples)) return(NULL)
    tryCatch(qc_flags(prepare_replicate_data(), prepare_plot_data(),
                      ref_sample = input$metrics_ref_sample %||% "",
                      ceiling    = input$qc_od_ceiling %||% 2.5),
             error = function(e) NULL)
  }
  qc_flags_ui_build <- function() {
    q <- qc_current()
    if (is.null(q) || nrow(q) == 0) return(NULL)
    div(style = "background:#fff3cd;border:1px solid #ffe69c;border-radius:4px;padding:6px 8px;margin:4px 0 8px;font-size:.78em;color:#664d03;",
        tags$b(paste0("Data quality flags (", nrow(q), ")")),
        tags$ul(style = "margin:4px 0 0;padding-left:16px;",
                lapply(seq_len(nrow(q)), function(i)
                  tags$li(tags$b(q$flag[i]), " ", q$sample[i], ": ", q$detail[i]))))
  }
  output$qc_flags_ui         <- renderUI(qc_flags_ui_build())
  output$qc_flags_ui_metrics <- renderUI(qc_flags_ui_build())
  output$dash_qc             <- renderUI(qc_flags_ui_build())

  metrics_csv_name <- function() paste0("curve_metrics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  metrics_csv_content <- function(file) {
    req(rv$data, input$selected_samples)
    write.csv(curve_metrics_current(), file, row.names = FALSE)
  }
  output$downloadMetricsCSV     <- downloadHandler(filename = metrics_csv_name, content = metrics_csv_content)
  output$downloadMetricsCSV_tab <- downloadHandler(filename = metrics_csv_name, content = metrics_csv_content)

  # KC-4: per-replicate metrics (mean +/- SD across replicates), additive
  # alongside curve_metrics_current() above.
  replicate_metrics_current <- function() {
    compute_replicate_metrics(prepare_replicate_data(),
                              smooth_window      = input$metrics_smooth_window %||% 5,
                              peak_smooth_window = input$metrics_peak_smooth_window %||% 1,
                              peak_mode          = input$metrics_peak_mode %||% "global",
                              lysis_threshold    = lysis_threshold_current())
  }

  repmetrics_csv_name <- function() paste0("replicate_metrics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  repmetrics_csv_content <- function(file) {
    req(rv$data, input$selected_samples)
    write.csv(replicate_metrics_current(), file, row.names = FALSE)
  }
  output$downloadReplicateMetricsCSV     <- downloadHandler(filename = repmetrics_csv_name, content = repmetrics_csv_content)
  output$downloadReplicateMetricsCSV_tab <- downloadHandler(filename = repmetrics_csv_name, content = repmetrics_csv_content)

  # ── Statistics tab (v2.16) ───────────────────────────────────────────────────
  # The descriptive statistics and metrics, reviewable and downloadable without the sidebar; the
  # replicate comparison uses the same builders as the Analysis tab (own output ids). Outputs in a
  # tab that is not showing are not computed, so this costs nothing until it is opened.
  STATS_TAB_MAX_ROWS <- 2000L
  stats_tab_summary_df <- function() { req(rv$data, input$selected_samples); stats_table_export() }
  output$stats_tab_summary_note <- renderUI({
    n <- nrow(stats_tab_summary_df())
    if (n > STATS_TAB_MAX_ROWS)
      p(class = "stats-note", sprintf("The first %s of %s rows are shown; the CSV has all of them.",
                                      STATS_TAB_MAX_ROWS, format(n, big.mark = ",")))
  })
  if (has_DT) {
    output$stats_tab_summary <- DT::renderDT({
      df <- utils::head(stats_tab_summary_df(), STATS_TAB_MAX_ROWS)
      df$n <- as.integer(df$n)
      DT::formatRound(DT::datatable(df, rownames = FALSE, options = list(pageLength = 25, searching = TRUE, scrollX = TRUE)),
                      intersect(c("mean", "sd", "sem", "ci95_half_width"), names(df)), digits = 4)
    }, server = FALSE)   # rows (capped) travel with the widget: the search box filters in the browser
  } else {
    output$stats_tab_summary <- renderTable({
      df <- utils::head(stats_tab_summary_df(), STATS_TAB_MAX_ROWS)
      df$n <- as.integer(df$n); df$time <- as.character(df$time)   # whole numbers, not 15.0000
      df
    }, striped = TRUE, digits = 4, spacing = "xs")
  }
  # 4 significant figures, "NA" for missing; whole numbers stay whole
  tab_sig <- function(df, sig = 4) {
    for (cc in names(df)) if (is.numeric(df[[cc]]))
      df[[cc]] <- vapply(df[[cc]], function(v) if (is.finite(v)) format(signif(v, sig), trim = TRUE, scientific = FALSE) else "NA", "")
    df
  }
  output$stats_tab_metrics <- renderTable({
    req(rv$data, input$selected_samples); m <- curve_metrics_current(); req(m); tab_sig(m)
  }, striped = TRUE, spacing = "xs")
  output$stats_tab_metric_defs <- renderTable({
    req(rv$data, input$selected_samples); m <- curve_metrics_current(); req(m)
    keys <- intersect(names(METRIC_DEFINITIONS), colnames(m))
    data.frame(Metric = keys, Definition = unname(METRIC_DEFINITIONS[keys]), stringsAsFactors = FALSE)
  }, striped = TRUE, spacing = "xs")
  output$stats_tab_repmetrics <- renderTable({
    req(rv$data, input$selected_samples); m <- replicate_metrics_current(); req(m); tab_sig(m)
  }, striped = TRUE, spacing = "xs")
  output$stats_tab_notes    <- renderUI(stats_notes_build())
  output$stats_tab_pairwise <- renderTable(stats_pairwise_build(), striped = TRUE, spacing = "xs")
  output$stats_tab_omnibus  <- renderTable(stats_omnibus_build(),  striped = TRUE, spacing = "xs")
  output$stats_tab_posthoc  <- renderTable(stats_posthoc_build(),  striped = TRUE, spacing = "xs")

  # Methods paragraph drafted from the current settings: what was measured, how the mean and the
  # error statistic were computed, how the metrics were defined and which tests were run. Plain text
  # (the y label keeps its "A550"); the user edits it and the reports include what is in the box.
  draft_methods_text <- function() {
    if (is.null(rv$data) || !length(input$selected_samples)) return("")
    pd <- tryCatch(prepare_plot_data(), error = function(e) NULL)
    if (is.null(pd) || nrow(pd) == 0) return("")
    nb <- function(id) { v <- input[[id]]; if (is.null(v) || !nzchar(trimws(as.character(v)))) NULL else trimws(as.character(v)) }
    samps <- input$selected_samples
    ylab  <- nb("y_axis_label") %||% "optical density"
    tunit <- time_unit_from_label(input$x_axis_label)
    n_rng <- suppressWarnings(range(pd$n, na.rm = TRUE))
    n_txt <- if (!all(is.finite(n_rng))) "replicate"
             else if (n_rng[1] == n_rng[2]) paste0(n_rng[1], " replicate", if (n_rng[1] == 1) "" else "s")
             else paste0(n_rng[1], "–", n_rng[2], " replicates")
    et <- input$error_type %||% "sem"; em <- input$error_multiplier %||% 1
    mult <- if (is.numeric(em) && is.finite(em) && em != 1) paste0(em, " × ") else ""
    shown <- if (identical(input$error_display_mode, "shadow")) "shaded bands" else "error bars"
    err <- switch(et,
      sd   = paste0("Mean curves are shown with ", shown, " of ", mult, "the standard deviation (SD) of the replicates."),
      sem  = paste0("Mean curves are shown with ", shown, " of ", mult, "the standard error of the mean (SEM = SD / √n)."),
      ci95 = paste0("Mean curves are shown with ", shown, " of ", mult, "the 95% confidence interval of the mean (t₀.₉₇₅,ₙ₋₁ × SEM)."),
      "Mean curves are shown without an error statistic.")
    s1 <- paste0(
      if (!is.null(nb("nb_host_strain")))
        paste0("Growth of ", nb("nb_host_strain"),
               if (!is.null(nb("nb_phage_plasmid"))) paste0(" infected with ", nb("nb_phage_plasmid")) else "",
               if (!is.null(nb("nb_moi"))) paste0(" (MOI ", nb("nb_moi"), ")") else "",
               if (!is.null(nb("nb_media"))) paste0(" in ", nb("nb_media")) else "",
               if (!is.null(nb("nb_temperature"))) paste0(" at ", nb("nb_temperature"), " °C") else "",
               " was followed as ", ylab, " over time (", tunit, ").")
      else paste0("Culture density was measured as ", ylab, " over time (", tunit, ")."),
      " ", length(samps), " sample", if (length(samps) == 1) "" else "s",
      " (", if (length(samps) <= 8) paste(samps, collapse = ", ") else paste0(paste(samps[1:8], collapse = ", "), ", ..."),
      ") were measured in ", n_txt, " each.")
    blank <- input$blank_od %||% 0
    s2 <- paste0(
      if (is.numeric(blank) && is.finite(blank) && blank != 0) paste0("Readings were blank-corrected by subtracting ", blank, ". ") else "",
      if (isTRUE(input$enable_time_filter)) "Only the displayed time window was analysed. " else "",
      "At each time point the mean of the replicates was plotted. ", err)
    win <- input$metrics_smooth_window %||% 5
    pw  <- input$metrics_peak_smooth_window %||% 1
    thr <- lysis_threshold_current() * 100
    first_local <- identical(input$metrics_peak_mode %||% "global", "first_local")
    s3 <- paste0(
      "Curve metrics were computed from each sample's mean curve. The maximum specific growth rate (μmax) is the largest ",
      "slope of ln(", ylab, ") against time in a centred rolling-regression window of ", win, " points, and the doubling time is ln 2 / μmax. ",
      "The peak was taken as ",
      if (first_local) "the first maximum followed by a decline of more than 10%" else "the global maximum of the curve",
      if (is.numeric(pw) && pw > 1) paste0(" (curves smoothed with a ", pw, "-point window for peak detection)") else " (no smoothing for peak detection)",
      ", and lysis onset as the first time after the peak at which the value fell more than ", thr, "% below it. ",
      "Per-replicate metrics were computed the same way on each replicate curve and are reported as mean ± SD.")
    ref <- input$metrics_ref_sample %||% ""
    nonpar <- identical(input$stats_method %||% "welch", "nonparametric")
    st_metric <- tryCatch(metric_label(analysis_stat_metric()), error = function(e) "the per-replicate metric")
    s4 <- paste0(
      if (length(samps) < 2) "No statistical comparison was made (a single sample)."
      else paste0(
        if (nzchar(ref) && ref %in% samps) paste0("Each sample was compared with the reference sample '", ref, "'")
        else "All pairs of samples were compared",
        " on the per-replicate metric '", st_metric, "' using ",
        if (nonpar) "the Mann–Whitney U test" else "Welch's t-test (unequal variances)",
        "; p values were adjusted for multiple comparisons by the Holm method, and effect sizes are reported as Hedges' g with the 95% confidence interval of the difference.",
        if (length(samps) >= 3)
          paste0(" Differences among three or more groups were tested with ",
                 if (nonpar) "the Kruskal–Wallis test." else "one-way ANOVA followed by Tukey's HSD.")
        else ""))
    s5 <- paste0("Analysis and figures were produced with the Lysis Curve OD Visualization App v", APP_VERSION,
                 " (R ", R.version$major, ".", R.version$minor, ").")
    paste(c(s1, s2, s3, s4, s5), collapse = " ")
  }
  observeEvent(input$methods_draft, {
    if (is.null(rv$data) || !length(input$selected_samples)) {
      guard_notify("methods_draft", "Load data and select samples first: the methods text is drafted from them.")
      return(invisible(NULL))
    }
    if (nzchar(trimws(input$methods_text %||% "")) && !isTRUE(input$methods_replace)) {
      guard_notify("methods_draft", "The box already has text. Tick 'replace existing text' to overwrite it with a new draft.")
      return(invisible(NULL))
    }
    updateTextAreaInput(session, "methods_text", value = draft_methods_text())
  })
  output$downloadMethodsTxt <- downloadHandler(
    filename = function() paste0("methods_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"),
    content  = function(file) writeLines(enc2utf8(input$methods_text %||% ""), file, useBytes = TRUE)
  )

  # v2.17: every Experiment Notebook field as a named list (label = value), custom keys appended under
  # their own names. Shared by the reports and the two notebook downloads.
  notebook_pairs <- function() {
    nb_pairs <- list(
      "Experiment ID"      = input$nb_exp_id,
      "Date"               = as.character(input$nb_date %||% ""),
      "Experimenter"       = input$nb_experimenter,
      "Project"            = input$nb_project,
      "Lab / Institution"  = input$nb_institution,
      "Experiment type"    = input$nb_exp_type,
      "Host strain(s)"     = input$nb_host_strain,
      "Phage / plasmid"    = input$nb_phage_plasmid,
      "MOI"                = input$nb_moi,
      "Replicate / batch"  = input$nb_replicate,
      "Passage #"          = input$nb_passage,
      "Growth medium"      = input$nb_media,
      "Temperature (°C)"   = input$nb_temperature,
      "Time of infection/induction" = input$nb_time_inf,
      "Inducer"            = input$nb_inducer,
      "Inducer concentration" = input$nb_inducer_conc,
      "Other condition"    = input$nb_other_condition,
      "Observations / results" = input$nb_observations,
      "Issues / anomalies" = input$nb_issues,
      "Next steps"         = input$nb_next_steps,
      "Tags"               = input$nb_tags
    )
    for (i in 1:3) {
      k <- input[[paste0("nb_custom_key", i)]]
      v <- input[[paste0("nb_custom_val", i)]]
      if (nzchar(trimws(k %||% ""))) nb_pairs[[trimws(k)]] <- v %||% ""
    }
    nb_pairs
  }
  # only the non-empty fields, as character
  notebook_filled <- function() {
    nb_pairs <- notebook_pairs()
    keep <- vapply(nb_pairs, function(v) nzchar(trimws(v %||% "")), logical(1))
    vapply(nb_pairs[keep], function(v) trimws(as.character(v)), character(1))
  }

  # ── Notebook downloads (v2.17) ───────────────────────────────────────────────
  # .txt: every filled notebook field as "key: value", then the figure legend text and the methods text.
  # .csv: one row, one column per filled field (custom keys become column names), for pasting into a
  # master experiment spreadsheet.
  notebook_file_stem <- function() {
    id <- gsub("[^A-Za-z0-9_-]+", "_", trimws(input$nb_exp_id %||% ""))
    paste0("notebook", if (nzchar(id)) paste0("_", id) else "", "_", format(Sys.time(), "%Y%m%d"))
  }
  notebook_txt_lines <- function() {
    f <- notebook_filled()
    kv <- vapply(seq_along(f), function(i) paste0(names(f)[i], ": ", gsub("\n", "\n    ", f[[i]])), character(1))
    leg <- trimws(figure_legend_string()); mth <- trimws(input$methods_text %||% "")
    c("EXPERIMENT NOTEBOOK", paste0("Exported ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                    " (Lysis Curve OD Visualization App v", APP_VERSION, ")"), "",
      if (length(kv)) kv else "(no notebook fields filled in)",
      if (nzchar(leg)) c("", "FIGURE LEGEND", leg),
      if (nzchar(mth)) c("", "METHODS", mth))
  }
  output$download_notebook_txt <- downloadHandler(
    filename = function() paste0(notebook_file_stem(), ".txt"),
    content  = function(file) writeLines(enc2utf8(notebook_txt_lines()), file, useBytes = TRUE)
  )
  notebook_row_df <- function() {
    f <- notebook_filled()
    if (!length(f)) return(data.frame(note = "no notebook fields filled in", stringsAsFactors = FALSE))
    as.data.frame(as.list(f), check.names = FALSE, stringsAsFactors = FALSE)
  }
  output$download_notebook_csv <- downloadHandler(
    filename = function() paste0(notebook_file_stem(), ".csv"),
    content  = function(file) utils::write.csv(notebook_row_df(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  # ── Figure bundle (v2.17) ────────────────────────────────────────────────────
  # One archive per figure: the main figure (SVG + PDF at the current size, the same code as the image
  # download), the composed figure (SVG, when two or more panels are selected), the settings, the tidy source
  # data, the summary statistics and curve metrics, the legend and methods text, and a README.
  bundle_size_mm <- function() round(export_size_in() * 25.4)
  bundle_folder_name <- function() { mm <- bundle_size_mm(); sprintf("figure_bundle_%dx%dmm", mm[1], mm[2]) }
  output$downloadFigureBundle <- downloadHandler(
    filename = function() paste0(bundle_folder_name(), ".", bundle_archive_kind()),
    content  = function(file) {
      req(rv$data, input$selected_samples)
      sz   <- export_size_in()
      tmp  <- tempfile("bundle_"); dir.create(tmp)
      on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
      fold <- bundle_folder_name(); dir <- file.path(tmp, fold); dir.create(dir)
      made <- character(0)
      main_obj <- main_plot_obj()
      for (fmt in c("svg", "pdf")) {
        fn <- paste0("figure_main.", fmt)
        ggsave(file.path(dir, fn), plot = main_obj, device = export_device(fmt), width = sz[1], height = sz[2],
               units = "in", bg = "white", limitsize = FALSE)
        made <- c(made, fn)
      }
      comp <- if (has_patchwork) tryCatch(compose_info(), error = function(e) NULL) else NULL
      if (!is.null(comp) && !is.null(comp$plot) && comp$n >= 2) {
        ggsave(file.path(dir, "figure_composed.svg"), plot = comp$plot, device = export_device("svg"),
               width = comp$width_mm, height = comp$height_mm, units = "mm", bg = "white", limitsize = FALSE)
        made <- c(made, "figure_composed.svg")
      }
      write_json(settings_json_list(), file.path(dir, "settings.json"), pretty = TRUE, auto_unbox = TRUE)
      rp <- prepare_replicate_data()
      src <- data.frame(time = rp$time, sample = as.character(rp$variable), replicate = rp$replicate,
                        value = rp$value, stringsAsFactors = FALSE)
      utils::write.csv(src, file.path(dir, "source_data_tidy.csv"), row.names = FALSE)
      utils::write.csv(stats_table_export(), file.path(dir, "summary_statistics.csv"), row.names = FALSE)
      cm <- tryCatch(curve_metrics_current(), error = function(e) NULL)
      if (!is.null(cm)) utils::write.csv(cm, file.path(dir, "curve_metrics.csv"), row.names = FALSE)
      leg <- trimws(figure_legend_string())
      writeLines(enc2utf8(leg), file.path(dir, "figure_legend.txt"), useBytes = TRUE)
      mth <- trimws(input$methods_text %||% "")
      if (nzchar(mth)) writeLines(enc2utf8(mth), file.path(dir, "methods.txt"), useBytes = TRUE)
      mm <- bundle_size_mm()
      files <- c(made, "settings.json", "source_data_tidy.csv", "summary_statistics.csv",
                 if (!is.null(cm)) "curve_metrics.csv", "figure_legend.txt", if (nzchar(mth)) "methods.txt", "README.txt")
      writeLines(enc2utf8(c(
        "FIGURE BUNDLE",
        paste0("Lysis Curve OD Visualization App v", APP_VERSION),
        paste0("Created: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        paste0("Data source: ", rv$data_source %||% "(unknown)"),
        paste0("Samples: ", paste(input$selected_samples, collapse = ", ")),
        sprintf("Figure size: %d x %d mm (%.2f x %.2f in)", mm[1], mm[2], sz[1], sz[2]),
        sprintf("Fonts: %s; title %s pt, axis labels %s pt, axis text %s pt, legend %s pt",
                input$font_family %||% "sans", input$title_font_size %||% "", input$axis_label_font_size %||% "",
                input$axis_text_font_size %||% "", input$legend_font_size %||% ""),
        "",
        "Files:", paste0("  ", files),
        "",
        "settings.json can be loaded back with Load Settings to reproduce the figure's look.")),
        file.path(dir, "README.txt"), useBytes = TRUE)
      write_bundle_archive(file, tmp, fold)
    }
  )

  # Shared metadata for both report formats
  report_meta <- function() {
    samps <- input$selected_samples
    filter_txt <- if (isTRUE(input$enable_time_filter)) {
      rng  <- input$time_filter_range
      excl <- parse_excluded_timepoints(input$exclude_timepoints, rv$all_timepoints)
      paste0("range ", if (!is.null(rng)) paste0(rng[1], "–", rng[2]) else "full",
             if (length(excl)) paste0("; excluded points: ", paste(sort(excl), collapse = ", "))
             else "")
    } else "none (all time points shown)"
    error_txt <- if (input$error_type == "none") "none" else
      paste0(switch(input$error_type, sd = "SD", sem = "SEM", ci95 = "95% CI"),
             if (!is.null(input$error_multiplier) && input$error_multiplier != 1)
               paste0(" × ", input$error_multiplier) else "",
             ", shown as ", if (identical(input$error_display_mode, "shadow"))
               "shaded ribbon" else "error bars")
    settings_df <- data.frame(
      setting = c("X scale", "Y scale", "X label", "Y label", "Palette",
                  "Line thickness", "Points shown", "Point size",
                  "Export size", "Export DPI",
                  "Blank/media OD subtracted",
                  "μmax smoothing window", "Peak-detection smoothing window",
                  "Peak used for lysis metrics", "Lysis-onset threshold (%)", "Reference sample",
                  "R vector memory limit", "Figure raster size"),
      value = c(input$x_scale_type, input$y_scale_type,
                input$x_axis_label, input$y_axis_label, input$color_palette,
                input$line_thickness, input$show_points, input$shape_size,
                paste0(input$export_width, " × ", input$export_height, " ", input$export_units %||% "in"),
                input$export_dpi,
                { b <- input$blank_od %||% 0; if (is.na(b) || b == 0) "none (off)" else b },
                input$metrics_smooth_window %||% 5,
                paste0(input$metrics_peak_smooth_window %||% 1,
                       if (identical(input$metrics_peak_smooth_window %||% 1, 1)) " (off)" else ""),
                if (identical(input$metrics_peak_mode %||% "global", "first_local"))
                  "first peak before a > 10 % decline" else "global maximum",
                input$metrics_lysis_threshold_pct %||% 5,
                if (nzchar(input$metrics_ref_sample %||% "")) input$metrics_ref_sample
                else "none (infection metrics omitted)",
                # v2.14.1 memory diagnostics: the limit R runs under and the pixel sizes used
                tryCatch({
                  mv <- if (exists("mem.maxVSize", mode = "function")) mem.maxVSize() else NA_real_
                  if (!is.finite(mv)) "unlimited (or not reported)" else sprintf("%s MB", format(round(mv), big.mark = ","))
                }, error = function(e) "not available"),
                tryCatch({
                  sz <- export_size_in(); dp <- as.numeric(safe_raster_dpi(sz[1], sz[2], input$export_dpi %||% 300))
                  rp <- as.numeric(safe_raster_dpi(sz[1], sz[2], if (sz[1] < 6) 300 else 150))
                  sprintf("export %s x %s px at %s dpi; report figures %s x %s px at %s dpi",
                          round(sz[1] * dp), round(sz[2] * dp), dp, round(sz[1] * rp), round(sz[2] * rp), rp)
                }, error = function(e) "not available")),
      stringsAsFactors = FALSE)
    # ── Experiment notebook: collect non-empty fields ─────────────────────────
    nb_pairs <- notebook_pairs()
    keep <- vapply(nb_pairs, function(v) nzchar(trimws(v %||% "")), logical(1))
    # Date always has a default; only emit the notebook when a real field is set
    notebook_df <- if (any(keep & names(nb_pairs) != "Date" &
                           names(nb_pairs) != "Experiment type")) {
      data.frame(field = names(nb_pairs)[keep],
                 value = unlist(nb_pairs[keep], use.names = FALSE),
                 stringsAsFactors = FALSE)
    } else NULL

    list(title       = if (nzchar(trimws(input$report_title))) input$report_title
                       else "Lysis Curve Analysis Report",
         author      = input$report_author,
         notes       = input$report_notes,
         methods     = trimws(input$methods_text %||% ""),   # v2.16: edited on the Statistics tab
         legend      = figure_legend_string(),               # v2.17: the Figure Legend Text, under the main figure
         qc_df       = qc_current(),                         # v2.17: QC flags section (NULL when not computable)
         generated   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
         data_file   = rv$data_source %||% "(unknown)",
         format      = if (rv$is_long_format) "long" else
                       paste0("wide (replicate columns pooled by name)"),
         time_col    = rv$time_col %||% "",
         samples_txt = paste(samps, collapse = ", "),
         filter_txt  = filter_txt,
         error_txt   = error_txt,
         settings_df = settings_df,
         notebook_df = notebook_df,
         font_pt     = input$report_font_size %||% 14)
  }

  # Every report PNG (main figure and analysis figures) goes through the same
  # device, export size and dpi rule.
  report_png_b64 <- function(p, size_in = NULL) {
    tmp_png <- tempfile(fileext = ".png")
    on.exit(unlink(tmp_png), add = TRUE)
    sz <- size_in %||% export_size_in()
    ggsave(tmp_png, plot = p, device = export_device("png"),
           width = sz[1], height = sz[2],
           units = "in", dpi = raster_dpi(sz[1], sz[2], if (sz[1] < 6) 300 else 150), bg = "white", limitsize = FALSE)
    base64enc::base64encode(tmp_png)
  }

  # v2.11.1: the analysis figures and replicate statistics for the reports.
  # NULL when the "Include analysis figures and statistics" box is off, so the
  # reports are exactly what they were in v2.11.0. Figure choice mirrors the
  # dashboard: metric summary, growth rate, normalized OD (replicate overlay
  # without a reference sample) and the dose-response only when an MOI is
  # known. A figure that cannot be built is recorded as a note, not as a
  # placeholder image.
  report_analysis_payload <- function(render_png = FALSE) {
    if (!isTRUE(input$report_include_analysis %||% TRUE)) return(NULL)
    samps <- input$selected_samples
    ref   <- input$metrics_ref_sample %||% ""
    tunit <- time_unit_from_label(input$x_axis_label)
    ylab  <- input$y_axis_label %||% "OD"
    mets  <- input$analysis_metrics %||% c("lysis_time", "od_drop")
    moi   <- moi_map(samps, input$moi_override)
    win   <- input$metrics_smooth_window %||% 5
    norm_mode <- input$analysis_norm_mode %||% "reference"
    types <- c("metric_summary", "growth_rate",
               if (nzchar(ref)) "normalized" else "replicate_overlay",
               "landmarks",
               # dose-response only with >= 2 distinct positive MOI levels (v2.12.4); a
               # single level would only produce the "Figure not available" message
               if (length(unique(moi[is.finite(moi) & moi > 0])) >= 2) "dose_response")
    titles <- c(metric_summary    = "Metric summary (replicates, mean ± SD)",
                growth_rate       = "Specific growth rate vs time",
                normalized        = "Normalized OD vs time",
                replicate_overlay = "Replicate overlay",
                landmarks         = "Curve landmarks",
                dose_response     = "MOI dose-response")
    captions <- c(
      metric_summary = paste0(
        "Per-replicate values (open circles), mean (bar) ± SD (whiskers) for ",
        paste(vapply(mets, metric_axis_label, "", tunit = tunit, ylab = ylab), collapse = ", "), ".",
        if (isTRUE(input$stats_mark) && nzchar(ref))
          paste0(" Marks: significance vs reference '", ref, "' (",
                 if (identical(input$stats_method, "nonparametric")) "Mann-Whitney" else "Welch t",
                 ", Holm-adjusted; see Replicate statistics).") else ""),
      growth_rate = paste0(
        "Specific growth rate μ(t) from a centred rolling regression on ln ", ylab,
        " (window ", win, " points), averaged across replicates; the band is ± SD. ",
        "The dashed line marks μ = 0."),
      normalized = if (norm_mode == "t0")
        paste0("Mean ", ylab, " of each sample divided by its own value at t = 0; the dashed line marks a ratio of 1.")
      else paste0("Mean ", ylab, " of each sample divided by the mean ", ylab, " of the reference '", ref,
                  "' at the same time point; the dashed line marks a ratio of 1."),
      replicate_overlay = "Individual replicate curves (thin lines) with the mean curve (thick line) per sample.",
      landmarks = paste0("Mean curves with the computed landmarks drawn where they come from: lag end, centre of the μmax window, ",
                         "peak, lysis onset (> ", lysis_threshold_current() * 100, " % decline from the peak), half of the peak and the trough."),
      dose_response = paste0(
        metric_axis_label(input$analysis_dose_metric %||% "lysis_time", tunit = tunit, ylab = ylab),
        " per replicate (open circles) and mean ± SD (filled symbols) against MOI (log scale)."))
    figs <- lapply(types, function(ty) {
      f <- list(type = ty, title = unname(titles[ty]), caption = unname(captions[ty]))
      p <- tryCatch(build_analysis_plot(ty), error = function(e) e)
      if (inherits(p, "error")) {
        if (ty == "dose_response") return(NULL)   # silently skipped: never embed a placeholder for it
        f$error <- conditionMessage(p); return(f)
      }
      f$plot <- p
      if (fixed_mode()) {          # v2.15: drawn with its fixed panel, at its own computed size (HTML and PDF)
        fg <- fixed_figure(p, ty)
        f$plot <- fg
        if (render_png) f$b64 <- report_png_b64(fg, size_in = size_from_grob(fg, ty))
      } else if (render_png) f$b64 <- report_png_b64(p)
      f
    })
    figs <- Filter(Negate(is.null), figs)
    # v2.13: the composed figure closes the list, when at least two panels are selected
    comp <- if (has_patchwork) tryCatch(compose_info(), error = function(e) NULL) else NULL
    if (!is.null(comp) && comp$n >= 2) {
      f <- list(type = "composed", title = "Composed figure",
                caption = paste0(if (isTRUE(input$compose_tags %||% TRUE)) paste0("Panels a-", letters[comp$n]) else paste0(comp$n, " panels"), " composed in the app at ",
                                 round(comp$width_mm), " x ", round(comp$height_mm), " mm (",
                                 comp$ncol, " column", if (comp$ncol == 1) "" else "s", "): ",
                                 paste(names(COMPOSE_PANEL_CHOICES)[match(comp$types, COMPOSE_PANEL_CHOICES)],
                                       collapse = "; "), "."),
                plot = comp$plot, aspect = comp$height_mm / comp$width_mm)
      if (render_png) f$b64 <- report_png_b64(comp$plot, size_in = c(comp$width_mm, comp$height_mm) / 25.4)
      figs <- c(figs, list(f))
    }
    st <- analysis_stats_current()
    list(figures = figs, stats = st, metric_label = metric_label(st$metric),
         ref_sample = ref, method = input$stats_method %||% "welch")
  }

  # The report bodies live outside the download handlers so the test suites
  # can call them through testServer. `meta` is a parameter so a caller can
  # pin the "Generated" timestamp.
  html_report_string <- function(meta = report_meta()) {
    req(rv$data, input$selected_samples)
    metrics <- curve_metrics_current()
    img_b64 <- report_png_b64(main_plot_obj())
    build_html_report(meta, img_b64, stats_table_export(), metrics,
                      analysis = report_analysis_payload(render_png = TRUE))
  }
  pdf_report_file <- function(file, meta = report_meta()) {
    req(rv$data, input$selected_samples)
    metrics <- curve_metrics_current()
    build_pdf_report(file, meta, main_plot_obj(),
                     stats_table_export(), metrics,
                     width  = max(export_size_in()[1], 8),
                     height = max(export_size_in()[2], 6),
                     analysis = report_analysis_payload(render_png = FALSE))
  }

  if (has_base64enc) {
    output$downloadHTMLReport <- downloadHandler(
      filename = function()
        paste0("lysis_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"),
      content = function(file) writeLines(html_report_string(), file, useBytes = TRUE)
    )
  }

  if (has_gridExtra) {
    output$downloadPDFReport <- downloadHandler(
      filename = function()
        paste0("lysis_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
      content = function(file) pdf_report_file(file)
    )
  }
  
  # ── PowerPoint Export ────────────────────────────────────────────────────────
  # Cumulative build: slide k shows samples 1..k with full colour (no grey mask).
  # Axes stay consistent across slides because pd_all (full dataset) is used for
  # every plot — only the subset of visible samples changes.
  # Each slide has a title text box naming the sample added on that slide.
  # Final slide = all samples, identical to the main plot.
  if (has_officer && has_rvg) {
    output$downloadPPTX <- downloadHandler(
      filename = function()
        paste0("od_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pptx"),
      content = function(file) {
        library(officer); library(rvg)
        req(rv$data, input$selected_samples)
        samps  <- input$selected_samples
        aes_v  <- resolve_aesthetics(samps)
        pd_all <- prepare_plot_data(samples = samps)
        sw     <- max(export_size_in()[1], 4)   # slide width  (inches)
        sh     <- max(export_size_in()[2], 3)   # slide height (inches)
        
        # Title bar: 0.5 in tall at top of slide; plot fills the rest
        title_h  <- 0.5
        plot_top <- title_h
        plot_h   <- sh - title_h
        
        # Font size for title: scale with slide width, min 12 pt
        title_pt <- max(12, round(sw * 2.2))
        
        prs <- read_pptx()
        
        for (k in seq_along(samps)) {
          visible_samps <- samps[1:k]
          new_samp      <- samps[k]   # the sample added on this slide
          
          # Slice aesthetics to only the visible subset (preserving original colours)
          aes_k <- list(
            shapes     = aes_v$shapes[    visible_samps],
            colors     = aes_v$colors[    visible_samps],
            line_types = aes_v$line_types[visible_samps],
            leg_labels = aes_v$leg_labels[visible_samps],
            filled_map = aes_v$filled_map[visible_samps]
          )
          
          # Data: only visible samples, but axes come from pd_all limits via build_plot
          pd_k <- pd_all[pd_all$variable %in% visible_samps, , drop = FALSE]
          
          # highlight_samples = NULL → all drawn samples in full colour, none greyed
          p_k <- build_plot(pd_k, visible_samps, aes_k, highlight_samples = NULL)
          
          prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
          
          # Title text box
          prs <- ph_with(prs,
                         value    = new_samp,
                         location = ph_location(left = 0, top = 0, width = sw, height = title_h))
          
          # Plot area below the title
          prs <- ph_with(prs,
                         dml(ggobj = p_k, bg = "white"),
                         location = ph_location(left = 0, top = plot_top,
                                                width = sw, height = plot_h))
        }
        
        # Final slide: all samples, full colour, no title override
        p_final <- build_plot(pd_all, samps, aes_v, highlight_samples = NULL)
        prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
        prs <- ph_with(prs,
                       dml(ggobj = p_final, bg = "white"),
                       location = ph_location(left = 0, top = 0, width = sw, height = sh))
        
        print(prs, target = file)
      }
    )
  }
  
  # ── GIF Export ───────────────────────────────────────────────────────────────
  # Cumulative build: frame k shows samples 1..k in full colour (no grey mask).
  # Axes stay consistent because pd_all establishes the full data range — only
  # the subset of plotted samples changes each frame.
  # Rendered by ggsave → gifski (no gganimate dependency).
  if (has_gifski) {
    output$downloadGIF <- downloadHandler(
      filename = function()
        paste0("od_anim_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".gif"),
      content = function(file) {
        library(gifski)
        req(rv$data, input$selected_samples)
        samps  <- input$selected_samples
        aes_v  <- resolve_aesthetics(samps)
        pd_all <- prepare_plot_data(samples = samps)
        px_in  <- function(x, lo, def) { x <- suppressWarnings(as.integer(x)); if (!is.finite(x)) def else min(max(x, lo), 4000L) }
        w_px   <- px_in(input$gif_width,  200L, 800L)       # v2.14.1: bounded, NA-safe
        h_px   <- px_in(input$gif_height, 150L, 600L)
        fps    <- max(suppressWarnings(as.numeric(input$gif_fps %||% 1)), 0.2, na.rm = TRUE)
        
        tmp_dir <- tempfile(); dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        
        frame_files <- character(length(samps))
        for (k in seq_along(samps)) {
          visible_samps <- samps[1:k]
          
          # Slice aesthetics to only the visible subset
          aes_k <- list(
            shapes     = aes_v$shapes[    visible_samps],
            colors     = aes_v$colors[    visible_samps],
            line_types = aes_v$line_types[visible_samps],
            leg_labels = aes_v$leg_labels[visible_samps],
            filled_map = aes_v$filled_map[visible_samps]
          )
          
          # Filter data to visible samples; axes scale from pd_all range implicitly
          # via the scale limits set in build_plot using the supplied data
          pd_k <- pd_all[pd_all$variable %in% visible_samps, , drop = FALSE]
          
          p_k   <- build_plot(pd_k, visible_samps, aes_k, highlight_samples = NULL)
          fpath <- file.path(tmp_dir, sprintf("frame_%03d.png", k))
          ggsave(fpath, plot = p_k, device = "png",
                 width = w_px / 96, height = h_px / 96,
                 units = "in", dpi = raster_dpi(w_px / 96, h_px / 96, 96))
          frame_files[k] <- fpath
        }
        
        gifski(frame_files, gif_file = file,
               width = w_px, height = h_px,
               delay = 1 / fps, loop = TRUE)
      }
    )
  }
  
}

shinyApp(ui, server)
