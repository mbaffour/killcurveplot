# ============================================================================
# v2.10 journal-figure export suite — tests the REAL app code:
#  - helpers (pt2lw, journal_width_mm, journal_preset_settings, export_device)
#  - every export format x every font family through build_plot() + ggsave()
#  - SVG is Inkscape-ready (physical size in pt, no textLength, named font)
#  - PDF embeds its fonts (FontFile objects present)
#  - the journal preset produces 6-8 pt text at 89 x 65 mm
#  - DEFAULT output is byte-identical to the pre-v2.10 app (SVG diff)
#  - the figure-legend text and the export-size helper
# Optional (skipped if absent): Inkscape CLI round trip.
# Run from the repository root:  Rscript tests/test_v210_figure_export.R
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
near <- function(a, b, tol = 1e-6) is.finite(a) && abs(a - b) < tol
# generate_plot() swallows build errors into a placeholder with one red label;
# every render below must be a real figure, never that placeholder.
plot_error <- function(p) {
  lab <- tryCatch(p$layers[[1]]$aes_params$label, error = function(e) NULL)
  if (is.character(lab) && any(startsWith(lab, "Plot error"))) lab else NULL
}
real_plot <- function(desc, p) {
  err <- plot_error(p)
  if (!is.null(err)) cat("   ->", err, "\n")
  check(desc, inherits(p, "ggplot") && is.null(err))
}

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.10.0 or later", package_version(env$APP_VERSION) >= "2.10.0")
check("APP_DEFAULTS exists with the UI defaults", is.list(env$APP_DEFAULTS) && identical(env$APP_DEFAULTS$y_scale_type, "log"))
# A bare testServer session has NO input values (no browser sends the widget
# defaults), so every plot test seeds the session with APP_DEFAULTS first.
seed_defaults <- function(session, defaults = env$APP_DEFAULTS) do.call(session$setInputs, defaults)

# Keep every file this suite writes in one temp dir
out_dir <- tempfile("v210_"); dir.create(out_dir)
of <- function(x) file.path(out_dir, x)
quiet <- function(expr) suppressWarnings(suppressMessages(expr))

cat("\n--- pt <-> linewidth conversion ---\n")
check("pt2lw(2.134) ~= 1 linewidth unit", near(env$pt2lw(ggplot2::.pt * 0.75), 1))
check("lw2pt(0.5) ~= 1.07 pt (the old hard-coded axis width)", near(env$lw2pt(0.5), 1.067, 1e-3))
check("pt2lw / lw2pt are inverses", near(env$lw2pt(env$pt2lw(0.75)), 0.75))

cat("\n--- journal width table ---\n")
check("Nature single column = 89 mm",  env$journal_width_mm("nature", "single") == 89)
check("Nature 1.5 column = 120 mm",    env$journal_width_mm("nature", "onehalf") == 120)
check("Nature double column = 183 mm", env$journal_width_mm("nature", "double") == 183)
check("half of a Nature double column = (183 - 5) / 2 mm",
      near(env$journal_width_mm("nature", "half_double"), (183 - 5) / 2))
check("Cell single = 85, ASM single = 84, Science single = 55, PLOS = 132",
      env$journal_width_mm("cell", "single") == 85 && env$journal_width_mm("asm", "single") == 84 &&
      env$journal_width_mm("science", "single") == 55 && env$journal_width_mm("plos", "single") == 132)
check("custom family returns the custom width", env$journal_width_mm("custom", "double", 100) == 100)
check("ASM / PLOS have no 1.5-column width (NA, never a made-up number)",
      is.na(env$journal_width_mm("asm", "onehalf")) && is.na(env$journal_width_mm("plos", "onehalf")))
check("unknown family falls back to custom width", env$journal_width_mm("nope", "single", 77) == 77)

cat("\n--- preset values are inside journal limits ---\n")
ps <- env$journal_preset_settings(89, 65, "a", x_range = 180)
check("preset sets mm units and 89 x 65", ps$export_units == "mm" && ps$export_width == 89 && ps$export_height == 65)
check("preset text sizes are 6-8 pt",
      all(c(ps$title_font_size, ps$axis_label_font_size, ps$axis_text_font_size,
            ps$legend_font_size, ps$label_font_size, ps$panel_label_size) >= 6) &&
      all(c(ps$title_font_size, ps$axis_label_font_size, ps$axis_text_font_size,
            ps$legend_font_size, ps$label_font_size, ps$panel_label_size) <= 8))
check("preset data line = 0.75 pt (+/- 0.05)", abs(env$lw2pt(ps$line_thickness) - 0.75) < 0.05)
check("preset error bar ~0.5 pt (0.25-1 pt band)", env$lw2pt(ps$error_bar_thickness) >= 0.25 && env$lw2pt(ps$error_bar_thickness) <= 1)
check("preset axis line 0.5 pt, tick 1.2 mm", ps$axis_line_pt == 0.5 && ps$tick_length_mm == 1.2)
check("preset uses Arial, Okabe-Ito palette, SVG, 600 dpi",
      ps$font_family == "Arial" && ps$color_palette == "colorblind" && ps$export_format == "svg" && ps$export_dpi == 600)
check("preset legend inside the panel, no border", startsWith(ps$legend_position, "inside-") && isFALSE(ps$legend_box_border))
check("preset error-bar cap width = 1.5 % of x span", near(ps$error_bar_width, round(180 * 0.015, 2)))
check("preset without data leaves error_bar_width alone", is.null(env$journal_preset_settings(89, 65)$error_bar_width))
check("preset carries the panel label", ps$panel_label == "a")

cat("\n--- export device resolver ---\n")
check("svg -> svglite wrapper (fix_text_size = FALSE)", identical(env$export_device("svg"), env$svglite_inkscape_device))
check("pdf -> a font-embedding device, never plain 'pdf' on this machine",
      is.function(env$export_device("pdf")))
check("png/tiff -> ragg when installed", !env$has_ragg || identical(env$export_device("png"), ragg::agg_png))

# ── Render through the real server ───────────────────────────────────────────
font_choices <- c("sans", "Arial", "Helvetica", "serif", "mono", "Helvetica Neue", "Garamond", "Palatino")
formats      <- c("pdf", "svg", "png", "tiff", "jpeg")
pdf_embeds_font <- function(f) {
  x <- readLines(f, warn = FALSE, encoding = "latin1")
  sum(grepl("FontFile", x, useBytes = TRUE)) > 0
}

testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  seed_defaults(session)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "")

  cat("\n--- export_size_in(): one source of truth ---\n")
  session$setInputs(export_units = "in", export_width = 10, export_height = 8)
  check("inches pass through", isTRUE(all.equal(export_size_in(), c(10, 8))))
  session$setInputs(export_units = "mm", export_width = 89, export_height = 65)
  check("mm are converted to inches", isTRUE(all.equal(export_size_in(), c(89, 65) / 25.4)))

  cat("\n--- default plot still builds and exports through the new devices ---\n")
  session$setInputs(export_units = "in", export_width = 10, export_height = 8)
  p_def <- generate_plot()
  real_plot("default plot renders (not the error placeholder)", p_def)

  n_ok <- 0L; n_try <- 0L
  for (fam in font_choices) {
    session$setInputs(font_family = fam)
    p <- generate_plot()
    real_plot(sprintf("plot renders with font family '%s'", fam), p)
    for (fmt in formats) {
      n_try <- n_try + 1L
      f <- of(sprintf("def_%s.%s", gsub("[^A-Za-z]", "", fam), fmt))
      args <- list(filename = f, plot = p, device = env$export_device(fmt),
                   width = 10, height = 8, units = "in", dpi = 150, bg = "white")
      if (fmt == "tiff") args$compression <- "lzw"
      ok <- tryCatch({ quiet(do.call(ggsave, args)); file.exists(f) && file.size(f) > 0 },
                     error = function(e) { cat("   ERROR", fam, fmt, ":", conditionMessage(e), "\n"); FALSE })
      if (ok) n_ok <- n_ok + 1L
      if (ok && fmt == "pdf") {
        installed <- fam %in% c("sans", "serif", "mono") ||
          grepl(gsub(" ", "", fam), gsub(" ", "", basename(systemfonts::match_fonts(fam)$path)), ignore.case = TRUE)
        if (installed) check(sprintf("PDF embeds fonts for family '%s'", fam), pdf_embeds_font(f))
        else skipit(sprintf("family '%s' is not installed on this machine: embedding check", fam))
      }
    }
  }
  check(sprintf("all %d font x format exports succeed (%d ok)", n_try, n_ok), n_ok == n_try)
  session$setInputs(font_family = "sans")

  cat("\n--- journal preset: 89 x 65 mm Nature single column ---\n")
  pd <- prepare_plot_data()
  vals <- env$journal_preset_settings(89, 65, "a", diff(range(pd$time)))
  do.call(session$setInputs, vals)
  p_j <- generate_plot()
  real_plot("preset plot renders (not the error placeholder)", p_j)
  sz <- export_size_in()
  check("preset export size = 89 x 65 mm in inches", isTRUE(all.equal(sz, c(89, 65) / 25.4)))

  svg_j <- of("journal_89x65.svg")
  quiet(ggsave(svg_j, p_j, device = env$export_device("svg"), width = sz[1], height = sz[2], units = "in", bg = "white"))
  x <- readLines(svg_j)
  hdr <- grep("<svg", x, value = TRUE)[1]
  check("SVG canvas is 89 mm wide (252.28 pt)", grepl("width='252.28pt'", hdr))
  check("SVG canvas is 65 mm tall (184.25 pt)", grepl("height='184.25pt'", hdr))
  txt <- grep("<text", x, value = TRUE)
  check("SVG has text elements (editable, not outlined)", length(txt) > 10)
  check("SVG text carries NO textLength (Inkscape edits do not stretch)", !any(grepl("textLength", txt)))
  check("SVG text is Arial", all(grepl('font-family: "Arial"', txt)))
  sizes <- as.numeric(sub('.*font-size: ([0-9.]+)px.*', '\\1', txt))
  check("SVG text sizes are all 6-8 pt", all(sizes >= 6 - 1e-6 & sizes <= 8 + 1e-6))
  check("SVG tick labels are 6 pt and axis titles 7 pt",
        any(abs(sizes - 6) < 1e-6) && any(abs(sizes - 7) < 1e-6))
  check("panel label 'a' present in bold 8 pt",
        any(grepl(">a</text>", txt) & grepl("font-size: 8.00px", txt) & grepl("font-weight: bold", txt)))
  # only elements that actually draw a stroke (background/key rects carry
  # stroke: none and inherit a meaningless default width)
  drawn   <- grep("stroke-width", x, value = TRUE)
  drawn   <- drawn[!grepl("stroke: none", drawn)]
  strokes <- as.numeric(sub('.*stroke-width: ([0-9.]+);.*', '\\1', drawn))
  strokes <- strokes[is.finite(strokes) & strokes > 0]
  check("every visible stroke in the journal SVG is within 0.25-1 pt (Nature: 0.25-1 pt)",
        length(strokes) > 20 && all(strokes >= 0.25 & strokes <= 1.0))
  check("data lines are 0.75 pt and axis/border lines 0.5 pt",
        any(abs(strokes - 0.75) < 0.01) && any(abs(strokes - 0.5) < 0.01))
  check("no textLength anywhere in the file", !any(grepl("textLength", x)))

  pdf_j <- of("journal_89x65.pdf")
  quiet(ggsave(pdf_j, p_j, device = env$export_device("pdf"), width = sz[1], height = sz[2], units = "in", bg = "white"))
  check("journal PDF written with embedded Arial", file.exists(pdf_j) && pdf_embeds_font(pdf_j))

  png_j <- of("journal_89x65.png")
  quiet(ggsave(png_j, p_j, device = env$export_device("png"), width = sz[1], height = sz[2], units = "in", dpi = 600, bg = "white"))
  check("journal PNG at 600 dpi exists", file.exists(png_j) && file.size(png_j) > 0)
  if (requireNamespace("png", quietly = TRUE)) {
    dm <- dim(png::readPNG(png_j))
    check("600 dpi PNG pixel width = 89 mm (2102 px +/- 2)", abs(dm[2] - round(89 / 25.4 * 600)) <= 2)
  } else skipit("png package not installed: pixel-size check")

  cat("\n--- figure legend text ---\n")
  session$setInputs(nb_host_strain = "E. coli K-12", nb_phage_plasmid = "phage T4",
                    nb_moi = "10", nb_media = "LB", nb_temperature = "37", nb_replicate = "3 biological replicates")
  lt <- output$figure_legend_text
  check("legend text names host, phage and MOI", grepl("E. coli K-12 infected with phage T4 \\(MOI 10\\)", lt))
  check("legend text states n = 3 and s.e.m.", grepl("n = 3 replicates", lt) && grepl("s.e.m.", lt, fixed = TRUE))
  check("legend text has no doubled period", !grepl("..", lt, fixed = TRUE))
  check("legend text notes the log axis", grepl("log10", lt))
  check("legend text starts with the panel label", startsWith(lt, "(a) "))
  check("legend lists the curves", grepl("Curves: Uninfected; Phage MOI 10; Phage MOI 0.1", lt))
  session$setInputs(error_type = "none")
  check("no error bars -> no error sentence", !grepl("error bars", output$figure_legend_text))
  session$setInputs(error_type = "sem")

  cat("\n--- layout inputs are honoured ---\n")
  session$setInputs(legend_position = "none")
  real_plot("plot renders with legend_position = none", generate_plot())
  th <- generate_plot()$theme
  check("legend_position = none reaches the theme", identical(th$legend.position, "none"))
  session$setInputs(legend_position = "inside-bottom-right")
  th <- generate_plot()$theme
  check("inside-bottom-right -> legend.position 'inside' with justification (1, 0)",
        identical(th$legend.position, "inside") && isTRUE(all.equal(th$legend.justification.inside, c(1, 0))))
  session$setInputs(legend_position = "right", panel_label = "")

  cat("\n--- loading a pre-v2.10 settings file while the UI is in mm ---\n")
  session$setInputs(export_units = "mm", export_width = 89, export_height = 65)
  old_file <- of("v29_settings.json")
  jsonlite::write_json(list(version = "2.9.0", saved_at = "x",
                            global_settings = list(export_width = 10, export_height = 8, line_thickness = 1),
                            sample_aesthetics = list()),
                       old_file, auto_unbox = TRUE)
  session$setInputs(loadSettings = list(name = "v29_settings.json", datapath = old_file,
                                        size = file.size(old_file), type = "application/json"))
  check("old file (no export_units) marks the units as inches so 10 x 8 is not read as mm",
        identical(rv$units_seen, "in"))
  check("settings load reported success", identical(rv$settings_status$type, "ok"))
})

# ── Byte-identical default output vs the pre-v2.10 app ──────────────────────
cat("\n--- default figure is byte-identical to v2.9 (opt-in only) ---\n")
old_path <- Sys.getenv("KC_OLD_APP", "")
if (old_path == "" && nzchar(Sys.which("git"))) {
  old_path <- file.path(out_dir, "old_app.R")
  rc <- suppressWarnings(system2("git", c("show", "aeeaf7d:Lysis_Curve_App_v28.R"),
                                 stdout = old_path, stderr = FALSE))
  if (!identical(rc, 0L) || !file.exists(old_path) || file.size(old_path) == 0) old_path <- ""
}
if (nzchar(old_path) && file.exists(old_path)) {
  defaults_v210 <- env$APP_DEFAULTS
  render_default_svg <- function(app_path, out) {
    e <- new.env(); a <- source(app_path, local = e)$value
    testServer(a, {
      ingest_data(e$make_demo_data(), "Demo data (built-in)")
      do.call(session$setInputs, defaults_v210)
      session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "")
      p <- generate_plot()
      real_plot(paste("default render is a real figure:", basename(app_path)), p)
      quiet(svglite::svglite(out, width = 10, height = 8)); print(p); dev.off()
    })
    readLines(out)
  }
  a <- render_default_svg(old_path, of("old_default.svg"))
  b <- render_default_svg("Lysis_Curve_App_v28.R", of("new_default.svg"))
  check("default-settings SVG from v2.9 and v2.10 is byte-identical", identical(a, b) && length(a) > 50)
  if (!identical(a, b)) { cat("   first differing lines:\n"); d <- which(a != b)[1:3]; print(cbind(old = a[d], new = b[d])) }
  # the theme_pubr path (Advanced Tick Customization off) takes the new axis.line branch
  defaults_v210$use_advanced_ticks <- FALSE
  a2 <- render_default_svg(old_path, of("old_pubr.svg"))
  b2 <- render_default_svg("Lysis_Curve_App_v28.R", of("new_pubr.svg"))
  check("theme_pubr (advanced ticks off) SVG is also byte-identical to v2.9", identical(a2, b2) && length(a2) > 50)
  if (!identical(a2, b2)) { cat("   first differing lines:\n"); d <- which(a2 != b2)[1:3]; print(cbind(old = a2[d], new = b2[d])) }
} else skipit("pre-v2.10 app not available (set KC_OLD_APP=path): byte-identical check")

# ── Inkscape round trip (optional) ───────────────────────────────────────────
cat("\n--- Inkscape round trip (optional) ---\n")
ink <- Sys.which("inkscape")
if (ink == "" && file.exists("/Applications/Inkscape.app/Contents/MacOS/inkscape"))
  ink <- "/Applications/Inkscape.app/Contents/MacOS/inkscape"
svg_j <- of("journal_89x65.svg")
if (nzchar(ink) && file.exists(svg_j)) {
  qw <- suppressWarnings(system2(ink, c("--query-width", shQuote(svg_j)), stdout = TRUE, stderr = FALSE))
  qw <- suppressWarnings(as.numeric(tail(qw, 1)))
  check("Inkscape reads the SVG as 89 mm wide (336.4 px @ 96 dpi)", is.finite(qw) && abs(qw - 89 / 25.4 * 96) < 0.5)
  pdf_rt <- of("inkscape_roundtrip.pdf")
  suppressWarnings(system2(ink, c("--export-type=pdf", paste0("--export-filename=", shQuote(pdf_rt)), shQuote(svg_j)),
                           stdout = FALSE, stderr = FALSE))
  check("Inkscape exports the SVG to PDF without error", file.exists(pdf_rt) && file.size(pdf_rt) > 0)
} else skipit("Inkscape CLI not found: round-trip check")

cat(sprintf("\n============ SUMMARY: %d passed / %d failed / %d skipped ============\n", pass, fail, skip))
cat("artifacts in:", out_dir, "\n")
if (fail > 0) quit(status = 1)
