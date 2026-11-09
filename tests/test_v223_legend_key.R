# ============================================================================
# v2.23.1 legend key: a standalone, colour-exact key cut from every figure's own legend (legend_key_grob() /
# write_legend_key()), its preview and download buttons, its place in the figure bundle and the batch export, and the
# colour words in the figure legend text ("Curves: MG1655 (black, filled circles); ...").
# Run from the repository root:  Rscript tests/test_v223_legend_key.R   (nothing here opens the default graphics device)
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v223_legend_key.R")

pass <- 0L; fail <- 0L; skip <- 0L
check  <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
skipit <- function(desc) { skip <<- skip + 1L; cat("SKIP:", desc, "\n") }

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.23.1 or later", package_version(env$APP_VERSION) >= "2.23.1")

# run `fn()` (which drives a testServer) with showNotification captured (the round-2 pattern)
with_notes <- function(fn) {
  got <- character(0); old <- env$showNotification
  env$showNotification <- function(ui, ..., type = "default") { got <<- c(got, paste0("[", type, "] ", as.character(ui))); invisible(NULL) }
  on.exit(env$showNotification <- old)
  val <- fn(); list(value = val, notes = got)
}
read_all <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")
hexes_in <- function(txt) toupper(unique(regmatches(txt, gregexpr("#[0-9A-Fa-f]{6}", txt))[[1]]))
xml_esc <- function(x) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)
ls_archive <- function(f) if (identical(env$bundle_archive_kind(), "zip")) basename(utils::unzip(f, list = TRUE)$Name) else basename(utils::untar(f, list = TRUE))

# ── colour_word ─────────────────────────────────────────────────────────────
cat("\n--- colour_word ---\n")
check("colour_word: black, white and grey",
      identical(env$colour_word("#000000"), "black") && identical(env$colour_word("#FFFFFF"), "white") && identical(env$colour_word("#999999"), "grey"))
check("colour_word: the Okabe-Ito colours (orange, sky blue -> blue, bluish green -> teal, vermillion -> orange)",
      identical(env$colour_word("#E69F00"), "orange") && identical(env$colour_word("#56B4E9"), "blue") &&
      identical(env$colour_word("#009E73"), "teal") && identical(env$colour_word("#D55E00"), "orange"))
check("colour_word: the Set1 colours (red, blue, green, purple, orange, brown, pink)",
      identical(env$colour_word(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628", "#F781BF")),
                c("red", "blue", "green", "purple", "orange", "brown", "pink")))
check("colour_word: case and named colours do not matter; lower-case hex = upper-case hex", identical(env$colour_word("#e69f00"), "orange") && identical(env$colour_word("red"), "red"))
check("colour_word: vectorised, and NA / blank / nonsense give NA instead of an error",
      identical(env$colour_word(c("#000000", NA, "", "not a colour")), c("black", NA, NA, NA)))
words <- names(env$COLOUR_WORD_RGB)
check("colour_words: two different blues become dark blue / light blue; a lone colour keeps its word; identical colours keep identical words",
      identical(env$colour_words(c("#0072B2", "#56B4E9")), c("dark blue", "light blue")) &&
        identical(env$colour_words(c("#56B4E9", "#000000", "#0072B2")), c("light blue", "black", "dark blue")) &&
        identical(env$colour_words(c("#0072B2", "#0072B2", "#E69F00")), c("blue", "blue", "orange")) &&
        identical(env$colour_words(c("#0072B2", "#377EB8", "#56B4E9")), c("dark blue", "blue", "light blue")))   # three shades: ends qualified, middle plain
check("colour_words: NA / blank stay NA and do not disturb the others", identical(env$colour_words(c(NA, "#0072B2", "", "#56B4E9")), c(NA, "dark blue", NA, "light blue")))
check("legend_sample_text takes the qualified word", identical(env$legend_sample_text("A", "#0072B2", 16, TRUE, "solid", word = "dark blue"), "A (dark blue, filled circles)"))
check("colour_word: the table is the twelve words of the spec",
      identical(sort(words), sort(c("black", "white", "grey", "red", "orange", "yellow", "green", "teal", "blue", "purple", "pink", "brown"))))
pal_hex <- unique(c(unlist(lapply(c("viridis", "plasma", "colorblind", "publication", "set1", "rainbow", "gray", "blues", "blue_red"),
                                  function(p) env$get_palette_colors(p, 12))),
                    "#000000", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#FFFF33", "#009E73"))
cw <- env$colour_word(pal_hex)
check(sprintf("colour_word: every colour of every palette and colour picker (%d) maps to one of the words", length(pal_hex)), !anyNA(cw) && all(cw %in% words))

cat("\n--- marker and line words ---\n")
mw <- env$legend_marker_words
check("marker words: filled / open come from the Filled box exactly as the figure draws it",
      identical(mw(16, TRUE), "filled circles") && identical(mw(16, FALSE), "open circles") && identical(mw(15, TRUE), "filled squares") &&
      identical(mw(15, FALSE), "open squares") && identical(mw(17, TRUE), "filled triangles") && identical(mw(17, FALSE), "open triangles") &&
      identical(mw(18, TRUE), "filled diamonds") && identical(mw(18, FALSE), "open diamonds"))
check("marker words: the hollow codes (123 / 125) are open whatever the Filled box says; down triangle", identical(mw(123, TRUE), "open diamonds") &&
      identical(mw(125, TRUE), "open inverted triangles") && identical(mw(25, TRUE), "filled inverted triangles") && identical(mw(21, FALSE), "open circles"))
check("marker words: line-drawn markers are just named; junk is NA",
      identical(mw(4, TRUE), "crosses") && identical(mw(3, FALSE), "plus signs") && is.na(mw(NA)) && is.na(mw("x")) && is.na(mw(999)))
lw <- env$legend_line_words
check("line words: solid and unknown give nothing, the others a phrase",
      is.na(lw("solid")) && is.na(lw("nope")) && identical(lw("dashed"), "dashed line") && identical(lw("dotted"), "dotted line") && identical(lw("dotdash"), "dot-dashed line"))
st <- env$legend_sample_text
check("sample text: colour, marker, line type in that order",
      identical(st("MG1655", "#000000", 16, TRUE), "MG1655 (black, filled circles)") &&
      identical(st("WT", "#E69F00", 15, FALSE, "dashed"), "WT (orange, open squares, dashed line)"))
check("sample text: no points -> no marker words; no lines -> no line words; a solid line adds nothing",
      identical(st("WT", "#E69F00", 15, TRUE, "dotted", show_points = FALSE), "WT (orange, dotted line)") &&
      identical(st("WT", "#E69F00", 15, TRUE, "dotted", show_lines = FALSE), "WT (orange, filled squares)") &&
      identical(st("WT", "#E69F00", 15, TRUE, "solid", show_points = FALSE), "WT (orange)") && identical(st("x", NA, NA, TRUE, "solid", FALSE), "x"))

# ── the key of a plain ggplot (no app state) ────────────────────────────────
cat("\n--- legend_key_grob on plain ggplots ---\n")
dd <- data.frame(x = rep(1:3, 3), y = c(1:3, 2:4, 3:5), g = rep(c("alpha", "beta", "gamma"), each = 3))
p0 <- ggplot(dd, aes(x, y, colour = g)) + geom_line()
had_rplots <- file.exists("Rplots.pdf")
k0 <- env$legend_key_grob(p0)
check("a plot with a legend gives a gtable key with a positive measured size (width_in / height_in)",
      inherits(k0, "gtable") && is.finite(attr(k0, "width_in")) && attr(k0, "width_in") > 0.3 && attr(k0, "height_in") > 0.3)
check("the key lists the entries in the legend's order", all(c("alpha", "beta", "gamma") %in% env$legend_key_labels(k0)) &&
      identical(env$legend_key_labels(k0)[env$legend_key_labels(k0) %in% c("alpha", "beta", "gamma")], c("alpha", "beta", "gamma")))
k1 <- env$legend_key_grob(p0 + theme(legend.position = "none"))
check("a plot with the legend hidden still gives its key", inherits(k1, "gtable") && all(c("alpha", "beta", "gamma") %in% env$legend_key_labels(k1)))
k2 <- env$legend_key_grob(p0 + theme(legend.position = "inside", legend.position.inside = c(.9, .9)))
check("a plot with the legend inside the panel gives its key", inherits(k2, "gtable") && all(c("alpha", "beta", "gamma") %in% env$legend_key_labels(k2)))
check("a plot with no legend at all (no colour mapping) gives NULL, never an error", is.null(env$legend_key_grob(ggplot(dd, aes(x, y)) + geom_line())))
big <- env$legend_key_grob(ggplot(data.frame(x = 1:24, y = 1:24, g = paste0("sample ", 1:24)), aes(x, y, colour = g)) + geom_point() + theme(legend.position = "bottom"))
check("a 24-entry bottom legend (horizontal, several rows) is a key too", inherits(big, "gtable") && sum(grepl("^sample", env$legend_key_labels(big))) == 24)
check("measuring the keys left no Rplots.pdf behind (explicit pdf(NULL) device)", had_rplots || !file.exists("Rplots.pdf"))
f_png <- tempfile(fileext = ".png"); env$write_legend_key(p0, f_png, "png", dpi = 150)
check("write_legend_key accepts a plot directly: a PNG whose size follows the key (width_in x dpi)", file.exists(f_png) && file.size(f_png) > 500 &&
      (!requireNamespace("png", quietly = TRUE) || abs(dim(png::readPNG(f_png))[2] - round(attr(k0, "width_in") * 150)) <= 2))
err0 <- tryCatch({ env$write_legend_key(ggplot(dd, aes(x, y)) + geom_line(), tempfile(fileext = ".svg"), "svg"); NULL }, error = function(e) conditionMessage(e))
check("write_legend_key of a plot without a legend stops with a clear message", is.character(err0) && grepl("no legend", err0))

# ── in the app ──────────────────────────────────────────────────────────────
cat("\n--- main figure key (testServer) ---\n")
demo_d <- env$make_demo_data("lysis")
mk_session <- function(code) testServer(app, {
  ingest_data(demo_d, "Demo"); do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = rv$od_vars, metrics_ref_sample = rv$od_vars[1], export_format = "svg")
  code
})
res <- list()
testServer(app, {
  ingest_data(demo_d, "Demo"); do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = rv$od_vars, metrics_ref_sample = rv$od_vars[1], export_format = "svg")
  priv <- environment(session$setInputs)$private
  samples <- sel_samples(); aes_v <- resolve_aesthetics(samples)
  svg_main_before <- tempfile(fileext = ".svg")
  ggsave(svg_main_before, plot = main_plot_obj(), device = env$export_device("svg"), width = 3.5, height = 2.56, units = "in", bg = "white")

  key <- env$legend_key_grob(generate_plot())
  labs_k <- env$legend_key_labels(key)
  check("main key: one entry per selected sample, in legend order",
        identical(labs_k[labs_k %in% unname(aes_v$leg_labels)], unname(aes_v$leg_labels[samples])) && length(samples) == 3)
  f <- tempfile(fileext = ".svg"); env$write_legend_key(key, f, "svg"); sv <- read_all(f)
  check("main key SVG: every display label is in the file", all(vapply(unname(aes_v$leg_labels[samples]), function(l) grepl(xml_esc(l), sv, fixed = TRUE), NA)))
  check("main key SVG: every sample colour (hex, any case) is in the file", all(toupper(aes_v$colors[samples]) %in% hexes_in(sv)))
  check("main key SVG: it is cut to the key (width / height of the key, a white background), not the figure",
        grepl(sprintf("width='%.2fpt'", attr(key, "width_in") * 72), sv, fixed = TRUE) && grepl("fill: #FFFFFF", sv, fixed = TRUE))
  check("main key SVG: a marker, a line and no leftover axis text", grepl("<line ", sv, fixed = TRUE) && !grepl(">Time<", sv, fixed = TRUE))

  cat("\n--- the figure itself is untouched ---\n")
  svg_main_after <- tempfile(fileext = ".svg")
  ggsave(svg_main_after, plot = main_plot_obj(), device = env$export_device("svg"), width = 3.5, height = 2.56, units = "in", bg = "white")
  check("the main figure's SVG is byte-identical before and after the keys were made", identical(readBin(svg_main_before, "raw", 1e6), readBin(svg_main_after, "raw", 1e6)))

  # a restyled sample: another colour, an open marker, a dashed line, a new display name
  v2 <- samples[2]; id2 <- env$safe_id(v2)
  rv$display_names <- setNames("Phage ten", v2)
  do.call(session$setInputs, setNames(list("custom", "#984EA3", "dashed", FALSE, 17),
                                      paste0(c("color_selector_", "color_", "line_type_", "shape_filled_", "shape_"), id2)))
  aes2 <- resolve_aesthetics(samples)
  key2 <- env$legend_key_grob(generate_plot()); f2 <- tempfile(fileext = ".svg"); env$write_legend_key(key2, f2, "svg"); sv2 <- read_all(f2)
  check("restyled key: the new colour is in the key file and the old one is not", "#984EA3" %in% hexes_in(sv2) && !(toupper(aes_v$colors[[v2]]) %in% hexes_in(sv2)))
  check("restyled key: the dashed line shows as a dash array, and the open triangle is white-filled",
        grepl("stroke-dasharray", sv2, fixed = TRUE) && grepl("fill: #FFFFFF", sv2, fixed = TRUE))
  check("restyled key: the display name (not the column name) is the label", "Phage ten" %in% env$legend_key_labels(key2) && !(v2 %in% env$legend_key_labels(key2)))
  lt2 <- output$figure_legend_text
  check("restyled legend text: purple, open triangles and the dashed line for that sample",
        grepl("(purple, open triangles, dashed line)", lt2, fixed = TRUE))
  rv$display_names <- NULL
  do.call(session$setInputs, setNames(list("#E69F00", "solid", TRUE, 15), paste0(c("color_selector_", "line_type_", "shape_filled_", "shape_"), id2)))

  cat("\n--- legend hidden / inside / direct labels ---\n")
  for (lp in c("none", "inside-bottom-right", "direct", "bottom")) {
    session$setInputs(legend_position = lp)
    kk <- env$legend_key_grob(generate_plot()); ff <- tempfile(fileext = ".svg"); env$write_legend_key(kk, ff, "svg"); sx <- read_all(ff)
    check(sprintf("legend_position '%s': the key still has every sample (labels and colours)", lp),
          inherits(kk, "gtable") && all(vapply(unname(aes_v$leg_labels[samples]), function(l) grepl(xml_esc(l), sx, fixed = TRUE), NA)) &&
            all(toupper(resolve_aesthetics(samples)$colors[samples]) %in% hexes_in(sx)))
  }
  session$setInputs(legend_position = "right")

  cat("\n--- analysis, fit, compose and compare keys ---\n")
  for (a in c("metric_summary", "growth_rate", "heatmap")) {
    priv$.input$set("analysis_type", a)
    fa <- output$downloadAnalysisLegendKey
    check(sprintf("analysis figure '%s': the key downloads as a non-empty SVG named legend_key_%s.svg", a, a),
          is.character(fa) && file.exists(fa) && file.size(fa) > 400 && grepl("^<\\?xml", readLines(fa, n = 1, warn = FALSE)))
  }
  priv$.input$set("analysis_type", "metric_summary")
  ms <- read_all(output$downloadAnalysisLegendKey)
  check("a figure without a sample legend (the metric summary) gets the main figure's key: every sample colour is in it",
        all(toupper(aes_v$colors[samples]) %in% hexes_in(ms)))
  priv$.input$set("analysis_type", "growth_rate")
  check("the key of an analysis figure is its own: growth_rate lists every sample", {
    g <- read_all(output$downloadAnalysisLegendKey); all(vapply(unname(aes_v$leg_labels[samples]), function(l) grepl(xml_esc(l), g, fixed = TRUE), NA)) })
  fit <- output$downloadFitLegendKey
  check("fit figure: the key downloads as a non-empty SVG with every sample colour", file.exists(fit) && file.size(fit) > 400 && all(toupper(aes_v$colors[samples]) %in% hexes_in(read_all(fit))))
  cmp <- output$downloadComposeLegendKey
  check("composed figure: the key (the main key) downloads", file.exists(cmp) && file.size(cmp) > 400 && all(toupper(aes_v$colors[samples]) %in% hexes_in(read_all(cmp))))
  rv_cmp$ds <- ingest_frame(env$make_demo_data("lysis"), "second", strict = FALSE)
  cpk <- output$downloadCompareLegendKey
  check("compare figure: the key downloads as a non-empty SVG", file.exists(cpk) && file.size(cpk) > 400)
  rv_cmp$ds <- NULL

  cat("\n--- formats ---\n")
  for (fmt in c("png", "pdf", "tiff", "jpeg")) {
    session$setInputs(export_format = fmt)
    ff <- output$downloadLegendKey
    check(sprintf("the main key downloads in the figure's export format (%s)", fmt), file.exists(ff) && file.size(ff) > 300)
  }
  session$setInputs(export_format = "svg")

  cat("\n--- preview and handlers exist ---\n")
  pv <- tryCatch(output$legend_key_preview, error = function(e) e)
  check("the preview output renders (a PNG with a src and a height that follows the key)", is.list(pv) && !is.null(pv$src) && is.numeric(pv$height) && pv$height >= 40)
  check("the preview height is taller for twelve samples than for three", {
    d12 <- data.frame(Time = seq(0, 100, 10)); for (i in 1:12) d12[[paste0("S", i)]] <- 0.1 + 0.02 * i + d12$Time / 1000
    h3 <- pv$height; ingest_data(d12, "Twelve"); do.call(session$setInputs, env$APP_DEFAULTS); session$setInputs(selected_samples = rv$od_vars)
    h12 <- output$legend_key_preview$height; ingest_data(demo_d, "Demo"); do.call(session$setInputs, env$APP_DEFAULTS); session$setInputs(selected_samples = rv$od_vars)
    is.numeric(h12) && h12 > h3 })
  hs <- c("downloadLegendKey", "downloadAnalysisLegendKey", "downloadFitLegendKey", "downloadComposeLegendKey", "downloadCompareLegendKey")
  check("every legend key download handler exists on the server (the compare one has its own check above)",
        all(vapply(setdiff(hs, "downloadCompareLegendKey"), function(h) is.character(tryCatch(output[[h]], error = function(e) NULL)), NA)))

  cat("\n--- legend text: colour words ---\n")
  lt <- output$figure_legend_text; aes_n <- resolve_aesthetics(samples)
  expect_one <- function(i, show_pts = TRUE) env$legend_sample_text(unname(aes_n$leg_labels[i]), aes_n$colors[[i]], aes_n$shapes[[i]], aes_n$filled_map[[i]], aes_n$line_types[[i]], show_pts)
  curves <- paste0("Curves: ", paste(vapply(samples, expect_one, ""), collapse = "; "), ".")
  check("the legend text's Curves sentence is the label + colour word + marker of every sample (words from colour_word of the actual colours)", grepl(curves, lt, fixed = TRUE))
  check("the demo palette gives black / orange / blue and filled circle / square / triangle",
        grepl("Uninfected (black, filled circles); Phage MOI 10 (orange, filled squares); Phage MOI 0.1 (blue, filled triangles)", lt, fixed = TRUE))
  do.call(session$setInputs, setNames(list(FALSE), paste0("shape_filled_", env$safe_id(samples[1]))))
  check("an unticked Filled box turns that sample's marker to 'open'", grepl("Uninfected (black, open circles)", output$figure_legend_text, fixed = TRUE) &&
        grepl("Phage MOI 10 (orange, filled squares)", output$figure_legend_text, fixed = TRUE))
  do.call(session$setInputs, setNames(list(TRUE), paste0("shape_filled_", env$safe_id(samples[1]))))
  session$setInputs(show_points = FALSE)
  lt_np <- output$figure_legend_text
  check("with points off the sentence carries the colour only (no marker words)", grepl("Curves: Uninfected (black); Phage MOI 10 (orange); Phage MOI 0.1 (blue).", lt_np, fixed = TRUE) && !grepl("filled|open", lt_np))
  session$setInputs(show_points = TRUE)

  cat("\n--- figure bundle and batch export ---\n")
  zb <- output$downloadFigureBundle; zl <- ls_archive(zb)
  check("the figure bundle contains legend_key.svg and legend_key.pdf next to the figure", all(c("legend_key.svg", "legend_key.pdf", "figure_main.svg", "figure_main.pdf") %in% zl))
  exb <- tempfile("kb_"); dir.create(exb); if (identical(env$bundle_archive_kind(), "zip")) utils::unzip(zb, exdir = exb) else utils::untar(zb, exdir = exb)
  kfile <- list.files(exb, "^legend_key\\.svg$", recursive = TRUE, full.names = TRUE)
  check("the bundle's legend_key.svg has every sample's colour and name", length(kfile) == 1 && all(toupper(aes_v$colors[samples]) %in% hexes_in(read_all(kfile))) &&
        all(vapply(unname(aes_v$leg_labels[samples]), function(l) grepl(xml_esc(l), read_all(kfile), fixed = TRUE), NA)))
  check("the README lists the legend key", any(grepl("legend_key.svg", readLines(list.files(exb, "README.txt", recursive = TRUE, full.names = TRUE)), fixed = TRUE)))
  check("batch export offers a 'Legend key' output (and 'all' includes it)", "legend_key" %in% unlist(env$batch_output_choices()))
  d1 <- file.path(tempfile("bx_")); dir.create(d1)
  r1 <- batch_export_build(d1, c("legend_key", "main"), "png", 150, NULL)
  check("batch export writes legend_key_main.png (and the figure) for the selected outputs", "legend_key_main.png" %in% r1$files && length(r1$files) == 2 && length(r1$skipped) == 0)
  d2 <- file.path(tempfile("bx_")); dir.create(d2)
  r2 <- batch_export_build(d2, "legend_key", "svg", 300, NULL)
  check("batch export of the legend key alone: legend_key_main.svg with the sample colours", identical(r2$files, "legend_key_main.svg") &&
        all(toupper(aes_v$colors[samples]) %in% hexes_in(read_all(file.path(d2, "legend_key_main.svg")))))
})

# ── empty state: a download refuses, writes nothing ─────────────────────────
cat("\n--- no data loaded ---\n")
handlers <- c("downloadLegendKey", "downloadAnalysisLegendKey", "downloadFitLegendKey", "downloadComposeLegendKey", "downloadCompareLegendKey")
empty <- with_notes(function() {
  out <- list()
  testServer(app, {
    do.call(session$setInputs, env$APP_DEFAULTS); session$setInputs(export_format = "svg")
    for (h in handlers) out[[h]] <<- tryCatch({ f <- output[[h]]; paste("FILE", file.exists(f)) }, error = function(e) conditionMessage(e))
  })
  out
})
check("with no data every legend key download refuses ('Nothing to export') and writes no file",
      all(vapply(handlers, function(h) grepl("^Nothing to export", empty$value[[h]]), NA)))
check("the refusal is a notification (warning), never a placeholder file", any(grepl("^\\[warning\\] Nothing to export", empty$notes)))
emp2 <- with_notes(function() {
  out <- NULL
  testServer(app, {
    ingest_data(demo_d, "Demo"); do.call(session$setInputs, env$APP_DEFAULTS)
    session$setInputs(selected_samples = rv$od_vars, export_format = "svg", enable_time_filter = TRUE, exclude_timepoints = paste(rv$all_timepoints, collapse = ","))
    out <<- tryCatch({ f <- output$downloadLegendKey; paste("FILE", file.exists(f)) }, error = function(e) conditionMessage(e))
  })
  out
})
check("with every time point filtered away the key download refuses too", grepl("^Nothing to export", emp2$value))

# ── the source: names and places ────────────────────────────────────────────
cat("\n--- UI ---\n")
ui_html <- as.character(env$ui)
has_id <- function(id) grepl(sprintf('id="%s"', id), ui_html, fixed = TRUE)
check("the UI has the preview and the five legend key buttons", all(vapply(c("legend_key_preview", handlers), has_id, NA)))
check("the Lysis curve tab's button sits in the Figure Legend Text section, after the legend text box",
      { a <- regexpr('id="figure_legend_text"', ui_html, fixed = TRUE); b <- regexpr('id="legend_key_preview"', ui_html, fixed = TRUE); c <- regexpr('id="downloadLegendKey"', ui_html, fixed = TRUE)
        a > 0 && b > a && c > b && c - a < 2500 })
check("the Analysis figures button sits next to 'Download figure' (downloadAnalysisPlot)",
      abs(regexpr('id="downloadAnalysisLegendKey"', ui_html, fixed = TRUE) - regexpr('id="downloadAnalysisPlot"', ui_html, fixed = TRUE)) < 600)
check("the fit / compose / compare buttons sit next to their figure downloads",
      abs(regexpr('id="downloadFitLegendKey"', ui_html, fixed = TRUE) - regexpr('id="downloadFitFigure"', ui_html, fixed = TRUE)) < 600 &&
      abs(regexpr('id="downloadComposeLegendKey"', ui_html, fixed = TRUE) - regexpr('id="downloadComposedFigure"', ui_html, fixed = TRUE)) < 600 &&
      abs(regexpr('id="downloadCompareLegendKey"', ui_html, fixed = TRUE) - regexpr('id="downloadCompare"', ui_html, fixed = TRUE)) < 600)
src <- readLines("Lysis_Curve_App_v28.R", warn = FALSE)
check("file names are legend_key_<figure>: main, <analysis_type>, fit, compose, compare",
      any(grepl('paste0("legend_key_", figure(), ".", sel1("export_format", "svg"))', src, fixed = TRUE)) &&
      any(grepl('legend_key_download(function() "main"', src, fixed = TRUE)) && any(grepl('legend_key_download(function() sel1("analysis_type", "figure")', src, fixed = TRUE)) &&
      any(grepl('legend_key_download(function() "fit"', src, fixed = TRUE)) && any(grepl('legend_key_download(function() "compose"', src, fixed = TRUE)) &&
      any(grepl('legend_key_download(function() "compare"', src, fixed = TRUE)))
check("no figure measurement opens the default device: the key helpers use kc_with_scratch_device (pdf(NULL) first)",
      any(grepl("kc_with_scratch_device <- function", src, fixed = TRUE)) && !any(grepl("^\\s*dev\\.new\\(", src)))
check("running the whole suite left no Rplots.pdf behind", had_rplots || !file.exists("Rplots.pdf"))

cat(sprintf("\nSUMMARY: %d passed, %d failed, %d skipped\n", pass, fail, skip))
if (fail > 0) quit(status = 1)
