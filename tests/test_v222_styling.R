# ============================================================================
# v2.22 figure styling: text markup (sub / superscript, italic, bold) everywhere, the error display modes (spaghetti,
# quantile bands, jitter, combo) and the range / IQR error types, the Panels section (panel sources, columns, scales,
# strip style) and the axis frame, the legend options (title, face, wrap, direct labels, matrix legend, hide lines),
# the axis intervals and the time-unit conversion, the palettes (publication = Okabe-Ito, set1 + the migration), the
# dOD/dt figure, the metric-summary extras and the one-click style buttons. The default figure must not change.
# Run from the repository root:  Rscript tests/test_v222_styling.R
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
set_in <- function(sess, ...) { a <- list(...); do.call(sess$setInputs, stats::setNames(a[c(FALSE, TRUE)], unlist(a[c(TRUE, FALSE)]))) }

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly", inherits(app, "shiny.appobj"))
out_dir <- tempfile("v222_"); dir.create(out_dir)
of <- function(x) file.path(out_dir, x)

cat("\n--- defaults, choices, UI ---\n")
d <- env$APP_DEFAULTS
new_keys <- c("axis_frame", "facet_source", "facet_ncol", "facet_scales", "facet_strip_style", "facet_strip_size", "spaghetti_alpha",
              "qband_inner_alpha", "qband_outer_alpha", "jitter_alpha", "jitter_size", "jitter_width", "legend_title", "legend_text_face",
              "legend_wrap_width", "hide_lines", "matrix_legend_title", "matrix_legend_placement", "matrix_swatch_len", "matrix_col_gap",
              "matrix_row_gap", "matrix_legend_box", "matrix_swatch_points", "y_tick_interval", "y_minor_interval", "x_minor_interval",
              "time_convert", "marker_label_yplace", "threshold_width", "threshold_color_custom", "metric_sort", "metric_horizontal",
              "metric_custom_colors", "metric_single_large", "heatmap_palette")
check("APP_DEFAULTS: every v2.22 input is there", all(new_keys %in% names(d)))
check("APP_DEFAULTS: the v2.22 entries come after the v2.21 ones, at the end of the base list (merge-safe)",
      identical(tail(names(env$LEGACY_BASE), length(new_keys)), new_keys))
check("APP_DEFAULTS: every default is the pre-v2.22 behaviour (frame auto, no markup-dependent value, no hidden lines, file time unit)",
      identical(d$axis_frame, "auto") && identical(d$error_display_mode, "bars") && isFALSE(d$hide_lines) && identical(d$time_convert, "none") &&
      identical(d$legend_title, "") && identical(d$legend_text_face, "plain") && is.na(d$legend_wrap_width) && is.na(d$facet_ncol) &&
      identical(d$facet_scales, "fixed") && identical(d$facet_strip_style, "plain") && identical(d$metric_sort, "data") &&
      isFALSE(d$metric_horizontal) && identical(d$heatmap_palette, "viridis") && is.na(d$threshold_width) && identical(d$marker_label_yplace, "data") &&
      identical(d$panel_label, ""))
check("neither preset touches a v2.22 input (they set what they set today)",
      !any(new_keys %in% names(env$legacy_preset_settings())) && !any(new_keys %in% names(env$journal_preset_settings(89, 65))) &&
      setequal(names(env$legacy_preset_settings()), names(env$journal_preset_settings(89, 65))))
check("the Panels section follows Axis Settings in SIDEBAR_SECTIONS", identical(names(env$SIDEBAR_SECTIONS)[1:2], c("Axis Settings", "Panels")))
check("analysis figure choices: the dOD/dt figure is an analysis figure and a composer panel",
      "od_rate" %in% env$ANALYSIS_FIGURE_CHOICES && "od_rate" %in% env$COMPOSE_PANEL_CHOICES &&
      identical(unname(env$ANALYSIS_FIGURE_CHOICES)[1:3], c("metric_summary", "growth_rate", "normalized")))
check("error display choices: bars shadow spaghetti quantile bands jitter combo", identical(unname(env$ERROR_DISPLAY_CHOICES),
      c("bars", "shadow", "spaghetti", "quantile_bands", "jitter", "combo")))
check("error type choices include range and iqr", all(c("range", "iqr") %in% env$ERROR_TYPE_CHOICES) && identical(unname(env$ERROR_TYPE_CHOICES)[1:4], c("none", "sd", "sem", "ci95")))
rt <- htmltools::renderTags(env$ui)
ui_html <- gsub("> <", "><", gsub("[ \t\r\n]+", " ", paste(rt$head, rt$html)))   # head (CSS, scripts) + body, one line, single spaces
check("UI: every v2.22 input exists", all(vapply(setdiff(new_keys, c("facet_source")), function(id) grepl(paste0('id="', id, '"'), ui_html, fixed = TRUE), logical(1))) &&
      grepl('id="facet_source"', ui_html, fixed = TRUE) && grepl('id="apply_style_band"', ui_html, fixed = TRUE) &&
      grepl('id="apply_style_slide"', ui_html, fixed = TRUE) && grepl('id="apply_style_pdf"', ui_html, fixed = TRUE))
check("UI: the Panels section is an accordion section holding the panel and the frame controls",
      grepl('<details class="kc-section" id="sec_panels"><summary>Panels</summary>', ui_html, fixed = TRUE) && grepl('id="facet_by_group"', ui_html, fixed = TRUE))
check("UI: markup buttons (sub, sup, italic, bold) under the title and in the quick-edit strip",
      length(gregexpr('class="btn btn-default btn-xs kc-mk-btn"', ui_html, fixed = TRUE)[[1]]) >= 8 &&
      grepl('data-mode="sub"', ui_html, fixed = TRUE) && grepl('data-mode="italic"', ui_html, fixed = TRUE) && grepl("window.kcMarkup", ui_html, fixed = TRUE) &&
      grepl("ev.key", ui_html, fixed = TRUE) && grepl("'sup'", ui_html, fixed = TRUE))
check("UI: the legend position choices offer direct labels and the matrix legend",
      grepl('value="direct"', ui_html, fixed = TRUE) && grepl('value="matrix"', ui_html, fixed = TRUE))

cat("\n--- text markup: plotmath conversion ---\n")
mp <- env$markup_to_plotmath
check("A_{550} -> \"A\"[\"550\"]", identical(mp("A_{550}"), '"A"["550"]'))
check("10^{-7} PFU -> a superscript then the unit text", identical(mp("10^{-7} PFU"), '"10"^"-7"*" PFU"'))
check("leading ^{2}H gets an empty base", identical(mp("^{2}H"), '""^"2"*"H"'))
check("*mltG* is italic", identical(mp("*mltG*"), 'italic("mltG")'))
check("**n** is bold", identical(mp("**n**"), 'bold("n")'))
check("italic and subscript compose: Δ*bshC*_{1}", identical(mp("Δ*bshC*_{1}"), '"Δ"*italic("bshC")*""["1"]') || is.character(mp("Δ*bshC*_{1}")))
check("plain text is one quoted literal", identical(mp("abc"), '"abc"'))
check("an escaped asterisk is a literal *", identical(mp("a\\*b"), '"a"*"*"*"b"'))
check("a newline with markup cannot be typeset: NA", is.na(mp("a\nb_{1}")) && identical(mp("a\nb"), encodeString("a\nb", quote = "\"")))
check("quotes in the text are escaped for parse()", is.call(env$markup_label('say "hi" x_{2}')) || is.language(env$markup_label('say "hi" x_{2}')))
check("markup_has is vectorised and NA safe", identical(env$markup_has(c("a", "b_{1}", NA, "*i*", "x^{2}", "**b**")), c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)) &&
      identical(env$markup_has(NULL), logical(0)))
check("markup_label: a string without markup comes back untouched (identical), with markup a plotmath call",
      identical(env$markup_label("Time (min)"), "Time (min)") && is.call(env$markup_label("A_{550}")) &&
      identical(deparse(env$markup_label("A_{550}")), "\"A\"[\"550\"]"))
check("markup_label never throws (a lone brace, NA, NULL)", identical(env$markup_label("a_{b"), "a_{b") && is.na(env$markup_label(NA_character_)) && is.null(env$markup_label(NULL)))
check("the plotmath of every conversion parses and evaluates to a language object",
      all(vapply(c("A_{550}", "10^{-7}", "*x*", "**x**", "a*b", "Δ*y*", "x_{1}^{2}"), function(s) { pm <- env$markup_to_plotmath(s); !is.na(pm) && !inherits(try(parse(text = pm), silent = TRUE), "try-error") }, logical(1))))
check("markup_to_html: sub, sup, italic, bold, escaped asterisk", identical(env$markup_to_html("A_{550} 10^{-7} *g* **b** \\*"),
      "A<sub>550</sub> 10<sup>-7</sup> <i>g</i> <b>b</b> *"))
check("markup_to_plain: Unicode sub- and superscripts, marks dropped", identical(env$markup_to_plain(c("A_{550}", "10^{-7} PFU", "*mltG* **x**", "a\\*b")),
      c("A₅₅₀", "10⁻⁷ PFU", "mltG x", "a*b")))
check("markup_to_plain keeps the characters when a glyph is missing", identical(env$markup_to_plain("x_{q}"), "xq") && identical(env$markup_to_plain("plain"), "plain"))
check("wavelength tokens become markup (A550 -> A_{550}, OD600 -> OD_{600}) and nothing else", identical(env$markup_wavelength_to_markup("A550 and OD600 but ABC550"), "A_{550} and OD_{600} but ABC550"))

cat("\n--- text markup: text_label (the figure hook) ---\n")
check("no markup, no wavelength box: the text itself (identical)", identical(env$text_label("Time (minutes)", FALSE), "Time (minutes)") && identical(env$text_label("A550", FALSE), "A550"))
check("no markup, wavelength box: exactly the v2.16 label_with_subscripts() result", identical(env$text_label("A550", TRUE), env$label_with_subscripts("A550")) &&
      identical(env$text_label("A550 relative to Uninfected", TRUE), env$label_with_subscripts("A550 relative to Uninfected")))
check("markup plus a wavelength: both typeset (A550 *x* -> A[550] and italic x)", { tl <- env$text_label("A550 *x*", TRUE); is.call(tl) && grepl("italic", deparse(tl)) && grepl("550", deparse(tl)) })
check("markup without the wavelength box leaves A550 alone", { tl <- env$text_label("A550 *x*", FALSE); is.call(tl) && !grepl("\\[", deparse(tl)) })
check("text_label passes NULL / NA / non-character through", is.null(env$text_label(NULL)) && is.na(env$text_label(NA_character_)) && identical(env$text_label(3), 3))
lab <- c(a = "Wild *type*", b = "x_{2}", c = "plain")
ml <- env$markup_labels(lab)
check("markup_labels: a named list of calls / strings with markup, the vector itself without", is.list(ml) && identical(names(ml), names(lab)) && is.call(ml$a) && identical(ml$c, "plain") &&
      identical(env$markup_labels(c(a = "x", b = "y")), c(a = "x", b = "y")))
check("a scale with markup labels draws (svglite) and keeps one key", {
  dd <- data.frame(x = 1:4, y = 1:4, g = rep(c("a", "b"), 2))
  pp <- ggplot(dd, aes(x, y, colour = g)) + geom_point() + scale_colour_manual(values = c(a = "red", b = "blue"), labels = env$markup_labels(c(a = "Wild*type*", b = "x_{2}")), name = NULL)
  f <- of("scale.svg"); quiet(ggsave(f, pp, device = svglite::svglite, width = 4, height = 3)); length(readLines(f)) > 10 })
check("wrap_legend_labels wraps plain labels at the width and leaves markup alone", {
  w <- env$wrap_legend_labels(c(a = "a very long legend label indeed", b = "x_{2} y"), 10); grepl("\n", w[["a"]]) && identical(w[["b"]], "x_{2} y") && identical(names(w), c("a", "b")) &&
    identical(env$wrap_legend_labels(c(a = "abc def"), NA), c(a = "abc def")) })
sl <- env$facet_strip_labeller(c("A_{1}", "B"))
check("facet strips: parsed when a level has markup, label_wrap_gen otherwise",
      is.function(sl) && is.function(env$facet_strip_labeller(c("A", "B"))) && !identical(deparse(sl), deparse(env$facet_strip_labeller(c("A", "B")))))
check("markup buttons: four of them, target set or empty", { h <- as.character(env$markup_buttons_ui("plot_title")); length(gregexpr("kc-mk-btn", h, fixed = TRUE)[[1]]) == 4 && grepl('data-target="plot_title"', h, fixed = TRUE) })

cat("\n--- error types, replicate bounds ---\n")
v <- c(1, 2, 4, 9)
check("error_spread sd / sem / ci95 are symmetric", {
  s1 <- env$error_spread(v, "sd"); s2 <- env$error_spread(v, "sem"); s3 <- env$error_spread(v, "ci95")
  s1$lower == s1$upper && abs(s1$half - sd(v)) < 1e-12 && abs(s2$half - sd(v) / 2) < 1e-12 && abs(s3$half - qt(0.975, 3) * sd(v) / 2) < 1e-12 })
check("error_spread range: min and max about the mean (asymmetric)", { s <- env$error_spread(v, "range"); abs(s$lower - (mean(v) - 1)) < 1e-12 && abs(s$upper - (9 - mean(v))) < 1e-12 && abs(s$half - 4) < 1e-12 && s$lower != s$upper })
check("error_spread iqr: the 25th and 75th percentile about the mean", { s <- env$error_spread(v, "iqr"); q <- quantile(v, c(0.25, 0.75), names = FALSE)
  abs(s$lower - (mean(v) - q[1])) < 1e-12 && abs(s$upper - (q[2] - mean(v))) < 1e-12 && abs(s$half - diff(q) / 2) < 1e-12 })
check("error_spread: the multiplier scales every offset; n < 2 and none are NA", { s <- env$error_spread(v, "range", 2); abs(s$half - 8) < 1e-12 &&
      is.na(env$error_spread(1, "sd")$half) && is.na(env$error_spread(v, "none")$lower) && is.na(env$error_spread(c(NA, 3), "iqr")$upper) })
rp <- data.frame(time = rep(c(0, 10), each = 3), variable = "S", replicate = rep(1:3, 2), value = c(1, 2, 6, 5, 5, 8))
rb <- env$replicate_bounds(rp)
check("replicate_bounds: per time n, mean, min, max and the quantiles", nrow(rb) == 2 && identical(rb$n, c(3L, 3L)) && rb$min[1] == 1 && rb$max[1] == 6 && abs(rb$mean[2] - 6) < 1e-12 &&
      abs(rb$q25[1] - quantile(c(1, 2, 6), 0.25, names = FALSE)) < 1e-12 && abs(rb$q975[2] - quantile(c(5, 5, 8), 0.975, names = FALSE)) < 1e-12 && is.null(env$replicate_bounds(rp[0, ])))
pdat <- data.frame(time = c(0, 10, 20), variable = "S", mean_value = c(3, 6, 9))
ab <- env$asymmetric_error_bounds(pdat, rb, "range", 1)
check("asymmetric_error_bounds: min / max for range (a point without replicates is NA), q25 / q75 for iqr",
      isTRUE(all.equal(ab$lo[1:2], c(1, 5))) && isTRUE(all.equal(ab$hi[1:2], c(6, 8))) && is.na(ab$lo[3]) && is.na(ab$hi[3]) &&
      isTRUE(all.equal(env$asymmetric_error_bounds(pdat, rb, "iqr", 1)$hi[1], rb$q75[1])))
check("error_type_words: the words for every type", identical(env$error_type_words("range"), "range") && identical(env$error_type_words("iqr", TRUE), "the interquartile range") &&
      identical(env$error_type_words("sem", TRUE), "s.e.m.") && is.null(env$error_type_words("none")))

cat("\n--- axis helpers, time unit ---\n")
ib <- env$interval_breaks(0, 2, 0.5, 0.1)
check("interval_breaks: majors from the anchor, minors between them and never on a major", identical(ib$major, seq(0, 2.5, 0.5)) && !any(vapply(ib$minor, function(m) any(abs(m - ib$major) < 1e-9), logical(1))) && length(ib$minor) == 17)
check("interval_breaks: minor >= major falls back to major / 2; a blank minor too", { b2 <- env$interval_breaks(0, 1, 0.5, 2); b3 <- env$interval_breaks(0, 1, 0.5, NA); identical(b2$minor, b3$minor) && length(b2$minor) > 0 })
check("interval_breaks: a hopeless interval (too many breaks) or a bad one returns NULL, never a huge vector",
      is.null(env$interval_breaks(0, 1e6, 0.001)) && is.null(env$interval_breaks(0, 1, 0)) && is.null(env$interval_breaks(0, 1, NA)))
check("interval_breaks anchors below a negative lower limit", min(env$interval_breaks(-1.3, 1, 0.5)$major) <= -1.3)
check("prism_guide_if_safe: waiver without minor ticks (or fewer than two in range), guide_prism_minor with two or more",
      inherits(env$prism_guide_if_safe(numeric(0), 0, 1), "waiver") && inherits(env$prism_guide_if_safe(c(0.5), 0, 1), "waiver") &&
      inherits(env$prism_guide_if_safe(c(5, 6), 0, 1), "waiver") && inherits(env$prism_guide_if_safe(c(0.1, 0.2), 0, 1), "guide"))
check("interval_or_na: a blank, zero, negative or text box is auto", is.na(env$interval_or_na(NA)) && is.na(env$interval_or_na(0)) && is.na(env$interval_or_na(-1)) && is.na(env$interval_or_na("x")) && env$interval_or_na("0.5") == 0.5)
check("marker_label_y: the data rule is the v2.9 formula, the limits rule is 0.95 x top / 1.05 x bottom / geometric mean",
      abs(env$marker_label_y("top", c(0.01, 1), TRUE, "data") - 10^(log10(1) - 2 * 0.08)) < 1e-12 && abs(env$marker_label_y("bottom", c(0, 10), FALSE, "data") - 0.8) < 1e-12 &&
      abs(env$marker_label_y("top", c(0.01, 1), TRUE, "limits") - 0.95) < 1e-12 && abs(env$marker_label_y("bottom", c(0.01, 1), TRUE, "limits") - 0.0105) < 1e-12 &&
      abs(env$marker_label_y("middle", c(0.01, 1), TRUE, "limits") - 0.1) < 1e-12 && abs(env$marker_label_y("middle", c(0, 10), FALSE, "limits") - 5) < 1e-12)
check("time conversion factors and the x-label unit word", env$time_convert_factor("none") == 1 && abs(env$time_convert_factor("min_to_h") - 1 / 60) < 1e-15 && env$time_convert_factor("h_to_min") == 60 &&
      identical(env$convert_time_label(c("Time (minutes)", "Time (min)", "Time"), "min_to_h"), c("Time (hours)", "Time (h)", "Time")) &&
      identical(env$convert_time_label(c("Time (hours)", "Time (h)"), "h_to_min"), c("Time (minutes)", "Time (min)")) && identical(env$convert_time_label("Time (minutes)", "none"), "Time (minutes)"))

cat("\n--- palettes and the settings migration ---\n")
okabe <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
set1  <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF")
check("publication is Okabe-Ito (what it means in the N4 paper app)", identical(env$get_palette_colors("publication", 8), okabe) && identical(env$get_palette_colors("publication", 3), okabe[1:3]))
check("set1 is the colour set 'publication' used to be", identical(env$get_palette_colors("set1", 8), set1) && identical(env$get_palette_colors("set1", 10), rep_len(set1, 10)))
check("colorblind is unchanged", identical(env$get_palette_colors("colorblind", 8), okabe) && is.null(env$get_palette_colors("custom", 3)))
check("migrate_styling_settings: a file without styling_schema meant Set1 for 'publication'; a v2.22 file means Okabe-Ito",
      identical(env$migrate_styling_settings(list(color_palette = "publication"), NULL)$color_palette, "set1") &&
      identical(env$migrate_styling_settings(list(color_palette = "publication"), 222L)$color_palette, "publication") &&
      identical(env$migrate_styling_settings(list(color_palette = "viridis"), NULL)$color_palette, "viridis") && length(env$migrate_styling_settings(list(), NULL)) == 0)
check("shape codes 123 / 125 are hollow (white fill), the Filled box still opens the solid shapes",
      identical(env$resolve_point_shape(123, TRUE, "red"), list(shape = 23L, fill = "white")) && identical(env$resolve_point_shape(125, TRUE, "red"), list(shape = 25L, fill = "white")) &&
      identical(env$resolve_point_shape(16, FALSE, "red"), list(shape = 21L, fill = "white")) && identical(env$resolve_point_shape(17, TRUE, "red"), list(shape = 17L, fill = "red")) &&
      identical(env$resolve_point_shape(25, TRUE, "red"), list(shape = 25L, fill = "red")))

cat("\n--- matrix legend (pure) ---\n")
spec <- list(rows = c("Control", "*mltG*"), cols = c("0", "10"),
             cells = setNames(list(list(sample = "a", colour = "#000000", linetype = "solid", shape = 16, filled = TRUE),
                                   list(sample = "b", colour = "#E69F00", linetype = "dashed", shape = 17, filled = FALSE)),
                              c(env$matrix_legend_key("Control", "0"), env$matrix_legend_key("*mltG*", "10"))))
leg <- env$matrix_legend_grob(spec, list(title = "MOI", fontsize = 7))
check("matrix_legend_grob builds a gtable with a corner title, column and row labels and a cell per row x column",
      inherits(leg, "gtable") && sum(grepl("^mlcell-", leg$layout$name)) == 4 && sum(grepl("^mlcol-", leg$layout$name)) == 2 && sum(grepl("^mlrow-", leg$layout$name)) == 2 && "mltitle" %in% leg$layout$name)
check("an unfilled cell is a null grob; a defined one a line (+ point)", inherits(leg$grobs[[which(leg$layout$name == "mlcell-1-2")]], "null") && inherits(leg$grobs[[which(leg$layout$name == "mlcell-1-1")]], "gTree"))
check("hide_lines draws the point only; no spec / no rows gives NULL", {
  l2 <- env$matrix_legend_grob(spec, list(hide_lines = TRUE)); g1 <- l2$grobs[[which(l2$layout$name == "mlcell-1-1")]]
  inherits(g1, "points") && is.null(env$matrix_legend_grob(NULL)) && is.null(env$matrix_legend_grob(list(rows = character(0), cols = "a", cells = list()))) })
check("the box option adds a background grob", { lb <- env$matrix_legend_grob(spec, list(box = TRUE)); "matrix-legend-box" %in% lb$layout$name })
gp <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) + geom_point() + theme(legend.position = "none")
g0 <- ggplot2::ggplotGrob(gp)
gr <- env$attach_matrix_legend(g0, leg, "right"); gl <- env$attach_matrix_legend(g0, leg, "left"); gt <- env$attach_matrix_legend(g0, leg, "top"); gb <- env$attach_matrix_legend(g0, leg, "bottom")
check("attach_matrix_legend: right / left add a column, top / bottom a row, the panel cell is untouched",
      ncol(gr) == ncol(g0) + 1 && ncol(gl) == ncol(g0) + 1 && nrow(gt) == nrow(g0) + 1 && nrow(gb) == nrow(g0) + 1 && "matrix-legend" %in% gr$layout$name &&
      identical(sum(grepl("^panel", gr$layout$name)), 1L))
gi <- env$attach_matrix_legend(g0, leg, "inside", x = 0.9, y = 0.9)
check("attach_matrix_legend inside: same size, an overlay grob in the panel cell", ncol(gi) == ncol(g0) && nrow(gi) == nrow(g0) && "matrix-legend" %in% gi$layout$name)
check("a pinned panel keeps its size with the legend attached (fixed_panel_grob reads the matrix_legend attribute)", {
  pm <- gp; attr(pm, "matrix_legend") <- list(grob = leg, placement = "right")
  a <- quiet(env$fixed_panel_grob(gp, 50, 40)); b <- quiet(env$fixed_panel_grob(pm, 50, 40))
  wa <- attr(a, "size_mm"); wb <- attr(b, "size_mm"); wb[1] > wa[1] + 5 && abs(wb[2] - wa[2]) < 0.5 })

cat("\n--- other pure helpers ---\n")
rep_pd <- do.call(rbind, lapply(1:3, function(r) data.frame(time = 0:9 * 10, variable = "S", replicate = r, value = c(0.1, 0.12, 0.2, 0.4, 0.7, 0.9, 0.6, 0.3, 0.2, 0.1) + 0.02 * r)))
oc <- env$compute_od_rate_curves(rep_pd, 3)
check("compute_od_rate_curves: one row per sample and time with n, mean and SD (rolling regression, base R)", is.data.frame(oc) && all(c("time", "variable", "n", "rate_mean", "rate_sd") %in% names(oc)) && all(oc$n == 3) && nrow(oc) == 8)
check("dOD/dt is positive in the rise, negative in the lysis", oc$rate_mean[oc$time == 30] > 0 && oc$rate_mean[oc$time == 70] < 0)
check("dOD/dt of a straight line is its slope", { ln <- data.frame(time = 0:9, variable = "L", replicate = 1L, value = 2 * (0:9) + 1); r <- env$compute_od_rate_curves(ln, 5); all(abs(r$rate_mean - 2) < 1e-9) })
check("dOD/dt: too few points -> NULL", is.null(env$compute_od_rate_curves(rep_pd[rep_pd$time < 10, ], 5)) && is.null(env$compute_od_rate_curves(NULL)))
check("metric_sample_order: asc / desc by mean, alpha by label, data keeps the default", {
  sm <- data.frame(sample = c("a", "b", "c"), mean = c(3, 1, 2))
  identical(env$metric_sample_order(c("b", "a", "c"), sm, "asc"), c("b", "c", "a")) && identical(env$metric_sample_order(c("b", "a", "c"), sm, "desc"), c("a", "c", "b")) &&
    identical(env$metric_sample_order(c("b", "a", "c"), sm, "data"), c("b", "a", "c")) && identical(env$metric_sample_order(c("b", "C", "a"), sm, "alpha"), c("a", "b", "C")) })
check("parse_bar_colors: names with = / : / ;, bare hex, colour names; junk ignored", { pc <- env$parse_bar_colors("Control = #112233\nPhage: FF0000; bad = zzz\nBlue = steelblue")
  identical(unname(pc[c("Control", "Phage")]), c("#112233", "#FF0000")) && "Blue" %in% names(pc) && !"bad" %in% names(pc) && length(env$parse_bar_colors("")) == 0 })
check("heatmap_palette_option: the four palettes, anything else is viridis", identical(vapply(c("inferno", "plasma", "magma", "viridis", "x", NA), env$heatmap_palette_option, ""), c(inferno = "inferno", plasma = "plasma", magma = "magma", viridis = "viridis", x = "viridis", "viridis")) ||
      all(vapply(c("inferno", "plasma", "magma", "viridis"), function(x) identical(env$heatmap_palette_option(x), x), logical(1))) && identical(env$heatmap_palette_option("x"), "viridis"))
check("style buttons set only existing inputs, with the documented looks", {
  b <- env$band_style_settings("sem"); sl <- env$slide_style_settings(); pv <- env$pdf_vector_style_settings()
  isFALSE(b$show_points) && b$error_display_mode == "shadow" && b$error_type == "sem" && b$axis_frame == "L" && env$band_style_settings(NULL)$error_type == "sd" && env$band_style_settings("none")$error_type == "sd" &&
    sl$export_format == "png" && sl$export_width == 10 && sl$export_height == 7.5 && pv$export_format == "pdf" && pv$export_width == 7 && pv$export_height == 5 &&
    all(names(c(b, sl, pv)) %in% c(names(env$APP_DEFAULTS), "axis_frame")) })
check("facet_layers: columns, scales, strip style and size", {
  f1 <- env$facet_layers(c("A", "B"), scales = "free_y", ncol = 1, strip_style = "box", strip_size = 9, axis_lw = 0.5)
  f2 <- env$facet_layers(c("A", "B"), strip_style = "none"); f3 <- env$facet_layers(c("A", "B"))
  inherits(f1[[1]], "FacetWrap") && isTRUE(f1[[1]]$params$free$y) && isFALSE(f1[[1]]$params$free$x) && f1[[1]]$params$ncol == 1 && inherits(f1[[2]]$strip.background, "element_rect") && f1[[2]]$strip.text$size == 9 &&
    inherits(f2[[2]]$strip.text, "element_blank") && inherits(f3[[2]]$strip.background, "element_blank") && is.null(f3[[1]]$params$ncol) })
check("panel_assignment: group / manual / meta sources, blanks stay blank, unknown source falls to meta", {
  g <- c(a = "G1", b = ""); m <- c(a = "M1", b = "M2")
  identical(env$panel_assignment(c("a", "b"), "group", groups = g), c(a = "G1", b = "")) && identical(env$panel_assignment(c("a", "b"), "manual", manual = m), c(a = "M1", b = "M2")) &&
    identical(env$panel_assignment(c("a", "b"), "col", meta = m), c(a = "M1", b = "M2")) && is.null(env$panel_assignment("a", "group", groups = NULL)) })

cat("\n--- server: figure text markup ---\n")
layer_classes <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], ""))
real_fig <- function(p) inherits(p, "ggplot") && !is.null(quiet(ggplot_build(p))) &&
  !isTRUE(tryCatch(startsWith(p$layers[[1]]$aes_params$label, "Plot error"), error = function(e) FALSE))
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1])
  p0 <- generate_plot()
  check("default figure: real, the y label is the typeset wavelength (A[550]), the x label plain text, no title",
        real_fig(p0) && identical(deparse(p0$labels$y), "A[550]") && identical(p0$labels$x, "Time (minutes)"))
  session$setInputs(plot_title = "Growth of *E. coli* in 10^{-7} PFU", plot_subtitle = "OD_{600} **bold**", x_axis_label = "Time_{h} (min)", y_axis_label = "A550 *rel*")
  p1 <- generate_plot()
  check("title, subtitle, x and y labels with markup become plotmath calls", real_fig(p1) && is.call(p1$labels$title) && is.call(p1$labels$subtitle) && is.call(p1$labels$x) && is.call(p1$labels$y))
  check("the y label keeps the wavelength subscript next to the italic part", grepl("550", deparse(p1$labels$y)) && grepl("italic", deparse(p1$labels$y)))
  svgf <- of("markup.svg"); sz <- export_size_in()
  quiet(ggsave(svgf, main_plot_obj(), device = env$export_device("svg"), width = sz[1], height = sz[2], units = "in", bg = "white", limitsize = FALSE))
  check("the SVG export of a figure with markup is written and carries the subscript / italic text", file.size(svgf) > 1000 && any(grepl("font-style: italic", readLines(svgf), fixed = TRUE)))
  session$setInputs(plot_title = "", plot_subtitle = "", x_axis_label = "Time (minutes)", y_axis_label = "A550")
  nm <- smp[1]
  session$setInputs(qe_show_event = list(sample = nm, show = TRUE))
  rv$display_names <- setNames(c("Wild-type", "Δ*bshC*_{1}", "Phage ^{2}"), smp)
  p2 <- generate_plot(); b2 <- ggplot_build(p2)
  lbs <- p2$scales$get_scales("colour")$labels
  check("legend labels (display names from the quick-edit strip) with markup are plotmath, plain ones stay strings",
        is.list(lbs) && is.call(lbs[[2]]) && identical(lbs[[1]], "Wild-type"))
  session$setInputs(legend_title = "Strain", legend_position = "right")
  p2t <- generate_plot()
  check("legend title: the same name on the colour and the linetype scale (one key)", identical(p2t$scales$get_scales("colour")$name, "Strain") && identical(p2t$scales$get_scales("linetype")$name, "Strain"))
  session$setInputs(legend_title = "")
  check("legend title blank: no title (name NULL) as before", is.null(generate_plot()$scales$get_scales("colour")$name) || identical(generate_plot()$scales$get_scales("colour")$name, waiver()))
  session$setInputs(show_end_labels = TRUE)
  pe <- generate_plot(); el <- pe$layers[[which(layer_classes(pe) == "GeomTextRepel")]]
  elab <- layer_data(pe, which(layer_classes(pe) == "GeomTextRepel"))$label
  check("end labels use the DISPLAY names (was the raw column name), markup parsed", isTRUE(el$geom_params$parse) && length(elab) == 3 && any(grepl("Wild-type", elab, fixed = TRUE)) && !any(elab == smp[1]) && all(grepl("^\"|^[a-z]", elab)))
  session$setInputs(show_end_labels = FALSE)
  rv$display_names <- NULL
  session$setInputs(enable_time_markers = TRUE, marker_count = 1, marker_time_1 = 100, marker_line_type_1 = "dashed", marker_color_1 = "#FF0000", marker_size_1 = 0.5,
                    marker_label_1 = TRUE, marker_text_1 = "Phage *x*_{1}", marker_label_size_1 = 3, marker_label_position_1 = "top", marker_label_hjust_1 = 0)
  pm <- generate_plot(); ann <- layer_data(pm, which(layer_classes(pm) == "GeomText")[1])
  check("marker labels with markup are parsed text (annotate parse = TRUE)", real_fig(pm) && isTRUE(pm$layers[[which(layer_classes(pm) == "GeomText")[1]]]$geom_params$parse) && grepl("italic", as.character(ann$label[1])))
  y_data <- 10^ann$y[1]                            # layer data of a log axis is log10
  session$setInputs(marker_label_yplace = "limits")
  y_lim <- 10^layer_data(generate_plot(), which(layer_classes(pm) == "GeomText")[1])$y[1]
  check("marker label placement 'by the axis limits': 0.95 x the upper limit (log axis, 0.01 to 10 default range) instead of the data fraction", abs(y_lim - 0.95 * 10) < 1e-6 && abs(y_data - y_lim) > 1e-6)
  session$setInputs(marker_label_yplace = "data", enable_time_markers = FALSE, enable_threshold = TRUE, threshold_value = 0.2, threshold_label = "OD_{thr}", threshold_color = "custom",
                    threshold_color_custom = "#FF8800", threshold_width = 2)
  pt <- generate_plot(); lh <- which(layer_classes(pt) == "GeomHline")
  check("threshold line: custom HEX colour and an explicit width in pt; the label takes markup",
        real_fig(pt) && identical(pt$layers[[lh]]$aes_params$colour, "#FF8800") && abs(pt$layers[[lh]]$aes_params$linewidth - env$pt2lw(2)) < 1e-9 &&
        isTRUE(pt$layers[[which(layer_classes(pt) == "GeomText")[1]]]$geom_params$parse))
  session$setInputs(threshold_width = NA_real_, threshold_color = "grey40", enable_threshold = FALSE)
  pt2 <- generate_plot()
  check("threshold width blank (default) and no threshold: back to the plain figure", real_fig(pt2) && !"GeomHline" %in% layer_classes(pt2))
  fl <- figure_legend_string()
  check("figure legend text: markup as plain text, no braces", is.character(fl) && !grepl("[_^]\\{", fl))
  session$setInputs(y_axis_label = "OD_{600}")
  check("figure legend text uses the plain form of the y label", grepl("OD₆₀₀", figure_legend_string(), fixed = TRUE))
  session$setInputs(y_axis_label = "A550")
})

cat("\n--- server: error display modes, range / iqr ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1], y_scale_type = "linear")
  rpd <- prepare_replicate_data(); nrep <- length(unique(rpd$replicate)); rb <- env$replicate_bounds(rpd)
  p_bars <- generate_plot()
  check("default: error bars, the mean line, the points", real_fig(p_bars) && identical(layer_classes(p_bars), c("GeomErrorbar", "GeomLine", "GeomPoint")))
  session$setInputs(error_display_mode = "shadow")
  check("shadow: a ribbon instead of the error bars", identical(layer_classes(generate_plot()), c("GeomRibbon", "GeomLine", "GeomPoint")))
  session$setInputs(error_display_mode = "spaghetti")
  pS <- generate_plot(); lc <- layer_classes(pS)
  check("spaghetti: a trace per replicate behind the mean line (two GeomLine layers), no error bars", real_fig(pS) && identical(lc, c("GeomLine", "GeomLine", "GeomPoint")))
  ds <- layer_data(pS, 1)
  check("spaghetti: one group per sample x replicate, drawn with the trace alpha", length(unique(ds$group)) == length(smp) * nrep && all(abs(ds$alpha - 0.35) < 1e-9))
  session$setInputs(spaghetti_alpha = 0.6)
  check("spaghetti alpha follows the control", all(abs(layer_data(generate_plot(), 1)$alpha - 0.6) < 1e-9))
  session$setInputs(error_type = "none")
  check("spaghetti needs no error type: it still draws with error type none", identical(layer_classes(generate_plot()), c("GeomLine", "GeomLine", "GeomPoint")))
  session$setInputs(error_display_mode = "bars")
  check("error type none with the bars mode: nothing drawn (as before)", identical(layer_classes(generate_plot()), c("GeomLine", "GeomPoint")))
  session$setInputs(error_display_mode = "quantile_bands")
  pQ <- generate_plot()
  check("quantile bands: two ribbons (outer 2.5-97.5 %, inner IQR) with no error type, then the line and points", real_fig(pQ) && identical(layer_classes(pQ), c("GeomRibbon", "GeomRibbon", "GeomLine", "GeomPoint")))
  qo <- layer_data(pQ, 1); qi <- layer_data(pQ, 2)
  check("quantile bands: the inner band lies inside the outer one, with the two alphas", all(qi$ymin >= qo$ymin - 1e-9 & qi$ymax <= qo$ymax + 1e-9) && all(abs(qo$alpha - 0.15) < 1e-9) && all(abs(qi$alpha - 0.40) < 1e-9))
  check("quantile bands: the outer band is the 2.5 and 97.5 percentile of the replicates (band data groups are alphabetical)", {
    s1 <- sort(smp)[1]; bs <- rb[rb$variable == s1, ]; ii <- qo$group == 1; k <- match(round(qo$x[ii], 8), round(bs$time, 8))
    all(!is.na(k)) && all(abs(qo$ymin[ii] - bs$q025[k]) < 1e-9) && all(abs(qo$ymax[ii] - bs$q975[k]) < 1e-9) })
  session$setInputs(qband_inner_alpha = 0.5, qband_outer_alpha = 0.25)
  pQ2 <- generate_plot()
  check("quantile band alphas follow the controls", all(abs(layer_data(pQ2, 2)$alpha - 0.5) < 1e-9) && all(abs(layer_data(pQ2, 1)$alpha - 0.25) < 1e-9))
  session$setInputs(error_display_mode = "jitter", jitter_alpha = 0.3, jitter_size = 2, jitter_width = 0)
  pJ <- generate_plot(); lj <- layer_classes(pJ)
  check("jitter: the replicate values as an extra point layer with their own alpha and size", real_fig(pJ) && identical(lj, c("GeomPoint", "GeomLine", "GeomPoint")))
  dj <- layer_data(pJ, 1)
  check("jitter: alpha 0.3, size 2, hollow circles, one point per replicate reading", all(abs(dj$alpha - 0.3) < 1e-9) && all(dj$size == 2) && all(dj$shape == 1) && nrow(dj) == nrow(rpd))
  session$setInputs(jitter_width = 3)
  dj3 <- layer_data(generate_plot(), 1)
  check("jitter width 0 keeps the times, width 3 moves the points sideways", all(abs(sort(dj$x) - sort(rpd$time)) < 1e-9) && max(abs(sort(dj3$x) - sort(dj$x))) > 0.1)
  session$setInputs(error_display_mode = "jitter", show_replicate_points = TRUE)
  check("jitter mode takes over from the replicate-points alias (drawn once)", sum(layer_classes(generate_plot()) == "GeomPoint") == 2)
  session$setInputs(error_display_mode = "bars", error_type = "sem", show_replicate_points = TRUE)
  check("the show_replicate_points alias still draws the open circles behind the mean", identical(layer_classes(generate_plot()), c("GeomErrorbar", "GeomPoint", "GeomLine", "GeomPoint")))
  session$setInputs(show_replicate_points = FALSE, error_display_mode = "combo")
  pC <- generate_plot()
  check("combo: a lighter ribbon, the replicate traces, the line and the points", real_fig(pC) && identical(layer_classes(pC), c("GeomRibbon", "GeomLine", "GeomLine", "GeomPoint")) &&
        all(abs(layer_data(pC, 1)$alpha - 0.2 * 0.6) < 1e-9))
  session$setInputs(error_display_mode = "shadow", error_type = "range")
  pR <- generate_plot(); dR <- layer_data(pR, 1)
  check("range: the ribbon runs from the minimum to the maximum of the replicates", real_fig(pR) && {
    s1 <- smp[1]; bs <- rb[rb$variable == s1, ]; ii <- dR$group == 1; k <- match(round(dR$x[ii], 8), round(bs$time, 8))
    all(abs(dR$ymin[ii] - bs$min[k]) < 1e-9) && all(abs(dR$ymax[ii] - bs$max[k]) < 1e-9) })
  session$setInputs(error_display_mode = "bars", error_type = "iqr")
  pI <- generate_plot(); dI <- layer_data(pI, 1)
  check("iqr: the error bars run from the 25th to the 75th percentile (asymmetric about the mean)", real_fig(pI) && {
    s1 <- smp[1]; bs <- rb[rb$variable == s1, ]; ii <- dI$group == 1; k <- match(round(dI$x[ii], 8), round(bs$time, 8))
    all(abs(dI$ymin[ii] - bs$q25[k]) < 1e-9) && all(abs(dI$ymax[ii] - bs$q75[k]) < 1e-9) && any(abs((bs$mean[k] - bs$q25[k]) - (bs$q75[k] - bs$mean[k])) > 1e-6) })
  session$setInputs(error_multiplier = 2)
  dI2 <- layer_data(generate_plot(), 1)
  check("the multiplier scales the distance from the mean", {
    s1 <- smp[1]; bs <- rb[rb$variable == s1, ]; ii <- dI2$group == 1; k <- match(round(dI2$x[ii], 8), round(bs$time, 8))
    all(abs(dI2$ymax[ii] - (bs$mean[k] + 2 * (bs$q75[k] - bs$mean[k]))) < 1e-9) })
  session$setInputs(error_multiplier = 1, y_scale_type = "log", error_type = "range", error_log_floor = TRUE)
  pL <- generate_plot()
  check("range on a log axis: lower bounds are floored at the axis minimum, so the bars stay drawn", real_fig(pL) && all(layer_data(pL, 1)$ymin >= log10(0.01) - 1e-9))
  caps <- function(p) p$labels$caption
  session$setInputs(show_error_caption = TRUE)
  c_range <- caps(generate_plot())
  session$setInputs(error_type = "iqr"); c_iqr <- caps(generate_plot())
  session$setInputs(error_display_mode = "spaghetti"); c_sp <- caps(generate_plot())
  session$setInputs(error_display_mode = "quantile_bands"); c_q <- caps(generate_plot())
  check("caption: range and iqr are not 'mean +/- x'; spaghetti carries no error caption, the bands describe themselves",
        grepl("range \\(minimum to maximum\\)", c_range) && grepl("interquartile", c_iqr) && is.null(c_sp) && grepl("Bands", c_q))
  session$setInputs(show_error_caption = FALSE, error_type = "sem", error_display_mode = "bars", y_scale_type = "log")
  fl <- function(mode, et = "sem") { session$setInputs(error_display_mode = mode, error_type = et); figure_legend_string() }
  check("figure legend text names the measure: range, iqr, bands, spaghetti, jitter",
        grepl("the range", fl("bars", "range"), fixed = TRUE) && grepl("interquartile", fl("shadow", "iqr")) && grepl("shaded bands", fl("shadow", "sd")) &&
        grepl("individual replicate curves", fl("spaghetti")) && grepl("2.5th", fl("quantile_bands")) && grepl("individual replicate values", fl("jitter")) && !grepl("indicate", fl("spaghetti")))
  session$setInputs(error_display_mode = "bars", error_type = "range", analysis_metrics = c("lysis_time", "od_drop"))
  pms <- quiet(generate_analysis_plot("metric_summary"))
  rm_ <- replicate_metric_rows_current(); v10 <- rm_$lysis_time[rm_$sample == "Phage MOI 10"]
  check("metric summary: range whiskers run from the minimum to the maximum replicate value", real_fig(pms) && {
    eb <- layer_data(pms, which(layer_classes(pms) == "GeomErrorbar")[1]); any(abs(eb$ymin - min(v10)) < 1e-9 & abs(eb$ymax - max(v10)) < 1e-9) })
  session$setInputs(error_type = "iqr")
  pmi <- quiet(generate_analysis_plot("metric_summary")); q <- quantile(v10, c(0.25, 0.75), names = FALSE)
  check("metric summary: iqr whiskers are asymmetric about the mean", real_fig(pmi) && {
    eb <- layer_data(pmi, which(layer_classes(pmi) == "GeomErrorbar")[1]); any(abs(eb$ymin - q[1]) < 1e-9 & abs(eb$ymax - q[2]) < 1e-9) })
  session$setInputs(error_type = "sd", error_multiplier = 1)
  ehw <- error_half_width()
  check("error_half_width: the half width is what it was for sd, lower / upper are available, n < 2 is NA",
        abs(ehw(c(1, 2, 4, 9)) - sd(c(1, 2, 4, 9))) < 1e-12 && abs(ehw(c(1, 2, 4, 9), "lower") - ehw(c(1, 2, 4, 9), "upper")) < 1e-12 && is.na(ehw(1)))
  session$setInputs(error_type = "sem", error_display_mode = "bars")

  for (md in c("bars", "shadow", "spaghetti", "quantile_bands", "jitter", "combo")) {
    session$setInputs(error_display_mode = md, y_scale_type = "log")
    g <- main_plot_obj(); sz <- export_size_in()
    f <- of(paste0("mode_", md, ".svg")); quiet(ggsave(f, g, device = env$export_device("svg"), width = sz[1], height = sz[2], units = "in", bg = "white", limitsize = FALSE))
    f2 <- of(paste0("mode_", md, ".png")); quiet(ggsave(f2, g, device = env$export_device("png"), width = sz[1], height = sz[2], units = "in", dpi = 100, bg = "white", limitsize = FALSE))
    aes_k <- resolve_aesthetics(smp[1:2]); pdk <- prepare_plot_data(samples = smp[1:2])
    pk <- build_plot(pdk[pdk$variable %in% smp[1:2], ], smp[1:2], aes_k)
    check(paste0("mode ", md, ": SVG and PNG exports and the cumulative (PPTX / GIF) build"), file.size(f) > 1000 && file.size(f2) > 500 && real_fig(pk))
  }
  session$setInputs(error_display_mode = "spaghetti")
  gf <- main_plot_obj()
  check("fixed panel with spaghetti: still a fixed-panel figure with a size", inherits(gf, "gtable") && length(attr(gf, "size_mm")) == 2)
  session$setInputs(error_display_mode = "bars")
})

cat("\n--- server: panels, axis frame ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1])
  gid <- function(s) paste0("group_", env$safe_id(s)); fid <- function(s) paste0("facet_of_", env$safe_id(s))
  set_in(session, gid(smp[1]), "Control", gid(smp[2]), "Phage", gid(smp[3]), "Phage", "facet_by_group", TRUE)
  p1 <- generate_plot(); b1 <- ggplot_build(p1)
  check("panels by group: two panels, plain strips as before (no background, axis-title size), shared scales, auto columns",
        real_fig(p1) && nrow(b1$layout$layout) == 2 && inherits(p1$theme$strip.background, "element_blank") && p1$theme$strip.text$size == 7 && isFALSE(p1$facet$params$free$y) && is.null(p1$facet$params$ncol))
  session$setInputs(facet_ncol = 1)
  b2 <- ggplot_build(generate_plot())
  check("panels per row = 1: the two panels are stacked", identical(sort(unique(b2$layout$layout$COL)), 1L) && identical(sort(unique(b2$layout$layout$ROW)), 1:2))
  session$setInputs(facet_ncol = 2, facet_scales = "free_y")
  p3 <- generate_plot()
  check("free y scales: free$y is TRUE, free$x FALSE", isTRUE(p3$facet$params$free$y) && isFALSE(p3$facet$params$free$x) && real_fig(p3))
  session$setInputs(facet_scales = "free"); pfr <- generate_plot()
  check("free both", isTRUE(pfr$facet$params$free$x) && isTRUE(pfr$facet$params$free$y))
  session$setInputs(facet_scales = "free_x"); pfx <- generate_plot()
  check("free x", isTRUE(pfx$facet$params$free$x) && isFALSE(pfx$facet$params$free$y))
  session$setInputs(facet_scales = "fixed", facet_strip_style = "box", facet_strip_size = 11)
  p4 <- generate_plot()
  check("strip style box: a framed strip background; the strip size follows the box", inherits(p4$theme$strip.background, "element_rect") && p4$theme$strip.text$size == 11)
  session$setInputs(facet_strip_style = "none"); p5 <- generate_plot()
  check("strip style none: no strip text, no background", inherits(p5$theme$strip.text, "element_blank") && inherits(p5$theme$strip.background, "element_blank"))
  session$setInputs(facet_strip_style = "plain", facet_strip_size = NA_real_)
  set_in(session, gid(smp[1]), "*mltG*", gid(smp[2]), "A_{1}", gid(smp[3]), "A_{1}")
  p6 <- generate_plot(); sv <- of("strips.svg"); quiet(ggsave(sv, p6, device = svglite::svglite, width = 5, height = 3))
  check("strip labels with markup are parsed (the figure draws; italic text in the SVG)", real_fig(p6) && any(grepl("font-style: italic", readLines(sv), fixed = TRUE)))
  set_in(session, gid(smp[1]), "Control", gid(smp[2]), "Phage", gid(smp[3]), "Phage")
  set_in(session, "facet_source", "manual", fid(smp[1]), "Left", fid(smp[2]), "Right", fid(smp[3]), "Right")
  pm <- generate_plot(); bm <- ggplot_build(pm)
  check("manual panel assignment: the typed panel names are the strips, the Group boxes are ignored", nrow(bm$layout$layout) == 2 && identical(sort(as.character(bm$layout$layout$group)), c("Left", "Right")))
  set_in(session, fid(smp[1]), "Same", fid(smp[2]), "Same", fid(smp[3]), "Same")
  check("manual: one distinct panel is no facet", inherits(generate_plot()$facet, "FacetNull"))
  set_in(session, "facet_source", "group", fid(smp[1]), "", fid(smp[2]), "", fid(smp[3]), "")
  session$setInputs(panel_size_mode = "fixed", panel_width_mm = 60, panel_height_mm = 60, panel_square = TRUE, facet_ncol = 2)
  gf <- main_fixed_info()
  pan <- gf$layout[grepl("^panel", gf$layout$name), ]
  wpan <- sum(as.numeric(grid::convertWidth(gf$widths[sort(unique(unlist(Map(seq, pan$l, pan$r))))], "mm")))
  hpan <- sum(as.numeric(grid::convertHeight(gf$heights[sort(unique(unlist(Map(seq, pan$t, pan$b))))], "mm")))
  check("fixed panel size with two columns: every sub-panel is pinned and the panel area stays 60 x 60 mm", nrow(pan) == 2 && abs(wpan - 60) < 0.5 && abs(hpan - 60) < 0.5)
  session$setInputs(facet_scales = "free_y", facet_ncol = 1)
  gf2 <- main_fixed_info(); pan2 <- gf2$layout[grepl("^panel", gf2$layout$name), ]
  hpan2 <- sum(as.numeric(grid::convertHeight(gf2$heights[sort(unique(unlist(Map(seq, pan2$t, pan2$b))))], "mm")))
  check("fixed panel with free y and one column: stacked panels share the 60 mm height", nrow(pan2) == 2 && abs(hpan2 - 60) < 0.5 && all(is.finite(attr(gf2, "size_mm"))))
  svgf <- of("facets_fixed.svg"); sz <- export_size_in(); quiet(ggsave(svgf, gf2, device = env$export_device("svg"), width = sz[1], height = sz[2], units = "in", bg = "white", limitsize = FALSE))
  check("the faceted fixed figure exports", file.size(svgf) > 1000)
  session$setInputs(panel_size_mode = "fit", facet_by_group = FALSE, facet_scales = "fixed", facet_ncol = NA_real_)

  pa <- generate_plot()
  check("axis frame auto (default): the box (panel.border) as before", inherits(pa$theme$panel.border, "element_rect"))
  session$setInputs(use_advanced_ticks = FALSE)
  check("auto with the advanced-ticks box off: the box and the axis lines (the pubr look) as before", inherits(generate_plot()$theme$panel.border, "element_rect") && inherits(generate_plot()$theme$axis.line, "element_line"))
  session$setInputs(use_advanced_ticks = TRUE, axis_frame = "L")
  pl <- generate_plot()
  check("axis frame L: no panel border, axis lines left and bottom", inherits(pl$theme$panel.border, "element_blank") && inherits(pl$theme$axis.line, "element_line"))
  session$setInputs(axis_frame = "none"); pn <- generate_plot()
  check("axis frame none: no border and no axis lines", inherits(pn$theme$panel.border, "element_blank") && inherits(pn$theme$axis.line, "element_blank"))
  session$setInputs(axis_frame = "box", use_advanced_ticks = FALSE); pb <- generate_plot()
  check("axis frame box is independent of the advanced-ticks box (no extra axis lines with pubr)", inherits(pb$theme$panel.border, "element_rect") && inherits(pb$theme$axis.line, "element_blank"))
  session$setInputs(axis_frame = "L", use_advanced_ticks = FALSE)
  check("axis frame L with the advanced-ticks box off", inherits(generate_plot()$theme$panel.border, "element_blank") && inherits(generate_plot()$theme$axis.line, "element_line"))
  fL <- of("frame_L.svg"); fB <- of("frame_box.svg")
  session$setInputs(use_advanced_ticks = TRUE, axis_frame = "L"); quiet(ggsave(fL, generate_plot(), device = svglite::svglite, width = 4, height = 3))
  session$setInputs(axis_frame = "auto"); quiet(ggsave(fB, generate_plot(), device = svglite::svglite, width = 4, height = 3))
  check("the L frame changes the rendered figure", !identical(readLines(fL), readLines(fB)))
  session$setInputs(axis_frame = "L")
  check("the analysis figures follow the same frame setting (shared theme)", inherits(quiet(generate_analysis_plot("growth_rate"))$theme$panel.border, "element_blank"))
  session$setInputs(axis_frame = "auto")
})


cat("\n--- server: legend options, hide lines, matrix legend, shapes ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1])
  p0 <- generate_plot()
  check("legend text face plain (default) is the theme's plain; bold / italic reach legend.text and legend.title",
        identical(p0$theme$legend.text$face, "plain") && { session$setInputs(legend_text_face = "bold"); g <- generate_plot(); identical(g$theme$legend.text$face, "bold") && identical(g$theme$legend.title$face, "bold") } &&
        { session$setInputs(legend_text_face = "italic"); identical(generate_plot()$theme$legend.text$face, "italic") })
  session$setInputs(legend_text_face = "plain")
  session$setInputs(legend_wrap_width = 8)
  lw <- generate_plot()$scales$get_scales("colour")$labels
  check("legend wrap width: long display names are wrapped on the colour and the linetype scale", any(grepl("\n", unlist(lw)))
        && identical(generate_plot()$scales$get_scales("linetype")$labels, lw))
  session$setInputs(legend_wrap_width = NA_real_)
  check("legend wrap blank: the labels are the display names untouched", identical(unname(generate_plot()$scales$get_scales("colour")$labels), unname(resolve_aesthetics(smp)$leg_labels)))
  rv$display_names <- setNames(c("Control", "Phage 10", "Phage 0.1"), smp)
  session$setInputs(legend_position = "direct")
  pd_ <- generate_plot(); lcd <- layer_classes(pd_); ge <- layer_data(pd_, which(lcd == "GeomTextRepel"))
  check("direct labels at line ends: the legend is hidden, a repelled label per line with the DISPLAY names",
        real_fig(pd_) && identical(pd_$theme$legend.position, "none") && "GeomTextRepel" %in% lcd && setequal(ge$label, c("Control", "Phage 10", "Phage 0.1")))
  session$setInputs(legend_position = "right", show_end_labels = TRUE)
  ge2 <- layer_data(generate_plot(), which(layer_classes(generate_plot()) == "GeomTextRepel"))
  check("the end-labels checkbox uses the display names too (was the raw column name)", setequal(ge2$label, c("Control", "Phage 10", "Phage 0.1")))
  session$setInputs(show_end_labels = FALSE, legend_position = "direct", panel_size_mode = "fixed")
  svd <- of("direct.svg"); sz <- export_size_in(); quiet(ggsave(svd, main_plot_obj(), device = env$export_device("svg"), width = sz[1], height = sz[2], units = "in", bg = "white", limitsize = FALSE))
  check("direct labels export (the fixed panel figure carries no legend box, the names are in the SVG)", file.size(svd) > 1000 && any(grepl("Phage 10", readLines(svd), fixed = TRUE)))
  fig_dir <- quiet(generate_analysis_plot("growth_rate"))
  check("an analysis figure has no end labels: 'direct' falls back to an ordinary key on the right", identical(fig_dir$theme$legend.position, "right"))
  session$setInputs(legend_position = "right", panel_size_mode = "fit")
  cat("   (hide lines)\n")
  session$setInputs(hide_lines = TRUE)
  ph <- generate_plot()
  check("hide lines: no GeomLine, the points and the error bars stay", real_fig(ph) && !"GeomLine" %in% layer_classes(ph) && all(c("GeomPoint", "GeomErrorbar") %in% layer_classes(ph)))
  check("hide lines: the error bar key is drawn as a point (legend glyph)", grepl("pointsGrob", paste(capture.output(print(ph$layers[[which(layer_classes(ph) == "GeomErrorbar")]]$geom$draw_key)), collapse = "")))
  session$setInputs(show_points = FALSE)
  ph2 <- generate_plot()
  check("hide lines with points off: error bars only, the figure still builds", real_fig(ph2) && identical(layer_classes(ph2), "GeomErrorbar"))
  session$setInputs(show_points = TRUE, hide_lines = FALSE)
  check("hide lines off: the line is back", "GeomLine" %in% layer_classes(generate_plot()))
  cat("   (hollow shapes)\n")
  set_in(session, paste0("shape_", env$safe_id(smp[2])), "123", paste0("shape_", env$safe_id(smp[3])), "125")
  psh <- generate_plot(); dp <- layer_data(psh, which(layer_classes(psh) == "GeomPoint")[1])
  check("open diamond (123) and open down-triangle (125) are plotted as shapes 23 and 25 with a white fill", real_fig(psh) && all(c(23, 25) %in% dp$shape) && all(dp$fill[dp$shape %in% c(23, 25)] == "white"))
  session$setInputs(show_points = FALSE)
  check("hollow shapes with points hidden: the legend shape scale still builds", real_fig(generate_plot()))
  session$setInputs(show_points = TRUE)
  set_in(session, paste0("shape_", env$safe_id(smp[2])), "17", paste0("shape_", env$safe_id(smp[3])), "15")

  cat("   (matrix legend)\n")
  rid <- function(s) paste0("legrow_", env$safe_id(s)); cid <- function(s) paste0("legcol_", env$safe_id(s))
  session$setInputs(legend_position = "matrix")
  pn <- generate_plot()
  check("matrix legend with nothing assigned yet: the ordinary key on the right, no attribute", identical(pn$theme$legend.position, "right") && is.null(attr(pn, "matrix_legend")))
  set_in(session, rid(smp[1]), "Control", cid(smp[1]), "0", rid(smp[2]), "*Phage*", cid(smp[2]), "10", rid(smp[3]), "*Phage*", cid(smp[3]), "0.1")
  pm <- generate_plot(); ml <- attr(pm, "matrix_legend")
  check("matrix legend: the plot carries the legend gtable and hides its own key", real_fig(pm) && !is.null(ml) && inherits(ml$grob, "gtable") && identical(pm$theme$legend.position, "none") && identical(ml$placement, "right"))
  check("the matrix: rows Control and *Phage*, columns 0 / 10 / 0.1, a swatch per assigned sample, in the sample colours",
        sum(grepl("^mlcell-", ml$grob$layout$name)) == 6 && sum(grepl("^mlrow-", ml$grob$layout$name)) == 2 && sum(grepl("^mlcol-", ml$grob$layout$name)) == 3)
  session$setInputs(panel_size_mode = "fixed", panel_width_mm = 60, panel_square = TRUE)
  w_mat <- attr(main_plot_obj(), "size_mm")[1]
  session$setInputs(legend_position = "none"); w_none <- attr(main_plot_obj(), "size_mm")[1]
  session$setInputs(legend_position = "matrix")
  gm <- main_plot_obj()
  check("fixed panel: the matrix legend is a gtable track beside the panel, the figure grows, the panel stays 60 mm",
        inherits(gm, "gtable") && "matrix-legend" %in% gm$layout$name && w_mat > w_none + 10 && {
          pan <- gm$layout[grepl("^panel", gm$layout$name), ]; abs(sum(as.numeric(grid::convertWidth(gm$widths[unique(unlist(Map(seq, pan$l, pan$r)))], "mm"))) - 60) < 0.5 })
  session$setInputs(matrix_legend_placement = "top")
  gt <- main_plot_obj(); h_top <- attr(gt, "size_mm")[2]
  session$setInputs(matrix_legend_placement = "inside"); gi <- main_plot_obj()
  check("placement top adds height, inside overlays the panel (the size is that of no legend)", h_top > attr(gi, "size_mm")[2] + 5 && abs(attr(gi, "size_mm")[1] - w_none) < 1)
  session$setInputs(matrix_legend_placement = "right")
  fsv <- of("matrix.svg"); fpng <- of("matrix.png"); sz <- export_size_in()
  quiet(ggsave(fsv, main_plot_obj(), device = env$export_device("svg"), width = sz[1], height = sz[2], units = "in", bg = "white", limitsize = FALSE))
  quiet(ggsave(fpng, main_plot_obj(), device = env$export_device("png"), width = sz[1], height = sz[2], units = "in", dpi = 100, bg = "white", limitsize = FALSE))
  check("the matrix legend exports (SVG carries the column labels; PNG written)", file.size(fsv) > 1000 && file.size(fpng) > 500 && any(grepl(">10<", readLines(fsv), fixed = TRUE)))
  session$setInputs(panel_size_mode = "fit")
  gfit <- main_plot_obj()
  check("fit mode with a matrix legend: the figure is a gtable with the legend attached", inherits(gfit, "gtable") && "matrix-legend" %in% gfit$layout$name)
  sfit <- of("matrix_fit.svg"); sz <- export_size_in(); quiet(ggsave(sfit, gfit, device = env$export_device("svg"), width = sz[1], height = sz[2], units = "in", bg = "white", limitsize = FALSE))
  check("fit mode export works", file.size(sfit) > 1000)
  if (env$has_officer && env$has_rvg) {
    pp <- tryCatch(output$downloadPPTX, error = function(e) e)
    check("PPTX with a matrix legend builds (cumulative slides, the legend drawn by grid)", is.character(pp) && file.exists(pp) && file.size(pp) > 5000)
  } else skipit("officer / rvg not installed: PPTX with a matrix legend")
  if (env$has_gifski) {
    session$setInputs(gif_width = 300, gif_height = 220, gif_fps = 5)
    gg <- tryCatch(output$downloadGIF, error = function(e) e)
    check("GIF with a matrix legend builds", is.character(gg) && file.exists(gg) && file.size(gg) > 1000)
  } else skipit("gifski not installed: GIF with a matrix legend")
  set_in(session, paste0("legcol_", env$safe_id(smp[3])), "")
  ml2 <- attr(generate_plot(), "matrix_legend")
  check("a sample with a blank column is left out of the matrix", sum(grepl("^mlcell-", ml2$grob$layout$name)) == 4)
  session$setInputs(legend_position = "right"); rv$display_names <- NULL
})

cat("\n--- server: axis intervals, time unit ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1], y_scale_type = "linear")
  sc_y <- function(p) p$scales$get_scales("y"); sc_x <- function(p) p$scales$get_scales("x")
  p0 <- generate_plot()
  check("blank y intervals (default): ggplot's own breaks", inherits(sc_y(p0)$breaks, "waiver") && inherits(sc_y(p0)$minor_breaks, "waiver"))
  session$setInputs(y_tick_interval = 0.5, y_minor_interval = 0.1)
  p1 <- generate_plot(); yl <- c(0, 2)
  check("linear y: the major and minor intervals become the breaks, with the prism minor guide", real_fig(p1) && isTRUE(all.equal(diff(sc_y(p1)$breaks[1:3]), c(0.5, 0.5))) &&
        isTRUE(all.equal(diff(sc_y(p1)$minor_breaks[1:3]), c(0.1, 0.1))) && inherits(sc_y(p1)$guide, "guide") && inherits(sc_y(p1)$guide, "prism_minor"))
  check("no minor tick sits on a major one", !any(vapply(sc_y(p1)$minor_breaks, function(m) any(abs(m - sc_y(p1)$breaks) < 1e-9), logical(1))))
  session$setInputs(y_tick_interval = NA_real_, y_minor_interval = 0.05)
  p2 <- generate_plot()
  check("only a minor interval: the major breaks stay automatic", real_fig(p2) && inherits(sc_y(p2)$breaks, "waiver") && is.numeric(sc_y(p2)$minor_breaks))
  session$setInputs(y_tick_interval = 1e-6, y_minor_interval = NA_real_)
  p3 <- generate_plot()
  check("a hopeless interval (millions of ticks) is guarded: the default breaks, the figure still renders", real_fig(p3) && inherits(sc_y(p3)$breaks, "waiver"))
  session$setInputs(y_tick_interval = 0.5, y_minor_interval = 0.1, y_scale_type = "sqrt")
  p4 <- generate_plot()
  check("sqrt y takes the intervals too", real_fig(p4) && isTRUE(all.equal(diff(sc_y(p4)$breaks[1:2]), 0.5)))
  session$setInputs(y_scale_type = "log", y_tick_interval = 0.5)
  check("log y ignores the linear intervals (its own decade ticks)", real_fig(generate_plot()) && is.null(tryCatch(sc_y(generate_plot())$breaks[["x"]], error = function(e) NULL)))
  session$setInputs(y_scale_type = "linear", y_tick_interval = NA_real_, y_minor_interval = NA_real_, x_tick_interval = 20)
  p5 <- generate_plot(); nm0 <- length(sc_x(p5)$minor_breaks)
  session$setInputs(x_minor_interval = 4)
  p6 <- generate_plot()
  check("x minor interval: minor ticks every 4 between the majors, a different set from the automatic one",
        real_fig(p6) && is.numeric(sc_x(p6)$minor_breaks) && isTRUE(all.equal(diff(sc_x(p6)$minor_breaks[1:3]), c(4, 4))) && length(sc_x(p6)$minor_breaks) != nm0)
  session$setInputs(x_minor_interval = 1e-6)
  check("a hopeless x minor interval is guarded (the automatic minors stay)", real_fig(generate_plot()) && length(sc_x(generate_plot())$minor_breaks) == nm0)
  session$setInputs(x_minor_interval = NA_real_, x_tick_interval = NA_real_)

  cat("   (time-unit conversion)\n")
  t_file <- max(prepare_plot_data()$time); r0 <- replicate_metric_rows_current()
  session$setInputs(time_convert = "min_to_h")
  pdh <- prepare_plot_data(); rph <- prepare_replicate_data()
  check("minutes to hours: the plotted and the replicate times are the file's / 60, the file data are untouched", abs(max(pdh$time) - t_file / 60) < 1e-9 && abs(max(rph$time) - t_file / 60) < 1e-9 && abs(max(rv$all_timepoints) - t_file) < 1e-9)
  ph <- generate_plot()
  check("the x label follows (Time (minutes) -> Time (hours)); the typed label box is untouched", identical(ph$labels$x, "Time (hours)") && identical(input$x_axis_label, "Time (minutes)"))
  check("the main figure's x data are in hours", max(layer_data(ph, which(layer_classes(ph) == "GeomLine")[1])$x) < t_file / 60 + 1e-6)
  rh <- replicate_metric_rows_current()
  check("the metrics' time columns are converted with their labels: lysis onset in hours = minutes / 60", {
    a <- r0$lysis_time[is.finite(r0$lysis_time)]; b <- rh$lysis_time[is.finite(rh$lysis_time)]; length(a) > 0 && isTRUE(all.equal(sort(b) * 60, sort(a), tolerance = 1e-9)) })
  pms <- quiet(generate_analysis_plot("metric_summary"))
  check("the metric summary labels its time metrics in hours", any(grepl("Lysis onset (h)", levels(pms$data$metric), fixed = TRUE)))
  grh <- quiet(generate_analysis_plot("growth_rate"))
  check("the growth-rate figure: x axis and rate unit both in hours", real_fig(grh) && grepl("h", grh$labels$y) && identical(grh$labels$x, "Time (hours)"))
  svt <- of("hours.svg"); sz <- export_size_in(); quiet(ggsave(svt, main_plot_obj(), device = env$export_device("svg"), width = sz[1], height = sz[2], units = "in", bg = "white", limitsize = FALSE))
  check("the converted figure exports", file.size(svt) > 1000 && any(grepl("hours", readLines(svt), fixed = TRUE)))
  session$setInputs(time_convert = "h_to_min")
  check("hours to minutes multiplies by 60", abs(max(prepare_plot_data()$time) - t_file * 60) < 1e-6 && identical(generate_plot()$labels$x, "Time (minutes)"))
  session$setInputs(time_convert = "none")
  check("none: the file's times and label again", abs(max(prepare_plot_data()$time) - t_file) < 1e-12 && identical(generate_plot()$labels$x, "Time (minutes)"))
})

cat("\n--- server: palettes, settings, style buttons ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1])
  session$setInputs(color_palette = "publication")
  check("the publication palette is Okabe-Ito", identical(rv$palette_colors, okabe[1:3]) && identical(unname(resolve_aesthetics(smp)$colors), okabe[1:3]))
  session$setInputs(color_palette = "set1")
  check("the set1 palette is the old publication set", identical(rv$palette_colors, set1[1:3]))
  session$setInputs(color_palette = "colorblind")
  js <- settings_json_list()
  check("saved settings: styling_schema marks the new palette meaning; the v2.22 inputs are saved as globals",
        identical(js$styling_schema, 222L) && all(c("axis_frame", "facet_source", "hide_lines", "legend_title", "time_convert", "metric_sort") %in% names(js$global_settings)))
  gid <- function(s) paste0("facet_of_", env$safe_id(s))
  set_in(session, gid(smp[1]), "Left", paste0("legrow_", env$safe_id(smp[1])), "R1", paste0("legcol_", env$safe_id(smp[1])), "C1")
  js2 <- settings_json_list()
  check("per-sample panel / matrix row / column are saved with the sample (not as globals)",
        identical(js2$sample_aesthetics[[smp[1]]]$panel, "Left") && identical(js2$sample_aesthetics[[smp[1]]]$leg_row, "R1") && identical(js2$sample_aesthetics[[smp[1]]]$leg_col, "C1") &&
        !any(grepl("^(facet_of_|legrow_|legcol_)", names(js2$global_settings))))
  f_old <- of("old_publication.json")
  jsonlite::write_json(list(version = "2.21.0", saved_at = "x", global_settings = list(color_palette = "publication", line_thickness = 1), sample_aesthetics = list()), f_old, auto_unbox = TRUE)
  session$setInputs(loadSettings = list(name = "old.json", datapath = f_old, size = file.size(f_old), type = "application/json"))
  check("loading a pre-v2.22 file: 'publication' is migrated to set1 (it meant Set1 then)", identical(rv$imported_settings$global$color_palette, "set1") && identical(rv$settings_status$type, "ok"))
  f_new <- of("new_publication.json")
  jsonlite::write_json(list(version = "2.22.0", styling_schema = 222L, saved_at = "x", global_settings = list(color_palette = "publication"), sample_aesthetics = list()), f_new, auto_unbox = TRUE)
  session$setInputs(loadSettings = list(name = "new.json", datapath = f_new, size = file.size(f_new), type = "application/json"))
  check("loading a v2.22 file: 'publication' stays Okabe-Ito", identical(rv$imported_settings$global$color_palette, "publication"))
  f_samp <- of("samp.json")
  jsonlite::write_json(list(version = "2.22.0", styling_schema = 222L, saved_at = "x", global_settings = list(axis_frame = "L"),
                            sample_aesthetics = setNames(list(list(color = "#112233", panel = "Right", leg_row = "R", leg_col = "C")), smp[2])), f_samp, auto_unbox = TRUE)
  session$setInputs(loadSettings = list(name = "samp.json", datapath = f_samp, size = file.size(f_samp), type = "application/json"))
  check("loading per-sample panel / row / column keys works (no error, status ok)", identical(rv$settings_status$type, "ok") && identical(rv$imported_settings$samples[[smp[2]]]$panel, "Right"))
  session$setInputs(apply_style_band = 1)
  session$setInputs(apply_style_slide = 1)
  session$setInputs(apply_style_pdf = 1)
  check("the three style buttons run without error and are not saved as settings", !any(c("apply_style_band", "apply_style_slide", "apply_style_pdf") %in% names(settings_json_list()$global_settings)))
})

cat("\n--- server: dOD/dt, metric summary extras, heatmap palette ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1],
                    analysis_metrics = c("lysis_time", "od_drop"), y_scale_type = "log")
  pr <- quiet(generate_analysis_plot("od_rate"))
  check("dOD/dt figure: a real figure (zero line, band, mean lines) with the rate on the y axis and the time unit", real_fig(pr) && identical(layer_classes(pr), c("GeomHline", "GeomRibbon", "GeomLine")) &&
        grepl("dOD/dt", deparse(pr$labels$y), fixed = TRUE) && grepl("min", deparse(pr$labels$y), fixed = TRUE))
  oc <- env$compute_od_rate_curves(prepare_replicate_data(), 5)
  dl <- layer_data(pr, 3)
  check("dOD/dt figure: the plotted curve is the mean rate of the per-replicate rolling regression", nrow(dl) == nrow(oc) && isTRUE(all.equal(sort(dl$y), sort(oc$rate_mean))) && layer_data(pr, 1)$yintercept == 0)
  check("dOD/dt of the lysing sample is negative somewhere, of the uninfected control it never is (beyond noise)", any(oc$rate_mean[oc$variable == smp[2]] < 0) && all(oc$rate_mean[oc$variable == smp[1]] > -0.005))
  session$setInputs(panel_size_mode = "fixed", panel_width_mm = 60, panel_square = TRUE, analysis_type = "od_rate")
  ga <- analysis_fixed_info()
  check("dOD/dt in the fixed panel: a square curve figure", inherits(ga, "gtable") && identical(panel_dims_mm("od_rate"), c(60, 60)) && length(attr(ga, "size_mm")) == 2)
  session$setInputs(compose_panels = c("main", "od_rate"), compose_ncol = "2")
  ci <- compose_info()
  check("the composer takes the dOD/dt panel", ci$n == 2 && "od_rate" %in% ci$types && !length(ci$skipped))
  rp <- report_analysis_payload(render_png = FALSE)
  check("reports: the dOD/dt figure follows the growth rate, with a caption", "od_rate" %in% vapply(rp$figures, function(f) f$type, "") && grepl("dOD/dt", rp$figures[[which(vapply(rp$figures, function(f) f$type, "") == "od_rate")]]$caption, fixed = TRUE) &&
        which(vapply(rp$figures, function(f) f$type, "") == "od_rate") == which(vapply(rp$figures, function(f) f$type, "") == "growth_rate") + 1)
  zf <- output$downloadAllFigures
  zl <- if (requireNamespace("zip", quietly = TRUE)) basename(zip::zip_list(zf)$filename) else basename(sub("^\\s*[0-9]+\\s+[0-9-]+\\s+[0-9:]+\\s+", "", system2("unzip", c("-l", zf), stdout = TRUE)))
  check("the download-all archive holds od_rate as SVG and PNG", any(grepl("^od_rate_.*\\.svg$", zl)) && any(grepl("^od_rate_.*\\.png$", zl)))
  session$setInputs(panel_size_mode = "fit")

  cat("   (metric summary extras)\n")
  m0 <- quiet(generate_analysis_plot("metric_summary"))
  lv0 <- levels(m0$data$sample)
  check("metric summary default: the legend order (selected), the vertical layout", identical(lv0, smp) && inherits(m0$coordinates, "CoordCartesian"))
  rm_ <- replicate_metric_rows_current()
  session$setInputs(metric_sort = "alpha"); la <- levels(quiet(generate_analysis_plot("metric_summary"))$data$sample)
  check("sort alphabetical", identical(la, smp[order(tolower(smp))]))
  session$setInputs(metric_sort = "asc"); lasc <- levels(quiet(generate_analysis_plot("metric_summary"))$data$sample)
  session$setInputs(metric_sort = "desc"); ldes <- levels(quiet(generate_analysis_plot("metric_summary"))$data$sample)
  check("sort by value: both directions list every sample", setequal(lasc, smp) && setequal(ldes, smp) && !identical(lasc, ldes))
  ex1 <- vapply(smp, function(s_) { v <- rm_$lysis_time[rm_$sample == s_]; v <- v[is.finite(v)]; if (length(v)) mean(v) else NA_real_ }, 0)
  check("sort by value follows the mean of the first metric shown (a sample without a value goes last)",
        identical(lasc, smp[order(ifelse(is.na(ex1), Inf, ex1))]) && identical(ldes, smp[order(ifelse(is.na(ex1), -Inf, ex1), decreasing = TRUE)]))
  session$setInputs(metric_sort = "data", metric_horizontal = TRUE)
  mh <- quiet(generate_analysis_plot("metric_summary"))
  check("horizontal bars: coord_flip, the first sample listed first on top (levels reversed), the figure builds", real_fig(mh) && inherits(mh$coordinates, "CoordFlip") && identical(levels(mh$data$sample), rev(smp)))
  session$setInputs(metric_horizontal = FALSE, metric_style = "bar", metric_custom_colors = "Phage MOI 0.1 = #112233; Phage MOI 10 = #AA0000; Uninfected = #445566")
  mc <- quiet(generate_analysis_plot("metric_summary")); gcol <- which(layer_classes(mc) == "GeomCol")
  fills <- unique(toupper(layer_data(mc, gcol)$fill))
  check("custom bar colours by name (bar style): the drawn samples take them (a sample without a value draws no bar)", real_fig(mc) && setequal(fills, c("#112233", "#AA0000")))
  session$setInputs(metric_custom_colors = "Phage MOI 10 = #AA0000")
  fills2 <- unique(toupper(layer_data(quiet(generate_analysis_plot("metric_summary")), gcol)$fill))
  check("a colour typed for one sample leaves the others on their own colour", "#AA0000" %in% fills2 && length(fills2) == 2 && !"#112233" %in% fills2)
  session$setInputs(metric_custom_colors = "", metric_style = "dot")
  session$setInputs(analysis_metrics = "lysis_time")
  d_small <- panel_dims_mm("metric_summary"); psz0 <- layer_data(quiet(generate_analysis_plot("metric_summary")), 3)$size[1]
  session$setInputs(metric_single_large = TRUE)
  d_large <- panel_dims_mm("metric_summary"); psz1 <- layer_data(quiet(generate_analysis_plot("metric_summary")), 3)$size[1]
  check("single-metric large mode: the panel is 1.5 x larger and the points 1.4 x", abs(d_large[1] - 1.5 * d_small[1]) < 1e-9 && abs(psz1 / psz0 - 1.4) < 1e-9)
  session$setInputs(analysis_metrics = c("lysis_time", "od_drop"))
  check("large mode does nothing with two metrics", identical(panel_dims_mm("metric_summary"), d_small))
  session$setInputs(metric_single_large = FALSE)

  cat("   (OD heatmap palette)\n")
  hv <- quiet(generate_analysis_plot("heatmap")); fv <- layer_data(hv, 1)$fill
  session$setInputs(heatmap_palette = "inferno"); hi <- quiet(generate_analysis_plot("heatmap")); fi <- layer_data(hi, 1)$fill
  session$setInputs(heatmap_palette = "magma"); fm <- layer_data(quiet(generate_analysis_plot("heatmap")), 1)$fill
  session$setInputs(heatmap_palette = "plasma"); fp <- layer_data(quiet(generate_analysis_plot("heatmap")), 1)$fill
  check("OD heatmap palette: viridis (default) differs from inferno, magma and plasma; all build", real_fig(hv) && real_fig(hi) && !identical(fv, fi) && !identical(fi, fm) && !identical(fm, fp) && !identical(fv, fp))
  check("the default heatmap is the plain viridis scale as before", identical(fv, layer_data(ggplot(prepare_plot_data() %>% mutate(variable = factor(variable, levels = rev(smp), labels = rev(smp)), fillv = log10(pmax(mean_value, 1e-6))), aes(time, variable, fill = fillv)) + geom_tile() + scale_fill_viridis_c(), 1)$fill))
  session$setInputs(heatmap_palette = "viridis")
})

cat("\n--- the default figure is unchanged (SVG vs the v2.21 app at 3c1f50e) ---\n")
old_path <- Sys.getenv("KC_BASE_APP", "")
if (old_path == "" && nzchar(Sys.which("git"))) {
  old_path <- file.path(out_dir, "base_app.R")
  rc <- suppressWarnings(system2("git", c("show", "3c1f50e:Lysis_Curve_App_v28.R"), stdout = old_path, stderr = FALSE))
  if (!identical(rc, 0L) || !file.exists(old_path) || file.size(old_path) == 0) old_path <- ""
}
if (nzchar(old_path) && file.exists(old_path)) {
  scen <- list(startup = list(), legacy = env$legacy_preset_settings(), shadow = list(error_display_mode = "shadow", error_type = "sd"),
               reppts = list(show_replicate_points = TRUE), endlab = list(show_end_labels = TRUE), pubr = list(use_advanced_ticks = FALSE),
               marks = list(enable_time_markers = TRUE, marker_count = 1, marker_time_1 = 100, marker_line_type_1 = "dashed", marker_color_1 = "#FF0000", marker_size_1 = 0.5,
                            marker_label_1 = TRUE, marker_text_1 = "Phage", marker_label_size_1 = 3, marker_label_position_1 = "top", marker_label_hjust_1 = 0,
                            enable_threshold = TRUE, threshold_value = 0.2, threshold_label = "thr"),
               linear = list(y_scale_type = "linear"), title = list(plot_title = "Title", plot_subtitle = "Sub", y_axis_label = "OD600 relative"))
  # Each app is rendered in its OWN R process: in one process the two apps share ggplot2's font-metric cache and the open
  # devices, so a size that depends on the measuring device (hardening round 2, R2-REG01) cannot show up as a difference.
  scen_rds <- of("scen.rds"); saveRDS(scen, scen_rds)
  render_script <- of("render_proc.R")
  writeLines(c(
    'args <- commandArgs(TRUE); app_path <- args[1]; scen <- readRDS(args[2]); tag <- args[3]; outdir <- args[4]',
    'suppressPackageStartupMessages({ library(shiny); library(tidyverse); library(ggpubr); library(scales); library(ggrepel); library(ggprism); library(svglite); library(jsonlite) })',
    'quiet <- function(expr) suppressWarnings(suppressMessages(expr))',
    'e <- new.env(); a <- quiet(source(app_path, local = e)$value); res <- list()',
    'for (nm in names(scen)) testServer(a, {',
    '  ingest_data(e$make_demo_data(), "Demo data (built-in)")',
    '  do.call(session$setInputs, modifyList(e$APP_DEFAULTS, scen[[nm]]))',
    '  session$setInputs(selected_samples = rv$od_vars, exclude_timepoints = "")',
    '  set.seed(1); g <- main_plot_obj(); sz <- export_size_in(); f <- file.path(outdir, paste0(tag, "_", nm, ".svg"))',
    '  quiet(ggsave(f, g, device = e$export_device("svg"), width = sz[1], height = sz[2], units = "in", bg = "white", limitsize = FALSE))',
    '  f2 <- file.path(outdir, paste0(tag, "_", nm, "_print.svg")); quiet(svglite::svglite(f2, width = 8, height = 6)); set.seed(1); print(generate_plot()); dev.off()',
    '  res[[nm]] <<- list(export = readLines(f), print = readLines(f2))',
    '})',
    'saveRDS(res, file.path(outdir, paste0(tag, ".rds")))'), render_script)
  render <- function(app_path, tag) {
    wd0 <- setwd(out_dir); on.exit(setwd(wd0))      # (the old app opens R's default device: its Rplots.pdf lands here, not in the repository)
    suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c(shQuote(render_script), shQuote(normalizePath(if (grepl("^(/|[A-Za-z]:)", app_path)) app_path else file.path(wd0, app_path))), shQuote(scen_rds), tag, shQuote(out_dir)), stdout = FALSE, stderr = FALSE))
    f <- of(paste0(tag, ".rds")); if (file.exists(f)) readRDS(f) else list()
  }
  ra <- render(old_path, "old"); rb_ <- render("Lysis_Curve_App_v28.R", "new")
  for (nm in names(scen)) check(paste0("scenario '", nm, "': the SVG export and the printed figure are byte-identical to the v2.21 app"),
                                length(ra[[nm]]$export) > 50 && identical(ra[[nm]]$export, rb_[[nm]]$export) && identical(ra[[nm]]$print, rb_[[nm]]$print))
} else skipit("the v2.21 app (git show 3c1f50e:Lysis_Curve_App_v28.R, or KC_BASE_APP=path) is not available: byte-identical default figure check")


cat("\n--- server: panels from a metadata column of a long file ---\n")
set.seed(1)
tt <- rep(seq(0, 120, 20), times = 4 * 3)
long <- data.frame(Time = tt, Condition = rep(c("WT A", "WT B", "Mut A", "Mut B"), each = 7 * 3), Strain = rep(c("WT", "WT", "Mut", "Mut"), each = 7 * 3),
                   Replicate = rep(rep(1:3, each = 7), 4), OD = round(0.05 + 0.01 * tt * runif(length(tt), 0.8, 1.2) / 10, 4), stringsAsFactors = FALSE)
testServer(app, {
  ingest_data(long, "long test")
  do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "")
  ch <- facet_source_choices()
  check("a long file offers its other columns as panel sources", all(c("group", "manual", "Strain") %in% unname(ch)) && !"Time" %in% unname(ch) && !"Replicate" %in% unname(ch) && !"OD" %in% unname(ch) && !"Condition" %in% unname(ch))
  session$setInputs(facet_by_group = TRUE, facet_source = "Strain")
  p <- generate_plot(); lay <- ggplot_build(p)$layout$layout
  check("panels by a metadata column: one panel per value, in first-appearance order", real_fig(p) && identical(as.character(lay$group), c("WT", "Mut")))
  check("the samples of each panel come from the column (WT A and WT B share the WT panel)", identical(unname(panel_groups(rv$od_vars, resolve_aesthetics(rv$od_vars))), c("WT", "WT", "Mut", "Mut")))
  d1 <- layer_data(generate_plot(), which(layer_classes(generate_plot()) == "GeomLine")[1])
  check("each panel holds its own samples' lines (2 + 2 lines)", identical(as.integer(tapply(d1$group, d1$PANEL, function(g) length(unique(g)))), c(2L, 2L)))
  session$setInputs(error_display_mode = "spaghetti", y_scale_type = "linear")
  ps <- generate_plot()
  check("spaghetti in panels: the replicate traces carry the panel column (each panel draws its own samples' replicates)",
        real_fig(ps) && { dl <- layer_data(ps, 1); identical(sort(unique(dl$PANEL)), factor(1:2)) || length(unique(dl$PANEL)) == 2 })
  session$setInputs(error_display_mode = "quantile_bands")
  check("quantile bands in panels build", real_fig(generate_plot()))
  session$setInputs(error_display_mode = "bars", facet_source = "nonexistent")
  check("a source column that is not in the file falls back to the typed Groups (no crash, no facets without groups)", real_fig(generate_plot()) && inherits(generate_plot()$facet, "FacetNull"))
})


cat("\n--- server: direct-label room, markup in the analysis figures ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1], analysis_metrics = c("lysis_time", "od_drop"))
  m0 <- ggplot2::calc_element("plot.margin", generate_plot()$theme)
  session$setInputs(legend_position = "direct")
  m1 <- ggplot2::calc_element("plot.margin", generate_plot()$theme)
  check("direct labels: the right plot margin grows to hold the longest label, the others are unchanged",
        as.numeric(m1[2]) > as.numeric(m0[2]) + 20 && as.numeric(m1[1]) == as.numeric(m0[1]) && as.numeric(m1[4]) == as.numeric(m0[4]))
  session$setInputs(legend_position = "right", show_end_labels = TRUE)
  check("the end-labels checkbox keeps the v2.21 margin (only the direct position adds room)", identical(as.numeric(ggplot2::calc_element("plot.margin", generate_plot()$theme)), as.numeric(m0)))
  session$setInputs(show_end_labels = FALSE, y_axis_label = "OD_{600}")
  pms <- quiet(generate_analysis_plot("metric_summary"))
  check("metric-summary facet titles with markup (a y label with a subscript) are parsed strips", real_fig(pms) && is.function(pms$facet$params$labeller) &&
        any(grepl("600", levels(pms$data$metric))))
  rv$display_names <- setNames(c("Control", "*Phage* A", "Phage_{2}"), smp)
  pms2 <- quiet(generate_analysis_plot("metric_summary")); f <- of("metric_markup.svg")
  quiet(ggsave(f, pms2, device = svglite::svglite, width = 5, height = 3))
  check("display names with markup: the metric summary's x axis labels are typeset (italic text in the SVG)", real_fig(pms2) && any(grepl("font-style: italic", readLines(f), fixed = TRUE)))
  prf <- quiet(generate_analysis_plot("replicate_facets"))
  check("replicate small multiples take markup in the panel titles", real_fig(prf) && is.function(prf$facet$params$labeller))
  rv$display_names <- NULL; session$setInputs(y_axis_label = "A550")
  pm_plain <- quiet(generate_analysis_plot("metric_summary"))
  check("without markup the metric facets keep the wrapped plain strips", real_fig(pm_plain))
})

cat("\n============ SUMMARY:", pass, "passed /", fail, "failed /", skip, "skipped ============\n")
