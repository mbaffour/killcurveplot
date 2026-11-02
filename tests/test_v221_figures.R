# ============================================================================
# v2.21 figures: the metric summary's styles (dot / bar / box / violin) and significance brackets, the OD-at-times and
# time-to-threshold extraction figures, replicate small multiples and the fraction-lysed curves.
# Run from the repository root:  Rscript tests/test_v221_figures.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root")

pass <- 0L; fail <- 0L
check <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
quiet <- function(expr) suppressWarnings(suppressMessages(expr))

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly", inherits(app, "shiny.appobj"))

cat("\n--- defaults, choices, UI ---\n")
d <- env$APP_DEFAULTS
check("APP_DEFAULTS: every v2.21 input, the default figure unchanged (dot, no brackets)",
      identical(d$metric_style, "dot") && identical(d$metric_brackets, FALSE) && identical(d$extract_style, "bar") &&
      isTRUE(d$extract_points) && identical(d$lysed_mode, "relative") && d$lysed_threshold == 0.5)
check("the v2.21 entries are contiguous at the end of APP_DEFAULTS' base list, later versions append after them (merge-safe)",
      identical(names(env$LEGACY_BASE)[match("metric_style", names(env$LEGACY_BASE)) + 0:5],
                c("metric_style", "metric_brackets", "extract_style", "extract_points", "lysed_mode", "lysed_threshold")))
check("neither preset touches them",
      !any(grepl("^(metric_style|metric_brackets|extract_style|extract_points|lysed_)", names(env$legacy_preset_settings()))) &&
      !any(grepl("^(metric_style|metric_brackets|extract_style|extract_points|lysed_)", names(env$journal_preset_settings(89, 65)))))
new_types <- c("od_at_times", "time_to_threshold", "replicate_facets", "lysed_fraction")
check("the four figures are analysis figures and composer panels",
      all(new_types %in% env$ANALYSIS_FIGURE_CHOICES) && all(new_types %in% env$COMPOSE_PANEL_CHOICES))
check("metric_style choices: dot first (default), bar, box, violin with the documented labels",
      identical(unname(env$METRIC_STYLE_CHOICES), c("dot", "bar", "box", "violin")) &&
      identical(names(env$METRIC_STYLE_CHOICES)[1], "Dot plot (replicates, mean bar)") &&
      identical(names(env$METRIC_STYLE_CHOICES)[2], "Bar (mean ± error, replicates as points)"))
ui_html <- as.character(env$ui)
check("UI: every new input and the note output",
      all(vapply(c("metric_style", "metric_brackets", "extract_style", "extract_points", "lysed_mode", "lysed_threshold", "analysis_figure_note"),
                 function(id) grepl(paste0('id="', id, '"'), ui_html, fixed = TRUE), logical(1))))

cat("\n--- helpers: brackets ---\n")
pw <- data.frame(sample = c("A", "A", "B", "A", "B", "C", "D"), versus = c("B", "C", "C", "D", "D", "D", "A"),
                 p_adj_holm = c(0.001, 0.2, 0.04, 0.0001, 0.5, 0.03, NA), stars = c("**", "ns", "*", "***", "ns", "*", ""),
                 stringsAsFactors = FALSE)
xp <- c(A = 1, B = 2, C = 3, D = 4)
L <- env$metric_bracket_layout(pw, xp, y_top = 10, y_range = 5)
check("brackets: only pairs with p_adj_holm < 0.05 (NA and ns left out)", L$n_sig == 4L && L$n_shown == 4L && nrow(L$labels) == 4L)
check("brackets: three segments each (bar and two ticks)", nrow(L$segments) == 12L)
check("brackets: stacked above the data, one level per bracket, narrowest lowest",
      all(L$labels$y > 10) && !anyDuplicated(L$labels$y) && !is.unsorted(L$labels$y) &&
      isTRUE(all.equal(L$labels$x, c(1.5, 2.5, 3.5, 2.5))) && L$y_max > max(L$labels$y))
check("brackets: the stars label is the pair's stars", setequal(L$labels$label, c("**", "*", "*", "***")))
pw8 <- data.frame(sample = rep("A", 9), versus = paste0("v", 1:9), p_adj_holm = seq(0.001, 0.009, length.out = 9), stars = "**", stringsAsFactors = FALSE)
L8 <- env$metric_bracket_layout(pw8, setNames(1:10, c("A", paste0("v", 1:9))), 1, 1)
check("brackets: at most 6, the most significant", L8$n_sig == 9L && L8$n_shown == 6L && all(L8$labels$p_adj_holm <= 0.006 + 1e-12))
check("brackets: nothing significant -> no layout", is.null(env$metric_bracket_layout(pw[pw$p_adj_holm > 0.1 & is.finite(pw$p_adj_holm), ], xp, 1, 1)$segments))

cat("\n--- helpers: replicate colours ---\n")
tc <- env$replicate_tint_colors("#E69F00")
lum <- function(h) { m <- grDevices::col2rgb(h); 0.2126 * m[1, ] + 0.7152 * m[2, ] + 0.0722 * m[3, ] }
check("three tints of the sample colour, each lighter than the last and lighter than the mean",
      length(tc) == 3 && !is.unsorted(lum(tc)) && !anyDuplicated(tc) && all(lum(tc) > lum("#E69F00")))
check("tints stay distinguishable in greyscale (>= 8 grey levels apart) even for black", {
  tb <- env$replicate_tint_colors("#000000"); all(diff(lum(tb)) >= 8) })
rc <- env$replicate_curve_colors("#56B4E9", c(2, 5, 7, 9))
check("replicate_curve_colors: ranks the ids and cycles the three tints", length(rc) == 4 && identical(unname(rc[4]), unname(rc[1])) && identical(names(rc), c("2", "5", "7", "9")))

cat("\n--- helpers: fraction lysed ---\n")
mk <- function(vals, times = c(0, 10, 20, 30, 40)) {   # vals: one list element per replicate
  do.call(rbind, lapply(seq_along(vals), function(r) data.frame(time = times, variable = "S", replicate = r, value = vals[[r]], stringsAsFactors = FALSE)))
}
syn <- mk(list(c(0.1, 0.5, 1.0, 0.4, 0.1), c(0.1, 0.5, 1.0, 0.9, 0.2), c(0.1, 0.5, 1.0, 0.95, 0.9)))
lc <- env$lysed_fraction_curves(syn, 0.5, "relative")
# means: 0.1, 0.5, 1.0, 0.75, 0.4: the peak is 1.0 at t = 20, the threshold 0.5
check("synthetic: peak, threshold and the fraction per time (growth-phase readings below the threshold are not lysis)",
      all(lc$peak_od == 1) && all(lc$t_peak == 20) && all(lc$threshold_od == 0.5) &&
      isTRUE(all.equal(lc$fraction, c(0, 0, 0, 1/3, 2/3))))
la <- env$lysed_fraction_curves(syn, 0.45, "absolute")
check("absolute mode: the threshold is an OD", all(la$threshold_od == 0.45) && isTRUE(all.equal(la$fraction, c(0, 0, 0, 1/3, 2/3))))
check("an absolute threshold above the peak is never reached, so nothing is lysed",
      all(env$lysed_fraction_curves(syn, 5, "absolute")$fraction == 0))
check("fraction is over the replicates read at that time",
      { s2 <- syn[!(syn$replicate == 3 & syn$time == 40), ]; l2 <- env$lysed_fraction_curves(s2, 0.5, "relative")
        l2$n[l2$time == 40] == 2L && isTRUE(all.equal(l2$fraction[l2$time == 40], 1)) })
t50 <- env$lysed_fraction_t50(lc)
check("t50: the first time the fraction reaches 0.5 (40 here)", identical(t50$time, 40) && isTRUE(all.equal(t50$fraction, 2/3)))
check("t50: NA when never reached", is.na(env$lysed_fraction_t50(env$lysed_fraction_curves(syn, 5, "absolute"))$time))
check("bad thresholds are refused", inherits(tryCatch(env$lysed_fraction_curves(syn, -1), error = function(e) e), "error") &&
      inherits(tryCatch(env$lysed_fraction_curves(syn, NA_real_), error = function(e) e), "error"))
check("empty data -> an empty frame", nrow(env$lysed_fraction_curves(syn[0, ], 0.5)) == 0)

cat("\n--- server: metric summary styles and brackets ---\n")
layer_classes <- function(p) unname(vapply(p$layers, function(l) class(l$geom)[1], ""))
# (is_real_fig is defined inside testServer: is_placeholder_plot lives in the server function)
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  is_real_fig <- function(p) inherits(p, "ggplot") && !is_placeholder_plot(p) && !is.null(quiet(ggplot_build(p)))
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "", metrics_ref_sample = smp[1],
                    analysis_metrics = c("lysis_time", "od_drop", "mu_max", "infection_strength"))

  p0 <- quiet(generate_analysis_plot("metric_summary"))
  check("default: the dot plot (error bar, crossbar, points), no segment layer", is_real_fig(p0) &&
        identical(layer_classes(p0), c("GeomErrorbar", "GeomCrossbar", "GeomPoint", "GeomText")) && is.null(attr(p0, "figure_note")))
  for (st in c("dot", "bar", "box", "violin")) {
    session$setInputs(metric_style = st)
    p <- quiet(generate_analysis_plot("metric_summary"))
    check(paste0("style ", st, ": renders a real figure with 4 facets"), is_real_fig(p) && nrow(ggplot_build(p)$layout$layout) == 4)
  }
  session$setInputs(metric_style = "bar")
  pb <- quiet(generate_analysis_plot("metric_summary")); lb <- layer_classes(pb)
  check("bar: columns, whiskers and replicate points", all(c("GeomCol", "GeomErrorbar", "GeomPoint") %in% lb))
  bb <- quiet(ggplot_build(pb)); col_i <- which(lb == "GeomCol")
  check("bar: width 0.6, fill alpha 0.85 in the sample colours",
        all(abs(bb$data[[col_i]]$xmax - bb$data[[col_i]]$xmin - 0.6) < 1e-9) && all(abs(bb$data[[col_i]]$alpha - 0.85) < 1e-9) &&
        all(toupper(bb$data[[col_i]]$fill) %in% toupper(c("#000000", "#E69F00", "#56B4E9"))))
  yr <- lapply(bb$layout$panel_params, function(pp) pp$y.range)
  check("bar: the y axis of every facet without negative values starts at 0 (the columns sit on the axis); a facet with negatives has room below",
        all(vapply(yr[1:3], function(r) abs(r[1]) < 1e-9, logical(1))) && yr[[4]][1] < 0)
  pt_i <- which(lb == "GeomPoint")
  check("bar: the replicate points are open circles (shape 21, white fill)", all(bb$data[[pt_i]]$shape == 21) && all(bb$data[[pt_i]]$fill == "white"))
  er <- bb$data[[which(lb == "GeomErrorbar")]]
  check("bar: the whiskers are the main error measure (SEM: ymax - ymin = 2 x SEM of the replicate values)", {
    rm <- replicate_metric_rows_current(); v <- rm$lysis_time[rm$sample == "Phage MOI 10"]
    any(abs((er$ymax - er$ymin) - 2 * sd(v) / sqrt(length(v))) < 1e-6) })
  session$setInputs(metric_style = "box")
  pbx <- quiet(generate_analysis_plot("metric_summary"))
  check("box: geom_boxplot with outlier shape 1 and the replicate points",
        all(c("GeomBoxplot", "GeomPoint") %in% layer_classes(pbx)) &&
        identical(unname(pbx$layers[[which(layer_classes(pbx) == "GeomBoxplot")]]$geom_params$outlier_gp$shape), 1))
  session$setInputs(metric_style = "violin")
  pv <- quiet(generate_analysis_plot("metric_summary")); lv <- layer_classes(pv)
  vl <- pv$layers[[which(lv == "GeomViolin")]]
  check("violin: untrimmed, equal widths, with the replicate points and a mean bar",
        identical(vl$stat_params$trim, FALSE) && identical(vl$stat_params$scale, "width") && all(c("GeomPoint", "GeomCrossbar") %in% lv))
  session$setInputs(metric_style = "dot")

  # brackets
  session$setInputs(metric_brackets = TRUE)
  pk <- quiet(generate_analysis_plot("metric_summary")); lk <- layer_classes(pk)
  bk <- quiet(ggplot_build(pk))
  seg <- bk$data[[which(lk == "GeomSegment")[1]]]
  check("brackets on: a GeomSegment layer with data", is_real_fig(pk) && "GeomSegment" %in% lk && nrow(seg) > 0)
  infl <- levels(pk$layers[[which(lk == "GeomSegment")[1]]]$data$metric)
  sg_d <- pk$layers[[which(lk == "GeomSegment")[1]]]$data
  check("brackets: the infection_strength facet has at least one bracket",
        any(grepl("Infection strength", as.character(sg_d$metric))) && sum(grepl("Infection strength", as.character(sg_d$metric))) >= 3)
  check("brackets: all pairs (3 for infection_strength with 3 samples, no reference used)",
        sum(grepl("Infection strength", as.character(sg_d$metric))) == 9)
  txt_i <- which(lk == "GeomText")
  check("brackets: the stars are drawn with geom_text at the axis-text size, and the vs-reference stars are off",
        length(txt_i) == 1 && all(pk$layers[[txt_i]]$data$label %in% c("*", "**", "***")) &&
        abs(pk$layers[[txt_i]]$aes_params$size - input$axis_text_font_size / ggplot2::.pt) < 1e-9)
  check("brackets: plain segments and text, no ggsignif layer", !any(grepl("Signif", layer_classes(pk))))
  seg_top <- tapply(sg_d$y, as.character(sg_d$metric), max)
  ys <- ggplot_build(pk)$layout$panel_params
  check("brackets: every bracket lies inside its facet's y range", all(vapply(seq_along(ys), function(i) {
    m <- as.character(ggplot_build(pk)$layout$layout$metric[i]); is.na(seg_top[m]) || seg_top[m] <= ys[[i]]$y.range[2] }, logical(1))))
  for (st in c("bar", "box", "violin")) {
    session$setInputs(metric_style = st)
    pp <- quiet(generate_analysis_plot("metric_summary"))
    check(paste0("brackets on ", st, ": renders with brackets"), is_real_fig(pp) && "GeomSegment" %in% layer_classes(pp))
  }
  session$setInputs(metric_style = "dot")
  # a single metric nothing separates -> a note, no segments
  session$setInputs(analysis_metrics = "mu_max")
  pn <- quiet(generate_analysis_plot("metric_summary"))
  check("no pair significant: no bracket layer, and the note says so", !"GeomSegment" %in% layer_classes(pn) && any(grepl("No pair", attr(pn, "figure_note"))))
  session$setInputs(analysis_metrics = c("lysis_time", "od_drop", "mu_max", "infection_strength"), metric_brackets = FALSE)
  p1 <- quiet(generate_analysis_plot("metric_summary"))
  check("brackets off again: the default figure is restored", identical(layer_classes(p1), layer_classes(p0)))

  # many samples: at most 6 per facet
  ingest_data(env$make_demo_data("moi_series"), "Demo data (built-in)")
  smp6 <- rv$od_vars
  session$setInputs(selected_samples = smp6, metrics_ref_sample = smp6[1], analysis_metrics = c("lysis_time"), metric_brackets = TRUE, metric_style = "dot")
  p6 <- quiet(generate_analysis_plot("metric_summary"))
  sg6 <- p6$layers[[which(layer_classes(p6) == "GeomSegment")[1]]]$data
  check("6 samples: at most 6 brackets in the facet (3 segments each), and the note says some were left out",
        nrow(sg6) == 18 && any(grepl("most significant are drawn", attr(p6, "figure_note"))))
  session$setInputs(metric_brackets = FALSE)
  # two replicates per sample: brackets still work (n >= 2) and the figure's note carries the n < 3 warning
  dd2 <- env$make_demo_data(); keep <- c(1, 2, 3, 5, 6, 8, 9); d2 <- dd2[, keep]; colnames(d2) <- colnames(dd2)[keep]
  ingest_data(d2, "two replicates")
  smp2 <- rv$od_vars
  session$setInputs(selected_samples = smp2, metrics_ref_sample = smp2[1], analysis_metrics = c("lysis_time", "infection_strength"),
                    metric_brackets = TRUE, metric_style = "dot", analysis_type = "metric_summary")
  p2 <- quiet(generate_analysis_plot("metric_summary"))
  check("n = 2: brackets are drawn and the figure's note carries the n < 3 warning",
        "GeomSegment" %in% layer_classes(p2) && any(grepl("n < 3", attr(p2, "figure_note"), fixed = TRUE)))
  check("the note is rendered under the controls (the n < 3 line)", grepl("n &lt; 3", as.character(output$analysis_figure_note$html), fixed = TRUE))
  session$setInputs(metric_brackets = FALSE)
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  session$setInputs(selected_samples = smp, metrics_ref_sample = smp[1], analysis_metrics = c("lysis_time", "od_drop", "mu_max", "infection_strength"))

  cat("\n--- server: OD at chosen times ---\n")
  po <- quiet(generate_analysis_plot("od_at_times")); lo <- layer_classes(po)
  check("od_at_times (log y, no times typed): renders a real figure", is_real_fig(po))
  check("it says it used the first, middle and last time points", any(grepl("first, middle and last", attr(po, "figure_note"))) &&
        grepl("0, 90, 180", paste(attr(po, "figure_note"), collapse = " ")))
  bo <- quiet(ggplot_build(po))
  check("log y: a log10 scale, bars drawn as rectangles from a floor below every value (not from OD 1)",
        grepl("log", bo$layout$panel_scales_y[[1]]$trans$name) && "GeomRect" %in% lo &&
        all(10^bo$data[[which(lo == "GeomRect")]]$ymin < min(10^bo$data[[which(lo == "GeomRect")]]$ymax)) &&
        all(10^bo$data[[which(lo == "GeomRect")]]$ymin > 0))
  rects <- bo$data[[which(lo == "GeomRect")]]
  cen <- (rects$xmin + rects$xmax) / 2
  check("3 times x 3 samples = 9 bars, side by side: each time's three bars are centred on it (0.8 wide in all)",
        nrow(rects) == 9 && isTRUE(all.equal(as.vector(tapply(cen, round(cen), mean)), c(1, 2, 3))) &&
        all(table(round(cen)) == 3) && all(abs((rects$xmax - rects$xmin) - 0.8 / 3 * 0.84) < 1e-9))
  check("x labels are the requested times", identical(bo$layout$panel_params[[1]]$x$get_labels(), c("0", "90", "180")))
  session$setInputs(extract_times = "30, 60, 120")
  po2 <- quiet(generate_analysis_plot("od_at_times"))
  check("typed times are used (labels 30, 60, 120) and no 'first, middle' note", is_real_fig(po2) &&
        identical(quiet(ggplot_build(po2))$layout$panel_params[[1]]$x$get_labels(), c("30", "60", "120")) && is.null(attr(po2, "figure_note")))
  # the bar heights are the mean OD at the time (interpolated)
  rp <- prepare_replicate_data(); ox <- env$od_at_times(rp, c(30, 60, 120)); mm <- tapply(ox$value, list(ox$time_requested, ox$sample), mean)
  rb <- quiet(ggplot_build(po2)); rr <- rb$data[[which(layer_classes(po2) == "GeomRect")]]
  check("the bars are the mean OD at the requested time (od_at_times; log10 scale)", all(vapply(10^rr$ymax, function(v) any(abs(v - mm) < 1e-9), logical(1))))
  session$setInputs(extract_style = "dot")
  pd <- quiet(generate_analysis_plot("od_at_times"))
  check("dots: no rectangles, the means as points with whiskers", is_real_fig(pd) && !"GeomRect" %in% layer_classes(pd) && "GeomErrorbar" %in% layer_classes(pd))
  session$setInputs(extract_points = FALSE)
  pnp <- quiet(generate_analysis_plot("od_at_times"))
  check("replicate points off: only the 9 mean points remain", sum(vapply(layer_classes(pnp) == "GeomPoint", isTRUE, TRUE)) == 1 &&
        nrow(quiet(ggplot_build(pnp))$data[[which(layer_classes(pnp) == "GeomPoint")]]) == 9)
  session$setInputs(extract_points = TRUE, extract_style = "bar", y_scale_type = "linear")
  pl <- quiet(generate_analysis_plot("od_at_times"))
  check("linear y: linear scale, bars from 0", is_real_fig(pl) && !grepl("log", quiet(ggplot_build(pl))$layout$panel_scales_y[[1]]$trans$name) &&
        all(quiet(ggplot_build(pl))$data[[which(layer_classes(pl) == "GeomRect")]]$ymin == 0))
  check("the y label is the y axis label (A550 as a subscript expression)", !is.null(pl$labels$y))
  session$setInputs(extract_times = "5, 500")
  check("times outside every curve: a clean 'not available' (no crash)", !is.null(quiet(generate_analysis_plot("od_at_times"))) )
  session$setInputs(extract_times = "", y_scale_type = "log")

  cat("\n--- server: time to threshold ---\n")
  session$setInputs(extract_threshold = 0.15, extract_direction = "down", extract_after = "peak")
  pt <- quiet(generate_analysis_plot("time_to_threshold")); lt <- layer_classes(pt)
  check("time_to_threshold (0.15, down, after the peak): renders a real figure", is_real_fig(pt))
  bt <- quiet(ggplot_build(pt))
  tt <- env$time_to_od(prepare_replicate_data(), 0.15, "down", "peak")
  check("dot-plot look: error bars, mean bars (crossbar) and replicate points",
        all(c("GeomErrorbar", "GeomCrossbar", "GeomPoint") %in% lt))
  pts <- bt$data[[which(lt == "GeomPoint")]]
  check("one replicate point per replicate that crosses (6 of 9: the uninfected control never goes down to 0.15)",
        nrow(pts) == sum(is.finite(tt$time)) && sum(is.finite(tt$time)) == 6)
  check("the y values are the crossing times", isTRUE(all.equal(sort(pts$y), sort(tt$time[is.finite(tt$time)]))))
  check("y label: 'Time to 0.15 (min)'", identical(pt$labels$y, "Time to 0.15 (min)"))
  check("samples that never cross are listed in the note", any(grepl("Never crosses.*Uninfected", attr(pt, "figure_note"))))
  check("the whiskers are the main error measure (SEM)", {
    er <- bt$data[[which(lt == "GeomErrorbar")]]; v <- tt$time[tt$sample == "Phage MOI 10"]
    any(abs((er$ymax - er$ymin) - 2 * sd(v) / sqrt(length(v))) < 1e-6) })
  check("all three samples keep their slot on the x axis (empty for the one that never crosses)", length(bt$layout$panel_params[[1]]$x$get_labels()) == 3)
  session$setInputs(extract_threshold = 99)
  check("a threshold nothing crosses: 'Figure not available' with the reason", is_placeholder_plot(generate_analysis_plot("time_to_threshold")))
  session$setInputs(extract_threshold = NA)
  pe <- quiet(generate_analysis_plot("time_to_threshold"))
  check("an emptied threshold box: the default OD 0.5 is used and the note says so (Download all figures still renders)",
        is_real_fig(pe) && any(grepl("default, OD 0.5", attr(pe, "figure_note"), fixed = TRUE)))
  session$setInputs(extract_threshold = 0.15)

  cat("\n--- server: replicate small multiples ---\n")
  pr <- quiet(generate_analysis_plot("replicate_facets")); lr <- layer_classes(pr)
  br <- quiet(ggplot_build(pr))
  check("replicate_facets: renders a real figure with 3 facets (one per sample)", is_real_fig(pr) && nrow(br$layout$layout) == 3)
  check("the facet titles are the display labels (sample order)", identical(as.character(br$layout$layout$panel), c("Uninfected", "Phage MOI 10", "Phage MOI 0.1")))
  rep_d <- br$data[[1]]; mean_d <- br$data[[2]]
  check("replicates thin, the mean bold (1.3 x the line width vs 0.5 x)", all(lr == "GeomLine") && mean(mean_d$linewidth) > 2 * mean(rep_d$linewidth))
  check("the mean is drawn in the sample colour", setequal(toupper(unique(mean_d$colour)), toupper(c("#000000", "#E69F00", "#56B4E9"))))
  ocol <- toupper(unique(rep_d$colour[rep_d$PANEL == 2]))
  check("each sample's replicates: three distinct tints, none equal to the mean colour, lighter than it",
        length(ocol) == 3 && !"#E69F00" %in% ocol && all(lum(ocol) > lum("#E69F00")))
  check("the y axis is shared and log (y_scale_type log)", length(unique(vapply(br$layout$panel_params, function(pp) paste(round(pp$y.range, 6), collapse = ","), ""))) == 1 &&
        grepl("log", br$layout$panel_scales_y[[1]]$trans$name))
  session$setInputs(y_scale_type = "linear")
  prl <- quiet(generate_analysis_plot("replicate_facets"))
  check("linear y: a linear shared scale", is_real_fig(prl) && !grepl("log", quiet(ggplot_build(prl))$layout$panel_scales_y[[1]]$trans$name))
  check("no legend (the strips name the samples)", identical(pr$theme$legend.position, "none"))
  session$setInputs(y_scale_type = "log")

  cat("\n--- server: fraction lysed ---\n")
  pf <- quiet(generate_analysis_plot("lysed_fraction")); lf_ <- layer_classes(pf)
  check("lysed_fraction: renders a real figure with step curves", is_real_fig(pf) && "GeomStep" %in% lf_)
  lfc <- env$lysed_fraction_curves(prepare_replicate_data(), 0.5, "relative")
  check("demo: Uninfected stays at 0 at every time", all(lfc$fraction[lfc$sample == "Uninfected"] == 0))
  check("demo: Phage MOI 10 reaches 1 by 105 min", any(lfc$fraction[lfc$sample == "Phage MOI 10" & lfc$time <= 105] == 1))
  t50d <- env$lysed_fraction_t50(lfc)
  check("demo: Phage MOI 0.1 is half lysed later than Phage MOI 10 (75 vs 120 min)", t50d$time[t50d$sample == "Phage MOI 0.1"] > t50d$time[t50d$sample == "Phage MOI 10"])
  bf <- quiet(ggplot_build(pf))
  check("y runs from 0 to 1 and the label is 'Fraction of replicates lysed'",
        isTRUE(all.equal(bf$layout$panel_params[[1]]$y$continuous_range, c(-0.03, 1.03), tolerance = 1e-6)) && identical(pf$labels$y, "Fraction of replicates lysed"))
  t50p <- bf$data[[which(lf_ == "GeomPoint")]]
  check("the time the fraction first reaches 0.5 is marked by a point (two samples reach it)",
        nrow(t50p) == 2 && all(abs(t50p$y - 0.5) < 1e-9) && setequal(t50p$x, env$lysed_fraction_t50(lfc)$time[is.finite(env$lysed_fraction_t50(lfc)$time)]))
  check("the note names the per-sample threshold and the sample that never reaches half", any(grepl("Uninfected", attr(pf, "figure_note"))) && any(grepl("Never reaches half", attr(pf, "figure_note"))))
  session$setInputs(lysed_mode = "absolute", lysed_threshold = 0.1)
  pfa <- quiet(generate_analysis_plot("lysed_fraction"))
  check("absolute mode (OD 0.1) renders; Phage MOI 0.1's t50 falls in its lysis (after 105 min)", is_real_fig(pfa) &&
        { a <- env$lysed_fraction_t50(env$lysed_fraction_curves(prepare_replicate_data(), 0.1, "absolute")); a$time[a$sample == "Phage MOI 0.1"] > 105 })
  session$setInputs(lysed_threshold = -1)
  check("a bad threshold: not available (no crash)", is_placeholder_plot(generate_analysis_plot("lysed_fraction")))
  session$setInputs(lysed_mode = "relative", lysed_threshold = 0.5)

  cat("\n--- server: fixed panel, composer, defaults ---\n")
  session$setInputs(panel_size_mode = "fixed", panel_width_mm = 65, panel_height_mm = 65)
  for (ty in c("od_at_times", "time_to_threshold", "replicate_facets", "lysed_fraction", "metric_summary")) {
    session$setInputs(analysis_type = ty)
    g <- analysis_fixed_info(); sz <- attr(g, "size_mm")
    check(paste0(ty, ": fixed-panel figure with a size (", paste(round(sz, 1), collapse = " x "), " mm)"), length(sz) == 2 && all(sz > 30))
  }
  session$setInputs(analysis_type = "replicate_facets")
  gm <- analysis_fixed_info()
  pan <- gm$layout[grepl("^panel", gm$layout$name), ]
  w_panels <- sum(as.numeric(grid::convertWidth(gm$widths[sort(unique(unlist(Map(seq, pan$l, pan$r))))], "mm")))
  check("replicate facets share the panel area: the panels together are 65 mm wide", abs(w_panels - 65) < 0.5)
  session$setInputs(compose_panels = c("main", "od_at_times", "replicate_facets", "lysed_fraction", "time_to_threshold"), compose_ncol = "2")
  ci <- compose_info()
  check("the composer takes the new figures", ci$n == 5 && !length(ci$skipped))
  session$setInputs(analysis_type = "lysed_fraction")
  check("the note output renders HTML for a figure with notes", grepl("lysed", as.character(output$analysis_figure_note$html)))
  session$setInputs(analysis_type = "growth_rate")
  check("figures without notes render no note", is.null(output$analysis_figure_note$html) || !nzchar(as.character(output$analysis_figure_note$html)))
})

cat("\n============ SUMMARY:", pass, "passed /", fail, "failed ============\n")
if (fail > 0) quit(status = 1)
