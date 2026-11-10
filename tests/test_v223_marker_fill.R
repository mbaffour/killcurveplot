# ============================================================================
# v2.23.9 filled vs open markers: ONE rule (normalise_marker / resolve_point_shape) read by the figure, the legend keys, the
# legend key image and the legend words; ONE scale pair (marker_scales: shape + fill) in every figure that draws sample
# markers, so an open marker is hollow in every error-display mode and every analysis figure and Compare.
# Run from the repository root:  Rscript tests/test_v223_marker_fill.R   (nothing here opens the default graphics device)
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v223_marker_fill.R")
Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_such_paper_style.json"))   # never pick up a real paper_style.json

pass <- 0L; fail <- 0L; skip <- 0L
check  <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.23.9 or later", package_version(env$APP_VERSION) >= "2.23.9")
out_dir <- tempfile("v2239_"); dir.create(out_dir)
of <- function(x) file.path(out_dir, x)
read_all <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")
quiet <- function(expr) suppressWarnings(suppressMessages(expr))
plot_error <- function(p) {
  lab <- tryCatch(p$layers[[1]]$aes_params$label, error = function(e) NULL)
  if (is.character(lab) && any(grepl("^(Plot error|Figure not available)", lab))) lab else NULL
}

# ── normalise_marker / resolve_point_shape: the table ───────────────────────
cat("\n--- normalise_marker / resolve_point_shape ---\n")
nm <- env$normalise_marker
same <- function(code, filled, shape, fl) { r <- nm(code, filled); identical(r$shape, as.integer(shape)) && identical(r$filled, fl) }
check("normalise: the five base forms keep their code and the box (16 15 17 18 25)",
      same(16, TRUE, 16, TRUE) && same(16, FALSE, 16, FALSE) && same(15, FALSE, 15, FALSE) && same(17, TRUE, 17, TRUE) && same(18, FALSE, 18, FALSE) && same(25, TRUE, 25, TRUE) && same(25, FALSE, 25, FALSE))
check("normalise: the alias codes 21 / 22 / 24 / 23 are the base form, filled as the box says (default filled)",
      same(21, TRUE, 16, TRUE) && same(21, FALSE, 16, FALSE) && same(22, TRUE, 15, TRUE) && same(22, FALSE, 15, FALSE) &&
      same(24, TRUE, 17, TRUE) && same(24, FALSE, 17, FALSE) && same(23, TRUE, 18, TRUE) && same(23, FALSE, 18, FALSE) && same(21, NA, 16, TRUE))
check("normalise: the always-hollow codes 1 0 2 5 6 and 123 125 are the base form with filled forced FALSE",
      same(1, TRUE, 16, FALSE) && same(0, TRUE, 15, FALSE) && same(2, TRUE, 17, FALSE) && same(5, TRUE, 18, FALSE) && same(6, TRUE, 25, FALSE) &&
      same(123, TRUE, 18, FALSE) && same(125, TRUE, 25, FALSE) && same(123, FALSE, 18, FALSE))
check("normalise: the line-drawn forms 4 8 3 10 keep their code",
      same(4, TRUE, 4, TRUE) && same(8, FALSE, 8, FALSE) && same(3, TRUE, 3, TRUE) && same(10, TRUE, 10, TRUE))
check("normalise: unknown / NA / NULL codes become the circle with filled = isTRUE(box)",
      same(NA, TRUE, 16, TRUE) && same(NA, FALSE, 16, FALSE) && same("x", TRUE, 16, TRUE) && same(999, FALSE, 16, FALSE) && identical(nm(NULL, TRUE), list(shape = 16L, filled = TRUE)) && same("21", TRUE, 16, TRUE))
rp <- env$resolve_point_shape
check("resolve: 21 + Filled draws exactly like 16 + Filled (one rule, no matter the code)",
      identical(rp(21, TRUE, "red"), list(shape = 16L, fill = "red")) && identical(rp(21, TRUE, "red"), rp(16, TRUE, "red")))
check("resolve: open = the stroke pch with a white fill (21 22 24 23 25)",
      identical(rp(16, FALSE, "red"), list(shape = 21L, fill = "white")) && identical(rp(15, FALSE, "red"), list(shape = 22L, fill = "white")) &&
      identical(rp(17, FALSE, "red"), list(shape = 24L, fill = "white")) && identical(rp(18, FALSE, "red"), list(shape = 23L, fill = "white")) &&
      identical(rp(25, FALSE, "red"), list(shape = 25L, fill = "white")))
check("resolve: filled = the solid pch; the down-triangle is pch 25 filled with the colour",
      identical(rp(15, TRUE, "red"), list(shape = 15L, fill = "red")) && identical(rp(17, TRUE, "red"), list(shape = 17L, fill = "red")) &&
      identical(rp(18, TRUE, "red"), list(shape = 18L, fill = "red")) && identical(rp(25, TRUE, "red"), list(shape = 25L, fill = "red")))
check("resolve: 123 / 125 stay the hollow diamond / down-triangle whatever the box says; the old hollow codes 2 and 1 too",
      identical(rp(123, TRUE, "red"), list(shape = 23L, fill = "white")) && identical(rp(125, TRUE, "red"), list(shape = 25L, fill = "white")) &&
      identical(rp(2, TRUE, "red"), list(shape = 24L, fill = "white")) && identical(rp(1, TRUE, "red"), list(shape = 21L, fill = "white")))
check("resolve: the line-drawn forms ignore Filled (4 off is still the cross with the colour as fill)",
      identical(rp(4, FALSE, "red"), list(shape = 4L, fill = "red")) && identical(rp(8, TRUE, "red"), list(shape = 8L, fill = "red")))
mw <- env$legend_marker_words
check("legend words follow resolve_point_shape: open / filled triangles, inverted triangles, crosses, 21 + Filled is a filled circle",
      identical(mw(17, FALSE), "open triangles") && identical(mw(17, TRUE), "filled triangles") && identical(mw(25, TRUE), "filled inverted triangles") &&
      identical(mw(25, FALSE), "open inverted triangles") && identical(mw(125, TRUE), "open inverted triangles") && identical(mw(4, TRUE), "crosses") && identical(mw(4, FALSE), "crosses") &&
      identical(mw(21, TRUE), "filled circles") && identical(mw(21, FALSE), "open circles") && identical(mw(123, TRUE), "open diamonds") && identical(mw(15, FALSE), "open squares"))
check("n4_shape_decode: the N4 open codes decode to a base form + Filled off; others keep their code and leave Filled alone",
      identical(env$n4_shape_decode(2), list(shape = 17L, filled = FALSE)) && identical(env$n4_shape_decode(15), list(shape = 15L, filled = NULL)) &&
      identical(env$n4_shape_decode(21)$shape, 16L))

# ── the figure, the keys, the words ─────────────────────────────────────────
mk_rows <- function(p) {   # the sample-marker rows of a built figure: shape / colour / fill
  b <- ggplot2::ggplot_build(p)$data
  do.call(rbind, lapply(seq_along(b), function(i) {
    d <- b[[i]]
    if (inherits(p$layers[[i]]$geom, "GeomPoint") && all(c("shape", "fill", "colour") %in% names(d)) && all(d$shape %in% c(15:18, 21:25, 3, 4, 8, 10)))
      data.frame(shape = d$shape, colour = toupper(d$colour), fill = ifelse(grepl("^#", d$fill), toupper(d$fill), d$fill), stringsAsFactors = FALSE)
  }))
}
open_sq_hollow <- function(p, col2) { r <- mk_rows(p); r2 <- r[r$colour == col2, , drop = FALSE]; nrow(r2) > 0 && all(r2$shape == 22) && all(r2$fill == "white") }
filled_ok      <- function(p, col1) { r <- mk_rows(p); r1 <- r[r$colour == col1, , drop = FALSE]; nrow(r1) > 0 && all(r1$shape == 16) && all(r1$fill == col1) }
seed <- function(session) do.call(session$setInputs, env$APP_DEFAULTS)

testServer(app, {
  S <- session$setInputs
  ingest_data(env$make_demo_data("lysis"), "Demo"); seed(session)
  S(selected_samples = rv$od_vars, metrics_ref_sample = rv$od_vars[1], export_format = "svg")
  v <- rv$od_vars; ids <- vapply(v, env$safe_id, "")
  sh <- function(i, code, filled) { a <- list(); a[[paste0("shape_", ids[i])]] <- code; a[[paste0("shape_filled_", ids[i])]] <- filled; do.call(S, a) }
  sh(1, 16, TRUE); sh(2, 15, FALSE); sh(3, 17, TRUE)
  aes_v <- resolve_aesthetics(v)
  c1 <- toupper(unname(aes_v$colors[1])); c2 <- toupper(unname(aes_v$colors[2])); c3 <- toupper(unname(aes_v$colors[3]))

  cat("\n--- (a) bars mode ---\n")
  check("resolve_aesthetics stores the normalised base shape and the filled flag", identical(unname(aes_v$shapes), c(16, 15, 17)) && identical(unname(aes_v$filled_map), c(TRUE, FALSE, TRUE)))
  S(error_display_mode = "bars"); p <- generate_plot()
  check("bars: the open square is drawn pch 22 with a WHITE fill, the filled circle solid (pch 16, colour fill)", open_sq_hollow(p, c2) && filled_ok(p, c1))
  gd <- ggplot2::get_guide_data(p, "fill")
  check("bars: the legend key carries the same fill: white for the open square, the colour for the filled samples",
        identical(unname(gd$fill[2]), "white") && identical(toupper(unname(gd$fill[1])), c1) && identical(gd$shape[2], 22) && identical(gd$shape[1], 16))
  check("bars: shape and fill merge into the ONE legend (colour, shape and fill guides are the same data)",
        identical(ggplot2::get_guide_data(p, "colour")$.label, ggplot2::get_guide_data(p, "shape")$.label) && identical(gd$.label, ggplot2::get_guide_data(p, "colour")$.label) && nrow(gd) == 3L)
  k_open <- env$legend_key_grob(p); f_open <- of("key_open.svg"); env$write_legend_key(k_open, f_open, "svg")
  sh(2, 15, TRUE); k_fill <- env$legend_key_grob(generate_plot()); f_fill <- of("key_filled.svg"); env$write_legend_key(k_fill, f_fill, "svg")
  nwhite <- function(f) length(gregexpr("fill: ?#FFFFFF", read_all(f), ignore.case = TRUE)[[1]][gregexpr("fill: ?#FFFFFF", read_all(f), ignore.case = TRUE)[[1]] > 0])
  check("the legend key image shows the open square white-filled (more white fills than the same key with Filled on)", nwhite(f_open) > nwhite(f_fill))
  sh(2, 15, FALSE)

  cat("\n--- (b) shadow / combo ---\n")
  for (dm in c("shadow", "combo")) {
    S(error_display_mode = dm); p <- generate_plot()
    check(paste0(dm, ": the open square is hollow (no sample-colour fill), the filled circle solid"), open_sq_hollow(p, c2) && filled_ok(p, c1))
    rb <- ggplot2::ggplot_build(p)$data[[1]]
    check(paste0(dm, ": the ribbons are still filled with the sample colours"), inherits(p$layers[[1]]$geom, "GeomRibbon") && all(c(c1, c2, c3) %in% toupper(rb$fill)))
    gd <- ggplot2::get_guide_data(p, "fill")
    check(paste0(dm, ": the key fill is white for the open square, the colour for the filled samples"), identical(unname(gd$fill[2]), "white") && identical(toupper(unname(gd$fill[3])), c3))
  }
  k_sh <- env$legend_key_grob(generate_plot()); f_sh <- of("key_shadow.svg"); env$write_legend_key(k_sh, f_sh, "svg")
  check("shadow: the key image is written and mentions every sample colour", file.exists(f_sh) && all(vapply(c(c1, c3), function(h) grepl(h, read_all(f_sh), ignore.case = TRUE), NA)))

  cat("\n--- (c) quantile bands ---\n")
  S(error_display_mode = "quantile_bands"); p <- generate_plot()
  check("quantile bands: the open square is hollow, the bands carry the sample colour",
        open_sq_hollow(p, c2) && filled_ok(p, c1) && inherits(p$layers[[1]]$geom, "GeomRibbon") && all(c(c1, c2) %in% toupper(ggplot2::ggplot_build(p)$data[[1]]$fill)))
  S(error_display_mode = "bars")

  cat("\n--- (d) analysis figures ---\n")
  S(analysis_norm_mode = "reference", diff_mode = "difference")
  S(analysis_dose_metric = "od_drop")
  for (ty in c("normalized", "dose_response", "difference")) {
    S(analysis_type = ty)
    ap <- quiet(generate_analysis_plot(ty))
    r <- mk_rows(ap); r2 <- r[r$colour == c2, , drop = FALSE]
    check(paste0("analysis figure ", ty, ": a real figure; the open square sample is drawn hollow (pch 22, white fill)"),
          inherits(ap, "ggplot") && is.null(plot_error(ap)) && sum(r2$shape == 22) > 0 && all(r2$fill[r2$shape == 22] == "white"))
    r3 <- r[r$colour == c1 & r$shape == 16, , drop = FALSE]
    check(paste0("analysis figure ", ty, ": the filled circle stays solid (fill = its colour)"), nrow(r3) == 0 || all(r3$fill == c1))
  }
  S(analysis_type = "metric_summary")
  sh(1, 16, FALSE)
  an <- quiet(generate_analysis_plot("normalized")); r <- mk_rows(an)
  check("analysis figure: a sample set to Filled off (pch 16) is hollow there now, not solid (the old inconsistency)",
        any(r$colour == c1 & r$shape == 21 & r$fill == "white") && !any(r$colour == c1 & r$shape == 16))
  sh(1, 16, TRUE)

  cat("\n--- (e) legend words and the samples table ---\n")
  lt <- figure_legend_string(); ld <- legend_samples_df()
  check("figure_legend_string says 'open squares' for the open sample and 'filled circles' for the filled one", grepl("open squares", lt, fixed = TRUE) && grepl("filled circles", lt, fixed = TRUE) && grepl("filled triangles", lt, fixed = TRUE))
  check("legend_samples_df matches the words (marker column)", identical(ld$marker, c("filled circles", "open squares", "filled triangles")))

  cat("\n--- (g) a legacy value sent straight into the picker ---\n")
  p16 <- { sh(2, 16, TRUE); r <- mk_rows(generate_plot()); r[r$colour == c2, ] }
  sh(2, 21, TRUE); p21 <- { r <- mk_rows(generate_plot()); r[r$colour == c2, ] }
  check("shape_<id> = 21 (Filled on) draws exactly like 16", nrow(p21) > 0 && identical(p21, p16) && all(p21$shape == 16))
  sh(2, 123, TRUE); r <- mk_rows(generate_plot()); r <- r[r$colour == c2, ]
  check("shape_<id> = 123 draws a hollow diamond (pch 23, white) whatever the box says", all(r$shape == 23) && all(r$fill == "white"))
  sh(2, 1, TRUE); r <- mk_rows(generate_plot()); r <- r[r$colour == c2, ]
  check("shape_<id> = 1 (an old hollow code) draws a hollow circle", all(r$shape == 21) && all(r$fill == "white"))
  sh(2, 15, FALSE)

  cat("\n--- (f) settings files with the old codes ---\n")
  f_old <- of("old_shapes.json")
  jsonlite::write_json(list(version = "2.23.8", styling_schema = 222L, saved_at = "x", global_settings = list(axis_frame = "L"),
                            sample_aesthetics = setNames(list(list(color = "#112233", shape = 21, shape_filled = FALSE),
                                                              list(color = "#223344", shape = 123, shape_filled = TRUE),
                                                              list(color = "#334455", shape = 22, shape_filled = TRUE)), v)), f_old, auto_unbox = TRUE)
  S(loadSettings = list(name = "old.json", datapath = f_old, size = file.size(f_old), type = "application/json"))
  check("loading a file with shape 21 / 123 / 22: status ok", identical(rv$settings_status$type, "ok"))
  imp <- rv$imported_settings$samples
  check("the imported styles are kept as base form + Filled (21 + off -> 16, off; 123 -> 18, off; 22 + on -> 15, on)",
        identical(as.integer(imp[[v[1]]]$shape), 16L) && identical(imp[[v[1]]]$shape_filled, FALSE) &&
        identical(as.integer(imp[[v[2]]]$shape), 18L) && identical(imp[[v[2]]]$shape_filled, FALSE) &&
        identical(as.integer(imp[[v[3]]]$shape), 15L) && identical(imp[[v[3]]]$shape_filled, TRUE))
  ui <- paste(as.character(output$var_settings$html), collapse = "\n")
  pick <- function(id, code) grepl(paste0("(?s)<select id=\"shape_", id, "\".*?<option value=\"", code, "\" selected"), ui, perl = TRUE)
  check("the sidebar picker draws the base form selected (Circle / Diamond / Square) and no 'Open' entries", pick(ids[1], 16) && pick(ids[2], 18) && pick(ids[3], 15) && !grepl("Open ", ui, fixed = TRUE) &&
          grepl(paste0("id=\"shape_filled_", ids[3], "\" type=\"checkbox\" class=\"shiny-input-checkbox\" checked"), ui, fixed = TRUE) &&
          !grepl(paste0("id=\"shape_filled_", ids[2], "\" type=\"checkbox\" class=\"shiny-input-checkbox\" checked"), ui, fixed = TRUE))
  # replay what the client would send after the updates (testServer has no browser)
  for (i in 1:3) { a <- list(); a[[paste0("shape_", ids[i])]] <- imp[[v[i]]]$shape; a[[paste0("shape_filled_", ids[i])]] <- imp[[v[i]]]$shape_filled; do.call(S, a) }
  a2 <- resolve_aesthetics(v)
  check("replayed, resolve_aesthetics gives the same look: (16, open) (18, open) (15, filled)",
        identical(unname(a2$shapes), c(16, 18, 15)) && identical(unname(a2$filled_map), c(FALSE, FALSE, TRUE)))
  check("the saved-back settings hold only base codes (no 21 / 22 / 123)",
        all(vapply(settings_json_list()$sample_aesthetics[v], function(a) as.integer(a$shape) %in% c(16L, 15L, 17L, 18L, 25L, 4L, 8L, 3L, 10L), NA)))
})

cat("\n--- Compare figure (overlay + facet) ---\n")
demo <- env$make_demo_data()
second <- demo; second[-1] <- second[-1] * 1.2
f2 <- of("second.csv"); write.csv(second, f2, row.names = FALSE)
testServer(app, {
  S <- session$setInputs
  ingest_data(demo, "Demo data (built-in)"); seed(session)
  S(selected_samples = rv$od_vars, error_type = "sem", exclude_timepoints = "", panel_size_mode = "fit",
    compare_samples_b = NULL, compare_label_a = "Run 1", compare_label_b = "Run 2")
  v <- rv$od_vars; ids <- vapply(v, env$safe_id, "")
  a <- list(); a[[paste0("shape_", ids[2])]] <- 15; a[[paste0("shape_filled_", ids[2])]] <- FALSE; do.call(S, a)
  c2 <- toupper(unname(resolve_aesthetics(v)$colors[2]))
  S(compare_file = list(name = "second.csv", datapath = f2, size = file.size(f2), type = "text/csv"))
  for (cm in c("overlay", "facet")) {
    S(compare_mode = cm, compare_overlay_style = "linetype", compare_common_only = FALSE)
    pc <- quiet(generate_compare_plot()); r <- mk_rows(pc); r2 <- r[r$colour == c2, , drop = FALSE]
    check(paste0("compare ", cm, ": a real figure; the open sample is hollow (pch 22, white fill)"),
          inherits(pc, "ggplot") && nrow(r2) > 0 && all(r2$shape == 22) && all(r2$fill == "white"))
  }
})

cat("\n--- the Shape menu ---\n")
src <- readLines("Lysis_Curve_App_v28.R", warn = FALSE)
check("the sidebar Shape menu offers base forms only (no 'Open ...' entries) and the box says what it does",
      !any(grepl("\"Open Circle\"|\"Open Square\"|\"Open Triangle\"|\"Open Diamond\"", src)) && any(grepl("Filled (off = open marker)", src, fixed = TRUE)) &&
      any(grepl("\"Triangle Down\" = 25", src, fixed = TRUE)))
check("no figure maps the raw shape codes any more (every sample marker scale is marker_scales())", !any(grepl("scale_shape_manual\\(values = shp, labels = labs_v, name = NULL\\) \\+$", src)))

if (file.exists("Rplots.pdf")) { cat("note: Rplots.pdf appeared\n") }
cat(sprintf("\n============ SUMMARY: %d passed / %d failed / %d skipped ============\n", pass, fail, skip))
if (fail > 0L) quit(status = 1)
