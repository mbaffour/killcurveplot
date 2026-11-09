# ============================================================================
# v2.23.4 paper style: paper_style.json (written by CFU Plot Studio) read at start through an explicit allowlist
# (font family, title size, font size, sample_palette), the colour registry by sample name beneath any colour set by hand,
# the status line and Reload button, and "nothing applied" for a missing or unreadable file.
# Run from the repository root:  Rscript tests/test_v223_paper_style.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v223_paper_style.R")
absent <- file.path(tempdir(), "no_such_paper_style.json")
Sys.setenv(CFU_PAPER_STYLE = absent)          # never pick up a real paper_style.json

pass <- 0L; fail <- 0L
check <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") } else { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.23.4 or later", package_version(env$APP_VERSION) >= "2.23.4")
fx <- function(f) normalizePath(file.path("tests", "fixtures", "paper_style", f))
read_all <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")
hexes_in <- function(txt) toupper(unique(regmatches(txt, gregexpr("#[0-9A-Fa-f]{6}", txt))[[1]]))

cat("\n--- path resolution ---\n")
check("the environment variable wins", identical(env$paper_style_path(), absent))
Sys.setenv(CFU_PAPER_STYLE = "")
check("without it the file is paper_style.json in the app folder (working directory here)", identical(env$paper_style_path(), file.path(getwd(), "paper_style.json")))
Sys.setenv(CFU_PAPER_STYLE = absent)

cat("\n--- paper_style_updates: the allowlist ---\n")
times <- jsonlite::fromJSON(fx("paper_style_times.json"))$settings
up <- env$paper_style_updates(times)
check("Times New Roman -> the Serif menu entry", identical(up$inputs$font_family, "serif"))
check("title_size -> title_font_size", identical(up$inputs$title_font_size, 15))
check("font_size 13 -> axis labels 13, axis text and legend 10 (0.8x, rounded)",
      identical(up$inputs$axis_label_font_size, 13) && identical(up$inputs$axis_text_font_size, 10) && identical(up$inputs$legend_font_size, 10))
check("only allowlisted input ids are ever produced",
      all(names(up$inputs) %in% c("font_family", "title_font_size", "axis_label_font_size", "axis_text_font_size", "legend_font_size")))
check("the palette has the 7 names in upper-case hex", length(up$palette) == 7 && all(grepl("^#[0-9A-F]{6,8}$", up$palette)) &&
      identical(unname(up$palette[["MG1655"]]), "#0072B2") && identical(unname(up$palette[["Strain only in the paper style"]]), "#CC79A7"))
check("cfu-only keys are ignored (plot_title, legend_position, x_angle, download_width, y_max, axis_color, axis_line_width, time_palette)",
      all(c("plot_title", "legend_position", "x_angle", "download_width", "y_max", "axis_color", "axis_line_width", "time_palette") %in% up$ignored) &&
      !any(c("font_family", "font_size", "title_size", "sample_palette") %in% up$ignored))
mf <- env$paper_style_updates(jsonlite::fromJSON(fx("paper_style_missing_font.json"))$settings)
check("a custom family not in the menu goes to the nearest entry (Sans Serif) with a note", identical(mf$inputs$font_family, "sans") && any(grepl("Segoe UI", mf$notes)))
check("a menu font maps to itself; sans/serif/mono keywords too",
      identical(env$paper_style_updates(list(font_family = "Palatino"))$inputs$font_family, "Palatino") &&
      identical(env$paper_style_updates(list(font_family = "mono"))$inputs$font_family, "mono") &&
      identical(env$paper_style_updates(list(font_family = "Courier New"))$inputs$font_family, "mono"))
bad <- env$paper_style_updates(list(font_family = list("a", "b"), title_size = c(1, 2), font_size = "x", sample_palette = list(A = "red", B = list("#000000"), C = "#12345", D = "#abcdef")))
check("a list / vector / text where one value is expected is ignored; bad hex is dropped; lower-case hex is upper-cased",
      length(bad$inputs) == 0 && identical(bad$palette, c(D = "#ABCDEF")))
big <- env$paper_style_updates(list(title_size = 1e9, font_size = -5))
check("sizes are clamped to the controls' ranges", identical(big$inputs$title_font_size, 48) && identical(big$inputs$axis_label_font_size, 5))
check("garbage settings (NULL, a scalar) give an empty result, no error",
      length(env$paper_style_updates(NULL)$inputs) == 0 && length(env$paper_style_updates("x")$palette) == 0)

cat("\n--- testServer with the Times fixture ---\n")
d0 <- env$make_demo_data("lysis")
d_mg <- d0; names(d_mg)[2:4] <- "MG1655"                     # a sample named MG1655 (in the palette), two that are not
boot_code <- quote({
  ingest_data(bd, "Demo"); do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = rv$od_vars, metrics_ref_sample = rv$od_vars[1], export_format = "svg")
})
boot <- function(d = d_mg) { bd <<- d; eval(boot_code, parent.frame()) }
bd <- NULL
Sys.setenv(CFU_PAPER_STYLE = fx("paper_style_times.json"))
testServer(app, {
  session$setInputs(flush_probe = 1)
  check("after the first flush the style was read: rv$paper_style_applied$path is the fixture", identical(rv$paper_style_applied$path, fx("paper_style_times.json")))
  check("the status line says where it came from and how many samples", grepl("Paper style: ", paper_style_status(), fixed = TRUE) && grepl("colours for 7 samples", paper_style_status(), fixed = TRUE))
  inp <- rv$paper_style_applied$inputs
  do.call(session$setInputs, inp)                           # the mock session does not move inputs: replay what was sent
  check("the font inputs received the values", identical(input$font_family, "serif") && identical(input$title_font_size, 15) && identical(input$axis_label_font_size, 13))
  boot()
  samples <- sel_samples(); aes_v <- resolve_aesthetics(samples)
  check("the demo has MG1655 and two samples outside the palette", "MG1655" %in% samples && length(samples) == 3)
  check("MG1655 gets the paper colour #0072B2", identical(toupper(aes_v$colors[["MG1655"]]), "#0072B2"))
  other <- setdiff(samples, "MG1655")
  pp_saved <- rv$paper_palette; rv$paper_palette <- NULL; plain <- resolve_aesthetics(samples)$colors; rv$paper_palette <- pp_saved
  check("a sample not in the palette keeps its default colour (as without the paper style)", identical(unname(aes_v$colors[other]), unname(plain[other])))
  key <- env$legend_key_grob(generate_plot()); f <- tempfile(fileext = ".svg"); env$write_legend_key(key, f, "svg")
  check("the legend key SVG carries the paper colour", "#0072B2" %in% hexes_in(read_all(f)))
  lg <- figure_legend_string()
  check("the legend text carries its colour word", grepl("MG1655 (blue", lg, fixed = TRUE))
  # a colour picked in the sample's own selector wins over the paper colour
  id <- env$safe_id("MG1655")
  do.call(session$setInputs, setNames(list("custom", "#984EA3"), paste0(c("color_selector_", "color_"), id)))
  check("a colour set by hand in the sample box beats the paper colour", identical(toupper(resolve_aesthetics(samples)$colors[["MG1655"]]), "#984EA3"))
  do.call(session$setInputs, setNames(list(NULL, NULL), paste0(c("color_selector_", "color_"), id)))
  rv$qe_colors <- list(MG1655 = "#FF0000")
  check("a quick-edit colour set afterwards beats the paper colour", identical(toupper(resolve_aesthetics(samples)$colors[["MG1655"]]), "#FF0000"))
  rv$qe_colors <- list()
  rv$display_names <- stats::setNames("WT", "Phage MOI 10")
  check("a sample whose display name is in the palette takes that colour", identical(toupper(resolve_aesthetics(samples)$colors[["Phage MOI 10"]]), "#0072B2"))
  rv$display_names <- NULL
})

cat("\n--- no file: nothing applied, figure byte-identical ---\n")
svg_of <- function(path_env) {
  if (is.null(path_env)) Sys.unsetenv("CFU_PAPER_STYLE") else Sys.setenv(CFU_PAPER_STYLE = path_env)
  out <- NULL; st <- NULL; ap <- "x"
  testServer(app, {
    session$setInputs(flush_probe = 1)
    boot(d0)
    st <<- paper_style_status(); ap <<- rv$paper_style_applied
    f <- tempfile(fileext = ".svg")
    ggsave(f, plot = main_plot_obj(), device = env$export_device("svg"), width = 3.5, height = 2.56, units = "in", bg = "white")
    out <<- read_all(f)
  })
  list(svg = out, status = st, applied = ap)
}
a1 <- svg_of(absent); a2 <- svg_of(NULL)
check("no file: the status says so and nothing was applied", grepl("No paper style (looked for", a1$status, fixed = TRUE) && is.null(a1$applied))
check("no file: the demo figure SVG is identical with the variable pointing at nothing and with it unset", nzchar(a1$svg) && identical(a1$svg, a2$svg))

cat("\n--- unreadable files ---\n")
big_f <- tempfile(fileext = ".json"); writeLines(paste0('"', strrep("a", 5e6), '"'), big_f)
for (u in list(c("a lone brace", fx("unreadable_brace.json")), c("a scalar", fx("unreadable_scalar.json")), c("a 5 MB string", big_f))) {
  Sys.setenv(CFU_PAPER_STYLE = u[2]); got <- NULL
  err <- tryCatch({
    testServer(app, { session$setInputs(flush_probe = 1); boot(d0); got <<- list(st = paper_style_status(), ap = rv$paper_style_applied, n = length(sel_samples()))
                      session$setInputs(reload_paper_style = 1) })
    NULL }, error = function(e) conditionMessage(e))
  check(paste0(u[1], ": no error, status says it could not be read, nothing applied, session alive"),
        is.null(err) && grepl("could not be read; nothing was applied", got$st, fixed = TRUE) && is.null(got$ap) && got$n == 3)
}
Sys.setenv(CFU_PAPER_STYLE = absent)
check("a directory in place of the file is 'No paper style', not an error", {
  Sys.setenv(CFU_PAPER_STYLE = tempdir()); r <- NULL
  testServer(app, { session$setInputs(flush_probe = 1); r <<- paper_style_status() }); Sys.setenv(CFU_PAPER_STYLE = absent)
  grepl("No paper style", r, fixed = TRUE) })

cat("\n--- Reload picks up a file that appears later ---\n")
rl <- tempfile(fileext = ".json")
testServer(app, {
  Sys.setenv(CFU_PAPER_STYLE = rl)
  session$setInputs(flush_probe = 1)
  check("before the file exists: nothing applied", is.null(rv$paper_style_applied))
  file.copy(fx("paper_style_times.json"), rl, overwrite = TRUE)
  session$setInputs(reload_paper_style = 1)
  check("Reload reads it", identical(rv$paper_style_applied$path, rl) && length(rv$paper_palette) == 7)
})
Sys.setenv(CFU_PAPER_STYLE = absent)

cat(sprintf("\nSUMMARY: %d passed, %d failed\n", pass, fail))
if (file.exists("Rplots.pdf")) unlink("Rplots.pdf")
if (fail > 0) quit(status = 1)
