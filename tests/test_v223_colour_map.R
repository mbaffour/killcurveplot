# ============================================================================
# v2.23.8 colour map: Load colour map (CSV: Sample, Hex, Colour_Name) and Download colour map (CSV).
# read_colour_map() on every fixture, the upload observer (good and malformed messages), the precedence (hand > map >
# paper style > palette > defaults), the figure / legend key / legend text following, Clear, the download's exact format,
# the round trip, and the refusal with no data.
# Run from the repository root:  Rscript tests/test_v223_colour_map.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v223_colour_map.R")
Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_such_paper_style.json"))   # never pick up a real paper_style.json

pass <- 0L; fail <- 0L
check <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") } else { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.23.8 or later", package_version(env$APP_VERSION) >= "2.23.8")
fx <- function(f) normalizePath(file.path("tests", "fixtures", "colour_map", f))
read_all <- function(f) paste(readLines(f, warn = FALSE), collapse = "\n")
hexes_in <- function(txt) toupper(unique(regmatches(txt, gregexpr("#[0-9A-Fa-f]{6}", txt))[[1]]))
up_msg <- function(f, name = basename(f)) data.frame(name = name, size = file.size(f), type = "text/csv", datapath = f, stringsAsFactors = FALSE)
with_notes <- function(fn) {
  got <- character(0); old <- env$showNotification
  env$showNotification <- function(ui, ..., type = "default") { got <<- c(got, paste0("[", type, "] ", as.character(ui))); invisible(NULL) }
  on.exit(env$showNotification <- old)
  val <- fn(); list(value = val, notes = got)
}

cat("\n--- read_colour_map on the fixtures ---\n")
ex <- env$read_colour_map(fx("example_colour_map.csv"))
check("the example file: 12 colours, none skipped, nothing missing", length(ex$palette) == 12 && ex$skipped == 0L && is.null(ex$missing))
check("the example file: names and upper-case hex as written (a name with a space and a dot-free decimal)",
      identical(unname(ex$palette[["pSJExD gp75 12.5nM"]]), "#0072B2") && identical(names(ex$palette)[1], "pSJExD 0nM") && all(grepl("^#[0-9A-F]{6}$", ex$palette)))
sb <- env$read_colour_map(fx("semicolon_bom_crlf.csv"))
check("semicolon + BOM + CRLF + quotes: 3 colours, first header not damaged by the BOM, hex upper-cased",
      identical(sb$palette, c(Alpha = "#0072B2", `Beta strain` = "#D55E00", Gamma = "#009E73")) && is.null(sb$missing))
lc <- env$read_colour_map(fx("lowercase_color.csv"))
check("lower-case 'sample,color' headers, an extra column ignored, names trimmed", identical(lc$palette, c(Alpha = "#0072B2", Beta = "#D55E00")))
br <- env$read_colour_map(fx("bad_rows.csv"))
check("bad rows (red, 5 digits, no #, empty name, GG) are skipped and counted; #RRGGBBAA is kept",
      identical(br$palette, c(Alpha = "#0072B2", Beta = "#D55E00FF")) && br$skipped == 5L)
du <- env$read_colour_map(fx("duplicates.csv"))
check("a repeated name: the first row wins", identical(du$palette, c(Alpha = "#111111", Beta = "#222222")))
check("no Hex column -> missing = 'Hex'", identical(env$read_colour_map(fx("no_hex.csv"))$missing, "Hex"))
check("no Sample column -> missing = 'Sample'", identical(env$read_colour_map(fx("no_sample.csv"))$missing, "Sample"))
ab <- env$read_colour_map(fx("all_bad.csv"))
check("every row bad -> an empty palette with the rows counted", length(ab$palette) == 0 && ab$skipped == 2L)
check("an empty file and a binary file are errors (not a silent empty map)",
      inherits(tryCatch(env$read_colour_map(fx("empty.csv")), error = function(e) e), "error") &&
      (inherits(tryCatch(env$read_colour_map(fx("binary.csv")), error = function(e) e), "error") ||
       { r <- env$read_colour_map(fx("binary.csv")); !is.null(r$missing) || length(r$palette) == 0 }))
big <- tempfile(fileext = ".csv")
writeLines(c("Sample,Hex", sprintf("s%d,#%06X", 1:2500, 1:2500)), big)
check("at most 2000 colours are kept", length(env$read_colour_map(big)$palette) == 2000L)

cat("\n--- testServer: the upload observer ---\n")
d0 <- env$make_demo_data("lysis")
boot_code <- quote({
  ingest_data(bd, "Demo"); do.call(session$setInputs, env$APP_DEFAULTS)
  session$setInputs(selected_samples = rv$od_vars, metrics_ref_sample = rv$od_vars[1], export_format = "svg")
})
boot <- function(d = d0) { bd <<- d; eval(boot_code, parent.frame()) }
bd <- NULL
demo_names <- setdiff(names(d0), names(d0)[1])      # the sample columns (the first is the time column)

r1 <- with_notes(function() {
  out <- list()
  testServer(app, {
    out$status0 <<- output$colour_map_status
    session$setInputs(colour_map_file = up_msg(fx("example_colour_map.csv")))
    out$map <<- rv$colour_map; out$src <<- rv$colour_map_source; out$status1 <<- output$colour_map_status
  })
  out
})
check("no data: the map loads (12 colours) and the status line names the file",
      length(r1$value$map) == 12 && identical(r1$value$src, "example_colour_map.csv") && identical(r1$value$status0, "No colour map loaded") &&
      identical(r1$value$status1, "Colour map: example_colour_map.csv (12 colours, 0 match)"))
check("no data: one notification, 'they apply when a file with those names is loaded'",
      sum(grepl("^\\[message\\] Colour map 'example_colour_map.csv': 12 colours; they apply when a file with those names is loaded", r1$notes)) == 1 && length(r1$notes) == 1)

r2 <- with_notes(function() {
  out <- list()
  testServer(app, {
    session$setInputs(flush_probe = 1); boot()
    session$setInputs(colour_map_file = up_msg(fx("demo_names.csv")))
    out$status <<- output$colour_map_status
  })
  out
})
check("with data: 'N colours, M match the loaded samples' (3 / 3), status 'Colour map: demo_names.csv (3 colours, 3 match)'",
      any(grepl("Colour map 'demo_names.csv': 3 colours, 3 match the loaded samples\\.", r2$notes)) && identical(r2$value$status, "Colour map: demo_names.csv (3 colours, 3 match)"))
r3 <- with_notes(function() {
  testServer(app, { session$setInputs(flush_probe = 1); boot(); session$setInputs(colour_map_file = up_msg(fx("bad_rows.csv"))) })
  NULL
})
check("bad rows: the notification adds '5 rows skipped (empty name or not a #RRGGBB colour)'",
      sum(grepl("5 rows skipped \\(empty name or not a #RRGGBB colour\\)", r3$notes)) == 1)

bad_files <- c("no_hex.csv", "no_sample.csv", "all_bad.csv", "empty.csv", "binary.csv")
for (bf in bad_files) {
  rr <- with_notes(function() {
    out <- list(); err <- NULL
    err <- tryCatch({
      testServer(app, { session$setInputs(flush_probe = 1); boot(); rv$colour_map <- c(Keep = "#123456"); rv$colour_map_source <- "kept.csv"
                        session$setInputs(colour_map_file = up_msg(fx(bf)))
                        out$map <<- rv$colour_map; out$src <<- rv$colour_map_source; out$n <<- length(sel_samples()) })
      NULL }, error = function(e) conditionMessage(e))
    out$err <- err; out
  })
  check(sprintf("%s: no error, one error notification, the map in place is unchanged, the session is alive", bf),
        is.null(rr$value$err) && identical(rr$value$map, c(Keep = "#123456")) && identical(rr$value$src, "kept.csv") && rr$value$n == 3 &&
        sum(grepl("^\\[error\\]", rr$notes)) == 1)
}
check("an unreadable file's message does not show raw R text (no 'Error in', no 'scan(')",
      { rr <- with_notes(function() { testServer(app, { session$setInputs(colour_map_file = up_msg(fx("empty.csv"))) }); NULL })
        length(rr$notes) == 1 && !grepl("Error in|scan\\(|read\\.table", rr$notes) })

malformed <- list(
  "a scalar" = "x", "NA" = NA, "an empty list" = list(), "a number" = 3,
  "a list without datapath" = list(name = "a.csv"),
  "a data frame without datapath" = data.frame(name = "a.csv", size = 1, type = "text/csv"),
  "a NA datapath" = data.frame(name = "a.csv", size = 1, type = "text/csv", datapath = NA_character_, stringsAsFactors = FALSE),
  "a vector datapath" = list(name = "a.csv", datapath = c(fx("duplicates.csv"), fx("duplicates.csv"))),
  "a missing file" = data.frame(name = "gone.csv", size = 1, type = "text/csv", datapath = file.path(tempdir(), "gone.csv"), stringsAsFactors = FALSE))
for (nm in names(malformed)) {
  msg <- malformed[[nm]]
  got <- with_notes(function() {
    err <- tryCatch({ testServer(app, { session$setInputs(flush_probe = 1); boot(); session$setInputs(colour_map_file = msg)
                                        session$setInputs(clear_colour_map = 1) }); NULL }, error = function(e) conditionMessage(e))
    err })
  check(sprintf("malformed upload message (%s): ignored or one error note, never an error", nm), is.null(got$value) && length(got$notes) <= 2)
}

cat("\n--- testServer: colours follow the map ---\n")
hand_id <- env$safe_id("Phage MOI 10")
testServer(app, {
  session$setInputs(flush_probe = 1); boot()
  samples <- sel_samples()
  plain <- resolve_aesthetics(samples)$colors
  session$setInputs(colour_map_file = up_msg(fx("demo_names.csv")))
  aes_m <- resolve_aesthetics(samples)
  check("a CSV with the demo names: every sample takes its map colour in resolve_aesthetics()",
        identical(unname(toupper(aes_m$colors[samples])), c("#CC79A7", "#009E73", "#F0E442")))
  check("the default colours differ from the map colours (so the check above means something)", !identical(unname(toupper(plain)), unname(toupper(aes_m$colors))))
  key <- env$legend_key_grob(generate_plot()); f <- tempfile(fileext = ".svg"); env$write_legend_key(key, f, "svg")
  check("the legend key SVG carries the three map colours", all(c("#CC79A7", "#009E73", "#F0E442") %in% hexes_in(read_all(f))))
  lg <- figure_legend_string()
  check("figure_legend_string() says the map's colour words (pink, teal, yellow)",
        grepl("Uninfected (pink", lg, fixed = TRUE) && grepl("Phage MOI 10 (teal", lg, fixed = TRUE) && grepl("Phage MOI 0.1 (yellow", lg, fixed = TRUE))
  sd <- legend_samples_df()
  check("the Samples-as-drawn table has the map hex values", identical(sd$hex, c("#CC79A7", "#009E73", "#F0E442")))
  # the map beats the paper style for the same name
  rv$paper_palette <- c(Uninfected = "#111111", `Phage MOI 10` = "#222222", `Phage MOI 0.1` = "#333333")
  check("the map beats the paper-style palette for the same name", identical(unname(toupper(resolve_aesthetics(samples)$colors[samples])), c("#CC79A7", "#009E73", "#F0E442")))
  rv$colour_map <- c(Uninfected = "#CC79A7")
  r <- toupper(resolve_aesthetics(samples)$colors)
  check("a sample missing from the map falls to the paper style, and the map still holds for the others",
        identical(unname(r[["Uninfected"]]), "#CC79A7") && identical(unname(r[["Phage MOI 10"]]), "#222222") && identical(unname(r[["Phage MOI 0.1"]]), "#333333"))
  rv$colour_map <- c(Uninfected = "#CC79A7", `Phage MOI 10` = "#009E73", `Phage MOI 0.1` = "#F0E442")
  # a colour picked by hand beats the map; so does a quick-edit colour
  do.call(session$setInputs, setNames(list("custom", "#984EA3"), paste0(c("color_selector_", "color_"), hand_id)))
  check("a colour set by hand in the sample box beats the map", identical(toupper(resolve_aesthetics(samples)$colors[["Phage MOI 10"]]), "#984EA3"))
  do.call(session$setInputs, setNames(list(NULL, NULL), paste0(c("color_selector_", "color_"), hand_id)))
  rv$qe_colors <- list(`Phage MOI 10` = "#FF0000")
  check("a quick-edit colour beats the map", identical(toupper(resolve_aesthetics(samples)$colors[["Phage MOI 10"]]), "#FF0000"))
  rv$qe_colors <- list()
  rv$display_names <- stats::setNames("WT", "Phage MOI 10"); rv$colour_map <- c(WT = "#ABCDEF")
  check("a map keyed by the display name also applies", identical(toupper(resolve_aesthetics(samples)$colors[["Phage MOI 10"]]), "#ABCDEF"))
  rv$display_names <- NULL; rv$colour_map <- c(Uninfected = "#CC79A7", `Phage MOI 10` = "#009E73", `Phage MOI 0.1` = "#F0E442"); rv$paper_palette <- NULL
  # Clear
  with_notes(function() { session$setInputs(clear_colour_map = 1); NULL }) -> cl
  check("Clear: the map and its source are NULL, one 'cleared' notification, the status line is back", is.null(rv$colour_map) && is.null(rv$colour_map_source) &&
        sum(grepl("Colour map cleared", cl$notes)) == 1 && identical(output$colour_map_status, "No colour map loaded"))
  check("Clear restores the default colours", identical(unname(toupper(resolve_aesthetics(samples)$colors)), unname(toupper(plain))))
})

cat("\n--- testServer: a map loaded first applies when data come later ---\n")
testServer(app, {
  session$setInputs(flush_probe = 1)
  session$setInputs(colour_map_file = up_msg(fx("demo_names.csv")))
  boot()
  check("the colours apply to samples loaded after the map", identical(unname(toupper(resolve_aesthetics(sel_samples())$colors)), c("#CC79A7", "#009E73", "#F0E442")))
  check("the status counts the matches once data are loaded", identical(output$colour_map_status, "Colour map: demo_names.csv (3 colours, 3 match)"))
})

cat("\n--- testServer: Download colour map ---\n")
dl_state <- new.env()
testServer(app, {
  session$setInputs(flush_probe = 1); boot()
  session$setInputs(colour_map_file = up_msg(fx("demo_names.csv")))
  f <- output$downloadColourMap
  dl_state$file <- f
  raw <- readBin(f, "raw", file.size(f))
  txt <- rawToChar(raw)
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  check("the download exists and has no BOM", file.exists(f) && !identical(raw[1:3], as.raw(c(0xef, 0xbb, 0xbf))))
  check("the header is exactly \"Sample\",\"Hex\",\"Colour_Name\"", identical(sub("\r$", "", lines[1]), '"Sample","Hex","Colour_Name"'))
  check("one quoted row per drawn sample, in legend order: display name, upper-case hex, colour word",
        identical(sub("\r$", "", lines[-1]), c('"Uninfected","#CC79A7","pink"', '"Phage MOI 10","#009E73","teal"', '"Phage MOI 0.1","#F0E442","yellow"')))
  back <- env$read_colour_map(f)
  sd <- legend_samples_df()
  check("round trip: the file loads back through read_colour_map() to exactly the colours drawn (names and hex)",
        identical(back$palette, stats::setNames(sd$hex, sd$name)) && back$skipped == 0L)
  check("the downloaded words are the legend text's words", identical(read.csv(f, stringsAsFactors = FALSE)$Colour_Name, sd$colour))
  # default colours also round trip, and a display-name edit shows in the Sample column
  rv$colour_map <- NULL; rv$display_names <- stats::setNames("Wild type", "Phage MOI 10")
  f2 <- output$downloadColourMap
  back2 <- env$read_colour_map(f2)
  check("default colours: the download carries the display name and loads back unchanged",
        identical(names(back2$palette), c("Uninfected", "Wild type", "Phage MOI 0.1")) && identical(unname(back2$palette), toupper(unname(resolve_aesthetics(sel_samples())$colors))))
  rv$display_names <- NULL
})
check("the example file's header line matches the download's header line exactly",
      identical(sub("\r$", "", readLines(fx("example_colour_map.csv"), n = 1)), '"Sample","Hex","Colour_Name"'))
check("the example file loads, and its re-written form loads to the same palette", {
  tmp <- tempfile(fileext = ".csv")
  q <- function(x) paste0('"', x, '"')
  writeLines(c('"Sample","Hex","Colour_Name"', paste(q(names(ex$palette)), q(unname(ex$palette)), q("x"), sep = ",")), tmp)
  identical(env$read_colour_map(tmp)$palette, ex$palette) })

cat("\n--- download with no data is refused ---\n")
emp <- with_notes(function() {
  out <- NULL
  testServer(app, {
    do.call(session$setInputs, env$APP_DEFAULTS); session$setInputs(export_format = "svg")
    out <<- tryCatch({ f <- output$downloadColourMap; paste("FILE", file.exists(f)) }, error = function(e) conditionMessage(e))
  })
  out
})
check("with no data the download is refused ('Nothing to export'), no file", grepl("^Nothing to export", emp$value))
check("the refusal is a warning notification", any(grepl("^\\[warning\\] Nothing to export", emp$notes)))
emp2 <- with_notes(function() {
  out <- NULL
  testServer(app, {
    ingest_data(d0, "Demo"); do.call(session$setInputs, env$APP_DEFAULTS)
    session$setInputs(selected_samples = rv$od_vars, export_format = "svg", enable_time_filter = TRUE, exclude_timepoints = paste(rv$all_timepoints, collapse = ","))
    out <<- tryCatch({ f <- output$downloadColourMap; paste("FILE", file.exists(f)) }, error = function(e) conditionMessage(e))
  })
  out
})
check("with every time point filtered away the download is refused too", grepl("^Nothing to export", emp2$value))

cat("\n--- the source: names and places ---\n")
src <- readLines("Lysis_Curve_App_v28.R", warn = FALSE)
check("the sidebar has the picker, the status line and the Clear link", any(grepl('fileInput("colour_map_file"', src, fixed = TRUE)) &&
      any(grepl('textOutput("colour_map_status"', src, fixed = TRUE)) && any(grepl('actionLink("clear_colour_map"', src, fixed = TRUE)))
check("the download is named colour_map_<date>.csv", any(grepl('paste0("colour_map_", format(Sys.Date(), "%Y-%m-%d"), ".csv")', src, fixed = TRUE)))
check("the Legend text tab has the download button", any(grepl('downloadButton("downloadColourMap"', src, fixed = TRUE)))
check("the colour map is not an APP_DEFAULTS entry (it is not a setting)", !any(grepl("colour_map", names(env$APP_DEFAULTS))))

cat(sprintf("\nSUMMARY: %d passed, %d failed\n", pass, fail))
if (file.exists("Rplots.pdf")) unlink("Rplots.pdf")
if (fail > 0) quit(status = 1)
