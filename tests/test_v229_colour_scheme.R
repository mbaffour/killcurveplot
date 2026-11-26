# ============================================================================
# v2.29.0: a colour scheme for a paper.
#   - the scheme's colours are offered where a sample's colour is picked (the Quick edit strip's colour square)
#   - a scheme (a theme file) holds its colours (`palette`) and, by sample name, each sample's style; saving ADDS the
#     loaded samples to it; samples it does not know take its next free colours
#   - it is chosen, used and saved from the Quick edit strip; it can be downloaded, loaded from a file, and it travels
#     inside a project file
# Run from the repository root:  Rscript tests/test_v229_colour_scheme.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v229_colour_scheme.R")
Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_paper_style_here.json"))
th_dir <- file.path(tempdir(), paste0("kc_schemes_", Sys.getpid())); unlink(th_dir, recursive = TRUE)
options(killcurve.themes_dir = th_dir)
pass <- 0L; fail <- 0L
check <- function(desc, cond) { if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") } else { fail <<- fail + 1L; cat("FAIL:", desc, "\n") } }
env <- new.env(); app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("APP_VERSION is 2.29.0 or later", package_version(env$APP_VERSION) >= "2.29.0")

cat("\n--- the scheme's colours as typed ---\n")
pc <- env$theme_palette_clean
check("hex with or without #, short hex, colour names; commas, semicolons or spaces; upper case, no repeats",
      identical(pc("#E69F00, 56b4e9; red  #abc skyblue #e69f00"), c("#E69F00", "#56B4E9", "#FF0000", "#AABBCC", "#87CEEB")))
check("junk is dropped (a bare number is R's palette index, not a colour); NULL, NA, a list and nothing give none",
      identical(pc("1 nope #12345 #GGGGGG 12"), character(0)) && identical(pc(NULL), character(0)) && identical(pc(NA), character(0)) &&
        identical(pc(list("#000000", list("blue"))), c("#000000", "#0000FF")) && identical(pc(""), character(0)))
check("at most SCHEME_PALETTE_MAX colours", length(pc(grDevices::hcl(seq(0, 359, length.out = 90), 60, 60))) == env$SCHEME_PALETTE_MAX)

cat("\n--- the scheme file: saving adds, colours are kept ---\n")
a1 <- list(EV = list(color = "#000000", shape = 16, shape_filled = TRUE, line_type = "solid", legend_label = "Empty vector"),
           `N4 gp60` = list(color = "#E69F00", shape = 15, shape_filled = TRUE, line_type = "solid", legend_label = "N4 gp60"))
k <- env$theme_write("Paper one", a1, palette = c("#000000", "#E69F00", "#56B4E9", "#009E73"))
a2 <- list(`n4  GP60` = list(color = "#D55E00", shape = 15, shape_filled = TRUE, line_type = "dashed", legend_label = "gp60"),
           Third = list(color = "#56B4E9", shape = 17, shape_filled = TRUE, line_type = "solid", legend_label = "Third"))
k2 <- env$theme_write("Paper one", a2)
r <- env$theme_read(k)
check("a second save to the same scheme keeps the first file's samples and adds the new ones",
      identical(k, "Paper_one") && identical(k2, k) && setequal(names(r$sample_aesthetics), c("EV", "n4  GP60", "Third")))
check("a sample saved again (capitals / spacing aside) is one entry with its new style", identical(r$sample_aesthetics[["n4  GP60"]]$color, "#D55E00") && identical(r$sample_aesthetics[["n4  GP60"]]$line_type, "dashed"))
check("the scheme's colours are kept when a save gives none", isTRUE(r$has_palette) && identical(r$palette, c("#000000", "#E69F00", "#56B4E9", "#009E73")))
env$theme_write("Paper one", a2, merge = FALSE)
check("merge = FALSE replaces the file (an imported scheme)", setequal(names(env$theme_read(k)$sample_aesthetics), c("n4  GP60", "Third")) && !isTRUE(env$theme_read(k)$has_palette))
write_json(list(version = "1.0", name = "Old", sample_aesthetics = list(WT = list(color = "#112233"), Mut = list(color = "#445566"))), file.path(th_dir, "Old.json"), auto_unbox = TRUE)
old <- env$theme_read("Old")
check("a theme saved before v2.29 reads as before: no colours of its own (its samples' colours stand in for the swatches)",
      !isTRUE(old$has_palette) && identical(old$palette, c("#112233", "#445566")))
write_json(list(version = "1.1", name = "Only colours", palette = list("#101010", "#202020")), file.path(th_dir, "Only_colours.json"), auto_unbox = TRUE)
oc <- env$theme_read("Only_colours")
check("colours and no samples yet is a scheme; a file with neither is not", isTRUE(oc$has_palette) && length(oc$sample_aesthetics) == 0L && {
  writeLines("{\"name\":\"x\"}", file.path(th_dir, "Empty.json")); is.null(env$theme_read("Empty")) })
m <- env$theme_for_samples(r, c("ev", "N4 gp60", "Unknown"))
check("samples are matched by name, capitals and spacing aside; the entries come back under the loaded names",
      identical(m$matched, c("ev", "N4 gp60")) && identical(names(m$sample_aesthetics), c("ev", "N4 gp60")) && identical(m$sample_aesthetics[["N4 gp60"]]$color, "#D55E00"))
fr <- env$scheme_free_colours(r, 2)
check("samples the scheme does not know take its colours that no known sample has, in order", identical(fr, c("#E69F00", "#009E73")))
fr6 <- env$scheme_free_colours(r, 6)
check("when the scheme's colours run out, further distinct colours follow (none repeated, none a scheme colour)",
      length(fr6) == 6 && !anyDuplicated(fr6) && identical(fr6[1:2], fr) && !any(fr6[3:6] %in% c(r$palette, "#D55E00")))
unlink(list.files(th_dir, full.names = TRUE))

cat("\n--- the page ---\n")
rt <- htmltools::renderTags(env$ui); h <- paste(rt$head, rt$html)
has <- function(...) all(vapply(c(...), function(x) grepl(x, h, fixed = TRUE), NA))
check("the Quick edit strip has the scheme menu, its colours, 'Use on these samples' and 'Save colours to scheme'",
      has("id=\"qe_scheme\"", "id=\"qe_scheme_swatches\"", "id=\"qe_scheme_apply\"", "id=\"qe_scheme_save\"", "Save colours to scheme"))
check("the sidebar box has the scheme's colours, download and load", has("id=\"theme_palette\"", "id=\"download_theme\"", "id=\"theme_file\"", "Colour scheme for a paper"))
check("the page script opens the scheme's colours from a row's colour square (keyboard: arrows, Escape)",
      has("function kcSchemeOpen", "function kcSchemeClose", "button.kc-qe-swatch", "ArrowRight", "kc-scheme-opt"))
check("the strip's menu is a run-time list, read against the saved schemes", "qe_scheme" %in% env$DYNAMIC_SELECT_IDS)

mk <- function(nm, seed = 1) { t <- seq(0, 120, 20); set.seed(seed); cols <- list(Time = t)
  for (i in seq_along(nm)) for (r in 1:3) cols[[length(cols) + 1]] <- round(0.05 * exp(0.02 * t) * (1 + i / 20) * exp(rnorm(length(t), 0, 0.02)), 4)
  d <- as.data.frame(cols); names(d) <- c("Time", rep(nm, each = 3)); d }
cat("\n--- one scheme over two experiments ---\n")
R <- new.env()
testServer(app, {
  S <- session$setInputs
  ingest_data(mk(c("EV", "N4 gp60", "N4 gp75")), "exp1.csv"); do.call(S, env$APP_DEFAULTS)
  S(selected_samples = rv$od_vars, active_theme = "", qe_scheme = "", theme_palette = "", export_format = "svg")
  R$pal_none <- scheme_palette_now()
  R$sw_none <- as.character(output$qe_scheme_swatches$html)
  R$rows <- as.character(output$quick_edit_rows$html)
  base <- resolve_aesthetics(rv$od_vars)$colors
  S(qe_color_N4_gp60 = "#D55E00"); session$flushReact()
  key <- theme_save_current("Paper one"); R$key <- key; R$t1 <- env$theme_read(key)
  S(active_theme = key, qe_scheme = key); session$flushReact()
  R$pal_scheme <- scheme_palette_now(); R$sw_scheme <- as.character(output$qe_scheme_swatches$html)
  gs <- settings_json_list()$global_settings; R$gs_names <- names(gs); R$gs_theme <- gs$active_theme
  # the second experiment: one known sample (other capitals), two new ones
  ingest_data(mk(c("ev", "New strain", "N4 GP75", "Other strain"), 2), "exp2.csv"); session$flushReact()
  S(selected_samples = rv$od_vars)
  R$c2 <- unlist(rv$qe_colors[rv$od_vars]); R$filled <- scheme_last$filled
  R$legend <- figure_legend_string()
  key2 <- theme_save_current("Paper one"); R$t2 <- env$theme_read(key2)
  # a project file carries the scheme
  pf <- tempfile(fileext = ".kcproj"); write_project_file(pf); R$doc <- fromJSON(pf, simplifyVector = FALSE)
  env$theme_delete(key2); R$gone <- !file.exists(env$theme_path(key2))
  R$opened <- load_project(pf, "p.kcproj"); R$back <- env$theme_read(key2)
  # a scheme that is here is not overwritten by a project's copy
  env$theme_write("Paper one", list(Local = list(color = "#ABCDEF")), merge = FALSE, key = key2)
  load_project(pf, "p.kcproj"); R$kept_local <- names(env$theme_read(key2)$sample_aesthetics)
  # scheme files
  R$imp_bad <- c(is.null(scheme_import_doc("junk", "x")), is.null(scheme_import_doc(list(name = "n"), "x")), is.null(scheme_import_doc(list(1, 2), "x")))
  R$imp <- scheme_import_doc(list(name = "Paper one", palette = list("#111111", "nope", "#222222"), sample_aesthetics = list(A = list(color = "#111111", evil = "x"), B = "not a list")), "file")
  R$imp_read <- env$theme_read(R$imp)
  # a theme without colours of its own styles the names it knows and nothing else
  write_json(list(version = "1.0", name = "Legacy", sample_aesthetics = list(ev = list(color = "#123456"))), file.path(th_dir, "Legacy.json"), auto_unbox = TRUE)
  rv$qe_colors <- list(); R$legacy_k <- theme_apply_named("Legacy"); R$legacy_cols <- rv$qe_colors; R$legacy_filled <- scheme_last$filled
  # no data: the typed colours alone make a scheme
  rv$od_vars <- NULL; S(theme_palette = "#010101, #020202")
  R$empty_key <- tryCatch(theme_save_current("Colours only"), error = function(e) conditionMessage(e)); R$empty <- env$theme_read("Colours_only")
  S(theme_palette = ""); R$empty_err <- tryCatch(theme_save_current("Nothing"), error = function(e) "stopped")
})
hx <- function(x) toupper(unname(x))
check("with no scheme the picker offers the palette in use (Okabe-Ito here)", identical(R$pal_none[1:3], c("#000000", "#E69F00", "#56B4E9")) && grepl("data-colors=\"#000000,#E69F00", R$sw_none, fixed = TRUE))
check("each row's colour square is a labelled button", grepl("<button[^>]*class=\"kc-qe-swatch\"[^>]*aria-label=\"Pick a scheme colour for EV\"", R$rows) || grepl("aria-label=\"Pick a scheme colour for EV\"", R$rows, fixed = TRUE) && grepl("button", R$rows, fixed = TRUE))
check("Save: the scheme holds the samples' colours by name and its colours include every colour in use",
      identical(R$t1$sample_aesthetics[["N4 gp60"]]$color, "#D55E00") && isTRUE(R$t1$has_palette) && all(c("#000000", "#D55E00", "#56B4E9") %in% R$t1$palette))
check("with a scheme chosen the picker offers the scheme's colours, named apart ('dark blue' / 'light blue')",
      identical(R$pal_scheme, R$t1$palette) && grepl("Colours of the scheme", R$sw_scheme, fixed = TRUE) && grepl("light blue", R$sw_scheme, fixed = TRUE) && grepl("dark blue", R$sw_scheme, fixed = TRUE))
check("the scheme's controls are not settings; the chosen scheme's name is",
      !any(c("qe_scheme", "theme_palette", "qe_scheme_save", "qe_scheme_apply", "theme_file") %in% R$gs_names) && identical(R$gs_theme, R$key))
check("the next file: known names (capitals aside) get their colours back",
      identical(hx(R$c2[["ev"]]), "#000000") && identical(hx(R$c2[["N4 GP75"]]), hx(R$t1$sample_aesthetics[["N4 gp75"]]$color)))
check("the next file: the two new samples take the scheme's next free colours, not a known sample's",
      identical(R$filled, 2L) && !any(hx(R$c2[c("New strain", "Other strain")]) %in% c("#000000", "#D55E00", "#56B4E9")) &&
        all(hx(R$c2[c("New strain", "Other strain")]) %in% R$t1$palette) && !anyDuplicated(hx(R$c2)))
check("the legend text names every sample's colour apart", { w <- regmatches(R$legend, gregexpr("\\(([a-z ]+),", R$legend))[[1]]; length(w) == 4 && !anyDuplicated(w) })
check("saving again adds the new samples (5 in all: EV / ev and N4 gp75 / N4 GP75 are one each)",
      length(R$t2$sample_aesthetics) == 5 && all(c("ev", "N4 gp60", "N4 GP75", "New strain", "Other strain") %in% names(R$t2$sample_aesthetics)))
check("a project file carries the scheme itself", identical(R$doc$scheme$key, R$key) && length(R$doc$scheme$file$sample_aesthetics) == 5 && length(R$doc$scheme$file$palette) >= 8)
check("opening the project where the scheme is missing brings it back", isTRUE(R$gone) && isTRUE(R$opened) && length(R$back$sample_aesthetics) == 5 && isTRUE(R$back$has_palette))
check("a scheme that is already here is left as it is", identical(R$kept_local, "Local"))
check("a file that is not a scheme is refused", all(R$imp_bad))
check("a loaded scheme file is rewritten clean, and does not replace a scheme of the same name",
      !identical(R$imp, R$key) && identical(names(R$imp_read$sample_aesthetics), "A") && is.null(R$imp_read$sample_aesthetics$A$evil) && identical(R$imp_read$palette, c("#111111", "#222222")))
check("a theme from before v2.29 styles the names it knows and nothing else", identical(R$legacy_k, 1L) && identical(names(R$legacy_cols), "ev") && identical(R$legacy_filled, 0L))
check("with no data the typed colours alone make a scheme; with neither, nothing is saved",
      identical(R$empty_key, "Colours_only") && identical(R$empty$palette, c("#010101", "#020202")) && identical(R$empty_err, "stopped") && is.null(env$theme_read("Nothing")))

cat("\n--- matched panels: the scheme's colours go to the curves that show them ---\n")
M <- new.env()
testServer(app, {
  S <- session$setInputs
  env$theme_write("Matched", list(), palette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), merge = FALSE)
  ingest_data(mk(c("EV", "N4 a", "N4 b", "EV + drug", "N4 a + drug", "N4 b + drug")), "m1.csv"); do.call(S, env$APP_DEFAULTS)
  S(selected_samples = rv$od_vars, active_theme = "Matched", qe_scheme = "Matched", theme_palette = "")
  S(group_EV = "plain", group_N4_a = "plain", group_N4_b = "plain", group_EV___drug = "drug", group_N4_a___drug = "drug", group_N4_b___drug = "drug",
    facet_by_group = TRUE, facet_source = "group"); session$flushReact()
  M$mode <- sel1("panel_style_match"); M$lead <- scheme_leaders(rv$od_vars)
  M$k <- theme_apply_named("Matched"); M$cols <- toupper(unlist(rv$qe_colors[rv$od_vars]))
  theme_save_current("Matched"); M$saved <- toupper(unname(env$theme_sample_colours(env$theme_read("Matched"))))
  M$free <- env$scheme_free_colours(env$theme_read("Matched"), 1)
})
check("matched panels are the default, and the second panel's samples follow the first panel's", identical(M$mode, "line") && identical(unname(M$lead[c("EV + drug", "N4 b + drug")]), c("EV", "N4 b")))
check("six samples in two matched panels use three of the scheme's colours: each follower has its match's colour",
      identical(unname(M$cols[c("EV", "N4 a", "N4 b")]), c("#000000", "#E69F00", "#56B4E9")) && identical(unname(M$cols[4:6]), unname(M$cols[1:3])))
check("the scheme keeps the colours the figures show, so the next new strain gets the scheme's fourth colour", length(unique(M$saved)) == 3L && identical(M$free, "#009E73"))

unlink(th_dir, recursive = TRUE)
cat("\n", pass, "passed,", fail, "failed\n")
if (fail > 0) quit(status = 1)
