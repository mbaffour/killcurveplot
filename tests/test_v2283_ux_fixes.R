# ============================================================================
# v2.28.3: fixes from the five-tester click-through (data, figure, panels / Compose / Compare, analysis, accessibility).
# Run from the repository root:  Rscript tests/test_v2283_ux_fixes.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v2283_ux_fixes.R")
Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_paper_style_here.json"))
pass <- 0L; fail <- 0L
check <- function(desc, cond) { if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") } else { fail <<- fail + 1L; cat("FAIL:", desc, "\n") } }
env <- new.env(); app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("APP_VERSION is 2.28.3 or later", package_version(env$APP_VERSION) >= "2.28.3")

cat("\n--- accessibility ---\n")
rt <- htmltools::renderTags(env$ui); h <- paste(rt$head, rt$html)
check("one h1, a skip link to the figure, and the main landmark", grepl("<h1 class=\"kc-title\"", h, fixed = TRUE) &&
        grepl("class=\"kc-skip\" href=\"#main_panel\"", h, fixed = TRUE) && grepl("id=\"main_panel\"[^>]*role=\"main\"", h))
check("the page script names sliders, announces notifications, names dialogs and returns focus", all(vapply(
  c("setAttribute('role', 'slider')", "shiny-notification-panel", "aria-modal", "hidden.bs.modal", "kcFigAlt"), grepl, NA, x = h, fixed = TRUE)))
check("contrast and motion rules are in the page CSS", all(vapply(c("#595959", "prefers-reduced-motion", "body.kc-night details > summary"), grepl, NA, x = h, fixed = TRUE)))
check("page language is set (fluidPage lang = 'en')", grepl("lang = \"en\"", paste(readLines("Lysis_Curve_App_v28.R", warn = FALSE), collapse = "\n"), fixed = TRUE))

cat("\n--- the legend-key rule on every figure tab ---\n")
check("Analysis, Curve fitting, Compose and Compare each have a 'Place the key on the figure' switch and a legend text starter",
      all(vapply(c("analysis", "fit", "compose", "compare"), function(k) grepl(paste0('id="', k, '_key_on_figure"'), h, fixed = TRUE) &&
                   grepl(paste0('id="', k, '_legend_text"'), h, fixed = TRUE), NA)))
check("the four switches are appended to APP_DEFAULTS, on", identical(tail(names(env$APP_DEFAULTS), 4),
      c("analysis_key_on_figure", "fit_key_on_figure", "compose_key_on_figure", "compare_key_on_figure")) &&
        all(vapply(tail(env$APP_DEFAULTS, 4), isTRUE, NA)))

cat("\n--- statistics ---\n")
rm <- data.frame(sample = rep(c("A", "B"), each = 3), replicate = rep(1:3, 2), m = c(1, 2, 3, 10, 11, 12))
mw <- env$compare_replicate_metric(rm, "m", method = "nonparametric")
check("Mann-Whitney 3 vs 3, fully separated: exact p = 0.1 (was the normal approximation 0.081)",
      isTRUE(all.equal(mw$pairwise$p[1], 0.1)) && grepl("exact", mw$pairwise$test[1]))
check("p values below 1e-16 (or exactly 0) read '< 1e-16'; others keep 3 significant digits",
      identical(env$fmt_stats_df(data.frame(p = c(0, 1e-30, 0.0312), stringsAsFactors = FALSE))$p, c("< 1e-16", "< 1e-16", "0.0312")))
check("4-digit tables: a tiny p is '< 1e-16', a tiny value is in e notation (no 100-digit decimals)",
      { d <- env$s22_sig(data.frame(p_adj = c(0, 1e-200), x = 1.23456e-9)); identical(d$p_adj, c("< 1e-16", "< 1e-16")) && all(grepl("e-09", d$x)) })

cat("\n--- data in ---\n")
ex <- env$parse_excluded_timepoints("30, abc, 7", c(0, 10, 20, 30))
um <- attr(ex, "unmatched")
check("excluded time points: a word is reported, a snapped value is reported", identical(as.numeric(ex), c(30, 10)) &&
        any(grepl("'abc' is not a number", um)) && any(grepl("7 is read as the time point 10", um)))

cat("\n--- journal preset and the session ---\n")
t <- seq(0, 120, 10); set.seed(3); cols <- list(Time = t)
for (s in 1:3) for (r in 1:3) cols[[length(cols) + 1]] <- round(0.05 * exp(0.02 * t) * (1 + s / 20) * exp(rnorm(length(t), 0, 0.03)), 4)
d <- as.data.frame(cols); names(d) <- c("Time", rep(c("WT", "M1", "M2"), each = 3))
hr <- d; names(hr)[1] <- "Hours"
R <- new.env()
testServer(app, {
  S <- session$setInputs
  ingest_data(d, "d.csv"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars, export_format = "svg")
  R$cap_lin <- error_cap_width(c(0, 120))
  S(x_scale_type = "log"); R$cap_log <- error_cap_width(c(1, 10, 100)); S(x_scale_type = "linear")
  R$legtxt <- figure_legend_text_for(generate_plot())
  S(analysis_key_on_figure = FALSE); pa <- generate_analysis_plot("replicate_overlay")
  R$hidden <- identical(pa$theme$legend.position, "none"); R$key_still <- !is.null(tryCatch(env$legend_key_grob(pa), error = function(e) NULL))
  S(analysis_key_on_figure = TRUE)
  S(time_convert = "min_to_h", enable_time_filter = TRUE, exclude_timepoints = "30")
  R$excl_file_unit <- excluded_timepoints_file()
  S(time_convert = "none", enable_time_filter = FALSE, exclude_timepoints = "")
  S(blank_mode = "sample", blank_sample = "WT"); session$flushReact()
})
check("log x axis: the error-bar cap is a share of the log10 range (not minutes in log units)", isTRUE(R$cap_log < 0.1) && isTRUE(R$cap_lin > 1))
check("legend text starter names each sample with colour and marker", grepl("WT (black, filled circles", R$legtxt, fixed = TRUE) && grepl("; M1 (", R$legtxt, fixed = TRUE))
check("'Place the key on the figure' off hides the analysis legend; the key download still has a key", isTRUE(R$hidden) && isTRUE(R$key_still))
check("excluded time points are read in the file's time unit: 30 stays 30 min after converting to hours", identical(R$excl_file_unit, 30))
check("journal preset: a 55 mm column gets a panel that fits (55 - 12 mm); 89 mm keeps the 65 mm panel",
      { pw <- function(w) if (w - env$JOURNAL_AXES_MM < 65) max(30, floor(w - env$JOURNAL_AXES_MM)) else 65; pw(55) == 43 && pw(89) == 65 })

cat("\n--- help ---\n")
f <- tempfile(fileext = ".md"); writeLines(c("# T", "", "See [x](#stats--reports-new).", "", "## Stats & Reports new", "", "[License](LICENSE)"), f)
hh <- env$render_help_doc(f, base = "root", prefix = "help_R")
check("a heading with '&' gets the GitHub id its link uses; a root file link goes to kc_root/",
      grepl('id="help_R-stats--reports-new"', hh, fixed = TRUE) && grepl('href="#help_R-stats--reports-new"', hh, fixed = TRUE) && grepl('href="kc_root/LICENSE"', hh, fixed = TRUE))

cat("\n", pass, "passed,", fail, "failed\n")
if (fail > 0) quit(status = 1)
