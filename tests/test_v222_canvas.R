# ============================================================================
# v2.22 click-to-edit labels on the figure: canvas_edit default, svg_inline_for_preview(), the inline-SVG preview
# (output od_plot_svg) and the page script / CSS for the popover.
# Run from the repository root:  Rscript tests/test_v222_canvas.R
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

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly", inherits(app, "shiny.appobj"))
check("APP_VERSION is 2.22.0 or later", package_version(env$APP_VERSION) >= "2.22.0")

cat("\n--- defaults ---\n")
d <- env$APP_DEFAULTS
check("canvas_edit is in APP_DEFAULTS, on", isTRUE(d$canvas_edit))
nm <- names(env$LEGACY_BASE)
check("it sits right after quick_edit_open (which follows sticky_figure after demo_scenario)",
      identical(nm[match("demo_scenario", nm) + 1:3], c("sticky_figure", "quick_edit_open", "canvas_edit")))
check("neither the journal preset nor the Legacy look touches it",
      !"canvas_edit" %in% names(env$legacy_preset_settings()) && !"canvas_edit" %in% names(env$journal_preset_settings(89, 65)))

cat("\n--- svg_inline_for_preview ---\n")
tf <- tempfile(fileext = ".svg")
writeLines(c("<?xml version='1.0' encoding='UTF-8' ?>",
             "<svg xmlns='http://www.w3.org/2000/svg' class='svglite' width='252.00pt' height='184.25pt' viewBox='0 0 252.00 184.25'>",
             "<rect width='100%' height='100%' style='stroke: none;'/>",
             "<text x='1' y='2'>Hello</text>", "</svg>"), tf)
out <- env$svg_inline_for_preview(tf, 673, 492)
root <- sub(">.*", ">", out)
check("returns one string starting with <svg", is.character(out) && length(out) == 1 && startsWith(out, "<svg"))
check("the <?xml ?> prolog is gone", !grepl("<?xml", out, fixed = TRUE))
check("the root has width='673px' height='492px'", grepl("width='673px'", root, fixed = TRUE) && grepl("height='492px'", root, fixed = TRUE) &&
        !grepl("pt'", root, fixed = TRUE))
check("the viewBox is kept", grepl("viewBox='0 0 252.00 184.25'", root, fixed = TRUE))
check("class keeps svglite and adds kc-canvas-svg", grepl("class='svglite kc-canvas-svg'", root, fixed = TRUE))
check("the nested <rect width='100%'> and the rest are untouched",
      grepl("<rect width='100%' height='100%' style='stroke: none;'/>", out, fixed = TRUE) && grepl("<text x='1' y='2'>Hello</text>", out, fixed = TRUE))
writeLines(c("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>", "",
             "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100.00pt\" height=\"50.00pt\" viewBox=\"0 0 100.00 50.00\">",
             "<rect width=\"100%\" height=\"100%\"/>", "</svg>"), tf)
out2 <- env$svg_inline_for_preview(tf, 200, 100); root2 <- sub(">.*", ">", out2)
check("double-quoted attributes are handled (no class yet: one is added)",
      startsWith(out2, "<svg") && grepl("width='200px'", root2, fixed = TRUE) && grepl("height='100px'", root2, fixed = TRUE) &&
        grepl("kc-canvas-svg", root2, fixed = TRUE) && grepl("<rect width=\"100%\" height=\"100%\"/>", out2, fixed = TRUE))
check("NULL when the file is larger than max_bytes", is.null(env$svg_inline_for_preview(tf, 200, 100, max_bytes = 10)))
check("NULL when the file is missing", is.null(env$svg_inline_for_preview(file.path(tempdir(), "nope.svg"), 200, 100)))
unlink(tf)
g <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) + geom_point() + labs(title = "T")
f2 <- tempfile(fileext = ".svg")
ggsave(f2, g, device = env$export_device("svg"), width = 3, height = 2, units = "in")
out3 <- env$svg_inline_for_preview(f2, 288, 192)
check("a real svglite file: starts with <svg, text elements survive, size set", startsWith(out3, "<svg") && grepl("<text", out3, fixed = TRUE) &&
        grepl("width='288px' height='192px'", out3, fixed = TRUE))
unlink(f2)

cat("\n--- UI: checkbox, script, CSS ---\n")
rt <- htmltools::renderTags(env$ui)
ui_html <- gsub("> <", "><", gsub("[ \t\r\n]+", " ", paste(rt$head, rt$html)))
check("the canvas_edit checkbox is in the quick-edit strip, checked",
      grepl('<input id="canvas_edit" type="checkbox" class="shiny-input-checkbox" checked="checked"/>', ui_html, fixed = TRUE) &&
      regexpr('id="canvas_edit"', ui_html, fixed = TRUE) > regexpr('id="kc_qe"', ui_html, fixed = TRUE) &&
      regexpr('id="canvas_edit"', ui_html, fixed = TRUE) < regexpr('id="x_axis_label"', ui_html, fixed = TRUE))
check("its label says what it does", grepl("Edit labels on the figure (click a legend entry, axis title or title)", ui_html, fixed = TRUE))
check("the script builds the popover kc_canvas_pop and reacts to od_plot_svg renders",
      grepl("kc_canvas_pop", ui_html, fixed = TRUE) && grepl("ev.name === 'od_plot_svg'", ui_html, fixed = TRUE) &&
      grepl("requestAnimationFrame(kcAfterRender)", ui_html, fixed = TRUE))
check("the script tags labels (kc-ed, data-kc) and writes into the existing inputs (qe_name_, qe_show_event)",
      grepl("classList.add('kc-ed')", ui_html, fixed = TRUE) && grepl("'data-kc'", ui_html, fixed = TRUE) &&
      grepl("'qe_name_' + kcCur.id", ui_html, fixed = TRUE) && grepl("qe_show_event", ui_html, fixed = TRUE) &&
      grepl("rotate\\(-90", ui_html, fixed = TRUE))
check("the CSS: clickable labels, hover colour, popover box",
      grepl(".kc-canvas-svg text.kc-ed { cursor:pointer; }", ui_html, fixed = TRUE) &&
      grepl(".kc-canvas-svg text.kc-ed:hover { fill:#0069d9", ui_html, fixed = TRUE) &&
      grepl("#kc_canvas_pop { position:absolute; z-index:50;", ui_html, fixed = TRUE))

cat("\n--- server: the inline-SVG preview ---\n")
testServer(app, {
  ingest_data(env$make_demo_data(), "Demo data (built-in)")
  do.call(session$setInputs, env$APP_DEFAULTS)
  smp <- rv$od_vars
  hi  <- smp[grepl("MOI 10$", smp)][1]
  session$setInputs(selected_samples = smp, error_type = "sem", exclude_timepoints = "")
  pc <- as.character(output$plot_container$html)
  check("the main preview container is the od_plot_svg uiOutput with class kc-canvas and the sized box",
        grepl("od_plot_svg", pc, fixed = TRUE) && grepl("kc-canvas", pc, fixed = TRUE) && grepl("width:[0-9]+px;height:[0-9]+px", pc) &&
        !grepl("od_plot_img", pc, fixed = TRUE))
  sv <- as.character(output$od_plot_svg$html)
  check("od_plot_svg: an inline <svg with <text elements, no xml prolog",
        startsWith(trimws(sv), "<svg") && grepl("<text", sv, fixed = TRUE) && !grepl("<?xml", sv, fixed = TRUE) &&
        grepl("kc-canvas-svg", sv, fixed = TRUE))
  check("od_plot_svg shows the sample name in the legend", grepl("Phage MOI 10", sv, fixed = TRUE))
  check("the svg size matches the container box",
        { m <- regmatches(pc, regexec("width:([0-9]+)px;height:([0-9]+)px", pc))[[1]]
          grepl(sprintf("width='%spx' height='%spx'", m[2], m[3]), sv, fixed = TRUE) })
  session$setInputs(qe_name_Phage_MOI_10 = "N4 wild type")
  sv2 <- as.character(output$od_plot_svg$html)
  check("after a rename the svg legend shows the new name and not the old", grepl("N4 wild type", sv2, fixed = TRUE) &&
        !grepl(">Phage MOI 10<", sv2, fixed = TRUE))
  session$setInputs(canvas_edit = FALSE)
  pc2 <- as.character(output$plot_container$html)
  check("canvas_edit off: the PNG image output as before", grepl("od_plot_img", pc2, fixed = TRUE) && !grepl("od_plot_svg", pc2, fixed = TRUE))
  check("canvas_edit off: the container keeps its width:<n>px box", grepl("width:[0-9]+px", pc2))
  session$setInputs(canvas_edit = TRUE, preview_mode = "canvas")
  pc3 <- as.character(output$plot_container$html)
  check("pixel-canvas mode: the od_plot plotOutput as before", grepl("od_plot", pc3, fixed = TRUE) && !grepl("od_plot_svg", pc3, fixed = TRUE) &&
        !grepl("od_plot_img", pc3, fixed = TRUE))
})

cat(sprintf("\n============ SUMMARY: %d passed / %d failed / %d skipped ============\n", pass, fail, skip))
if (fail > 0) quit(status = 1)
