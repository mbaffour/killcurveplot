#!/usr/bin/env Rscript
# Auto-installs any missing packages on first run, then opens the Shiny app in your browser.
# Launched by the "Run Lysis Curve Plotter" (.bat / .command) scripts, or run:  Rscript run_app.R
APP  <- "Lysis_Curve_App_v28.R"
PKGS <- c("shiny","tidyverse","ggpubr","ggprism","ggrepel","scales","svglite","jsonlite",
          "gifski","officer","rvg","base64enc","gridExtra","ragg","patchwork","DT","readxl")
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Use a writable personal library — R in "Program Files" usually isn't user-writable,
# so first-run installs would otherwise fail without admin.
ul <- Sys.getenv("R_LIBS_USER")
if (nzchar(ul)) {
  ul <- strsplit(ul, .Platform$path.sep, fixed = TRUE)[[1]][1]
} else {
  mm <- strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]
  ul <- file.path(path.expand("~"), "R", "library", paste0(R.version$major, ".", mm))
}
if (!dir.exists(ul)) dir.create(ul, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(ul, .libPaths()))

missing <- PKGS[!vapply(PKGS, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing R packages into ", ul, " (first run only): ", paste(missing, collapse = ", "))
  install.packages(missing, lib = ul)
}
app <- source(APP)$value

# Started by "Lysis Curve Plotter.app": a fixed port (KC_PORT), so clicking the app again reopens the same page, and
# KC_AUTO_STOP=1 stops the server about 15 s after the last browser tab closes (a page reload reconnects well within that).
port <- suppressWarnings(as.integer(Sys.getenv("KC_PORT", "")))
if (identical(Sys.getenv("KC_AUTO_STOP"), "1")) {
  server <- app$serverFuncSource()
  open_tabs <- 0L
  app$serverFuncSource <- function() function(input, output, session) {
    open_tabs <<- open_tabs + 1L
    session$onSessionEnded(function() {
      open_tabs <<- open_tabs - 1L
      later::later(function() if (open_tabs <= 0L) shiny::stopApp(), 15)
    })
    server(input, output, session)
  }
}
message("Opening Lysis Curve Plotter in your browser...")
if (!is.na(port)) shiny::runApp(app, port = port, host = "127.0.0.1", launch.browser = TRUE) else shiny::runApp(app, launch.browser = TRUE)
