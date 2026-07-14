#!/usr/bin/env Rscript
# Auto-installs any missing packages on first run, then opens the Shiny app in your browser.
# Launched by the "Run Lysis Curve Plotter" (.bat / .command) scripts, or run:  Rscript run_app.R
APP  <- "Lysis_Curve_App_v27.R"
PKGS <- c("shiny","tidyverse","ggpubr","ggprism","ggrepel","scales","svglite","jsonlite","gifski","officer","rvg")
options(repos = c(CRAN = "https://cloud.r-project.org"))
missing <- PKGS[!vapply(PKGS, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing R packages (first run only): ", paste(missing, collapse = ", "))
  install.packages(missing)
}
app <- source(APP)$value
message("Opening ", "Lysis Curve Plotter", " in your browser...")
shiny::runApp(app, launch.browser = TRUE)
