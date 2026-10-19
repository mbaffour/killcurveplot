# ============================================================================
# Render every figure the app can make, from the built-in demo data, with the
# journal preset (Nature single column, 89 x 60 mm, Arial 6-8 pt, SD error
# bars with replicate points) -- through the REAL app code, not a re-draw.
# Writes one SVG + PNG per figure and a two-column gallery (183 mm wide), for
# each of the five demo scenarios, into <out_dir>/<scenario>/. The "lysis"
# scenario's gallery.png is also copied to docs/gallery/gallery.png, which the
# README links to.
#   Rscript tools/render_gallery.R [out_dir]      (default: docs/gallery)
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite); library(patchwork)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root")
out <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(out) || !nzchar(out)) out <- "docs/gallery"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
env <- new.env(); app <- source("Lysis_Curve_App_v28.R", local = env)$value
quiet <- function(expr) suppressWarnings(suppressMessages(expr))

for (sc in c("lysis", "moi_series", "resistance", "hours", "two_phages")) {
  cat("\n=== scenario:", sc, "===\n")
  sc_out <- file.path(out, sc)
  dir.create(sc_out, recursive = TRUE, showWarnings = FALSE)
  testServer(app, {
    ingest_data(env$make_demo_data(sc), "Demo data (built-in)")
    do.call(session$setInputs, env$APP_DEFAULTS)
    session$setInputs(selected_samples = rv$od_vars, exclude_timepoints = "",
                      error_type = "sd", show_replicate_points = TRUE, error_bar_color = "black",
                      metrics_ref_sample = rv$od_vars[1],   # Uninfected / Control is first in every scenario
                      x_axis_label = if (sc == "hours") "Time (hours)" else "Time (minutes)",
                      nb_host_strain = "E. coli B", nb_phage_plasmid = "phage T4", nb_media = "LB",
                      nb_temperature = "37", nb_replicate = "3 biological replicates")
    do.call(session$setInputs, env$journal_preset_settings(89, 60, "a"))
    session$setInputs(x_axis_label = if (sc == "hours") "Time (hours)" else "Time (minutes)")
    session$setInputs(analysis_metrics = c("lysis_time", "od_drop", "mu_max", "infection_strength"),
                      analysis_dose_metric = "lysis_time")   # MOI parses from the sample names; no override box
    sz <- export_size_in()
    figs <- c(main = "Lysis curve (mean ± SD, replicates)", landmarks = "Curve landmarks",
              replicate_overlay = "Replicate overlay", growth_rate = "Specific growth rate",
              normalized = "Normalized OD", metric_summary = "Metric summary + stats",
              dose_response = "MOI dose-response", heatmap = "OD heatmap")
    plots <- list()
    for (nm in names(figs)) {
      p <- if (nm == "main") generate_plot() else generate_analysis_plot(nm)
      plots[[nm]] <- p
      quiet(ggsave(file.path(sc_out, paste0(nm, ".svg")), p, device = env$export_device("svg"),
                   width = sz[1], height = sz[2], units = "in", bg = "white"))
      quiet(ggsave(file.path(sc_out, paste0(nm, ".png")), p, device = env$export_device("png"),
                   width = sz[1], height = sz[2], units = "in", dpi = 300, bg = "white"))
      cat("rendered", nm, "\n")
    }
    # two-column gallery at a full Nature double column (183 mm): panels keep
    # their native 89 mm size, which is exactly how they would be assembled
    g <- wrap_plots(lapply(plots, function(p) p + labs(tag = NULL)), ncol = 2) +
      plot_annotation(tag_levels = "a") &
      theme(plot.tag = element_text(size = 8, face = "bold", family = "Arial"))
    quiet(ggsave(file.path(sc_out, "gallery.png"), g, device = env$export_device("png"),
                 width = 183, height = 245, units = "mm", dpi = 300, bg = "white"))
    quiet(ggsave(file.path(sc_out, "gallery.svg"), g, device = env$export_device("svg"),
                 width = 183, height = 245, units = "mm", bg = "white"))
    writeLines(c("Figure legend (drafted by the app):", output$figure_legend_text, "",
                 "Replicate statistics (first metric):", capture.output(print(analysis_stats_current()$pairwise))),
               file.path(sc_out, "legend_and_stats.txt"))
    cat("gallery written to", sc_out, "\n")
  })
  if (sc == "lysis") {   # keep the README link working
    dir.create("docs/gallery", recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(sc_out, "gallery.png"), "docs/gallery/gallery.png", overwrite = TRUE)
  }
}
