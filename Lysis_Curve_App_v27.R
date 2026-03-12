# Lysis Curve OD Visualization App  v2.7
# Michael Baffour Awuah / Ramsey Lab
#
# v2.7 -- Bug fixes:
#   SHADOW FIX         -- Shadow/ribbon error display no longer crashes when
#                         points are also enabled. The fill scale conflict
#                         between geom_ribbon (scale_fill_manual) and
#                         geom_point (scale_fill_identity) is resolved by
#                         using a unified scale_fill_manual when shadow mode
#                         is active.
#   NA ERROR FIX       -- Single-replicate samples (where sd() returns NA)
#                         no longer crash error bars or ribbons. NA sd values
#                         are replaced with 0 before computing error bounds.
#   OPERATOR ORDER FIX -- The %||% (null-coalescing) operator is now defined
#                         before its first use in renderUI.
#   ERROR GUARD        -- Error column values are explicitly guarded against
#                         NA before computing err_lo / err_hi bounds.
#
# v2.6 -- Stability & performance improvements for large replicate counts:
#   OBSERVER LEAK FIX  -- Color observeEvent handlers are registered at most
#                         once per sample ID (tracked in registered_color_obs).
#   POINT LOOP -> O(1) -- build_plot() no longer adds one geom_point() layer
#                         per sample; shape/fill are vectorised into the data
#                         frame and a single geom_point() call covers all.
#   PIVOT GUARD        -- prepare_plot_data() pre-selects only the requested
#                         sample columns before pivot_longer().
#   VECTORISED FILTER  -- apply_time_filters() uses outer() matrix comparison
#                         instead of per-row vapply for time-point exclusions.
#   DEBOUNCE           -- Main plot render waits 400 ms after the last input
#                         change before rebuilding, preventing render pile-up.
#   CRASH GUARD        -- build_plot() wrapped in tryCatch(); errors display
#                         as an in-plot message rather than crashing the session.
#   PAGINATION         -- Variable Styling panel shows at most 20 samples at a
#                         time with Prev/Next navigation; keeps the DOM small.on App  v2.5
# Michael Baffour Awuah / Ramsey Lab
#
# v2.5 changes:
#   SETTINGS  – Save/load now stores BOTH per-sample aesthetics (color, shape,
#               linetype keyed by sample name) AND all global visual settings.
#               On import: matched samples get their saved aesthetics applied
#               automatically; a status panel shows exactly which names matched
#               and which didn't.  "Clear Imported" fully revokes everything and
#               restores defaults — per-sample overrides included.
#
#   GIF       – Rebuilt without gganimate. Each frame is a real ggplot rendered
#               to PNG via ragg/png, then stitched with gifski. Full fidelity:
#               same theme, axes, error bars, shapes, colors as the main plot.
#               Each frame adds one more line (cumulative reveal).
#
#   PPTX      – Each slide is a pixel-perfect render of build_plot() with the
#               cumulative subset + grey-out of non-active samples. Final slide
#               shows all samples in full color. Slide dimensions driven by the
#               export width/height inputs.

# ── Packages ──────────────────────────────────────────────────────────────────
library(shiny)
library(tidyverse)
library(ggpubr)
library(scales)
library(ggrepel)
library(ggprism)
library(svglite)
library(jsonlite)

has_officer   <- requireNamespace("officer",   quietly = TRUE)
has_rvg       <- requireNamespace("rvg",       quietly = TRUE)
has_gifski    <- requireNamespace("gifski",    quietly = TRUE)

# ── Helpers ───────────────────────────────────────────────────────────────────

safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

normalize_hex_color <- function(hex) {
  if (is.null(hex) || is.na(hex) || nchar(trimws(hex)) == 0) return("#000000")
  hex <- trimws(hex)
  if (!grepl("^#", hex)) hex <- paste0("#", hex)
  if (grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", hex)) return(toupper(hex))
  "#000000"
}

rgb_to_hex <- function(r, g, b, a = 1) {
  rgb(min(max(as.integer(r), 0), 255) / 255,
      min(max(as.integer(g), 0), 255) / 255,
      min(max(as.integer(b), 0), 255) / 255,
      min(max(a, 0), 1))
}

parse_excluded_timepoints <- function(text, all_timepoints) {
  if (is.null(text) || nchar(trimws(text)) == 0) return(numeric(0))
  parts <- strsplit(trimws(text), "[,; ]+")[[1]]
  vals  <- suppressWarnings(as.numeric(parts))
  vals  <- vals[!is.na(vals)]
  if (length(vals) == 0 || is.null(all_timepoints)) return(numeric(0))
  matched <- sapply(vals, function(v) {
    diffs <- abs(all_timepoints - v)
    tol   <- max(abs(all_timepoints)) * 0.001 + 0.001
    if (min(diffs) <= tol) all_timepoints[which.min(diffs)] else NA_real_
  })
  unique(matched[!is.na(matched)])
}

# ── Optional UI widgets — must be pre-computed before fluidPage() ─────────────

pptx_ui <- if (has_officer && has_rvg) {
  downloadButton("downloadPPTX", "Download PowerPoint (.pptx)",
                 style = "background:#C0392B;color:white;border:none;width:100%;")
} else {
  p(style = "color:#999;font-size:.85em;",
    "Install 'officer' + 'rvg' to enable PPTX export.")
}

gif_ui <- if (has_gifski) {
  downloadButton("downloadGIF", "Download Animated GIF",
                 style = "background:#27AE60;color:white;border:none;width:100%;")
} else {
  p(style = "color:#999;font-size:.85em;",
    "Install 'gifski' to enable GIF export.")
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("Optical Density (OD) Time-Series Visualization"),
  
  tags$head(
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('updateColorPreview', function(msg) {
         var el = document.getElementById(msg.id);
         if (el) el.style.backgroundColor = msg.color;
       });"
    )),
    tags$style(HTML("
      .panel-section {
        background:#f8f9fa; padding:12px; border-radius:5px;
        margin:10px 0; border:1px solid #e9ecef;
      }
      .panel-title { margin-top:0; margin-bottom:10px; font-weight:bold; color:#495057; }
      details > summary {
        cursor:pointer; font-weight:bold; padding:6px 10px;
        background:#e9ecef; border-radius:3px; list-style:none; user-select:none;
      }
      details > summary::-webkit-details-marker { display:none; }
      details[open] > summary::before { content:'\\25BC  '; font-size:.75em; }
      details > summary::before       { content:'\\25B6  '; font-size:.75em; }
      details { margin-bottom:6px; }
      .color-preview {
        display:inline-block; width:28px; height:28px;
        border:1px solid #ccc; border-radius:4px; flex-shrink:0;
      }
      .settings-status { padding:8px 12px; border-radius:4px; font-size:.85em; margin-top:6px; }
      .settings-status.ok   { background:#d4edda; color:#155724; border:1px solid #c3e6cb; }
      .settings-status.warn { background:#fff3cd; color:#856404; border:1px solid #ffeeba; }
      .match-list { margin:4px 0 0 0; padding-left:16px; }
      .excl-preview {
        background:#fff3cd; color:#856404; padding:6px 10px;
        border-radius:4px; font-size:.82em; margin-top:4px;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 
                 fileInput("file", "Choose CSV File", accept = c("text/csv", ".csv")),
                 
                 # ── Settings ─────────────────────────────────────────────────────────────
                 div(class = "panel-section",
                     h4("Save / Load Settings", class = "panel-title"),
                     p(style = "font-size:.82em;color:#555;margin-bottom:8px;",
                       "Saves all visual settings: axis scales, labels, error bars, fonts, line/point options, ",
                       "AND per-sample aesthetics (color, shape, linetype) keyed by sample name. ",
                       "Loading restores all those settings — but never touches your data, ",
                       "sample selection, or time filter."),
                     fluidRow(
                       column(6, downloadButton("saveSettings",  "Save Settings",  style = "width:100%")),
                       column(6, actionButton("clearSettings", "Clear & Reset",
                                              icon  = icon("times"),
                                              style = "width:100%;background:#dc3545;color:white;border:none;"))
                     ),
                     br(),
                     fileInput("loadSettings", "Load Settings File",
                               accept = c("application/json", ".json")),
                     uiOutput("settings_status_ui")
                 ),
                 
                 # ── Axis Settings ─────────────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Axis Settings"),
                   div(class = "panel-section",
                       h4("Axis Scales", class = "panel-title"),
                       selectInput("x_scale_type", "X-axis Scale:",
                                   choices  = c("Linear" = "linear", "Logarithmic" = "log",
                                                "Square Root" = "sqrt", "Reverse" = "reverse"),
                                   selected = "linear"),
                       selectInput("y_scale_type", "Y-axis Scale:",
                                   choices  = c("Linear" = "linear", "Logarithmic" = "log",
                                                "Square Root" = "sqrt", "Reverse" = "reverse"),
                                   selected = "log"),
                       checkboxInput("use_advanced_ticks", "Advanced Tick Customization", value = TRUE),
                       conditionalPanel(
                         condition = "input.use_advanced_ticks == true && input.y_scale_type == 'log'",
                         h5("Y-axis Log Tick Range"),
                         fluidRow(
                           column(6, numericInput("y_log_min_exponent", "Min Exponent:", value = -2)),
                           column(6, numericInput("y_log_max_exponent", "Max Exponent:", value =  0))
                         )
                       ),
                       conditionalPanel(
                         condition = "input.use_advanced_ticks == true && input.x_scale_type == 'linear'",
                         h5("X-axis Tick Control", style = "margin:10px 0 4px;"),
                         fluidRow(
                           column(6,
                                  numericInput("x_tick_interval", "Major Interval:",
                                               value = NA, min = 0.001, step = 1)
                           ),
                           column(6,
                                  p(style = "font-size:.8em;color:#777;margin-top:28px;",
                                    "Leave blank = auto")
                           )
                         ),
                         textInput("x_extra_ticks", "Extra tick positions:",
                                   value = "", placeholder = "e.g. 270, 450"),
                         p(style = "font-size:.8em;color:#777;margin-top:-6px;",
                           "Values always shown regardless of interval. Comma-separated.")
                       ),
                       checkboxInput("custom_x_limits", "Custom X-axis limits", value = FALSE),
                       conditionalPanel(condition = "input.custom_x_limits == true",
                                        fluidRow(
                                          column(6, numericInput("x_min", "X-min:", value = 0)),
                                          column(6, numericInput("x_max", "X-max:", value = 100))
                                        )
                       ),
                       checkboxInput("custom_y_limits", "Custom Y-axis limits", value = FALSE),
                       conditionalPanel(condition = "input.custom_y_limits == true",
                                        fluidRow(
                                          column(6, numericInput("y_min", "Y-min:", value = 0.01)),
                                          column(6, numericInput("y_max", "Y-max:", value = 2))
                                        )
                       ),
                       sliderInput("x_expand_left",   "X Left Expand:",   0, 0.1, 0,    0.01),
                       sliderInput("x_expand_right",  "X Right Expand:",  0, 0.1, 0.05, 0.01),
                       sliderInput("y_expand_bottom", "Y Bottom Expand:", 0, 0.1, 0,    0.01),
                       sliderInput("y_expand_top",    "Y Top Expand:",    0, 0.1, 0.05, 0.01)
                   ),
                   div(class = "panel-section",
                       h4("Labels & Formatting", class = "panel-title"),
                       textInput("x_axis_label",  "X-axis Label:",  "Time (minutes)"),
                       textInput("y_axis_label",  "Y-axis Label:",  "A550"),
                       textInput("plot_title",    "Plot Title:",    ""),
                       textInput("plot_subtitle", "Plot Subtitle:", ""),
                       h5("Gridlines", style = "margin-top:12px;"),
                       checkboxInput("show_major_gridlines", "Show Major Gridlines", FALSE),
                       checkboxInput("show_minor_gridlines", "Show Minor Gridlines", FALSE),
                       conditionalPanel(condition = "input.show_major_gridlines == true",
                                        selectInput("major_gridline_color", "Major Gridline Color:",
                                                    choices  = c("Light Gray" = "#E6E6E6", "Medium Gray" = "#CCCCCC",
                                                                 "Dark Gray"  = "#999999", "Light Blue"  = "#DEEBF7",
                                                                 "Custom"     = "custom"),
                                                    selected = "#E6E6E6"),
                                        conditionalPanel(condition = "input.major_gridline_color == 'custom'",
                                                         textInput("major_gridline_color_custom", "HEX:", "#E6E6E6")),
                                        sliderInput("major_gridline_size", "Width:", 0.1, 1, 0.5, 0.1)
                       ),
                       conditionalPanel(condition = "input.show_minor_gridlines == true",
                                        selectInput("minor_gridline_color", "Minor Gridline Color:",
                                                    choices  = c("Light Gray" = "#F2F2F2", "Medium Gray" = "#E6E6E6",
                                                                 "Dark Gray"  = "#CCCCCC", "Light Blue"  = "#EFF6FB",
                                                                 "Custom"     = "custom"),
                                                    selected = "#F2F2F2"),
                                        conditionalPanel(condition = "input.minor_gridline_color == 'custom'",
                                                         textInput("minor_gridline_color_custom", "HEX:", "#F2F2F2")),
                                        sliderInput("minor_gridline_size", "Width:", 0.1, 1, 0.3, 0.1)
                       ),
                       h5("Font Settings", style = "margin-top:12px;"),
                       selectInput("font_family", "Font Family:",
                                   choices  = c("Sans Serif (Arial/Helvetica)" = "sans",
                                                "Serif (Times New Roman)"      = "serif",
                                                "Monospace (Courier New)"      = "mono",
                                                "Helvetica Neue"               = "Helvetica Neue",
                                                "Garamond"                     = "Garamond",
                                                "Palatino"                     = "Palatino"),
                                   selected = "sans"),
                       numericInput("title_font_size",      "Title Font Size:",  20, 8, 48),
                       numericInput("axis_label_font_size", "Axis Label Size:",  20, 8, 36),
                       numericInput("axis_text_font_size",  "Axis Text Size:",   16, 6, 32),
                       checkboxInput("bold_title",         "Bold Title",         TRUE),
                       checkboxInput("italic_axis_labels", "Italic Axis Labels", FALSE),
                       selectInput("axis_text_angle", "X-axis Text Angle:",
                                   choices  = c("Horizontal (0\u00b0)" = "0",
                                                "Angled (45\u00b0)"    = "45",
                                                "Vertical (90\u00b0)"  = "90"),
                                   selected = "0")
                   )
                 ),
                 
                 # ── Time Point Filtering ──────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Time Point Filtering"),
                   div(class = "panel-section",
                       h4("Restrict Displayed Time Range", class = "panel-title"),
                       p(style = "font-size:.85em;color:#555;",
                         "Filters data before plotting. All statistics, error bars, and exports reflect the filtered data."),
                       checkboxInput("enable_time_filter", "Enable Time Filtering", value = FALSE),
                       conditionalPanel(condition = "input.enable_time_filter == true",
                                        h5("Time Range", style = "margin:8px 0 4px;"),
                                        uiOutput("time_range_slider_ui"),
                                        hr(style = "margin:10px 0;"),
                                        h5("Exclude Specific Time Points", style = "margin:8px 0 4px;"),
                                        p(style = "font-size:.82em;color:#666;", "Comma-separated values to remove (e.g. 0, 5, 180)."),
                                        textInput("exclude_timepoints", label = NULL, value = "", placeholder = "e.g. 0, 5, 180"),
                                        uiOutput("excluded_timepoints_preview")
                       )
                   )
                 ),
                 
                 # ── Region Highlighting ───────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Region Highlighting"),
                   div(class = "panel-section",
                       checkboxInput("enable_highlighting", "Enable Region Highlighting", FALSE),
                       conditionalPanel(condition = "input.enable_highlighting == true",
                                        numericInput("region_count", "Number of Regions:", 1, 1, 5),
                                        uiOutput("region_settings")
                       )
                   )
                 ),
                 
                 # ── Time Point Markers ────────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Time Point Markers"),
                   div(class = "panel-section",
                       checkboxInput("enable_time_markers", "Enable Time Markers", FALSE),
                       conditionalPanel(condition = "input.enable_time_markers == true",
                                        numericInput("marker_count", "Number of Markers:", 1, 1, 10),
                                        uiOutput("time_marker_settings")
                       )
                   )
                 ),
                 
                 # ── Color Palettes ────────────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Color Palettes"),
                   div(class = "panel-section",
                       selectInput("color_palette", "Color Palette:",
                                   choices  = c("Custom"              = "custom",
                                                "Viridis"             = "viridis",
                                                "Plasma"              = "plasma",
                                                "Colorblind-friendly" = "colorblind",
                                                "Publication"         = "publication",
                                                "Rainbow"             = "rainbow",
                                                "Grayscale"           = "gray"),
                                   selected = "custom"),
                       htmlOutput("palette_preview")
                   )
                 ),
                 
                 # ── Line & Point Settings ─────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Line & Point Settings"),
                   div(class = "panel-section",
                       h4("Line Options", class = "panel-title"),
                       sliderInput("line_thickness", "Line Thickness:", 0.1, 3, 1, 0.1)
                   ),
                   div(class = "panel-section",
                       h4("Point Options", class = "panel-title"),
                       checkboxInput("show_points", "Show Points", TRUE),
                       conditionalPanel(condition = "input.show_points == true",
                                        sliderInput("shape_size",   "Point Size:",   0.5, 8, 3,   0.1),
                                        sliderInput("point_stroke", "Stroke Width:", 0,   2, 0.5, 0.1)
                       )
                   )
                 ),
                 
                 # ── Label Options ─────────────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Label Options"),
                   div(class = "panel-section",
                       checkboxInput("show_end_labels", "Show End-of-Line Labels", FALSE),
                       conditionalPanel(condition = "input.show_end_labels == true",
                                        numericInput("label_font_size", "Label Font Size (pt):", 12, 3, 36),
                                        checkboxInput("label_bold",  "Bold Labels",              TRUE),
                                        numericInput("label_offset", "Label Offset (% x-axis):", 3.5, 0, 20)
                       )
                   )
                 ),
                 
                 # ── Error Bars / Shadow ───────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Error Bars / Shadow"),
                   div(class = "panel-section",
                       selectInput("error_type", "Error Type:",
                                   choices  = c("None" = "none", "SD" = "sd",
                                                "SEM"  = "sem",  "95% CI" = "ci95"),
                                   selected = "sem"),
                       conditionalPanel(condition = "input.error_type != 'none'",
                                        selectInput("error_display_mode", "Display Mode:",
                                                    choices  = c("Error Bars" = "bars", "Shadow/Ribbon" = "shadow"),
                                                    selected = "bars"),
                                        numericInput("error_multiplier", "Error Multiplier:", 1, 0.1, 5, 0.1),
                                        checkboxInput("asymmetric_error", "Floor errors at zero", FALSE),
                                        conditionalPanel(condition = "input.error_display_mode == 'bars'",
                                                         selectInput("error_bar_style", "Bar Style:",
                                                                     choices  = c("T-shaped"     = "T",
                                                                                  "Solid lines"  = "solid",
                                                                                  "Dashed lines" = "dashed"),
                                                                     selected = "T"),
                                                         sliderInput("error_bar_width",     "Bar Width:",     0.01, 2, 1,   0.01),
                                                         sliderInput("error_bar_thickness", "Bar Thickness:", 0.1,  2, 0.8, 0.1),
                                                         selectInput("error_bar_position", "Position:",
                                                                     choices  = c("Middle" = "middle", "Dodge" = "dodge"),
                                                                     selected = "middle")
                                        ),
                                        conditionalPanel(condition = "input.error_display_mode == 'shadow'",
                                                         sliderInput("shadow_alpha", "Ribbon Alpha:", 0.05, 0.6, 0.2, 0.05)
                                        )
                       )
                   )
                 ),
                 
                 # ── Plot Dimensions & Export ──────────────────────────────────────────────
                 tags$details(
                   tags$summary("Plot Dimensions & Export"),
                   div(class = "panel-section",
                       h4("Canvas Size", class = "panel-title"),
                       fluidRow(
                         column(6, numericInput("plot_width",  "Width (px):",  820, 400, 2000)),
                         column(6, numericInput("plot_height", "Height (px):", 600, 300, 1500))
                       ),
                       checkboxInput("custom_aspect_ratio", "Custom Aspect Ratio", FALSE),
                       conditionalPanel(condition = "input.custom_aspect_ratio == true",
                                        numericInput("aspect_ratio", "Width/Height:", 1, 0.3, 4, 0.1)
                       ),
                       h4("Image Export", class = "panel-title"),
                       selectInput("export_format", "Format:",
                                   choices  = c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg",
                                                "SVG" = "svg", "TIFF" = "tiff"),
                                   selected = "pdf"),
                       fluidRow(
                         column(6, numericInput("export_width",  "Width (in):",  10, 2, 30)),
                         column(6, numericInput("export_height", "Height (in):", 8,  2, 20))
                       ),
                       numericInput("export_dpi", "DPI:", 300, 72, 1200),
                       downloadButton("downloadPlot", "Download Image"),
                       hr(),
                       h4("PowerPoint Export", class = "panel-title"),
                       p(style = "font-size:.85em;color:#666;",
                         "Cumulative build: slide 1 shows sample 1, slide 2 adds sample 2, etc. Each slide title shows the newly-added sample name. Final slide = all samples, identical to the main plot."),
                       pptx_ui,
                       hr(),
                       h4("Animated GIF Export", class = "panel-title"),
                       p(style = "font-size:.85em;color:#666;",
                         "Cumulative build: each frame adds one more line. No grey masks — lines already drawn stay fully visible. Axes, theme, and error bars match the main plot exactly."),
                       numericInput("gif_fps",    "Frames per second:", 1,   0.2, 10,   0.2),
                       numericInput("gif_width",  "GIF Width (px):",    800, 300, 2000, 50),
                       numericInput("gif_height", "GIF Height (px):",   600, 200, 1500, 50),
                       gif_ui
                   )
                 ),
                 
                 # ── Variable Styling ──────────────────────────────────────────────────────
                 tags$details(
                   tags$summary("Variable Styling"),
                   div(class = "panel-section",
                       h4("Sample Selection", class = "panel-title"),
                       selectInput("selected_samples", "Select Samples:",
                                   choices = character(0), multiple = TRUE),
                       uiOutput("var_settings")
                   )
                 )
                 
    ), # end sidebarPanel
    
    mainPanel(width = 9,
              uiOutput("plot_container")
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data              = NULL,
    time_col          = NULL,
    od_vars           = NULL,
    od_vars_raw       = NULL,
    group_col         = NULL,
    value_col         = NULL,
    is_long_format    = FALSE,
    palette_colors    = NULL,
    all_timepoints    = NULL,
    # imported_settings is a list with two named sublists:
    #   $global  — named list of scalar values for global inputs
    #   $samples — named list keyed by SAMPLE NAME, each a list of per-sample aesthetics
    imported_settings = NULL,
    settings_status   = NULL,
    style_page        = 1L      # current page in Variable Styling panel (pagination)
  )
  
  # ── Palette helpers ──────────────────────────────────────────────────────────
  get_palette_colors <- function(pal, n) {
    switch(pal,
           viridis     = scales::viridis_pal(option = "D")(n),
           plasma      = scales::viridis_pal(option = "C")(n),
           colorblind  = rep_len(c("#000000","#E69F00","#56B4E9","#009E73",
                                   "#F0E442","#0072B2","#D55E00","#CC79A7"), n),
           publication = rep_len(c("#E41A1C","#377EB8","#4DAF4A","#984EA3",
                                   "#FF7F00","#FFFF33","#A65628","#F781BF"), n),
           rainbow     = rainbow(n),
           gray        = gray.colors(n, start = 0.15, end = 0.85),
           NULL
    )
  }
  
  observeEvent(input$color_palette, {
    rv$palette_colors <- if (input$color_palette != "custom" && !is.null(rv$od_vars))
      get_palette_colors(input$color_palette, length(rv$od_vars)) else NULL
  })
  
  output$palette_preview <- renderUI({
    if (input$color_palette == "custom" || is.null(rv$od_vars)) return(NULL)
    cols <- get_palette_colors(input$color_palette, min(8, length(rv$od_vars)))
    div(p("Preview:"),
        div(style = "margin-top:6px;",
            lapply(cols, function(col)
              div(style = paste0("display:inline-block;width:28px;height:20px;",
                                 "background:", col, ";margin-right:4px;",
                                 "border:1px solid #ccc;")))))
  })
  
  # ── Time filter UI ───────────────────────────────────────────────────────────
  output$time_range_slider_ui <- renderUI({
    tp <- rv$all_timepoints
    if (is.null(tp) || length(tp) < 2)
      return(p(style = "color:#999;font-size:.85em;", "Load a data file to enable the range slider."))
    t_min  <- min(tp); t_max <- max(tp)
    t_step <- max(min(diff(tp)), 0.001)
    sliderInput("time_filter_range", label = NULL,
                min = t_min, max = t_max, value = c(t_min, t_max),
                step = t_step, width = "100%")
  })
  
  output$excluded_timepoints_preview <- renderUI({
    if (is.null(rv$all_timepoints)) return(NULL)
    excl <- parse_excluded_timepoints(input$exclude_timepoints, rv$all_timepoints)
    if (length(excl) == 0) return(NULL)
    div(class = "excl-preview",
        paste0("Will exclude ", length(excl), " point(s): ", paste(sort(excl), collapse = ", ")))
  })
  
  # ── Data loading ─────────────────────────────────────────────────────────────
  is_long_format_detect <- function(data) {
    if (nrow(data) == 0) return(FALSE)
    potential <- c("variable","Variable","condition","Condition","treatment","Treatment",
                   "sample","Sample","group","Group")
    if (any(potential %in% colnames(data))) return(TRUE)
    if (ncol(data) <= 4) {
      vd <- sapply(data, function(x) length(unique(x)) / nrow(data))
      return(any(vd < 0.3 & vd > 0))
    }
    FALSE
  }
  
  detect_time_column <- function(data) {
    hits <- grep("^(time|Time|TIME|t|T)$", colnames(data), value = TRUE)
    if (length(hits)) return(hits[1])
    hits2 <- grep("time|Time", colnames(data), value = TRUE)
    if (length(hits2)) return(hits2[1])
    for (nm in colnames(data)) {
      v <- suppressWarnings(as.numeric(data[[nm]]))
      if (!any(is.na(v)) && length(v) > 1 && all(diff(v) >= 0)) return(nm)
    }
    colnames(data)[1]
  }
  
  detect_od_columns <- function(data, exclude_cols) {
    cols <- setdiff(colnames(data), exclude_cols)
    hits <- grep("od|OD|absorbance|Absorbance|value|Value", cols,
                 ignore.case = TRUE, value = TRUE)
    if (length(hits)) return(hits)
    cols[sapply(data[cols], is.numeric)]
  }
  
  observeEvent(input$file, {
    req(input$file)
    data <- tryCatch(
      read.csv(input$file$datapath, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e)
        read.csv(input$file$datapath, stringsAsFactors = FALSE, check.names = TRUE)
    )
    rv$is_long_format <- is_long_format_detect(data)
    rv$time_col       <- detect_time_column(data)
    
    if (rv$is_long_format) {
      potential <- c("variable","Variable","condition","Condition","treatment","Treatment",
                     "sample","Sample","group","Group")
      found     <- potential[potential %in% colnames(data)]
      group_col <- if (length(found) > 0) found[1] else {
        dv    <- sapply(data, function(x) length(unique(x)) / nrow(data))
        cands <- setdiff(names(which(dv < 0.3 & dv > 0)), rv$time_col)
        if (length(cands)) cands[1] else setdiff(colnames(data), rv$time_col)[1]
      }
      vc <- detect_od_columns(data, c(rv$time_col, group_col))
      if (length(vc) > 1) vc <- vc[1]
      rv$group_col   <- group_col
      rv$value_col   <- vc
      rv$od_vars     <- as.character(unique(data[[group_col]]))
      rv$od_vars_raw <- rv$od_vars
    } else {
      oc <- detect_od_columns(data, rv$time_col)
      rv$od_vars     <- oc
      rv$od_vars_raw <- oc
    }
    rv$data <- data
    
    tr <- range(suppressWarnings(as.numeric(as.character(data[[rv$time_col]]))), na.rm = TRUE)
    updateNumericInput(session, "x_min", value = tr[1])
    updateNumericInput(session, "x_max", value = tr[2])
    updateSelectInput(session, "selected_samples", choices = rv$od_vars, selected = rv$od_vars)
    
    all_t <- sort(unique(suppressWarnings(as.numeric(as.character(data[[rv$time_col]])))))
    rv$all_timepoints <- all_t[is.finite(all_t)]
    updateTextInput(session, "exclude_timepoints", value = "")
    
    if (!is.null(input$color_palette) && input$color_palette != "custom")
      rv$palette_colors <- get_palette_colors(input$color_palette, length(rv$od_vars))
    
    # After loading data, auto-apply any imported per-sample aesthetics for matching names
    apply_imported_sample_aesthetics()
  })
  
  # ── Settings: Save ───────────────────────────────────────────────────────────
  # Saves global visual settings AND per-sample aesthetics keyed by sample name.
  # Explicitly excluded: file path, loaded CSV, sample selection, time filter range —
  # anything that belongs to the data rather than the visualization.
  output$saveSettings <- downloadHandler(
    filename = function()
      paste0("plot_settings_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"),
    content = function(file) {
      all_inputs <- reactiveValuesToList(input)
      
      # Inputs that belong to the data/session — never saved
      data_inputs     <- c("file", "loadSettings", "selected_samples",
                           "time_filter_range", "exclude_timepoints")
      # Per-sample dynamic input prefixes — handled separately below
      sample_prefixes <- c("line_type_","shape_","shape_filled_","color_selector_",
                           "color_","use_rgb_","red_","green_","blue_","alpha_",
                           "use_hex_","hex_color_","legend_label_",
                           "apply_rgb_","apply_hex_")
      is_sample_inp <- function(nm) any(vapply(sample_prefixes, startsWith, logical(1), x = nm))
      
      # Global visual settings: everything that isn't data-specific or per-sample
      global_settings <- all_inputs[vapply(names(all_inputs), function(nm)
        !(nm %in% data_inputs) && !is_sample_inp(nm), logical(1))]
      
      # Per-sample aesthetics keyed by sample name
      sample_aesthetics <- list()
      samps <- isolate(input$selected_samples)
      if (!is.null(samps)) {
        for (vn in samps) {
          vid     <- safe_id(vn)
          leg     <- input[[paste0("legend_label_", vid)]]
          col_sel <- input[[paste0("color_selector_", vid)]]
          color   <- if (!is.null(col_sel) && col_sel == "custom")
            normalize_hex_color(input[[paste0("color_", vid)]])
          else if (!is.null(col_sel)) col_sel
          else "#000000"
          sample_aesthetics[[vn]] <- list(
            color        = color,
            shape        = input[[paste0("shape_",        vid)]],
            shape_filled = input[[paste0("shape_filled_", vid)]],
            line_type    = input[[paste0("line_type_",    vid)]],
            legend_label = if (!is.null(leg) && nchar(leg) > 0) leg else vn
          )
        }
      }
      
      out <- list(
        version           = "2.5",
        saved_at          = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        global_settings   = global_settings,
        sample_aesthetics = sample_aesthetics
      )
      write_json(out, file, pretty = TRUE, auto_unbox = TRUE)
    }
  )
  
  # ── Settings: Load ───────────────────────────────────────────────────────────
  # Applies global visual settings AND per-sample aesthetics from the file.
  # Never touches: file input, sample selection, time filter range.
  observeEvent(input$loadSettings, {
    req(input$loadSettings)
    s <- tryCatch(
      fromJSON(input$loadSettings$datapath, simplifyVector = FALSE),
      error = function(e) {
        rv$settings_status <- list(type = "warn",
                                   msg = paste("Could not parse settings file:", e$message), details = NULL)
        NULL
      }
    )
    if (is.null(s)) return()
    
    # Support both v2.5 structured format and legacy flat format
    global     <- if (!is.null(s$global_settings))   s$global_settings   else list()
    sample_aes <- if (!is.null(s$sample_aesthetics)) s$sample_aesthetics
    else if (!is.null(s$samples))       s$samples
    else list()
    
    rv$imported_settings <- list(global = global, samples = sample_aes)
    
    # ── Apply global visual settings ──────────────────────────────────────────
    # Skip anything data-specific or per-sample — only visual controls
    data_inputs     <- c("file","loadSettings","selected_samples",
                         "time_filter_range","exclude_timepoints")
    sample_prefixes <- c("line_type_","shape_","shape_filled_","color_selector_",
                         "color_","use_rgb_","red_","green_","blue_","alpha_",
                         "use_hex_","hex_color_","legend_label_","apply_rgb_","apply_hex_")
    is_sample_inp <- function(nm) any(vapply(sample_prefixes, startsWith, logical(1), x = nm))
    
    n_global <- 0L
    for (nm in names(global)) {
      if (nm %in% data_inputs || is_sample_inp(nm)) next
      if (!(nm %in% names(input)))                  next
      tryCatch({
        val <- global[[nm]]
        if (is.logical(val) && length(val) == 1) {
          updateCheckboxInput(session, nm, value = val)
        } else if (is.numeric(val) && length(val) == 1) {
          updateNumericInput(session, nm, value = val)
        } else if (is.character(val) && length(val) == 1) {
          updateSelectInput(session, nm, selected = val)
          updateTextInput(session,   nm, value    = val)
        }
        n_global <- n_global + 1L
      }, error = function(e) invisible(NULL))
    }
    
    # ── Apply per-sample aesthetics for any matching names ────────────────────
    apply_imported_sample_aesthetics()
    
    cur_samps   <- isolate(input$selected_samples)
    saved_names <- names(sample_aes)
    matched     <- intersect(cur_samps,  saved_names)
    unmatched   <- setdiff(saved_names,  cur_samps)
    
    sample_msg <- if (length(saved_names) == 0) "No sample styles in file."
    else paste0(length(matched), "/", length(saved_names), " sample style(s) applied.")
    
    details_html <- if (length(saved_names) > 0) {
      match_lines   <- if (length(matched)   > 0)
        paste0("<li style='color:#155724'>&#10003; ", matched, "</li>", collapse = "")
      else ""
      unmatch_lines <- if (length(unmatched) > 0)
        paste0("<li style='color:#856404'>&#9675; ", unmatched,
               " <em>(not in current data)</em></li>", collapse = "")
      else ""
      paste0("<ul class='match-list'>", match_lines, unmatch_lines, "</ul>")
    } else NULL
    
    rv$settings_status <- list(
      type    = "ok",
      msg     = paste0(n_global, " global setting(s) applied. ", sample_msg),
      details = details_html
    )
  })
  
  # Applies imported per-sample aesthetics to any matching currently-loaded samples.
  # Called both after loadSettings and after a new CSV is loaded.
  apply_imported_sample_aesthetics <- function() {
    imp <- rv$imported_settings
    if (is.null(imp) || is.null(imp$samples) || length(imp$samples) == 0) return()
    cur_samps <- isolate(input$selected_samples)
    if (is.null(cur_samps) || length(cur_samps) == 0) return()
    for (vn in cur_samps) {
      if (!vn %in% names(imp$samples)) next
      aes   <- imp$samples[[vn]]
      vid   <- safe_id(vn)
      if (!is.null(aes$color)) {
        updateSelectInput(session, paste0("color_selector_", vid), selected = "custom")
        updateTextInput(session,   paste0("color_",          vid), value   = aes$color)
        updateTextInput(session,   paste0("hex_color_",      vid), value   = aes$color)
      }
      if (!is.null(aes$shape))
        updateSelectInput(session, paste0("shape_", vid), selected = as.character(aes$shape))
      if (!is.null(aes$shape_filled))
        updateCheckboxInput(session, paste0("shape_filled_", vid), value = aes$shape_filled)
      if (!is.null(aes$line_type))
        updateSelectInput(session, paste0("line_type_", vid), selected = aes$line_type)
      if (!is.null(aes$legend_label))
        updateTextInput(session, paste0("legend_label_", vid), value = aes$legend_label)
    }
  }
  
  # ── Settings: Clear ──────────────────────────────────────────────────────────
  # Revokes all imported settings: resets global visual settings to app defaults
  # AND restores per-sample aesthetics to position-based defaults.
  # Never touches: data, sample selection, time filter.
  observeEvent(input$clearSettings, {
    if (is.null(rv$imported_settings)) {
      rv$settings_status <- list(type = "warn", msg = "No imported settings to clear.", details = NULL)
      return()
    }
    
    # ── Reset global visual settings to app defaults ──────────────────────────
    defs <- list(
      x_scale_type          = "linear",  y_scale_type           = "log",
      use_advanced_ticks    = TRUE,       y_log_min_exponent     = -2,
      y_log_max_exponent    = 0,          custom_x_limits        = FALSE,
      custom_y_limits       = FALSE,      x_expand_left          = 0,
      x_expand_right        = 0.05,       y_expand_bottom        = 0,
      y_expand_top          = 0.05,       x_axis_label           = "Time (minutes)",
      y_axis_label          = "A550",     plot_title             = "",
      plot_subtitle         = "",         show_major_gridlines   = FALSE,
      show_minor_gridlines  = FALSE,      font_family            = "sans",
      title_font_size       = 20,         axis_label_font_size   = 20,
      axis_text_font_size   = 16,         bold_title             = TRUE,
      italic_axis_labels    = FALSE,      axis_text_angle        = "0",
      enable_highlighting   = FALSE,      enable_time_markers    = FALSE,
      color_palette         = "custom",   line_thickness         = 1,
      show_points           = TRUE,       shape_size             = 3,
      point_stroke          = 0.5,        show_end_labels        = FALSE,
      label_font_size       = 12,         label_bold             = TRUE,
      label_offset          = 3.5,        error_type             = "sem",
      error_display_mode    = "bars",     error_multiplier       = 1,
      asymmetric_error      = FALSE,      error_bar_style        = "T",
      error_bar_width       = 1,          error_bar_thickness    = 0.8,
      error_bar_position    = "middle",   shadow_alpha           = 0.2,
      custom_aspect_ratio   = FALSE,      aspect_ratio           = 1,
      export_format         = "pdf",      export_width           = 10,
      export_height         = 8,          export_dpi             = 300,
      plot_width            = 820,        plot_height            = 600,
      gif_fps               = 1,          gif_width              = 800,
      gif_height            = 600,        x_extra_ticks          = ""
    )
    for (nm in names(defs)) {
      tryCatch({
        val <- defs[[nm]]
        if (is.logical(val))      updateCheckboxInput(session, nm, value    = val)
        else if (is.numeric(val)) updateNumericInput( session, nm, value    = val)
        else { updateSelectInput(session, nm, selected = val)
          updateTextInput(session,  nm, value    = val) }
      }, error = function(e) invisible(NULL))
    }
    
    # ── Reset per-sample aesthetics to position-based defaults ───────────────
    cur_samps  <- isolate(input$selected_samples)
    def_colors <- c("#000000","#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00",
                    "#A65628","#F781BF","#999999","#66C2A5","#FC8D62","#8DA0CB")
    shape_opts <- c(16, 15, 17, 18, 25, 4, 8, 23, 3, 10)
    if (!is.null(cur_samps)) {
      for (i in seq_along(cur_samps)) {
        vn        <- cur_samps[i]; vid <- safe_id(vn)
        def_col   <- def_colors[(i - 1) %% length(def_colors) + 1]
        def_shape <- as.character(shape_opts[(i - 1) %% length(shape_opts) + 1])
        tryCatch({
          updateSelectInput(session,   paste0("color_selector_", vid), selected = "custom")
          updateTextInput(session,     paste0("color_",          vid), value    = def_col)
          updateTextInput(session,     paste0("hex_color_",      vid), value    = def_col)
          updateSelectInput(session,   paste0("shape_",          vid), selected = def_shape)
          updateCheckboxInput(session, paste0("shape_filled_",   vid), value    = TRUE)
          updateSelectInput(session,   paste0("line_type_",      vid), selected = "solid")
          updateTextInput(session,     paste0("legend_label_",   vid), value    = vn)
        }, error = function(e) invisible(NULL))
      }
    }
    
    rv$imported_settings <- NULL
    rv$settings_status   <- list(
      type    = "ok",
      msg     = "All imported settings cleared. Global and per-sample settings restored to defaults.",
      details = NULL
    )
  })
  
  output$settings_status_ui <- renderUI({
    if (is.null(rv$settings_status)) return(NULL)
    cls <- if (rv$settings_status$type == "ok") "settings-status ok" else "settings-status warn"
    tagList(
      div(class = cls, rv$settings_status$msg),
      if (!is.null(rv$settings_status$details))
        HTML(paste0('<div style="font-size:.82em;margin-top:4px;">',
                    rv$settings_status$details, '</div>'))
    )
  })
  
  # Null-coalescing operator (must be defined before first use in renderUI below)
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # ── Dynamic UI: var_settings (paginated for large sample counts) ─────────────
  # When there are more than PAGE_SIZE samples, render only one page at a time.
  # This keeps the DOM small and prevents Shiny from serialising hundreds of inputs.
  PAGE_SIZE <- 20L
  
  # Reset to page 1 whenever the sample selection changes
  observeEvent(input$selected_samples, { rv$style_page <- 1L }, ignoreNULL = FALSE)
  observeEvent(input$style_prev_page,  { rv$style_page <- max(1L, rv$style_page - 1L) })
  observeEvent(input$style_next_page,  {
    n_pages <- ceiling(length(input$selected_samples) / PAGE_SIZE)
    rv$style_page <- min(n_pages, rv$style_page + 1L)
  })
  
  output$var_settings <- renderUI({
    req(rv$od_vars, input$selected_samples)
    all_samps <- input$selected_samples
    n_total   <- length(all_samps)
    n_pages   <- ceiling(n_total / PAGE_SIZE)
    page      <- rv$style_page
    idx_start <- (page - 1L) * PAGE_SIZE + 1L
    idx_end   <- min(page * PAGE_SIZE, n_total)
    page_samps <- all_samps[idx_start:idx_end]
    
    shape_opts <- c("Circle" = 16, "Square" = 15, "Triangle Up" = 17,
                    "Diamond" = 18, "Triangle Down" = 25, "Cross" = 4,
                    "X" = 8, "Open Circle" = 21, "Open Square" = 22,
                    "Open Triangle" = 24, "Plus" = 3, "Star" = 10)
    default_colors <- c("#000000","#E41A1C","#377EB8","#4DAF4A",
                        "#984EA3","#FF7F00","#A65628","#F781BF",
                        "#999999","#66C2A5","#FC8D62","#8DA0CB")
    named_colors   <- c("#000000","#E41A1C","#377EB8","#4DAF4A",
                        "#984EA3","#FF7F00","#FFFF33","#A65628",
                        "#F781BF","#999999","#009E73")
    
    # Pagination nav bar (only shown when needed)
    nav_bar <- if (n_pages > 1L) {
      div(style = "display:flex;align-items:center;justify-content:space-between;margin-bottom:10px;",
          actionButton("style_prev_page", "\u25C4 Prev",
                       style = "font-size:.8em;padding:3px 8px;",
                       disabled = if (page <= 1L) "disabled" else NULL),
          span(style = "font-size:.85em;color:#555;",
               paste0("Page ", page, " of ", n_pages,
                      " (samples ", idx_start, "\u2013", idx_end, " of ", n_total, ")")),
          actionButton("style_next_page", "Next \u25BA",
                       style = "font-size:.8em;padding:3px 8px;",
                       disabled = if (page >= n_pages) "disabled" else NULL)
      )
    } else NULL
    
    sample_widgets <- lapply(seq_along(page_samps), function(j) {
      i   <- idx_start + j - 1L   # global index across all samples
      vn  <- page_samps[j]
      vid <- safe_id(vn)
      shape_def <- as.character(shape_opts[1 + (i - 1L) %% length(shape_opts)])
      
      imp_aes <- rv$imported_settings$samples[[vn]]
      
      if (!is.null(imp_aes)) {
        color_def <- imp_aes$color %||% default_colors[1]
        sel_def   <- "custom"
      } else if (!is.null(rv$palette_colors) && input$color_palette != "custom") {
        idx       <- match(vn, rv$od_vars)
        color_def <- if (!is.na(idx)) rv$palette_colors[idx] else default_colors[1]
        sel_def   <- "custom"
      } else {
        color_def <- default_colors[1 + (i - 1L) %% length(default_colors)]
        sel_def   <- if (color_def %in% named_colors) color_def else "custom"
      }
      if (is.na(color_def) || is.null(color_def)) color_def <- "#000000"
      
      default_shape  <- if (!is.null(imp_aes$shape))       as.character(imp_aes$shape)    else shape_def
      default_lt     <- if (!is.null(imp_aes$line_type))    imp_aes$line_type              else "solid"
      default_filled <- if (!is.null(imp_aes$shape_filled)) imp_aes$shape_filled           else TRUE
      default_leg    <- if (!is.null(imp_aes$legend_label)) imp_aes$legend_label           else vn
      
      div(
        style = "margin-bottom:12px;padding:10px;border:1px solid #ddd;border-radius:6px;background:#fafafa;",
        h4(vn, style = "margin:0 0 8px 0;font-size:1em;color:#333;"),
        selectInput(paste0("line_type_", vid), "Line Type:",
                    choices  = c("Solid" = "solid", "Dashed" = "dashed",
                                 "Dotted" = "dotted", "DotDash" = "dotdash",
                                 "LongDash" = "longdash", "TwoDash" = "twodash"),
                    selected = default_lt),
        h5("Shape", style = "margin:8px 0 4px;"),
        fluidRow(
          column(6, selectInput(paste0("shape_", vid), "Shape:",
                                choices = shape_opts, selected = default_shape)),
          column(6, checkboxInput(paste0("shape_filled_", vid), "Filled", default_filled))
        ),
        div(class = "panel-section",
            h5("Color", class = "panel-title"),
            selectInput(paste0("color_selector_", vid), "Pick Color:",
                        choices  = c("Black" = "#000000", "Red"    = "#E41A1C",
                                     "Blue"  = "#377EB8", "Green"  = "#4DAF4A",
                                     "Purple"= "#984EA3", "Orange" = "#FF7F00",
                                     "Yellow"= "#FFFF33", "Brown"  = "#A65628",
                                     "Pink"  = "#F781BF", "Gray"   = "#999999",
                                     "Teal"  = "#009E73", "Custom" = "custom"),
                        selected = sel_def),
            conditionalPanel(
              condition = paste0("input['color_selector_", vid, "'] == 'custom'"),
              div(style = "display:flex;align-items:center;gap:8px;",
                  textInput(paste0("color_", vid), "HEX:", value = color_def),
                  div(id    = paste0("color_preview_", vid),
                      class = "color-preview",
                      style = paste0("background-color:", color_def, ";"))
              )
            ),
            checkboxInput(paste0("use_rgb_", vid), "RGB Sliders", FALSE),
            conditionalPanel(
              condition = paste0("input['use_rgb_", vid, "'] == true"),
              sliderInput(paste0("red_",   vid), "R:", 0, 255, 0, 1),
              sliderInput(paste0("green_", vid), "G:", 0, 255, 0, 1),
              sliderInput(paste0("blue_",  vid), "B:", 0, 255, 0, 1),
              sliderInput(paste0("alpha_", vid), "A:", 0,   1, 1, 0.01),
              actionButton(paste0("apply_rgb_", vid), "Apply RGB",
                           style = "margin-top:4px;font-size:.85em;")
            ),
            checkboxInput(paste0("use_hex_", vid), "Direct HEX Entry", FALSE),
            conditionalPanel(
              condition = paste0("input['use_hex_", vid, "'] == true"),
              div(style = "display:flex;align-items:center;gap:8px;",
                  textInput(paste0("hex_color_", vid), "HEX Code:", value = color_def),
                  actionButton(paste0("apply_hex_", vid), "Apply",
                               style = "font-size:.85em;")
              )
            )
        ),
        textInput(paste0("legend_label_", vid), "Legend Label:", value = default_leg)
      )
    })
    
    tagList(nav_bar, do.call(tagList, sample_widgets))
  })
  
  # ── Color reactivity ─────────────────────────────────────────────────────────
  # Observer registry: track which sample IDs have already had observers registered
  # so we never register the same observeEvent twice (prevents accumulation / leak).
  registered_color_obs <- character(0)
  
  observe({
    req(input$selected_samples, rv$data)
    new_samps <- setdiff(safe_id(input$selected_samples), registered_color_obs)
    if (length(new_samps) == 0) return()
    
    for (vn in input$selected_samples[safe_id(input$selected_samples) %in% new_samps]) {
      local({
        vi <- safe_id(vn)
        registered_color_obs <<- c(registered_color_obs, vi)
        
        observeEvent(input[[paste0("color_selector_", vi)]], {
          sel <- input[[paste0("color_selector_", vi)]]
          if (!is.null(sel) && sel != "custom") {
            rc <- tryCatch(col2rgb(sel), error = function(e) NULL)
            if (!is.null(rc)) {
              updateSliderInput(session, paste0("red_",   vi), value = rc[1, 1])
              updateSliderInput(session, paste0("green_", vi), value = rc[2, 1])
              updateSliderInput(session, paste0("blue_",  vi), value = rc[3, 1])
              updateTextInput(session,   paste0("color_",     vi), value = sel)
              updateTextInput(session,   paste0("hex_color_", vi), value = sel)
            }
          }
        }, ignoreInit = TRUE)
        
        observeEvent(input[[paste0("color_", vi)]], {
          col <- normalize_hex_color(input[[paste0("color_", vi)]])
          session$sendCustomMessage("updateColorPreview",
                                    list(id = paste0("color_preview_", vi), color = col))
          rc <- tryCatch(col2rgb(col), error = function(e) NULL)
          if (!is.null(rc)) {
            updateSliderInput(session, paste0("red_",   vi), value = rc[1, 1])
            updateSliderInput(session, paste0("green_", vi), value = rc[2, 1])
            updateSliderInput(session, paste0("blue_",  vi), value = rc[3, 1])
          }
        }, ignoreInit = TRUE)
        
        observeEvent(input[[paste0("apply_rgb_", vi)]], {
          hex <- rgb_to_hex(input[[paste0("red_",   vi)]],
                            input[[paste0("green_", vi)]],
                            input[[paste0("blue_",  vi)]],
                            input[[paste0("alpha_", vi)]])
          updateSelectInput(session, paste0("color_selector_", vi), selected = "custom")
          updateTextInput(session,   paste0("color_",     vi), value = hex)
          updateTextInput(session,   paste0("hex_color_", vi), value = hex)
        })
        
        observeEvent(input[[paste0("apply_hex_", vi)]], {
          hex <- normalize_hex_color(input[[paste0("hex_color_", vi)]])
          updateSelectInput(session, paste0("color_selector_", vi), selected = "custom")
          updateTextInput(session, paste0("color_", vi), value = hex)
          rc <- tryCatch(col2rgb(hex), error = function(e) NULL)
          if (!is.null(rc)) {
            updateSliderInput(session, paste0("red_",   vi), value = rc[1, 1])
            updateSliderInput(session, paste0("green_", vi), value = rc[2, 1])
            updateSliderInput(session, paste0("blue_",  vi), value = rc[3, 1])
          }
        })
      })
    }
  })
  
  # ── Dynamic UI: region / marker settings ────────────────────────────────────
  output$region_settings <- renderUI({
    req(input$region_count, input$enable_highlighting)
    do.call(tagList, lapply(seq_len(input$region_count), function(i) {
      div(style = "border:1px solid #ddd;padding:10px;margin-bottom:8px;border-radius:5px;",
          h5(paste("Region", i)),
          fluidRow(
            column(6, numericInput(paste0("region_x_min_", i), "X min:", 0)),
            column(6, numericInput(paste0("region_x_max_", i), "X max:", 60))
          ),
          fluidRow(
            column(6, numericInput(paste0("region_y_min_", i), "Y min:", 0.01)),
            column(6, numericInput(paste0("region_y_max_", i), "Y max:", 1))
          ),
          selectInput(paste0("region_color_", i), "Fill Color:",
                      choices  = c("Light Gray"   = "#ebebeb", "Light Blue"  = "#e6f3ff",
                                   "Light Red"    = "#ffebeb", "Light Green" = "#ebffeb",
                                   "Light Yellow" = "#ffffeb", "Custom"      = "custom"),
                      selected = "#ebebeb"),
          conditionalPanel(
            condition = paste0("input['region_color_", i, "'] == 'custom'"),
            textInput(paste0("region_color_custom_", i), "HEX:", "#ebebeb")
          ),
          sliderInput(paste0("region_alpha_", i), "Transparency:", 0, 1, 0.3, 0.05)
      )
    }))
  })
  
  output$time_marker_settings <- renderUI({
    req(input$marker_count, input$enable_time_markers)
    do.call(tagList, lapply(seq_len(input$marker_count), function(i) {
      div(style = "border:1px solid #ddd;padding:10px;margin-bottom:8px;border-radius:5px;",
          h5(paste("Marker", i)),
          numericInput(paste0("marker_time_", i), "Time Point:", 30),
          selectInput(paste0("marker_line_type_", i), "Line Type:",
                      choices  = c("Dashed" = "dashed", "Solid" = "solid",
                                   "Dotted" = "dotted", "DotDash" = "dotdash",
                                   "LongDash" = "longdash"),
                      selected = "dashed"),
          selectInput(paste0("marker_color_", i), "Color:",
                      choices  = c("Black" = "#000000", "Red"   = "#E41A1C",
                                   "Blue"  = "#377EB8", "Green" = "#4DAF4A",
                                   "Gray"  = "#999999", "Custom" = "custom"),
                      selected = "#000000"),
          conditionalPanel(
            condition = paste0("input['marker_color_", i, "'] == 'custom'"),
            textInput(paste0("marker_color_custom_", i), "HEX:", "#000000")
          ),
          sliderInput(paste0("marker_size_", i), "Width:", 0.1, 3, 1, 0.1),
          checkboxInput(paste0("marker_label_", i), "Add Label", FALSE),
          conditionalPanel(
            condition = paste0("input['marker_label_", i, "'] == true"),
            textInput(paste0("marker_text_", i),           "Label:",    paste0("t=", 30)),
            numericInput(paste0("marker_label_size_", i),  "Size:",     4, 2, 12),
            selectInput(paste0("marker_label_position_", i), "Position:",
                        choices = c("Top" = "top", "Middle" = "middle", "Bottom" = "bottom"),
                        selected = "top"),
            numericInput(paste0("marker_label_hjust_", i), "H-Offset:", 0, -5, 5, 0.1)
          )
      )
    }))
  })
  
  # ── Time filtering ───────────────────────────────────────────────────────────
  apply_time_filters <- function(d, time_col) {
    d[[time_col]] <- suppressWarnings(as.numeric(as.character(d[[time_col]])))
    if (!isTRUE(input$enable_time_filter)) return(d)
    rng <- input$time_filter_range
    if (!is.null(rng) && length(rng) == 2) {
      t_vec <- d[[time_col]]
      d <- d[!is.na(t_vec) & t_vec >= rng[1] & t_vec <= rng[2], , drop = FALSE]
    }
    excl <- parse_excluded_timepoints(input$exclude_timepoints, rv$all_timepoints)
    if (length(excl) > 0) {
      t_vec <- d[[time_col]]
      tol   <- max(abs(rv$all_timepoints), na.rm = TRUE) * 0.001 + 0.001
      # Vectorised: matrix of abs differences, any column within tol means exclude
      hit   <- rowSums(abs(outer(t_vec, excl, "-")) <= tol) == 0L
      d     <- d[hit, , drop = FALSE]
    }
    d
  }
  
  # ── Data prep ────────────────────────────────────────────────────────────────
  prepare_plot_data <- function(samples = NULL) {
    req(rv$data, rv$time_col, input$selected_samples)
    samps <- if (!is.null(samples)) samples else input$selected_samples
    if (rv$is_long_format) {
      req(rv$group_col, rv$value_col)
      d  <- apply_time_filters(rv$data, rv$time_col)
      # Pre-filter rows to only requested samples before any grouping
      d  <- d[d[[rv$group_col]] %in% samps, , drop = FALSE]
      pd <- d %>%
        group_by(across(all_of(c(rv$time_col, rv$group_col)))) %>%
        summarise(mean_value = mean(.data[[rv$value_col]], na.rm = TRUE),
                  sd_value   = sd(  .data[[rv$value_col]], na.rm = TRUE),
                  n          = n(), .groups = "drop") %>%
        mutate(sd_value   = ifelse(is.na(sd_value), 0, sd_value),
               sem_value  = sd_value / sqrt(n),
               ci95_value = qt(0.975, df = pmax(n - 1, 1)) * sem_value)
      names(pd)[names(pd) == rv$group_col] <- "variable"
      names(pd)[names(pd) == rv$time_col]  <- "time"
    } else {
      d    <- apply_time_filters(rv$data, rv$time_col)
      meas <- intersect(rv$od_vars_raw, samps)
      # Select only the time column + requested sample columns before pivot
      # — avoids creating a huge intermediate frame for wide data with many replicates
      d_sub <- d[, c(rv$time_col, meas), drop = FALSE]
      pd   <- d_sub %>%
        pivot_longer(cols = all_of(meas), names_to = "variable", values_to = "value") %>%
        group_by(time = .data[[rv$time_col]], variable) %>%
        summarise(mean_value = mean(value, na.rm = TRUE),
                  sd_value   = sd(  value, na.rm = TRUE),
                  n          = n(), .groups = "drop") %>%
        mutate(sd_value   = ifelse(is.na(sd_value), 0, sd_value),
               sem_value  = sd_value / sqrt(n),
               ci95_value = qt(0.975, df = pmax(n - 1, 1)) * sem_value)
    }
    pd %>% filter(is.finite(time) & is.finite(mean_value))
  }
  
  # ── Aesthetics resolver ──────────────────────────────────────────────────────
  resolve_aesthetics <- function(samples) {
    n          <- length(samples)
    shapes     <- setNames(numeric(n),   samples)
    colors     <- setNames(character(n), samples)
    line_types <- setNames(character(n), samples)
    leg_labels <- setNames(character(n), samples)
    filled_map <- setNames(logical(n),   samples)
    def_colors <- c("#000000","#E41A1C","#377EB8","#4DAF4A",
                    "#984EA3","#FF7F00","#A65628","#F781BF",
                    "#999999","#66C2A5","#FC8D62","#8DA0CB")
    def_shapes <- c(16, 15, 17, 18, 25, 4, 8, 23, 3, 10)
    
    for (i in seq_len(n)) {
      vn  <- samples[i]; vid <- safe_id(vn)
      si  <- input[[paste0("shape_", vid)]]
      shapes[i] <- if (!is.null(si)) as.numeric(si)
      else def_shapes[(i - 1) %% length(def_shapes) + 1]
      
      sel <- input[[paste0("color_selector_", vid)]]
      if (!is.null(sel) && sel == "custom")
        colors[i] <- normalize_hex_color(input[[paste0("color_", vid)]])
      else if (!is.null(sel))
        colors[i] <- sel
      else if (!is.null(rv$palette_colors)) {
        idx       <- match(vn, rv$od_vars)
        colors[i] <- if (!is.na(idx)) rv$palette_colors[idx]
        else def_colors[(i - 1) %% length(def_colors) + 1]
      } else
        colors[i] <- def_colors[(i - 1) %% length(def_colors) + 1]
      
      lt <- input[[paste0("line_type_", vid)]]
      line_types[i] <- if (!is.null(lt)) lt else "solid"
      
      ll <- input[[paste0("legend_label_", vid)]]
      leg_labels[i] <- if (!is.null(ll) && nchar(ll) > 0) ll else vn
      
      fi <- input[[paste0("shape_filled_", vid)]]
      filled_map[i] <- if (!is.null(fi)) fi else TRUE
    }
    list(shapes = shapes, colors = colors, line_types = line_types,
         leg_labels = leg_labels, filled_map = filled_map)
  }
  
  # ── Core plot builder ────────────────────────────────────────────────────────
  # highlight_samples: if non-NULL, all other samples are greyed out (not removed)
  build_plot <- function(plot_data, samples, aes_vals, highlight_samples = NULL) {
    shapes     <- aes_vals$shapes;     colors     <- aes_vals$colors
    line_types <- aes_vals$line_types; leg_labels <- aes_vals$leg_labels
    filled_map <- aes_vals$filled_map; n_vars     <- length(samples)
    
    # Grey out non-highlighted samples while keeping axis scale stable
    display_colors <- colors
    if (!is.null(highlight_samples))
      for (i in seq_len(n_vars))
        if (!samples[i] %in% highlight_samples) display_colors[i] <- "#DDDDDD"
    
    p <- ggplot(plot_data,
                aes(x = time, y = mean_value, group = variable,
                    color = variable, shape = variable, linetype = variable))
    
    # Region highlighting
    if (isTRUE(input$enable_highlighting) && !is.null(input$region_count)) {
      for (i in seq_len(input$region_count)) {
        x1 <- input[[paste0("region_x_min_", i)]]; x2 <- input[[paste0("region_x_max_", i)]]
        y1 <- input[[paste0("region_y_min_", i)]]; y2 <- input[[paste0("region_y_max_", i)]]
        rc  <- input[[paste0("region_color_", i)]]
        col <- if (!is.null(rc) && rc == "custom") input[[paste0("region_color_custom_", i)]] else rc
        alp <- input[[paste0("region_alpha_", i)]]
        if (!is.null(x1) && !is.null(col) && !is.null(alp))
          p <- p + annotate("rect", xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=col, alpha=alp)
      }
    }
    
    # Time markers
    if (isTRUE(input$enable_time_markers) && !is.null(input$marker_count)) {
      yr <- range(plot_data$mean_value, na.rm = TRUE)
      if (input$y_scale_type == "log") yr[1] <- max(1e-9, yr[1])
      for (i in seq_len(input$marker_count)) {
        tp <- input[[paste0("marker_time_", i)]]
        lt <- input[[paste0("marker_line_type_", i)]]
        mc <- input[[paste0("marker_color_", i)]]
        lc <- if (!is.null(mc) && mc == "custom") input[[paste0("marker_color_custom_", i)]] else mc
        ls <- input[[paste0("marker_size_", i)]]
        if (!is.null(tp) && !is.null(lc))
          p <- p + geom_vline(xintercept = tp, linetype = lt, color = lc, linewidth = ls)
        if (isTRUE(input[[paste0("marker_label_", i)]])) {
          lbl  <- input[[paste0("marker_text_",         i)]]
          lsz  <- input[[paste0("marker_label_size_",   i)]]
          lpos <- input[[paste0("marker_label_position_",i)]]
          lhj  <- input[[paste0("marker_label_hjust_",  i)]]; if (is.null(lhj)) lhj <- 0
          yp <- switch(lpos,
                       top    = if (input$y_scale_type == "log") 10^(log10(yr[2]) - diff(log10(yr)) * 0.08) else yr[1] + diff(yr) * 0.92,
                       bottom = if (input$y_scale_type == "log") 10^(log10(yr[1]) + diff(log10(yr)) * 0.08) else yr[1] + diff(yr) * 0.08,
                       middle = if (input$y_scale_type == "log") 10^(mean(log10(yr))) else mean(yr))
          p <- p + annotate("text", x = tp + lhj, y = yp, label = lbl, size = lsz, color = lc)
        }
      }
    }
    
    # Error bars / shadow
    # Track whether shadow/ribbon mode is used so we can unify the fill scale
    # with the points layer later (ggplot2 only allows ONE scale per aesthetic).
    using_shadow <- FALSE
    
    if (input$error_type != "none") {
      ec     <- switch(input$error_type, sd="sd_value", sem="sem_value", ci95="ci95_value")
      em     <- input$error_multiplier
      # Guard against NA error values (single-replicate samples produce NA sd)
      err_vals <- plot_data[[ec]]
      err_vals[is.na(err_vals)] <- 0
      
      lo_val <- if (input$y_scale_type == "log")
        pmax(plot_data$mean_value - err_vals * em, 1e-9)
      else if (isTRUE(input$asymmetric_error))
        pmax(plot_data$mean_value - err_vals * em, 0)
      else
        plot_data$mean_value - err_vals * em
      plot_data$err_lo <- lo_val
      plot_data$err_hi <- plot_data$mean_value + err_vals * em
      
      dm <- if (!is.null(input$error_display_mode)) input$error_display_mode else "bars"
      if (dm == "shadow") {
        using_shadow <- TRUE
        p <- p +
          geom_ribbon(data = plot_data,
                      aes(x = time, ymin = err_lo, ymax = err_hi,
                          group = variable, fill = variable),
                      alpha = input$shadow_alpha, color = NA, inherit.aes = FALSE)
        # NOTE: scale_fill_manual is added AFTER the points section to unify
        # the fill scale for both ribbon and points (if points are enabled).
      } else {
        pos <- if (!is.null(input$error_bar_position) && input$error_bar_position == "dodge")
          position_dodge(0.2) else position_identity()
        blt <- if (!is.null(input$error_bar_style) && input$error_bar_style == "dashed") "dashed" else "solid"
        if (!is.null(input$error_bar_style) && input$error_bar_style == "T") {
          p <- p + geom_errorbar(data = plot_data,
                                 aes(x = time, ymin = err_lo, ymax = err_hi, color = variable),
                                 width = input$error_bar_width, linewidth = input$error_bar_thickness,
                                 linetype = blt, position = pos, inherit.aes = FALSE)
        } else {
          p <- p + geom_linerange(data = plot_data,
                                  aes(x = time, ymin = err_lo, ymax = err_hi, color = variable),
                                  linewidth = input$error_bar_thickness,
                                  linetype = blt, position = pos, inherit.aes = FALSE)
        }
      }
    }
    
    p <- p + geom_line(aes(group = variable), linewidth = input$line_thickness)
    
    if (input$show_points) {
      # Build lookup tables of shape and fill per variable.
      shape_map <- integer(n_vars)
      fill_map  <- character(n_vars)
      for (i in seq_len(n_vars)) {
        pt_s <- as.integer(shapes[i])
        if (!filled_map[i]) {
          pt_s         <- switch(as.character(pt_s),
                                 "16" = 21L, "15" = 22L, "17" = 24L, "18" = 23L, pt_s)
          fill_map[i]  <- "white"
        } else {
          fill_map[i]  <- display_colors[i]
        }
        shape_map[i] <- pt_s
      }
      names(shape_map) <- samples
      names(fill_map)  <- samples
      
      if (using_shadow) {
        # When shadow/ribbon is active, both ribbon and points share the 'fill'
        # aesthetic mapped to 'variable'.  We use a single scale_fill_manual that
        # maps each variable to its display color.  For open (unfilled) shapes the
        # fill is overridden to white via `override.aes` — open shapes use stroke-
        # based variants (21/22/24/23) whose interior respects the fill scale, so
        # we simply accept the fill color from the scale for filled shapes.
        # 
        # Trade-off: open shapes will show the ribbon color fill instead of white
        # inside the point.  This is acceptable because the alternative (two
        # conflicting fill scales) crashes ggplot2 entirely.  Users who need white-
        # filled open shapes can switch error display to "bars" mode.
        p <- p + geom_point(
          data        = plot_data,
          aes(x = time, y = mean_value, color = variable, shape = variable,
              fill = variable),
          size        = input$shape_size,
          stroke      = input$point_stroke,
          inherit.aes = FALSE
        ) +
          scale_shape_manual(values = shape_map, labels = leg_labels,
                             name = NULL, guide = "none") +
          scale_fill_manual(values = setNames(display_colors, samples),
                            labels = leg_labels, guide = "none")
      } else {
        # No shadow: use scale_fill_identity for per-point fill control
        pt_data          <- plot_data
        pt_data$pt_fill  <- fill_map[pt_data$variable]
        
        p <- p + geom_point(
          data        = pt_data,
          aes(x = time, y = mean_value, color = variable, shape = variable,
              fill = pt_fill),
          size        = input$shape_size,
          stroke      = input$point_stroke,
          inherit.aes = FALSE
        ) +
          scale_shape_manual(values = shape_map, labels = leg_labels,
                             name = NULL, guide = "none") +
          scale_fill_identity()
      }
    } else if (using_shadow) {
      # Points disabled but shadow is active: still need the fill scale for ribbon
      p <- p + scale_fill_manual(values = setNames(display_colors, samples),
                                  labels = leg_labels, guide = "none")
    }
    
    if (input$show_end_labels) {
      mt  <- max(plot_data$time, na.rm = TRUE)
      off <- mt * (input$label_offset / 100)
      ep  <- plot_data %>% group_by(variable) %>% filter(time == max(time)) %>% ungroup()
      p   <- p + geom_text_repel(data = ep,
                                 aes(label = variable, color = variable, x = Inf, y = mean_value),
                                 direction = "y", xlim = c(mt + off, Inf),
                                 min.segment.length = Inf, hjust = 0,
                                 size = input$label_font_size / 2.835,
                                 fontface = if (input$label_bold) "bold" else "plain")
    }
    
    p <- p +
      scale_color_manual(   values = setNames(display_colors, samples), labels = leg_labels, name = NULL) +
      scale_linetype_manual(values = line_types,                         labels = leg_labels, name = NULL)
    
    # When points are disabled the shape scale wasn't added inside the points block;
    # add it now for a consistent legend.
    if (!isTRUE(input$show_points))
      p <- p + scale_shape_manual(values = shapes, labels = leg_labels, name = NULL)
    
    p <- p +
      guides(shape = guide_legend(override.aes = list(alpha = 1)),
             color = guide_legend(override.aes = list(alpha = 1)))
    
    x_exp  <- expansion(mult = c(input$x_expand_left,   input$x_expand_right))
    y_exp  <- expansion(mult = c(input$y_expand_bottom, input$y_expand_top))
    x_lims <- if (isTRUE(input$custom_x_limits)) c(input$x_min, input$x_max) else NULL
    y_lims <- if (isTRUE(input$custom_y_limits)) c(input$y_min, input$y_max) else NULL
    
    if (input$x_scale_type == "log") {
      p <- p + scale_x_log10(limits = x_lims, expand = x_exp)
    } else if (input$x_scale_type == "sqrt") {
      p <- p + scale_x_sqrt(limits = x_lims, expand = x_exp)
    } else if (input$x_scale_type == "reverse") {
      p <- p + scale_x_reverse(limits = x_lims, expand = x_exp)
    } else if (isTRUE(input$use_advanced_ticks)) {
      # Determine the upper bound for tick generation
      mt2 <- if (isTRUE(input$custom_x_limits)) input$x_max
      else max(plot_data$time, na.rm = TRUE)
      x0  <- if (isTRUE(input$custom_x_limits)) input$x_min else 0
      
      # Parse any user-specified extra tick positions
      extra_ticks <- tryCatch({
        raw <- trimws(input$x_extra_ticks)
        if (nchar(raw) == 0) numeric(0)
        else {
          vals <- suppressWarnings(as.numeric(strsplit(raw, "[,;[:space:]]+")[[1]]))
          vals[is.finite(vals)]
        }
      }, error = function(e) numeric(0))
      
      # Determine major interval: manual if provided, otherwise auto
      manual_interval <- input$x_tick_interval
      use_manual <- !is.null(manual_interval) &&
        !is.na(manual_interval) &&
        is.numeric(manual_interval) &&
        manual_interval > 0
      
      if (use_manual) {
        iv  <- manual_interval
        mjb <- seq(x0, ceiling((mt2 - x0) / iv) * iv + x0, by = iv)
        # Always include the actual data endpoint if it isn't already a break
        if (!isTRUE(input$custom_x_limits) && !mt2 %in% mjb)
          mjb <- sort(unique(c(mjb, mt2)))
        mnb <- seq(x0, max(mjb), by = iv / 2)
      } else {
        # Auto interval (original logic)
        if (mt2 <= 60)       { iv <- 10;  mjb <- seq(0, ceiling(mt2/10)*10,  10);  mnb <- seq(0, ceiling(mt2/10)*10,  5) }
        else if (mt2 <= 120) { iv <- 30;  mjb <- seq(0, ceiling(mt2/30)*30,  30);  mnb <- seq(0, ceiling(mt2/30)*30, 10) }
        else                 { iv <- 60;  mjb <- seq(0, ceiling(mt2/60)*60,  60);  mnb <- seq(0, ceiling(mt2/60)*60, 30) }
        # Always include the actual data endpoint even in auto mode
        if (!isTRUE(input$custom_x_limits) && !mt2 %in% mjb)
          mjb <- sort(unique(c(mjb, mt2)))
      }
      
      # Union in any extra positions the user explicitly requested
      if (length(extra_ticks) > 0)
        mjb <- sort(unique(c(mjb, extra_ticks)))
      
      p <- p + scale_x_continuous(limits = x_lims, expand = x_exp, breaks = mjb,
                                  minor_breaks = mnb, guide = guide_prism_minor())
    } else {
      p <- p + scale_x_continuous(limits = x_lims, expand = x_exp)
    }
    
    if (input$y_scale_type == "log") {
      if (isTRUE(input$use_advanced_ticks)) {
        minE <- input$y_log_min_exponent; maxE <- input$y_log_max_exponent
        if (is.null(y_lims)) y_lims <- c(10^minE, if (maxE >= 0) 10^(maxE+1) else 10^maxE)
        ymn <- c(rep(1:9, maxE-minE+1) * 10^rep(minE:maxE, each=9))
        if (maxE >= 0) ymn <- c(ymn, 10^(maxE+1))
        p <- p + scale_y_log10(limits = y_lims, expand = y_exp, minor_breaks = ymn,
                               guide = guide_prism_minor())
      } else {
        p <- p + scale_y_log10(limits = y_lims, expand = y_exp)
      }
    } else if (input$y_scale_type == "sqrt") {
      p <- p + scale_y_sqrt(limits = y_lims, expand = y_exp)
    } else if (input$y_scale_type == "reverse") {
      p <- p + scale_y_reverse(limits = y_lims, expand = y_exp)
    } else {
      p <- p + scale_y_continuous(limits = y_lims, expand = y_exp)
    }
    
    tf      <- if (isTRUE(input$bold_title))        "bold"   else "plain"
    af      <- if (isTRUE(input$italic_axis_labels)) "italic" else "plain"
    base_t  <- if (isTRUE(input$use_advanced_ticks)) theme_prism(border=TRUE) else theme_pubr()
    maj_col <- if (!is.null(input$major_gridline_color) && input$major_gridline_color == "custom")
      input$major_gridline_color_custom else input$major_gridline_color
    min_col <- if (!is.null(input$minor_gridline_color) && input$minor_gridline_color == "custom")
      input$minor_gridline_color_custom else input$minor_gridline_color
    x_ang   <- as.numeric(input$axis_text_angle)
    
    p <- p + base_t + theme(
      text             = element_text(family = input$font_family),
      plot.title       = element_text(size = input$title_font_size,       face = tf, hjust = 0.5),
      axis.title       = element_text(size = input$axis_label_font_size,  face = af),
      axis.text        = element_text(size = input$axis_text_font_size),
      axis.text.x      = element_text(angle = x_ang,
                                      hjust = if (x_ang > 0) 1 else 0.5,
                                      vjust = if (x_ang > 0) 1 else 0.5),
      panel.grid.major = if (isTRUE(input$show_major_gridlines))
        element_line(color = maj_col, linewidth = input$major_gridline_size) else element_blank(),
      panel.grid.minor = if (isTRUE(input$show_minor_gridlines))
        element_line(color = min_col, linewidth = input$minor_gridline_size) else element_blank(),
      axis.ticks        = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length = unit(0.15, "cm"),
      legend.position   = "right",
      legend.background = element_rect(fill = "white", color = "gray80"),
      legend.key        = element_rect(fill = NA),
      panel.border      = element_rect(color = "black", fill = NA, linewidth = 0.5)
    )
    
    if (isTRUE(input$custom_aspect_ratio))
      p <- p + theme(aspect.ratio = 1 / input$aspect_ratio)
    
    p + coord_cartesian(clip = "off") +
      labs(x        = input$x_axis_label,
           y        = input$y_axis_label,
           title    = input$plot_title,
           subtitle = if (nchar(trimws(input$plot_subtitle)) > 0) input$plot_subtitle else NULL)
  }
  
  generate_plot <- function() {
    req(rv$data, input$selected_samples)
    tryCatch(
      build_plot(prepare_plot_data(), input$selected_samples,
                 resolve_aesthetics(input$selected_samples)),
      error = function(e) {
        # Return a blank ggplot with the error message rather than crashing the session
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = paste("Plot error:", conditionMessage(e)),
                   color = "red", size = 4, hjust = 0.5) +
          theme_void()
      }
    )
  }
  
  # Debounced reactive: coalesces rapid input changes (slider drags, typing) into
  # a single plot rebuild after 400 ms of inactivity — prevents render pile-up.
  plot_inputs_debounced <- debounce(reactive({
    list(
      selected_samples    = input$selected_samples,
      error_type          = input$error_type,
      error_display_mode  = input$error_display_mode,
      error_multiplier    = input$error_multiplier,
      line_thickness      = input$line_thickness,
      show_points         = input$show_points,
      shape_size          = input$shape_size,
      x_scale_type        = input$x_scale_type,
      y_scale_type        = input$y_scale_type,
      custom_x_limits     = input$custom_x_limits,
      custom_y_limits     = input$custom_y_limits,
      x_min = input$x_min, x_max = input$x_max,
      y_min = input$y_min, y_max = input$y_max,
      enable_time_filter  = input$enable_time_filter,
      time_filter_range   = input$time_filter_range,
      exclude_timepoints  = input$exclude_timepoints
    )
  }), millis = 400)
  
  output$plot_container <- renderUI({
    plotOutput("od_plot",
               height = paste0(input$plot_height, "px"),
               width  = paste0(input$plot_width,  "px"))
  })
  
  output$od_plot <- renderPlot({
    plot_inputs_debounced()   # take debounced dependency
    generate_plot()
  }, res = 96)
  
  output$downloadPlot <- downloadHandler(
    filename = function()
      paste0("od_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", input$export_format),
    content  = function(file)
      ggsave(file, plot = generate_plot(), device = input$export_format,
             width = input$export_width, height = input$export_height,
             units = "in", dpi = input$export_dpi)
  )
  
  # ── PowerPoint Export ────────────────────────────────────────────────────────
  # Cumulative build: slide k shows samples 1..k with full colour (no grey mask).
  # Axes stay consistent across slides because pd_all (full dataset) is used for
  # every plot — only the subset of visible samples changes.
  # Each slide has a title text box naming the sample added on that slide.
  # Final slide = all samples, identical to the main plot.
  if (has_officer && has_rvg) {
    output$downloadPPTX <- downloadHandler(
      filename = function()
        paste0("od_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pptx"),
      content = function(file) {
        library(officer); library(rvg)
        req(rv$data, input$selected_samples)
        samps  <- input$selected_samples
        aes_v  <- resolve_aesthetics(samps)
        pd_all <- prepare_plot_data(samples = samps)
        sw     <- max(input$export_width,  4)   # slide width  (inches)
        sh     <- max(input$export_height, 3)   # slide height (inches)
        
        # Title bar: 0.5 in tall at top of slide; plot fills the rest
        title_h  <- 0.5
        plot_top <- title_h
        plot_h   <- sh - title_h
        
        # Font size for title: scale with slide width, min 12 pt
        title_pt <- max(12, round(sw * 2.2))
        
        prs <- read_pptx()
        
        for (k in seq_along(samps)) {
          visible_samps <- samps[1:k]
          new_samp      <- samps[k]   # the sample added on this slide
          
          # Slice aesthetics to only the visible subset (preserving original colours)
          aes_k <- list(
            shapes     = aes_v$shapes[    visible_samps],
            colors     = aes_v$colors[    visible_samps],
            line_types = aes_v$line_types[visible_samps],
            leg_labels = aes_v$leg_labels[visible_samps],
            filled_map = aes_v$filled_map[visible_samps]
          )
          
          # Data: only visible samples, but axes come from pd_all limits via build_plot
          pd_k <- pd_all[pd_all$variable %in% visible_samps, , drop = FALSE]
          
          # highlight_samples = NULL → all drawn samples in full colour, none greyed
          p_k <- build_plot(pd_k, visible_samps, aes_k, highlight_samples = NULL)
          
          prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
          
          # Title text box
          prs <- ph_with(prs,
                         value    = new_samp,
                         location = ph_location(left = 0, top = 0, width = sw, height = title_h))
          
          # Plot area below the title
          prs <- ph_with(prs,
                         dml(ggobj = p_k, bg = "white"),
                         location = ph_location(left = 0, top = plot_top,
                                                width = sw, height = plot_h))
        }
        
        # Final slide: all samples, full colour, no title override
        p_final <- build_plot(pd_all, samps, aes_v, highlight_samples = NULL)
        prs <- add_slide(prs, layout = "Blank", master = "Office Theme")
        prs <- ph_with(prs,
                       dml(ggobj = p_final, bg = "white"),
                       location = ph_location(left = 0, top = 0, width = sw, height = sh))
        
        print(prs, target = file)
      }
    )
  }
  
  # ── GIF Export ───────────────────────────────────────────────────────────────
  # Cumulative build: frame k shows samples 1..k in full colour (no grey mask).
  # Axes stay consistent because pd_all establishes the full data range — only
  # the subset of plotted samples changes each frame.
  # Rendered by ggsave → gifski (no gganimate dependency).
  if (has_gifski) {
    output$downloadGIF <- downloadHandler(
      filename = function()
        paste0("od_anim_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".gif"),
      content = function(file) {
        library(gifski)
        req(rv$data, input$selected_samples)
        samps  <- input$selected_samples
        aes_v  <- resolve_aesthetics(samps)
        pd_all <- prepare_plot_data(samples = samps)
        w_px   <- max(as.integer(input$gif_width),  200L)
        h_px   <- max(as.integer(input$gif_height), 150L)
        fps    <- max(input$gif_fps, 0.2)
        
        tmp_dir <- tempfile(); dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        
        frame_files <- character(length(samps))
        for (k in seq_along(samps)) {
          visible_samps <- samps[1:k]
          
          # Slice aesthetics to only the visible subset
          aes_k <- list(
            shapes     = aes_v$shapes[    visible_samps],
            colors     = aes_v$colors[    visible_samps],
            line_types = aes_v$line_types[visible_samps],
            leg_labels = aes_v$leg_labels[visible_samps],
            filled_map = aes_v$filled_map[visible_samps]
          )
          
          # Filter data to visible samples; axes scale from pd_all range implicitly
          # via the scale limits set in build_plot using the supplied data
          pd_k <- pd_all[pd_all$variable %in% visible_samps, , drop = FALSE]
          
          p_k   <- build_plot(pd_k, visible_samps, aes_k, highlight_samples = NULL)
          fpath <- file.path(tmp_dir, sprintf("frame_%03d.png", k))
          ggsave(fpath, plot = p_k, device = "png",
                 width = w_px / 96, height = h_px / 96,
                 units = "in", dpi = 96)
          frame_files[k] <- fpath
        }
        
        gifski(frame_files, gif_file = file,
               width = w_px, height = h_px,
               delay = 1 / fps, loop = TRUE)
      }
    )
  }
  
}

shinyApp(ui, server)