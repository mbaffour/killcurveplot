# ============================================================================
# v2.26.1: the palettes no longer repeat past 8 samples (12 for the default colours), plus the fixes from the review.
# Run from the repository root:  Rscript tests/test_v2261_fixes.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v2261_fixes.R")
Sys.setenv(CFU_PAPER_STYLE = file.path(tempdir(), "no_paper_style_here.json"))
pass <- 0L; fail <- 0L
check  <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("APP_VERSION is 2.26.1 or later", package_version(env$APP_VERSION) >= "2.26.1")

cat("\n--- palettes past their own length ---\n")
okabe <- c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
set1  <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628","#F781BF")
cust  <- c("#000000","#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#A65628","#F781BF","#999999","#66C2A5","#FC8D62","#8DA0CB")
gp <- env$get_palette_colors
check("up to 8 samples: Okabe-Ito / Set1 exactly as before", identical(gp("colorblind", 8), okabe) && identical(gp("publication", 5), okabe[1:5]) && identical(gp("set1", 8), set1))
check("up to 12 samples: the default colours exactly as before", identical(env$default_sample_colours(12), cust) && identical(env$default_sample_colours(3), cust))
for (n in c(9, 12, 16, 24, 40, 80)) {
  ok <- vapply(c("colorblind", "publication", "set1"), function(p) { x <- gp(p, n); length(x) == n && !anyDuplicated(x) && identical(x[1:8], if (p == "set1") set1 else okabe) }, NA)
  check(sprintf("%d samples: colorblind / publication / set1 give %d different colours, the first 8 unchanged", n, n), all(ok))
  d <- env$default_sample_colours(n)
  check(sprintf("%d samples: the default colours are all different", n), length(d) >= n && !anyDuplicated(d[seq_len(n)]))
}
check("the 9th Okabe-Ito colour is not black (the old repeat): purple", identical(gp("colorblind", 9)[9], "#800080"))
x <- gp("colorblind", 16)
dmin <- min(as.dist(farver::compare_colour(farver::decode_colour(x), farver::decode_colour(x), "rgb", method = "cie2000")))
check(sprintf("16 Okabe-Ito-based colours: every pair at least 15 CIEDE2000 apart (min %.1f)", dmin), dmin >= 15)
check("colours 9-16 have legend words of their own (no two of the 16 share a word)", !anyDuplicated(env$colour_words(x)))
check("the extras are valid hex colours and none is near-white", all(grepl("^#[0-9A-F]{6}$", x)) &&
        all(farver::convert_colour(farver::decode_colour(x), "rgb", "lab")[, "l"] < 92))
check("distinct_colours: n = 0 / NA / negative give no colours, not an error",
      identical(env$distinct_colours(okabe, 0), character(0)) && identical(env$distinct_colours(okabe, NA), character(0)) && identical(env$distinct_colours(okabe, -2), character(0)))
check("distinct_colours: the same answer twice (cached) and deterministic", identical(env$distinct_colours(okabe, 30), env$distinct_colours(okabe, 30)))
check("viridis / gradients unchanged", identical(gp("viridis", 4), scales::viridis_pal(option = "D")(4)) && gp("blues", 6)[1] == "#000000")

cat("\n--- the app with twelve and twenty samples ---\n")
mk <- function(nm) {
  t <- seq(0, 120, 20); set.seed(5); cols <- list(Time = t)
  for (i in seq_along(nm)) for (r in 1:2) cols[[length(cols) + 1]] <- round(0.05 * exp(0.02 * t) * (1 + i / 50) * exp(rnorm(length(t), 0, 0.02)), 4)
  d <- as.data.frame(cols); names(d) <- c("Time", rep(nm, each = 2)); d
}
r <- new.env()
testServer(app, {
  S <- session$setInputs
  ingest_data(mk(c("Control", paste("Phage", 1:11))), "twelve"); do.call(S, env$APP_DEFAULTS); S(selected_samples = rv$od_vars)
  r$start12 <- unname(resolve_aesthetics(rv$od_vars)$colors)   # the startup (journal) palette
  S(color_palette = "custom"); r$cust12 <- unname(resolve_aesthetics(rv$od_vars)$colors)
  S(color_palette = "colorblind"); r$cb12 <- unname(resolve_aesthetics(rv$od_vars)$colors)
  r$legend12 <- figure_legend_string()
  S(color_palette = "custom")
  ingest_data(mk(paste("S", 1:20)), "twenty"); S(selected_samples = rv$od_vars)
  r$cust20 <- unname(resolve_aesthetics(rv$od_vars)$colors)
  S(color_palette = "set1"); r$s120 <- unname(resolve_aesthetics(rv$od_vars)$colors)
})
check("12 samples at startup (the journal standard, Okabe-Ito): 12 different colours, the first 8 as before",
      !anyDuplicated(toupper(r$start12)) && identical(toupper(r$start12[1:8]), okabe))
check("12 samples, default colours: 12 different colours, as before", identical(toupper(r$cust12), cust))
check("12 samples, Okabe-Ito: 12 different colours, the 9th ('Phage 8') no longer black", !anyDuplicated(toupper(r$cb12)) && toupper(r$cb12[9]) != "#000000")
check("12 samples, Okabe-Ito: the legend text names Phage 8 purple", grepl("Phage 8 (purple", r$legend12, fixed = TRUE))
check("20 samples, default colours: 20 different colours", length(r$cust20) == 20 && !anyDuplicated(toupper(r$cust20)))
check("20 samples, Set1: 20 different colours", length(r$s120) == 20 && !anyDuplicated(toupper(r$s120)))

cat("\n--- review fixes: statistics and file reading ---\n")
set.seed(42); g <- rep(c("A","B","C","D"), c(3,4,5,3)); x <- rnorm(15, rep(c(1,1.5,3,1.2), c(3,4,5,3)), rep(c(.2,.5,.3,.1), c(3,4,5,3)))
sd_ <- env$posthoc_table(x, g, "sidak"); rr <- sd_[sd_$group1 == "B" & sd_$group2 == "A", ]
w <- t.test(x[g == "B"], x[g == "A"], conf.level = 0.95^(1/6))$conf.int
check("Sidak post-hoc CI at level^(1/m) (99.15% for 6 pairs), not 1 - alpha^(1/m) (39%)", abs(rr$ci_lo - w[1]) < 1e-9 && abs(rr$ci_hi - w[2]) < 1e-9)
check("Sidak CI: slightly narrower than Bonferroni's, not a fraction of it",
      { b <- env$posthoc_table(x, g, "bonferroni"); rb <- b[b$group1 == "B" & b$group2 == "A", ]; (rr$ci_hi - rr$ci_lo) <= (rb$ci_hi - rb$ci_lo) && (rr$ci_hi - rr$ci_lo) > 0.95 * (rb$ci_hi - rb$ci_lo) })
rm3 <- data.frame(sample = rep(c("WT", "Mut-1", "Phage"), each = 3), lysis_time = c(50,52,51, 60,63,61, 80,84,79))
st <- env$compare_replicate_metric(rm3, "lysis_time")
lt <- env$stats_long_table("lysis_time", list(pairwise = st$pairwise, omnibus = st$omnibus, posthoc = st$posthoc))
ph <- lt[lt$section == "posthoc", ]
check("stats CSV: Tukey rows keep both sample names (hyphenated names too), the adjusted p and the method",
      nrow(ph) == 3 && !anyNA(ph$group1) && !anyNA(ph$group2) && all(c(ph$group1, ph$group2) %in% c("WT", "Mut-1", "Phage")) &&
        !anyNA(ph$p_adj) && all(ph$method == "Tukey HSD"))
check("Tukey rows: group1 - group2 is the difference's direction",
      { r <- st$posthoc[st$posthoc$group1 == "Phage" & st$posthoc$group2 == "WT", ]; nrow(r) == 1 && abs(r$difference - (mean(c(80,84,79)) - mean(c(50,52,51)))) < 1e-9 } ||
      { r <- st$posthoc[st$posthoc$group1 == "WT" & st$posthoc$group2 == "Phage", ]; nrow(r) == 1 && abs(r$difference - (mean(c(50,52,51)) - mean(c(80,84,79)))) < 1e-9 })
check("dd/mm/yyyy across midnight, no seconds: 60 min, not a month", identical(env$parse_time_column(c("02/01/2024 23:30", "03/01/2024 00:30"))$values, c(0, 60)))
check("mm/dd/yyyy across midnight, with seconds: 60 min, not a month", identical(env$parse_time_column(c("02/01/2024 23:30:00", "02/02/2024 00:30:00"))$values, c(0, 60)))
ts <- format(seq(as.POSIXct("2024-01-12 14:00", tz = "UTC"), by = "30 min", length.out = 22), "%d/%m/%Y %H:%M")
v <- env$parse_time_column(ts)$values
check("a dd/mm run past day 12 keeps every row, half-hourly", !anyNA(v) && all(diff(v) == 30))
ts2 <- format(seq(as.POSIXct("2024-03-05 08:00", tz = "UTC"), by = "15 min", length.out = 9), "%m/%d/%Y %H:%M")
check("an unambiguous mm/dd run still reads as before", identical(env$parse_time_column(ts2)$values, seq(0, 120, 15)))
check("ISO date-times with seconds keep the seconds", identical(env$parse_time_column(c("2024-01-01 10:00:00", "2024-01-01 10:30:45"))$values, c(0, 30.75)))
t <- seq(0, 300, 10)
rr3 <- do.call(rbind, lapply(1:3, function(r) data.frame(time = t, variable = "X", replicate = r, value = c(0.500, 0.501, 0.505)[r])))
check("replicate outlier flag: three replicates within 1% are not flagged", !any(env$replicate_outlier_flags(rr3)$flagged))
bad <- rr3; bad$value[bad$replicate == 3] <- 0.9
check("replicate outlier flag: a replicate 80% off is still flagged", sum(env$replicate_outlier_flags(bad)$flagged) == 1)
t <- seq(0, 120, 10); ref <- 0.05 * exp(0.03 * pmin(t, 80))
pd <- rbind(data.frame(time = t, variable = "Ref", replicate = 1, mean_value = ref), data.frame(time = t, variable = "P", replicate = 1, mean_value = ref * 0.9),
            data.frame(time = t[t <= 40], variable = "P", replicate = 2, mean_value = (ref * 0.9)[t <= 40]))
lv <- env$calculate_local_virulence(pd, "Ref")
check("local virulence: a full curve 10% under the reference is 0.1; one that stops before t_stat is NA, not 0.87",
      abs(lv$local_virulence[lv$sample == "P" & lv$replicate == 1] - 0.1) < 1e-9 && is.na(lv$local_virulence[lv$sample == "P" & lv$replicate == 2]))
d <- data.frame(Time = seq(0, 60, 10), check.names = FALSE)
for (nm in c("WT temp 30", "Temp-sensitive mutant", "WT")) d[[nm]] <- c(.05,.08,.13,.2,.31,.45,.6)
d[["Temp"]] <- 37; d[["T° 600"]] <- 37.1; d[["Temperature [°C]"]] <- 30
ws <- env$wide_sample_columns(d, "Time")
check("columns named 'WT temp 30' / 'Temp-sensitive mutant' holding OD values are samples", all(c("WT temp 30", "Temp-sensitive mutant", "WT") %in% ws$used))
check("real temperature traces (Temp, T° 600, Temperature [°C]) are still dropped", !any(c("Temp", "T° 600", "Temperature [°C]") %in% ws$used) && sum(ws$dropped$reason == "temperature column") == 3)
set.seed(3); tmin <- seq(0, 240, 10)
curve <- function(mu) 0.05 * exp(env$gm_logistic(tmin, 3, mu, 30))
df <- do.call(rbind, lapply(1:3, function(r) rbind(data.frame(t = tmin, od = curve(0.03) * exp(rnorm(25, 0, .03)), sample = "A"),
                                                   data.frame(t = tmin, od = curve(0.02) * exp(rnorm(25, 0, .03)), sample = "B"))))
df$od[df$t > 170] <- df$od[df$t > 170] * 0.5
dfh <- df; dfh$t <- dfh$t / 60
check("compare fits: time in hours fits the same points as in minutes", identical(env$compare_fits(df, "logistic", "mu")$test$n, env$compare_fits(dfh, "logistic", "mu")$test$n))
rm2 <- data.frame(sample = rep(c("A", "B", "C"), each = 3), m = c(1, 1.1, .9, 2, 2.2, 2.1, 3, 3.3, 3.1))
r7 <- env$s22_posthoc_for_metric(rm2, "m", "dunnett", reference = "")
check("Dunnett without a reference: the note names the sample used, not 'all pairs'",
      any(grepl("'A' (the first sample) is used", r7$notes, fixed = TRUE)) && !any(grepl("all pairs are compared meanwhile", r7$notes)))

cat("\n--- review fixes: settings, sidebar, panels ---\n")
jf <- function(x) { f <- tempfile(fileext = ".json"); writeLines(x, f); f }
q <- new.env()
mk22 <- mk(paste("S", 1:22))
calls <- list()   # testServer has no browser: the update*() calls are recorded (and still sent)
for (f in c("updateTextInput", "updateNumericInput", "updateSelectInput", "updateCheckboxInput")) local({
  orig <- get(f, envir = asNamespace("shiny")); fn <- f
  assign(f, function(session, inputId, ...) { calls[[length(calls) + 1L]] <<- list(fn = fn, id = inputId, args = list(...)); orig(session, inputId, ...) }, envir = env)
})
last_update <- function(id, what) { v <- NULL; for (c0 in calls) if (identical(c0$id, id)) v <- c0$args[[what]]; v }
testServer(app, {
  S <- session$setInputs
  S(load_demo = 1); session$flushReact(); do.call(S, env$APP_DEFAULTS)
  samps <- rv$od_vars; S(selected_samples = samps); q$samps <- samps
  invisible(output$var_settings)
  id2 <- env$safe_id(samps[2])
  a <- list("dashed", "8", "custom", "#984EA3"); names(a) <- paste0(c("line_type_", "shape_", "color_selector_", "color_"), id2); do.call(S, a)
  S(selected_samples = samps[-1])
  h <- as.character(output$var_settings$html)
  q$keep_lt  <- grepl(paste0('<select id="line_type_', id2, '"[^>]*>.*?<option value="dashed" selected>'), gsub("\n", " ", h))
  q$keep_sh  <- grepl(paste0('<select id="shape_', id2, '"[^>]*>.*?<option value="8" selected>'), gsub("\n", " ", h))
  q$keep_col <- grepl(paste0('id="color_', id2, '"[^>]*value="#984EA3"'), h)
  # Clear & Reset: the default palette's colours by position, as at startup
  calls <<- list(); S(clearSettings_confirm = 1); session$flushReact()
  q$reset_cols <- vapply(samps, function(v) toupper(last_update(paste0("color_", env$safe_id(v)), "value") %||% NA_character_), "")
  q$start_cols <- toupper(env$get_palette_colors(env$APP_DEFAULTS$color_palette, length(samps)))
  # regions and markers come back from a settings file even when their lists were not drawn
  S(enable_highlighting = FALSE, region_count = 1, enable_time_markers = FALSE)
  S(loadSettings = list(name = "s.json", type = "x", size = 1, datapath = jf('{"version":"2.26.1","styling_schema":222,"global_settings":{"enable_highlighting":true,"region_count":2,"region_x_min_1":10,"region_x_max_1":20,"region_x_min_2":40,"region_x_max_2":50,"enable_time_markers":true,"marker_count":1,"marker_time_1":75},"sample_aesthetics":{}}')))
  session$flushReact(); S(enable_highlighting = TRUE, region_count = 2, enable_time_markers = TRUE, marker_count = 1)
  rh <- as.character(output$region_settings$html); mh <- as.character(output$time_marker_settings$html)
  q$reg <- grepl('id="region_x_min_2"[^>]*value="40"', rh) && grepl('id="region_x_max_1"[^>]*value="20"', rh)
  q$mark <- grepl('id="marker_time_1"[^>]*value="75"', mh)
  q$reg_status <- rv$settings_status$details %||% ""
  # a file whose sample has only shape_filled = TRUE keeps the Filled box on (no $shape partial match)
  calls <<- list()
  S(loadSettings = list(name = "s2.json", type = "x", size = 1, datapath = jf(sprintf('{"version":"2.26.1","styling_schema":222,"global_settings":{},"sample_aesthetics":{"%s":{"shape_filled":true},"<img src=x onerror=alert(1)>":{"color":"#FF0000"}}}', samps[2]))))
  session$flushReact()
  q$filled <- last_update(paste0("shape_filled_", env$safe_id(samps[2])), "value")
  q$status_html <- paste(unlist(rv$settings_status), collapse = " ")
  # 22 samples: Save Settings writes what the figure draws for samples 21-22 (no sidebar widgets on page 2)
  ingest_data(mk22, "twentytwo"); S(selected_samples = rv$od_vars); invisible(output$var_settings)
  doc <- settings_json_list(); ra <- resolve_aesthetics(rv$od_vars)
  q$save21 <- c(toupper(doc$sample_aesthetics[["S 21"]]$color), toupper(ra$colors[["S 21"]]), as.character(doc$sample_aesthetics[["S 21"]]$shape), as.character(ra$shapes[["S 21"]]))
  q$shape8 <- c(as.numeric(ra$shapes[["S 8"]]), as.numeric(input[[paste0("shape_", env$safe_id("S 8"))]] %||% NA))
})
check("redraw of the sidebar (a sample hidden) keeps another sample's line type, marker and colour", isTRUE(q$keep_lt) && isTRUE(q$keep_sh) && isTRUE(q$keep_col))
check("Clear & Reset gives each shown sample its startup colour (the default palette, by place in the file), not the red/blue cycle",
      identical(unname(q$reset_cols[2:3]), q$start_cols[2:3]))
check("regions and markers from a settings file are drawn with the file's values", isTRUE(q$reg) && isTRUE(q$mark))
check("a sample entry with only shape_filled = TRUE keeps the Filled box on", isTRUE(q$filled))
check("sample names from a settings file are escaped in the status line", !grepl("<img src=x", q$status_html, fixed = TRUE) && grepl("&lt;img", q$status_html, fixed = TRUE))
check("Save Settings: sample 21 (page 2, no widgets) is saved as drawn, not black", length(q$save21) == 4 && q$save21[1] == q$save21[2] && q$save21[1] != "#000000" && q$save21[3] == q$save21[4])
check("sample 8: the figure's default marker is the sidebar's (one cycle)", is.na(q$shape8[2]) || q$shape8[1] == q$shape8[2])
check("one default marker cycle: sample 8 is a circle, 11 a plus", env$default_sample_shape(8) == 16 && env$default_sample_shape(11) == 3 && env$default_sample_shape(13) == 16)
pm <- env$panel_membership(c("a", "b", "c"), c(a = "A", b = "Other", c = ""))
check("panels: a panel typed 'Other' plus a sample with none -> one 'Other' level, no duplicated factor level",
      identical(pm$levels, c("A", "Other")) && identical(levels(env$panel_expand(data.frame(variable = c("a", "b", "c")), pm)$group), c("A", "Other")))
pm2 <- env$panel_membership(c("x", "y", "z"), list(x = "WT; no phage", y = "MOI 10", z = c("P1", "P2")))
check("panels from a list: a name with ';' is one panel; a picked sample can be in two", identical(pm2$levels, c("WT; no phage", "MOI 10", "P1", "P2")) && identical(pm2$map$z, c("P1", "P2")))
check("panels typed 'A; B' (manual / group) still split", identical(env$panel_membership(c("x", "y"), c(x = "A; B", y = "B"))$map$x, c("A", "B")))

cat("\n", pass, "passed,", fail, "failed\n")
if (fail > 0) quit(status = 1)
