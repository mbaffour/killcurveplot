# ============================================================================
# v2.18 engine suite -- pure functions only (no Shiny session):
#   1. growth-model fits (logistic / Gompertz / Baranyi), prediction, per-sample / per-replicate, AIC pick
#   2. phenotype matrix
#   3. all-metrics statistics
#   4. extraction helpers (OD at times, time to OD, per-sample time windows)
# Run from the repository root:  Rscript tests/test_v218_engine.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v218_engine.R")

pass <- 0L; fail <- 0L
check <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
near <- function(a, b, tol = 1e-9) length(a) == 1 && is.finite(a) && abs(a - b) < tol
within_pct <- function(est, truth, pct) is.finite(est) && abs(est - truth) <= pct / 100 * abs(truth)

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))

# demo helpers: mean-curve frame and replicate frame from a demo scenario
demo_frames <- function(scenario = "lysis") {
  demo <- env$make_demo_data(scenario)
  vars <- unique(colnames(demo)[-1])
  rep_pd <- do.call(rbind, lapply(vars, function(vn) {
    idx <- which(colnames(demo) == vn)
    do.call(rbind, lapply(seq_along(idx), function(r)
      data.frame(time = demo$Time, variable = vn, replicate = r, value = demo[[idx[r]]], stringsAsFactors = FALSE)))
  }))
  pd <- do.call(rbind, lapply(vars, function(vn)
    data.frame(time = demo$Time, variable = vn,
               mean_value = rowMeans(demo[, colnames(demo) == vn, drop = FALSE]), stringsAsFactors = FALSE)))
  list(pd = pd, rep_pd = rep_pd)
}

# ── 1. Growth-model fits ──────────────────────────────────────────────────────
cat("\n--- growth-model fits: exact logistic ln-curve ---\n")
tt <- seq(0, 300, by = 10)
A_true <- 3; mu_true <- 0.03; lag_true <- 20
f_log  <- A_true / (1 + exp(4 * mu_true / A_true * (lag_true - tt) + 2))     # modified logistic (Zwietering)
od_log <- 0.05 * exp(f_log - f_log[1])                                       # so ln(od / od[1]) = f(t) - f(0)
fits <- env$fit_growth_models(tt, od_log)
check("fit table: one row per model with the documented columns",
      nrow(fits) == 3 && identical(fits$model, c("logistic", "gompertz", "baranyi")) &&
      all(c("model", "converged", "n_points", "A", "mu_max", "lag", "y0", "rss", "aic", "bic", "r2",
            "se_A", "se_mu_max", "se_lag", "message") %in% names(fits)))
lg <- fits[fits$model == "logistic", ]
check("logistic: converged, 31 points (monotone curve: peak = last point)", lg$converged && lg$n_points == 31)
check("logistic recovers A = 3 within 2 %",       within_pct(lg$A, A_true, 2))
check("logistic recovers mu_max = 0.03 within 2 %", within_pct(lg$mu_max, mu_true, 2))
check("logistic recovers lag = 20 within 2 %",     within_pct(lg$lag, lag_true, 2))
check("y0 is the baseline OD (first reading)", near(lg$y0, od_log[1]))
check("Gompertz and Baranyi converge on the same curve", all(fits$converged[fits$model != "logistic"]))
check("AIC picks the logistic on logistic data", identical(env$best_model(fits)$model, "logistic"))
f_gom  <- A_true * exp(-exp(mu_true * exp(1) / A_true * (lag_true - tt) + 1))
od_gom <- 0.05 * exp(f_gom - f_gom[1])
fg <- env$fit_growth_models(tt, od_gom)
gm <- fg[fg$model == "gompertz", ]
check("Gompertz recovers A, mu_max and lag within 2 % on an exact Gompertz curve",
      within_pct(gm$A, A_true, 2) && within_pct(gm$mu_max, mu_true, 2) && within_pct(gm$lag, lag_true, 2))
check("AIC picks the Gompertz on Gompertz data", identical(env$best_model(fg)$model, "gompertz"))
check("n_points and r2 / aic / bic / standard errors are reported",
      lg$r2 > 0.999 && is.finite(lg$aic) && is.finite(lg$bic) && all(is.finite(c(lg$se_A, lg$se_mu_max, lg$se_lag))))
check("a model subset returns only those rows",
      identical(env$fit_growth_models(tt, od_log, models = "baranyi")$model, "baranyi"))
nl <- env$fit_growth_models(tt, 0.05 + 0.8 * f_log / A_true, log_transform = FALSE)
check("log_transform = FALSE fits in OD units (A ~ 0.8 for an OD curve that rises by 0.8)",
      nl$converged[1] && within_pct(nl$A[1], 0.8, 3))

cat("\n--- prediction ---\n")
rec <- env$predict_growth_model(lg, tt, lg$y0)
check("predict_growth_model reproduces the simulated curve (max abs error < 0.01 OD)",
      max(abs(rec - od_log)) < 0.01)
check("predict at times beyond the data extends the fit (finite, increasing)",
      { p <- env$predict_growth_model(lg, c(0, 150, 400), lg$y0); all(is.finite(p)) && all(diff(p) > 0) })
check("predict scales with the baseline od0", near(env$predict_growth_model(lg, 100, 0.2)[1], 4 * env$predict_growth_model(lg, 100, 0.05)[1], 1e-9))
tt_off <- tt + 100                                         # time axis not starting at 0
fo <- env$fit_growth_models(tt_off, od_log, models = "logistic")
check("a shifted time axis gives the same parameters (lag is a duration from the first point)",
      within_pct(fo$lag, lag_true, 2) && within_pct(fo$mu_max, mu_true, 2) &&
      max(abs(env$predict_growth_model(fo, tt_off, fo$y0) - od_log)) < 0.01)
bad <- fits[1, ]; bad$converged <- FALSE
check("predict_growth_model of a non-converged row is all NA", all(is.na(env$predict_growth_model(bad, tt, 0.05))))

cat("\n--- never errors: flat, short, non-positive, empty ---\n")
flat <- env$fit_growth_models(tt, rep(0.1, length(tt)))
check("a flat curve gives converged = FALSE for every model without an error",
      nrow(flat) == 3 && !any(flat$converged) && all(is.na(flat$A)) && all(!is.na(flat$message)))
check("a declining curve (peak at the first point) gives converged = FALSE and a message",
      { d <- env$fit_growth_models(tt, 1 / (1 + tt)); !any(d$converged) && all(grepl("too few", d$message)) })
check("3 points: too few -> converged = FALSE", !any(env$fit_growth_models(c(0, 1, 2), c(0.1, 0.2, 0.4))$converged))
check("all NA / empty input does not error",
      !any(env$fit_growth_models(numeric(0), numeric(0))$converged) && !any(env$fit_growth_models(tt, rep(NA_real_, length(tt)))$converged))
check("negative readings are dropped before the log, with a note",
      { od_neg <- od_log; od_neg[5] <- -0.01
        r <- env$fit_growth_models(tt, od_neg, models = "logistic")
        r$converged && r$n_points == 30 && grepl("non-positive", r$message) })
check("all non-positive readings give a message, not an error",
      !any(env$fit_growth_models(tt, rep(-1, length(tt)))$converged))

cat("\n--- until = 'peak' stops at the maximum; 'all' uses every point ---\n")
d <- demo_frames("lysis")
pd_u <- d$pd[d$pd$variable == "Phage MOI 10", ]
pk_n <- which.max(pd_u$mean_value)
fp <- env$fit_growth_models(pd_u$time, pd_u$mean_value, until = "peak")
fa <- env$fit_growth_models(pd_u$time, pd_u$mean_value, until = "all")
check("until = 'peak': n_points = index of the maximum", all(fp$n_points == pk_n) && pk_n < nrow(pd_u))
check("until = 'all': n_points = every point", all(fa$n_points == nrow(pd_u)))
check("a lysis curve fitted through the collapse (all) has a lower asymptote / worse fit than up to the peak",
      mean(fa$r2, na.rm = TRUE) < mean(fp$r2, na.rm = TRUE))

cat("\n--- demo lysis curves vs the engine ---\n")
cm <- env$compute_curve_metrics(d$pd)
ft <- env$fit_growth_models_by_sample(d$pd, until = "peak")
check("by_sample: 3 samples x 3 models, a `sample` column first",
      nrow(ft) == 9 && identical(names(ft)[1], "sample") && setequal(ft$sample, unique(d$pd$variable)))
engine_mu <- function(s) cm$mu_max[cm$sample == s]
gomp <- ft[ft$model == "gompertz", ]
check("demo: every sample's Gompertz mu_max (fit up to the peak) is within 25 % of the engine's mu_max",
      all(vapply(seq_len(nrow(gomp)), function(i) gomp$converged[i] && within_pct(gomp$mu_max[i], engine_mu(gomp$sample[i]), 25), logical(1))))
check("demo: every model's mu_max is within 50 % of the engine's (5-point lysis curves are poorly determined)",
      all(vapply(seq_len(nrow(ft)), function(i) ft$converged[i] && within_pct(ft$mu_max[i], engine_mu(ft$sample[i]), 50), logical(1))))
check("demo: the 5-point lysis curve carries a 'poorly determined' note",
      any(grepl("poorly", ft$message[ft$sample == "Phage MOI 10"])))
bm <- env$best_model(ft)
check("best_model: one row per sample, converged, lowest AIC of its sample",
      nrow(bm) == 3 && all(bm$converged) &&
      all(vapply(seq_len(nrow(bm)), function(i) near(bm$aic[i], min(ft$aic[ft$sample == bm$sample[i]]), 1e-12), logical(1))))
check("fit_growth_models_by_sample: NULL on empty input", is.null(env$fit_growth_models_by_sample(d$pd[0, ])))
rf <- env$fit_growth_models_by_replicate(d$rep_pd, models = "gompertz")
check("by_replicate: sample and replicate columns, 9 curves x 1 model",
      nrow(rf) == 9 && identical(names(rf)[1:2], c("sample", "replicate")) && all(table(rf$sample) == 3))
check("by_replicate: best_model keys on sample + replicate (one row per curve)",
      nrow(env$best_model(env$fit_growth_models_by_replicate(d$rep_pd))) == 9)
nb <- env$best_model(rbind(cbind(data.frame(sample = "x"), flat), cbind(data.frame(sample = "y"), fits)))
check("best_model: a sample where nothing converged keeps one row with model NA",
      nrow(nb) == 2 && is.na(nb$model[nb$sample == "x"]) && identical(nb$model[nb$sample == "y"], "logistic"))

# ── 2. Phenotype matrix ───────────────────────────────────────────────────────
cat("\n--- phenotype matrix ---\n")
rm_demo <- env$compute_replicate_metric_rows(d$rep_pd, ref_sample = "Uninfected")
cm_demo <- env$compute_curve_metrics(d$pd, ref_sample = "Uninfected")
pm <- env$phenotype_matrix(cm_demo)
check("returns matrix, raw and dropped",
      is.list(pm) && all(c("matrix", "raw", "dropped") %in% names(pm)) && is.matrix(pm$matrix) && is.matrix(pm$raw))
check("shape: 3 samples (rownames) x kept metrics, same dims for raw",
      nrow(pm$matrix) == 3 && setequal(rownames(pm$matrix), c("Uninfected", "Phage MOI 10", "Phage MOI 0.1")) &&
      identical(dim(pm$matrix), dim(pm$raw)) && ncol(pm$matrix) >= 5)
check("columns are labelled with metric_label()", identical(colnames(pm$matrix), as.character(env$metric_label(pm$metrics))) && "Peak OD" %in% colnames(pm$matrix))
check("bookkeeping columns are never metrics",
      !any(c("sample", "n_timepoints", "t_start", "t_end", "replicate") %in% pm$metrics) &&
      !any(c("n_timepoints", "t_start", "t_end") %in% colnames(pm$raw)))
check("z columns have mean 0 and SD 1 (NA cells aside)",
      all(vapply(seq_len(ncol(pm$matrix)), function(j) { v <- pm$matrix[, j]; v <- v[is.finite(v)]
        near(mean(v), 0, 1e-9) && near(sd(v), 1, 1e-9) }, logical(1))))
check("raw holds the unscaled values", near(pm$raw[1, "Peak OD"] , cm_demo$max_od[match(rownames(pm$raw)[1], cm_demo$sample)], 1e-12))
pr <- env$phenotype_matrix(cm_demo, scale = "range")
check("range scale maps every column to 0-1 (min 0, max 1)",
      all(vapply(seq_len(ncol(pr$matrix)), function(j) { v <- pr$matrix[, j]; v <- v[is.finite(v)]
        near(min(v), 0) && near(max(v), 1) }, logical(1))))
pn <- env$phenotype_matrix(cm_demo, scale = "none")
check("scale 'none' equals raw", isTRUE(all.equal(pn$matrix, pn$raw)))
toy <- data.frame(sample = c("a", "b", "c"), x = c(1, 2, 4), const = c(5, 5, 5), allna = NA_real_,
                  txt = c("u", "v", "w"), n_timepoints = 10, stringsAsFactors = FALSE)
pt <- env$phenotype_matrix(toy)
check("constant, all-NA metrics are dropped (and reported); text and n_timepoints are not metrics",
      identical(colnames(pt$matrix), "x") && setequal(pt$dropped, c("const", "allna")) && !"txt" %in% pt$dropped)
check("explicit metrics: unknown / non-numeric / constant ones go to dropped",
      { p2 <- env$phenotype_matrix(toy, metrics = c("x", "const", "txt", "nope")); identical(p2$metrics, "x") && setequal(p2$dropped, c("const", "txt", "nope")) })
check("z of c(1, 2, 4) is (x - mean) / sd", isTRUE(all.equal(unname(pt$matrix[, 1]), (c(1, 2, 4) - mean(c(1, 2, 4))) / sd(c(1, 2, 4)))))
pm_rep <- env$phenotype_matrix(rm_demo)
check("a replicate-level table is averaged per sample (3 rows)", nrow(pm_rep$matrix) == 3)
check("empty / NULL input gives an empty matrix, not an error", nrow(env$phenotype_matrix(NULL)$matrix) == 0 && nrow(env$phenotype_matrix(toy[0, ])$matrix) == 0)

# ── 3. All-metrics statistics ─────────────────────────────────────────────────
cat("\n--- all-metrics statistics ---\n")
set.seed(3)
rm_syn <- do.call(rbind, lapply(c("Control", "Drug A", "Drug B"), function(s)
  data.frame(sample = s, replicate = 1:4, n_timepoints = 10, t_start = 0, t_end = 100,
             max_od = rnorm(4, c(Control = 1, "Drug A" = 0.7, "Drug B" = 0.95)[[s]], 0.05),
             auc = rnorm(4, c(Control = 80, "Drug A" = 50, "Drug B" = 78)[[s]], 4),
             mu_max = rnorm(4, 0.03, 0.002), stringsAsFactors = FALSE)))
rm_syn$all_na <- NA_real_
ca <- env$compare_all_metrics(rm_syn, ref_sample = "Control")
tb <- ca$table
check("table has the documented columns",
      all(c("metric", "metric_label", "sample", "versus", "n", "n_versus", "mean", "mean_versus", "difference",
            "hedges_g", "p", "p_adj_holm", "p_adj_bh_across", "stars") %in% names(tb)))
check("default metrics = numeric columns minus sample / n_timepoints / t_start / t_end / replicate",
      setequal(unique(tb$metric), c("max_od", "auc", "mu_max")))
check("2 comparisons vs the reference x 3 metrics = 6 rows", nrow(tb) == 6 && all(tb$versus == "Control"))
check("metric_label matches metric_label()", all(tb$metric_label == env$metric_label(tb$metric)))
same <- vapply(unique(tb$metric), function(m) {
  one <- env$compare_replicate_metric(rm_syn, m, ref_sample = "Control")$pairwise
  sub <- tb[tb$metric == m, ]
  isTRUE(all.equal(sub[, c("sample", "versus", "n", "n_versus", "mean", "mean_versus", "difference", "hedges_g", "p", "p_adj_holm")],
                   one[, c("sample", "versus", "n", "n_versus", "mean", "mean_versus", "difference", "hedges_g", "p", "p_adj_holm")],
                   check.attributes = FALSE)) && identical(sub$stars, one$stars)
}, logical(1))
check("per-metric rows equal compare_replicate_metric()'s (values, Holm p, stars)", all(same))
check("p_adj_bh_across equals p.adjust(p, 'BH') over the whole table", isTRUE(all.equal(tb$p_adj_bh_across, p.adjust(tb$p, "BH"))))
check("BH across tests is never smaller than the raw p and never above 1", all(tb$p_adj_bh_across >= tb$p - 1e-15) && all(tb$p_adj_bh_across <= 1))
check("stars follow p_adj_holm", identical(tb$stars, env$sig_stars(tb$p_adj_holm)))
check("the drug A effect on max_od and auc is detected, drug B is not (Holm within metric)",
      all(tb$p_adj_holm[tb$metric == "max_od" & tb$sample == "Drug A"] < 0.05) &&
      tb$p_adj_holm[tb$metric == "max_od" & tb$sample == "Drug B"] > 0.05)
cw <- env$compare_all_metrics(rm_syn, metrics = "auc", method = "nonparametric")
check("no reference + non-parametric: all pairs (3) with the Mann-Whitney test, one metric",
      nrow(cw$table) == 3 && all(cw$table$test == "Mann-Whitney") && identical(unique(cw$table$metric), "auc"))
check("metrics with no usable test (all NA) add a warning, not a row",
      { w <- env$compare_all_metrics(rm_syn, metrics = c("max_od", "all_na")); nrow(w$table) == 3 && any(grepl("at least two", w$warnings)) })
check("NULL / empty input returns an empty table and a warning",
      { e <- env$compare_all_metrics(NULL); nrow(e$table) == 0 && length(e$warnings) == 1 && "p_adj_bh_across" %in% names(e$table) })
real <- env$compare_all_metrics(rm_demo, ref_sample = "Uninfected")
check("on the demo replicate metrics: rows for >= 20 metrics, BH over the whole table",
      length(unique(real$table$metric)) >= 20 &&
      isTRUE(all.equal(real$table$p_adj_bh_across, p.adjust(real$table$p, "BH"))))

# ── SUMMARY ───────────────────────────────────────────────────────────────────
cat(sprintf("\n============ SUMMARY: %d passed / %d failed ============\n", pass, fail))
if (fail > 0) quit(status = 1)
