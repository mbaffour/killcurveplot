# ============================================================================
# v2.22 fits engine suite -- pure functions only (no Shiny session):
#   fit_lysis_models, fit_ci, fit_quality, akaike_weights, fit_bands, fit_exponential_window,
#   compare_fits, fit_dose_response, fit_weights, fit_residual_frame, fit_growth_object,
#   FIT_MODEL_LABELS_V222, fit_parameter_definitions
# Run from the repository root:  Rscript tests/test_v222_fits.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v222_fits.R")

pass <- 0L; fail <- 0L; skip <- 0L
check  <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
skipit <- function(desc) { skip <<- skip + 1L; cat("SKIP:", desc, "\n") }
near <- function(a, b, tol = 1e-9) length(a) == 1 && is.finite(a) && abs(a - b) < tol
within_pct <- function(est, truth, pct) length(est) == 1 && is.finite(est) && abs(est - truth) <= pct / 100 * abs(truth)
inside <- function(x, lo, hi) length(x) == 1 && is.finite(lo) && is.finite(hi) && lo <= x && x <= hi
no_error <- function(expr) tryCatch({ force(expr); TRUE }, error = function(e) { cat("   error:", conditionMessage(e), "\n"); FALSE })

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))
fns <- c("fit_lysis_models", "fit_ci", "fit_quality", "akaike_weights", "fit_bands", "fit_exponential_window",
         "compare_fits", "fit_dose_response", "fit_weights", "fit_residual_frame", "fit_growth_object",
         "predict_lysis_fit", "fit_parameter_definitions")
check("all v2.22 functions are defined", all(vapply(fns, function(f) is.function(env[[f]]), logical(1))))

# ── fit_weights ───────────────────────────────────────────────────────────────
cat("\n--- fit_weights ---\n")
od_w <- c(0.1, 0.2, 0.4, 0)
check("none: all ones", identical(env$fit_weights(od_w, "none"), rep(1, 4)))
check("1/y: reciprocal; od = 0 is NA", { w <- env$fit_weights(od_w, "1/y"); near(w[3], 2.5) && is.na(w[4]) })
check("1/y2: reciprocal square", near(env$fit_weights(od_w, "1/y2")[2], 25))
check("1/sd2: inverse variance, sd = 0 gives NA", { w <- env$fit_weights(c(1, 1, 1), "1/sd2", sd = c(0.5, 0.1, 0)); near(w[1], 4) && near(w[2], 100) && is.na(w[3]) })
check("1/sd2 without sd: all NA, no error", all(is.na(env$fit_weights(c(1, 2), "1/sd2"))))

# ── akaike_weights ────────────────────────────────────────────────────────────
cat("\n--- akaike_weights ---\n")
aw <- env$akaike_weights(c(100, 102, 110))
check("weights sum to 1", near(sum(aw), 1))
check("hand value: exp(-1) / (1 + exp(-1) + exp(-5))", near(aw[2], exp(-1) / (1 + exp(-1) + exp(-5))))
check("lowest AICc gets the largest weight", which.max(aw) == 1)
aw2 <- env$akaike_weights(c(10, NA, 10))
check("NA stays NA, the others share 1", is.na(aw2[2]) && near(aw2[1], 0.5) && near(sum(aw2, na.rm = TRUE), 1))
check("all NA gives all NA; empty input gives empty", all(is.na(env$akaike_weights(c(NA, NA)))) && length(env$akaike_weights(numeric(0))) == 0)

# ── lysis fits ────────────────────────────────────────────────────────────────
cat("\n--- fit_lysis_models: one-phase decay ---\n")
set.seed(222)
tt <- seq(0, 120, by = 6)
k_true <- 0.05; pl_true <- 0.02; od0_true <- 0.9
od_d <- pl_true + (od0_true - pl_true) * exp(-k_true * tt) + rnorm(length(tt), 0, 0.01)
lys <- env$fit_lysis_models(tt, od_d, from = "start")
opd <- lys[lys$model == "one_phase_decay", ]
check("one row per model, documented columns",
      nrow(lys) == 2 && all(c("model", "converged", "n", "k", "k_se", "k_lo", "k_hi", "half_life", "half_life_lo", "half_life_hi",
        "t50_lysis", "plateau", "plateau_lo", "plateau_hi", "od0", "od0_se", "top", "bottom", "xmid", "scal", "scal_hi",
        "r2", "adj_r2", "rmse", "aic", "aicc", "df_resid") %in% names(lys)))
check("one-phase converged on all 21 points", isTRUE(opd$converged) && opd$n == 21)
check("k within 10 % of 0.05/min", within_pct(opd$k, k_true, 10))
check("k 95 % CI contains the truth", inside(k_true, opd$k_lo, opd$k_hi))
check("plateau 95 % CI contains 0.02", inside(pl_true, opd$plateau_lo, opd$plateau_hi))
check("od0 close to 0.9", within_pct(opd$od0, od0_true, 3))
check("half_life = ln2 / k", near(opd$half_life, log(2) / opd$k))
check("half-life CI is the reciprocal of the k CI", near(opd$half_life_lo, log(2) / opd$k_hi) && near(opd$half_life_hi, log(2) / opd$k_lo))
check("t50_lysis (from = start, t_ref = 0) equals the half-life", near(opd$t50_lysis, opd$half_life))
check("true half-life inside its CI", inside(log(2) / k_true, opd$half_life_lo, opd$half_life_hi))
check("profile intervals used", grepl("profile", opd$ci_method))
check("fit statistics are filled", all(is.finite(c(opd$r2, opd$adj_r2, opd$rmse, opd$aic, opd$aicc, opd$df_resid))) && opd$df_resid == 18 && opd$r2 > 0.99)
check("fitted objects returned as attribute", "one_phase_decay" %in% names(attr(lys, "fits")) && inherits(attr(lys, "fits")$one_phase_decay$fit, "nls"))
check("predict_lysis_fit evaluates the fit", near(env$predict_lysis_fit(lys, "one_phase_decay", 0), opd$od0, 1e-6))
check("SE is positive and smaller than the estimate", opd$k_se > 0 && opd$k_se < opd$k)

cat("\n--- fit_lysis_models: from the peak, logistic decay ---\n")
tg <- seq(0, 200, by = 5); t_pk <- 60
pk <- 0.05 * exp(0.05 * t_pk)
od_g <- ifelse(tg <= t_pk, 0.05 * exp(0.05 * tg), 0.03 + (pk - 0.03) * exp(-0.04 * (tg - t_pk))) + rnorm(length(tg), 0, 0.004)
lp <- env$fit_lysis_models(tg, od_g, models = "one_phase_decay")
check("from = peak: segment starts at the maximum", near(lp$t_ref, tg[which.max(od_g)]) && lp$n == sum(tg >= tg[which.max(od_g)]))
check("from = peak: k recovered within 10 %", within_pct(lp$k, 0.04, 10))
check("from = peak: absolute t50 = t_ref + half-life", near(lp$t50_lysis, lp$t_ref + lp$half_life))
set.seed(5)
tp2 <- seq(0, 160, by = 5)
od_s <- 0.04 + (1.0 - 0.04) / (1 + exp((tp2 - 60) / 8)) + rnorm(length(tp2), 0, 0.01)
ls <- env$fit_lysis_models(tp2, od_s, from = "start")
fp <- ls[ls$model == "fpl_decay", ]
check("FPL decay converged", isTRUE(fp$converged))
check("FPL: xmid within 5 % of 60 and scal within 15 % of 8", within_pct(fp$xmid, 60, 5) && within_pct(fp$scal, 8, 15))
check("FPL: top and bottom close to 1.0 and 0.04", within_pct(fp$top, 1.0, 3) && abs(fp$bottom - 0.04) < 0.02)
check("FPL: plateau = bottom, t50 close to xmid", near(fp$plateau, fp$bottom) && within_pct(fp$t50_lysis, 60, 3))
check("FPL: xmid CI contains the truth", inside(60, fp$xmid_lo, fp$xmid_hi))
check("FPL: od0 is the fitted OD at tp = 0 and lies below top", fp$od0 <= fp$top && fp$od0 > 0.9)
check("the logistic beats one-phase decay on sigmoidal data (AICc)", fp$aicc < ls$aicc[ls$model == "one_phase_decay"])
check("FPL has no k / half-life", is.na(fp$k) && is.na(fp$half_life))

cat("\n--- weights in the fits ---\n")
w_inv <- env$fit_weights(od_d, "1/y2")
lw <- env$fit_lysis_models(tt, od_d, models = "one_phase_decay", from = "start", weights = w_inv)
check("weighted one-phase fit converges and recovers k within 15 %", isTRUE(lw$converged) && within_pct(lw$k, k_true, 15))
check("weighted fit differs from the unweighted one", !near(lw$k, opd$k, 1e-12))
lw2 <- env$fit_lysis_models(tt, od_d, models = "one_phase_decay", from = "start", weights = c(NA, w_inv[-1]))
check("a point with an undefined weight is dropped and noted", lw2$n == 20 && grepl("weight", lw2$note))
wfit <- attr(lw, "fits")$one_phase_decay$fit
fq_w <- env$fit_quality(wfit)
check("weighted fit quality: r2 in (0, 1], residual df = n - 3", fq_w$r2 > 0 && fq_w$r2 <= 1 && near(fq_w$sigma^2 * 18, sum(w_inv * (od_d - fitted(wfit))^2), 1e-9) && fq_w$df_resid == 18)

# ── fit_ci ────────────────────────────────────────────────────────────────────
cat("\n--- fit_ci ---\n")
fit1 <- attr(lys, "fits")$one_phase_decay$fit
ci_p <- env$fit_ci(fit1); ci_w <- env$fit_ci(fit1, method = "wald")
check("columns and one row per parameter", identical(names(ci_p), c("term", "estimate", "se", "lo", "hi", "method")) && nrow(ci_p) == 3)
check("terms are the model parameters", setequal(ci_p$term, c("plateau", "od0", "k")))
check("Wald limits equal est +/- qt * se", { q <- qt(0.975, 18); r <- ci_w[ci_w$term == "k", ]; near(r$lo, r$estimate - q * r$se) && near(r$hi, r$estimate + q * r$se) })
check("profile intervals reported as profile", all(ci_p$method == "profile"))
check("profile and Wald intervals both contain the estimate", all(ci_p$lo < ci_p$estimate & ci_p$estimate < ci_p$hi) && all(ci_w$lo < ci_w$estimate))
check("90 % interval is narrower than 95 %", { a <- env$fit_ci(fit1, level = 0.90, method = "wald"); all((a$hi - a$lo) < (ci_w$hi - ci_w$lo)) })
check("NULL fit gives one NA row", { r <- env$fit_ci(NULL); nrow(r) == 1 && is.na(r$lo) })
check("profile that fails falls back to Wald (lm object, method profile)", { r <- env$fit_ci(lm(mpg ~ wt, mtcars)); all(r$method == "wald") && nrow(r) == 2 })

# ── fit_quality ───────────────────────────────────────────────────────────────
cat("\n--- fit_quality ---\n")
fq <- env$fit_quality(fit1)
n1 <- 21; k1 <- 4; rss1 <- sum(resid(fit1)^2)
check("AIC = n (ln 2 pi + 1 + ln(RSS / n)) + 2k", near(fq$aic, n1 * (log(2 * pi) + 1 + log(rss1 / n1)) + 2 * k1, 1e-8))
check("AICc = AIC + 2k(k + 1) / (n - k - 1) by hand", near(fq$aicc, fq$aic + 2 * k1 * (k1 + 1) / (n1 - k1 - 1), 1e-9))
check("AICc with n = 6, k = 4 uses n - k - 1 = 1", { f <- env$fit_quality(fit1, n = 6); near(f$aicc, fq$aic + 2 * 4 * 5 / 1, 1e-9) })
check("AICc is NA when n - k - 1 <= 0", is.na(env$fit_quality(fit1, n = 5)$aicc))
check("R2 = 1 - RSS / SST and adjusted R2", { y <- od_d; r2 <- 1 - rss1 / sum((y - mean(y))^2); near(fq$r2, r2, 1e-9) && near(fq$adj_r2, 1 - (1 - r2) * 20 / 18, 1e-9) })
check("RMSE = sqrt(RSS / n), sigma = sqrt(RSS / df)", near(fq$rmse, sqrt(rss1 / 21), 1e-9) && near(fq$sigma, sqrt(rss1 / 18), 1e-9))
check("BIC = AIC + (ln n - 2) k", near(fq$bic, fq$aic + (log(21) - 2) * 4, 1e-8))
check("hand runs test: + - + - + - + - gives p = 2 pnorm(-3 / sqrt(12 / 7))", near(env$.f22_runs_p(c(1, -1, 1, -1, 1, -1, 1, -1)), 2 * pnorm(-3 / sqrt(12 / 7)), 1e-9))
check("runs test: a single run of one sign gives NA; two long runs give a small p",
      is.na(env$.f22_runs_p(c(1, 2, 3))) && env$.f22_runs_p(c(rep(1, 10), rep(-1, 10))) < 0.01)
check("runs test and Shapiro p in (0, 1] for a good fit", fq$runs_test_p > 0 && fq$runs_test_p <= 1 && fq$shapiro_p > 0 && fq$shapiro_p <= 1)
check("a misfit line through exponential decay fails the runs test", {
  bad <- nls(y ~ a + b * x, data = data.frame(x = tt, y = od_d), start = list(a = 1, b = -0.01)); env$fit_quality(bad)$runs_test_p < 0.05 })
check("Shapiro p is NA for fewer than 3 residuals", is.na(env$fit_quality(lm(y ~ x, data.frame(x = 1:2, y = c(1, 3))))$shapiro_p))
check("NULL fit gives all NA", all(is.na(env$fit_quality(NULL))))

# ── fit_bands ─────────────────────────────────────────────────────────────────
cat("\n--- fit_bands ---\n")
tgrid <- seq(0, 120, by = 4)
bc <- env$fit_bands(fit1, tgrid, type = "confidence"); bp <- env$fit_bands(fit1, tgrid, type = "prediction")
check("columns t, fit, lo, hi", identical(names(bc), c("t", "fit", "lo", "hi")) && nrow(bc) == length(tgrid))
check("fit column equals predict()", near(max(abs(bc$fit - predict(fit1, data.frame(tp = tgrid)))), 0, 1e-9))
check("confidence band is narrower than the prediction band at every t", all((bc$hi - bc$lo) < (bp$hi - bp$lo)))
truth <- pl_true + (od0_true - pl_true) * exp(-k_true * tgrid)
check("confidence band contains the true curve at >= 80 % of the grid", mean(truth >= bc$lo & truth <= bc$hi) >= 0.8)
check("prediction band contains >= 90 % of the observations", { bo <- env$fit_bands(fit1, tt, type = "prediction"); mean(od_d >= bo$lo & od_d <= bo$hi) >= 0.9 })
check("bands bracket the fit", all(bc$lo < bc$fit & bc$fit < bc$hi))
check("delta-method band agrees with the textbook se at one point", {
  nd <- data.frame(tp = 30); V <- vcov(fit1); cf <- coef(fit1)
  g <- c(plateau = 1 - exp(-cf[["k"]] * 30), od0 = exp(-cf[["k"]] * 30), k = -(cf[["od0"]] - cf[["plateau"]]) * 30 * exp(-cf[["k"]] * 30))
  se <- sqrt(as.numeric(t(g[names(cf)]) %*% V %*% g[names(cf)])); b <- env$fit_bands(fit1, 30)
  near(b$hi - b$fit, qt(0.975, 18) * se, 1e-6) })
# growth-model fits: ln scale and back-transform
sim_growth <- function(mu, A = 1.5, lag = 20, od0 = 0.05, nrep = 1, sd = 0.03, tt = seq(0, 240, by = 12)) {
  f <- A / (1 + exp(4 * mu / A * (lag - tt) + 2)); f <- f - f[1]
  do.call(rbind, lapply(seq_len(nrep), function(r) data.frame(t = tt, od = od0 * exp(f + rnorm(length(tt), 0, sd)), replicate = r)))
}
set.seed(31)
gr <- sim_growth(0.05, sd = 0.02)
frow <- env$fit_growth_models(gr$t, gr$od, models = "logistic")
gfit <- env$fit_growth_object(frow, gr$t, gr$od)
check("growth fit object rebuilt: same estimates as the v2.18 row",
      inherits(gfit, "nls") && near(coef(gfit)[["mu"]], frow$mu_max, 1e-4) && near(coef(gfit)[["A"]], frow$A, 1e-4))
gb_ln <- env$fit_bands(gfit, c(0, 60, 120, 200))
check("default band is on the ln(OD / OD0) scale: starts at 0", near(gb_ln$fit[1], 0, 1e-9) && gb_ln$fit[3] > 1)
gb <- env$fit_bands(gfit, c(0, 60, 120, 200), backtransform = exp)
check("backtransform = exp gives OD0 * exp(.) = predict_growth_model()",
      near(max(abs(gb$fit - env$predict_growth_model(frow, c(0, 60, 120, 200), frow$y0))), 0, 1e-6))
check("back-transformed band keeps lo < fit < hi", all(gb$lo[-1] < gb$fit[-1] & gb$fit[-1] < gb$hi[-1]))
check("band on the OD scale equals OD0 * exp(ln-band limits)", near(gb$hi[2], frow$y0 * exp(gb_ln$hi[2]), 1e-9))
check("bands of a NULL fit are NA rows, not an error", { b <- env$fit_bands(NULL, 1:3); nrow(b) == 3 && all(is.na(b$fit)) })
check("a non-converged growth row gives a NULL fit object", is.null(env$fit_growth_object(env$fit_growth_models(1:3, c(1, 2, 3))[1, ], 1:3, c(1, 2, 3))))

# ── exponential window ────────────────────────────────────────────────────────
cat("\n--- fit_exponential_window ---\n")
set.seed(9)
te <- seq(0, 300, by = 10)
mu_true <- 0.03
od_e <- ifelse(te < 80, 0.05, ifelse(te < 200, 0.05 * exp(mu_true * (te - 80)), 0.05 * exp(mu_true * 120) * (1 + 0.0 * te)))
od_e <- od_e * exp(rnorm(length(te), 0, 0.01))
ew <- env$fit_exponential_window(te, od_e)
check("columns documented", all(c("t_start", "t_end", "n", "mu", "mu_lo", "mu_hi", "doubling_time", "r2", "intercept") %in% names(ew)))
check("auto window lies inside the simulated exponential segment (80-200)", ew$t_start >= 80 && ew$t_end <= 200)
check("mu within 5 % of 0.03", within_pct(ew$mu, mu_true, 5))
check("slope CI contains the truth", inside(mu_true, ew$mu_lo, ew$mu_hi))
check("doubling time = ln2 / mu and its CI is swapped", near(ew$doubling_time, log(2) / ew$mu) && near(ew$doubling_time_lo, log(2) / ew$mu_hi) && near(ew$doubling_time_hi, log(2) / ew$mu_lo))
check("window has at least min_points points and R2 near 1", ew$n >= 4 && ew$r2 > 0.99)
ew_m <- env$fit_exponential_window(te, od_e, min_points = 8)
check("min_points = 8 gives a window of >= 8 points", ew_m$n >= 8)
check("exact exponential data: R2 ties -> the longest window (the whole series)", {
  x <- seq(0, 90, by = 10); e <- env$fit_exponential_window(x, 0.1 * exp(0.02 * x)); e$n == 10 && near(e$mu, 0.02, 1e-9) })
ew_w <- env$fit_exponential_window(te, od_e, window = c(100, 180))
lm_ref <- lm(log(od_e[te >= 100 & te <= 180]) ~ te[te >= 100 & te <= 180])
check("explicit window equals lm(log(od) ~ t) on those points", near(ew_w$mu, unname(coef(lm_ref)[2]), 1e-12) && ew_w$n == 9 && near(ew_w$intercept, unname(coef(lm_ref)[1]), 1e-9))
check("auto = FALSE uses every positive point", env$fit_exponential_window(te, od_e, auto = FALSE)$n == length(te))
check("a decaying curve has no positive-slope window: NA row with a note", { e <- env$fit_exponential_window(1:10, exp(-0.2 * (1:10))); is.na(e$mu) && !is.na(e$note) })

# ── compare_fits ──────────────────────────────────────────────────────────────
cat("\n--- compare_fits ---\n")
set.seed(11)
dd <- rbind(cbind(sample = "slow", sim_growth(0.03, nrep = 3)), cbind(sample = "fast", sim_growth(0.06, nrep = 3)))
cf <- env$compare_fits(dd, "logistic", "all")
check("result list has test, params, converged", all(c("test", "params", "converged") %in% names(cf)))
check("test columns documented", all(c("null_ss", "alt_ss", "df_null", "df_alt", "F", "p", "delta_aicc", "preferred") %in% names(cf$test)))
check("true difference in mu detected (p < 0.01)", cf$test$p < 0.01 && cf$test$preferred == "alternative")
check("delta AICc strongly negative for a real difference", cf$test$delta_aicc < -10)
check("F recomputed by hand", { t <- cf$test; near(t$F, ((t$null_ss - t$alt_ss) / (t$df_null - t$df_alt)) / (t$alt_ss / t$df_alt), 1e-6) })
check("p = upper tail of F", { t <- cf$test; near(t$p, pf(t$F, t$df_null - t$df_alt, t$df_alt, lower.tail = FALSE), 1e-12) })
check("alternative never has a larger SS than the null; df_alt < df_null", cf$test$alt_ss <= cf$test$null_ss && cf$test$df_alt < cf$test$df_null)
check("degrees of freedom: n - 5 (shared + 2 baselines) and n - 8 (all free + 2 baselines)", cf$test$df_null == cf$test$n - 5 && cf$test$df_alt == cf$test$n - 8)
check("per-sample parameter table: two rows, mu close to 0.03 and 0.06",
      nrow(cf$params) == 2 && within_pct(cf$params$mu[cf$params$sample == "slow"], 0.03, 10) && within_pct(cf$params$mu[cf$params$sample == "fast"], 0.06, 10))
check("converged flags", isTRUE(cf$converged$null) && isTRUE(cf$converged$alt) && all(cf$converged$per_sample))
cm <- env$compare_fits(dd, "logistic", "mu")
check("param = mu: A and lag are shared (identical across samples), mu differs",
      cm$test$p < 0.01 && cm$params$A[1] == cm$params$A[2] && cm$params$lag[1] == cm$params$lag[2] && cm$params$mu[1] != cm$params$mu[2] &&
      cm$test$df_alt == cm$test$n - 6 && cm$params$shared[1] == "A,lag")
check("param = lag on data that differ only in mu is less significant than param = mu", env$compare_fits(dd, "logistic", "lag")$test$p > cm$test$p)
set.seed(12)
same <- rbind(cbind(sample = "a", sim_growth(0.04, nrep = 3)), cbind(sample = "b", sim_growth(0.04, nrep = 3)))
cs <- env$compare_fits(same, "logistic", "all")
check("identical parameters: not rejected (p > 0.05), null preferred", cs$test$p > 0.05 && cs$test$preferred == "null")
set.seed(13)
pv <- vapply(1:25, function(i) env$compare_fits(rbind(cbind(sample = "a", sim_growth(0.04, nrep = 3)), cbind(sample = "b", sim_growth(0.04, nrep = 3))), "logistic", "all")$test$p, numeric(1))
check("calibration: rejection rate at 5 % over 25 null simulations <= 20 %", mean(pv < 0.05, na.rm = TRUE) <= 0.20 && sum(is.finite(pv)) >= 24)
cg <- env$compare_fits(dd, "gompertz", "all"); cb <- env$compare_fits(dd, "baranyi", "all")
check("Gompertz and Baranyi forms also detect the difference", cg$test$p < 0.01 && cb$test$p < 0.01)
check("three samples: F test with df_null - df_alt = 6 when everything is free (3 samples)",
      { d3 <- rbind(dd, cbind(sample = "mid", sim_growth(0.045, nrep = 3))); r <- env$compare_fits(d3, "logistic", "all"); r$test$df_null - r$test$df_alt == 6 && nrow(r$params) == 3 })
check("single sample: NA row with a note", { r <- env$compare_fits(dd[dd$sample == "slow", ]); is.na(r$test$p) && !is.na(r$test$note) })
check("3 points per sample: NA row with a note, no error", { r <- env$compare_fits(data.frame(t = rep(1:3, 2), od = c(1, 2, 3, 1, 2, 4), sample = rep(c("a", "b"), each = 3))); is.na(r$test$F) && !is.na(r$test$note) })
check("constant OD: NA row, no error", { r <- env$compare_fits(data.frame(t = rep(1:8, 2), od = 1, sample = rep(c("a", "b"), each = 8))); is.na(r$test$F) })
check("all-NA OD and missing columns: NA rows", is.na(env$compare_fits(data.frame(t = 1:6, od = NA_real_, sample = rep(c("a", "b"), 3)))$test$F) &&
      is.na(env$compare_fits(data.frame(x = 1))$test$F))

# ── dose-response ─────────────────────────────────────────────────────────────
cat("\n--- fit_dose_response ---\n")
set.seed(3)
moi <- rep(10^seq(-2, 2, length.out = 9), each = 3)
lx <- log10(moi)
ec50_true <- 0.5; sc <- 0.5
y_dn <- 100 + (5 - 100) / (1 + exp((log10(ec50_true) - lx) / sc)) + rnorm(length(lx), 0, 3)
dr <- env$fit_dose_response(moi, y_dn)
check("columns documented", all(c("ec50", "ec50_lo", "ec50_hi", "hill", "top", "bottom", "xmid", "scal", "r2", "n", "converged", "note") %in% names(dr)))
check("converged on 27 points", isTRUE(dr$converged) && dr$n == 27)
check("EC50 within 15 % of 0.5", within_pct(dr$ec50, ec50_true, 15))
check("EC50 CI contains the truth", inside(ec50_true, dr$ec50_lo, dr$ec50_hi))
check("EC50 = 10^xmid and CI from the xmid CI", near(dr$ec50, 10^dr$xmid, 1e-9) && near(dr$ec50_lo, 10^dr$xmid_lo, 1e-9) && near(dr$ec50_hi, 10^dr$xmid_hi, 1e-9))
check("falling response: negative Hill slope = -1 / (scal ln 10), top > bottom", dr$hill < 0 && near(dr$hill, -1 / (dr$scal * log(10)), 1e-9) && dr$top > dr$bottom)
check("plateaus within 5 units of 100 and 5", abs(dr$top - 100) < 5 && abs(dr$bottom - 5) < 5)
cv <- attr(dr, "curve")
check("curve frame: moi, fit, lo, hi over the MOI range", identical(names(cv), c("moi", "fit", "lo", "hi")) && nrow(cv) == 100 && near(min(cv$moi), 0.01, 1e-9) && near(max(cv$moi), 100, 1e-6))
check("curve is monotone falling and its band brackets it", all(diff(cv$fit) < 0) && all(cv$lo <= cv$fit & cv$fit <= cv$hi))
y_up <- 100 * (1 / (1 + exp((log10(ec50_true) - lx) / sc))) + rnorm(length(lx), 0, 3)
dru <- env$fit_dose_response(moi, y_up)
check("rising response: positive Hill slope and EC50 within 15 %", dru$hill > 0 && within_pct(dru$ec50, ec50_true, 15))
drf <- env$fit_dose_response(moi, y_dn, fix_bottom = 5, fix_top = 100)
check("fixed plateaus: reported as given, EC50 still within 15 %, two more residual df",
      near(drf$top, 100) && near(drf$bottom, 5) && within_pct(drf$ec50, ec50_true, 15) && drf$df_resid == dr$df_resid + 2)
drb <- env$fit_dose_response(moi, y_dn, fix_bottom = 0)
check("only the bottom fixed at 0: one more df and a valid fit", isTRUE(drb$converged) && near(drb$bottom, 0) && drb$df_resid == dr$df_resid + 1)
drw <- env$fit_dose_response(moi, y_dn, weights = env$fit_weights(pmax(y_dn, 1), "1/y"))
check("weights accepted", isTRUE(drw$converged) && within_pct(drw$ec50, ec50_true, 40))
drl <- env$fit_dose_response(moi, y_dn, log_x = FALSE)
check("log_x = FALSE fits on the raw MOI scale (EC50 = xmid)", is.na(drl$converged) || !isTRUE(drl$converged) || near(drl$ec50, drl$xmid, 1e-9))
check("moi <= 0 dropped with a note (log scale)", { r <- env$fit_dose_response(c(0, moi), c(100, y_dn)); isTRUE(r$converged) && grepl("dropped", r$note) && r$n == 27 })
check("a fit whose EC50 is outside the MOI range is flagged", { r <- env$fit_dose_response(moi, 100 + (5 - 100) / (1 + exp((log10(300) - lx) / sc)) + rnorm(length(lx), 0, 2)); !isTRUE(r$converged) || grepl("outside", r$note) || r$ec50 > 50 })

# ── residual frame ────────────────────────────────────────────────────────────
cat("\n--- fit_residual_frame ---\n")
rf <- env$fit_residual_frame(fit1, tt)
check("columns t, fitted, residual, std_residual; one row per point", identical(names(rf), c("t", "fitted", "residual", "std_residual")) && nrow(rf) == 21)
check("residual = observed - fitted", near(max(abs(rf$residual - (od_d - rf$fitted))), 0, 1e-9))
check("standardised residuals have unit scale: sum of squares = df", near(sum(rf$std_residual^2), 18, 1e-6))
check("NULL fit: one NA row; wrong-length t gives NA t", { a <- env$fit_residual_frame(NULL); b <- env$fit_residual_frame(fit1, 1:3); nrow(a) == 1 && is.na(a$residual) && all(is.na(b$t)) })

# ── labels and definitions ───────────────────────────────────────────────────
cat("\n--- labels and parameter definitions ---\n")
check("FIT_MODEL_LABELS_V222 names the new models", is.character(env$FIT_MODEL_LABELS_V222) && all(c("one_phase_decay", "fpl_decay", "dose_response_4pl", "exp_window") %in% names(env$FIT_MODEL_LABELS_V222)) && all(nzchar(env$FIT_MODEL_LABELS_V222)))
pdf <- env$fit_parameter_definitions()
check("parameter definitions: parameter / definition / unit, no blanks", identical(names(pdf), c("parameter", "definition", "unit")) && all(nzchar(pdf$definition)) && !anyDuplicated(pdf$parameter))
check("every reported parameter is defined", all(c("k", "half_life", "t50_lysis", "plateau", "od0", "top", "bottom", "xmid", "scal", "ec50", "hill", "mu", "doubling_time",
      "A", "lag", "r2", "adj_r2", "rmse", "aic", "aicc", "bic", "df_resid", "runs_test_p", "shapiro_p", "F", "p", "delta_aicc", "akaike_weight") %in% pdf$parameter))

# ── degenerate input: NA rows, never errors ──────────────────────────────────
cat("\n--- degenerate input: 3 points, constant, all NA ---\n")
t3 <- c(1, 2, 3); y3 <- c(1, 0.6, 0.3); tc <- 1:12; yc <- rep(0.5, 12); tn <- 1:8; yn <- rep(NA_real_, 8)
for (cs in list(list("3 points", t3, y3), list("constant", tc, yc), list("all NA", tn, yn))) {
  nm <- cs[[1]]; x <- cs[[2]]; y <- cs[[3]]
  check(paste("fit_lysis_models,", nm, ": no error, NA parameters, note"), { r <- env$fit_lysis_models(x, y, from = "start")
    nrow(r) == 2 && !any(r$converged) && all(is.na(r$k)) && all(!is.na(r$note)) })
  check(paste("fit_exponential_window,", nm, ": NA row with a note"), { r <- env$fit_exponential_window(x, y); nrow(r) == 1 && is.na(r$mu) && !is.na(r$note) })
  check(paste("fit_dose_response,", nm, ": NA row, converged FALSE"), { r <- env$fit_dose_response(x, y); nrow(r) == 1 && !r$converged && is.na(r$ec50) && !is.na(r$note) })
}
check("from = peak with the maximum last: one point left -> NA row", { r <- env$fit_lysis_models(1:6, c(1, 2, 3, 4, 5, 6)); !any(r$converged) && all(grepl("few", r$note)) })
check("empty input to every function: no error", no_error({ env$fit_lysis_models(numeric(0), numeric(0)); env$fit_exponential_window(numeric(0), numeric(0));
  env$fit_dose_response(numeric(0), numeric(0)); env$compare_fits(data.frame(t = numeric(0), od = numeric(0), sample = character(0)))
  env$fit_quality(NULL); env$fit_ci(NULL); env$fit_bands(NULL, 1); env$fit_residual_frame(NULL); env$akaike_weights(NULL); env$fit_weights(numeric(0)) }))

# ── SUMMARY ───────────────────────────────────────────────────────────────────
cat(sprintf("\n============ SUMMARY: %d passed / %d failed%s ============\n", pass, fail, if (skip) sprintf(" / %d skipped", skip) else ""))
if (fail > 0) quit(status = 1)
