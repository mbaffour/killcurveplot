# ============================================================================
# v2.22 statistics engine suite -- pure functions only (no Shiny session):
#   column statistics, normality / variance checks, omnibus and post-hoc tests, two-way time ANOVA and
#   per-time comparisons, outlier tests, AUC, one-sample tests, correlation / regression, error measures,
#   methods sentences. Validated against base R, TukeyHSD, rstatix (when installed), analytic integrals and
#   published critical values; every function is also run on degenerate input (n = 1, constant, NA).
# Run from the repository root:  Rscript tests/test_v222_stats.R
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(ggpubr); library(scales)
  library(ggrepel); library(ggprism); library(svglite); library(jsonlite)
})
if (!file.exists("Lysis_Curve_App_v28.R")) stop("Run from the repository root: Rscript tests/test_v222_stats.R")

pass <- 0L; fail <- 0L; skip <- 0L
check  <- function(desc, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1L; cat("PASS:", desc, "\n") }
  else              { fail <<- fail + 1L; cat("FAIL:", desc, "\n") }
}
skipit <- function(desc) { skip <<- skip + 1L; cat("SKIP:", desc, "\n") }
near   <- function(a, b, tol = 1e-9) length(a) == 1 && length(b) == 1 && is.finite(a) && is.finite(b) && abs(a - b) < tol
near6  <- function(a, b) near(a, b, 1e-6)
has_pkg <- function(p) requireNamespace(p, quietly = TRUE)

env <- new.env()
app <- source("Lysis_Curve_App_v28.R", local = env)$value
check("app sources cleanly -> shiny.appobj", inherits(app, "shiny.appobj"))

# demo helpers: replicate frame (the app's own shape: time, variable, replicate, value)
demo <- env$make_demo_data("lysis"); vars <- unique(colnames(demo)[-1])
rep_pd <- do.call(rbind, lapply(vars, function(vn) {
  idx <- which(colnames(demo) == vn)
  do.call(rbind, lapply(seq_along(idx), function(r)
    data.frame(time = demo$Time, variable = vn, replicate = r, value = demo[[idx[r]]], stringsAsFactors = FALSE)))
}))
rr <- env$compute_replicate_metric_rows(rep_pd)
ref_name <- "Uninfected"

# ── 1. describe_column / describe_metrics ─────────────────────────────────────
cat("\n--- describe_column ---\n")
x <- c(2.1, 3.4, 2.9, 4.8, 3.3, 3.9)
d <- env$describe_column(x)
tci <- t.test(x)$conf.int
check("describe_column: n, mean, sd, sem match base R",
      d$n == 6 && near(d$mean, mean(x)) && near(d$sd, sd(x)) && near(d$sem, sd(x) / sqrt(6)))
check("describe_column: cv_pct = 100 sd / mean", near(d$cv_pct, 100 * sd(x) / mean(x)))
check("describe_column: t-based 95% CI equals t.test", near(d$ci_lo, tci[1]) && near(d$ci_hi, tci[2]))
check("describe_column: level = 0.9 changes the CI",
      { d9 <- env$describe_column(x, 0.9); near(d9$ci_lo, t.test(x, conf.level = 0.9)$conf.int[1]) })
check("describe_column: median, q1, q3 (type 7), iqr, min, max, range",
      near(d$median, median(x)) && near(d$q1, quantile(x, .25, type = 7, names = FALSE)) &&
      near(d$q3, quantile(x, .75, type = 7, names = FALSE)) && near(d$iqr, IQR(x, type = 7)) &&
      d$min == 2.1 && d$max == 4.8 && near(d$range, 2.7))
n <- length(x); s <- sd(x); m <- mean(x)
G1 <- n / ((n - 1) * (n - 2)) * sum(((x - m) / s)^3)
G2 <- n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sum(((x - m) / s)^4) - 3 * (n - 1)^2 / ((n - 2) * (n - 3))
check("describe_column: skewness equals the bias-corrected G1 (independent sum form)", near(d$skewness, G1, 1e-9))
check("describe_column: excess kurtosis equals the bias-corrected G2 (independent sum form)", near(d$kurtosis, G2, 1e-9))
check("describe_column: geo_mean = exp(mean(log x)); shapiro_p equals shapiro.test",
      near(d$geo_mean, exp(mean(log(x)))) && near(d$shapiro_p, shapiro.test(x)$p.value))
check("describe_column: geo_mean NA when any value <= 0", is.na(env$describe_column(c(0, 1, 2))$geo_mean))
check("describe_column: n = 3 -> skewness defined, kurtosis NA, shapiro defined",
      { z <- env$describe_column(c(1, 2, 4)); !is.na(z$skewness) && is.na(z$kurtosis) && is.finite(z$shapiro_p) })
check("describe_column: n = 2 -> shapiro_p and skewness NA, sd defined",
      { z <- env$describe_column(c(1, 2)); is.na(z$shapiro_p) && is.na(z$skewness) && is.finite(z$sd) })
check("describe_column: n = 1 -> one row, mean defined, spread NA, note",
      { z <- env$describe_column(5); nrow(z) == 1 && z$mean == 5 && is.na(z$sd) && is.na(z$ci_lo) && nzchar(z$note) })
check("describe_column: constant values -> sd 0, skewness NA, no error",
      { z <- env$describe_column(c(2, 2, 2)); z$sd == 0 && is.na(z$skewness) && is.na(z$shapiro_p) })
check("describe_column: NA / non-finite values dropped; all-NA -> n 0, NA row",
      { z <- env$describe_column(c(1, NA, 3, Inf)); a <- env$describe_column(c(NA, NA)); z$n == 2 && a$n == 0 && is.na(a$mean) && nzchar(a$note) })
check("describe_column: zero-length and NULL input do not error",
      env$describe_column(numeric(0))$n == 0 && env$describe_column(NULL)$n == 0)

cat("\n--- describe_metrics ---\n")
dm <- env$describe_metrics(rr, c("auc", "max_od"))
check("describe_metrics: one row per sample x metric, columns sample / metric / describe",
      nrow(dm) == 6 && all(c("sample", "metric", "n", "mean", "sd", "sem", "ci_lo", "shapiro_p") %in% names(dm)))
check("describe_metrics: mean equals the per-sample mean of the replicate rows",
      near(dm$mean[dm$sample == "Phage MOI 10" & dm$metric == "auc"], mean(rr$auc[rr$sample == "Phage MOI 10"])))
check("describe_metrics: metrics = NULL uses all numeric metric columns (not replicate)",
      { a <- env$describe_metrics(rr); !"replicate" %in% a$metric && "auc" %in% a$metric && "mu_max" %in% a$metric })
check("describe_metrics: NULL / empty / unknown metric give an empty table, not an error",
      nrow(env$describe_metrics(NULL)) == 0 && nrow(env$describe_metrics(rr[0, ])) == 0 && nrow(env$describe_metrics(rr, "nope")) == 0)

# ── 2. Normality ──────────────────────────────────────────────────────────────
cat("\n--- normality_table / qq_frame ---\n")
nt <- env$normality_table(rr, c("auc", "max_od"))
check("normality_table: one row per sample x metric plus a pooled-residual row per metric", nrow(nt) == 2 * (3 + 1))
check("normality_table: Shapiro p equals shapiro.test per sample",
      near(nt$shapiro_p[nt$sample == "Phage MOI 10" & nt$metric == "auc"], shapiro.test(rr$auc[rr$sample == "Phage MOI 10"])$p.value))
res_auc <- unlist(lapply(split(rr$auc, rr$sample), function(v) v - mean(v)))
check("normality_table: pooled residual Shapiro equals shapiro.test of the within-sample residuals",
      near(nt$shapiro_p[nt$sample == "(pooled residuals)" & nt$metric == "auc"], shapiro.test(res_auc)$p.value))
check("normality_table: n = 3 carries a low-power note",
      all(grepl("no power", nt$note[nt$sample != "(pooled residuals)"])))
check("normality_table: single replicate / constant / NULL -> NA p, no error",
      { one <- data.frame(sample = "a", replicate = 1, auc = 1); k <- data.frame(sample = "a", replicate = 1:3, auc = 2)
        is.na(env$normality_table(one, "auc")$shapiro_p[1]) && is.na(env$normality_table(k, "auc")$shapiro_p[1]) && nrow(env$normality_table(NULL)) == 0 })
qf <- env$qq_frame(c(3, 1, 2, 5, 4)); qn <- qqnorm(c(3, 1, 2, 5, 4), plot.it = FALSE)
check("qq_frame: theoretical quantiles are ppoints normal quantiles, sample is sorted",
      identical(names(qf), c("theoretical", "sample")) && all(abs(qf$theoretical - qnorm(ppoints(5))) < 1e-12) && !is.unsorted(qf$sample))
check("qq_frame: agrees with qqnorm for the same points", all(abs(sort(qn$x) - qf$theoretical) < 1e-12))
check("qq_frame: empty / NA input gives zero rows", nrow(env$qq_frame(numeric(0))) == 0 && nrow(env$qq_frame(c(NA, NA))) == 0)

# ── 3. Variance check ─────────────────────────────────────────────────────────
cat("\n--- variance_check ---\n")
vv <- c(1.1, 1.3, 1.2, 2.0, 2.6, 3.1, 5, 5.2, 4.9, 4.6); gg <- rep(c("a", "b", "c", "d"), c(3, 3, 2, 2))
vc <- env$variance_check(vv, gg)
check("variance_check: two rows, Brown-Forsythe and Bartlett", nrow(vc) == 2 && grepl("Brown", vc$test[1]) && vc$test[2] == "Bartlett")
check("variance_check: Bartlett equals bartlett.test", near(vc$p[2], bartlett.test(vv, factor(gg))$p.value) && near(vc$statistic[2], unname(bartlett.test(vv, factor(gg))$statistic)))
if (has_pkg("car")) {
  lv <- car::leveneTest(vv, factor(gg), center = median)
  check("variance_check: Brown-Forsythe equals car::leveneTest(center = median)",
        near(vc$statistic[1], lv[["F value"]][1]) && near(vc$p[1], lv[["Pr(>F)"]][1]) && vc$df1[1] == 3 && vc$df2[1] == 6)
} else skipit("car not installed: leveneTest comparison")
check("variance_check: hand formula anova(lm(|x - median| ~ g))",
      { dv <- abs(vv - ave(vv, gg, FUN = median)); a <- anova(lm(dv ~ factor(gg))); near(vc$p[1], a[["Pr(>F)"]][1]) })
check("variance_check: constant data / one group / NA -> NA rows with notes, no error",
      { a <- env$variance_check(rep(1, 6), rep(c("a", "b"), 3)); b <- env$variance_check(1:4, rep("a", 4)); c0 <- env$variance_check(c(NA, NA), c("a", "b"))
        all(is.na(a$p)) && all(nzchar(a$note)) && all(is.na(b$p)) && all(is.na(c0$p)) })
check("variance_check: a zero-variance group makes Bartlett NA but Brown-Forsythe may still run",
      { z <- env$variance_check(c(1, 1, 1, 2, 3, 4), rep(c("a", "b"), each = 3)); is.na(z$p[2]) && nzchar(z$note[2]) })

# ── 4. Omnibus and post-hoc ───────────────────────────────────────────────────
cat("\n--- omnibus_table ---\n")
xs <- c(1, 2, 3, 2.5, 4, 5, 6.1, 7, 9, 8.5, 3.3, 4.4); gs <- rep(c("a", "b", "c", "d"), each = 3)
om <- env$omnibus_table(xs, gs)
fit <- aov(xs ~ factor(gs)); sm <- summary(fit)[[1]]; ow <- oneway.test(xs ~ factor(gs)); kw <- kruskal.test(xs ~ factor(gs))
check("omnibus_table: three rows (ANOVA, Welch ANOVA, Kruskal-Wallis)", identical(om$test, c("One-way ANOVA", "Welch ANOVA", "Kruskal-Wallis")))
check("omnibus_table: ANOVA F, df and p equal aov", near(om$statistic[1], sm[["F value"]][1]) && om$df1[1] == 3 && om$df2[1] == 8 && near(om$p[1], sm[["Pr(>F)"]][1]))
check("omnibus_table: eta^2 = SS between / SS total", near(om$eta_sq[1], sm[["Sum Sq"]][1] / sum(sm[["Sum Sq"]])))
check("omnibus_table: Welch ANOVA equals oneway.test", near(om$statistic[2], unname(ow$statistic)) && near(om$df2[2], unname(ow$parameter[2])) && near(om$p[2], ow$p.value))
check("omnibus_table: Kruskal-Wallis H and p equal kruskal.test; epsilon^2 = H / (N - 1)",
      near(om$statistic[3], unname(kw$statistic)) && near(om$p[3], kw$p.value) && near(om$epsilon_sq[3], unname(kw$statistic) / 11))
check("omnibus_table: constant values -> NA rows with notes, no error",
      { z <- env$omnibus_table(rep(3, 6), rep(c("a", "b"), 3)); all(is.na(z$p[1:2])) && all(nzchar(z$note)) })
check("omnibus_table: one group / NA -> NA rows",
      all(is.na(env$omnibus_table(1:3, rep("a", 3))$p)) && all(is.na(env$omnibus_table(c(NA, NA), c("a", "b"))$p)))
check("omnibus_table: singleton groups are excluded and said so",
      { z <- env$omnibus_table(c(1, 2, 3, 4, 5, 6, 9), c("a", "a", "a", "b", "b", "b", "c")); is.finite(z$p[1]) && grepl("excluded", z$note[1]) })

cat("\n--- posthoc_table: Tukey ---\n")
ph_cols <- c("group1", "group2", "estimate", "ci_lo", "ci_hi", "statistic", "p_raw", "p_adj", "method", "stars")
tk <- env$posthoc_table(xs, gs, "tukey")
check("posthoc_table: documented columns present", all(ph_cols %in% names(tk)))
tkb <- TukeyHSD(aov(xs ~ factor(gs)))[[1]]
key <- function(a, b) paste(b, a, sep = "-")
tk_ok <- all(vapply(seq_len(nrow(tk)), function(i) {
  rn <- paste(tk$group1[i], tk$group2[i], sep = "-"); j <- match(rn, rownames(tkb))
  !is.na(j) && near(tk$estimate[i], tkb[j, "diff"]) && near(tk$ci_lo[i], tkb[j, "lwr"]) && near(tk$ci_hi[i], tkb[j, "upr"]) && near(tk$p_adj[i], tkb[j, "p adj"])
}, logical(1)))
check("posthoc_table Tukey: 6 rows, diff / CI / p adj equal TukeyHSD", nrow(tk) == 6 && tk_ok)
check("posthoc_table Tukey: hand values from a verified TukeyHSD run (b-a: 1.833333, [-2.7511156, 6.417782], p 0.5985128)",
      { r <- tk[tk$group1 == "b" & tk$group2 == "a", ]; near6(r$estimate, 1.833333333) && near6(r$ci_lo, -2.7511156) && near6(r$ci_hi, 6.417782) && near6(r$p_adj, 0.5985128) })
check("posthoc_table Tukey: stars come from sig_stars(p_adj)", identical(tk$stars, env$sig_stars(tk$p_adj)))
check("posthoc_table Tukey: level = 0.99 widens the CI",
      { t99 <- env$posthoc_table(xs, gs, "tukey", level = 0.99); all(t99$ci_hi - t99$ci_lo > tk$ci_hi - tk$ci_lo) })
hy <- env$posthoc_table(xs, c("x-1", "x-2", "x-3", "y-1")[rep(1:4, each = 3)], "tukey")
check("posthoc_table Tukey: group names containing hyphens survive", all(c("x-1", "x-2", "x-3", "y-1") %in% c(hy$group1, hy$group2)) && nrow(hy) == 6)
tkr <- env$posthoc_table(xs, gs, "tukey", reference = "a")
check("posthoc_table Tukey with a reference: only pairs with the reference (family stays all-pairs)", nrow(tkr) == 3 && all(c(tkr$group1, tkr$group2) %in% c("a", "b", "c", "d")) && all(tkr$group2 == "a" | tkr$group1 == "a"))
check("posthoc_table: unbalanced groups give Tukey-Kramer equal to TukeyHSD",
      { xu <- c(1, 2, 3, 4, 6, 7, 8, 9, 3.5, 4.5); gu <- c("a", "a", "a", "b", "b", "b", "b", "c", "c", "c"); t1 <- env$posthoc_table(xu, gu, "tukey"); t2 <- TukeyHSD(aov(xu ~ factor(gu)))[[1]]
        near(t1$p_adj[t1$group1 == "c" & t1$group2 == "b"], t2["c-b", "p adj"]) })

cat("\n--- posthoc_table: Games-Howell / Dunn ---\n")
gh <- env$posthoc_table(xs, gs, "games_howell")
check("posthoc_table Games-Howell: hand values from a verified rstatix run (a-b: 1.833333, [-2.041884, 5.708550], p 0.33462943)",
      { r <- gh[gh$group1 == "b" & gh$group2 == "a", ]; near6(r$estimate, 1.833333333) && near(r$ci_lo, -2.041884, 1e-5) && near(r$ci_hi, 5.708550, 1e-5) && near(r$p_adj, 0.33462943, 1e-7) })
check("posthoc_table Games-Howell: independent formula (Welch df, ptukey) on pair c-a",
      { a <- xs[gs == "a"]; cc <- xs[gs == "c"]; va <- var(a) / 3; vc_ <- var(cc) / 3; tt <- abs(mean(cc) - mean(a)) / sqrt(va + vc_)
        df <- (va + vc_)^2 / (va^2 / 2 + vc_^2 / 2); p <- ptukey(tt * sqrt(2), 4, df, lower.tail = FALSE)
        r <- gh[gh$group1 == "c" & gh$group2 == "a", ]; near(r$p_adj, p) && near(r$statistic, (mean(cc) - mean(a)) / sqrt(va + vc_)) })
if (has_pkg("rstatix")) {
  rg <- as.data.frame(rstatix::games_howell_test(data.frame(x = xs, g = gs), x ~ g))
  ok <- all(vapply(seq_len(nrow(rg)), function(i) { r <- gh[gh$group1 == rg$group2[i] & gh$group2 == rg$group1[i], ]
    nrow(r) == 1 && near6(r$estimate, rg$estimate[i]) && near6(r$ci_lo, rg$conf.low[i]) && near6(r$ci_hi, rg$conf.high[i]) && near6(r$p_adj, rg$p.adj[i]) }, logical(1)))
  check("posthoc_table Games-Howell equals rstatix::games_howell_test within 1e-6 (estimate, CI, p.adj)", nrow(rg) == 6 && ok)
} else skipit("rstatix not installed: Games-Howell comparison")
dn <- env$posthoc_table(xs, gs, "dunn")
check("posthoc_table Dunn: hand values from a verified rstatix run (a-b: z 1.1322770, p.adj 0.77255395)",
      { r <- dn[dn$group1 == "b" & dn$group2 == "a", ]; near(r$statistic, 1.1322770, 1e-6) && near(r$p_adj, 0.77255395, 1e-7) && near(r$p_raw, 0.257517982, 1e-7) })
if (has_pkg("rstatix")) {
  rd <- as.data.frame(rstatix::dunn_test(data.frame(x = xs, g = gs), x ~ g, p.adjust.method = "holm"))
  ok <- all(vapply(seq_len(nrow(rd)), function(i) { r <- dn[dn$group1 == rd$group2[i] & dn$group2 == rd$group1[i], ]
    nrow(r) == 1 && near6(abs(r$statistic), abs(rd$statistic[i])) && near6(r$p_raw, rd$p[i]) && near6(r$p_adj, rd$p.adj[i]) }, logical(1)))
  check("posthoc_table Dunn equals rstatix::dunn_test (holm) within 1e-6", nrow(rd) == 6 && ok)
  xt <- c(1, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 8); rt <- as.data.frame(rstatix::dunn_test(data.frame(x = xt, g = gs), x ~ g, p.adjust.method = "bonferroni"))
  dt <- env$posthoc_table(xt, gs, "dunn", dunn_adjust = "bonferroni")
  ok <- all(vapply(seq_len(nrow(rt)), function(i) { r <- dt[dt$group1 == rt$group2[i] & dt$group2 == rt$group1[i], ]; near6(r$p_adj, rt$p.adj[i]) && near6(abs(r$statistic), abs(rt$statistic[i])) }, logical(1)))
  check("posthoc_table Dunn with ties and Bonferroni equals rstatix (tie correction)", ok)
} else { skipit("rstatix not installed: Dunn comparison"); skipit("rstatix not installed: Dunn tie-correction comparison") }
check("posthoc_table Dunn: estimate is the mean-rank difference, CI NA",
      { rk <- rank(xs); r <- dn[dn$group1 == "c" & dn$group2 == "a", ]; near(r$estimate, mean(rk[gs == "c"]) - mean(rk[gs == "a"])) && is.na(r$ci_lo) })

cat("\n--- posthoc_table: Welch families, Dunnett ---\n")
hw <- env$posthoc_table(xs, gs, "holm_welch"); tt_ca <- t.test(xs[gs == "c"], xs[gs == "a"])
check("posthoc_table holm_welch: p_raw equals t.test p and CI equals t.test CI",
      { r <- hw[hw$group1 == "c" & hw$group2 == "a", ]; near(r$p_raw, tt_ca$p.value) && near(r$ci_lo, tt_ca$conf.int[1]) && near(r$ci_hi, tt_ca$conf.int[2]) && near(r$statistic, unname(tt_ca$statistic)) })
check("posthoc_table holm_welch: p_adj = p.adjust(p_raw, holm)", all(abs(hw$p_adj - p.adjust(hw$p_raw, "holm")) < 1e-12))
bf <- env$posthoc_table(xs, gs, "bonferroni"); sd_ <- env$posthoc_table(xs, gs, "sidak")
check("posthoc_table bonferroni / sidak: p_adj = min(1, m p) and 1 - (1 - p)^m",
      all(abs(bf$p_adj - pmin(1, 6 * bf$p_raw)) < 1e-12) && all(abs(sd_$p_adj - (1 - (1 - sd_$p_raw)^6)) < 1e-12))
check("posthoc_table bonferroni: CI widened to 1 - 0.05 / m",
      { r <- bf[bf$group1 == "c" & bf$group2 == "a", ]; w <- t.test(xs[gs == "c"], xs[gs == "a"], conf.level = 1 - 0.05 / 6)$conf.int; near(r$ci_lo, w[1]) && near(r$ci_hi, w[2]) })
check("posthoc_table sidak: CI at 1 - (1 - level)^(1/m)",
      { r <- sd_[sd_$group1 == "c" & sd_$group2 == "a", ]; w <- t.test(xs[gs == "c"], xs[gs == "a"], conf.level = 1 - 0.05^(1 / 6))$conf.int; near(r$ci_lo, w[1]) })
dnt <- env$posthoc_table(xs, gs, "dunnett", reference = "a")
if (has_pkg("mvtnorm")) {
  check("posthoc_table dunnett (mvtnorm available): 3 rows vs the reference, multivariate-t method label",
        nrow(dnt) == 3 && all(dnt$group2 == "a") && grepl("Dunnett", dnt$method[1]) && !grepl("fallback", dnt$method[1]))
  check("posthoc_table dunnett (mvtnorm): adjusted p >= unadjusted pooled-t p",
        { mse <- sum(resid(aov(xs ~ factor(gs)))^2) / 8; tt <- (mean(xs[gs == "b"]) - mean(xs[gs == "a"])) / sqrt(mse * 2 / 3); r <- dnt[dnt$group1 == "b", ]; r$p_adj >= 2 * pt(-abs(tt), 8) - 1e-9 })
} else {
  skipit("mvtnorm not installed: multivariate-t Dunnett branch (code path is guarded and falls back)")
  check("posthoc_table dunnett without mvtnorm: Holm-adjusted Welch t vs the reference, and the method column says so",
        nrow(dnt) == 3 && all(dnt$group2 == "a") && all(grepl("fallback", dnt$method)) && all(grepl("mvtnorm not installed", dnt$method)) &&
        all(abs(dnt$p_adj - p.adjust(dnt$p_raw, "holm")) < 1e-12))
}
check("posthoc_table dunnett without a reference uses the first group and notes it",
      { z <- env$posthoc_table(xs, gs, "dunnett"); all(z$group2 == "a") && grepl("no reference", z$note[1]) })
check("posthoc_table: a reference vs all others gives n - 1 rows, group1 = other sample, estimate = other - reference",
      { z <- env$posthoc_table(xs, gs, "holm_welch", reference = "b"); nrow(z) == 3 && all(z$group2 == "b") && near(z$estimate[z$group1 == "a"], mean(xs[gs == "a"]) - mean(xs[gs == "b"])) })
check("posthoc_table: unknown reference falls back to all pairs with a note",
      { z <- env$posthoc_table(xs, gs, "holm_welch", reference = "zzz"); nrow(z) == 6 && grepl("reference 'zzz'", z$note[1]) })
check("posthoc_table: n < 3 is flagged as low power", { z <- env$posthoc_table(c(1, 2, 3, 4, 5, 7), rep(c("a", "b"), each = 3)); grepl("n < 3", env$posthoc_table(c(1, 2, 4, 5), rep(c("a", "b"), each = 2), "holm_welch")$note[1]) && nrow(z) == 1 })
check("posthoc_table: attribute n carries the n per group", identical(unname(attr(tk, "n")), rep(3L, 4)))
check("posthoc_table: a singleton group is excluded with a note",
      { z <- env$posthoc_table(c(1, 2, 3, 4, 5, 6, 9), c("a", "a", "a", "b", "b", "b", "c"), "holm_welch"); nrow(z) == 1 && grepl("excluded", z$note[1]) && !"c" %in% c(z$group1, z$group2) })
for (mth in c("tukey", "holm_welch", "games_howell", "dunn", "dunnett", "bonferroni", "sidak")) {
  check(paste0("posthoc_table ", mth, ": constant / single group / NA / empty input -> NA row, no error"),
        { a <- env$posthoc_table(rep(2, 6), rep(c("a", "b"), 3), mth); b <- env$posthoc_table(1:3, rep("a", 3), mth)
          c0 <- env$posthoc_table(c(NA, NA), c("a", "b"), mth); e0 <- env$posthoc_table(numeric(0), character(0), mth)
          all(is.na(a$p_adj)) && all(is.na(b$p_adj)) && all(is.na(c0$p_adj)) && all(is.na(e0$p_adj)) && all(nzchar(c(a$note, b$note))) })
}

# ── 5. Two-way time ANOVA and per-time comparisons ────────────────────────────
cat("\n--- two_way_time_anova / per_time_comparisons ---\n")
tw <- env$two_way_time_anova(rep_pd)
check("two_way_time_anova: list(anova, method, note, model_used) with terms Sample / Time / Sample x Time",
      is.list(tw) && identical(names(tw), c("anova", "method", "note", "model_used")) && identical(tw$anova$term, c("Sample", "Time", "Sample x Time")) &&
      identical(names(tw$anova), c("term", "df_num", "df_den", "F", "p")))
check("two_way_time_anova on the demo data: sample x time interaction p < 0.05", is.finite(tw$anova$p[3]) && tw$anova$p[3] < 0.05)
check("two_way_time_anova: interaction df = (k - 1)(T - 1) = 2 x 12 = 24", tw$anova$df_num[3] == 24 && tw$anova$df_num[1] == 2 && tw$anova$df_num[2] == 12)
if (has_pkg("nlme")) {
  check("two_way_time_anova: uses the nlme mixed model with AR(1) when nlme is available", grepl("nlme", tw$method) && grepl("AR\\(1\\)", tw$method))
} else skipit("nlme not installed: mixed-model path")
tw_ex <- env$two_way_time_anova(transform(rep_pd, sample = variable, od = value))
check("two_way_time_anova: accepts explicit sample / od columns as well as variable / value", near(tw_ex$anova$F[3], tw$anova$F[3], 1e-6))
tw_nr <- env$two_way_time_anova(rep_pd, rm = FALSE)
check("two_way_time_anova rm = FALSE: plain two-way aov, method says it ignores repeated measures",
      grepl("ignores", tw_nr$method) && is.finite(tw_nr$anova$p[3]) && tw_nr$anova$df_den[3] == 78)
lg <- rep_pd; lg$y <- log(lg$value); lg$tf <- factor(lg$time); lg$s <- factor(lg$variable); pa <- anova(lm(y ~ s * tf, lg))
check("two_way_time_anova rm = FALSE: F equals anova(lm(log y ~ sample * factor(time)))", near(tw_nr$anova$F[3], pa[["F value"]][3], 1e-8) && near(tw_nr$anova$F[1], pa[["F value"]][1], 1e-8))
tw_lin <- env$two_way_time_anova(rep_pd, log = FALSE, rm = FALSE); pl <- anova(lm(value ~ s * tf, lg))
check("two_way_time_anova log = FALSE analyses the raw readings", near(tw_lin$anova$F[3], pl[["F value"]][3], 1e-8))
nz <- rep_pd; nz$value[1] <- 0
check("two_way_time_anova: a non-positive reading is dropped before the log and noted", grepl("<= 0", env$two_way_time_anova(nz)$note))
# the optional packages masked: the Error(well) and plain-aov fallbacks must still run (the app must work without nlme)
env$requireNamespace <- function(package, ..., quietly = FALSE) FALSE
tw_aov <- env$two_way_time_anova(rep_pd)
dn_nopkg <- env$posthoc_table(xs, gs, "dunnett", reference = "a")
rm("requireNamespace", envir = env)
check("two_way_time_anova without nlme: falls back to aov Error(well) and says so",
      grepl("Error\\(well\\)", tw_aov$method) && is.finite(tw_aov$anova$p[3]) && tw_aov$anova$p[3] < 0.05 && grepl("nlme not installed", tw_aov$note))
check("two_way_time_anova Error(well): Sample is tested against the between-well stratum (df_den = 6), Time and interaction within (72)",
      tw_aov$anova$df_den[1] == 6 && tw_aov$anova$df_den[2] == 72 && tw_aov$anova$df_den[3] == 72)
check("posthoc_table dunnett with every optional package masked still returns a labelled fallback", all(grepl("fallback", dn_nopkg$method)) && nrow(dn_nopkg) == 3)
check("two_way_time_anova: one sample / one time / empty / constant input -> NA table with a note, no error",
      { a <- env$two_way_time_anova(rep_pd[rep_pd$variable == "Uninfected", ]); b <- env$two_way_time_anova(rep_pd[rep_pd$time == 0, ])
        c0 <- env$two_way_time_anova(NULL); d0 <- env$two_way_time_anova(rep_pd[0, ]); k <- rep_pd; k$value <- 0.5; e0 <- env$two_way_time_anova(k)
        all(is.na(a$anova$p)) && nzchar(a$note) && all(is.na(b$anova$p)) && all(is.na(c0$anova$p)) && all(is.na(d0$anova$p)) && is.list(e0) })
check("two_way_time_anova: a single replicate per cell cannot test the interaction",
      all(is.na(env$two_way_time_anova(rep_pd[rep_pd$replicate == 1, ])$anova$p)))
ptc <- env$per_time_comparisons(rep_pd, ref_name)
check("per_time_comparisons: one row per time per non-reference sample (13 x 2 = 26)",
      nrow(ptc) == 26 && !ref_name %in% ptc$sample && all(c("time", "sample", "diff", "ci_lo", "ci_hi", "t", "df", "p_raw", "p_adj", "stars") %in% names(ptc)))
r0 <- ptc[ptc$sample == "Phage MOI 10" & ptc$time == 90, ]; tt0 <- t.test(rep_pd$value[rep_pd$variable == "Phage MOI 10" & rep_pd$time == 90], rep_pd$value[rep_pd$variable == ref_name & rep_pd$time == 90])
check("per_time_comparisons: diff, CI, t, df and p_raw equal t.test at that time",
      near(r0$diff, unname(tt0$estimate[1] - tt0$estimate[2])) && near(r0$ci_lo, tt0$conf.int[1]) && near(r0$t, unname(tt0$statistic)) && near(r0$df, unname(tt0$parameter)) && near(r0$p_raw, tt0$p.value))
check("per_time_comparisons: Sidak p_adj = 1 - (1 - p)^13 per sample", all(abs(ptc$p_adj - (1 - (1 - ptc$p_raw)^13)) < 1e-9, na.rm = TRUE))
check("per_time_comparisons: holm / bonferroni adjustments",
      { h <- env$per_time_comparisons(rep_pd, ref_name, "holm"); b <- env$per_time_comparisons(rep_pd, ref_name, "bonferroni")
        s1 <- h$sample == "Phage MOI 10"; all(abs(h$p_adj[s1] - p.adjust(h$p_raw[s1], "holm")) < 1e-12) && all(abs(b$p_adj[s1] - pmin(1, 13 * b$p_raw[s1])) < 1e-12) })
check("per_time_comparisons: late times separate from the reference, early times do not",
      { s <- ptc[ptc$sample == "Phage MOI 10", ]; s$p_adj[s$time == max(s$time)] < 0.05 && s$p_adj[s$time == 0] > 0.05 })
check("per_time_comparisons: accepts explicit sample / od columns", nrow(env$per_time_comparisons(transform(rep_pd, sample = variable, od = value), ref_name)) == 26)
check("per_time_comparisons: unknown reference / NULL / one replicate / constant -> NA row with note, no error",
      { a <- env$per_time_comparisons(rep_pd, "nope"); b <- env$per_time_comparisons(NULL, ref_name); c1 <- env$per_time_comparisons(rep_pd[rep_pd$replicate == 1, ], ref_name)
        k <- rep_pd; k$value <- 1; d0 <- env$per_time_comparisons(k, ref_name)
        nrow(a) == 1 && is.na(a$p_raw) && nzchar(a$note) && nrow(b) == 1 && all(is.na(c1$p_raw)) && nrow(c1) == 26 && all(is.na(d0$p_adj)) })

# ── 6. Outliers ───────────────────────────────────────────────────────────────
cat("\n--- outlier_tests / replicate_outlier_flags ---\n")
gx <- c(199.31, 199.53, 200.19, 200.82, 201.92, 201.95, 202.18, 245.57)
og <- env$outlier_tests(gx, "grubbs")
Gx <- (245.57 - mean(gx)) / sd(gx)
check("outlier_tests grubbs: documented columns and one row per value", identical(names(og)[1:6], c("index", "value", "flagged", "method", "statistic", "p")) && nrow(og) == 8)
check("outlier_tests grubbs: G = |x - mean| / sd of the extreme point, flagged is that point", near(og$statistic[8], Gx) && which(og$flagged) == 8 && sum(og$flagged) == 1)
check("grubbs_critical reproduces the published two-sided 5% table (n = 3: 1.1543, 8: 2.1266, 10: 2.2900, 20: 2.7082)",
      near(env$grubbs_critical(3), 1.1543, 2e-4) && near(env$grubbs_critical(8), 2.1266, 2e-4) && near(env$grubbs_critical(10), 2.2900, 2e-4) && near(env$grubbs_critical(20), 2.7082, 2e-4))
check("grubbs_p equals 0.05 at the critical G", near(env$grubbs_p(env$grubbs_critical(10), 10), 0.05, 1e-9) && near(env$grubbs_p(env$grubbs_critical(25), 25), 0.05, 1e-9))
check("outlier_tests grubbs: p from the t formula p = 2 n pt(t, n - 2, upper)", { n <- 8; tt <- sqrt(n * (n - 2) * Gx^2 / ((n - 1)^2 - n * Gx^2)); near(og$p[8], min(1, 2 * n * pt(tt, n - 2, lower.tail = FALSE))) })
check("outlier_tests grubbs: a well-behaved sample is not flagged", !any(env$outlier_tests(c(10, 10.2, 9.9, 10.1, 9.8, 10.05), "grubbs")$flagged))
check("outlier_tests grubbs: n = 3 has G max = 1.1547 (critical 1.1543): ordinary data are not flagged and the low-power note is shown",
      { z <- env$outlier_tests(c(1, 2, 4), "grubbs"); !any(z$flagged) && grepl("low power", z$note[1]) })
check("outlier_tests grubbs: n < 3 / constant / NA -> no flag, note, no error",
      { a <- env$outlier_tests(c(1, 2), "grubbs"); b <- env$outlier_tests(c(5, 5, 5, 5), "grubbs"); c0 <- env$outlier_tests(c(NA, 1, 2, 3, 40), "grubbs"); e0 <- env$outlier_tests(numeric(0), "grubbs")
        !any(a$flagged) && nzchar(a$note[1]) && !any(b$flagged) && nrow(c0) == 5 && is.na(c0$value[1]) && nrow(e0) == 0 })
om_ <- env$outlier_tests(c(1, 2, 3, 2, 50), "mad")
check("outlier_tests mad: |x - median| / (1.4826 MAD) > 3 flags only the extreme value", identical(om_$flagged, c(FALSE, FALSE, FALSE, FALSE, TRUE)) && near(om_$statistic[5], 48 / (1.4826 * 1)))
check("outlier_tests mad: MAD = 0 -> no flag and a note", { z <- env$outlier_tests(c(2, 2, 2, 2, 9), "mad"); !any(z$flagged) && grepl("MAD = 0", z$note[1]) })
oi <- env$outlier_tests(c(1, 2, 3, 4, 5, 6, 7, 8, 30), "iqr")
q <- quantile(c(1, 2, 3, 4, 5, 6, 7, 8, 30), c(.25, .75), type = 7)
check("outlier_tests iqr: Tukey fences, only the value beyond q3 + 1.5 IQR flagged", sum(oi$flagged) == 1 && oi$flagged[9] && 30 > q[2] + 1.5 * diff(q) && 8 <= q[2] + 1.5 * diff(q))
check("outlier_tests iqr: n < 4 -> no flag, note", { z <- env$outlier_tests(c(1, 2, 90), "iqr"); !any(z$flagged) && nzchar(z$note[1]) })
check("outlier_tests: every method returns the six documented columns", all(vapply(c("grubbs", "mad", "iqr"), function(m) all(c("index", "value", "flagged", "method", "statistic", "p") %in% names(env$outlier_tests(1:6, m))), logical(1))))
rof <- env$replicate_outlier_flags(rep_pd)
check("replicate_outlier_flags: one row per replicate curve (9), documented columns", nrow(rof) == 9 && all(c("sample", "replicate", "well", "distance", "flagged") %in% names(rof)))
check("replicate_outlier_flags: clean demo replicates are not flagged", !any(rof$flagged))
bad <- rep_pd; bad$value[bad$variable == "Phage MOI 10" & bad$replicate == 2] <- bad$value[bad$variable == "Phage MOI 10" & bad$replicate == 2] + seq(0.3, 0.9, length.out = 13)
rb <- env$replicate_outlier_flags(bad)
check("replicate_outlier_flags: a wrecked replicate curve is flagged and the others are not",
      sum(rb$flagged) == 1 && rb$flagged[rb$sample == "Phage MOI 10" & rb$replicate == "2"])
check("replicate_outlier_flags: fewer than 3 replicates -> no call, note; NULL -> empty",
      { z <- env$replicate_outlier_flags(rep_pd[rep_pd$replicate <= 2, ]); !any(z$flagged) && all(nzchar(z$note)) && nrow(env$replicate_outlier_flags(NULL)) == 0 })
check("replicate_outlier_flags: identical replicates (median deviation 0) -> NA distance, no error",
      { k <- rep_pd; k$value <- k$time; z <- env$replicate_outlier_flags(k); all(is.na(z$distance)) && !any(z$flagged) })

# ── 7. AUC ────────────────────────────────────────────────────────────────────
cat("\n--- auc_table / auc_compare ---\n")
tri <- data.frame(sample = "tri", replicate = 1, time = c(0, 5, 10), od = c(0, 5, 0))
at <- env$auc_table(tri)
check("auc_table: triangle (base 10, height 5) integrates to 25", near(at$replicates$auc, 25) && at$summary$n == 1)
check("auc_table: window 2.5-7.5 interpolates the edges (analytic 18.75)", near(env$auc_table(tri, t_from = 2.5, t_to = 7.5)$replicates$auc, 18.75))
check("auc_table: window 0-5 gives half the triangle (12.5)", near(env$auc_table(tri, t_from = 0, t_to = 5)$replicates$auc, 12.5))
trap <- data.frame(sample = "tz", replicate = 1, time = c(0, 2, 6, 10), od = c(1, 3, 3, 1))
check("auc_table: trapezoid with unequal spacing (analytic 2 x 2 + 4 x 3 + 4 x 2 = 24)", near(env$auc_table(trap)$replicates$auc, 24))
check("auc_table: baseline 'first' subtracts the first reading (triangle on a pedestal of 1 -> 25)", near(env$auc_table(transform(tri, od = od + 1), baseline = "first")$replicates$auc, 25))
check("auc_table: baseline 'value' integrates exactly above the baseline (triangle above 2: width 6 x height 3 / 2 = 9)",
      near(env$auc_table(tri, baseline = "value", baseline_value = 2)$replicates$auc, 9))
check("auc_table: baseline 'min' on a curve starting above 0",  near(env$auc_table(transform(tri, od = od + 3), baseline = "min")$replicates$auc, 25))
check("auc_table: clipped values never contribute negative area (od below the baseline -> 0)", near(env$auc_table(tri, baseline = "value", baseline_value = 50)$replicates$auc, 0))
ar <- env$auc_table(rep_pd, relative_to = ref_name)
check("auc_table: replicates frame has one row per curve; summary one row per sample with CI",
      nrow(ar$replicates) == 9 && nrow(ar$summary) == 3 && all(c("n", "mean", "sd", "sem", "ci_lo", "ci_hi") %in% names(ar$summary)))
check("auc_table: per-replicate AUC equals the trapezoid sum on the demo curves",
      { g <- rep_pd[rep_pd$variable == "Phage MOI 10" & rep_pd$replicate == 1, ]; near(ar$replicates$auc[ar$replicates$sample == "Phage MOI 10" & ar$replicates$replicate == "1"], sum(diff(g$time) * (head(g$value, -1) + tail(g$value, -1)) / 2)) })
check("auc_table: summary mean / sd / CI match base R on the replicate AUCs",
      { v <- ar$replicates$auc[ar$replicates$sample == ref_name]; s <- ar$summary[ar$summary$sample == ref_name, ]; near(s$mean, mean(v)) && near(s$sd, sd(v)) && near(s$ci_lo, t.test(v)$conf.int[1]) })
check("auc_table: relative AUC = mean / reference mean; reference is 1; CI brackets the ratio",
      { s <- ar$summary; r <- s[s$sample == "Phage MOI 10", ]; near(r$rel_auc, r$mean / s$mean[s$sample == ref_name]) && s$rel_auc[s$sample == ref_name] == 1 && r$rel_lo < r$rel_auc && r$rel_auc < r$rel_hi })
check("auc_table: relative-AUC CI is the log-delta interval with Welch df (hand formula)",
      { s <- ar$summary; r <- s[s$sample == "Phage MOI 10", ]; f <- s[s$sample == ref_name, ]; R <- r$mean / f$mean; se <- sqrt((r$sem / r$mean)^2 + (f$sem / f$mean)^2)
        df <- (r$sem^2 + f$sem^2)^2 / (r$sem^4 / 2 + f$sem^4 / 2); near(r$rel_lo, R * exp(-qt(.975, df) * se)) && near(r$rel_hi, R * exp(qt(.975, df) * se)) })
check("auc_table: relative AUC with exact ratio 0.5 (100 vs 50)", { z <- rbind(data.frame(sample = "r", replicate = 1:3, time = rep(c(0, 10), each = 3), od = rep(c(9, 10, 11), 2)), data.frame(sample = "s", replicate = 1:3, time = rep(c(0, 10), each = 3), od = rep(c(4.5, 5, 5.5), 2)))
  s <- env$auc_table(z, relative_to = "r")$summary; near(s$rel_auc[s$sample == "s"], 0.5) })
check("auc_table: accepts the explicit sample / od / well columns", near(env$auc_table(transform(rep_pd, sample = variable, od = value))$summary$mean[1], ar$summary$mean[1]))
check("auc_table: bad window, one reading, NULL, unknown reference -> NA with notes, no error",
      { a <- env$auc_table(tri, t_from = 8, t_to = 2); b <- env$auc_table(tri[1, ]); c0 <- env$auc_table(NULL); d0 <- env$auc_table(rep_pd, relative_to = "nope"); e0 <- env$auc_table(tri, t_from = 20, t_to = 30)
        is.na(a$replicates$auc) && nzchar(a$replicates$note) && is.na(b$replicates$auc) && nrow(c0$replicates) == 0 && all(is.na(d0$summary$rel_auc)) && is.na(e0$replicates$auc) })
ac <- env$auc_compare(ar, ref_name)
check("auc_compare: reuses posthoc_table (holm_welch) vs the reference, 2 rows",
      nrow(ac) == 2 && all(ac$group2 == ref_name) && all(grepl("Holm", ac$method)) && all(ac$p_adj < 0.05))
check("auc_compare: equals posthoc_table on the replicate AUCs; accepts the replicates frame; other methods pass through",
      { pt <- env$posthoc_table(ar$replicates$auc, ar$replicates$sample, "holm_welch", reference = ref_name)
        near(ac$p_adj[1], pt$p_adj[1]) && nrow(env$auc_compare(ar$replicates, ref_name)) == 2 && grepl("Tukey", env$auc_compare(ar, ref_name, "tukey")$method[1]) })
check("auc_compare: empty input -> NA row, no error", all(is.na(env$auc_compare(NULL, "x")$p_adj)))

# ── 8. One-sample tests ───────────────────────────────────────────────────────
cat("\n--- one_sample_tests ---\n")
y1 <- c(1.2, 0.8, 1.5, 1.1, 1.7, 0.9)
os <- env$one_sample_tests(y1, mu = 1)
check("one_sample_tests: two rows (t and Wilcoxon signed-rank)", nrow(os) == 2 && grepl("t", os$test[1]) && grepl("Wilcoxon", os$test[2]))
check("one_sample_tests: t statistic, df, p and CI of the mean equal t.test",
      { tt <- t.test(y1, mu = 1); near(os$statistic[1], unname(tt$statistic)) && os$df[1] == 5 && near(os$p[1], tt$p.value) && near(os$ci_lo[1], tt$conf.int[1]) && near(os$ci_hi[1], tt$conf.int[2]) })
check("one_sample_tests: with tied |x - mu| the Wilcoxon p is the normal approximation (equals wilcox.test(exact = FALSE))",
      { w <- suppressWarnings(wilcox.test(y1, mu = 1, exact = FALSE)); near(os$statistic[2], unname(w$statistic)) && near(os$p[2], w$p.value) && grepl("normal approximation", os$note[2]) })
check("one_sample_tests: without ties the Wilcoxon p is exact and equals wilcox.test",
      { y2 <- c(1.31, 1.62, 0.55, 1.94, 2.2, 0.17); z <- env$one_sample_tests(y2, 1); w <- wilcox.test(y2, mu = 1); near(z$statistic[2], unname(w$statistic)) && near(z$p[2], w$p.value) && grepl("exact", z$note[2]) })
check("one_sample_tests: n = 3 -> smallest attainable Wilcoxon p is 0.25 and the note says so",
      { z <- env$one_sample_tests(c(1.1, 0.9, 1.3), 1); grepl("0.25", z$note[2]) && grepl("cannot reach 0.05", z$note[2]) })
check("one_sample_tests: n = 1 / constant / NA -> NA p with a note, no error",
      { a <- env$one_sample_tests(2, 1); b <- env$one_sample_tests(c(3, 3, 3), 3); c0 <- env$one_sample_tests(c(NA, NA), 0); d0 <- env$one_sample_tests(numeric(0))
        all(is.na(a$p)) && all(nzchar(a$note)) && is.na(b$p[1]) && all(is.na(c0$p)) && all(is.na(d0$p)) })

# ── 9. Correlation and regression ─────────────────────────────────────────────
cat("\n--- correlation_table / linear_fit_table / linear_band ---\n")
cx <- c(1, 2, 3, 4, 5, 6, 7); cy <- c(2.1, 3.9, 3.2, 6.8, 8.1, 7.7, 11)
cp <- env$correlation_table(cx, cy, "pearson"); ctp <- cor.test(cx, cy)
check("correlation_table pearson: r, p, CI, df equal cor.test", near(cp$r, unname(ctp$estimate)) && near(cp$p, ctp$p.value) && near(cp$ci_lo, ctp$conf.int[1]) && near(cp$ci_hi, ctp$conf.int[2]) && cp$df == 5 && near(cp$r2, cp$r^2))
cs <- env$correlation_table(cx, cy, "spearman"); cts <- cor.test(cx, cy, method = "spearman")
check("correlation_table spearman: rho and p equal cor.test; Fisher-z CI brackets rho", near(cs$r, unname(cts$estimate)) && near(cs$p, cts$p.value) && cs$ci_lo < cs$r && cs$r < cs$ci_hi)
check("correlation_table: NA pairs dropped; n < 3 / constant -> NA row with note, no error",
      { a <- env$correlation_table(c(1, 2, NA, 4, 5), c(2, 3, 4, NA, 8)); b <- env$correlation_table(1:2, 1:2); c0 <- env$correlation_table(1:5, rep(3, 5)); d0 <- env$correlation_table(numeric(0), numeric(0))
        a$n == 3 && is.finite(a$r) && is.na(b$r) && nzchar(b$note) && is.na(c0$r) && is.na(d0$r) })
lf <- env$linear_fit_table(cx, cy); lm1 <- lm(cy ~ cx); ci1 <- confint(lm1)
check("linear_fit_table: slope / intercept estimates, SE, p equal lm", near(lf$estimate[2], coef(lm1)[[2]]) && near(lf$estimate[1], coef(lm1)[[1]]) && near(lf$p[2], summary(lm1)$coefficients[2, 4]) && near(lf$se[2], summary(lm1)$coefficients[2, 2]))
check("linear_fit_table: CIs equal confint; r2 equals summary(lm)", near(lf$ci_lo[2], ci1[2, 1]) && near(lf$ci_hi[1], ci1[1, 2]) && near(lf$r2[1], summary(lm1)$r.squared) && lf$n[1] == 7)
check("linear_fit_table: level = 0.9 narrows the CI", { l9 <- env$linear_fit_table(cx, cy, 0.9); (l9$ci_hi[2] - l9$ci_lo[2]) < (lf$ci_hi[2] - lf$ci_lo[2]) })
check("linear_fit_table: n < 3 / constant x / constant y / NA -> NA or noted, no error",
      { a <- env$linear_fit_table(1:2, 1:2); b <- env$linear_fit_table(rep(1, 4), 1:4); c0 <- env$linear_fit_table(1:4, rep(2, 4)); d0 <- env$linear_fit_table(c(NA, NA), c(NA, NA))
        all(is.na(a$p)) && all(is.na(b$p)) && is.na(c0$r2[1]) && nzchar(c0$note[1]) && all(is.na(d0$p)) })
nx <- c(0, 3.5, 8); lb <- env$linear_band(cx, cy, nx); pr <- predict(lm1, data.frame(cx = nx), interval = "confidence")
check("linear_band: fit / lo / hi equal predict(interval = 'confidence')", identical(names(lb), c("x", "fit", "lo", "hi")) && all(abs(lb$fit - pr[, "fit"]) < 1e-9) && all(abs(lb$lo - pr[, "lwr"]) < 1e-9) && all(abs(lb$hi - pr[, "upr"]) < 1e-9))
check("linear_band: prediction band is wider than the confidence band; default newx gives 50 points",
      { p <- env$linear_band(cx, cy, nx, type = "prediction"); all(p$hi - p$lo > lb$hi - lb$lo) && nrow(env$linear_band(cx, cy)) == 50 })
check("linear_band: too few points -> NA band, no error", all(is.na(env$linear_band(1:2, 1:2, c(1, 2))$fit)) && all(is.na(env$linear_band(rep(1, 4), 1:4, 1:2)$lo)))

# ── 10. Extra error measures ──────────────────────────────────────────────────
cat("\n--- error_half_width_extra ---\n")
ev <- c(1, 2, 6)
er <- env$error_half_width_extra(ev, "range")
check("error_half_width_extra range: offsets mean - min and max - mean (asymmetric)", near(er$lower, 3 - 1) && near(er$upper, 6 - 3) && near(er$ymin, 1) && near(er$ymax, 6) && near(er$center, 3))
ei <- env$error_half_width_extra(ev, "iqr"); qi <- quantile(ev, c(.25, .75), type = 7, names = FALSE)
check("error_half_width_extra iqr: bar from q1 to q3 around the mean", near(ei$ymin, qi[1]) && near(ei$ymax, qi[2]) && near(ei$lower, 3 - qi[1]) && near(ei$upper, qi[2] - 3))
e90 <- env$error_half_width_extra(ev, "ci90"); e99 <- env$error_half_width_extra(ev, "ci99")
check("error_half_width_extra ci90 / ci99: symmetric t intervals", near(e90$upper, qt(.95, 2) * sd(ev) / sqrt(3)) && near(e90$lower, e90$upper) && near(e99$upper, qt(.995, 2) * sd(ev) / sqrt(3)) && e99$upper > e90$upper)
check("error_half_width_extra: n = 1 / NA / empty -> NA, no error",
      is.na(env$error_half_width_extra(5, "range")$lower) && is.na(env$error_half_width_extra(c(NA, NA), "iqr")$upper) && is.na(env$error_half_width_extra(numeric(0), "ci90")$lower))
check("error_half_width_extra: constant values -> zero-width bars", { z <- env$error_half_width_extra(c(2, 2, 2), "range"); z$lower == 0 && z$upper == 0 })

# ── 11. Methods sentences and definitions ─────────────────────────────────────
cat("\n--- stats_method_sentence / STATS_V222_DEFINITIONS ---\n")
ms <- function(res, k, ...) env$stats_method_sentence(res, k, ...)
check("stats_method_sentence posthoc Games-Howell: the documented example wording",
      identical(ms(gh, "posthoc", what = "Lysis onset"), "Lysis onset was compared between samples by one-way Welch ANOVA followed by Games–Howell post-hoc tests (n = 3 biological replicates per sample)."))
check("stats_method_sentence posthoc: Tukey -> one-way ANOVA; Dunn -> Kruskal-Wallis",
      grepl("one-way ANOVA followed by Tukey", ms(tk, "posthoc")) && grepl("Kruskal–Wallis test followed by Dunn", ms(dn, "posthoc")))
check("stats_method_sentence posthoc: Holm / Bonferroni / Sidak and the Dunnett fallback name their methods",
      grepl("Holm correction", ms(hw, "posthoc")) && grepl("Bonferroni correction", ms(bf, "posthoc")) && grepl("Šidák", ms(sd_, "posthoc")) && grepl("Dunnett's test was not available|Dunnett's multiple", ms(dnt, "posthoc")))
check("stats_method_sentence: every kind returns one non-empty sentence ending in a full stop",
      { res <- list(describe = d, normality = nt, variance = vc, omnibus = om, posthoc = gh, two_way = tw, per_time = ptc, outlier = og, auc = ar, one_sample = os, correlation = cp, linear = lf)
        all(vapply(names(res), function(k) { s <- ms(res[[k]], k, what = "Lysis onset"); length(s) == 1 && nzchar(s) && grepl("\\.$", s) }, logical(1))) })
check("stats_method_sentence two_way: names the mixed model and the log transform", { s <- ms(tw, "two_way", what = "OD"); grepl("repeated-measures", s) && grepl("log-transformed", s) })
check("stats_method_sentence: n argument overrides and ranges are shown", grepl("n = 2–3", ms(gh, "posthoc", n = c(2, 3, 3))) && grepl("n = 5 ", ms(gh, "posthoc", n = c(5, 5))))
check("stats_method_sentence: unknown / empty result does not error", is.character(ms(NULL, "posthoc")) && is.character(ms(data.frame(), "outlier")))
sdf <- env$STATS_V222_DEFINITIONS
check("STATS_V222_DEFINITIONS: data.frame(name, definition, reference), every field filled, unique names",
      is.data.frame(sdf) && identical(names(sdf), c("name", "definition", "reference")) && nrow(sdf) >= 25 && all(nzchar(sdf$name)) && all(nzchar(sdf$definition)) && all(nzchar(sdf$reference)) && !anyDuplicated(sdf$name))

# ── 12. Engine-wide robustness ────────────────────────────────────────────────
cat("\n--- robustness: bad input never errors ---\n")
bad_inputs <- list(numeric(0), 5, c(5, 5, 5), c(NA_real_, NA_real_), c(1, NA, 3), c(Inf, 1, 2), NULL, character(0))
check("column functions survive empty / n = 1 / constant / NA / Inf / NULL input",
      all(vapply(bad_inputs, function(b) {
        r <- try(list(env$describe_column(b), env$qq_frame(b), env$one_sample_tests(b), env$error_half_width_extra(b, "range"), env$outlier_tests(b, "grubbs"), env$outlier_tests(b, "mad"), env$outlier_tests(b, "iqr")), silent = TRUE)
        !inherits(r, "try-error") }, logical(1))))
check("pair functions survive mismatched / degenerate x, y",
      !inherits(try(list(env$correlation_table(1, 2), env$correlation_table(c(1, 1), c(2, 2)), env$linear_fit_table(NULL, NULL), env$linear_band(NULL, NULL, 1:2), env$correlation_table(1:5, 1:3)), silent = TRUE), "try-error"))
check("group functions survive degenerate values / groups",
      !inherits(try(list(env$variance_check(NULL, NULL), env$omnibus_table(NULL, NULL), env$posthoc_table(NULL, NULL), env$posthoc_table(1:6, factor(rep(c("a", "b"), 3)), "games_howell", reference = NA),
                         env$posthoc_table(c(1, 2, 3, 4, 5, 6), c(NA, NA, "a", "a", "b", "b"), "dunn")), silent = TRUE), "try-error"))
check("time-course functions survive missing columns and empty frames",
      !inherits(try(list(env$two_way_time_anova(data.frame(a = 1)), env$per_time_comparisons(data.frame(a = 1), "x"), env$replicate_outlier_flags(data.frame(a = 1)), env$auc_table(data.frame(a = 1))), silent = TRUE), "try-error"))
check("posthoc_table accepts factor groups and keeps the factor's level order",
      { z <- env$posthoc_table(xs, factor(gs, levels = c("d", "c", "b", "a")), "holm_welch"); z$group2[1] == "d" })
check("v2.22 block leaves the earlier engine untouched: compare_replicate_metric still runs", is.list(env$compare_replicate_metric(rr, "auc", ref_name)))

# ── SUMMARY ───────────────────────────────────────────────────────────────────
cat(sprintf("\n============ SUMMARY: %d passed / %d failed / %d skipped ============\n", pass, fail, skip))
if (fail > 0) quit(status = 1)
