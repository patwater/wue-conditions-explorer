# Stage 8: validation suite and report. Compares the matched DiD estimates with the planted effects, summarizes
# every gate, runs the optional checks behind flags, and writes reports/validation.html and reports/environment.txt.
#   Rscript R/10_validate.R --profile default [--scenarios] [--refit] [--sbc] [--causal] --cores 4
# The question answered is narrow: does matched DiD recover an effect we planted, in a world with turnover, vacancy,
# missing rows, drought shocks and selection on conservation attitude, and how robust is that recovery?
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), "00_common.R"))
suppressPackageStartupMessages({ library(ggplot2); library(parallel) })
ctx <- init_ctx("10_validate")
G <- pv(ctx, "units", "gallons_per_ccf"); MDAYS <- 30.4375
lvl <- pv(ctx, "estimation", "interval_level")
tm <- ctx$time
P_ <- ctx$paths

# ---------------------------------------------------------------------------------------------
# Planted effect over each household's headline window, and the planted-versus-estimated comparison
# ---------------------------------------------------------------------------------------------

planted_over_windows <- function(out_dir, win) {
  clean <- read_table(file.path(out_dir, "truth", "clean_panel.parquet"))[treated_post == TRUE | planted_turf_saving_ccf != 0,
                                                                          .(location_id, t, planted_turf_saving_ccf)]
  clean[, `:=`(ms = tm$month_start[t], me = tm$month_end[t], dim = tm$days[t])]
  x <- clean[win, on = .(location_id), allow.cartesian = TRUE, nomatch = NULL]
  x[, ov := pmax(0L, as.integer(pmin(me, head_end) - pmax(ms, head_start)) + 1L)]
  p <- x[, .(planted_ccf = sum(planted_turf_saving_ccf * ov / dim)), by = .(location_id, quantity)]
  p <- merge(win, p, by = c("location_id", "quantity"), all.x = TRUE)
  p[is.na(planted_ccf), planted_ccf := 0]
  p[, window_days := as.integer(head_end - head_start) + 1L]
  p[, planted := planted_ccf * G / quantity * 365.25 / window_days]
  p[, .(location_id, planted)]
}

metrics <- function(d) {
  d <- d[is.finite(estimate) & is.finite(planted)]
  if (nrow(d) < 3) return(data.table(n = nrow(d), mean_estimate = NA_real_, mean_planted = NA_real_, bias = NA_real_, rmse = NA_real_,
                                      mae = NA_real_, coverage90 = NA_real_, spearman = NA_real_, slope = NA_real_))
  cover <- if ("lo90" %in% names(d)) mean(d$planted >= d$lo90 & d$planted <= d$hi90, na.rm = TRUE) else NA_real_
  data.table(n = nrow(d), mean_estimate = mean(d$estimate), mean_planted = mean(d$planted), bias = mean(d$estimate - d$planted),
             rmse = sqrt(mean((d$estimate - d$planted)^2)), mae = mean(abs(d$estimate - d$planted)),
             coverage90 = cover, spearman = cor(d$estimate, d$planted, method = "spearman"),
             slope = unname(coef(lm(estimate ~ planted, data = d))[2]))
}

compare_dir <- function(out_dir) {
  est <- read_table(file.path(out_dir, "estimates", "household_estimates.csv"))
  bs_f <- file.path(out_dir, "estimates", "bsts_household_estimates.csv")
  win <- unique(est[window == "headline", .(location_id, quantity, head_start, head_end)])
  pl <- planted_over_windows(out_dir, win)
  e <- merge(est[window == "headline"], pl, by = "location_id")
  if (file.exists(bs_f)) {
    b <- merge(read_table(bs_f), pl, by = "location_id")
    b[, `:=`(lo90 = estimate - qnorm(1 - (1 - lvl) / 2) * se, hi90 = estimate + qnorm(1 - (1 - lvl) / 2) * se)]
    e <- rbind(e, b, fill = TRUE)
  }
  e
}

base_cmp <- compare_dir(P_$out)
hs <- read_table(file.path(P_$truth, "household_savings.csv"))
base_cmp <- merge(base_cmp, hs[, .(location_id, turnover_in_pre_period, turnover_in_post_period, has_hecw, has_wbic)],
                  by = "location_id", all.x = TRUE)
by_est <- base_cmp[, metrics(.SD), by = estimator]
prim_name <- paste0("matched_", pv(ctx, "estimation", "match_method"))
prim <- base_cmp[estimator == prim_name]
prim[, ctrl_other_bin := cut(share_controls_other_rebate, c(-Inf, 0, 0.34, Inf), labels = c("none", "1-2 of 6", "3+ of 6"))]
breakouts <- rbindlist(list(
  prim[, metrics(.SD), by = .(group = paste("retailer:", utility_name))],
  prim[, metrics(.SD), by = .(group = paste("billing:", billing))],
  prim[, metrics(.SD), by = .(group = paste("turnover in pre-period:", turnover_in_pre_period))],
  prim[, metrics(.SD), by = .(group = paste("turnover in post-period:", turnover_in_post_period))],
  prim[, metrics(.SD), by = .(group = paste("matched controls with other rebates:", ctrl_other_bin))],
  prim[, metrics(.SD), by = .(group = paste("co-treated in window:", cotreated_in_window))]), fill = TRUE)
write_table(by_est, file.path(P_$reports, "planted_vs_estimated_by_estimator.csv"))
write_table(breakouts, file.path(P_$reports, "planted_vs_estimated_breakouts.csv"))
write_table(base_cmp, file.path(P_$reports, "planted_vs_estimated_households.csv"), c("estimator", "location_id"))

plac <- read_table(file.path(P_$out, "estimates", "placebo_estimates.csv"))
vd <- data.table(sd_estimated = sd(prim$estimate), sd_planted = sd(prim$planted), sd_placebo = sd(plac$estimate))
vd[, `:=`(implied_sd_estimated = sqrt(sd_planted^2 + sd_placebo^2), ratio = sd_estimated / sqrt(sd_planted^2 + sd_placebo^2))]
rob <- data.table(iqr_estimated = IQR(prim$estimate), iqr_planted = IQR(prim$planted), iqr_placebo = IQR(plac$estimate))
write_table(cbind(vd, rob), file.path(P_$reports, "variance_decomposition.csv"))

# Gates on the comparison (bias gate is part of the estimated-side set; non-blocking, documented if missed).
ge <- pv(ctx, "gates", "estimated")
pm <- by_est[estimator == prim_name]
ctx <- gate(ctx, "absolute bias of matched DiD against the planted effect (gal/sqft/yr)", abs(pm$bias) < ge$max_abs_bias,
            pm$bias, sprintf("|bias| < %s", ge$max_abs_bias), blocking = FALSE)
ctx <- gate(ctx, "90 percent interval coverage of the planted effect (reported)", TRUE, pm$coverage90, "nominal 0.90")
ctx <- gate(ctx, "variance decomposition: estimated SD vs sqrt(planted^2 + placebo^2) (reported)", TRUE,
            c(vd$sd_estimated, vd$implied_sd_estimated), "ratio near 1")

# Meta-regression signs against the planted regression (Table 3 sign pattern).
mt <- read_table(file.path(P_$out, "estimates", "meta_regression.csv"))
want <- c(eff_pre_observed = 1, log_quantity = -1, log_area_ratio = -1, log_income = -1, et_post = 1)
mt[, planted_sign := want[term]]
mt[, sign_match := sign(estimate) == planted_sign]
ctx <- gate(ctx, "meta-regression signs match the planted regression (reported)", TRUE,
            sprintf("%d of %d", sum(mt$sign_match, na.rm = TRUE), sum(!is.na(mt$planted_sign))), "report only")
write_table(mt, file.path(P_$reports, "meta_regression_signs.csv"))

# Seasonal profile: estimated (household seasonal fits) against planted (first 12 post-months, by calendar month).
seas <- read_table(file.path(P_$out, "estimates", "seasonal_estimates.csv"))
cl <- read_table(file.path(P_$truth, "clean_panel.parquet"))[planted_turf_saving_ccf != 0 & location_id %in% seas$location_id]
cl <- merge(cl, hs[, .(location_id, t_effect, quantity)], by = "location_id")[t >= t_effect & t < t_effect + 12L]
seas_cmp <- merge(seas[, .(estimated = mean(est_gal_per_sqft_month)), by = month],
                  cl[, .(planted = mean(planted_turf_gal_per_sqft)), by = .(month = tm$month[t])], by = "month")
write_table(seas_cmp, file.path(P_$reports, "seasonal_planted_vs_estimated.csv"), "month")

# Other rebate types against their planted effects.
oth <- read_table(file.path(P_$out, "estimates", "other_rebate_estimates.csv"))
orb <- pv(ctx, "treatment", "other_rebates")
oth[, planted := fifelse(type == "hecw", orb$hecw$gal_per_day, orb$wbic$outdoor_frac)]
oth[, covered := planted >= converted_lo90 & planted <= converted_hi90]
write_table(oth, file.path(P_$reports, "other_rebates_planted_vs_estimated.csv"), "type")

# ---------------------------------------------------------------------------------------------
# Edge cases, RNG keys, reproducibility, memory
# ---------------------------------------------------------------------------------------------
usage <- read_table(file.path(P_$data, "usage.parquet"))
flags <- read_table(file.path(P_$truth, "observation_flags.parquet"))
vac <- cl <- NULL
clean_all <- read_table(file.path(P_$truth, "clean_panel.parquet"))[, .(location_id, t, vacant)]
billing <- read_table(file.path(P_$truth, "billing_clean.parquet"))[, .(location_id, period_index, usage_start_date)]
billing[, t := month_index(ctx, usage_start_date)]
vb <- merge(billing, clean_all, by = c("location_id", "t"))
vb <- merge(vb, flags[, .(location_id, period_index, missing)], by = c("location_id", "period_index"))
co <- read_table(file.path(P_$out, "estimates", "turf_cohort.csv"))
ret <- read_table(wpath(ctx, "retailers.csv"))
edge <- data.table(
  case = c("vacancy rows present in observed usage", "missing rows during occupancy (feed gaps)", "vacancy rows also missing",
           "turf rebates failing the 24-month pre filter (short history)", "turf rebates failing the 12-month post filter (late rebates)",
           "treated households in bimonthly retailers", "turf locations dropped for defaulted area", "duplicate turf rebates dropped",
           "treated households with turnover within 6 months of the rebate"),
  count = c(vb[vacant == TRUE & missing == FALSE, .N], vb[vacant == FALSE & missing == TRUE, .N], vb[vacant == TRUE & missing == TRUE, .N],
            co[pre_months < 24, .N], co[pre_months >= 24 & post_months < 12, .N],
            co[passes_filters == TRUE & utility_name %in% ret[billing == "bimonthly", name], .N],
            co[defaulted_area == TRUE, .N], co[duplicate == TRUE, .N],
            {ev <- read_table(file.path(P_$truth, "occupancy_events.csv"))[event_type == "turnover"]
             x <- merge(ev, hs[, .(location_id, t_rebate)], by = "location_id"); x[abs(t - t_rebate) <= 6, uniqueN(location_id)]}))
write_table(edge, file.path(P_$reports, "edge_cases.csv"))
ctx <- gate(ctx, "edge cases are all present in the data", all(edge$count[c(1, 2, 4, 5, 6, 7, 8)] > 0), edge$count, "each > 0",
            absolute = TRUE)
keys <- fread(wpath(ctx, "rng_keys.csv"))
ctx <- gate(ctx, "no two stage-unit RNG keys share a seed integer", !anyDuplicated(keys$int), c(nrow(keys), uniqueN(keys$int)),
            "all distinct")
rep_f <- file.path(P_$reports, "reproducibility.csv")
if (file.exists(rep_f)) {
  rp <- fread(rep_f)
  ctx <- gate(ctx, "data and truth files identical across core counts", all(rp$identical), c(sum(rp$identical), nrow(rp)),
              "all files identical")
} else ctx <- gate(ctx, "reproducibility test (tests/test_reproducibility.sh)", NA, "not run", "run the test script")
runs <- fread(wpath(ctx, "stage_runs.csv"))

# ---------------------------------------------------------------------------------------------
# Scenarios (behind --scenarios): rerun the whole pipeline in each planted world, estimator fixed
# ---------------------------------------------------------------------------------------------
scen_tab <- NULL
if (isTRUE(ctx$opt$scenarios)) {
  scs <- names(pv(ctx, "scenarios"))
  stages <- c("01_geography", "02_accounts", "03_occupancy", "04_climate", "05_usage", "06_treatment", "07_billing",
              "08_observation", "09_estimate_savings")
  run_sc <- function(sc) {
    out <- file.path(P_$root, "build", "scenarios", ctx$opt$profile, sc)
    for (st in stages) {
      s <- system2("Rscript", c(file.path(P_$root, "R", paste0(st, ".R")), "--profile", ctx$opt$profile, "--scenario", sc,
                                "--out", out, "--force", "--cores", "2"), stdout = FALSE, stderr = FALSE)
      if (s != 0) return(data.table(scenario = sc, error = st))
    }
    e <- compare_dir(out)
    r <- e[, metrics(.SD), by = estimator][, scenario := sc]
    r
  }
  scen_tab <- rbindlist(c(list(by_est[, scenario := "base"]), mclapply(scs, run_sc, mc.cores = max(1, ctx$opt$cores %/% 2))), fill = TRUE)
  desc <- vapply(scs, function(s) pv(ctx, "scenarios")[[s]]$description, "")
  scen_tab[, description := c(base = "Anchored base world (frozen calibration).", desc)[scenario]]
  write_table(scen_tab, file.path(P_$reports, "scenario_table.csv"), c("scenario", "estimator"))
  ctx <- gate(ctx, "scenario runs completed", !"error" %in% names(scen_tab) || all(is.na(scen_tab$error)),
              uniqueN(scen_tab$scenario), sprintf("%d scenarios plus base", length(scs)))
}

# ---------------------------------------------------------------------------------------------
# Optional: multilevel recovery refit (--refit) and simulation-based calibration (--sbc)
# ---------------------------------------------------------------------------------------------
flagged <- data.table(check = c("matched DiD (default)", "--refit", "--sbc", "--causal (BSTS)", "--scenarios"),
                      ran = c(TRUE, isTRUE(ctx$opt$refit), isTRUE(ctx$opt$sbc), file.exists(file.path(P_$out, "estimates", "bsts_household_estimates.csv")),
                              isTRUE(ctx$opt$scenarios)))
if (isTRUE(ctx$opt$refit)) source(file.path(P_$root, "R", "lib_refit.R"), local = TRUE)
if (isTRUE(ctx$opt$sbc)) source(file.path(P_$root, "R", "lib_sbc.R"), local = TRUE)
write_table(flagged, file.path(P_$reports, "flagged_checks.csv"))

# ---------------------------------------------------------------------------------------------
# Environment record
# ---------------------------------------------------------------------------------------------
gxx <- tryCatch(system2("g++", "--version", stdout = TRUE)[1], error = function(e) "unknown")
pk <- c("data.table", "yaml", "digest", "jsonlite", "nanoparquet", "ggplot2", "fixest", "sandwich", "metafor", "brms", "rstan",
        "StanHeaders", "CausalImpact", "bsts", "Boom", "renv")
ver <- vapply(pk, function(p) tryCatch(as.character(packageVersion(p)), error = function(e) "not installed"), "")
writeLines(c("# Environment record for the synthetic SCUBA generator", "", paste("Date:", Sys.Date()), R.version.string,
             paste("C++ compiler:", gxx), paste("Platform:", R.version$platform), "", "## Key package versions",
             paste(pk, ver, sep = ": "), "", "## sessionInfo()", capture.output(sessionInfo())),
           file.path(P_$reports, "environment.txt"))

# ---------------------------------------------------------------------------------------------
# Figures
# ---------------------------------------------------------------------------------------------
th <- theme_minimal(base_size = 11)
p <- ggplot(prim, aes(planted, estimate)) + geom_abline(linetype = 2, colour = "grey50") + geom_point(alpha = 0.5) +
  coord_cartesian(xlim = quantile(prim$planted, c(0.005, 0.995)), ylim = quantile(prim$estimate, c(0.02, 0.98))) +
  labs(x = "Planted savings over the headline window (gal/sqft/yr)", y = "Matched DiD estimate (gal/sqft/yr)",
       title = "Estimated versus planted savings, one point per treated household") + th
save_fig(ctx, p, "estimated_vs_planted")
dd <- rbind(data.table(kind = "estimated (matched DiD)", value = prim$estimate), data.table(kind = "planted", value = prim$planted),
            data.table(kind = "placebo (noise floor)", value = plac$estimate))
p <- ggplot(dd, aes(value, colour = kind)) + geom_density() + coord_cartesian(xlim = c(-150, 200)) +
  annotate("rect", xmin = 8, xmax = 32, ymin = -Inf, ymax = Inf, alpha = 0.08) +
  labs(x = "gal/sqft/yr (shaded: the paper's estimated interquartile range, about 8 to 32)", y = "Density", colour = NULL,
       title = "Planted, estimated and placebo distributions") + th + theme(legend.position = "bottom")
save_fig(ctx, p, "distributions_planted_estimated_placebo")
p <- ggplot(melt(seas_cmp, id.vars = "month"), aes(month, value, colour = variable)) + geom_line() + geom_point() +
  scale_x_continuous(breaks = 1:12) + labs(x = "Month", y = "gal/sqft/month", colour = NULL,
  title = "Seasonal savings: planted versus matched DiD estimate") + th
save_fig(ctx, p, "seasonal_planted_vs_estimated")
es <- read_table(file.path(P_$out, "estimates", "event_study.csv"))
p <- ggplot(es, aes(event_bin, estimate)) + geom_hline(yintercept = 0, colour = "grey50") + geom_vline(xintercept = -0.5, linetype = 2) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se)) +
  labs(x = "Event time in 3-month bins (reference: last pre bin)", y = "Savings (gal/sqft/yr)", title = "Event study, pooled stacked DiD") + th
save_fig(ctx, p, "event_study")
if (!is.null(scen_tab)) {
  p <- ggplot(scen_tab[!is.na(bias)], aes(bias, scenario, colour = estimator)) + geom_vline(xintercept = 0, colour = "grey60") +
    geom_point(size = 2) + labs(x = "Bias against the planted effect (gal/sqft/yr)", y = NULL, colour = NULL,
    title = "Estimator bias by planted scenario") + th + theme(legend.position = "bottom")
  save_fig(ctx, p, "scenario_bias", height = 6)
}

# ---------------------------------------------------------------------------------------------
# HTML report
# ---------------------------------------------------------------------------------------------
finish_gates <- function() rbindlist(lapply(list.files(P_$gates, pattern = "\\.csv$", full.names = TRUE), fread), fill = TRUE)
fwrite(ctx$gates, file.path(P_$gates, "10_validate.csv"))
gates <- finish_gates()
html_table <- function(d, digits = 3) {
  d <- copy(as.data.table(d))
  for (c in names(d)) if (is.numeric(d[[c]])) set(d, j = c, value = format(signif(d[[c]], digits), trim = TRUE, scientific = FALSE))
  hdr <- paste0("<tr>", paste0("<th>", names(d), "</th>", collapse = ""), "</tr>")
  rows <- apply(d, 1, function(r) paste0("<tr>", paste0("<td>", htmltools_escape(r), "</td>", collapse = ""), "</tr>"))
  paste0("<div class='tw'><table>", hdr, paste(rows, collapse = ""), "</table></div>")
}
htmltools_escape <- function(x) { x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); gsub(">", "&gt;", x) }
img <- function(name, alt) {
  f <- file.path(P_$figures, paste0(name, ".png"))
  if (!file.exists(f)) return("")
  sprintf("<figure><img alt='%s' src='data:image/png;base64,%s'><figcaption>%s</figcaption></figure>", alt,
          base64enc::base64encode(f), alt)
}
g2 <- gates[, .(stage, check, value, threshold,
                result = fifelse(is.na(pass), "SKIP", fifelse(pass, "PASS", fifelse(blocking %in% FALSE, "MISS (documented)", "FAIL"))))]
seedf <- file.path(P_$reports, c("seed_robustness_stage4.csv", "seed_robustness_heldout.csv"))
calf <- file.path(P_$reports, "calibration_log.csv")
sec <- function(title, ...) paste0("<section><h2>", title, "</h2>", ..., "</section>")
para <- function(...) paste0("<p>", ..., "</p>")
html <- paste0("<!doctype html><html lang='en'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width, initial-scale=1'>",
  "<title>Synthetic SCUBA Validation</title><style>",
  ":root{--bg:#fff;--fg:#1d2433;--muted:#5b6475;--line:#d8dde6;--accent:#1f5f8b;--bad:#a8322d;--ok:#2f6b3a}",
  "@media (prefers-color-scheme: dark){:root:not([data-theme='light']){--bg:#14171c;--fg:#e6e9ef;--muted:#a3abb9;--line:#2d333d;--accent:#7fb3d9;--bad:#e7837d;--ok:#8fc79a}}",
  "body{background:var(--bg);color:var(--fg);font:15px/1.55 system-ui,sans-serif;max-width:1100px;margin:0 auto;padding:16px}",
  ".banner{border:2px solid var(--bad);padding:10px 14px;border-radius:6px;font-weight:600}",
  "h1{font-size:1.6em}h2{border-bottom:1px solid var(--line);padding-bottom:4px;margin-top:2em}",
  ".tw{overflow-x:auto}table{border-collapse:collapse;font-size:13px;margin:8px 0}th,td{border:1px solid var(--line);padding:3px 6px;text-align:left}",
  "img{max-width:100%;height:auto;background:#fff}figcaption{color:var(--muted);font-size:13px}</style></head><body>",
  "<p class='banner'>ALL DATA IN THIS REPORT IS SIMULATED. It must not be mistaken for real utility records. ",
  "Planted effects are assumptions written into the generator, not knowledge about real households.</p>",
  "<h1>Synthetic SCUBA validation report</h1>",
  para(sprintf("Profile <b>%s</b>, root seed %s, generated %s.", ctx$opt$profile, ctx$seed, Sys.Date())),
  sec("What this report answers",
      para("Nobody knows the true savings of real households, because the counterfactual usage is never observed. The generator knows each synthetic household's savings only because it planted them. This report therefore asks a narrower question: does matched difference-in-differences recover an effect we planted, in a world with turnover, vacancy, missing rows, drought shocks and selection on conservation attitude, and how robust is that recovery when the planted world changes? The paper's savings figures are estimates, so they anchor the estimated side of the results, not the planted effect.")),
  sec("Checks that ran", html_table(flagged)),
  sec("Planted versus estimated", para("Headline window: the first 12 post-months. Coverage is for 90 percent intervals."),
      html_table(by_est), img("estimated_vs_planted", "Estimated versus planted savings by household"),
      img("distributions_planted_estimated_placebo", "Planted, estimated and placebo distributions"),
      "<h3>Breakouts for the primary estimator</h3>", html_table(breakouts)),
  sec("Variance decomposition", para("If estimation noise were independent of the planted effect, the estimated variance would equal the planted variance plus the placebo variance."),
      html_table(cbind(vd, rob))),
  sec("Seasonal profile", html_table(seas_cmp), img("seasonal_planted_vs_estimated", "Seasonal savings")),
  sec("Diagnostics", img("event_study", "Event study"), "<h3>Pre-trend tests by retailer</h3>",
      html_table(read_table(file.path(P_$out, "estimates", "pretrend_tests.csv"))),
      "<h3>Placebo</h3>", para(sprintf("Placebo households: %d, mean %.2f, SD %.2f, IQR %.2f gal/sqft/yr.", nrow(plac), mean(plac$estimate),
                                         sd(plac$estimate), IQR(plac$estimate))),
      "<h3>Meta-regression signs</h3>", html_table(mt)),
  sec("Other rebate types", para("Planted effects: 15 gallons per household per day for clothes washers and 10 percent of outdoor demand for irrigation controllers. Wide intervals are expected with about 50 to 60 households per type."),
      html_table(oth)),
  if (!is.null(scen_tab)) sec("Scenarios", para("The estimator is fixed while the planted world changes. Only the base scenario is held to the estimated-side gates."),
                              html_table(scen_tab[, .(scenario, estimator, n, bias, rmse, coverage90, spearman, slope)]), img("scenario_bias", "Scenario bias")) else "",
  if (file.exists(calf)) sec("Anchor calibration log", html_table(fread(calf))) else "",
  sec("Seed robustness", paste(vapply(seedf[file.exists(seedf)], function(f) paste0("<h3>", basename(f), "</h3>", html_table(fread(f))), ""), collapse = "")),
  sec("Edge cases", html_table(edge)),
  sec("All gates", para("MISS (documented) marks an estimated-side anchor that the frozen calibration does not reach; it is described in the README and not tuned away."),
      html_table(g2)),
  sec("Run times and peak memory", html_table(runs)),
  sec("Figures from the generation stages", paste0(vapply(c("climate_et0", "usage_spaghetti", "usage_seasonal_profile", "prior_predictive",
      "planted_savings_distribution", "planted_savings_seasonal", "planted_savings_by_year"), function(n) img(n, n), ""), collapse = "")),
  "</body></html>")
writeLines(html, file.path(P_$reports, "validation.html"))
finish_stage(ctx)
