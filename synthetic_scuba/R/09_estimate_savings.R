# Stage 7: estimate rebate savings with matched difference-in-differences, in the spirit of Schmitt, Tull and
# Atwater (2018). This script reads ONLY the observed tables in data/ and never the truth folder, so it behaves
# like an analyst with real data. Results go to estimates/.
#
# Event: R = rebate received date, P = post-inspection date. Pre-period = billing periods ending before R;
# post-period = billing periods starting on or after P; periods overlapping R..P are the installation window.
# Outcome: usage per 30.4375 days (CCF), so monthly and bimonthly periods share a scale.
# Household DiD: d_t = y_treated,t - mean(y_controls,t) ~ sin + cos (calendar month) + post, Newey-West errors;
# savings (gal/sqft/yr) = -beta_post * 12 * 748.052 / turf area.
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), "00_common.R"))
suppressPackageStartupMessages({ library(sandwich); library(fixest); library(metafor); library(parallel) })
ctx <- init_ctx("09_estimate_savings")
est_dir <- file.path(ctx$paths$out, "estimates"); dir.create(est_dir, showWarnings = FALSE)
E <- function(n) pv(ctx, "estimation", n)
G <- pv(ctx, "units", "gallons_per_ccf"); MDAYS <- 30.4375
NC <- pv(ctx, "matching", "n_controls")
PRE <- pv(ctx, "panel", "analysis_pre_months"); POST <- pv(ctx, "panel", "analysis_post_months")
zq <- qnorm(1 - (1 - E("interval_level")) / 2)
setFixest_nthreads(1); setFixest_notes(FALSE)

usage <- read_table(file.path(ctx$paths$data, "usage.parquet"))
customers <- read_table(file.path(ctx$paths$data, "customers.csv"))
rebates <- read_table(file.path(ctx$paths$data, "rebates.csv"))
acs <- read_table(file.path(ctx$paths$data, "acs_block_groups.csv"))

# ---- Cleaning, as an analyst would: drop negative reads and isolated spikes above five times the location median.
U <- usage[, .(location_id = location_id_from_utility, usage_start_date, usage_end_date, usage_ccf, usage_et_amount,
               budget = usage_indoor_budget_ccf + usage_outdoor_budget_ccf,
               days = as.integer(usage_end_date - usage_start_date) + 1L)]
U[, y := usage_ccf / days * MDAYS]
U[, med := median(y[usage_ccf >= 0]), by = location_id]
U[, flag := fifelse(usage_ccf < 0, "negative", fifelse(y > 5 * med & y > 40, "spike", "ok"))]
write_table(U[flag != "ok", .N, by = flag], file.path(est_dir, "cleaning_log.csv"))
U <- U[flag == "ok"]
U[, key := format(usage_start_date + 15L, "%Y-%m")]
U[, kmonth := as.integer(substr(key, 6, 7))]
U <- U[, .SD[1], by = .(location_id, key)]   # a key can repeat only for short split periods; keep the first

loc <- observed_locations(customers, pv(ctx, "observation", "bunching_min_count"))
loc <- merge(loc, observed_cycles(usage), by = "location_id")
bg <- customers[, .(block_group_id = block_group_id[1]), by = .(location_id = location_id_from_utility)]
loc <- merge(loc, merge(bg, acs[, .(block_group_id, median_hh_income)], by = "block_group_id"), by = "location_id", all.x = TRUE)
loc[, pool := paste(cust_loc_zip, billing, billing_cycle)]
types <- c(turf = "turf", hecw = "high efficiency clothes washer", wbic = "weather based irrigation controller")
holders <- lapply(types, function(ty) unique(rebates[rebate_instance_type == ty, location_id_from_utility]))

# Wide usage matrices by matching pool (zip code, billing frequency and cycle).
keys_all <- sort(unique(U$key))
pools <- split(loc[cust_loc_class == "single family residential" & !defaulted_area], by = "pool")
W <- lapply(pools, function(p) {
  x <- dcast(U[location_id %in% p$location_id], key ~ location_id, value.var = "y")
  m <- as.matrix(x[, -1]); rownames(m) <- x$key; m
})
key_month <- function(k) as.integer(substr(k, 6, 7))

# ---- Event windows for a location from its own observed periods.
event_windows <- function(lid, R, P) {
  u <- U[location_id == lid]
  pre <- u[usage_end_date < R, key]
  post <- u[usage_start_date >= P][order(usage_start_date)]
  nh <- if (nrow(post) && median(post$days) > 45) ceiling(E("headline_post_months") / 2) else E("headline_post_months")
  list(pre = pre, head = head(post$key, nh), allpost = post$key,
       head_start = if (nrow(post)) min(post$usage_start_date) else as.Date(NA),
       head_end = if (nrow(post)) post$usage_end_date[min(nh, nrow(post))] else as.Date(NA))
}

# Candidate controls for one event: same pool, not holding `exclude` rebates, not the unit itself, and observed in at
# least 90 percent of the unit's pre and headline periods.
candidates <- function(lid, pool, win, exclude) {
  m <- W[[pool]]
  cand <- setdiff(colnames(m), c(lid, exclude))
  ks <- intersect(c(win$pre, win$head), rownames(m))
  if (!length(cand) || !length(ks)) return(character(0))
  cov <- colMeans(!is.na(m[ks, cand, drop = FALSE]))
  cand[cov >= E("control_min_coverage")]
}

match_controls <- function(lid, pool, win, cand, method) {
  m <- W[[pool]]
  ks <- intersect(win$pre, rownames(m))
  if (length(cand) == 0 || length(ks) < 6) return(NULL)
  xt <- m[ks, lid]; M <- m[ks, cand, drop = FALSE]
  if (method == "corr_deseasonalized") {
    km <- key_month(ks)
    ds <- function(v) v - ave(v, km, FUN = function(z) mean(z, na.rm = TRUE))
    xt <- ds(xt); M <- apply(M, 2, ds)
    if (!is.matrix(M)) M <- matrix(M, ncol = length(cand), dimnames = list(ks, cand))
  }
  r <- suppressWarnings(as.vector(cor(xt, M, use = "pairwise.complete.obs")))
  lvl <- abs(colMeans(m[ks, cand, drop = FALSE], na.rm = TRUE) - mean(m[ks, lid], na.rm = TRUE))
  d <- data.table(control = cand, corr = r, level_diff = lvl,
                  ratio = colMeans(m[ks, cand, drop = FALSE], na.rm = TRUE) / mean(m[ks, lid], na.rm = TRUE))
  if (method == "corr_plus_level") d <- d[ratio >= 1 / E("level_factor") & ratio <= E("level_factor")]
  d <- d[!is.na(corr)][order(-corr, level_diff)]
  head(d, NC)[, rank := seq_len(.N)][]
}

# Household DiD on the difference series; `scale` converts the per-period CCF coefficient to the reported unit.
hh_did <- function(lid, pool, win, controls, post_keys, scale, log_outcome = FALSE, seasonal = FALSE, controls_mean = TRUE) {
  m <- W[[pool]]
  ks <- intersect(c(win$pre, post_keys), rownames(m))
  yt <- m[ks, lid]
  yc <- if (length(controls)) rowMeans(m[ks, controls, drop = FALSE], na.rm = TRUE) else rep(0, length(ks))
  if (log_outcome) { yt <- log(yt + 1); yc <- if (length(controls)) rowMeans(log(m[ks, controls, drop = FALSE] + 1), na.rm = TRUE) else 0 }
  d <- data.table(pkey = ks, dy = yt - yc, post = as.integer(ks %in% post_keys), mo = key_month(ks))[is.finite(dy)]
  if (sum(d$post) < 2 || sum(1 - d$post) < 6) return(NULL)
  d[, `:=`(s = sin(2 * pi * mo / 12), c = cos(2 * pi * mo / 12))]
  bim <- median(diff(sort(match(d$pkey, keys_all)))) > 1
  lag <- if (bim) ceiling(E("newey_west_lag") / 2) else E("newey_west_lag")
  f <- if (seasonal) dy ~ s + c + post + post:s + post:c else dy ~ s + c + post
  fit <- lm(f, data = d)
  V <- tryCatch(NeweyWest(fit, lag = lag, prewhite = FALSE), error = function(e) vcov(fit))
  b <- coef(fit)
  if (!seasonal) {
    est <- -b[["post"]] * scale; se <- sqrt(V["post", "post"]) * abs(scale)
    return(list(estimate = est, se = se, n_pre = sum(d$post == 0), n_post = sum(d$post)))
  }
  mo <- 1:12
  X <- cbind(1, sin(2 * pi * mo / 12), cos(2 * pi * mo / 12))
  idx <- c("post", "s:post", "c:post")
  idx <- vapply(idx, function(n) if (n %in% names(b)) n else sub("(.):post", "post:\\1", n), "")
  prof <- -as.vector(X %*% b[idx]) * scale
  list(profile = prof, coef = b[idx], V = V[idx, idx])
}

# ---- Turf cohort and windows.
co <- analysis_cohort(usage, customers, rebates, "turf", PRE, POST, pv(ctx, "observation", "bunching_min_count"))
write_table(co, file.path(est_dir, "turf_cohort.csv"), "location_id")
tc <- merge(co[passes_filters == TRUE], loc[, .(location_id, pool, billing, billing_cycle, median_hh_income)], by = "location_id")
tc <- tc[pool %in% names(W)]
setorder(tc, location_id)
wins <- lapply(seq_len(nrow(tc)), function(i) event_windows(tc$location_id[i], tc$received_date[i], tc$post_date[i]))
names(wins) <- tc$location_id
tc[, `:=`(head_start = as.Date(vapply(wins, function(w) as.numeric(w$head_start), 0)),
          head_end = as.Date(vapply(wins, function(w) as.numeric(w$head_end), 0)))]
other_holders <- unique(c(holders$hecw, holders$wbic))

# Co-treatment: another rebate posted between the start of the pre window and the end of the headline window.
oth <- rebates[rebate_instance_type != "turf", .(location_id = location_id_from_utility, opost = rebate_instance_post_date)]
tc[, cotreated_in_window := location_id %in% oth[tc[, .(location_id, s = head_start - 365L * 2L, e = head_end)],
                                                  on = .(location_id, opost >= s, opost <= e), nomatch = NULL, location_id]]

# ---- Matching (all three methods) and the household estimators.
row_of <- function(estimator, window, r) data.table(estimator = estimator, window = window, estimate = r$estimate, se = r$se,
                                                    n_pre = r$n_pre, n_post = r$n_post)
run_turf <- function(i) {
  lid <- tc$location_id[i]; pool <- tc$pool[i]; w <- wins[[lid]]; q <- tc$quantity[i]
  sc <- 12 * G / q
  cand <- candidates(lid, pool, w, holders$turf)
  res <- list(); mats <- list()
  for (mm in E("match_methods")) {
    mt <- match_controls(lid, pool, w, cand, mm)
    if (is.null(mt) || !nrow(mt)) next
    mt[, `:=`(location_id = lid, method = mm, control_other_rebate = control %in% other_holders)]
    mats[[mm]] <- mt
    for (win_name in c("headline", "allpost")) {
      pk <- if (win_name == "headline") w$head else w$allpost
      r <- hh_did(lid, pool, w, mt$control, pk, sc)
      if (!is.null(r)) res[[length(res) + 1]] <- row_of(paste0("matched_", mm), win_name, r)
    }
  }
  prim <- mats[[E("match_method")]]
  if (!is.null(prim)) {
    s <- hh_did(lid, pool, w, prim$control, w$head, G / q, seasonal = TRUE)
    sea <- if (!is.null(s)) data.table(location_id = lid, month = 1:12, est_gal_per_sqft_month = s$profile) else NULL
    rl <- hh_did(lid, pool, w, prim$control, w$head, 1, log_outcome = TRUE)
    if (!is.null(rl)) {
      base <- mean(W[[pool]][intersect(w$pre, rownames(W[[pool]])), lid], na.rm = TRUE)
      res[[length(res) + 1]] <- data.table(estimator = "matched_log", window = "headline", estimate = rl$estimate * base * sc,
                                           se = rl$se * base * sc, n_pre = rl$n_pre, n_post = rl$n_post)
    }
    noth <- match_controls(lid, pool, w, setdiff(cand, other_holders), E("match_method"))
    if (!is.null(noth) && nrow(noth)) {
      r <- hh_did(lid, pool, w, noth$control, w$head, sc)
      if (!is.null(r)) res[[length(res) + 1]] <- row_of("matched_no_other_rebate_controls", "headline", r)
    }
  } else sea <- NULL
  r <- hh_did(lid, pool, w, character(0), w$head, sc)
  if (!is.null(r)) res[[length(res) + 1]] <- row_of("naive_pre_post", "headline", r)
  r <- hh_did(lid, pool, w, cand, w$head, sc)
  if (!is.null(r)) res[[length(res) + 1]] <- row_of("unmatched_did", "headline", r)
  list(est = rbindlist(res)[, location_id := lid], matches = rbindlist(mats), seasonal = sea,
       n_candidates = length(cand))
}
out <- mclapply(seq_len(nrow(tc)), run_turf, mc.cores = ctx$opt$cores)
est <- rbindlist(lapply(out, `[[`, "est"))
matches <- rbindlist(lapply(out, `[[`, "matches"))
seas <- rbindlist(lapply(out, `[[`, "seasonal"))
tc[, n_candidates := vapply(out, `[[`, 0L, "n_candidates")]
est[, `:=`(lo90 = estimate - zq * se, hi90 = estimate + zq * se)]
share_oth <- matches[method == E("match_method"), .(share_controls_other_rebate = mean(control_other_rebate)), by = location_id]
est <- merge(est, tc[, .(location_id, utility_name, billing, billing_cycle, cust_loc_zip, quantity, received_date, post_date,
                         head_start, head_end, cotreated_in_window, n_candidates)], by = "location_id")
est <- merge(est, share_oth, by = "location_id", all.x = TRUE)
est[, primary := estimator == paste0("matched_", E("match_method")) & window == "headline"]
write_table(est, file.path(est_dir, "household_estimates.csv"), c("location_id", "estimator", "window"))
write_table(matches[, .(method, location_id, rank, control, corr, level_diff, control_other_rebate)],
            file.path(est_dir, "matches.csv"), c("method", "location_id", "rank"))
write_table(seas, file.path(est_dir, "seasonal_estimates.csv"), c("location_id", "month"))

# ---- Pooled stacked DiD (fixest) and the event study.
# sel: data.table with location_id, pool and billing; wl and cl: lists of windows and control vectors in the same order.
stack_blocks <- function(sel, wl, cl, outcome_div = NULL, pre_months_limit = 24) {
  rbindlist(lapply(seq_len(nrow(sel)), function(i) {
    lid <- sel$location_id[i]; pool <- sel$pool[i]; ew <- wl[[i]]
    ctrl <- cl[[i]]
    m <- W[[pool]]
    pre_keep <- tail(sort(ew$pre), if (sel$billing[i] == "bimonthly") pre_months_limit / 2 else pre_months_limit)
    ks <- intersect(c(pre_keep, ew$head), rownames(m))
    units <- c(lid, ctrl)
    dv <- if (is.null(outcome_div)) 1 else outcome_div[i]
    d <- data.table(block = lid, pkey = rep(ks, length(units)), unit = rep(units, each = length(ks)),
                    y = as.vector(m[ks, units]) / dv, treated = rep(c(1L, rep(0L, length(ctrl))), each = length(ks)),
                    w = rep(c(1, rep(1 / length(ctrl), length(ctrl))), each = length(ks)))
    d[, post := as.integer(pkey %in% ew$head)]
    kidx <- match(d$pkey, keys_all); first_post <- min(match(ew$head, keys_all)); last_pre <- max(match(pre_keep, keys_all))
    step <- if (sel$billing[i] == "bimonthly") 2L else 1L
    d[, event_bin := fifelse(post == 1L, (kidx - first_post) %/% 3L, -((last_pre - kidx) %/% 3L) - 1L)]
    d[, mo := key_month(pkey)]
    d[!is.na(y)]
  }))
}
prim_m <- matches[method == E("match_method")]
sel <- tc[location_id %in% prim_m$location_id]
sel_ctrl <- lapply(sel$location_id, function(l) prim_m[location_id == l, control])
sel_win <- wins[sel$location_id]
st <- stack_blocks(sel, sel_win, sel_ctrl, sel$quantity)
st[, `:=`(unit_block = paste(unit, block), period_block = paste(pkey, block), treat_post = treated * post)]
st <- merge(st, sel[, .(block = location_id, utility_name)], by = "block")
fs <- feols(y ~ treat_post | unit_block + period_block, data = st, weights = ~w, cluster = ~block)
pooled <- data.table(model = "stacked_did_fixest", estimate = -coef(fs)[["treat_post"]] * 12 * G,
                     se = se(fs)[["treat_post"]] * 12 * G, n_blocks = uniqueN(st$block))
st[, `:=`(ps = treat_post * sin(2 * pi * mo / 12), pc = treat_post * cos(2 * pi * mo / 12))]
fs2 <- feols(y ~ treat_post + ps + pc | unit_block + period_block, data = st, weights = ~w, cluster = ~block)
bb <- coef(fs2); VV <- vcov(fs2)
Xm <- cbind(1, sin(2 * pi * (1:12) / 12), cos(2 * pi * (1:12) / 12))
pooled_seas <- data.table(month = 1:12, est_gal_per_sqft_month = -as.vector(Xm %*% bb) * G,
                          se = sqrt(diag(Xm %*% VV %*% t(Xm))) * G)
write_table(pooled_seas, file.path(est_dir, "pooled_seasonal_profile.csv"), "month")

hp <- est[primary == TRUE & is.finite(se) & se > 0]
rm0 <- rma(yi = hp$estimate, sei = hp$se, method = "REML")
pooled <- rbind(pooled, data.table(model = "metafor_rma_household", estimate = as.numeric(rm0$b), se = rm0$se, n_blocks = nrow(hp)))
write_table(pooled, file.path(est_dir, "pooled_estimates.csv"))

# Meta-regression on observed covariates; signs are compared with the planted regression in 10_validate.
effo <- merge(U[, .(location_id, usage_end_date, usage_ccf, budget)], tc[, .(location_id, received_date)], by = "location_id")[
  usage_end_date < received_date & !is.na(budget), .(eff_pre_observed = mean(log((usage_ccf + 1) / (budget + 1)))), by = location_id]
etp <- merge(U[, .(location_id, key, usage_et_amount, days)], rbindlist(lapply(names(wins), function(l) data.table(location_id = l, pkey = wins[[l]]$head)))[, .(location_id, key = pkey)],
             by = c("location_id", "key"))[, .(et_post = sum(usage_et_amount) / sum(days) * MDAYS), by = location_id]
mr <- merge(hp[, .(location_id, estimate, se, quantity)], effo, by = "location_id")
mr <- merge(mr, etp, by = "location_id")
mr <- merge(mr, loc[, .(location_id, irr_area, median_hh_income)], by = "location_id")
mr <- mr[!is.na(irr_area) & irr_area > 0]
mr[, `:=`(log_quantity = log(quantity), log_area_ratio = log(quantity / irr_area), log_income = log(median_hh_income))]
mreg <- rma(yi = estimate, sei = se, mods = ~ eff_pre_observed + log_quantity + log_area_ratio + log_income + et_post, data = mr, method = "REML")
meta_tab <- data.table(term = rownames(mreg$beta), estimate = as.numeric(mreg$beta), se = mreg$se, pval = mreg$pval, n = nrow(mr))
write_table(meta_tab, file.path(est_dir, "meta_regression.csv"))
write_table(mr, file.path(est_dir, "meta_regression_data.csv"), "location_id")

# Event study in 3-month bins (reference: the last pre bin) and pre-trend tests by retailer.
st[, bin_f := factor(event_bin)]
es <- feols(y ~ i(event_bin, treated, ref = -1) | unit_block + period_block, data = st, weights = ~w, cluster = ~block)
es_tab <- data.table(term = names(coef(es)), estimate = -coef(es) * 12 * G, se = se(es) * 12 * G)
es_tab[, event_bin := as.integer(sub(".*::(-?[0-9]+):.*", "\\1", term))]
write_table(es_tab[order(event_bin)], file.path(est_dir, "event_study.csv"))
pretrend <- rbindlist(lapply(sort(unique(st$utility_name)), function(r) {
  f <- tryCatch(feols(y ~ i(event_bin, treated, ref = -1) | unit_block + period_block, data = st[utility_name == r],
                      weights = ~w, cluster = ~block), error = function(e) NULL)
  if (is.null(f)) return(data.table(utility_name = r, n_blocks = st[utility_name == r, uniqueN(block)], wald_stat = NA_real_, df = NA_integer_,
                                    pval = NA_real_, wald_reliable = FALSE))
  nm <- names(coef(f)); pre <- nm[as.integer(sub(".*::(-?[0-9]+):.*", "\\1", nm)) < -1]
  b <- coef(f)[pre]; V <- vcov(f)[pre, pre, drop = FALSE]
  ws <- tryCatch(as.numeric(t(b) %*% solve(V, b)), error = function(e) NA_real_)
  nb <- st[utility_name == r, uniqueN(block)]
  # A single-degree-of-freedom linear pre-trend test (treated-by-event-time slope in the pre-period) stays well
  # conditioned with few blocks, unlike the joint Wald test, which needs many more clusters than coefficients.
  pre_d <- st[utility_name == r & post == 0L][, trend_t := treated * event_bin]
  fl <- tryCatch(feols(y ~ trend_t | unit_block + period_block, data = pre_d, weights = ~w, cluster = ~block), error = function(e) NULL)
  data.table(utility_name = r, n_blocks = nb,
             linear_slope_gal_per_sqft_yr_per_bin = if (is.null(fl)) NA_real_ else -coef(fl)[["trend_t"]] * 12 * G,
             linear_slope_se = if (is.null(fl)) NA_real_ else se(fl)[["trend_t"]] * 12 * G,
             linear_pval = if (is.null(fl)) NA_real_ else pvalue(fl)[["trend_t"]],
             wald_stat = ws, df = length(pre), pval = pchisq(ws, length(pre), lower.tail = FALSE),
             wald_reliable = nb >= 5 * length(pre))
}))
write_table(pretrend, file.path(est_dir, "pretrend_tests.csv"), "utility_name")

# ---- Placebo: each distinct matched control receives a fake rebate drawn from the treated date distribution
# and is analyzed exactly like a treated household. The spread of these estimates is the method's noise floor.
pl_units <- head(sort(unique(prim_m$control)), E("max_placebo"))
pl_units <- setdiff(pl_units, holders$turf)
donors <- tc[, .(received_date, post_date, quantity)]
run_placebo <- function(cid) {
  use_stream(ctx, "placebo", cid)
  pool <- loc[location_id == cid, pool]
  for (k in 1:10) {
    dd <- donors[sample.int(nrow(donors), 1)]
    w <- event_windows(cid, dd$received_date, dd$post_date)
    pre_m <- U[location_id == cid & usage_end_date < dd$received_date, sum(days)] / MDAYS
    post_m <- U[location_id == cid & usage_start_date >= dd$post_date, sum(days)] / MDAYS
    if (round(pre_m) >= PRE && round(post_m) >= POST) break
    w <- NULL
  }
  if (is.null(w)) return(NULL)
  cand <- candidates(cid, pool, w, holders$turf)
  mt <- match_controls(cid, pool, w, cand, E("match_method"))
  if (is.null(mt) || !nrow(mt)) return(NULL)
  r <- hh_did(cid, pool, w, mt$control, w$head, 12 * G / dd$quantity)
  if (is.null(r)) return(NULL)
  data.table(location_id = cid, fake_received_date = dd$received_date, fake_post_date = dd$post_date, quantity = dd$quantity,
             estimate = r$estimate, se = r$se)
}
plac <- rbindlist(mclapply(pl_units, run_placebo, mc.cores = ctx$opt$cores))
register_keys(ctx, "placebo", pl_units)
write_table(plac, file.path(est_dir, "placebo_estimates.csv"), "location_id")

# ---- Other rebate types with the same machinery.
other_out <- list(); other_seas <- list(); other_tests <- list()
for (ty in c("hecw", "wbic")) {
  oc <- analysis_cohort(usage, customers, rebates, types[[ty]], PRE, POST, pv(ctx, "observation", "bunching_min_count"))
  oc <- merge(oc[passes_filters == TRUE], loc[, .(location_id, pool, billing)], by = "location_id")[pool %in% names(W)]
  if (!nrow(oc)) next
  excl <- unique(c(holders$turf, holders[[setdiff(c("hecw", "wbic"), ty)]]))
  rows <- rbindlist(lapply(seq_len(nrow(oc)), function(i) {
    lid <- oc$location_id[i]; w <- event_windows(lid, oc$received_date[i], oc$post_date[i])
    cand <- candidates(lid, oc$pool[i], w, c(holders[[ty]], excl))
    mt <- match_controls(lid, oc$pool[i], w, cand, E("match_method"))
    if (is.null(mt) || !nrow(mt)) return(NULL)
    r <- hh_did(lid, oc$pool[i], w, mt$control, w$head, 1)
    if (is.null(r)) return(NULL)
    base <- W[[oc$pool[i]]][intersect(w$pre, rownames(W[[oc$pool[i]]])), lid]
    km <- key_month(intersect(w$pre, rownames(W[[oc$pool[i]]])))
    mmean <- tapply(base, km, mean, na.rm = TRUE)
    data.table(type = ty, location_id = lid, billing = oc$billing[i], saving_ccf_per_month = r$estimate, se = r$se,
               pre_mean_ccf = mean(base, na.rm = TRUE), outdoor_est_ccf = mean(pmax(mmean - min(mmean, na.rm = TRUE), 0), na.rm = TRUE),
               pool = oc$pool[i], ctl = paste(mt$control, collapse = ";"), received_date = oc$received_date[i], post_date = oc$post_date[i])
  }))
  if (!nrow(rows)) next
  owl <- lapply(seq_len(nrow(rows)), function(i) event_windows(rows$location_id[i], rows$received_date[i], rows$post_date[i]))
  so <- stack_blocks(rows, owl, strsplit(rows$ctl, ";", fixed = TRUE))
  so[, `:=`(unit_block = paste(unit, block), period_block = paste(pkey, block), treat_post = treated * post)]
  so[, `:=`(ps = treat_post * sin(2 * pi * mo / 12), pc = treat_post * cos(2 * pi * mo / 12))]
  f1 <- feols(y ~ treat_post | unit_block + period_block, data = so, weights = ~w, cluster = ~block)
  f2 <- feols(y ~ treat_post + ps + pc | unit_block + period_block, data = so, weights = ~w, cluster = ~block)
  b2 <- coef(f2); V2 <- vcov(f2)
  prof <- data.table(type = ty, month = 1:12, saving_ccf_per_month = -as.vector(Xm %*% b2), se = sqrt(diag(Xm %*% V2 %*% t(Xm))))
  hv <- c("ps", "pc"); wstat <- as.numeric(t(b2[hv]) %*% solve(V2[hv, hv], b2[hv]))
  # Summer peak: mean saving in July-September minus January-March, with its standard error.
  cvec <- colMeans(Xm[7:9, ]) - colMeans(Xm[1:3, ])
  diff_js <- -sum(cvec * b2); se_js <- sqrt(as.numeric(t(cvec) %*% V2 %*% cvec))
  pooled_ccf <- -coef(f1)[["treat_post"]]; pooled_se <- se(f1)[["treat_post"]]
  conv <- if (ty == "hecw") G / MDAYS else 1 / mean(rows$outdoor_est_ccf)
  other_tests[[ty]] <- data.table(type = ty, n_households = nrow(rows), pooled_saving_ccf_per_month = pooled_ccf, pooled_se_ccf = pooled_se,
    converted_estimate = pooled_ccf * conv, converted_lo90 = (pooled_ccf - zq * pooled_se) * conv, converted_hi90 = (pooled_ccf + zq * pooled_se) * conv,
    converted_unit = if (ty == "hecw") "gallons per household per day" else "share of estimated outdoor demand",
    seasonal_wald_stat = wstat, seasonal_pval = pchisq(wstat, 2, lower.tail = FALSE),
    summer_minus_winter_ccf = diff_js, summer_minus_winter_se = se_js)
  other_out[[ty]] <- rows[, .(type, location_id, billing, saving_ccf_per_month, se, pre_mean_ccf, outdoor_est_ccf)]
  other_seas[[ty]] <- prof
}
write_table(rbindlist(other_out), file.path(est_dir, "other_rebate_household_estimates.csv"), c("type", "location_id"))
write_table(rbindlist(other_seas), file.path(est_dir, "other_rebate_seasonal_profile.csv"), c("type", "month"))
write_table(rbindlist(other_tests), file.path(est_dir, "other_rebate_estimates.csv"), "type")

# ---- Optional BSTS (CausalImpact) comparison with the paper's method.
if (isTRUE(ctx$opt$causal)) {
  suppressPackageStartupMessages(library(CausalImpact))
  bdir <- file.path(est_dir, "bsts_parts"); dir.create(bdir, showWarnings = FALSE)
  run_bsts <- function(i) {
    lid <- sel$location_id[i]; f <- file.path(bdir, paste0(lid, ".csv"))
    if (file.exists(f)) return(fread(f))
    w <- sel_win[[i]]; m <- W[[sel$pool[i]]]
    ks <- intersect(c(w$pre, w$head), rownames(m))
    X <- m[ks, c(lid, sel_ctrl[[i]]), drop = FALSE]
    X[, -1][is.na(X[, -1])] <- rowMeans(X[, -1], na.rm = TRUE)[row(X[, -1])][is.na(X[, -1])]
    ok <- complete.cases(X); X <- X[ok, , drop = FALSE]; ks <- ks[ok]
    pre_idx <- which(!ks %in% w$head); post_idx <- which(ks %in% w$head)
    if (length(pre_idx) < 6 || length(post_idx) < 2) return(NULL)
    set.seed(stream_int(ctx$seed, "bsts", lid))
    ci <- tryCatch(CausalImpact(X, c(1, max(pre_idx)), c(min(post_idx), max(post_idx)),
                                model.args = list(niter = pv(ctx, "compute", "causal_bsts_iter"), nseasons = 1)), error = function(e) NULL)
    if (is.null(ci)) return(NULL)
    s <- ci$summary["Average", ]
    r <- data.table(location_id = lid, estimator = "bsts_causalimpact", window = "headline",
                    estimate = -s$AbsEffect * 12 * G / sel$quantity[i], se = s$AbsEffect.sd * 12 * G / sel$quantity[i])
    fwrite(r, f); r
  }
  bs <- rbindlist(mclapply(seq_len(nrow(sel)), run_bsts, mc.cores = ctx$opt$cores))
  write_table(bs, file.path(est_dir, "bsts_household_estimates.csv"), "location_id")
}

# ---- Estimated-side gates (anchors from the paper, tolerances assumed).
ge <- pv(ctx, "gates", "estimated")
hp <- est[primary == TRUE]
mn <- mean(hp$estimate); qq <- quantile(hp$estimate, c(0.25, 0.5, 0.75))
jas <- seas[month %in% 7:9, mean(est_gal_per_sqft_month)]; jfm <- seas[month %in% 1:3, mean(est_gal_per_sqft_month)]
ctx <- gate(ctx, "estimated annual mean (gal/sqft/yr)", in_band(mn, ge$annual_mean), mn, paste(ge$annual_mean, collapse = " to "), blocking = FALSE)
ctx <- gate(ctx, "estimated July-September mean (gal/sqft/month)", in_band(jas, ge$jas_mean), jas, paste(ge$jas_mean, collapse = " to "), blocking = FALSE)
ctx <- gate(ctx, "estimated January-March mean (gal/sqft/month)", in_band(jfm, ge$jfm_mean), jfm, paste(ge$jfm_mean, collapse = " to "), blocking = FALSE)
ctx <- gate(ctx, "estimated median", in_band(qq[2], ge$median), qq[2], paste(ge$median, collapse = " to "), blocking = FALSE)
ctx <- gate(ctx, "estimated 25th percentile", in_band(qq[1], ge$p25), qq[1], paste(ge$p25, collapse = " to "), blocking = FALSE)
ctx <- gate(ctx, "estimated 75th percentile", in_band(qq[3], ge$p75), qq[3], paste(ge$p75, collapse = " to "), blocking = FALSE)
ctx <- share_gate(ctx, "estimated share above 60 gal/sqft/yr", sum(hp$estimate > 60), nrow(hp), ge$share_above_60, blocking = FALSE)
ctx <- share_gate(ctx, "estimated share at or below zero", sum(hp$estimate <= 0), nrow(hp), ge$share_at_or_below_zero, blocking = FALSE)
ctx <- gate(ctx, "placebo estimates centered on zero", abs(mean(plac$estimate)) < 2 * sd(plac$estimate) / sqrt(nrow(plac)) + 1,
            c(mean(plac$estimate), sd(plac$estimate), nrow(plac)), "|mean| < 2 SE + 1 gal/sqft/yr")
ctx <- gate(ctx, "households estimated (reported)", TRUE, c(nrow(hp), nrow(tc)), "estimated, cohort")
summ <- est[window == "headline", .(n = .N, mean = mean(estimate), median = median(estimate), sd = sd(estimate)), by = estimator]
write_table(summ, file.path(est_dir, "estimator_summary.csv"), "estimator")
moments <- data.table(annual_mean = mn, median = qq[[2]], p25 = qq[[1]], p75 = qq[[3]], share_above_60 = mean(hp$estimate > 60),
                      share_at_or_below_zero = mean(hp$estimate <= 0), jas_mean = jas, jfm_mean = jfm, seasonal_ratio = jas / jfm,
                      sd_estimate = sd(hp$estimate), sd_placebo = sd(plac$estimate), n_households = nrow(hp))
write_table(moments, file.path(est_dir, "estimated_moments.csv"))
writeLines(c("# estimates/", "", "Matched difference-in-differences estimates computed by R/09_estimate_savings.R from the observed",
             "tables in data/ only. The script never reads truth/. THE UNDERLYING DATA IS SIMULATED."), file.path(est_dir, "README.md"))
finish_stage(ctx)
