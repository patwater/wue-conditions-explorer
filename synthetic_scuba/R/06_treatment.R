# Stage 4: rebate selection, turf removal, planted savings and the other rebate types.
# "Planted" effects are assumptions written into the data-generating process. Nobody knows the savings of real
# households; the generator knows these only because it planted them.
#
# Selection:  logit P(rebate) = b0 + b_inc*log(income/80k) + b_area*log(area/4000) + b_eff*eff_2010_11 + b_att*a_i,
#             with b0 calibrated so the post-filter analysis cohort hits the configured target.
# Planted savings: lambda_it (gal/sqft/yr) = structural_i + behavioral_k(t)
#             structural_i = b0_s + b_eff*(eff_i) + b_q*log(q/1200) + b_ar*log(ar/0.25) + b_inc*log(income/80k)
#                            + split-normal skew (sd_lower below the median, sd_upper above)
#             behavioral_k = sigma_beh*Z_k (+ extreme bump for the rebate occupant), redrawn on turnover with corr rho
#             g_it (gal/sqft/month) = lambda_it/12*(1 + kappa*cos(2*pi*(month - peak)/12))
#                                     + b_et*(ET0_rt - mean ET0_r) + e_it
#             savings_it (gal) = min(g_it * q_i * exp(-decay * years), untreated outdoor demand after other rebates),
#             for months starting on or after the post-inspection date. b0_s is calibrated so the capped annual
#             mean over the first 12 post-months hits the target (provisional; see the anchor calibration).
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), "00_common.R"))
ctx <- init_ctx("06_treatment")

ret <- read_table(wpath(ctx, "retailers.csv"))
bgs <- read_table(wpath(ctx, "block_groups.csv"))
locs <- read_table(wpath(ctx, "locations.csv"))
ten <- read_table(wpath(ctx, "tenures.csv"))
pan <- read_table(wpath(ctx, "usage_untreated.parquet"))
clim <- read_table(file.path(ctx$paths$truth, "climate_monthly.parquet"))
tm <- ctx$time; nT <- nrow(tm)
G <- pv(ctx, "units", "gallons_per_ccf"); G748 <- pv(ctx, "billing", "owrs_gal_per_ccf")
setkey(pan, location_id, t)

# ---- Monthly water budgets for single family homes (OWRS formula with the actual household size and area).
sf_pan <- pan[cust_class == "SF"]
sf_pan <- merge(sf_pan, ten[, .(cust_loc_id, hhsize, attitude)], by = "cust_loc_id")
sf_pan <- merge(sf_pan, locs[, .(location_id, irr_area_true)], by = "location_id")
sf_pan <- merge(sf_pan, clim[, .(code, t, usage_et_amount, eto_in)], by = c("code", "t"))
sf_pan[, budget := hhsize * pv(ctx, "billing", "budget_gpcd") * days / G748 +
         pv(ctx, "billing", "landscape_factor") * usage_et_amount * irr_area_true * pv(ctx, "billing", "et_factor") / G748]
sf_pan[, eff_term := log((y_untreated + 1) / (budget + 1))]
setkey(sf_pan, location_id, t)

# ---- Candidates: single family locations present in January 2010 (new construction is not eligible).
cand <- locs[cust_class == "SF" & is_new_construction == FALSE,
             .(location_id, code, block_group_id, irr_area_true, first_t, last_t)]
cand <- merge(cand, bgs[, .(block_group_id, median_hh_income, cust_loc_zip)], by = "block_group_id")
cand <- merge(cand, sf_pan[t <= 24, .(eff_2010_11 = mean(eff_term)), by = location_id], by = "location_id")
tw <- pv(ctx, "treatment", "timing")
win <- function(w) month_index(ctx, as.Date(unlist(w)))
we <- win(tw$early_window); wm <- win(tw$main_window); wl <- win(tw$late_window)
rl <- pv(ctx, "treatment", "received_lead_days"); cl <- pv(ctx, "treatment", "check_lag_days")
cand[, `:=`(t_rebate = NA_integer_, post_day = NA_integer_, u_sel = NA_real_, lead = NA_integer_, lag = NA_integer_)]
for (i in seq_len(nrow(cand))) {
  use_stream(ctx, "rebate_timing", cand$location_id[i])
  u <- runif(6)
  tr <- if (u[1] < tw$early_share) {
    we[1] + floor(u[2] * (we[2] - we[1] + 1))
  } else if (u[1] < tw$early_share + tw$main_share) {
    a <- tw$main_ramp
    x <- (-1 + sqrt(1 + 2 * a * u[2] * (1 + a / 2))) / a
    wm[1] + min(floor(x * (wm[2] - wm[1] + 1)), wm[2] - wm[1])
  } else wl[1] + floor(u[2] * (wl[2] - wl[1] + 1))
  set(cand, i, c("t_rebate", "post_day", "u_sel", "lead", "lag"),
      list(as.integer(tr), as.integer(1 + floor(u[3] * tm$days[tr])), u[4],
           as.integer(rl[1] + floor(u[5] * (rl[2] - rl[1] + 1))), as.integer(cl[1] + floor(u[6] * (cl[2] - cl[1] + 1)))))
}
register_keys(ctx, "rebate_timing", cand$location_id)
cand[, post_date := tm$month_start[t_rebate] + post_day - 1L]
cand[, `:=`(received_date = post_date - lead, check_date = post_date + lag)]
# Planted savings begin with the first month that starts on or after the post-inspection date.
cand[, t_effect := t_rebate + as.integer(post_day > 1L)]
cand <- cand[last_t >= t_effect]   # the property must still exist after the rebate

# Occupant at the rebate month supplies the attitude that drives selection.
occ_at <- ten[cand[, .(location_id, t_rebate)], on = .(location_id, start_t <= t_rebate, end_t >= t_rebate),
              .(location_id, cust_loc_id, cust_id_from_utility, attitude, tenure_index)]
cand <- merge(cand, occ_at, by = "location_id")

# ---- Observation plan and billing periods, used to calibrate against the post-filter count.
plan <- observation_plan(ctx, merge(locs, ret[, .(code)], by = "code"), ten)
write_table(plan$locations, wpath(ctx, "obs_plan_locations.csv"), "location_id")
write_table(plan$tenures, wpath(ctx, "obs_plan_tenures.csv"), "cust_loc_id")
register_keys(ctx, "obs_location", locs$location_id)
register_keys(ctx, "obs_tenure", plan$tenures[code %in% names(pv(ctx, "observation", "missing_hhsize_share")), cust_loc_id])
bp_in <- merge(cand[, .(location_id)], locs[, .(location_id, code, first_t, last_t)], by = "location_id")
bp_in <- merge(bp_in, ret[, .(code, billing)], by = "code")
bp_in <- merge(bp_in, ten[, .(tenure_starts = list(start_t)), by = location_id], by = "location_id")
periods <- billing_periods(ctx, bp_in)
miss <- period_missing(ctx, periods, plan$locations)
periods_obs <- merge(periods, miss, by = c("location_id", "period_index"))[missing == FALSE]
pp_months <- count_pre_post_months(periods_obs, cand[, .(location_id, received_date, post_date)])
cand <- merge(cand, pp_months, by = "location_id", all.x = TRUE)
cand[is.na(pre_months), `:=`(pre_months = 0, post_months = 0)]
# Defaulted areas are detected the way an analyst would, by bunching of the area on file, which can also flag a
# home whose measured area happens to equal the default value.
onfile <- merge(locs[cust_class == "SF", .(location_id, code, irr_area_true)],
                plan$locations[, .(location_id, default_area, default_area_value, missing_area)], by = "location_id")
onfile[, area_on_file := fifelse(default_area, default_area_value, irr_area_true)]
nsf <- locs[cust_class == "SF", .(n_sf = .N), by = code]
bunch <- merge(onfile[missing_area == FALSE, .N, by = .(code, area_on_file)], nsf, by = "code")[
  N >= bunch_threshold(n_sf, pv(ctx, "observation", "bunching_min_count"))]
onfile[, area_bunched := FALSE]
onfile[bunch, on = .(code, area_on_file), area_bunched := missing_area == FALSE]
cand <- merge(cand, plan$locations[, .(location_id, default_area, missing_area, duplicate_turf)], by = "location_id")
cand <- merge(cand, onfile[, .(location_id, area_bunched)], by = "location_id")
cand[, passes_filters := pre_months >= pv(ctx, "panel", "analysis_pre_months") &
       post_months >= pv(ctx, "panel", "analysis_post_months") & !area_bunched & !duplicate_turf]

# ---- Selection with the intercept calibrated exactly: a candidate is selected when b0 > c_i, so the cohort
# size is the number of passing candidates with c_i < b0.
sc <- pv(ctx, "treatment", "selection_coefs")
cand[, xb := sc$log_income * log(median_hh_income / 80000) + sc$log_area * log(irr_area_true / 4000) +
       sc$efficiency * eff_2010_11 + sc$attitude * attitude]
cand[, c_i := qlogis(u_sel) - xb]
target <- ctx$profile$cohort_target
cp <- sort(cand[passes_filters == TRUE, c_i])
if (length(cp) <= target) stop("Not enough passing candidates to reach the cohort target")
b0 <- (cp[target] + cp[target + 1]) / 2
cand[, p_select := plogis(b0 + xb)]
cand[, treated := c_i < b0]
setorder(cand, location_id)
message(sprintf("  selection intercept %.3f, treated %d of %d candidates (%.1f%%), passing %d",
                b0, sum(cand$treated), nrow(cand), 100 * mean(cand$treated), sum(cand$treated & cand$passes_filters)))

# ---- Turf area removed.
ta <- pv(ctx, "treatment", "turf_area")
tr <- cand[treated == TRUE]
tr[, quantity := NA_real_]
for (i in seq_len(nrow(tr))) {
  use_stream(ctx, "turf", tr$location_id[i])
  q <- round(exp(log(ta$median) + ta$sdlog * rnorm(1)))
  cap <- floor(ta$cap_frac * tr$irr_area_true[i])
  set(tr, i, "quantity", as.numeric(min(max(q, ta$floor), cap)))
}
register_keys(ctx, "turf", tr$location_id)
tr[, area_ratio := quantity / irr_area_true]

# Pre-removal efficiency known to the generator: all months before the rebate month, clean usage, actual budgets.
eff_pre <- sf_pan[tr[, .(location_id, t_rebate)], on = .(location_id, t < t_rebate), .(location_id, eff_term)][
  , .(eff_pre_clean = mean(eff_term)), by = location_id]
tr <- merge(tr, eff_pre, by = "location_id")

# ---- Structural and behavioral savings components.
cf <- pv(ctx, "treatment", "savings_coefs"); ce <- pv(ctx, "treatment", "savings_centers")
sk <- pv(ctx, "treatment", "structural_skew"); ex <- pv(ctx, "treatment", "extreme_savers")
rho <- pv(ctx, "treatment", "turnover_behavioral_corr"); sbeh <- pv(ctx, "treatment", "behavioral_sd")
# The structural effect and the extreme savers are drawn by stratified sampling within the cohort (a shared
# named stream), so the cohort reproduces the configured distribution rather than one noisy draw of it.
use_stream(ctx, "savings_strata", "cohort")
n_tr <- nrow(tr)
z_strat <- sample(qnorm((seq_len(n_tr) - 0.5) / n_tr))
ext_idx <- sample.int(n_tr, round(ex$share * n_tr))
bump <- numeric(n_tr); bump[ext_idx] <- ex$range[1] + runif(length(ext_idx)) * (ex$range[2] - ex$range[1])
register_keys(ctx, "savings_strata", "cohort")
sk_sc <- if (is.null(sk$scale)) 1 else sk$scale
tr[, `:=`(skew = sk_sc * z_strat * ifelse(z_strat < 0, sk$sd_lower, sk$sd_upper), behavioral_first = NA_real_, extreme_saver = bump > 0)]
for (i in seq_len(n_tr)) {
  use_stream(ctx, "savings", tr$location_id[i])
  set(tr, i, "behavioral_first", sbeh * rnorm(1) + bump[i])
}
register_keys(ctx, "savings", tr$location_id)
tr[, structural_x := cf$efficiency * (eff_pre_clean - ce$efficiency) + cf$log_quantity * log(quantity / ce$quantity) +
       cf$log_area_ratio * log(area_ratio / ce$area_ratio) + cf$log_income * log(median_hh_income / ce$income) + skew]

# Behavioral effect by tenure from the rebate occupant onward: redrawn on turnover with correlation rho.
beh <- ten[tr[, .(location_id, tenure_index_rebate = tenure_index, behavioral_first)], on = "location_id"][
  tenure_index >= tenure_index_rebate, .(location_id, tenure_index, cust_loc_id, tenure_index_rebate, behavioral_first)]
setorder(beh, location_id, tenure_index)
beh[, behavioral := NA_real_]
for (loc in unique(beh$location_id)) {
  idx <- which(beh$location_id == loc)
  b <- beh$behavioral_first[idx[1]]
  set(beh, idx[1], "behavioral", b)
  if (length(idx) > 1) for (j in idx[-1]) {
    use_stream(ctx, "savings_behavioral", paste(loc, beh$tenure_index[j]))
    b <- rho * b + sqrt(1 - rho^2) * sbeh * rnorm(1)
    set(beh, j, "behavioral", b)
  }
}
register_keys(ctx, "savings_behavioral", beh[tenure_index > tenure_index_rebate, paste(location_id, tenure_index)])

# ---- Other rebate types: high efficiency clothes washers and weather based irrigation controllers.
orb <- pv(ctx, "treatment", "other_rebates")
ow <- win(orb$window)
sfl <- locs[cust_class == "SF", .(location_id, code, first_t, last_t)]
mean_att <- mean(ten[cust_class == "SF", attitude])
other <- rbindlist(lapply(seq_len(nrow(sfl)), function(i) {
  L <- sfl[i]
  use_stream(ctx, "other_rebates", L$location_id)
  u <- runif(6)
  lo <- max(ow[1], L$first_t); hi <- min(ow[2], L$last_t - 1L)
  if (hi < lo) return(NULL)
  out <- list()
  for (k in c("hecw", "wbic")) {
    o <- if (k == "hecw") 1 else 4
    t_i <- lo + floor(u[o] * (hi - lo + 1))
    a <- ten[location_id == L$location_id & start_t <= t_i & end_t >= t_i, attitude]
    p <- plogis(qlogis(orb[[k]]$share) + orb[[k]]$attitude_coef * (a - mean_att))
    if (u[o + 1] < p) out[[k]] <- data.table(location_id = L$location_id, type = k, t_install = as.integer(t_i),
                                             post_day = as.integer(1 + floor(u[o + 2] * tm$days[t_i])))
  }
  rbindlist(out)
}))
register_keys(ctx, "other_rebates", sfl$location_id)

# ---- Build the treated panel: other rebate effects first, then turf savings with the feasibility cap.
P <- pan[, .(location_id, cust_loc_id, tenure_index, code, cust_class, t, vacant, days, indoor_real, outdoor_real, y_untreated)]
P[, `:=`(hecw_saving = 0, wbic_saving = 0)]
if (nrow(other)) {
  h <- other[type == "hecw"]; w <- other[type == "wbic"]
  P[h, on = .(location_id, t > t_install), hecw_saving := pmin(orb$hecw$gal_per_day * days / G * (!vacant),
                                                                orb$hecw$max_indoor_frac * indoor_real)]
  P[w, on = .(location_id, t > t_install), wbic_saving := orb$wbic$outdoor_frac * outdoor_real]
}
P[, y0 := y_untreated - hecw_saving - wbic_saving]
P[, `:=`(indoor_after_other = indoor_real - hecw_saving, outdoor_after_other = outdoor_real - wbic_saving)]

tp <- P[tr[, .(location_id, t_effect, quantity)], on = .(location_id, t >= t_effect), nomatch = NULL,
        .(location_id, t = x.t, tenure_index, days, outdoor_after_other, quantity)]
tp <- merge(tp, beh[, .(location_id, tenure_index, behavioral)], by = c("location_id", "tenure_index"))
tp <- merge(tp, tr[, .(location_id, structural_x, code, t_effect)], by = "location_id")
tp[, decay := exp(-pv(ctx, "treatment", "savings_decay_per_year") * (t - t_effect + 0.5) / 12)]
tp[, post_month_index := t - t_effect + 1L]
tp[, month := tm$month[t]]
sea <- pv(ctx, "treatment", "seasonal")
et_mean <- et0_baseline(ctx)[, .(et_annual_mean = mean(et0_base)), by = code]
tp <- merge(tp, clim[, .(code, t, eto_in)], by = c("code", "t"))
tp <- merge(tp, et_mean, by = "code")
tp[, seas := 1 + sea$amplitude * cos(2 * pi * (month - sea$peak_month) / 12)]
setorder(tp, location_id, t)
tp[, noise := {
  use_stream(ctx, "savings_monthly", location_id[1]); pv(ctx, "treatment", "monthly_noise_sd") * rnorm(.N)
}, by = location_id]
register_keys(ctx, "savings_monthly", tr$location_id)
tp[, g_base := (structural_x + behavioral) / 12 * seas + cf$et * (eto_in - et_annual_mean) + noise]
tp[, cap_g := outdoor_after_other * G / quantity]   # gallons per sq ft that would remove all outdoor use

# Calibration of the structural intercept: capped annual mean over the first 12 post-months of households with
# at least 12 post-months in the panel (the same window as the headline estimate).
full12 <- tr[t_effect + 11L <= last_t, location_id]
w12 <- tp$location_id %in% full12 & tp$post_month_index <= 12L
calib <- function(b0s) {
  g <- pmin((b0s / 12 * tp$seas[w12] + tp$g_base[w12]) * tp$decay[w12], tp$cap_g[w12])
  mean(data.table(location_id = tp$location_id[w12], g = g)[, .(annual = sum(g)), by = location_id]$annual)
}
target_mean <- pv(ctx, "treatment", "savings_target_annual_mean")
lo <- -100; hi <- 200
for (k in 1:80) { mid <- (lo + hi) / 2; if (calib(mid) < target_mean) lo <- mid else hi <- mid }
b0s <- (lo + hi) / 2
message(sprintf("  planted structural intercept %.3f gives capped annual mean %.3f", b0s, calib(b0s)))
tp[, g_uncapped := (b0s / 12 * seas + g_base) * decay]
tp[, cap_binds := g_uncapped > cap_g]
tp[, g := pmin(g_uncapped, cap_g)]
tp[, turf_saving := g * quantity / G]   # CCF
tr[, structural := b0s + structural_x]

P <- merge(P, tp[, .(location_id, t, turf_saving, g, cap_binds)], by = c("location_id", "t"), all.x = TRUE)
P[is.na(turf_saving), `:=`(turf_saving = 0, cap_binds = FALSE)]
turf_locs <- tr$location_id
P[, y1 := ifelse(location_id %in% turf_locs, y0 - turf_saving, NA_real_)]
P[, y_clean := y0 - turf_saving]
P[, treated_post := location_id %in% turf_locs & turf_saving != 0]

# ---- Truth outputs.
clean <- merge(pan[, .(location_id, t, cust_loc_id, tenure_index, code, block_group_id, cust_class, vacant, days,
                       indoor_struct, outdoor_struct, mu_struct, alpha_r, alpha_bg, eta, ar, shock, conservation, trend,
                       log_error, y_untreated, indoor_real, outdoor_real)],
               P[, .(location_id, t, planted_hecw_saving_ccf = hecw_saving, planted_wbic_saving_ccf = wbic_saving, y0, y1,
                     planted_turf_saving_ccf = turf_saving, planted_turf_gal_per_sqft = g, cap_binds, treated_post, y_clean)],
               by = c("location_id", "t"))
clean[, month_start := tm$month_start[t]]
clean[, is_synthetic := TRUE]
write_table(clean, file.path(ctx$paths$truth, "clean_panel.parquet"), c("location_id", "t"))

turn <- ten[, .(location_id, start_t, tenure_index)]
tr <- merge(tr, turn[tenure_index > 1][tr[, .(location_id, t_rebate)], on = "location_id", nomatch = NULL][
  , .(turnover_in_pre_period = any(start_t <= t_rebate), turnover_in_post_period = any(start_t > t_rebate)), by = location_id],
  by = "location_id", all.x = TRUE)
tr[is.na(turnover_in_pre_period), `:=`(turnover_in_pre_period = FALSE, turnover_in_post_period = FALSE)]
hh_ann <- tp[, .(n_post_months = .N, cap_binding_share = mean(cap_binds),
                 planted_first12_gal_per_sqft = if (.N >= 12) sum(g[post_month_index <= 12]) else NA_real_,
                 planted_allpost_annual_gal_per_sqft = 12 * mean(g)), by = location_id]
# Overlap with the other rebate types (driven by the shared latent attitude).
if (nrow(other)) {
  ov <- dcast(other, location_id ~ type, value.var = "t_install")
  for (k in c("hecw", "wbic")) if (!k %in% names(ov)) ov[, (k) := NA_integer_]
  tr <- merge(tr, ov[, .(location_id, t_hecw = hecw, t_wbic = wbic)], by = "location_id", all.x = TRUE)
} else tr[, `:=`(t_hecw = NA_integer_, t_wbic = NA_integer_)]
tr <- merge(tr, hh_ann, by = "location_id", all.x = TRUE)
hs <- tr[, .(location_id, code, cust_loc_zip, cust_loc_id_rebate = cust_loc_id, cust_id_rebate = cust_id_from_utility,
             t_rebate, t_effect, received_date, post_date, quantity, irr_area_true, area_ratio, median_hh_income,
             eff_pre_clean, eff_2010_11,
             planted_structural_annual_gal_per_sqft = structural, planted_behavioral_annual_gal_per_sqft = behavioral_first,
             extreme_saver, planted_annual_gal_per_sqft = structural + behavioral_first, n_post_months,
             planted_first12_gal_per_sqft, planted_allpost_annual_gal_per_sqft, cap_binding_share,
             turnover_in_pre_period, turnover_in_post_period,
             has_hecw = !is.na(t_hecw), t_hecw, has_wbic = !is.na(t_wbic), t_wbic,
             passes_filters, pre_months_observed = pre_months, post_months_observed = post_months, is_synthetic = TRUE)]
write_table(hs, file.path(ctx$paths$truth, "household_savings.csv"), "location_id")
write_table(beh[, .(location_id, tenure_index, cust_loc_id, planted_behavioral_annual_gal_per_sqft = behavioral,
                    is_rebate_occupant = tenure_index == tenure_index_rebate, is_synthetic = TRUE)],
            file.path(ctx$paths$truth, "planted_behavioral_by_tenure.csv"), c("location_id", "tenure_index"))
write_table(ten[, .(location_id, tenure_index, cust_loc_id, cust_id_from_utility, cust_class, start_t, end_t,
                    vacancy_months, hhsize, latent_attitude = attitude, gpcd, overwater, vacancy_irrigation_frac,
                    is_synthetic = TRUE)], file.path(ctx$paths$truth, "latent_attitude.csv"), c("location_id", "tenure_index"))
oe <- P[, .(planted_hecw_total_ccf = sum(hecw_saving), planted_wbic_total_ccf = sum(wbic_saving)), by = location_id][
  planted_hecw_total_ccf > 0 | planted_wbic_total_ccf > 0]
if (nrow(other)) {
  oe <- merge(dcast(other, location_id ~ type, value.var = "t_install"), oe, by = "location_id", all.x = TRUE)
  setnames(oe, intersect(c("hecw", "wbic"), names(oe)), paste0("t_install_", intersect(c("hecw", "wbic"), names(oe))))
  oe[, has_turf := location_id %in% tr$location_id]
}
oe[, is_synthetic := TRUE]
write_table(oe, file.path(ctx$paths$truth, "planted_other_rebate_effects.csv"), "location_id")
write_table(cand[, .(location_id, code, cust_loc_zip, median_hh_income, irr_area_true, eff_2010_11, attitude, t_rebate,
                     received_date, post_date, pre_months, post_months, default_area, missing_area, duplicate_turf, passes_filters,
                     p_select, treated)], wpath(ctx, "selection.csv"), "location_id")

# Rebate records (truth): turf, washers and controllers, with received < post inspection < check dates.
pay <- pv(ctx, "treatment", "payment_per_sqft")
rb_turf <- tr[, .(location_id, cust_id_from_utility, rebate_instance_type = "turf", rebate_instance_quantity = quantity,
                  rebate_instance_payment = round(pay * quantity, 2), rebate_instance_post_date = post_date,
                  rebate_instance_received_date = received_date, rebate_instance_check_date = check_date)]
rb_other <- if (nrow(other)) {
  o <- copy(other)
  o[, rebate_instance_post_date := tm$month_start[t_install] + post_day - 1L]
  o <- o[ten, on = .(location_id, t_install >= start_t, t_install <= end_t), nomatch = NULL,
         .(location_id, cust_id_from_utility, type, rebate_instance_post_date)]
  o[, `:=`(rebate_instance_type = ifelse(type == "hecw", "high efficiency clothes washer", "weather based irrigation controller"),
           rebate_instance_quantity = 1, rebate_instance_payment = ifelse(type == "hecw", orb$hecw$payment, orb$wbic$payment))]
  o[, type := NULL]
} else NULL
if (!is.null(rb_other)) {
  rb_other[, rebate_key := paste(location_id, rebate_instance_type, rebate_instance_post_date)]
  rb_other[, c("lead", "lag") := {
    use_stream(ctx, "rebate_dates", rebate_key)
    list(rl[1] + floor(runif(1) * (rl[2] - rl[1] + 1)), cl[1] + floor(runif(1) * (cl[2] - cl[1] + 1)))
  }, by = rebate_key]
  register_keys(ctx, "rebate_dates", rb_other$rebate_key)
  rb_other[, `:=`(rebate_instance_received_date = rebate_instance_post_date - lead,
                  rebate_instance_check_date = rebate_instance_post_date + lag)]
  rb_other[, c("lead", "lag", "rebate_key") := NULL]
}
rb <- rbind(rb_turf, rb_other, use.names = TRUE)
setorder(rb, location_id, rebate_instance_post_date, rebate_instance_type)
write_table(rb, wpath(ctx, "rebates_truth.csv"), c("location_id", "rebate_instance_post_date", "rebate_instance_type"))

tpj <- read_json(wpath(ctx, "generator_params.json"))
tpj$treatment <- list(selection_intercept = b0, selection_coefs = sc, structural_savings_intercept = b0s,
                      savings_coefs = cf, seasonal = sea, structural_skew = sk, behavioral_sd = sbeh,
                      savings_decay_per_year = pv(ctx, "treatment", "savings_decay_per_year"),
                      extreme_savers = ex, turnover_behavioral_corr = rho, cohort_target = target,
                      treated_share_of_candidates = mean(cand$treated))
write_json(tpj, wpath(ctx, "generator_params.json"), auto_unbox = TRUE, digits = NA, pretty = TRUE)

# ---- Gates.
fit <- glm(treated ~ log(median_hh_income / 80000) + log(irr_area_true / 4000) + eff_2010_11 + attitude,
           data = cand, family = binomial())
est <- coef(fit)[-1]
ctx <- gate(ctx, "logistic regression recovers the selection coefficient signs",
            all(sign(est) == sign(unlist(sc[c("log_income", "log_area", "efficiency", "attitude")]))), est, "all signs as configured")
ctx <- gate(ctx, "treated share of single family candidates", mean(cand$treated) > 0.07 & mean(cand$treated) < 0.13,
            mean(cand$treated), "about 9 to 11 percent expected (0.07 to 0.13)")
# Planted-side gates (relaxed bands, tagged assumed). The paper's tighter bands apply to the estimates in stage 7.
gp <- pv(ctx, "gates", "planted")
tp12 <- tp[location_id %in% full12 & post_month_index <= 12L]
hh <- tp12[, .(annual = sum(g)), by = location_id]
ctx <- gate(ctx, "planted capped annual mean, first 12 post-months (gal/sqft/yr)", in_band(mean(hh$annual), gp$annual_mean),
            mean(hh$annual), paste(gp$annual_mean, collapse = " to "))
jas <- tp12[month %in% 7:9, mean(g)]; jfm <- tp12[month %in% 1:3, mean(g)]
ctx <- gate(ctx, "planted July-September monthly mean", in_band(jas, gp$jas_mean), jas, paste(gp$jas_mean, collapse = " to "))
ctx <- gate(ctx, "planted January-March monthly mean", in_band(jfm, gp$jfm_mean), jfm, paste(gp$jfm_mean, collapse = " to "))
q <- quantile(hh$annual, c(0.25, 0.5, 0.75))
ctx <- gate(ctx, "planted mean exceeds planted median", mean(hh$annual) > q[2], c(mean(hh$annual), q[2]), "mean > median")
ctx <- gate(ctx, "planted quartiles (reported)", TRUE, q, "report only")
ctx <- share_gate(ctx, "planted share above 60 gal/sqft/yr", sum(hh$annual > 60), nrow(hh), gp$share_above_60)
ctx <- share_gate(ctx, "planted share at or below zero", sum(hh$annual <= 0), nrow(hh), gp$share_at_or_below_zero)
feas <- P[location_id %in% turf_locs]
f1 <- feas[, all(y1 >= indoor_after_other - 1e-9)]
f2 <- tp[, all(g * quantity <= outdoor_after_other * G + 1e-6)]
ctx <- gate(ctx, "feasibility: treated usage never below indoor demand", f1, mean(feas$y1 >= feas$indoor_after_other - 1e-9), "100 percent")
ctx <- gate(ctx, "feasibility: monthly savings never exceed untreated outdoor demand", f2, mean(tp$g * tp$quantity <= tp$outdoor_after_other * G + 1e-6), "100 percent")
ctx <- gate(ctx, "share of treated household-months where the cap binds (reported)", TRUE, mean(tp$cap_binds), "report only")

library(ggplot2)
hh2 <- merge(hh, tr[, .(location_id, yr = tm$year[t_rebate])], by = "location_id")
write_table(hh, wpath(ctx, "planted_first12.csv"), "location_id")
p <- ggplot(hh2, aes(annual)) + geom_histogram(binwidth = 4, boundary = 0) + geom_vline(xintercept = mean(hh2$annual), linetype = 2) +
  labs(x = "Planted savings, first 12 post-months (gal per sq ft of turf removed, capped)", y = "Households",
       title = "Distribution of planted household savings") + theme_minimal()
save_fig(ctx, p, "planted_savings_distribution")
p <- ggplot(hh2, aes(factor(yr), annual)) + geom_boxplot() + labs(x = "Rebate year", y = "Planted savings (gal/sqft/yr)",
  title = "Planted savings by rebate year") + theme_minimal()
save_fig(ctx, p, "planted_savings_by_year")
mm <- tp12[, .(mean_g = mean(g)), by = month]
p <- ggplot(mm, aes(month, mean_g)) + geom_line() + geom_point() + scale_x_continuous(breaks = 1:12) +
  annotate("rect", xmin = 7, xmax = 9, ymin = gp$jas_mean[1], ymax = gp$jas_mean[2], alpha = 0.15) +
  annotate("rect", xmin = 1, xmax = 3, ymin = gp$jfm_mean[1], ymax = gp$jfm_mean[2], alpha = 0.15) +
  labs(x = "Month", y = "Mean planted savings (gal/sqft/month)", title = "Planted seasonal savings with the planted-side bands shaded") +
  theme_minimal()
save_fig(ctx, p, "planted_savings_seasonal")
finish_stage(ctx)
