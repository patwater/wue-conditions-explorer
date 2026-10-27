# Stage 3b: untreated monthly usage for every location-month, with no rebates and no observation noise.
#
# Model (single family, CCF per month):
#   indoor_it  = hhsize_i * gpcd_i * days_t / 748.052                      (x vacancy_indoor_frac when vacant)
#   outdoor_it = 0.623 * depth_rt * area_i * plant_factor / efficiency * overwater_i / 748.052
#                                                                          (x vacancy_irrigation_frac when vacant)
#   depth_rt   = max(ET0_rt - effective_rain_rt (floored at 0), irrigation_floor * ET0_rt)
#   mu_it      = indoor_it + outdoor_it                                     (structural mean, stored in truth)
#   log y_it   = log mu_it + alpha_r + alpha_bg + eta_i + u_it + s_rt + c_t + trend_it
#   eta_i      = beta_attitude * a_i + sigma_hh * z_i
#   u_it       = phi * u_i,t-1 + sigma_ar * e_it                            (household AR(1), restarts at turnover)
#   s_rt       ~ N(0, sigma_shock)                                          (retailer-by-month shock)
#   c_t        = voluntary * 1[t >= Feb 2014] + mandatory * 1[t >= Apr 2015]
#   trend_it   = (slope_r + slope_bg) * (t - t_mid) / 12 + level_it         (level_it is a slow random walk)
#                + attitude_trend * a_i * (t - t_mid) / 12                  (zero except in the differential trends scenario)
#   sigma_hh and sigma_ar are multiplied by noise_scale (one except in the noise scenarios)
# Other classes use simpler structural means (see README) and share the same error structure without a_i.
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), "00_common.R"))
suppressPackageStartupMessages(library(parallel))
ctx <- init_ctx("05_usage")

ret <- read_table(wpath(ctx, "retailers.csv"))
bgs <- read_table(wpath(ctx, "block_groups.csv"))
locs <- read_table(wpath(ctx, "locations.csv"))
ten <- read_table(wpath(ctx, "tenures.csv"))
clim <- read_table(file.path(ctx$paths$truth, "climate_monthly.parquet"))
tm <- ctx$time; nT <- nrow(tm); t_mid <- (nT + 1) / 2
G <- pv(ctx, "units", "gallons_per_ccf"); k_in <- pv(ctx, "units", "gal_per_sqft_inch")
PF <- pv(ctx, "accounts", "plant_factor"); IE <- pv(ctx, "accounts", "irrigation_efficiency")
P <- function(n) pv(ctx, "usage", n)

# ---- Panel skeleton: one row per tenure-month.
ten[, n_months := end_t - start_t + 1L]
pan <- ten[rep(seq_len(nrow(ten)), n_months), .(location_id, cust_loc_id, tenure_index, code, cust_class,
                                                 block_group_id, start_t, occupied_from_t, hhsize, attitude, gpcd,
                                                 overwater, vacancy_irrigation_frac)]
pan[, t := start_t + rowid(cust_loc_id) - 1L]
pan[, vacant := t < occupied_from_t]
pan <- merge(pan, locs[, .(location_id, irr_area_true, unit_count, base_ccf)], by = "location_id")
pan <- merge(pan, clim[, .(code, t, days, eto_in, applied_depth_in)], by = c("code", "t"))
mean_et <- clim[, .(mean_eto = mean(eto_in)), by = code]
pan <- merge(pan, mean_et, by = "code")

# ---- Structural means.
s_cii <- pv(ctx, "accounts", "cii_seasonal_share")
ppu <- pv(ctx, "accounts", "mf_persons_per_unit"); gpcd0 <- pv(ctx, "accounts", "gpcd_median")
pan[, `:=`(indoor_struct = 0, outdoor_struct = 0)]
pan[cust_class == "SF", `:=`(indoor_struct = hhsize * gpcd * days / G,
                             outdoor_struct = k_in * applied_depth_in * irr_area_true * PF / IE * overwater / G)]
pan[cust_class == "SF" & vacant == TRUE, `:=`(indoor_struct = indoor_struct * pv(ctx, "occupancy", "vacancy_indoor_frac"),
                                              outdoor_struct = outdoor_struct * vacancy_irrigation_frac)]
pan[cust_class == "MF", `:=`(indoor_struct = unit_count * ppu * gpcd0 * days / G,
                             outdoor_struct = k_in * applied_depth_in * irr_area_true * PF / IE / G)]
pan[cust_class == "IRR", outdoor_struct := k_in * applied_depth_in * irr_area_true * PF / IE / G]
pan[cust_class %in% c("COM", "INST", "IND", "OTH"), `:=`(indoor_struct = base_ccf * days / 30.4375 * (1 - s_cii),
                                                         outdoor_struct = base_ccf * days / 30.4375 * s_cii * eto_in / mean_eto)]
pan[, mu_struct := indoor_struct + outdoor_struct]

# ---- Error components. Retailer and block group effects, retailer-month shocks, then household paths.
re_r <- rbindlist(lapply(ret$code, function(cc) {
  use_stream(ctx, "usage_retailer", cc); z <- rnorm(2)
  data.table(code = cc, alpha_r = P("sigma_retailer") * z[1], slope_r = P("trend_slope_mean") + P("sigma_slope_retailer") * z[2])
}))
re_bg <- rbindlist(lapply(bgs$block_group_id, function(b) {
  use_stream(ctx, "usage_bg", b); z <- rnorm(2)
  data.table(block_group_id = b, alpha_bg = P("sigma_bg") * z[1], slope_bg = P("sigma_slope_bg") * z[2])
}))
shock <- rbindlist(lapply(ret$code, function(cc) {
  use_stream(ctx, "usage_shock", cc)
  data.table(code = cc, t = seq_len(nT), shock = P("sigma_shock") * rnorm(nT))
}))
register_keys(ctx, "usage_retailer", ret$code); register_keys(ctx, "usage_bg", bgs$block_group_id)
register_keys(ctx, "usage_shock", ret$code)

hh_path <- function(i) {
  x <- ten[i]
  use_stream(ctx, "usage_hh", paste(x$location_id, x$tenure_index))
  n <- x$n_months
  z <- rnorm(2); e <- rnorm(n); w <- rnorm(n)
  phi <- P("ar_phi"); s <- P("ar_sigma") * P("noise_scale")
  u <- numeric(n); u[1] <- s / sqrt(1 - phi^2) * z[2]
  if (n > 1) for (k in 2:n) u[k] <- phi * u[k - 1] + s * e[k]
  level <- cumsum(c(0, P("local_level_sd") * w[-1]))
  att <- if (x$cust_class == "SF") x$attitude else 0
  list(eta = P("beta_attitude") * att + P("sigma_hh") * P("noise_scale") * z[1], ar = u, level = level)
}
paths <- mclapply(seq_len(nrow(ten)), hh_path, mc.cores = ctx$opt$cores, mc.preschedule = TRUE)
register_keys(ctx, "usage_hh", ten[, paste(location_id, tenure_index)])
hp <- data.table(cust_loc_id = rep(ten$cust_loc_id, ten$n_months),
                 t = unlist(Map(function(a, b) a:b, ten$start_t, ten$end_t)),
                 eta = rep(vapply(paths, `[[`, 0, "eta"), ten$n_months),
                 ar = unlist(lapply(paths, `[[`, "ar")), level = unlist(lapply(paths, `[[`, "level")))
pan <- merge(pan, hp, by = c("cust_loc_id", "t"))
pan <- merge(pan, re_r, by = "code"); pan <- merge(pan, re_bg, by = "block_group_id")
pan <- merge(pan, shock, by = c("code", "t"))

vol <- P("voluntary_effect"); man <- P("mandatory_effect")
t_vol <- month_index(ctx, vol$start); t_man <- month_index(ctx, man$start)
pan[, conservation := vol$log_effect * (t >= t_vol) + man$log_effect * (t >= t_man)]
pan[, trend := (slope_r + slope_bg) * (t - t_mid) / 12 + level]
# Scenario knob (zero in the base world): conservation-minded households trend down faster without a rebate.
pan[cust_class == "SF", trend := trend + P("attitude_trend") * attitude * (t - t_mid) / 12]
pan[, log_error := alpha_r + alpha_bg + eta + ar + shock + conservation + trend]
pan[, mult := exp(log_error)]
pan[, `:=`(y_untreated = mu_struct * mult, indoor_real = indoor_struct * mult, outdoor_real = outdoor_struct * mult)]

keep <- c("location_id", "cust_loc_id", "tenure_index", "code", "block_group_id", "cust_class", "t", "vacant",
          "days", "indoor_struct", "outdoor_struct", "mu_struct", "alpha_r", "alpha_bg", "eta", "ar", "shock",
          "conservation", "trend", "log_error", "y_untreated", "indoor_real", "outdoor_real")
pan <- pan[, ..keep]
setkey(pan, location_id, t)
write_table(pan, wpath(ctx, "usage_untreated.parquet"), c("location_id", "t"))

# True usage parameters for the truth folder.
truth_par <- list(usage = list(
  sigma_retailer = P("sigma_retailer"), sigma_bg = P("sigma_bg"), sigma_hh = P("sigma_hh") * P("noise_scale"),
  beta_attitude = P("beta_attitude"), ar_phi = P("ar_phi"), ar_sigma = P("ar_sigma") * P("noise_scale"),
  sigma_shock = P("sigma_shock"), attitude_trend = P("attitude_trend"), noise_scale = P("noise_scale"),
  voluntary_effect = vol$log_effect, mandatory_effect = man$log_effect,
  realized_retailer_effects = setNames(as.list(re_r$alpha_r), re_r$code),
  realized_retailer_slopes = setNames(as.list(re_r$slope_r), re_r$code)))
write_json(truth_par, wpath(ctx, "generator_params.json"), auto_unbox = TRUE, digits = NA, pretty = TRUE)
write_table(re_bg, wpath(ctx, "usage_bg_effects.csv"), "block_group_id")

# ---- Gates on the realized panel (occupied single family months).
sf <- pan[cust_class == "SF"]
occ <- sf[vacant == FALSE]
share_a <- mean(occ$y_untreated >= 0 & occ$y_untreated <= 100)
ctx <- gate(ctx, "occupied single family account-months between 0 and 100 CCF", share_a >= 0.90, share_a, ">= 0.90")
occ[, year := tm$year[t]]
ann <- occ[year <= 2015, .(n = .N, tot = sum(y_untreated)), by = .(location_id, year)][n == 12]
share_b <- mean(ann$tot >= 100 & ann$tot <= 600)
ctx <- gate(ctx, "single family annual totals between 100 and 600 CCF", share_b >= 0.80, share_b, ">= 0.80")
occ[, month := tm$month[t]]
jj <- dcast(occ[month %in% c(1, 7)], location_id + code + year ~ month, value.var = "y_untreated")
setnames(jj, c("1", "7"), c("jan", "jul"))
rat <- jj[!is.na(jan) & !is.na(jul), .(ratio = median(jul / jan)), by = code]
ctx <- gate(ctx, "median July/January usage ratio larger for Inland Enclave and Valley Small Town than Surf City",
            all(rat[code %in% c("IE", "VS"), ratio] > rat[code == "SC", ratio]),
            setNames(rat$ratio, rat$code), "IE and VS > SC")
jd <- occ[year %in% c(2013, 2015) & month >= 6, .(n = .N, tot = sum(y_untreated)), by = .(location_id, year)][n == 7]
both <- jd[, .N, by = location_id][N == 2, location_id]
drop <- jd[location_id %in% both & year == 2015, median(tot)] / jd[location_id %in% both & year == 2013, median(tot)] - 1
ctx <- gate(ctx, "median single family usage June-December 2015 vs same months of 2013", drop >= -0.30 & drop <= -0.15,
            drop, "between -0.30 and -0.15")
osh <- occ[, .(outdoor_share = sum(outdoor_real) / sum(y_untreated)), by = code]
ctx <- gate(ctx, "outdoor share larger inland and in the valley than in Surf City",
            all(osh[code %in% c("IE", "VS"), outdoor_share] > osh[code == "SC", outdoor_share]),
            setNames(osh$outdoor_share, osh$code), "IE and VS > SC")

# ---- Prior predictive check: redraw the hyperparameters from their priors and re-simulate a sample of
# single family households, keeping the structural attributes. Each draw has its own stream.
pri <- pv(ctx, "usage", "priors")
draw_prior <- function(p) switch(p$dist, halfnormal = abs(rnorm(1, 0, p$scale)), normal = rnorm(1, p$mean, p$sd),
                                 beta = rbeta(1, p$a, p$b))
use_stream(ctx, "prior_pred_sample", "all")
sf_ids <- sort(unique(ten[cust_class == "SF" & tenure_index == 1 & start_t == 1 & end_t == nT, cust_loc_id]))
samp <- sort(sample(sf_ids, min(P("prior_predictive_accounts"), length(sf_ids))))
ps <- pan[cust_loc_id %in% samp & vacant == FALSE, .(cust_loc_id, code, block_group_id, t, indoor_struct, outdoor_struct)]
ps <- merge(ps, ten[, .(cust_loc_id, gpcd, overwater, attitude)], by = "cust_loc_id")
setorder(ps, cust_loc_id, t)
nd <- P("prior_predictive_draws")
pp <- rbindlist(lapply(seq_len(nd), function(k) {
  use_stream(ctx, "prior_pred", k)
  th <- lapply(pri, draw_prior)
  ar_r <- setNames(rnorm(nrow(ret), 0, th$sigma_retailer), ret$code)
  ar_b <- setNames(rnorm(nrow(bgs), 0, th$sigma_bg), bgs$block_group_id)
  sh <- matrix(rnorm(nrow(ret) * nT, 0, th$sigma_shock), nrow(ret), dimnames = list(ret$code, NULL))
  ids <- unique(ps$cust_loc_id)
  zh <- setNames(rnorm(length(ids)), ids); zg <- setNames(rnorm(length(ids)), ids); zo <- setNames(rnorm(length(ids)), ids)
  d <- copy(ps)
  d[, gp := th$gpcd_median * exp(pv(ctx, "accounts", "gpcd_sdlog") * zg[cust_loc_id])]
  d[, ow := exp(th$overwater_sdlog * zo[cust_loc_id])]
  d[, mu := indoor_struct * gp / gpcd + outdoor_struct * ow / overwater]
  d[, u := {
    e <- rnorm(.N); x <- numeric(.N); x[1] <- th$ar_sigma / sqrt(1 - th$ar_phi^2) * e[1]
    if (.N > 1) for (j in 2:.N) x[j] <- th$ar_phi * x[j - 1] + th$ar_sigma * e[j]
    x
  }, by = cust_loc_id]
  d[, cons := th$voluntary_effect * (t >= t_vol) + th$mandatory_effect * (t >= t_man)]
  d[, y := mu * exp(ar_r[code] + ar_b[block_group_id] + P("beta_attitude") * attitude + th$sigma_hh * zh[cust_loc_id] +
                      u + sh[cbind(match(code, ret$code), t)] + cons)]
  d[, `:=`(year = tm$year[t], month = tm$month[t])]
  a <- d[year <= 2015, .(tot = sum(y)), by = .(cust_loc_id, year)]
  j <- dcast(d[month %in% c(1, 7) & year <= 2015], cust_loc_id + code + year ~ month, value.var = "y"); setnames(j, c("1", "7"), c("jan", "jul"))
  rr <- j[, .(r = median(jul / jan)), by = code]
  dd <- d[year %in% c(2013, 2015) & month >= 6, .(tot = sum(y)), by = .(cust_loc_id, year)]
  data.table(draw = k, share_0_100 = mean(d$y <= 100), share_annual_100_600 = mean(a$tot >= 100 & a$tot <= 600),
             inland_gt_coastal = all(rr[code %in% c("IE", "VS"), r] > rr[code == "SC", r]),
             drop_2015 = dd[year == 2015, median(tot)] / dd[year == 2013, median(tot)] - 1)
}))
register_keys(ctx, "prior_pred", seq_len(nd)); register_keys(ctx, "prior_pred_sample", "all")
write_table(pp, file.path(ctx$paths$reports, "prior_predictive.csv"), "draw")
# The numeric thresholds bind on the realized panel above. For the prior, the median draw must meet them and
# at least half of the draws must meet both.
ok_draw <- pp$share_0_100 >= 0.90 & pp$share_annual_100_600 >= 0.80
ctx <- gate(ctx, "prior predictive: median draw meets the 0-100 CCF and 100-600 CCF thresholds",
            median(pp$share_0_100) >= 0.90 & median(pp$share_annual_100_600) >= 0.80,
            c(median(pp$share_0_100), median(pp$share_annual_100_600)), ">= 0.90 and >= 0.80")
ctx <- gate(ctx, "prior predictive: share of draws meeting both thresholds", mean(ok_draw) >= 0.5, mean(ok_draw), ">= 0.5 of draws")
ctx <- gate(ctx, "prior predictive: share of draws with inland/valley seasonal ratio above Surf City",
            mean(pp$inland_gt_coastal) >= 0.75, mean(pp$inland_gt_coastal), ">= 0.75 of draws")
ctx <- gate(ctx, "prior predictive: median 2015 vs 2013 drop across draws", median(pp$drop_2015) >= -0.30 & median(pp$drop_2015) <= -0.15,
            median(pp$drop_2015), "between -0.30 and -0.15")

# ---- Figures.
library(ggplot2)
use_stream(ctx, "figure_sample", "spaghetti")
sp_ids <- unlist(lapply(ret$code, function(cc) sample(sort(unique(occ[code == cc & tenure_index == 1, cust_loc_id])), 12)))
spd <- sf[cust_loc_id %in% sp_ids]
spd[, date := tm$month_start[t]]
p <- ggplot(spd, aes(date, y_untreated, group = cust_loc_id)) + geom_line(alpha = 0.4, linewidth = 0.3) +
  facet_wrap(~code, ncol = 1, scales = "free_y") + labs(x = NULL, y = "Untreated usage (CCF per month)",
  title = "Untreated single family usage, 12 households per retailer") + theme_minimal()
save_fig(ctx, p, "usage_spaghetti", height = 8)
prof <- occ[year %in% 2010:2013, .(median_ccf = median(y_untreated)), by = .(code, month)]
p <- ggplot(prof, aes(month, median_ccf, colour = code)) + geom_line(linewidth = 0.8) + scale_x_continuous(breaks = 1:12) +
  labs(x = "Month", y = "Median usage (CCF per month)", colour = "Retailer", title = "Seasonal profile, 2010-2013") + theme_minimal()
save_fig(ctx, p, "usage_seasonal_profile")
ppl <- melt(pp, id.vars = "draw", measure.vars = c("share_0_100", "share_annual_100_600", "drop_2015"))
p <- ggplot(ppl, aes(value)) + geom_histogram(bins = 20) + facet_wrap(~variable, scales = "free") +
  labs(x = NULL, y = "Prior draws", title = "Prior predictive distribution of the stage 3 statistics") + theme_minimal()
save_fig(ctx, p, "prior_predictive")
finish_stage(ctx)
