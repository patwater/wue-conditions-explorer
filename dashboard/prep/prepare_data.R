# Prepares the small JSON files the dashboard loads. THE DATA IS SIMULATED.
#   Rscript dashboard/prep/prepare_data.R
# Main files come from the observed tables (synthetic_scuba/data/) and the matched DiD estimates
# (synthetic_scuba/estimates/). The answer key (answer_key.json) is the only file that reads the generator's planted
# effects (synthetic_scuba/truth/ and the validation reports), and the app loads it only in answer key mode.
# Every aggregate built from fewer than MIN_CELL accounts or households is suppressed (null).
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(nanoparquet); library(yaml) })
setDTthreads(1L)
here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
root <- normalizePath(file.path(here, "..", ".."))
S <- file.path(root, "synthetic_scuba")
OUT <- file.path(root, "dashboard", "app", "data")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
MIN_CELL <- 10L
MDAYS <- 30.4375
LIFE_YEARS <- 10          # assumed useful life of a rebate measure, for dollars per acre-foot (undiscounted)
GAL_PER_CCF <- 748.052
rd <- function(f) { f <- file.path(S, f); if (grepl("parquet$", f)) as.data.table(read_parquet(f)) else fread(f, na.strings = "") }
put <- function(x, name) write_json(x, file.path(OUT, name), auto_unbox = TRUE, digits = 4, na = "null", dataframe = "rows", pretty = FALSE)
supp <- function(v, n) fifelse(n >= MIN_CELL, v, NA_real_)

params <- read_yaml(file.path(S, "config", "params.yaml"))
ret_cfg <- rbindlist(lapply(params$retailers$value, function(x) data.table(code = x$code, name = x$name, setting = x$setting,
  billing = x$billing, rate_type = x$rate_type, et0_baseline = sum(unlist(x$et0)), precip_baseline = x$precip_annual)))
ret_cfg[, order := .I]

usage <- rd("data/usage.parquet"); customers <- rd("data/customers.csv"); rebates <- rd("data/rebates.csv")
acs <- rd("data/acs_block_groups.csv"); climate <- rd("data/climate_et_precip.csv"); supplier <- rd("data/supplier_report.csv")

# ---- Retailer overview -----------------------------------------------------------------------------------------
loc <- customers[, .(utility_name = utility_name[1], cls = cust_loc_class[1], zip = cust_loc_zip[1], bg = block_group_id[1],
                     area = cust_loc_irr_area_sf[!is.na(cust_loc_irr_area_sf)][1], start = min(cust_loc_info_start_date)),
                 by = .(location_id = location_id_from_utility)]
nsf <- loc[cls == "single family residential", .(n_sf = .N), by = utility_name]
bunch <- merge(loc[cls == "single family residential" & !is.na(area), .N, by = .(utility_name, area)], nsf, by = "utility_name")[
  N >= pmax(5, ceiling(0.02 * n_sf))]
loc[, defaulted := FALSE][bunch, on = .(utility_name, area), defaulted := TRUE]
loc <- merge(loc, acs[, .(bg = block_group_id, income = median_hh_income)], by = "bg", all.x = TRUE)
ann <- climate[, .(et0 = sum(cimis_eto_in), precip = sum(precip_in)), by = .(utility_name, wy = year + (month >= 10))][wy %between% c(2011, 2015)]
overview <- loc[start <= as.IDate("2010-01-31"), .(accounts_jan2010 = .N, sf_share = mean(cls == "single family residential"),
  median_irrigable_area_sf = as.numeric(median(area[cls == "single family residential" & !defaulted], na.rm = TRUE)),
  median_income = as.numeric(median(income, na.rm = TRUE)), zip_codes = uniqueN(zip), block_groups = uniqueN(bg)), by = utility_name]
overview <- merge(overview, loc[, .(locations_ever = .N, new_construction = sum(start > as.IDate("2010-01-31"))), by = utility_name], by = "utility_name")
overview <- merge(overview, ann[, .(et0_annual_2011_2015 = mean(et0), precip_annual_2011_2015 = mean(precip)), by = utility_name], by = "utility_name")
overview <- merge(ret_cfg, overview, by.x = "name", by.y = "utility_name")[order(order)]

# ---- Usage, climate and supplier by retailer-month --------------------------------------------------------------
U <- usage[, .(location_id = location_id_from_utility, utility_name, usage_ccf, start = usage_start_date,
               days = as.integer(usage_end_date - usage_start_date) + 1L)]
U[, y := usage_ccf / days * MDAYS]
U[, med := median(y[usage_ccf >= 0]), by = location_id]
cleaning <- U[, .(negative_reads = sum(usage_ccf < 0), spikes = sum(usage_ccf >= 0 & y > 5 * med & y > 40), rows = .N), by = utility_name]
U <- U[usage_ccf >= 0 & !(y > 5 * med & y > 40)]
U[, month := as.IDate(format(start + 15L, "%Y-%m-01"))]
U <- merge(U, loc[, .(location_id, cls)], by = "location_id")
sfm <- U[cls == "single family residential"]
um <- rbind(sfm[, .(n = uniqueN(location_id), median_ccf = median(y), mean_ccf = mean(y)), by = .(utility_name, month)],
            sfm[, .(utility_name = "All retailers", n = uniqueN(location_id), median_ccf = median(y), mean_ccf = mean(y)), by = month])
um[, `:=`(median_ccf = supp(median_ccf, n), mean_ccf = supp(mean_ccf, n))]
um[, moy := as.integer(format(month, "%m"))][, yr := as.integer(format(month, "%Y"))]
b13 <- um[yr == 2013, .(utility_name, moy, base13 = median_ccf)]
um <- merge(um, b13, by = c("utility_name", "moy"), all.x = TRUE)
um[, pct_vs_2013 := fifelse(yr >= 2014, median_ccf / base13 - 1, NA_real_)]
um <- um[, .(utility_name, month = format(month, "%Y-%m"), n, median_ccf, mean_ccf, pct_vs_2013)][order(utility_name, month)]
cl <- climate[, .(utility_name, month = sprintf("%d-%02d", year, month), et0_in = cimis_eto_in, precip_in)]
sup <- supplier[, .(utility_name = report_agency_name, month = format(as.IDate(report_month), "%Y-%m"), gpcd = report_gpcd_calculated,
                    pct_reduction_vs_2013 = report_percent_reduction, production_af = report_total_production_af,
                    complaints = report_complaints, warnings = report_warnings)]

# ---- Rebates ---------------------------------------------------------------------------------------------------
rb <- merge(rebates, loc[, .(location_id_from_utility = location_id, utility_name)], by = "location_id_from_utility")
rb[, yr := as.integer(format(rebate_instance_post_date, "%Y"))]
rb_year <- rb[, .(n = .N, quantity = sum(rebate_instance_quantity), payment = sum(rebate_instance_payment)), by = .(type = rebate_instance_type, yr)]
rb_year[, `:=`(quantity = supp(quantity, n), payment = supp(payment, n), n = fifelse(n >= MIN_CELL, n, NA_integer_))]
rb_ret_year <- rb[, .(n = .N), by = .(utility_name, type = rebate_instance_type, yr)][, n := fifelse(n >= MIN_CELL, n, NA_integer_)]
rb_ret <- rb[, .(n = .N, median_quantity = as.numeric(median(rebate_instance_quantity))), by = .(utility_name, type = rebate_instance_type)]
rb_ret[, `:=`(median_quantity = supp(median_quantity, n), n = fifelse(n >= MIN_CELL, n, NA_integer_))]
dup <- rb[rebate_instance_type == "turf", .N, by = location_id_from_utility][N > 1, .N]

# ---- Savings (matched DiD estimates only) -----------------------------------------------------------------------
E <- function(f) rd(file.path("estimates", f))
he <- E("household_estimates.csv")[window == "headline"]
prim <- he[primary == TRUE]
pooled <- E("pooled_estimates.csv")
plac <- E("placebo_estimates.csv")
probs <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
qtab <- function(x, label) data.table(series = label, p = probs, value = as.numeric(quantile(x, probs)), n = length(x))
mean_ci <- function(x) { m <- mean(x); s <- sd(x) / sqrt(length(x)); list(mean = m, lo90 = m - 1.645 * s, hi90 = m + 1.645 * s) }
est_labels <- c(matched_corr_raw = "Matched DiD (primary)", matched_corr_deseasonalized = "Matched DiD, deseasonalized correlation",
                matched_corr_plus_level = "Matched DiD, correlation plus level", matched_no_other_rebate_controls = "Matched DiD, no other-rebate controls",
                unmatched_did = "Unmatched DiD (all zip controls)", matched_log = "Matched DiD, log outcome", naive_pre_post = "Naive pre-post (no controls)")
estimators <- he[, c(list(n = .N, median = median(estimate)), mean_ci(estimate)), by = estimator][, label := est_labels[estimator]][
  match(names(est_labels), estimator)]
by_ret <- prim[, c(list(n = .N, median = median(estimate)), mean_ci(estimate)), by = utility_name]
for (c in c("median", "mean", "lo90", "hi90")) set(by_ret, j = c, value = supp(by_ret[[c]], by_ret$n))
seas <- E("pooled_seasonal_profile.csv")
oth <- E("other_rebate_estimates.csv")
cot <- prim[, .(cotreated = sum(cotreated_in_window), n = .N)]
savings <- list(
  cohort = list(households = nrow(prim), cotreated_in_window = cot$cotreated,
                window = "first 12 post-months (6 billing periods for bimonthly retailers)"),
  pooled = pooled[, .(model = fifelse(model == "stacked_did_fixest", "Stacked DiD (fixest)", "Pooled household estimates (metafor)"),
                      estimate, se, lo90 = estimate - 1.645 * se, hi90 = estimate + 1.645 * se, n = n_blocks)],
  estimators = estimators, by_retailer = by_ret,
  quantiles = rbind(qtab(prim$estimate, "Matched DiD estimate"), qtab(plac$estimate, "Placebo (noise floor)")),
  placebo = list(n = nrow(plac), mean = mean(plac$estimate), sd = sd(plac$estimate), iqr = IQR(plac$estimate)),
  seasonal = seas[, .(month, estimate = est_gal_per_sqft_month, lo90 = est_gal_per_sqft_month - 1.645 * se, hi90 = est_gal_per_sqft_month + 1.645 * se)],
  event_study = E("event_study.csv")[, .(event_bin, estimate, lo90 = estimate - 1.645 * se, hi90 = estimate + 1.645 * se)],
  pretrend = E("pretrend_tests.csv")[, .(utility_name, n_blocks, slope = linear_slope_gal_per_sqft_yr_per_bin, se = linear_slope_se, p = linear_pval)],
  meta_regression = E("meta_regression.csv")[term != "intrcpt", .(term, estimate, se, p = pval, n)],
  other_rebates = oth[, .(type = fifelse(type == "hecw", "High efficiency clothes washer", "Weather based irrigation controller"),
                          n = n_households, estimate = converted_estimate, lo90 = converted_lo90, hi90 = converted_hi90, unit = converted_unit,
                          seasonal_p = seasonal_pval, summer_minus_winter_ccf, summer_minus_winter_se)])

# ---- Cost-effectiveness and water saved ----------------------------------------------------------------------
# Dollars per acre-foot = rebate payment / (annual savings per unit x LIFE_YEARS / gallons per acre-foot), using the
# matched DiD estimates; the 90% range carries the savings interval through (an interval that reaches zero leaves the
# cost unbounded). Duplicate rebate records are counted once per location and type, as in the analysis.
GAL_AF <- params$units$gallons_per_af$value
first_rb <- rb[order(rebate_instance_post_date)][, .SD[1], by = .(location_id_from_utility, rebate_instance_type)]
prog_lab <- c(turf = "Turf removal", "high efficiency clothes washer" = "Clothes washer", "weather based irrigation controller" = "Irrigation controller")
ccf_gal_yr <- function(x) x * 12 * GAL_PER_CCF
turf_pooled <- pooled[model == "stacked_did_fixest"]
oth_ix <- setNames(seq_len(nrow(oth)), fifelse(oth$type == "hecw", "high efficiency clothes washer", "weather based irrigation controller"))
annual <- rbind(
  data.table(type = "turf", unit = "square foot of turf removed", gal_yr = turf_pooled$estimate,
             gal_yr_lo = turf_pooled$estimate - 1.645 * turf_pooled$se, gal_yr_hi = turf_pooled$estimate + 1.645 * turf_pooled$se,
             basis = "stacked matched DiD, first 12 post-months"),
  rbindlist(lapply(names(oth_ix), function(ty) { o <- oth[oth_ix[[ty]]]
    data.table(type = ty, unit = "rebate", gal_yr = ccf_gal_yr(o$pooled_saving_ccf_per_month),
               gal_yr_lo = ccf_gal_yr(o$pooled_saving_ccf_per_month - 1.645 * o$pooled_se_ccf),
               gal_yr_hi = ccf_gal_yr(o$pooled_saving_ccf_per_month + 1.645 * o$pooled_se_ccf),
               basis = "stacked matched DiD, first 12 post-months") })))
pay <- first_rb[, .(rebates = .N, units = sum(rebate_instance_quantity), paid = sum(rebate_instance_payment)), by = .(type = rebate_instance_type)]
ce <- merge(annual, pay, by = "type")
ce[, payment_per_unit := paid / units]
ce[, `:=`(af_life_per_unit = gal_yr * LIFE_YEARS / GAL_AF)]
dpa <- function(pay, gal) fifelse(gal > 0, pay / (gal * LIFE_YEARS / GAL_AF), NA_real_)
ce[, `:=`(dollars_per_af = dpa(payment_per_unit, gal_yr), dollars_per_af_low = dpa(payment_per_unit, gal_yr_hi),
          dollars_per_af_high = dpa(payment_per_unit, gal_yr_lo), af_life_total = units * gal_yr * LIFE_YEARS / GAL_AF,
          af_life_total_lo = units * gal_yr_lo * LIFE_YEARS / GAL_AF, af_life_total_hi = units * gal_yr_hi * LIFE_YEARS / GAL_AF,
          program = prog_lab[type])]
ce <- ce[match(names(prog_lab), type)]
# Annual water saved by rebates installed by each year end (acre-feet per year), by program and retailer.
first_rb <- merge(first_rb, loc[, .(location_id_from_utility = location_id, utility_name_loc = utility_name)], by = "location_id_from_utility")
ends <- data.table(period = c(as.character(2010:2015), "2016 (Apr)"), end = as.IDate(c(sprintf("%d-12-31", 2010:2015), "2016-04-30")))
inst <- rbindlist(lapply(seq_len(nrow(ends)), function(i) {
  x <- first_rb[rebate_instance_post_date <= ends$end[i]]
  rbind(x[, .(utility_name = "All retailers", rebates = .N, units = sum(rebate_instance_quantity)), by = .(type = rebate_instance_type)],
        x[, .(rebates = .N, units = sum(rebate_instance_quantity)), by = .(utility_name = utility_name_loc, type = rebate_instance_type)])[
    , period := ends$period[i]]
}))
inst <- merge(inst, annual[, .(type, gal_yr, gal_yr_lo, gal_yr_hi)], by = "type")
inst[, `:=`(af_per_year = units * gal_yr / GAL_AF, af_per_year_lo = units * gal_yr_lo / GAL_AF, af_per_year_hi = units * gal_yr_hi / GAL_AF,
            program = prog_lab[type])]
for (c in c("af_per_year", "af_per_year_lo", "af_per_year_hi")) set(inst, j = c, value = supp(inst[[c]], inst$rebates))
inst[, rebates := fifelse(rebates >= MIN_CELL, rebates, NA_integer_)]
water_saved <- list(life_years = LIFE_YEARS, gallons_per_af = GAL_AF,
  cost_effectiveness = ce[, .(type, program, unit, basis, rebates, units, paid, payment_per_unit, gal_yr, gal_yr_lo, gal_yr_hi,
                              af_life_per_unit, dollars_per_af, dollars_per_af_low, dollars_per_af_high, af_life_total, af_life_total_lo, af_life_total_hi)],
  installed = inst[, .(utility_name, period, type, program, rebates, af_per_year, af_per_year_lo, af_per_year_hi)][order(utility_name, period, type)])

# ---- Data quality ------------------------------------------------------------------------------------------------
cov <- usage[, .(location_id = location_id_from_utility, utility_name, s = usage_start_date, e = usage_end_date)]
span <- cov[, .(first = min(s), last = max(e)), by = .(location_id, utility_name)]
months <- seq(as.IDate("2010-01-01"), as.IDate("2016-04-01"), by = "month")
mgrid <- data.table(m = months, me = c(months[-1] - 1L, as.IDate("2016-04-30")))
act <- span[mgrid, on = .(first <= me, last >= m), .(utility_name, location_id, month = i.m), nomatch = NULL, allow.cartesian = TRUE]
pres <- unique(cov[mgrid, on = .(s <= me, e >= m), .(location_id, month = i.m), nomatch = NULL, allow.cartesian = TRUE])
pres[, present := TRUE]
act <- merge(act, pres, by = c("location_id", "month"), all.x = TRUE)
coverage <- act[, .(n = .N, missing_share = mean(is.na(present))), by = .(utility_name, month)][, month := format(month, "%Y-%m")]
coverage[, missing_share := supp(missing_share, n)]
ten <- customers[, .(tenures = uniqueN(cust_loc_id), first = min(cust_loc_info_start_date), last = max(cust_loc_info_end_date),
                     cls = cust_loc_class[1], utility_name = utility_name[1]), by = location_id_from_utility][cls == "single family residential"]
turn <- ten[, .(turnover_per_year = sum(tenures - 1) / sum(as.numeric(last - first + 1) / 365.25)), by = utility_name]
qual <- loc[cls == "single family residential", .(sf_locations = .N, defaulted_area_share = mean(defaulted), missing_area_share = mean(is.na(area))),
            by = utility_name]
hh <- customers[cust_loc_class == "single family residential", .(missing_hhsize_share = mean(is.na(cust_loc_hhsize))), by = utility_name]
zero <- usage[, .(zero_usage_share = mean(usage_ccf == 0)), by = utility_name]
quality <- Reduce(function(a, b) merge(a, b, by = "utility_name"), list(qual, hh, turn, zero, cleaning))
quality <- list(by_retailer = quality, coverage = coverage, duplicate_turf_locations = dup)

meta <- list(generated = as.character(Sys.Date()), profile = "default", root_seed = params$seed$root_seed$value, life_years = LIFE_YEARS,
             retailers = ret_cfg$name, min_cell = MIN_CELL,
             note = "All data is simulated. Planted effects are assumptions written into the generator, not knowledge about real households.")
put(meta, "meta.json"); put(overview, "retailers.json"); put(um, "usage_monthly.json"); put(cl, "climate_monthly.json")
put(sup, "supplier_monthly.json"); put(list(by_year = rb_year, by_retailer = rb_ret, by_retailer_year = rb_ret_year), "rebates.json"); put(savings, "savings.json")
put(quality, "quality.json"); put(water_saved, "water_saved.json")

# ---- Answer key (planted effects known only to the generator) ---------------------------------------------------
R <- function(f) fread(file.path(S, "reports", f))
pvh <- R("planted_vs_estimated_households.csv")[estimator == "matched_corr_raw"]
pvh[, quint := cut(planted, quantile(planted, 0:5 / 5), include.lowest = TRUE, labels = FALSE)]
calib <- pvh[, .(n = .N, planted = mean(planted), estimated = mean(estimate)), by = quint][order(quint)]
by_est <- R("planted_vs_estimated_by_estimator.csv")[, label := est_labels[estimator]]
by_ret_pl <- pvh[, .(n = .N, planted_mean = mean(planted)), by = utility_name][, planted_mean := supp(planted_mean, n)]
scen <- R("scenario_table.csv")[estimator %in% c("matched_corr_raw", "unmatched_did", "naive_pre_post", "matched_log"),
                                 .(scenario, description, estimator, label = est_labels[estimator], bias, rmse, coverage90, spearman)]
orb <- params$treatment$value
answer_key <- list(
  warning = "Answer key: planted effects are assumptions written into the generator. Real households have no answer key.",
  planted_mean = mean(pvh$planted), planted_quantiles = qtab(pvh$planted, "Planted effect"),
  by_estimator = by_est[, .(estimator, label, n, mean_estimate, mean_planted, bias, rmse, mae, coverage90, spearman, slope)],
  by_retailer = by_ret_pl, calibration = calib,
  seasonal = R("seasonal_planted_vs_estimated.csv")[, .(month, planted)],
  variance = R("variance_decomposition.csv"),
  other_rebates = data.table(type = c("High efficiency clothes washer", "Weather based irrigation controller"),
                             planted = c(params$treatment$other_rebates$value$hecw$gal_per_day, params$treatment$other_rebates$value$wbic$outdoor_frac)),
  scenarios = scen)
# Planted cost-effectiveness: turf uses the planted mean over the same headline windows as the estimate; washers and
# controllers use the mean planted monthly saving among holders after installation, from the clean panel.
clean <- rd("truth/clean_panel.parquet")[, .(location_id, t, planted_hecw_saving_ccf, planted_wbic_saving_ccf)]
po <- rd("truth/planted_other_rebate_effects.csv")
pm <- function(col, tcol) { h <- po[!is.na(get(tcol)), .(location_id, ti = get(tcol))]
  x <- merge(clean, h, by = "location_id")[t > ti]; mean(x[[col]]) }
pl_gal <- c(turf = answer_key$planted_mean,
            "high efficiency clothes washer" = ccf_gal_yr(pm("planted_hecw_saving_ccf", "t_install_hecw")),
            "weather based irrigation controller" = ccf_gal_yr(pm("planted_wbic_saving_ccf", "t_install_wbic")))
answer_key$cost_effectiveness <- ce[, .(type, program, planted_gal_yr = pl_gal[type],
                                        planted_dollars_per_af = dpa(payment_per_unit, pl_gal[type]),
                                        planted_af_life_total = units * pl_gal[type] * LIFE_YEARS / GAL_AF)]
put(answer_key, "answer_key.json")
message("Prepared ", length(list.files(OUT)), " files in ", OUT)
