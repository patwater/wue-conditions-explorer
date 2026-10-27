# Stage 6: the observation layer. Applies data quality features to the clean billing panel and writes the observed
# tables under data/ with catalogue field names. The clean versions stay in truth/.
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), "00_common.R"))
ctx <- init_ctx("08_observation")

ret <- read_table(wpath(ctx, "retailers.csv"))
bgs <- read_table(wpath(ctx, "block_groups.csv"))
locs <- read_table(wpath(ctx, "locations.csv"))
ten <- read_table(wpath(ctx, "tenures.csv"))
bills <- read_table(file.path(ctx$paths$truth, "billing_clean.parquet"))
plan_loc <- read_table(wpath(ctx, "obs_plan_locations.csv"))
plan_ten <- read_table(wpath(ctx, "obs_plan_tenures.csv"))
rb <- read_table(wpath(ctx, "rebates_truth.csv"))
clim <- read_table(file.path(ctx$paths$truth, "climate_monthly.parquet"))
tm <- ctx$time
O <- function(n) pv(ctx, "observation", n)

# ---- Usage: drop feed gaps and outages, then corrupt a small share of rows (bills are left as billed).
miss <- period_missing(ctx, bills[, .(location_id, period_index, usage_end_date)], plan_loc)
u <- merge(bills, miss, by = c("location_id", "period_index"))
u[, `:=`(error_type = "none", usage_ccf_clean = usage_ccf)]
setorder(u, location_id, period_index)
u[, err_u := { use_stream(ctx, "obs_errors", location_id[1]); runif(.N) }, by = location_id]
register_keys(ctx, "obs_errors", unique(u$location_id))
u[err_u < O("error_spike_share"), `:=`(error_type = "spike", usage_ccf = usage_ccf * O("error_spike_factor"))]
u[err_u >= O("error_spike_share") & err_u < O("error_spike_share") + O("error_negative_share") & usage_ccf > 0,
  `:=`(error_type = "negative", usage_ccf = -usage_ccf)]
write_table(u[, .(location_id, period_index, missing, error_type, usage_ccf_clean, usage_ccf_observed = usage_ccf)],
            file.path(ctx$paths$truth, "observation_flags.parquet"), c("location_id", "period_index"))
uo <- u[missing == FALSE]
usage <- uo[, .(cust_id_from_utility, cust_loc_id, location_id_from_utility = location_id, utility_name,
                cust_loc_class_from_utility, usage_start_date, usage_end_date, usage_month, usage_year, usage_ccf,
                usage_et_amount, usage_et_amount_default, usage_indoor_budget_ccf, usage_outdoor_budget_ccf,
                usage_total_bill, is_synthetic = TRUE)]
write_table(usage, file.path(ctx$paths$data, "usage.parquet"), c("location_id_from_utility", "usage_start_date"))

# ---- Customers: one row per customer-location tenure.
cu <- merge(ten, locs[, .(location_id, block_group_id, cust_loc_zip, irr_area_true, unit_count, year_built, meter_size,
                          pressure_zone, street_num, apn, lat, lon, water_type, retailer_index, first_t, last_t)],
            by = c("location_id", "block_group_id"))
cu <- merge(cu, ret[, .(code, name)], by = "code")
cu <- merge(cu, plan_loc[, .(location_id, default_area, default_area_value, missing_area)], by = "location_id")
cu <- merge(cu, plan_ten[, .(cust_loc_id, missing_hhsize)], by = "cust_loc_id")
cu[, cust_loc_irr_area_sf := fifelse(default_area, default_area_value, irr_area_true)]
cu[missing_area == TRUE, cust_loc_irr_area_sf := NA_real_]
cu[, cust_loc_hhsize := fifelse(missing_hhsize, NA_integer_, hhsize)]
cu[cust_class == "MF", cust_loc_hhsize := as.integer(round(unit_count * pv(ctx, "accounts", "mf_persons_per_unit")))]
rate_codes <- c(SF = "RESIDENTIAL_SINGLE", MF = "RESIDENTIAL_MULTI", COM = "COMMERCIAL", IND = "INDUSTRIAL",
                INST = "INSTITUTIONAL", OTH = "OTHER", IRR = "IRRIGATION")
cu[, cust_loc_class_from_utility := fifelse(water_type == "raw", "RAW",
                                     fifelse(water_type == "recycled", "IRRIGATION_RECYCLED", rate_codes[cust_class]))]
customers <- cu[, .(cust_id_from_utility, cust_loc_id, location_id_from_utility = location_id, utility_name = name,
                    cust_loc_class_from_utility, cust_loc_class = class_labels[cust_class], cust_loc_hhsize,
                    cust_loc_water_type = water_type, cust_loc_is_master_meter = cust_class == "MF",
                    cust_loc_irr_area_sf, cust_loc_apn = apn, cust_loc_street_num = street_num,
                    cust_loc_street_name = sprintf("Synthetic Way %s", sub(".*-", "", block_group_id)),
                    cust_loc_city = name, cust_loc_zip,
                    cust_loc_info_start_date = tm$month_start[start_t], cust_loc_info_end_date = tm$month_end[end_t],
                    et_zone_from_utility = paste0("SYN-ET-", code), cust_loc_latitude = round(lat, 6),
                    cust_loc_longitude = round(lon, 6), cust_loc_meter_size = meter_size,
                    cust_loc_pressure_zone = pressure_zone, block_group_id, cust_loc_unit_count = unit_count,
                    cust_loc_year_built = year_built, is_synthetic = TRUE)]
write_table(customers, file.path(ctx$paths$data, "customers.csv"), c("location_id_from_utility", "cust_loc_info_start_date"))

# ---- Rebates: add duplicate turf records for flagged locations (the paper drops these).
turf <- rb[rebate_instance_type == "turf"]
dups <- merge(turf, plan_loc[duplicate_turf == TRUE, .(location_id)], by = "location_id")
if (nrow(dups)) {
  dups[, shift_days := { use_stream(ctx, "obs_duplicate", location_id); sample(5:45, 1) }, by = location_id]
  register_keys(ctx, "obs_duplicate", dups$location_id)
  dups[, `:=`(rebate_instance_received_date = rebate_instance_received_date + shift_days,
              rebate_instance_post_date = rebate_instance_post_date + shift_days,
              rebate_instance_check_date = rebate_instance_check_date + shift_days)]
  dups[, shift_days := NULL]
}
rebates <- rbind(rb, dups)
setorder(rebates, location_id, rebate_instance_post_date, rebate_instance_type)
rebates[, rebate_instance_id := sprintf("SYN-RB-%06d", seq_len(.N))]
rebates <- rebates[, .(rebate_instance_id, rebate_instance_type, rebate_instance_payment, rebate_instance_quantity,
                       rebate_instance_received_date, rebate_instance_post_date, rebate_instance_check_date,
                       cust_id_from_utility, location_id_from_utility = location_id, is_synthetic = TRUE)]
write_table(rebates, file.path(ctx$paths$data, "rebates.csv"), "rebate_instance_id")

# ---- ACS block groups, supplier report and climate.
acs <- bgs[, .(block_group_id, utility_name = name, pop_density_acs_2015, total_population, total_households, avg_hhsize,
               pop_25_and_over, edu_less_than_high_school, edu_high_school, edu_some_college, edu_bachelors, edu_graduate,
               median_hh_income, median_year_structure_built, is_synthetic = TRUE)]
write_table(acs, file.path(ctx$paths$data, "acs_block_groups.csv"), "block_group_id")
write_table(read_table(wpath(ctx, "supplier_report.csv")), file.path(ctx$paths$data, "supplier_report.csv"), c("utility_id", "report_month"))
file.copy(wpath(ctx, "generator_params.json"), file.path(ctx$paths$truth, "generator_parameters.json"), overwrite = TRUE)
cl <- merge(clim, ret[, .(code, name)], by = "code")[, .(utility_name = name, et_zone_from_utility = paste0("SYN-ET-", code),
  year, month, usage_et_amount, usage_et_amount_default, cimis_eto_in = eto_in, precip_in, is_synthetic = TRUE)]
write_table(cl, file.path(ctx$paths$data, "climate_et_precip.csv"), c("utility_name", "year", "month"))

# ---- Data dictionary: catalogue field names are marked catalogue; everything else is inferred.
catalogue_fields <- c("cust_id_from_utility", "usage_et_amount", "usage_outdoor_budget_ccf", "usage_indoor_budget_ccf",
  "usage_total_bill", "usage_start_date", "usage_end_date", "usage_month", "usage_year", "usage_ccf", "cust_loc_id",
  "cust_loc_class_from_utility", "usage_et_amount_default", "cust_loc_class", "cust_loc_hhsize", "cust_loc_water_type",
  "cust_loc_is_master_meter", "cust_loc_irr_area_sf", "cust_loc_apn", "cust_loc_street_num", "cust_loc_street_name",
  "cust_loc_city", "cust_loc_zip", "cust_loc_info_start_date", "cust_loc_info_end_date", "et_zone_from_utility",
  "cust_loc_latitude", "cust_loc_longitude", "cust_loc_meter_size", "cust_loc_pressure_zone", "rebate_instance_type",
  "rebate_instance_payment", "rebate_instance_quantity", "rebate_instance_post_date", "location_id_from_utility",
  "rebate_instance_check_date", "rebate_instance_received_date", "pop_density_acs_2015", "total_population",
  "total_households", "avg_hhsize", "pop_25_and_over", "median_hh_income", "median_year_structure_built",
  grep("^report_", names(read_table(file.path(ctx$paths$data, "supplier_report.csv"))), value = TRUE), "utility_id", "utility_name")
dd <- rbindlist(list(data.table(table = "usage.parquet", field = names(usage)), data.table(table = "customers.csv", field = names(customers)),
                     data.table(table = "rebates.csv", field = names(rebates)), data.table(table = "acs_block_groups.csv", field = names(acs)),
                     data.table(table = "supplier_report.csv", field = names(read_table(file.path(ctx$paths$data, "supplier_report.csv")))),
                     data.table(table = "climate_et_precip.csv", field = names(cl))))
dd[, name_source := fifelse(field %in% catalogue_fields, "catalogue", "inferred")]
dd[field == "location_id_from_utility" & table != "rebates.csv", name_source := "catalogue (rebate table); used here for linkage"]
dd[field == "utility_name", name_source := "catalogue (QC code); used here for linkage"]
dd[grepl("^edu_", field), name_source := "inferred (catalogue lists education counts without field names)"]
dd[field %in% c("report_month", "year", "month"), name_source := "inferred"]
write_table(dd, file.path(ctx$paths$data, "data_dictionary.csv"))

# ---- Gates. The cohort is computed from the observed tables with the estimator's own filter.
co <- analysis_cohort(usage, customers, rebates, "turf", pv(ctx, "panel", "analysis_pre_months"),
                      pv(ctx, "panel", "analysis_post_months"), O("bunching_min_count"))
n_co <- sum(co$passes_filters)
cr <- unlist(ctx$profile$cohort_range)
ctx <- gate(ctx, "treated households passing the analysis filters", n_co >= cr[1] & n_co <= cr[2], n_co,
            sprintf("%d to %d", cr[1], cr[2]), absolute = TRUE)
ol <- observed_locations(customers, O("bunching_min_count"))
turf_locs <- unique(rebates[rebate_instance_type == "turf", location_id_from_utility])
present <- unique(usage$location_id_from_utility)
cc <- ol[cust_loc_class == "single family residential" & !location_id %in% turf_locs & location_id %in% present, .N, by = cust_loc_zip]
ctx <- gate(ctx, "single family control candidates in every zip code", min(cc$N) >= pv(ctx, "matching", "min_controls_per_zip"),
            min(cc$N), ">= 100 in every zip", absolute = TRUE)
rate_gate <- function(ctx, check, k, n, rate) share_gate(ctx, paste(check, sprintf("(configured %.3f)", rate)), k, n, rate * c(0.8, 1.2))
ep <- plan_loc[, .(n = .N, k = sum(!is.na(gap_start)))]
ctx <- rate_gate(ctx, "share of locations with a feed gap", ep$k, ep$n, O("missing_row_episode_prob"))
mh <- O("missing_hhsize_share")
for (cc_ in names(mh)) {
  x <- merge(plan_ten[code == cc_], ten[, .(cust_loc_id, cust_class)], by = "cust_loc_id")[cust_class == "SF"]
  ctx <- rate_gate(ctx, paste("missing household size,", cc_), sum(x$missing_hhsize), nrow(x), mh[[cc_]])
}
ma <- O("missing_irr_area_share")
for (cc_ in names(ma)) {
  x <- plan_loc[code == cc_ & cust_class == "SF"]
  ctx <- rate_gate(ctx, paste("missing irrigable area,", cc_), sum(x$missing_area), nrow(x), ma[[cc_]])
}
da <- O("default_irr_area")
for (cc_ in names(da)) {
  x <- plan_loc[code == cc_ & cust_class == "SF"]
  ctx <- rate_gate(ctx, paste("defaulted irrigable area,", cc_), sum(x$default_area), nrow(x), da[[cc_]]$share)
}
tl <- unique(turf$location_id)
ctx <- rate_gate(ctx, "duplicate turf rebates", sum(plan_loc[location_id %in% tl, duplicate_turf]), length(tl), O("duplicate_turf_share"))
ctx <- rate_gate(ctx, "spike errors in usage rows", sum(uo$error_type == "spike"), nrow(uo), O("error_spike_share"))
ctx <- rate_gate(ctx, "negative errors in usage rows", sum(uo$error_type == "negative"), nrow(uo), O("error_negative_share"))
ctx <- gate(ctx, "share of billing rows missing (reported)", TRUE, mean(u$missing), "report only")
det <- ol[defaulted_area == TRUE, location_id]; pl_def <- plan_loc[default_area == TRUE & missing_area == FALSE, location_id]
ctx <- gate(ctx, "bunching detection finds every defaulted area, with at most 1 percent false positives",
            length(setdiff(pl_def, det)) == 0 && length(setdiff(det, pl_def)) <= max(1, 0.01 * length(det)),
            c(length(det), length(setdiff(pl_def, det)), length(setdiff(det, pl_def))), "detected, missed, false positives")
finish_stage(ctx)
