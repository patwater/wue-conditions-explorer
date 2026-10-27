# Stage 5: billing periods, integer meter reads, water budgets, synthetic OWRS rate files and bills, and the
# supplier report. Bills use the rate in effect on the period end date.
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), "00_common.R"))
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), "lib_owrs.R"))
ctx <- init_ctx("07_billing")

ret <- read_table(wpath(ctx, "retailers.csv"))
locs <- read_table(wpath(ctx, "locations.csv"))
ten <- read_table(wpath(ctx, "tenures.csv"))
clean <- read_table(file.path(ctx$paths$truth, "clean_panel.parquet"))
clim <- read_table(file.path(ctx$paths$truth, "climate_monthly.parquet"))
plan_loc <- read_table(wpath(ctx, "obs_plan_locations.csv"))
tm <- ctx$time; nT <- nrow(tm)
B <- ctx$params$billing

# ---- Billing periods for every location, split at tenure changes.
bp_in <- merge(locs[, .(location_id, code, first_t, last_t)], ret[, .(code, billing)], by = "code")
bp_in <- merge(bp_in, ten[, .(tenure_starts = list(start_t)), by = location_id], by = "location_id")
periods <- billing_periods(ctx, bp_in)
register_keys(ctx, "billing_cycle", bp_in[billing == "bimonthly", location_id])
ov <- period_month_overlap(ctx, periods)

# Prorate the monthly clean usage and the reported ET to billing periods by days.
mon <- merge(clean[, .(location_id, t, code, y_clean)], clim[, .(code, t, usage_et_amount, usage_et_amount_default)], by = c("code", "t"))
ov <- merge(ov, mon, by = c("location_id", "t"))
agg <- ov[, .(usage_true_ccf = sum(y_clean * w), et_raw = sum(usage_et_amount * w), etd_raw = sum(usage_et_amount_default * w)),
          by = .(location_id, period_index)]
bills <- merge(periods, agg, by = c("location_id", "period_index"))
setorder(bills, location_id, period_index)

# Integer meter reads: each period's usage is the difference of floored cumulative reads, so totals reconcile.
bills[, cum := cumsum(usage_true_ccf), by = location_id]
bills[, read := floor(cum + 1e-9)]
bills[, usage_ccf := read - shift(read, fill = 0), by = location_id]
bills[, `:=`(usage_et_amount = round(et_raw, 2), usage_et_amount_default = round(etd_raw, 2))]

# Tenure (customer) for each period, from the period's start month.
bills[, t_start := month_index(ctx, usage_start_date)]
bills <- ten[, .(location_id, start_t, end_t, cust_loc_id, cust_id_from_utility, hhsize, cust_class)][
  bills, on = .(location_id, start_t <= t_start, end_t >= t_start)]
bills <- merge(bills, locs[, .(location_id, code, irr_area_true, meter_size, water_type)], by = "location_id")
bills <- merge(bills, plan_loc[, .(location_id, default_area, default_area_value)], by = "location_id")
bills[, irr_area_on_file := ifelse(default_area %in% TRUE, default_area_value, irr_area_true)]
bills <- merge(bills, ret[, .(code, name, billing, rate_type, retailer_index)], by = "code")

# Water budgets (OWRS formula) for single family accounts; utilities hold the default area where it was defaulted.
g748 <- B$owrs_gal_per_ccf$value
bills[cust_class == "SF", `:=`(usage_indoor_budget_ccf = round(hhsize * B$budget_gpcd$value * days / g748, 2),
                               usage_outdoor_budget_ccf = round(B$landscape_factor$value * usage_et_amount * irr_area_on_file *
                                                                  B$et_factor$value / g748, 2))]

# ---- Rate schedules: an initial schedule and a 4 percent escalation each July 1.
esc <- B$escalation$value
eff_dates <- c(tm$month_start[1], seq(as.Date(sprintf("%d-%02d-01", tm$year[1], esc$month)),
                                      tm$month_end[nT], by = "year"))
rate_class_of <- function(cls, wt) fifelse(wt == "raw", "RAW", fifelse(wt == "recycled" & cls == "IRR", "IRRIGATION_RECYCLED",
  c(SF = "RESIDENTIAL_SINGLE", MF = "RESIDENTIAL_MULTI", COM = "COMMERCIAL", IND = "INDUSTRIAL", INST = "INSTITUTIONAL",
    OTH = "OTHER", IRR = "IRRIGATION")[cls]))
bills[, cust_loc_class_from_utility := rate_class_of(cust_class, water_type)]
uni_key <- c(RESIDENTIAL_MULTI = "MF", COMMERCIAL = "COM", INDUSTRIAL = "IND", INSTITUTIONAL = "INST", OTHER = "OTH",
             IRRIGATION = "IRR", IRRIGATION_RECYCLED = "IRR_RECYCLED", RAW = "RAW")
svc0 <- unlist(B$service_charge_monthly$value)
owrs_dir <- file.path(ctx$paths$data, "rates_owrs")
unlink(owrs_dir, recursive = TRUE)
for (i in seq_len(nrow(ret))) {
  R <- ret[i]
  mult <- if (R$billing == "bimonthly") 2 else 1
  udir <- file.path(owrs_dir, sprintf("%s - SYN%02d", R$name, R$retailer_index))
  dir.create(udir, recursive = TRUE, showWarnings = FALSE)
  for (k in seq_along(eff_dates)) {
    f <- (1 + esc$rate)^(k - 1)
    svc <- as.list(round(svc0 * mult * f, 2))
    rs <- list()
    rs$RESIDENTIAL_SINGLE <- if (R$rate_type == "budget") {
      list(service_charge = list(depends_on = list("meter_size"), values = svc), commodity_charge = "Budget",
           gpcd_commodity = B$budget_gpcd$value, indoor_commodity = "hhsize*gpcd*days_in_period*(1/748)",
           outdoor_commodity = "landscape_factor*et_amount*irr_area*0.62*(1/748)", budget_commodity = "indoor+outdoor",
           landscape_factor_commodity = B$landscape_factor$value,
           tier_starts_commodity = list(0, "indoor", "100%", "150%"),
           tier_prices_commodity = as.list(round(unlist(B$budget_prices$value) * f, 2)),
           bill = "service_charge+commodity_charge")
    } else {
      list(service_charge = list(depends_on = list("meter_size"), values = svc), commodity_charge = "Tiered",
           tier_starts_commodity = as.list(unlist(B$tiered_starts_monthly$value) * mult),
           tier_prices_commodity = as.list(round(unlist(B$tiered_prices$value[[R$code]]) * f, 2)),
           bill = "service_charge+commodity_charge")
    }
    for (cl in names(uni_key)) rs[[cl]] <- list(service_charge = list(depends_on = list("meter_size"), values = svc),
      commodity_charge = "Uniform", price_commodity = round(B$uniform_prices$value[[uni_key[[cl]]]] * f, 2),
      bill = "service_charge+commodity_charge")
    doc <- list(metadata = list(effective_date = format(eff_dates[k], "%m/%d/%Y"), utility_name = R$name,
                                bill_frequency = if (R$billing == "bimonthly") "Bimonthly" else "Monthly", bill_unit = "ccf",
                                note = "SYNTHETIC rate structure for simulated data; not any real utility's rates."),
                rate_structure = rs)
    writeLines(yaml::as.yaml(doc), file.path(udir, paste0(format(eff_dates[k], "%m-%d-%Y"), ".owrs")))
  }
}

# ---- Bills, computed from the generator's own rate table (independent of the published files).
bills[, k_rate := findInterval(as.numeric(usage_end_date), as.numeric(eff_dates))]
bills[, f := (1 + esc$rate)^(k_rate - 1)]
bills[, mult := fifelse(billing == "bimonthly", 2, 1)]
bills[, service_charge := round(svc0[meter_size] * mult * f, 2)]
bills[, commodity_charge := 0]
tier_sum <- function(u, starts, prices) {
  com <- 0
  for (k in seq_along(starts)) {
    up <- if (k < length(starts)) starts[[k + 1]] else Inf
    com <- com + prices[[k]] * pmax(0, pmin(u, up) - starts[[k]])
  }
  com
}
bp <- unlist(B$budget_prices$value)
x <- bills[cust_loc_class_from_utility == "RESIDENTIAL_SINGLE" & rate_type == "budget"]
bills[x, on = c("location_id", "period_index"), commodity_charge := {
  ib <- i.usage_indoor_budget_ccf; tb <- ib + i.usage_outdoor_budget_ccf; ff <- i.f
  tier_sum(i.usage_ccf, list(0, ib, tb, 1.5 * tb), lapply(bp, function(p) round(p * ff, 2)))
}]
for (cc in ret[rate_type == "tiered", code]) {
  tp_ <- unlist(B$tiered_prices$value[[cc]]); ts_ <- unlist(B$tiered_starts_monthly$value)
  x <- bills[cust_loc_class_from_utility == "RESIDENTIAL_SINGLE" & code == cc]
  bills[x, on = c("location_id", "period_index"), commodity_charge := {
    m <- i.mult; ff <- i.f
    tier_sum(i.usage_ccf, lapply(ts_, function(s0) s0 * m), lapply(tp_, function(p) round(p * ff, 2)))
  }]
}
bills[cust_loc_class_from_utility != "RESIDENTIAL_SINGLE",
      commodity_charge := round(unlist(B$uniform_prices$value)[uni_key[cust_loc_class_from_utility]] * f, 2) * usage_ccf]
bills[, usage_total_bill := round(service_charge + commodity_charge, 2)]
bills[, `:=`(usage_month = as.integer(format(usage_end_date, "%m")), usage_year = as.integer(format(usage_end_date, "%Y")),
             utility_name = name, cust_loc_meter_size = meter_size, is_synthetic = TRUE)]

keep <- c("location_id", "period_index", "cust_loc_id", "cust_id_from_utility", "code", "utility_name", "billing", "billing_cycle",
          "cust_class", "cust_loc_class_from_utility", "cust_loc_meter_size", "usage_start_date", "usage_end_date", "days",
          "usage_month", "usage_year", "usage_true_ccf", "usage_ccf", "usage_et_amount", "usage_et_amount_default",
          "usage_indoor_budget_ccf", "usage_outdoor_budget_ccf", "irr_area_on_file", "service_charge", "commodity_charge",
          "usage_total_bill", "is_synthetic")
bills <- bills[, ..keep]
write_table(bills, file.path(ctx$paths$truth, "billing_clean.parquet"), c("location_id", "period_index"))

# ---- Supplier report (monthly, from the clean monthly truth scaled to the service population).
cl2 <- merge(clean[, .(location_id, t, code, cust_class, cust_loc_id, vacant, y_clean)],
             locs[, .(location_id, water_type, unit_count, irr_area_true)], by = "location_id")
cl2 <- merge(cl2, ten[, .(cust_loc_id, hhsize)], by = "cust_loc_id")
ppu <- pv(ctx, "accounts", "mf_persons_per_unit")
cl2[, persons := fifelse(cust_class == "SF" & !vacant, as.numeric(hhsize), fifelse(cust_class == "MF", unit_count * ppu, 0))]
mon <- cl2[, .(sales_ccf = sum(y_clean[water_type == "potable"]),
               res_ccf = sum(y_clean[water_type == "potable" & cust_class %in% c("SF", "MF")]),
               cii_ccf = sum(y_clean[water_type == "potable" & cust_class %in% c("COM", "IND", "INST")]),
               recycled_ccf = sum(y_clean[water_type == "recycled"]),
               sample_pop = sum(persons), irr_area = sum(irr_area_true[cust_class %in% c("SF", "MF")], na.rm = TRUE)),
           by = .(code, t)]
mon <- merge(mon, ret[, .(code, name, retailer_index, service_population, hydrologic_region)], by = "code")
mon[, k := service_population / sample_pop[t == 1], by = code]
nrw <- pv(ctx, "supplier", "nonrevenue_water"); af <- pv(ctx, "units", "ccf_per_af"); gal_af <- pv(ctx, "units", "gallons_per_af")
mon[, `:=`(year = tm$year[t], month = tm$month[t], days = tm$days[t])]
mon[, report_total_production_af := round(sales_ccf * k * (1 + nrw) / af, 2)]
mon[, report_cii_production_af := round(cii_ccf * k / af, 2)]
mon[, report_recycled_production_af := round(recycled_ccf * k / af, 2)]
p13 <- mon[year == 2013, .(code, month, report_total_production_af_2013 = report_total_production_af)]
mon <- merge(mon, p13, by = c("code", "month"), all.x = TRUE)
mon[, report_population := round(k * sample_pop)]
mon[, report_percent_residential := round(res_ccf / sales_ccf, 4)]
mon[, report_production_calculated := round(report_total_production_af * gal_af)]
mon[, report_production_calculated_2013 := round(report_total_production_af_2013 * gal_af)]
mon[, report_gpcd_calculated := round(report_production_calculated * report_percent_residential / (days * report_population), 2)]
mon[, report_percent_reduction := fifelse(year >= 2014, round(1 - report_total_production_af / report_total_production_af_2013, 4), NA_real_)]
er <- pv(ctx, "supplier", "enforcement_rates")
t_mid14 <- month_index(ctx, "2014-07-01"); t_man <- month_index(ctx, pv(ctx, "usage", "mandatory_effect")$start)
t_vol <- month_index(ctx, pv(ctx, "usage", "voluntary_effect")$start)
setorder(mon, code, t)
mon[, c("report_gpcd_reported", "report_complaints", "report_followups", "report_warnings", "report_penalties_rate",
        "report_penalties_other") := {
  use_stream(ctx, "supplier", code[1])
  m <- fifelse(t >= t_man, er$mult_apr2015, fifelse(t >= t_mid14, er$mult_mid2014, 1))
  lam <- report_population / 10000 * m
  list(round(report_gpcd_calculated * exp(pv(ctx, "supplier", "gpcd_reported_sdlog") * rnorm(.N)), 2),
       rpois(.N, er$complaints * lam), rpois(.N, er$followups * lam), rpois(.N, er$warnings * lam),
       rpois(.N, er$penalties_rate * lam), rpois(.N, er$penalties_other * lam))
}, by = code]
register_keys(ctx, "supplier", ret$code)
eto <- clim[, .(code, t, report_eto = usage_et_amount_default)]
mon <- merge(mon, eto, by = c("code", "t"))
sup <- mon[, .(report_id = sprintf("SYN-%s-%d%02d", code, year, month), utility_id = retailer_index,
               report_agency_name = name, report_pwsid = sprintf("SYN%05d", retailer_index),
               report_hydrologic_region = hydrologic_region, report_month = tm$month_start[t], year, month,
               report_has_restrictions = t >= t_man,
               report_actions_implemented = fifelse(t >= t_man, "Mandatory outdoor irrigation restrictions",
                                                    fifelse(t >= t_vol, "Voluntary conservation outreach", NA_character_)),
               report_total_production_af, report_total_production_af_2013, report_cii_production_af,
               report_ag_production_af = 0, report_ag_production_af_2013 = 0, report_recycled_production_af,
               report_population, report_percent_residential, report_gpcd_reported, report_gpcd_calculated,
               report_ag_cert = FALSE, report_production_calculated, report_production_calculated_2013,
               report_percent_reduction, report_complaints, report_followups, report_warnings, report_penalties_rate,
               report_penalties_other, report_irr_area_sf = round(k * irr_area), report_eto, is_synthetic = TRUE)]
write_table(sup, wpath(ctx, "supplier_report.csv"), c("utility_id", "report_month"))
write_table(mon[, .(code, t, k, sales_ccf, sample_pop)], wpath(ctx, "supplier_scaling.csv"), c("code", "t"))

# ---- Gates.
rates <- read_owrs_dir(owrs_dir)
re <- recompute_bills(bills, rates)
match_share <- mean(abs(re - bills$usage_total_bill) < 0.005)
ctx <- gate(ctx, "every bill recomputed from usage and the published OWRS files matches to the cent", match_share == 1,
            match_share, "share matching = 1")
rec_sup <- mon[, .(rel = abs(sum(report_total_production_af) * af / (1 + nrw) / k[1] - sum(sales_ccf)) / sum(sales_ccf)), by = code]
ctx <- gate(ctx, "supplier production reconciles with account totals after scaling and losses", all(rec_sup$rel < 0.005),
            max(rec_sup$rel), "relative difference < 0.005")
pot <- merge(bills[, .(location_id, usage_ccf, code)], locs[, .(location_id, water_type)], by = "location_id")[water_type == "potable"]
rec_bill <- merge(pot[, .(billed = sum(usage_ccf)), by = code],
                  cl2[water_type == "potable", .(truth = sum(y_clean)), by = code], by = "code")
rec_bill[, rel := abs(billed - truth) / truth]
ctx <- gate(ctx, "billed usage reconciles with the monthly truth panel", all(rec_bill$rel < 0.005), max(rec_bill$rel),
            "relative difference < 0.005")
rb <- read_table(wpath(ctx, "rebates_truth.csv"))
ctx <- gate(ctx, "rebate dates ordered received < post inspection < check",
            all(rb$rebate_instance_received_date < rb$rebate_instance_post_date & rb$rebate_instance_post_date < rb$rebate_instance_check_date),
            nrow(rb), "all rebates")
red <- sup[year == 2015 & month >= 6, .(r = mean(report_percent_reduction)), by = report_agency_name]
ctx <- gate(ctx, "supplier percent reduction vs 2013 in June-December 2015 (reported)", TRUE, red$r, "report only")
finish_stage(ctx)
