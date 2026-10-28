# Reconciliation test: recomputes headline dashboard figures directly from the synthetic SCUBA tables and compares
# them with the prepared JSON in dashboard/app/data/. Exits with status 1 on any mismatch.
#   Rscript dashboard/tests/reconcile.R
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(nanoparquet) })
here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
root <- normalizePath(file.path(here, "..", ".."))
S <- file.path(root, "synthetic_scuba"); J <- file.path(root, "dashboard", "app", "data")
js <- function(f) fromJSON(file.path(J, f))
fails <- 0
check <- function(name, got, want, tol = 1e-3) {
  ok <- length(got) == length(want) && all(abs(as.numeric(got) - as.numeric(want)) <= tol * pmax(1, abs(as.numeric(want))), na.rm = FALSE)
  cat(sprintf("%-4s %s\n", if (isTRUE(ok)) "PASS" else "FAIL", name))
  if (!isTRUE(ok)) { fails <<- fails + 1; print(list(dashboard = got, recomputed = want)) }
}
cu <- fread(file.path(S, "data", "customers.csv"))
usage <- as.data.table(read_parquet(file.path(S, "data", "usage.parquet")))
rb <- fread(file.path(S, "data", "rebates.csv"))

# 1. Accounts active in January 2010 by retailer.
acc <- cu[, .(start = min(cust_loc_info_start_date), u = utility_name[1]), by = location_id_from_utility][start <= as.IDate("2010-01-31"), .N, by = u]
r <- as.data.table(js("retailers.json"))
check("accounts active in January 2010", r[order(name), accounts_jan2010], acc[order(u), N])

# 2. Median single family use in July 2013 for Surf City (monthly billing), cleaned as the estimator cleans it.
cls <- cu[, .(cls = cust_loc_class[1]), by = .(location_id_from_utility)]
u <- merge(usage, cls, by = "location_id_from_utility")[utility_name == "Surf City" & cls == "single family residential"]
u[, y := usage_ccf / (as.integer(usage_end_date - usage_start_date) + 1L) * 30.4375]
u[, med := median(y[usage_ccf >= 0]), by = location_id_from_utility]
u <- u[usage_ccf >= 0 & !(y > 5 * med & y > 40) & format(usage_start_date + 15L, "%Y-%m") == "2013-07"]
um <- as.data.table(js("usage_monthly.json"))
check("median Surf City single family use, July 2013", um[utility_name == "Surf City" & month == "2013-07", median_ccf], median(u$y))

# 3. Turf rebates by year (all retailers, cells of 10 or more).
ty <- rb[rebate_instance_type == "turf", .N, by = .(yr = as.integer(format(rebate_instance_post_date, "%Y")))][N >= 10][order(yr)]
rj <- as.data.table(js("rebates.json")$by_year)[type == "turf" & !is.na(n)][order(yr)]
check("turf rebates by year", rj$n, ty$N, 0)

# 4. Pooled savings estimates and the household count.
pe <- fread(file.path(S, "estimates", "pooled_estimates.csv"))
sj <- js("savings.json")
check("pooled savings estimates", sj$pooled$estimate, pe$estimate)
he <- fread(file.path(S, "estimates", "household_estimates.csv"))[primary == TRUE]
check("treated households analyzed", sj$cohort$households, nrow(he), 0)
check("mean of primary household estimates", as.data.table(sj$estimators)[estimator == "matched_corr_raw", mean], mean(he$estimate))

# 5. Answer key planted mean against the validation report.
pv <- fread(file.path(S, "reports", "planted_vs_estimated_households.csv"))[estimator == "matched_corr_raw"]
check("answer key planted mean", js("answer_key.json")$planted_mean, mean(pv$planted))

# 6. Minimum cell size: no usage aggregate built from fewer than 10 accounts is shown.
check("no usage cell below the minimum size is shown", sum(!is.na(um$median_ccf) & um$n < 10), 0, 0)

# 7. Dollars per acre-foot for turf: payment per sq ft / (pooled gal per sq ft per yr x 10 years / gallons per AF),
#    with duplicate rebate records counted once per location.
ws <- js("water_saved.json")
tr <- rb[rebate_instance_type == "turf"][order(rebate_instance_post_date)][, .SD[1], by = location_id_from_utility]
want <- (sum(tr$rebate_instance_payment) / sum(tr$rebate_instance_quantity)) / (pe[model == "stacked_did_fixest", estimate] * ws$life_years / 325851)
check("turf rebate dollars per acre-foot (10-year life)", as.data.table(ws$cost_effectiveness)[type == "turf", dollars_per_af], want)

# 8. Water saved per year by the end of the panel, all retailers: units installed x annual savings per unit.
inst <- as.data.table(ws$installed)[utility_name == "All retailers" & period == "2016 (Apr)" & type == "turf"]
check("turf acre-feet per year saved by April 2016", inst$af_per_year, sum(tr$rebate_instance_quantity) * pe[model == "stacked_did_fixest", estimate] / 325851)

if (fails > 0) { cat(fails, "check(s) failed\n"); quit(status = 1) } else cat("All reconciliation checks passed\n")
