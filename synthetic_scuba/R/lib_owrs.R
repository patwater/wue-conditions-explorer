# Independent bill calculator that reads the published OWRS rate files. Used by 07_billing (gate) and
# 09_validate to confirm that every bill can be recomputed from the observed tables and the rate files.

read_owrs_dir <- function(dir) {
  files <- list.files(dir, pattern = "\\.owrs$", recursive = TRUE, full.names = TRUE)
  rbindlist(lapply(files, function(f) {
    y <- yaml::read_yaml(f)
    eff <- as.Date(y$metadata$effective_date, format = "%m/%d/%Y")
    rbindlist(lapply(names(y$rate_structure), function(cl) {
      r <- y$rate_structure[[cl]]
      data.table(utility_name = y$metadata$utility_name, effective_date = eff, rate_class = cl,
                 bill_frequency = y$metadata$bill_frequency, commodity_charge = r$commodity_charge,
                 service = list(unlist(r$service_charge$values)),
                 tier_starts = list(unlist(r$tier_starts_commodity)), tier_prices = list(unlist(r$tier_prices_commodity)),
                 price = if (is.null(r$price_commodity)) NA_real_ else as.numeric(r$price_commodity))
    }))
  }))
}

# Bills for many usage rows under one rate schedule. usage, indoor and budget in CCF; dollars rounded to the cent.
owrs_bill <- function(rate, usage, meter_size, indoor_budget, total_budget) {
  svc <- as.numeric(rate$service[[1]][meter_size])
  if (rate$commodity_charge == "Uniform") {
    com <- rate$price * usage
  } else {
    st <- rate$tier_starts[[1]]; pr <- as.numeric(rate$tier_prices[[1]])
    starts <- lapply(st, function(x) {
      if (x == "indoor") indoor_budget
      else if (grepl("%$", x)) as.numeric(sub("%", "", x)) / 100 * total_budget
      else rep(as.numeric(x), length(usage))
    })
    com <- 0
    for (k in seq_along(starts)) {
      upper <- if (k < length(starts)) starts[[k + 1]] else Inf
      com <- com + pr[k] * pmax(0, pmin(usage, upper) - starts[[k]])
    }
  }
  round(svc + com, 2)
}

# Joins each bill to the schedule in effect on its end date, then computes bills schedule by schedule.
recompute_bills <- function(bills, rates) {
  r <- copy(rates)[, rate_id := .I]
  setkey(r, utility_name, rate_class, effective_date)
  b <- bills[, .(row = .I, utility_name, rate_class = cust_loc_class_from_utility, effective_date = usage_end_date,
                 usage_ccf, cust_loc_meter_size,
                 ib = fcoalesce(as.numeric(usage_indoor_budget_ccf), 0),
                 ob = fcoalesce(as.numeric(usage_outdoor_budget_ccf), 0))]
  b <- r[, .(utility_name, rate_class, effective_date, rate_id)][b, on = .(utility_name, rate_class, effective_date), roll = TRUE]
  out <- rep(NA_real_, nrow(bills))
  for (id in unique(b$rate_id[!is.na(b$rate_id)])) {
    x <- b[rate_id == id]
    out[x$row] <- owrs_bill(r[rate_id == id], x$usage_ccf, x$cust_loc_meter_size, x$ib, x$ib + x$ob)
  }
  out
}
