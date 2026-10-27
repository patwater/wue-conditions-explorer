# Stage 2c: occupancy timeline. Occupant turnover, new construction and closures are three distinct
# processes, and none of them is missing data. Generated before usage so the panel is built once.
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), "00_common.R"))
ctx <- init_ctx("03_occupancy")

ret <- read_table(wpath(ctx, "retailers.csv"))
bgs <- read_table(wpath(ctx, "block_groups.csv"))
locs <- read_table(wpath(ctx, "locations_initial.csv"))
tm <- ctx$time; nT <- nrow(tm)

# ---- New construction: Poisson arrivals by retailer-month, concentrated in So Cal Suburb and Inland Enclave.
nc_shares <- unlist(pv(ctx, "occupancy", "new_construction_class_shares"))
new_locs <- rbindlist(lapply(seq_len(nrow(ret)), function(i) {
  R <- ret[i]
  use_stream(ctx, "construction", R$code)
  n_t <- rpois(nT - 1L, R$new_construction_rate * R$n_accounts / 12)
  n <- sum(n_t)
  if (n == 0) return(NULL)
  B <- bgs[code == R$code]
  data.table(code = R$code, first_t = rep(2:nT, n_t),
             block_group_id = B$block_group_id[sample.int(nrow(B), n, replace = TRUE)],
             cust_class = sample(names(nc_shares), n, replace = TRUE, prob = nc_shares),
             location_id = sprintf("%s-L%06d", R$code, R$n_accounts + seq_len(n)))
}))
register_keys(ctx, "construction", ret$code)
if (nrow(new_locs)) {
  props <- rbindlist(lapply(seq_len(nrow(new_locs)), function(i) {
    L <- new_locs[i]
    B <- bgs[block_group_id == L$block_group_id]
    R <- ret[code == L$code]
    as.data.table(draw_property(ctx, L, B, R, new_construction_year = tm$year[L$first_t]))
  }))
  new_locs <- cbind(new_locs, props)
  new_locs[, raw_u := NULL]
  new_locs[, is_new_construction := TRUE]
  new_locs <- merge(new_locs, bgs[, .(block_group_id, cust_loc_zip, retailer_index)], by = "block_group_id")
  register_keys(ctx, "accounts_new", new_locs$location_id)
}
locs <- rbind(locs, new_locs, use.names = TRUE, fill = TRUE)
setorder(locs, retailer_index, location_id)

# ---- Closures: very rare; rows stop from the closure month on.
h_close <- 1 - (1 - pv(ctx, "occupancy", "closure_rate_annual"))^(1 / 12)
locs[, last_t := nT]
for (i in seq_len(nrow(locs))) {
  use_stream(ctx, "closure", locs$location_id[i])
  u <- runif(nT)
  ft <- locs$first_t[i]
  hit <- which(u < h_close & seq_len(nT) > ft + 1L)
  if (length(hit)) set(locs, i, "last_t", as.integer(hit[1] - 1L))
}
register_keys(ctx, "closure", locs$location_id)
locs[, closed := last_t < nT]

# ---- Occupant turnover for single family homes, with a vacancy of 0 to 3 months under the incoming account.
h_turn <- 1 - (1 - pv(ctx, "occupancy", "turnover_rate_annual"))^(1 / 12)
vmax <- pv(ctx, "occupancy", "vacancy_months_max")
ten_list <- vector("list", nrow(locs))
for (i in seq_len(nrow(locs))) {
  L <- locs[i]
  starts <- L$first_t; vac <- 0L
  if (L$cust_class == "SF") {
    use_stream(ctx, "occupancy", L$location_id)
    u <- runif(nT); vd <- sample(0:vmax, 40, replace = TRUE)
    t <- L$first_t + 1L; k <- 1L
    while (t <= L$last_t) {
      occupied_from <- starts[k] + vac[k]
      if (t > occupied_from && u[t] < h_turn) {
        k <- k + 1L
        starts[k] <- t
        vac[k] <- min(vd[k], L$last_t - t + 1L)
      }
      t <- t + 1L
    }
  }
  n <- length(starts)
  ten_list[[i]] <- data.table(location_id = L$location_id, code = L$code, cust_class = L$cust_class,
                              tenure_index = seq_len(n), start_t = starts,
                              end_t = c(starts[-1] - 1L, L$last_t), vacancy_months = vac)
}
register_keys(ctx, "occupancy", locs[cust_class == "SF", location_id])
ten <- rbindlist(ten_list)
ten[, occupied_from_t := start_t + vacancy_months]

# ---- Occupant attributes: household size, latent attitude, indoor gpcd, overwatering factor and the
# fraction of irrigation kept running during a vacancy. Each tenure has its own stream.
ten <- merge(ten, locs[, .(location_id, block_group_id)], by = "location_id")
ten <- merge(ten, bgs[, .(block_group_id, avg_hhsize, attitude_bg_mean)], by = "block_group_id")
hmax <- pv(ctx, "accounts", "hhsize_max")
ten[, `:=`(hhsize = NA_integer_, attitude = NA_real_, gpcd = NA_real_, overwater = NA_real_, vacancy_irrigation_frac = NA_real_)]
for (i in seq_len(nrow(ten))) {
  if (ten$cust_class[i] != "SF") next
  use_stream(ctx, "occupant", paste(ten$location_id[i], ten$tenure_index[i]))
  lam <- max(ten$avg_hhsize[i] - 1, 0.1)
  h <- 1L + rpois(1, lam)
  for (k in 1:50) if (h > hmax) h <- 1L + rpois(1, lam) else break
  set(ten, i, c("hhsize", "attitude", "gpcd", "overwater", "vacancy_irrigation_frac"),
      list(as.integer(min(h, hmax)), ten$attitude_bg_mean[i] + pv(ctx, "accounts", "attitude_sd") * rnorm(1),
           exp(log(pv(ctx, "accounts", "gpcd_median")) + pv(ctx, "accounts", "gpcd_sdlog") * rnorm(1)),
           exp(pv(ctx, "accounts", "overwater_sdlog") * rnorm(1)), runif(1)))
}
register_keys(ctx, "occupant", ten[cust_class == "SF", paste(location_id, tenure_index)])

# Opaque customer identifiers, numbered within retailer in a fixed order.
setorder(ten, code, location_id, tenure_index)
ten[, n := seq_len(.N), by = code]
ten[, `:=`(cust_loc_id = sprintf("%s-CL%07d", code, n), cust_id_from_utility = sprintf("%s-C%07d", code, n))]
ten[, c("n", "avg_hhsize", "attitude_bg_mean") := NULL]

# ---- Events for the truth folder.
tdate <- function(t) tm$month_start[t]
ten[, cust_loc_id_prev := shift(cust_loc_id), by = location_id]
ev_turn <- ten[tenure_index > 1, .(event_type = "turnover", location_id, t = start_t, cust_loc_id_new = cust_loc_id,
                                    vacancy_months, cust_loc_id_old = cust_loc_id_prev)]
ten[, cust_loc_id_prev := NULL]
ev_new <- ten[tenure_index == 1][locs[is_new_construction == TRUE, .(location_id, first_t)], on = "location_id"][
  , .(event_type = "new_construction", location_id, t = first_t, cust_loc_id_new = cust_loc_id, vacancy_months = 0L,
      cust_loc_id_old = NA_character_)]
last_ten <- ten[, .SD[which.max(tenure_index)], by = location_id]
ev_close <- last_ten[locs[closed == TRUE, .(location_id, last_t)], on = "location_id"][
  , .(event_type = "closure", location_id, t = last_t + 1L, cust_loc_id_new = NA_character_, vacancy_months = NA_integer_,
      cust_loc_id_old = cust_loc_id)]
events <- rbind(ev_turn, ev_new, ev_close, use.names = TRUE)
events[, event_date := tdate(t)]
events[, is_synthetic := TRUE]

write_table(locs, wpath(ctx, "locations.csv"), c("retailer_index", "location_id"))
write_table(ten, wpath(ctx, "tenures.csv"), c("code", "location_id", "tenure_index"))
write_table(events, file.path(ctx$paths$truth, "occupancy_events.csv"), c("event_type", "location_id", "t"))

# ---- Gates.
sf_ten <- ten[cust_class == "SF"]
exposure_years <- sf_ten[, sum(pmax(0L, end_t - occupied_from_t))] / 12
rate <- sum(sf_ten$tenure_index > 1) / exposure_years
ctx <- gate(ctx, "realized single family turnover rate per year", rate >= 0.06 & rate <= 0.08, rate, "between 0.06 and 0.08")
nc <- locs[is_new_construction == TRUE]
ctx <- gate(ctx, "new construction year built equals first billing year",
            all(nc$year_built == tm$year[nc$first_t]), nrow(nc), "all")
share_conc <- if (nrow(nc)) mean(nc$code %in% c("SS", "IE")) else NA
ctx <- gate(ctx, "new construction concentrated in So Cal Suburb and Inland Enclave", isTRUE(share_conc > 0.6) || nrow(nc) < 5,
            share_conc, "share > 0.6")
nc_rate <- nrow(nc) / sum(ret$n_accounts) / ((nT - 1) / 12)
ctx <- gate(ctx, "new construction rate near 1 percent of accounts per year", nc_rate > 0.006 & nc_rate < 0.016,
            nc_rate, "between 0.006 and 0.016")
ctx <- gate(ctx, "closures are rare", mean(locs$closed) < 0.02, mean(locs$closed), "share of locations closed < 0.02")
ctx <- gate(ctx, "vacancy gaps between 0 and 3 months", all(ten$vacancy_months >= 0 & ten$vacancy_months <= vmax),
            range(ten$vacancy_months), "all inside")
finish_stage(ctx)
