# Stage 2b: locations active in January 2010, their customer class and property attributes.
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), "00_common.R"))
ctx <- init_ctx("02_accounts")

ret <- read_table(wpath(ctx, "retailers.csv"))
bgs <- read_table(wpath(ctx, "block_groups.csv"))
shares <- unlist(pv(ctx, "classes", "shares"))

# Exact class counts per retailer by largest remainder, so realized shares match the configuration.
class_counts <- function(n, s) {
  raw <- n * s; k <- floor(raw)
  rem <- n - sum(k)
  if (rem > 0) k[order(-(raw - k), names(s))[seq_len(rem)]] <- k[order(-(raw - k), names(s))[seq_len(rem)]] + 1
  k
}

locs <- rbindlist(lapply(seq_len(nrow(ret)), function(i) {
  R <- ret[i]
  B <- bgs[code == R$code]
  per_bg <- rep(R$n_accounts %/% nrow(B), nrow(B))
  extra <- R$n_accounts - sum(per_bg)
  if (extra > 0) per_bg[seq_len(extra)] <- per_bg[seq_len(extra)] + 1L
  dt <- data.table(code = R$code, block_group_id = rep(B$block_group_id, per_bg))
  dt[, location_id := sprintf("%s-L%06d", code, seq_len(.N))]
  use_stream(ctx, "classes", R$code)
  k <- class_counts(R$n_accounts, shares)
  dt[, cust_class := sample(rep(names(k), k))]
  dt
}))
register_keys(ctx, "classes", ret$code)

props <- rbindlist(lapply(seq_len(nrow(locs)), function(i) {
  L <- locs[i]
  B <- bgs[block_group_id == L$block_group_id]
  R <- ret[code == L$code]
  as.data.table(draw_property(ctx, L, B, R))
}))
locs <- cbind(locs, props)
register_keys(ctx, "accounts", locs$location_id)

# A handful of Valley Small Town irrigation and industrial accounts receive raw water.
n_raw <- pv(ctx, "classes", "raw_water_accounts_valley")
cand <- locs[code == "VS" & cust_class %in% c("IRR", "IND") & water_type == "potable"]
if (nrow(cand)) {
  raw_ids <- cand[order(raw_u)][seq_len(min(n_raw, .N)), location_id]
  locs[location_id %in% raw_ids, water_type := "raw"]
}
locs[, raw_u := NULL]
locs[, `:=`(first_t = 1L, is_new_construction = FALSE)]
locs <- merge(locs, bgs[, .(block_group_id, cust_loc_zip, retailer_index)], by = "block_group_id")
setorder(locs, retailer_index, location_id)
write_table(locs, wpath(ctx, "locations_initial.csv"), c("retailer_index", "location_id"))

# Gates: class shares and bounded quantities.
real <- locs[, .N, by = cust_class][, share := N / sum(N)]
cmp <- merge(data.table(cust_class = names(shares), target = shares), real, by = "cust_class", all.x = TRUE)
cmp[is.na(share), share := 0]
ctx <- gate(ctx, "class shares within 1 percentage point of configuration",
            all(abs(cmp$share - cmp$target) <= 0.01), max(abs(cmp$share - cmp$target)), "max abs diff <= 0.01")
yb_b <- pv(ctx, "accounts", "year_built_bounds")
ctx <- gate(ctx, "account year built within [1900, 2009]", all(locs$year_built >= yb_b[1] & locs$year_built <= yb_b[2]),
            range(locs$year_built), "all inside")
ab <- pv(ctx, "accounts", "irr_area_bounds")
sf <- locs[cust_class == "SF"]
ctx <- gate(ctx, "single family irrigable area within [500, 40,000]", all(sf$irr_area_true >= ab[1] & sf$irr_area_true <= ab[2]),
            range(sf$irr_area_true), "all inside")
ia <- pv(ctx, "accounts", "irrigation_area")
ir <- locs[cust_class == "IRR"]
ctx <- gate(ctx, "irrigation account area within [5,000, 200,000]", all(ir$irr_area_true >= ia$bounds[1] & ir$irr_area_true <= ia$bounds[2]),
            if (nrow(ir)) range(ir$irr_area_true) else NA, "all inside")
med <- sf[, .(med_area = median(irr_area_true)), by = code][ret[, .(code, med_irr_area)], on = "code"]
ctx <- gate(ctx, "inland and valley median irrigable area exceeds Surf City",
            all(med[code %in% c("IE", "VS"), med_area] > med[code == "SC", med_area]),
            med$med_area, "IE and VS > SC")
finish_stage(ctx)
