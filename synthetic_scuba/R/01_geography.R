# Stage 2a: retailers, zip codes, block groups and American Community Survey covariates.
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), "00_common.R"))
ctx <- init_ctx("01_geography")

ret <- retailers_table(ctx)
inc_sd <- pv(ctx, "geography", "bg_income_sdlog"); inc_b <- pv(ctx, "geography", "bg_income_bounds")
yb_sd <- pv(ctx, "geography", "bg_year_built_sd"); yb_b <- pv(ctx, "geography", "bg_year_built_bounds")
hh_sd <- pv(ctx, "geography", "bg_hhsize_sd"); hh_b <- pv(ctx, "geography", "bg_hhsize_bounds")
load <- pv(ctx, "geography", "shared_effect_loadings")
hh_range <- pv(ctx, "geography", "households_per_bg")
p25 <- pv(ctx, "geography", "pop_25_share")

# Zip codes and block groups. Identifiers are synthetic and cannot resolve to real places.
zips <- ret[, .(zip_index = seq_len(n_zips)), by = .(code, retailer_index)]
zips[, cust_loc_zip := sprintf("SZ%d%02d", retailer_index, zip_index)]
bgs <- ret[, .(bg_index = seq_len(n_bgs), bgs_per_zip = bgs_per_zip[1]), by = .(code, retailer_index)]
bgs[, zip_index := (bg_index - 1L) %/% bgs_per_zip + 1L]
bgs <- merge(bgs, zips[, .(code, zip_index, cust_loc_zip)], by = c("code", "zip_index"))
bgs[, block_group_id := sprintf("SBG-%s-%02d", code, bg_index)]
bgs <- merge(bgs, ret[, .(code, med_income, med_year_built, base_hhsize, pop_density, name)], by = "code")
setorder(bgs, retailer_index, bg_index)

# Block group covariates share an effect z so income, year built and household size covary.
cols <- c("z_shared", "median_hh_income", "median_year_structure_built", "avg_hhsize", "total_households",
          "pop_density_acs_2015", "pop_25_share", "attitude_bg_mean", "lat_center", "lon_center")
for (cc in cols) set(bgs, j = cc, value = NA_real_)
for (i in seq_len(nrow(bgs))) {
  b <- bgs[i]
  use_stream(ctx, "geography", b$block_group_id)
  z <- rnorm(1); e <- rnorm(5)
  inc <- exp(log(b$med_income) + inc_sd * (load$income * z + sqrt(1 - load$income^2) * e[1]))
  inc <- min(max(inc, inc_b[1]), inc_b[2])
  yb <- round(b$med_year_built + yb_sd * (load$year_built * z + sqrt(1 - load$year_built^2) * e[2]))
  yb <- min(max(yb, yb_b[1]), yb_b[2])
  hh <- b$base_hhsize + hh_sd * (load$hhsize * z + sqrt(1 - load$hhsize^2) * e[3])
  hh <- min(max(hh, hh_b[1]), hh_b[2])
  nh <- round(runif(1, hh_range[1], hh_range[2]))
  dens <- b$pop_density * exp(0.3 * e[4])
  s25 <- runif(1, p25[1], p25[2])
  att <- pv(ctx, "geography", "attitude_bg_income_loading") * z + pv(ctx, "geography", "attitude_bg_mean_sd") * e[5]
  # Fictional coordinates on a grid near (0, 0): retailers 0.6 degrees apart, block groups 0.03 degrees apart.
  set(bgs, i, cols, list(z, round(inc, -2), yb, round(hh, 2), nh, round(dens), s25, att,
                         0.6 * (b$retailer_index - 1) + 0.03 * ((b$bg_index - 1) %/% 4),
                         0.03 * ((b$bg_index - 1) %% 4)))
}
register_keys(ctx, "geography", bgs$block_group_id)

# ACS fields (names from the catalogue; education category names are inferred).
bgs[, total_population := round(total_households * avg_hhsize)]
bgs[, pop_25_and_over := round(total_population * pop_25_share)]
lr <- log(bgs$median_hh_income / 80000)
p_lths <- plogis(-2.0 - 1.4 * lr); p_ba <- plogis(-0.6 + 1.6 * lr)
p_grad <- 0.35 * p_ba; p_bach <- p_ba - p_grad
p_rest <- 1 - p_lths - p_ba
bgs[, edu_less_than_high_school := round(pop_25_and_over * p_lths)]
bgs[, edu_bachelors := round(pop_25_and_over * p_bach)]
bgs[, edu_graduate := round(pop_25_and_over * p_grad)]
bgs[, edu_high_school := round(pop_25_and_over * p_rest * 0.45)]
bgs[, edu_some_college := pop_25_and_over - edu_less_than_high_school - edu_bachelors - edu_graduate - edu_high_school]

write_table(ret, wpath(ctx, "retailers.csv"), "retailer_index")
write_table(zips, wpath(ctx, "zips.csv"), c("retailer_index", "zip_index"))
write_table(bgs, wpath(ctx, "block_groups.csv"), c("retailer_index", "bg_index"))

# Gate: bounds on block group quantities.
ctx <- gate(ctx, "block group median income within [25,000, 250,000]",
            all(bgs$median_hh_income >= inc_b[1] & bgs$median_hh_income <= inc_b[2]),
            range(bgs$median_hh_income), "all inside")
ctx <- gate(ctx, "block group median year built within [1940, 2010]",
            all(bgs$median_year_structure_built >= yb_b[1] & bgs$median_year_structure_built <= yb_b[2]),
            range(bgs$median_year_structure_built), "all inside")
ctx <- gate(ctx, "block group average household size within [2, 4]",
            all(bgs$avg_hhsize >= hh_b[1] & bgs$avg_hhsize <= hh_b[2]), range(bgs$avg_hhsize), "all inside")
ctx <- gate(ctx, "zip and block group counts", TRUE,
            c(nrow(zips), nrow(bgs)), "about 12 zips and 40 block groups in the default profile")
finish_stage(ctx)
