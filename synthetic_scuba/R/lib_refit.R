# Fake-data recovery (behind --refit), sourced by 10_validate.R. Refits the multilevel error structure of the
# generator to untreated single family usage with brms (rstan backend), using the log of the stored structural mean
# as an offset, on a subsample stratified by retailer.
suppressPackageStartupMessages(library(brms))
t_start <- Sys.time()
cl <- read_table(file.path(P_$truth, "clean_panel.parquet"))[cust_class == "SF" & vacant == FALSE]
n_sub <- pv(ctx, "compute", "refit_accounts")
locs_sf <- unique(cl[, .(location_id, code)])
use_stream(ctx, "refit_sample", "all")
sub <- locs_sf[, .(location_id = sample(location_id, max(1, round(n_sub * .N / nrow(locs_sf))))), by = code]
d <- cl[location_id %in% sub$location_id, .(y = log(y_untreated), log_mu = log(mu_struct), code, block_group_id, cust_loc_id, t,
                                            rm = paste(code, t))]
vol <- pv(ctx, "usage", "voluntary_effect"); man <- pv(ctx, "usage", "mandatory_effect")
d[, `:=`(vol = as.integer(t >= month_index(ctx, vol$start)), man = as.integer(t >= month_index(ctx, man$start)))]
pri <- pv(ctx, "usage", "priors")
priors <- c(prior_string(sprintf("normal(%s, %s)", pri$voluntary_effect$mean, pri$voluntary_effect$sd), coef = "vol"),
            prior_string(sprintf("normal(%s, %s)", pri$mandatory_effect$mean, pri$mandatory_effect$sd), coef = "man"),
            prior(normal(0, 0.2), class = "Intercept"),
            prior_string(sprintf("normal(0, %s)", pri$sigma_retailer$scale), class = "sd", group = "code"),
            prior_string(sprintf("normal(0, %s)", pri$sigma_bg$scale), class = "sd", group = "block_group_id"),
            prior_string(sprintf("normal(0, %s)", pri$sigma_hh$scale), class = "sd", group = "cust_loc_id"),
            prior_string(sprintf("normal(0, %s)", pri$sigma_shock$scale), class = "sd", group = "rm"),
            prior_string(sprintf("normal(0, %s)", pri$ar_sigma$scale), class = "sigma"))
it <- pv(ctx, "compute", "refit_iter")
fit <- brm(bf(y ~ offset(log_mu) + vol + man + (1 | code) + (1 | block_group_id) + (1 | cust_loc_id) + (1 | rm) +
                ar(time = t, gr = cust_loc_id, p = 1)), data = d, prior = priors, backend = "rstan", chains = 4,
           cores = ctx$opt$cores, iter = sum(it), warmup = it[1], seed = stream_int(ctx$seed, "refit", "brms"), refresh = 0,
           control = list(adapt_delta = 0.9))
gp <- read_json(wpath(ctx, "generator_params.json"))$usage
bgm <- read_table(wpath(ctx, "block_groups.csv"))
truth_vals <- c(sd_code = gp$sigma_retailer, sd_bg = sqrt(gp$sigma_bg^2 + gp$beta_attitude^2 * var(bgm$attitude_bg_mean)),
                sd_hh = sqrt(gp$sigma_hh^2 + gp$beta_attitude^2 * pv(ctx, "accounts", "attitude_sd")^2),
                ar_phi = gp$ar_phi, sigma = gp$ar_sigma, sd_shock = gp$sigma_shock, vol = gp$voluntary_effect, man = gp$mandatory_effect)
dr <- as_draws_df(fit)
col <- c(sd_code = "sd_code__Intercept", sd_bg = "sd_block_group_id__Intercept", sd_hh = "sd_cust_loc_id__Intercept",
         ar_phi = "ar[1]", sigma = "sigma", sd_shock = "sd_rm__Intercept", vol = "b_vol", man = "b_man")
rec <- rbindlist(lapply(names(col), function(n) {
  x <- dr[[col[[n]]]]
  q <- quantile(x, c(0.05, 0.5, 0.95))
  data.table(parameter = n, generator_value = truth_vals[[n]], post_median = q[2], q05 = q[1], q95 = q[3],
             covered90 = truth_vals[[n]] >= q[1] & truth_vals[[n]] <= q[3])
}))
rec[, note := fifelse(parameter == "sd_code", "Only five retailers, so this scale is weakly identified.",
              fifelse(parameter %in% c("ar_phi", "sigma"), "The generator's slow trend and local level are not in the fitted model and leak into the AR term.", ""))]
write_table(rec, file.path(P_$reports, "refit_recovery.csv"))
mins <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
ctx <- gate(ctx, "refit: generator values inside 90 percent posterior intervals (reported)", TRUE,
            c(sum(rec$covered90), nrow(rec), round(mins, 1)), "covered, parameters, minutes")
