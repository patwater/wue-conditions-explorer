# Simulation-based calibration (behind --sbc), sourced by 10_validate.R. Draws parameters from the priors, simulates a
# small panel from the error model that the refit uses (300 accounts, 36 months), refits it with brms and records the
# rank of each prior draw among the posterior draws. Uniform ranks indicate the generator and the fitting code agree.
suppressPackageStartupMessages(library(brms))
t_start <- Sys.time()
budget <- pv(ctx, "compute", "budget_minutes")$sbc
n_acc <- pv(ctx, "compute", "sbc_accounts"); n_mon <- pv(ctx, "compute", "sbc_months")
reps <- pv(ctx, "compute", "sbc_reps"); it <- pv(ctx, "compute", "sbc_iter")
pri <- pv(ctx, "usage", "priors")
n_ret <- 5; n_bg <- 20
draw <- function(p) switch(p$dist, halfnormal = abs(rnorm(1, 0, p$scale)), normal = rnorm(1, p$mean, p$sd), beta = rbeta(1, p$a, p$b))
sim_one <- function(k) {
  use_stream(ctx, "sbc", k)
  th <- list(sd_code = draw(pri$sigma_retailer), sd_bg = draw(pri$sigma_bg), sd_hh = draw(pri$sigma_hh),
             sd_shock = draw(pri$sigma_shock), sigma = draw(pri$ar_sigma), vol = draw(pri$voluntary_effect), man = draw(pri$mandatory_effect))
  acc <- data.table(acc = seq_len(n_acc), bg = sample.int(n_bg, n_acc, replace = TRUE))
  acc[, code := (bg - 1L) %% n_ret + 1L]
  a_r <- rnorm(n_ret, 0, th$sd_code); a_b <- rnorm(n_bg, 0, th$sd_bg); a_h <- rnorm(n_acc, 0, th$sd_hh)
  sh <- matrix(rnorm(n_ret * n_mon, 0, th$sd_shock), n_ret)
  d <- acc[rep(seq_len(n_acc), each = n_mon)][, t := rep(seq_len(n_mon), n_acc)]
  d[, `:=`(vol = as.integer(t > 18), man = as.integer(t > 28))]
  d[, y := a_r[code] + a_b[bg] + a_h[acc] + sh[cbind(code, t)] + th$vol * vol + th$man * man + rnorm(.N, 0, th$sigma)]
  d[, `:=`(code = factor(code), bg = factor(bg), acc = factor(acc), rm = paste(code, t))]
  list(data = d, theta = th)
}
priors <- c(prior_string(sprintf("normal(%s, %s)", pri$voluntary_effect$mean, pri$voluntary_effect$sd), coef = "vol"),
            prior_string(sprintf("normal(%s, %s)", pri$mandatory_effect$mean, pri$mandatory_effect$sd), coef = "man"),
            prior(constant(0), class = "Intercept"),
            prior_string(sprintf("normal(0, %s)", pri$sigma_retailer$scale), class = "sd", group = "code"),
            prior_string(sprintf("normal(0, %s)", pri$sigma_bg$scale), class = "sd", group = "bg"),
            prior_string(sprintf("normal(0, %s)", pri$sigma_hh$scale), class = "sd", group = "acc"),
            prior_string(sprintf("normal(0, %s)", pri$sigma_shock$scale), class = "sd", group = "rm"),
            prior_string(sprintf("normal(0, %s)", pri$ar_sigma$scale), class = "sigma"))
form <- bf(y ~ 1 + vol + man + (1 | code) + (1 | bg) + (1 | acc) + (1 | rm))
# The SBC model omits the AR(1) term to keep the 100 refits within budget; the AR term is checked by --refit.
s1 <- sim_one(1)
fit0 <- brm(form, data = s1$data, prior = priors, backend = "rstan", chains = 1, iter = sum(it), warmup = it[1], refresh = 0,
            seed = stream_int(ctx$seed, "sbc_fit", 1))
cols <- c(sd_code = "sd_code__Intercept", sd_bg = "sd_bg__Intercept", sd_hh = "sd_acc__Intercept", sd_shock = "sd_rm__Intercept",
          sigma = "sigma", vol = "b_vol", man = "b_man")
one_rank <- function(k) {
  s <- if (k == 1) s1 else sim_one(k)
  f <- update(fit0, newdata = s$data, recompile = FALSE, refresh = 0, seed = stream_int(ctx$seed, "sbc_fit", k))
  dr <- as_draws_df(f)
  th_idx <- seq(1, nrow(dr), length.out = min(100, nrow(dr)))
  data.table(rep = k, parameter = names(cols), rank = vapply(names(cols), function(n) sum(dr[[cols[[n]]]][th_idx] < s$theta[[n]]), 0),
             n_draws = length(th_idx))
}
ranks <- list(); done <- 0
for (k in seq_len(reps)) {
  ranks[[k]] <- one_rank(k); done <- k
  if (as.numeric(difftime(Sys.time(), t_start, units = "mins")) > budget) break
}
rk <- rbindlist(ranks)
write_table(rk, file.path(P_$reports, "sbc_ranks.csv"), c("parameter", "rep"))
chi <- rk[, { h <- tabulate(pmin(rank %/% 10, 9) + 1, 10); e <- .N / 10; .(chisq = sum((h - e)^2 / e), pval = pchisq(sum((h - e)^2 / e), 9, lower.tail = FALSE)) },
          by = parameter]
write_table(chi, file.path(P_$reports, "sbc_uniformity.csv"))
p <- ggplot(rk, aes(rank)) + geom_histogram(bins = 10, boundary = 0) + facet_wrap(~parameter) +
  labs(x = "Rank of the prior draw among posterior draws", y = "Replications", title = "Simulation-based calibration rank histograms") + theme_minimal()
save_fig(ctx, p, "sbc_ranks")
ctx <- gate(ctx, sprintf("SBC: replications completed (reduced from %d if over the %d-minute budget)", reps, budget), TRUE, done, "report")
ctx <- gate(ctx, "SBC: no parameter with clearly non-uniform ranks (chi-square p > 0.01)", all(chi$pval > 0.01), min(chi$pval), "all p > 0.01",
            blocking = FALSE)
