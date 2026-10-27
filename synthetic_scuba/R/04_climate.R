# Stage 3a: monthly reference evapotranspiration (ET0) and precipitation by retailer.
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), "00_common.R"))
ctx <- init_ctx("04_climate")

ret <- read_table(wpath(ctx, "retailers.csv"))
tm <- ctx$time
et0b <- et0_baseline(ctx)
shares <- unlist(pv(ctx, "climate", "precip_monthly_shares")); shares <- shares / sum(shares)
drought_wy <- unlist(pv(ctx, "climate", "drought_water_years"))
wys <- sort(unique(tm$water_year))

# Water-year anomalies shared across retailers (own named stream), then local deviations per retailer.
use_stream(ctx, "climate_shared", "water_years")
shared <- data.table(water_year = wys, zp = rnorm(length(wys)), ze = rnorm(length(wys)))
register_keys(ctx, "climate_shared", "water_years")

clim <- rbindlist(lapply(seq_len(nrow(ret)), function(i) {
  R <- ret[i]
  use_stream(ctx, "climate_local", R$code)
  loc <- data.table(water_year = wys, lp = rnorm(length(wys)), le = rnorm(length(wys)))
  n <- nrow(tm)
  me <- rnorm(n)
  cv <- pv(ctx, "climate", "precip_month_cv")
  mp <- rgamma(n, shape = 1 / cv^2, rate = 1 / cv^2)
  er <- rnorm(n); sb <- rnorm(1); sr <- rnorm(n)
  d <- copy(tm)[, code := R$code]
  d <- merge(d, shared, by = "water_year"); d <- merge(d, loc, by = "water_year")
  setorder(d, t)
  d <- merge(d, et0b[code == R$code, .(month, et0_base)], by = "month"); setorder(d, t)
  dr <- d$water_year %in% drought_wy
  pmult <- exp(log(pv(ctx, "climate", "drought_precip_mult")) * dr +
               pv(ctx, "climate", "precip_shared_sdlog") * d$zp + pv(ctx, "climate", "precip_local_sdlog") * d$lp)
  emult <- exp(log(pv(ctx, "climate", "drought_et_mult")) * dr +
               pv(ctx, "climate", "et_shared_sdlog") * d$ze + pv(ctx, "climate", "et_local_sdlog") * d$le)
  d[, eto_in := round(et0_base * emult * exp(pv(ctx, "climate", "et_month_sdlog") * me), 3)]
  d[, precip_in := round(R$precip_annual * shares[month] * pmult * mp, 3)]
  # Reported ET: the retailer's own value with measurement noise, and a smoothed nearby-station value.
  d[, usage_et_amount := round(eto_in * exp(pv(ctx, "climate", "et_report_sdlog") * er), 2)]
  st <- d$eto_in * exp(pv(ctx, "climate", "et_station_bias_sdlog") * sb)
  sm <- st; for (k in 2:(n - 1)) sm[k] <- 0.25 * st[k - 1] + 0.5 * st[k] + 0.25 * st[k + 1]
  d[, usage_et_amount_default := round(sm, 2)]
  d[, .(code, t, year, month, water_year, days, et0_base, eto_in, precip_in, usage_et_amount, usage_et_amount_default)]
}))
register_keys(ctx, "climate_local", ret$code)

eff <- pv(ctx, "climate", "effective_rain_frac"); flo <- pv(ctx, "climate", "irrigation_floor_frac")
clim[, effective_rain_in := round(eff * precip_in, 3)]
clim[, net_et_in := pmax(eto_in - effective_rain_in, 0)]
clim[, applied_depth_in := pmax(net_et_in, flo * eto_in)]
clim[, drought_year := water_year %in% drought_wy]
write_table(clim, file.path(ctx$paths$truth, "climate_monthly.parquet"), c("code", "t"))

# Gates.
# Drought water years are compared with the baseline climate (instructions: drier and slightly higher in ET0
# than the baseline).
clim <- merge(clim, ret[, .(code, precip_annual)], by = "code")
clim[, precip_base := precip_annual * shares[month]]
cmp <- clim[drought_year == TRUE, .(et_ratio = sum(eto_in) / sum(et0_base), pr_ratio = sum(precip_in) / sum(precip_base)), by = code]
clim[, c("precip_annual", "precip_base") := NULL]
ctx <- gate(ctx, "drought water years drier than baseline in every retailer", all(cmp$pr_ratio < 1), cmp$pr_ratio, "ratio < 1")
ctx <- gate(ctx, "drought water years slightly higher in ET0 than baseline", all(cmp$et_ratio > 1 & cmp$et_ratio < 1.1),
            cmp$et_ratio, "1 < ratio < 1.1")
amp <- clim[, .(ratio = mean(eto_in[month == 7]) / mean(eto_in[month == 1])), by = code]
ctx <- gate(ctx, "inland and valley July/January ET0 ratio exceeds Surf City",
            all(amp[code %in% c("IE", "VS"), ratio] > amp[code == "SC", ratio]), amp$ratio, "IE and VS > SC")
wet <- clim[, .(share = sum(precip_in[month %in% c(11, 12, 1, 2, 3)]) / sum(precip_in)), by = code]
ctx <- gate(ctx, "precipitation concentrated November through March", all(wet$share > 0.7), wet$share, "share > 0.7")

library(ggplot2)
p <- ggplot(clim, aes(as.Date(paste(year, month, 1, sep = "-")), eto_in, colour = code)) + geom_line() +
  labs(x = NULL, y = "ET0 (inches per month)", colour = "Retailer", title = "Monthly ET0 by retailer") + theme_minimal()
save_fig(ctx, p, "climate_et0")
p <- ggplot(clim, aes(as.Date(paste(year, month, 1, sep = "-")), precip_in, fill = drought_year)) + geom_col() +
  facet_wrap(~code, ncol = 1) + labs(x = NULL, y = "Precipitation (inches per month)", fill = "Drought water year") + theme_minimal()
save_fig(ctx, p, "climate_precip", height = 7)
finish_stage(ctx)
