# Shared helpers for the synthetic SCUBA generator.
# THE DATA PRODUCED BY THESE SCRIPTS IS SIMULATED and must not be mistaken for real utility records.

suppressPackageStartupMessages({
  library(data.table)
  library(yaml)
  library(digest)
  library(jsonlite)
  library(nanoparquet)
})

# Generation must not depend on thread scheduling or multithreaded floating-point reduction order.
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
setDTthreads(1L)
options(stringsAsFactors = FALSE, scipen = 999, warn = 1)

# ---------------------------------------------------------------------------------------------
# Paths and arguments
# ---------------------------------------------------------------------------------------------

script_dir <- function() {
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) return(normalizePath(dirname(sub("^--file=", "", a[1]))))
  normalizePath("R")
}

parse_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  opt <- list(profile = "default", cores = 1L, out = NULL, seed = NULL, params = NULL, refit = FALSE, sbc = FALSE, causal = FALSE,
              scenarios = FALSE, scenario = "base", force = FALSE)
  i <- 1L
  while (i <= length(args)) {
    k <- args[i]
    if (k %in% c("--profile", "--cores", "--out", "--seed", "--scenario", "--params")) {
      v <- args[i + 1L]
      opt[[sub("^--", "", k)]] <- if (k %in% c("--cores", "--seed")) as.integer(v) else v
      i <- i + 2L
    } else if (k %in% c("--refit", "--sbc", "--causal", "--scenarios", "--force")) {
      opt[[sub("^--", "", k)]] <- TRUE
      i <- i + 1L
    } else stop("Unknown argument: ", k)
  }
  opt
}

init_ctx <- function(stage, args = commandArgs(trailingOnly = TRUE)) {
  opt <- parse_args(args)
  root <- normalizePath(file.path(script_dir(), ".."))
  out <- if (!is.null(opt$out)) opt$out else if (opt$profile == "default") root else file.path(root, "build", opt$profile)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  out <- normalizePath(out)
  paths <- list(root = root, out = out,
                data = file.path(out, "data"), truth = file.path(out, "truth"), work = file.path(out, "work"),
                reports = file.path(out, "reports"), gates = file.path(out, "reports", "gates"),
                figures = file.path(out, "reports", "figures"))
  for (p in paths[-(1:2)]) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  params <- yaml::read_yaml(if (is.null(opt$params)) file.path(root, "config", "params.yaml") else opt$params)
  check_param_tags(params)
  profile <- yaml::read_yaml(file.path(root, "config", "profiles", paste0(opt$profile, ".yaml")))
  params <- apply_scenario(params, opt$scenario)
  ctx <- list(stage = stage, opt = opt, paths = paths, params = params, profile = profile,
              seed = if (is.null(opt$seed)) params$seed$root_seed$value else opt$seed, t0 = Sys.time())
  ctx$gates <- data.table(stage = character(), check = character(), value = character(),
                          threshold = character(), pass = logical(), blocking = logical())
  ctx$time <- time_axis(ctx)
  message(sprintf("[%s] profile=%s out=%s", stage, opt$profile, out))
  ctx
}

# Parameter nodes carry `value` and `source`; assumed values must have a rationale.
check_param_tags <- function(p, path = "") {
  if (is.list(p) && !is.null(p$source) && "value" %in% names(p)) {
    if (!p$source %in% c("catalogue", "paper", "assumed")) stop("Bad source tag at ", path)
    if (p$source == "assumed" && is.null(p$rationale)) stop("Assumed parameter without rationale at ", path)
    return(invisible(TRUE))
  }
  if (is.list(p) && length(p)) {
    for (n in names(p)) check_param_tags(p[[n]], paste(path, n, sep = "/"))
    return(invisible(TRUE))
  }
  stop("Parameter without value/source node at ", path)
}

# A scenario overrides parameter values ("section/name" = value, or a nested list merged into the value).
apply_scenario <- function(params, scenario) {
  if (is.null(scenario) || scenario == "base") return(params)
  sc <- params$scenarios$value[[scenario]]
  if (is.null(sc)) stop("Unknown scenario: ", scenario)
  for (k in names(sc$overrides)) {
    path <- strsplit(k, "/", fixed = TRUE)[[1]]
    node <- params[[path]]$value
    v <- sc$overrides[[k]]
    params[[path]]$value <- if (is.list(node) && is.list(v)) modifyList(node, v) else v
  }
  params
}

# pv(ctx, "usage", "ar_phi") returns the value of a tagged parameter.
pv <- function(ctx, ...) {
  node <- ctx$params
  for (k in c(...)) {
    node <- node[[k]]
    if (is.null(node)) stop("Missing parameter: ", paste(c(...), collapse = "/"))
  }
  node$value
}

# ---------------------------------------------------------------------------------------------
# Time axis
# ---------------------------------------------------------------------------------------------

time_axis <- function(ctx) {
  s <- as.Date(pv(ctx, "panel", "start_month"))
  e <- as.Date(pv(ctx, "panel", "end_month"))
  m <- seq(s, e, by = "month")
  nxt <- seq(s, by = "month", length.out = length(m) + 1L)[-1L]
  data.table(t = seq_along(m), month_start = m, month_end = nxt - 1L,
             year = as.integer(format(m, "%Y")), month = as.integer(format(m, "%m")),
             days = as.integer(nxt - m),
             water_year = as.integer(format(m, "%Y")) + as.integer(format(m, "%m") >= "10"))
}

month_index <- function(ctx, date) {
  d <- as.Date(date)
  y <- as.integer(format(d, "%Y")); mo <- as.integer(format(d, "%m"))
  (y - ctx$time$year[1]) * 12L + (mo - ctx$time$month[1]) + 1L
}

# ---------------------------------------------------------------------------------------------
# Random number streams keyed to stage and unit ID (section 6 of the instructions)
# ---------------------------------------------------------------------------------------------

stream_int <- function(seed, stage, unit) digest::digest2int(paste(seed, stage, unit, sep = "|"), seed = 0L)

use_stream <- function(ctx, stage, unit) {
  set.seed(stream_int(ctx$seed, stage, unit), kind = "L'Ecuyer-CMRG", normal.kind = "Inversion", sample.kind = "Rejection")
  invisible(NULL)
}

# Every stage logs the stage-unit keys it used, so 09_validate can check that no two keys collide.
register_keys <- function(ctx, stage, units) {
  f <- file.path(ctx$paths$work, "rng_keys.csv")
  dt <- data.table(stage = stage, unit = as.character(units))
  dt[, int := stream_int(ctx$seed, stage, unit)]
  old <- if (file.exists(f)) fread(f, colClasses = c(int = "integer")) else NULL
  if (!is.null(old)) old <- old[stage != dt$stage[1]]
  all <- rbind(old, dt)
  setorder(all, stage, unit)
  fwrite(all, f)
  invisible(all)
}

# Truncated draws by rejection keep each unit's stream self-contained.
rlnorm_bounded <- function(n, meanlog, sdlog, lo, hi) {
  x <- rlnorm(n, meanlog, sdlog)
  for (k in 1:200) {
    bad <- which(x < lo | x > hi)
    if (!length(bad)) break
    x[bad] <- rlnorm(length(bad), if (length(meanlog) > 1) meanlog[bad] else meanlog, sdlog)
  }
  pmin(pmax(x, lo), hi)
}

# ---------------------------------------------------------------------------------------------
# Deterministic I/O
# ---------------------------------------------------------------------------------------------

write_table <- function(dt, file, keys = NULL) {
  dt <- as.data.table(dt)
  if (!is.null(keys)) setorderv(dt, keys)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  if (grepl("\\.parquet$", file)) {
    nanoparquet::write_parquet(as.data.frame(dt), file, compression = "snappy")
  } else {
    fwrite(dt, file, dateTimeAs = "ISO", na = "")
  }
  invisible(file)
}

read_table <- function(file) {
  if (grepl("\\.parquet$", file)) as.data.table(nanoparquet::read_parquet(file)) else fread(file, na.strings = "")
}

wpath <- function(ctx, name) file.path(ctx$paths$work, name)

# ---------------------------------------------------------------------------------------------
# Gates, memory and stage wrap-up
# ---------------------------------------------------------------------------------------------

# Non-blocking gates (blocking = FALSE) are reported as pass/fail but do not stop the pipeline; they are used for the
# estimated-side anchors, whose misses are documented rather than tuned away after calibration.
gate <- function(ctx, check, pass, value, threshold, absolute = FALSE, blocking = TRUE) {
  if (absolute && !isTRUE(ctx$profile$check_absolute_counts)) {
    pass <- NA
    threshold <- paste(threshold, "(skipped for this profile)")
  }
  fmt <- function(x) if (is.numeric(x)) paste(format(signif(x, 4), trim = TRUE), collapse = ", ") else paste(x, collapse = ", ")
  ctx$gates <- rbind(ctx$gates, data.table(stage = ctx$stage, check = check, value = fmt(value),
                                           threshold = threshold, pass = as.logical(pass), blocking = blocking))
  message(sprintf("  gate %-4s %s: %s (%s)", if (is.na(pass)) "SKIP" else if (isTRUE(pass)) "PASS" else if (blocking) "FAIL" else "MISS",
                  check, fmt(value), threshold))
  ctx
}

# Share gates use a binomial rule: pass when the target band overlaps the 95 percent Clopper-Pearson interval
# around the observed share, with n the number of households.
share_gate <- function(ctx, check, k, n, band, absolute = FALSE, blocking = TRUE) {
  ci <- if (n > 0) binom.test(k, n)$conf.int else c(0, 1)
  pass <- ci[2] >= band[1] && ci[1] <= band[2]
  gate(ctx, check, pass, c(k / n, ci[1], ci[2]),
       sprintf("band [%s, %s] overlaps 95%% binomial interval (n = %d)", band[1], band[2], n), absolute, blocking)
}

in_band <- function(x, band) x >= band[1] & x <= band[2]

peak_mem_mb <- function() {
  x <- readLines("/proc/self/status")
  as.numeric(gsub("\\D", "", grep("^VmHWM", x, value = TRUE))) / 1024
}

finish_stage <- function(ctx) {
  fwrite(ctx$gates, file.path(ctx$paths$gates, paste0(ctx$stage, ".csv")))
  mf <- file.path(ctx$paths$work, "stage_runs.csv")
  run <- data.table(stage = ctx$stage, profile = ctx$opt$profile, cores = ctx$opt$cores,
                    seconds = round(as.numeric(difftime(Sys.time(), ctx$t0, units = "secs")), 1),
                    peak_mem_mb = round(peak_mem_mb(), 1))
  old <- if (file.exists(mf)) fread(mf) else NULL
  if (!is.null(old)) old <- old[stage != ctx$stage]
  fwrite(rbind(old, run), mf)
  nfail <- sum(ctx$gates$pass %in% FALSE & ctx$gates$blocking %in% TRUE)
  message(sprintf("[%s] done in %.1fs, peak memory %.0f MB, %d gate failure(s)", ctx$stage, run$seconds, run$peak_mem_mb, nfail))
  if (nfail > 0 && !isTRUE(ctx$opt$force)) quit(status = 2)
  invisible(ctx)
}

save_fig <- function(ctx, plot, name, width = 8, height = 5) {
  f <- file.path(ctx$paths$figures, paste0(name, ".png"))
  ggplot2::ggsave(f, plot, width = width, height = height, dpi = 110, bg = "white")
  invisible(f)
}

# ---------------------------------------------------------------------------------------------
# Retailers, profiles and shared lookups
# ---------------------------------------------------------------------------------------------

retailers_table <- function(ctx) {
  r <- pv(ctx, "retailers")
  dt <- rbindlist(lapply(seq_along(r), function(i) {
    x <- r[[i]]
    data.table(retailer_index = i, code = x$code, name = x$name, setting = x$setting, billing = x$billing,
               rate_type = x$rate_type, n_accounts_base = x$n_accounts, med_income = x$med_income,
               med_irr_area = x$med_irr_area, med_year_built = x$med_year_built, base_hhsize = x$base_hhsize,
               pop_density = x$pop_density, service_population = x$service_population,
               precip_annual = x$precip_annual, coastal = isTRUE(x$coastal),
               new_construction_rate = x$new_construction_rate, hydrologic_region = x$hydrologic_region)
  }))
  pr <- ctx$profile
  dt[, n_accounts := if (!is.null(pr$accounts_per_retailer)) as.integer(pr$accounts_per_retailer)
                     else as.integer(round(n_accounts_base * pr$accounts_scale))]
  dt[, n_zips := pmax(1L, as.integer(round(n_accounts / pr$accounts_per_zip)))]
  dt[, bgs_per_zip := pmax(1L, as.integer(round(n_accounts / n_zips / pr$accounts_per_bg)))]
  dt[, n_bgs := n_zips * bgs_per_zip]
  dt[]
}

et0_baseline <- function(ctx) {
  r <- pv(ctx, "retailers")
  rbindlist(lapply(r, function(x) data.table(code = x$code, month = 1:12, et0_base = unlist(x$et0))))
}

class_labels <- c(SF = "single family residential", MF = "multi-family residential", COM = "commercial",
                  IND = "industrial", IRR = "irrigation", INST = "institutional", OTH = "other")

# ---------------------------------------------------------------------------------------------
# Billing periods (shared by 06_treatment for calibration and 07_billing for bills)
# ---------------------------------------------------------------------------------------------

# Monthly retailers bill calendar months. Bimonthly retailers put each location on cycle A (periods
# start in odd months) or cycle B (even months) and jitter each read date by up to `jitter` days.
# Periods are clipped to the location's active months and split at tenure changes (final and opening reads).
billing_periods <- function(ctx, loc) {
  # loc: data.table with location_id, billing, first_t, last_t, and a list column tenure_starts (t values)
  tm <- ctx$time
  jitter <- pv(ctx, "billing", "read_jitter_days")
  panel_start <- tm$month_start[1]; panel_end <- tm$month_end[nrow(tm)]
  out <- vector("list", nrow(loc))
  for (i in seq_len(nrow(loc))) {
    L <- loc[i]
    if (L$billing == "monthly") {
      b <- tm$month_start
      cyc <- "M"
    } else {
      use_stream(ctx, "billing_cycle", L$location_id)
      cyc <- if (runif(1) < 0.5) "A" else "B"
      starts <- if (cyc == "A") seq(1L, nrow(tm), by = 2L) else seq(2L, nrow(tm), by = 2L)
      b <- tm$month_start[starts] + sample(-jitter:jitter, length(starts), replace = TRUE)
      b[1] <- tm$month_start[starts[1]]
    }
    b <- c(b, panel_end + 1L)
    first_day <- tm$month_start[L$first_t]; last_day <- tm$month_end[L$last_t]
    ts <- unlist(L$tenure_starts)
    extra <- tm$month_start[ts[ts > L$first_t]]
    b <- sort(unique(c(b[b > first_day & b <= last_day], first_day, extra, last_day + 1L)))
    p <- data.table(location_id = L$location_id, billing_cycle = cyc,
                    usage_start_date = b[-length(b)], usage_end_date = b[-1] - 1L)
    # A bimonthly cycle B location starts its first full period in February; January 2010 is a short first bill.
    out[[i]] <- p
  }
  pr <- rbindlist(out)
  pr[, days := as.integer(usage_end_date - usage_start_date) + 1L]
  pr[, period_index := seq_len(.N), by = location_id]
  pr[]
}

# Overlap of billing periods with calendar months, for prorating monthly truth by days.
period_month_overlap <- function(ctx, periods) {
  tm <- ctx$time
  ps <- periods[, .(location_id, period_index, usage_start_date, usage_end_date)]
  t_first <- month_index(ctx, ps$usage_start_date)
  t_last <- month_index(ctx, ps$usage_end_date)
  n <- t_last - t_first + 1L
  ov <- ps[rep(seq_len(nrow(ps)), n)]
  ov[, t := unlist(mapply(seq, t_first, t_last, SIMPLIFY = FALSE))]
  ov[, `:=`(ms = tm$month_start[t], me = tm$month_end[t], dim = tm$days[t])]
  ov[, overlap := as.integer(pmin(usage_end_date, me) - pmax(usage_start_date, ms)) + 1L]
  ov[, w := overlap / dim]
  ov[overlap > 0, .(location_id, period_index, t, overlap, w)]
}

# ---------------------------------------------------------------------------------------------
# Observation-layer plan (shared by 06_treatment for calibration and 08_observation)
# ---------------------------------------------------------------------------------------------

# All observation-layer draws are keyed to the location or tenure, so the plan for a location does not
# depend on which locations end up treated. 06_treatment uses the same plan to calibrate the selection
# intercept against the post-filter cohort count.
observation_plan <- function(ctx, locations, tenures) {
  tm <- ctx$time
  ep_p <- pv(ctx, "observation", "missing_row_episode_prob")
  ep_len <- pv(ctx, "observation", "missing_row_episode_months")
  def <- pv(ctx, "observation", "default_irr_area")
  miss_area <- pv(ctx, "observation", "missing_irr_area_share")
  miss_hh <- pv(ctx, "observation", "missing_hhsize_share")
  dup <- pv(ctx, "observation", "duplicate_turf_share")
  outage <- pv(ctx, "observation", "feed_outage")
  out_t <- month_index(ctx, as.Date(unlist(outage$months)))

  loc_plan <- locations[, .(location_id, code, cust_class, first_t, last_t)]
  loc_plan[, `:=`(gap_start = NA_integer_, gap_end = NA_integer_, default_area = FALSE, missing_area = FALSE,
                  duplicate_turf = FALSE, dup_u = NA_real_)]
  for (i in seq_len(nrow(loc_plan))) {
    L <- loc_plan[i]
    use_stream(ctx, "obs_location", L$location_id)
    u <- runif(6)
    if (u[1] < ep_p) {
      len <- ep_len[1] + floor(u[2] * (ep_len[2] - ep_len[1] + 1))
      st <- L$first_t + floor(u[3] * max(1, L$last_t - L$first_t + 1))
      set(loc_plan, i, "gap_start", as.integer(st))
      set(loc_plan, i, "gap_end", as.integer(min(L$last_t, st + len - 1)))
    }
    d <- def[[L$code]]
    if (!is.null(d) && L$cust_class == "SF" && u[4] < d$share) set(loc_plan, i, "default_area", TRUE)
    ma <- miss_area[[L$code]]
    if (!is.null(ma) && L$cust_class == "SF" && u[5] < ma) set(loc_plan, i, "missing_area", TRUE)
    set(loc_plan, i, "duplicate_turf", u[6] < dup)
    set(loc_plan, i, "dup_u", u[6])
  }
  loc_plan[, default_area_value := vapply(code, function(cc) if (is.null(def[[cc]])) NA_real_ else as.numeric(def[[cc]]$area), 0)]
  loc_plan[default_area == FALSE, default_area_value := NA_real_]
  loc_plan[, outage_start := ifelse(code == outage$retailer, min(out_t), NA_integer_)]
  loc_plan[, outage_end := ifelse(code == outage$retailer, max(out_t), NA_integer_)]

  ten_plan <- tenures[, .(cust_loc_id, location_id, code)]
  ten_plan[, missing_hhsize := FALSE]
  for (i in seq_len(nrow(ten_plan))) {
    mh <- miss_hh[[ten_plan$code[i]]]
    if (is.null(mh)) next
    use_stream(ctx, "obs_tenure", ten_plan$cust_loc_id[i])
    set(ten_plan, i, "missing_hhsize", runif(1) < mh)
  }
  list(locations = loc_plan[], tenures = ten_plan[])
}

# A billing period is missing from the extract when its end month falls in the location's feed gap or in a
# retailer-wide outage.
period_missing <- function(ctx, periods, loc_plan) {
  p <- merge(periods[, .(location_id, period_index, usage_end_date)],
             loc_plan[, .(location_id, gap_start, gap_end, outage_start, outage_end)], by = "location_id")
  p[, te := month_index(ctx, usage_end_date)]
  p[, missing := (!is.na(gap_start) & te >= gap_start & te <= gap_end) |
                 (!is.na(outage_start) & te >= outage_start & te <= outage_end)]
  p[, .(location_id, period_index, missing)]
}

# Months of observed data before and after a rebate, counted from billing periods: pre periods end before the
# received date, post periods start on or after the post-inspection date (periods overlapping the installation
# window are dropped), and months = days / 30.4375.
count_pre_post_months <- function(periods_obs, rebates) {
  x <- merge(periods_obs[, .(location_id, usage_start_date, usage_end_date, days)],
             rebates[, .(location_id, received_date, post_date)], by = "location_id", allow.cartesian = TRUE)
  x[, .(pre_months = round(sum(days[usage_end_date < received_date]) / 30.4375),
        post_months = round(sum(days[usage_start_date >= post_date]) / 30.4375)), by = location_id]
}

# ---------------------------------------------------------------------------------------------
# Property attributes (shared by 02_accounts and the new construction in 03_occupancy)
# ---------------------------------------------------------------------------------------------

# Draws the physical attributes of one location from its own stream. `L` holds location_id, code,
# cust_class and block_group_id; `B` is the block group row and `R` the retailer row.
draw_property <- function(ctx, L, B, R, new_construction_year = NA_integer_) {
  use_stream(ctx, "accounts", L$location_id)
  yb_b <- pv(ctx, "accounts", "year_built_bounds")
  u <- runif(8); z <- rnorm(4)
  yb <- if (is.na(new_construction_year)) {
    min(max(round(B$median_year_structure_built + pv(ctx, "accounts", "year_built_sd") * z[1]), yb_b[1]), yb_b[2])
  } else new_construction_year
  cls <- L$cust_class
  irr_area <- NA_real_; units <- NA_integer_; base_ccf <- NA_real_
  if (cls == "SF") {
    ab <- pv(ctx, "accounts", "irr_area_bounds")
    yb_for_area <- if (is.na(new_construction_year)) yb else B$median_year_structure_built
    m <- log(R$med_irr_area) + pv(ctx, "accounts", "irr_area_income_elasticity") * log(B$median_hh_income / R$med_income) +
      pv(ctx, "accounts", "irr_area_year_built_slope") * (yb_for_area - R$med_year_built)
    a <- exp(m + pv(ctx, "accounts", "irr_area_sdlog") * z[2])
    if (!is.na(new_construction_year)) a <- a * pv(ctx, "occupancy", "new_construction_area_factor")
    irr_area <- round(min(max(a, ab[1]), ab[2]))
  } else if (cls == "MF") {
    ub <- pv(ctx, "accounts", "mf_units")
    units <- as.integer(round(exp(log(ub[1]) + u[1] * (log(ub[2]) - log(ub[1])))))
    irr_area <- round(pv(ctx, "accounts", "mf_landscape_per_sqrt_unit") * sqrt(units))
  } else if (cls == "IRR") {
    ia <- pv(ctx, "accounts", "irrigation_area")
    irr_area <- round(min(max(exp(log(ia$median) + ia$sdlog * z[2]), ia$bounds[1]), ia$bounds[2]))
  } else {
    cb <- pv(ctx, "accounts", "cii_base_ccf")[[cls]]
    base_ccf <- exp(log(cb[[1]]) + cb[[2]] * z[2])
  }
  meter <- switch(cls,
    SF = if (!is.na(irr_area) && irr_area > 12000) '1' else if (u[2] < 0.3) '5/8' else '3/4',
    MF = if (units <= 4) '1' else if (units <= 16) '1|1/2' else '2',
    IRR = if (irr_area < 20000) '1|1/2' else if (irr_area < 60000) '2' else '3',
    if (base_ccf < 30) '1' else if (base_ccf < 100) '1|1/2' else if (base_ccf < 300) '2' else '3')
  water_type <- if (cls == "IRR" && u[3] < pv(ctx, "classes", "irrigation_recycled_share")) "recycled" else "potable"
  list(year_built = as.integer(yb), irr_area_true = irr_area, unit_count = units, base_ccf = base_ccf,
       meter_size = meter, pressure_zone = sprintf("PZ%d", 1L + floor(u[4] * 3)),
       street_num = as.integer(100 + floor(u[5] * 9900)),
       apn = sprintf("SYN-%s-%06d", L$code, as.integer(floor(u[6] * 1e6))),
       lat = B$lat_center + 0.012 * (u[7] - 0.5), lon = B$lon_center + 0.012 * (u[8] - 0.5),
       water_type = water_type, raw_u = u[3])
}

# ---------------------------------------------------------------------------------------------
# Analysis cohort from the observed tables only (shared by 08_observation and 09_estimate_savings)
# ---------------------------------------------------------------------------------------------

# Bunching threshold for defaulted areas: max(min, share * single family locations in the retailer).
bunch_threshold <- function(n_sf, rule) pmax(rule$min, ceiling(rule$share * n_sf))

# Location-level view of the observed customer table, with defaulted irrigable areas detected by bunching:
# an area value shared by at least the bunching threshold of single family locations in a retailer is a default.
observed_locations <- function(customers, rule) {
  loc <- customers[, .(utility_name = utility_name[1], cust_loc_class = cust_loc_class[1], cust_loc_zip = cust_loc_zip[1],
                       irr_area = cust_loc_irr_area_sf[!is.na(cust_loc_irr_area_sf)][1]),
                   by = .(location_id = location_id_from_utility)]
  nsf <- loc[cust_loc_class == "single family residential", .(n_sf = .N), by = utility_name]
  bunch <- loc[cust_loc_class == "single family residential" & !is.na(irr_area), .N, by = .(utility_name, irr_area)]
  bunch <- merge(bunch, nsf, by = "utility_name")[N >= bunch_threshold(n_sf, rule)]
  loc[, defaulted_area := FALSE]
  loc[bunch, on = .(utility_name, irr_area), defaulted_area := TRUE]
  loc[]
}

# Billing frequency and cycle inferred from the observed read dates: a bimonthly location whose periods start in odd
# months is on cycle A, even months cycle B.
observed_cycles <- function(usage) {
  u <- usage[, .(days = as.numeric(median(as.integer(usage_end_date - usage_start_date) + 1L)),
                 odd = mean(as.integer(format(usage_start_date + 15L, "%m")) %% 2 == 1)), by = .(location_id = location_id_from_utility)]
  u[, billing := fifelse(days > 45, "bimonthly", "monthly")]
  u[, billing_cycle := fifelse(billing == "monthly", "M", fifelse(odd >= 0.5, "A", "B"))]
  u[, .(location_id, billing, billing_cycle)]
}

# Turf (or other) rebate events with the analysis filters: at least `pre` months before the received date, at least
# `post` months starting on or after the post-inspection date, a single rebate record of that type, a single family
# location and a non-defaulted irrigable area.
analysis_cohort <- function(usage, customers, rebates, type = "turf", pre = 24, post = 12, rule = list(min = 5, share = 0.02)) {
  loc <- observed_locations(customers, rule)
  rb <- rebates[rebate_instance_type == type, .(n_records = .N, received_date = min(rebate_instance_received_date),
                                                post_date = min(rebate_instance_post_date),
                                                quantity = rebate_instance_quantity[1]),
                by = .(location_id = location_id_from_utility)]
  per <- usage[, .(location_id = location_id_from_utility, usage_start_date, usage_end_date,
                   days = as.integer(usage_end_date - usage_start_date) + 1L)]
  pm <- count_pre_post_months(per, rb[, .(location_id, received_date, post_date)])
  rb <- merge(rb, pm, by = "location_id", all.x = TRUE)
  rb[is.na(pre_months), `:=`(pre_months = 0, post_months = 0)]
  rb <- merge(rb, loc, by = "location_id", all.x = TRUE)
  rb[, `:=`(duplicate = n_records > 1, single_family = cust_loc_class %in% "single family residential")]
  rb[, passes_filters := single_family & !duplicate & !(defaulted_area %in% TRUE) & pre_months >= pre & post_months >= post]
  rb[]
}
