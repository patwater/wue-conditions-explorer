# Bounded anchor calibration (indirect inference). Chooses the planted savings level and spread so that the matched
# DiD estimates reproduce the paper's estimated moments. At most `max_runs` runs on the large profile over three
# knobs: savings_target_annual_mean (mean phase first), structural_skew scale and extreme_savers share (spread phase).
# Every run is written to reports/calibration_log.csv; the lowest-loss run is frozen into config/params.yaml.
#   Rscript R/calibrate.R --cores 4
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), "00_common.R"))
ctx <- init_ctx("calibrate", c(commandArgs(trailingOnly = TRUE)))
cal <- pv(ctx, "calibration")
tg <- cal$targets
scales <- c(annual_mean = 2.5, median = 3, p25 = 4, p75 = 4, share_above_60 = 0.02, share_at_or_below_zero = 0.04, seasonal_ratio = 0.3)
root <- ctx$paths$root
cdir <- file.path(root, "build", "calibration"); dir.create(cdir, recursive = TRUE, showWarnings = FALSE)
large_out <- file.path(root, "build", "large")
log_file <- file.path(ctx$paths$reports, "calibration_log.csv")
base <- yaml::read_yaml(file.path(root, "config", "params.yaml"))
runs <- list()

loss_of <- function(m) sum(((unlist(m[names(scales)]) - unlist(tg[names(scales)])) / scales)^2)

# The mean knob is clamped inside the planted-side band so stage 4 gates stay valid; the estimated mean is a noisy
# statistic (its standard error on the large profile is several gal/sqft/yr), so steps are also limited.
gp <- pv(ctx, "gates", "planted")$annual_mean
clamp <- function(x) min(max(x, gp[1] + 0.5), gp[2] - 0.5)
run_one <- function(mean_knob, scale_knob, ext_knob, phase) {
  mean_knob <- clamp(mean_knob)
  k <- length(runs) + 1L
  if (k > cal$max_runs) return(NULL)
  p <- base
  p$treatment$savings_target_annual_mean$value <- mean_knob
  p$treatment$structural_skew$value$scale <- scale_knob
  p$treatment$extreme_savers$value$share <- ext_knob
  pf <- file.path(cdir, sprintf("params_run%02d.yaml", k))
  writeLines(yaml::as.yaml(p), pf)
  ok <- TRUE
  for (st in c("06_treatment", "07_billing", "08_observation", "09_estimate_savings")) {
    s <- system2("Rscript", c(file.path(root, "R", paste0(st, ".R")), "--profile", "large", "--cores", ctx$opt$cores,
                              "--params", pf, "--force"), stdout = FALSE, stderr = FALSE)
    if (s != 0) { ok <- FALSE; break }
  }
  m <- if (ok) as.list(fread(file.path(large_out, "estimates", "estimated_moments.csv"))) else NULL
  pl <- if (ok) fread(file.path(large_out, "work", "planted_first12.csv")) else NULL
  row <- data.table(run = k, phase = phase, savings_target_annual_mean = mean_knob, structural_skew_scale = scale_knob,
                    extreme_saver_share = ext_knob, ok = ok)
  if (ok) {
    row <- cbind(row, as.data.table(m[c(names(scales), "jas_mean", "jfm_mean", "sd_estimate", "sd_placebo", "n_households")]))
    row[, `:=`(planted_mean = mean(pl$annual), planted_sd = sd(pl$annual), loss = loss_of(m))]
  }
  runs[[k]] <<- row
  fwrite(rbindlist(runs, fill = TRUE), log_file)
  message(sprintf("  run %d [%s] mean=%.2f scale=%.2f ext=%.3f -> est mean %.2f, loss %.2f", k, phase, mean_knob, scale_knob,
                  ext_knob, if (ok) m$annual_mean else NA, if (ok) row$loss else NA))
  row
}

m0 <- base$treatment$savings_target_annual_mean$value
s0 <- base$treatment$structural_skew$value$scale; if (is.null(s0)) s0 <- 1
e0 <- base$treatment$extreme_savers$value$share

# Mean phase: secant steps on the estimated mean, at most four runs.
r <- run_one(m0, s0, e0, "mean")
x_prev <- m0; f_prev <- r$annual_mean - tg$annual_mean
x <- m0 - f_prev
for (i in 1:3) {
  r <- run_one(x, s0, e0, "mean"); f <- r$annual_mean - tg$annual_mean
  if (abs(f) < 0.3) break
  slope <- if (abs(x - x_prev) > 1e-6) (f - f_prev) / (x - x_prev) else 1
  if (!is.finite(slope) || abs(slope) < 0.2) slope <- 1
  x_prev <- x; f_prev <- f; x <- x - f / slope
}
best_mean <- rbindlist(runs, fill = TRUE)[phase == "mean"][which.min(abs(annual_mean - tg$annual_mean)), savings_target_annual_mean]

# Spread phase: one-at-a-time grid, then the best combination.
for (g in list(c(0.5, e0), c(1.5, e0), c(s0, 0.002), c(s0, 0.015))) run_one(best_mean, g[1], g[2], "spread")
R <- rbindlist(runs, fill = TRUE)[ok == TRUE]
bs <- R[phase %in% c("mean", "spread") & extreme_saver_share == e0][which.min(loss), structural_skew_scale]
be <- R[phase %in% c("mean", "spread") & structural_skew_scale == s0][which.min(loss), extreme_saver_share]
run_one(best_mean, bs, be, "combine")
run_one(best_mean, (bs + s0) / 2, be, "combine")

# Re-adjust the mean at the best spread found so far.
R <- rbindlist(runs, fill = TRUE)[ok == TRUE]
b <- R[which.min(loss)]
if (length(runs) < cal$max_runs) run_one(b$savings_target_annual_mean - (b$annual_mean - tg$annual_mean), b$structural_skew_scale,
                                         b$extreme_saver_share, "mean_refine")

R <- rbindlist(runs, fill = TRUE)[ok == TRUE]
best <- R[which.min(loss)]
R[, chosen := run == best$run]
fwrite(R, log_file)
message(sprintf("  chosen run %d: mean=%.3f scale=%.3f ext=%.4f loss=%.2f", best$run, best$savings_target_annual_mean,
                best$structural_skew_scale, best$extreme_saver_share, best$loss))

# Freeze: write the chosen knobs into config/params.yaml in place, preserving comments.
pf <- file.path(root, "config", "params.yaml")
txt <- readLines(pf)
i <- grep("^  savings_target_annual_mean:", txt)
txt[i] <- sub("value: [0-9.]+", sprintf("value: %s", format(round(best$savings_target_annual_mean, 3))), txt[i])
i <- grep("^  structural_skew:", txt)
txt[i] <- sub("scale: [0-9.]+", sprintf("scale: %s", format(round(best$structural_skew_scale, 3))), txt[i])
i <- grep("^  extreme_savers:", txt)
txt[i] <- sub("share: [0-9.]+", sprintf("share: %s", format(round(best$extreme_saver_share, 4))), txt[i])
i <- grep("^  # PROVISIONAL", txt)
if (length(i)) txt[i:(i + 2)] <- c(sprintf("  # FROZEN on %s by the bounded anchor calibration (run %d of %d, see reports/calibration_log.csv).",
                                           Sys.Date(), best$run, nrow(R)),
                                   "  # savings_target_annual_mean, the structural_skew scale and the extreme_savers share were chosen so the",
                                   "  # matched DiD estimates reproduce the paper's estimated moments. No tuning is allowed after this step.")
writeLines(txt, pf)
ctx <- gate(ctx, "calibration runs within budget", nrow(R) <= cal$max_runs, nrow(R), sprintf("<= %d", cal$max_runs))
finish_stage(ctx)
