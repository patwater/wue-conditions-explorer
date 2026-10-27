# Seed robustness: reruns a range of stages with several root seeds and reports each gate's pass rate.
# A gate passes overall when it passes in at least `min_pass` of `n_seeds` seeds.
#   Rscript R/seed_check.R --profile default --cores 4 --set stage4
#   Rscript R/seed_check.R --profile default --cores 4 --set heldout
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), "00_common.R"))
suppressPackageStartupMessages(library(parallel))
args <- commandArgs(trailingOnly = TRUE)
set_name <- if ("--set" %in% args) args[which(args == "--set") + 1L] else "stage4"
args <- args[!(args %in% c("--set", set_name))]
ctx <- init_ctx(paste0("seed_check_", set_name), args)
sc <- pv(ctx, "gates", "seed_check")
start <- if (set_name == "stage4") sc$stage4_seed_start else sc$heldout_seed_start
seeds <- start + seq_len(sc$n_seeds) - 1L
stages <- if (set_name == "stage4") c("01_geography", "02_accounts", "03_occupancy", "04_climate", "05_usage", "06_treatment") else
          c("01_geography", "02_accounts", "03_occupancy", "04_climate", "05_usage", "06_treatment",
                 "07_billing", "08_observation", "09_estimate_savings")
rdir <- file.path(ctx$paths$root, "R")
run_seed <- function(seed) {
  out <- file.path(ctx$paths$root, "build", "seeds", ctx$opt$profile, seed)
  for (st in stages) {
    status <- system2("Rscript", c(file.path(rdir, paste0(st, ".R")), "--profile", ctx$opt$profile, "--seed", seed,
                                   "--out", out, "--force"), stdout = FALSE, stderr = FALSE)
    if (status != 0) return(data.table(seed = seed, stage = st, check = "stage ran", value = "error", threshold = "", pass = FALSE))
  }
  g <- rbindlist(lapply(list.files(file.path(out, "reports", "gates"), full.names = TRUE), fread), fill = TRUE)
  g[stage %in% stages][, seed := seed][]
}
res <- rbindlist(mclapply(seeds, run_seed, mc.cores = ctx$opt$cores), fill = TRUE)
res[, pass := as.logical(pass)]
summ <- res[, .(n_seeds = .N, n_pass = sum(pass %in% TRUE), n_skip = sum(is.na(pass))), by = .(stage, check)]
summ[, overall := n_pass + n_skip >= sc$min_pass]
write_table(res, file.path(ctx$paths$reports, paste0("seed_robustness_", set_name, "_detail.csv")), c("stage", "check", "seed"))
write_table(summ, file.path(ctx$paths$reports, paste0("seed_robustness_", set_name, ".csv")), c("stage", "check"))
for (i in seq_len(nrow(summ))) ctx <- gate(ctx, paste(summ$stage[i], summ$check[i], sep = ": "), summ$overall[i],
                                           summ$n_pass[i], sprintf(">= %d of %d seeds", sc$min_pass, sc$n_seeds))
finish_stage(ctx)
