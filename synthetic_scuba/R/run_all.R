# One command for the whole pipeline: generation (01-08), matched DiD estimation (09) and validation (10).
#   Rscript R/run_all.R                         default profile, generation + matched DiD + descriptive validation
#   Rscript R/run_all.R --profile tiny --cores 4
#   Rscript R/run_all.R --scenarios --refit --sbc --causal    optional, expensive checks
# A stage whose blocking gate fails stops the run unless --force is given.
args <- commandArgs(trailingOnly = TRUE)
here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
gen_args <- args[!args %in% c("--scenarios", "--refit", "--sbc")]
stages <- c("01_geography", "02_accounts", "03_occupancy", "04_climate", "05_usage", "06_treatment", "07_billing",
            "08_observation", "09_estimate_savings", "10_validate")
for (st in stages) {
  a <- if (st == "10_validate") args else gen_args
  if (st != "09_estimate_savings") a <- a[a != "--causal"]
  status <- system2("Rscript", c(file.path(here, paste0(st, ".R")), a))
  if (status != 0) stop(sprintf("Stage %s stopped the run (exit status %d). See reports/gates/%s.csv.", st, status, st))
}
message("Pipeline complete. Open reports/validation.html.")
