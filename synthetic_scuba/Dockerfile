# Container recipe for the synthetic SCUBA generator. Optional: developers with a matching toolchain can run the
# scripts directly. NOT BUILT OR TESTED: the Docker daemon is not available in the Claude Code cloud environment.
FROM rocker/r-ver:4.3.3
ENV OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MAKEFLAGS=-j4
RUN apt-get update && apt-get install -y --no-install-recommends \
      libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
      libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libv8-dev g++ make \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /synthetic_scuba
COPY renv.lock renv.lock
RUN R -e 'install.packages("renv", repos = "https://cloud.r-project.org"); renv::restore(lockfile = "renv.lock", prompt = FALSE)'
COPY . .
CMD ["Rscript", "R/run_all.R"]
