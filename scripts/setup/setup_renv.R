# One-time setup of the R environment for this project. Run from the repo root:
#   source("scripts/setup/setup_renv.R")
# It creates renv/ and renv.lock with the packages the pipeline uses. Commit renv.lock.

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::init(bare = TRUE)
renv::install(c(
  # pipeline
  "targets", "tarchetypes",
  # data
  "dplyr", "tidyr", "readr", "readxl", "haven", "janitor", "stringr", "purrr", "tibble", "cli", "here",
  # later phases
  "survey", "srvyr", "sampling", "ggplot2", "gt", "arrow",
  # tests and reports
  "testthat", "quarto", "visNetwork"
))
renv::snapshot(type = "all", prompt = FALSE)
