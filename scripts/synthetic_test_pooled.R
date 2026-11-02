# WARNING: heavy and slow. Runs the full original-data pipeline
# (scripts/models_original.R + scripts/analysis_original.R, including 500
# bootstrap deviance fits) on every synthetic replicate, one fresh R process per
# replicate, then pools the results. Expect hours and high memory use.
# Run as its own process from the project root:
#   Rscript scripts/synthetic_test_parallel.R
# Each replicate writes only to outputs/synthetic/replicate_<i>/ (resumable via
# checkpoints) and never touches the main-pipeline outputs.
# Replicates may equally be run elsewhere by a third party (same env vars,
# see run_replicate()) and copied into outputs/synthetic/replicate_<i>/.
library(callr)

replicates <- 1:10
workers <- min(length(replicates), parallel::detectCores() - 1L)
log_dir <- file.path("outputs", "synthetic", "replicate_logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

run_replicate <- function(i) {
  # Outer parallelism is across replicates, so each uses one inner worker.
  Sys.setenv(
    ORIGINAL_WORKERS = "1",
    ORIGINAL_REPLICATE = as.character(i),
    ORIGINAL_FILE = "osf/data.syn.csv",
    RESTRICTED_DIR = file.path(tempdir(), paste0("replicate_test_", i))
  )
  source("scripts/models_original.R")
  source("scripts/analysis_original.R")
  invisible(TRUE)
}

pending <- replicates
running <- list()
failed <- integer()
while (length(pending) > 0L || length(running) > 0L) {
  while (length(pending) > 0L && length(running) < workers) {
    i <- pending[[1]]
    pending <- pending[-1]
    message("Starting replicate ", i)
    running[[as.character(i)]] <- callr::r_bg(
      run_replicate,
      args = list(i = i),
      stdout = file.path(log_dir, paste0("replicate_", i, ".log")),
      stderr = "2>&1"
    )
  }
  Sys.sleep(5)
  for (name in names(running)) {
    if (!running[[name]]$is_alive()) {
      status <- tryCatch(
        {
          running[[name]]$get_result()
          "finished"
        },
        error = function(e) {
          failed <<- c(failed, as.integer(name))
          paste("FAILED:", conditionMessage(e))
        }
      )
      message("Replicate ", name, " ", status)
      running[[name]] <- NULL
    }
  }
}
if (length(failed) > 0L) {
  stop("Replicates failed: ", paste(sort(failed), collapse = ", "))
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(mgcv)
})
z_975 <- qnorm(0.975)
source(file.path("R", "analysis_helpers.R"))
source(file.path("R", "pool_replicate_runs.R"))
pooled <- pool_replicate_runs(synthetic_dir, replicates)
pooled$table
save(pooled, file = file.path("outputs", "synthetic", "pooled.rda"))
