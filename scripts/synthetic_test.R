#for the purpose of verifying the replicates produce similar results.
# R/analysis_helpers.R reads these environment variables when sourced, so
# they must be set before it runs (plain variables would be overwritten).
Sys.setenv(
  ORIGINAL_WORKERS = "10",
  ORIGINAL_REPLICATE = "2",
  ORIGINAL_FILE = "data.syn.csv",
  RESTRICTED_DIR = file.path(tempdir(), "replicate_test")
)
source("scripts/models_original.R")
source("scripts/analysis_original.R")
