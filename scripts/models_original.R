# Restricted original-data model building
#
# Run from the project root. Fits all original-data models and saves them to
# outputs/original_restricted/models/. Outputs must not be published.
# Run scripts/analysis_original.R afterwards to produce tables and figures.
library(tidyverse)
library(readxl)
library(mgcv)
library(broom)
library(future)
library(future.apply)
library(progressr)

options(scipen = 999, progressr.enable = TRUE)
theme_set(theme_classic())

z_975 <- qnorm(0.975)

source(file.path("R", "analysis_helpers.R"))

# Synthetic (.csv) runs save to outputs/synthetic/replicate_<n>/ so they never
# touch the restricted original outputs.
use_synthetic <- grepl("\\.csv$", original_file)
save_outputs <- TRUE
replicate_index <- as.integer(Sys.getenv("ORIGINAL_REPLICATE", "1"))
output_dir <- if (use_synthetic) {
  file.path(synthetic_dir, paste0("replicate_", replicate_index))
} else {
  restricted_dir
}
model_dir <- file.path(output_dir, "models")
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
checkpoint_path <- function(...) file.path(output_dir, "checkpoints", ...)

section("IMPORT AND PREPARATION")
start_analysis_progress(total = 20L)
if (use_synthetic) {
  # Validation run on one synthetic replicate (see R/analysis_helpers.R)
  original <- prepare_replicate_data(
    read_csv(original_file, show_col_types = FALSE),
    replicate_index
  )
} else {
  original_wide <- read_original_workbook(original_file)
  validate_wide_data(original_wide)
  original <- prepare_analysis_data(original_wide)
  validate_analysis_data(original, expected_matches = 236)
  print_data_checks(original_wide, original, "restricted original data")
}

section("ORIGINAL-DATA MODELS")
original_workers <- min(
  as.integer(Sys.getenv("ORIGINAL_WORKERS", "10")),
  future::availableCores()
)
message(
  "Running original candidate and sensitivity fits with ",
  original_workers,
  " parallel workers."
)
original_fits <- fit_dataset(
  original,
  source = "Original",
  save_diagnostic_files = FALSE,
  print_diagnostics = FALSE,
  candidate_workers = original_workers,
  checkpoint_dir = checkpoint_path("candidate_models"),
  engine = "bam",
  method = "fREML",
  discrete = TRUE
)

section("ORIGINAL-DATA RESIDUAL DIAGNOSTICS (QQ AND ACF)")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
walk(original_fits, \(fit) {
  prefix <- file.path(
    output_dir,
    paste0(str_replace_all(tolower(fit$outcome), " ", "_"), "_original")
  )
  save_diagnostics(
    fit$final,
    original,
    fit$outcome,
    prefix,
    save = save_outputs
  )
})

section("ORIGINAL-DATA LINEARITY AND BASIS SENSITIVITY")
selected_structures <- map_dfr(original_fits, "evidence") |>
  filter(delta_AIC == 0) |>
  select(outcome, selected_structure = model)
original_linearity <- map2_dfr(
  selected_structures$outcome,
  selected_structures$selected_structure,
  \(outcome, kind) {
    linearity_sensitivity(
      original,
      outcome,
      kind,
      workers = original_workers,
      checkpoint_dir = checkpoint_path("linearity")
    )
  }
)
original_basis_sensitivity <- map2_dfr(
  selected_structures$outcome,
  selected_structures$selected_structure,
  \(outcome, kind) {
    basis_sensitivity(
      original,
      outcome,
      kind,
      workers = original_workers,
      checkpoint_dir = checkpoint_path("basis")
    )
  }
)

section("PLAYER MOVEMENT EXPLANATION OF REFEREE MOVEMENT")
original_player_explanatory <- map_dfr(
  levels(original$Variable),
  \(outcome) {
    player_explanatory_comparison(
      original,
      outcome,
      workers = original_workers,
      checkpoint_dir = checkpoint_path("player_explanatory")
    )
  }
)

section("PLAYER DEVIANCE (REFEREE-LEVEL BOOTSTRAP)")
original_player_deviance <- player_deviance_analysis(
  original,
  original_fits,
  pairs = "same",
  n_boot = 500L,
  workers = original_workers,
  seed = 7183
)

if (save_outputs) {
  saveRDS(original, file.path(model_dir, "original_data.rds"))
  saveRDS(original_fits, file.path(model_dir, "original_fits.rds"))
  saveRDS(original_linearity, file.path(model_dir, "original_linearity.rds"))
  saveRDS(
    original_basis_sensitivity,
    file.path(model_dir, "original_basis_sensitivity.rds")
  )
  saveRDS(
    original_player_explanatory,
    file.path(model_dir, "original_player_explanatory.rds")
  )
  saveRDS(
    original_player_deviance,
    file.path(model_dir, "original_player_deviance.rds")
  )
}
close_analysis_progress()
message("Completed model building. Saved to: ", model_dir)
