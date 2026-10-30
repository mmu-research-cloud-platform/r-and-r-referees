# Synthetic-data generation and model building
#
# Run from the project root. Synthesises the original data, fits all
# synthetic-replicate models and utility measures, and saves them to
# outputs/synthetic/models/. Run scripts/analysis_synthetic.R afterwards to pool
# results and produce public outputs. This script does not fit the
# original-data models.
library(tidyverse)
library(readxl)
library(mgcv)
library(gratia)
library(synthpop)
library(broom)
library(future)
library(future.apply)
library(progressr)

options(scipen = 999, progressr.enable = TRUE)
theme_set(theme_classic())

seed_synthesis <- 7183
m_synthetic <- 10
z_975 <- qnorm(0.975)

source(file.path("R", "analysis_helpers.R"))

model_dir <- file.path(synthetic_dir, "models")
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("osf", showWarnings = FALSE)

section("IMPORT AND PREPARATION FOR SYNTHESIS")
start_analysis_progress(total = m_synthetic + 2L)
original_wide <- read_original_workbook(original_file)
validate_wide_data(original_wide)
original <- prepare_analysis_data(original_wide)
validate_analysis_data(original, expected_matches = 236)
print_data_checks(original_wide, original, "restricted original data")

section("HIERARCHY-AWARE SYNTHESIS: TEN REPLICATES")
synthesis_attempt <- 0L
repeat {
  primary_synthesis <- synthesise_hierarchy(
    original_wide,
    m = m_synthetic,
    seed = seed_synthesis + synthesis_attempt
  )
  synthetic_all <- synthesis_to_long(primary_synthesis)
  invalid_replicates <- synthetic_all |>
    summarise(
      invalid = anyNA(Distance) |
        any(Distance < 0, na.rm = TRUE) |
        any(Variable == "Low Speed Activity" & Distance <= 0, na.rm = TRUE),
      .by = replicate
    ) |>
    filter(invalid) |>
    pull(replicate)
  if (length(invalid_replicates) == 0L) {
    break
  }
  message(
    "Retrying synthesis: invalid replicate(s) ",
    paste(invalid_replicates, collapse = ", "),
    " with seed ",
    seed_synthesis + synthesis_attempt + 1L
  )
  synthesis_attempt <- synthesis_attempt + 1L
  if (synthesis_attempt >= 20L) {
    stop("Could not generate valid synthetic replicates in 20 attempts.")
  }
}
message(
  "Using valid hierarchy-aware synthesis seed: ",
  seed_synthesis + synthesis_attempt
)
advance_analysis_progress()
walk(
  split(synthetic_all, synthetic_all$replicate),
  validate_analysis_data,
  expected_matches = 236
)

write_csv(synthetic_all, file.path("osf", "data.syn.csv"))
write_csv(
  filter(synthetic_all, replicate == 1) |>
    select(
      -replicate,
      -subject_id,
      -H1,
      -H2,
      -RefH1,
      -RefH2,
      -PlayerH1,
      -PlayerH2
    ),
  file.path("osf", "data.syn.csv")
)

section("LEGACY SYNTHESIS SENSITIVITY: TEN REPLICATES")
legacy_synthesis <- synthesise_legacy(
  original,
  m = m_synthetic,
  seed = seed_synthesis
)
advance_analysis_progress()
legacy_utility <- map2_dfr(
  legacy_synthesis$syn,
  seq_along(legacy_synthesis$syn),
  function(data, replicate) {
    utility_summary(
      original,
      data |>
        mutate(
          Match = factor(Match),
          id = factor(id),
          Role = factor(Role, levels = levels(original$Role)),
          Variable = factor(Variable, levels = levels(original$Variable))
        )
    ) |>
      mutate(replicate = replicate, synthesis_method = "Legacy long-format")
  }
)

section("SYNTHETIC MODELS ACROSS TEN REPLICATES")
parallel_workers <- min(m_synthetic, future::availableCores())
message(
  "Running synthetic replicates with ",
  parallel_workers,
  " parallel workers."
)
# Avoid interleaving the sequential and replicate-level terminal progress bars.
close_analysis_progress()
synthetic_fits <- run_synthetic_fits_parallel(
  synthetic_all,
  m = m_synthetic,
  workers = parallel_workers,
  seed = seed_synthesis + 2L,
  checkpoint_dir = file.path(synthetic_dir, "checkpoints", "candidate_models"),
  engine = "bam",
  method = "fREML",
  discrete = TRUE
)
resume_analysis_progress()

section("SYNTHETIC LINEARITY AND BASIS SENSITIVITY")
synthetic_linearity <- map_dfr(seq_len(m_synthetic), function(replicate) {
  replicate_data <- filter(synthetic_all, replicate == !!replicate) |>
    select(-replicate)
  selected_structures <- map_dfr(synthetic_fits[[replicate]], "evidence") |>
    filter(delta_AIC == 0) |>
    select(outcome, selected_structure = model)
  map2_dfr(
    selected_structures$outcome,
    selected_structures$selected_structure,
    function(outcome, kind) {
      linearity_sensitivity(
        replicate_data,
        outcome,
        kind,
        workers = parallel_workers,
        checkpoint_dir = file.path(
          synthetic_dir,
          "checkpoints",
          "linearity",
          paste0("replicate_", replicate)
        )
      ) |>
        mutate(replicate = replicate, .before = 1)
    }
  )
})
synthetic_basis_sensitivity <- map_dfr(
  seq_len(m_synthetic),
  function(replicate) {
    replicate_data <- filter(synthetic_all, replicate == !!replicate) |>
      select(-replicate)
    selected_structures <- map_dfr(synthetic_fits[[replicate]], "evidence") |>
      filter(delta_AIC == 0) |>
      select(outcome, selected_structure = model)
    map2_dfr(
      selected_structures$outcome,
      selected_structures$selected_structure,
      function(outcome, kind) {
        basis_sensitivity(
          replicate_data,
          outcome,
          kind,
          workers = parallel_workers,
          checkpoint_dir = file.path(
            synthetic_dir,
            "checkpoints",
            "basis",
            paste0("replicate_", replicate)
          )
        ) |>
          mutate(replicate = replicate, .before = 1)
      }
    )
  }
)

section("SYNTHESIS UTILITY")
primary_utility <- map_dfr(seq_len(m_synthetic), function(replicate) {
  utility_summary(original, filter(synthetic_all, replicate == !!replicate)) |>
    mutate(replicate = replicate, synthesis_method = "Hierarchy-aware wide")
})
utility_all <- bind_rows(primary_utility, legacy_utility)

# synthpop general utility includes named pMSE measures and their null references.
primary_utility_gen <- map(seq_len(m_synthetic), function(replicate) {
  synthetic_simple <- filter(synthetic_all, replicate == !!replicate) |>
    transmute(Role, Time, Variable, Distance) |>
    as.data.frame()
  original_simple <- original |>
    transmute(Role, Time, Variable, Distance) |>
    as.data.frame()
  utility <- utility.gen(synthetic_simple, original_simple, method = "logit")
  advance_analysis_progress()
  utility
})

# Original data are kept only for the original-vs-synthetic density plot.
saveRDS(original, file.path(model_dir, "original_data.rds"))
saveRDS(synthetic_all, file.path(model_dir, "synthetic_all.rds"))
saveRDS(synthetic_fits, file.path(model_dir, "synthetic_fits.rds"))
saveRDS(synthetic_linearity, file.path(model_dir, "synthetic_linearity.rds"))
saveRDS(
  synthetic_basis_sensitivity,
  file.path(model_dir, "synthetic_basis_sensitivity.rds")
)
saveRDS(utility_all, file.path(model_dir, "utility_all.rds"))
saveRDS(primary_utility_gen, file.path(model_dir, "primary_utility_gen.rds"))
close_analysis_progress()
message("Completed synthetic-data model building. Saved to: ", model_dir)
