# Stage: prep_synthetic. Requires R/analysis_helpers.R to be sourced first.

prep_synthetic <- function(
  original_path,
  output_dir,
  m = 10L,
  seed = 7183,
  public_dir = NULL,
  expected_matches = 236L
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  original_wide <- read_original_workbook(original_path)
  validate_wide_data(original_wide)
  original <- prepare_analysis_data(original_wide)
  validate_analysis_data(original, expected_matches = expected_matches)

  generated <- generate_valid_synthesis(original_wide, m = m, seed = seed)
  synthetic_all <- generated$synthetic_all
  walk(
    split(synthetic_all, synthetic_all$replicate),
    validate_analysis_data,
    expected_matches = expected_matches
  )

  # One prepared_dataset per replicate, built through the same path an outsider
  # uses, so steps 2 and 3 cannot tell the two apart.
  long_csv <- file.path(output_dir, "data.syn.csv")
  readr::write_csv(synthetic_all, long_csv)
  replicate_files <- vapply(
    seq_len(m),
    function(replicate) {
      prepared <- prep_dataset(long_csv, replicate = replicate)
      prepared$metadata$seed <- generated$seed
      file <- sprintf("replicate_%02d.rds", replicate)
      saveRDS(prepared, file.path(output_dir, file))
      file
    },
    character(1)
  )

  utility <- synthesis_utility(original, synthetic_all, m = m, seed = seed)
  saveRDS(utility$summary, file.path(output_dir, "utility_all.rds"))
  saveRDS(utility$general, file.path(output_dir, "utility_general.rds"))

  if (!is.null(public_dir)) {
    dir.create(public_dir, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(
      synthetic_all,
      file.path(public_dir, "data.syn.csv")
    )
    synthetic_all |>
      filter(replicate == 1) |>
      select(
        -replicate,
        -subject_id,
        -H1,
        -H2,
        -RefH1,
        -RefH2,
        -PlayerH1,
        -PlayerH2
      ) |>
      readr::write_csv(file.path(public_dir, "data.syn.csv"))
  }

  invisible(list(
    seed = generated$seed,
    replicate_files = file.path(output_dir, replicate_files),
    utility = utility$summary
  ))
}

