# Stage: build_models. Requires R/analysis_helpers.R to be sourced first.

build_models <- function(
  prepared,
  output_dir,
  checkpoint_dir = NULL,
  workers = min(2L, future::availableCores()),
  diagnostic_dir = NULL,
  player_pairs = c("same", "all"),
  n_boot = 0L,
  seed = NULL,
  verbose = FALSE
) {
  stopifnot(inherits(prepared, "prepared_dataset"))
  player_pairs <- match.arg(player_pairs)
  data <- prepared$data
  label <- paste(
    prepared$source,
    if (!is.na(prepared$replicate)) {
      paste("replicate", prepared$replicate)
    }
  )
  checkpoint_for <- function(name) {
    if (is.null(checkpoint_dir)) NULL else file.path(checkpoint_dir, name)
  }

  if (verbose) {
    section(paste("MODELS:", label))
  }
  fits <- fit_dataset(
    data,
    source = prepared$source,
    replicate = prepared$replicate,
    save_diagnostic_files = !is.null(diagnostic_dir),
    diagnostic_dir = diagnostic_dir,
    report_progress = FALSE,
    verbose = verbose,
    candidate_workers = workers,
    checkpoint_dir = checkpoint_for("candidate_models"),
    engine = "bam",
    method = "fREML",
    discrete = TRUE
  )

  selected <- map_dfr(fits, "evidence") |>
    filter(delta_AIC == 0) |>
    select(outcome, selected_structure = model)
  tag <- function(x) {
    mutate(
      x,
      source = prepared$source,
      replicate = prepared$replicate,
      .before = 1
    )
  }

  linearity <- map2_dfr(
    selected$outcome,
    selected$selected_structure,
    \(outcome, kind) {
      linearity_sensitivity(
        data,
        outcome,
        kind,
        workers = workers,
        checkpoint_dir = checkpoint_for("linearity")
      )
    }
  ) |>
    tag()
  basis <- map2_dfr(
    selected$outcome,
    selected$selected_structure,
    \(outcome, kind) {
      basis_sensitivity(
        data,
        outcome,
        kind,
        workers = workers,
        checkpoint_dir = checkpoint_for("basis")
      )
    }
  ) |>
    tag()
  player_explanatory <- map_dfr(levels(data$Variable), \(outcome) {
    player_explanatory_comparison(
      data,
      outcome,
      workers = workers,
      report_progress = FALSE,
      checkpoint_dir = checkpoint_for("player_explanatory")
    )
  }) |>
    tag()
  player_deviance <- player_deviance_analysis(
    data,
    fits,
    pairs = player_pairs,
    n_boot = n_boot,
    workers = workers,
    seed = seed
  ) |>
    tag()

  bundle <- structure(
    list(
      source = prepared$source,
      replicate = prepared$replicate,
      data = data,
      fits = fits,
      linearity = linearity,
      basis_sensitivity = basis,
      player_explanatory = player_explanatory,
      player_deviance = player_deviance,
      metadata = c(
        prepared$metadata,
        list(
          built_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
          checkpointing = !is.null(checkpoint_dir),
          player_pairs = player_pairs,
          n_boot = as.integer(n_boot),
          bootstrap_seed = seed,
          k_check_seed = formals(k_diagnostics)$seed,
          session_info = utils::capture.output(print(sessionInfo()))
        )
      )
    ),
    class = "model_bundle"
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(bundle, file.path(output_dir, "models.rds"))
  invisible(bundle)
}

