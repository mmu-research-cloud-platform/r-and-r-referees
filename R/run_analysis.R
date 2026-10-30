# Stage: run_analysis. Requires R/analysis_helpers.R to be sourced first.

run_analysis <- function(models, output_dir) {
  bundle <- if (is.character(models)) readRDS(models) else models
  if (!inherits(bundle, "model_bundle")) {
    stop("`models` must be a model_bundle or the path to a models.rds file.")
  }
  tables <- analysis_tables(bundle)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  purrr::iwalk(tables, function(table, name) {
    readr::write_csv(table, file.path(output_dir, paste0(name, ".csv")))
  })
  invisible(tables)
}

