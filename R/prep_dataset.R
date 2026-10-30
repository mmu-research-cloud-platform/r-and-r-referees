# Stage: prep_dataset. Requires R/analysis_helpers.R to be sourced first.

prep_dataset <- function(
  path,
  replicate = NULL,
  expected_matches = NULL,
  output_path = NULL,
  verbose = TRUE
) {
  if (!file.exists(path)) {
    stop("Input file not found: ", path)
  }
  extension <- tolower(tools::file_ext(path))

  if (extension %in% c("xlsx", "xls")) {
    if (!is.null(replicate)) {
      stop("`replicate` applies only to synthetic CSV inputs.")
    }
    wide <- read_original_workbook(path)
    validate_wide_data(wide)
    data <- prepare_analysis_data(wide)
    validate_analysis_data(
      data,
      expected_matches = expected_matches %||% 236L
    )
    if (verbose) {
      print_data_checks(wide, data, "original data")
    }
    prepared <- new_prepared_dataset(
      data,
      source = "Original",
      replicate = NA_integer_,
      input_path = path
    )
  } else if (extension == "csv") {
    raw <- readr::read_csv(path, show_col_types = FALSE)
    if (!"replicate" %in% names(raw)) {
      raw$replicate <- 1L
      replicate <- 1L
    } else if (is.null(replicate)) {
      available <- sort(unique(raw$replicate))
      if (length(available) != 1L) {
        stop(
          "`replicate` is required: the file contains replicates ",
          paste(available, collapse = ", "),
          "."
        )
      }
      replicate <- available
    }
    data <- prepare_replicate_data(raw, replicate)
    if (!is.null(expected_matches)) {
      validate_analysis_data(data, expected_matches = expected_matches)
    }
    prepared <- new_prepared_dataset(
      data,
      source = "Hierarchy-aware synthetic",
      replicate = replicate,
      input_path = path
    )
  } else {
    stop("Unsupported input type: .", extension)
  }

  if (!is.null(output_path)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(prepared, output_path)
  }
  prepared
}

