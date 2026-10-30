# Analysis helpers for the original and synthetic pipelines.
# Source from the project root. Attaches packages, sets shared settings and
# defines every function used by the stage files and the models_/analysis_ scripts.

library(tidyverse)
library(readxl)
library(mgcv)
library(gratia)
library(synthpop)
library(broom)
library(patchwork)
library(future)
library(future.apply)
library(progressr)
options(scipen = 999, progressr.enable = TRUE)
theme_set(theme_classic())
seed_synthesis <- 7183
m_synthetic <- 10
z_975 <- qnorm(0.975)
# Environment overrides let scripts/models_original.R and scripts/analysis_original.R be
# validated on one synthetic replicate (long-format .csv with a replicate
# column; ORIGINAL_REPLICATE selects it) without touching restricted outputs.
original_file <- Sys.getenv("ORIGINAL_FILE", "data.orginal.xlsx")
restricted_dir <- Sys.getenv(
  "RESTRICTED_DIR",
  file.path("outputs", "original_restricted")
)
synthetic_dir <- file.path("outputs", "synthetic")
dir.create(restricted_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(synthetic_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("osf", showWarnings = FALSE)

section <- function(title) {
  message("\n", strrep("=", 78), "\n", title, "\n", strrep("=", 78))
}

analysis_progress <- NULL

analysis_progress_step <- 0L

# The main bar tracks sequential work; parallel replicates have a separate bar.
analysis_progress_total <- 26L

start_analysis_progress <- function(total = analysis_progress_total) {
  analysis_progress_total <<- as.integer(total)
  analysis_progress_step <<- 0L
  analysis_progress <<- txtProgressBar(
    min = 0,
    max = analysis_progress_total,
    initial = 0,
    style = 3
  )
}

advance_analysis_progress <- function(steps = 1L) {
  if (is.null(analysis_progress)) {
    return(invisible(analysis_progress_step))
  }
  analysis_progress_step <<- analysis_progress_step + steps
  setTxtProgressBar(
    analysis_progress,
    min(analysis_progress_step, analysis_progress_total)
  )
  invisible(analysis_progress_step)
}

close_analysis_progress <- function() {
  if (!is.null(analysis_progress)) {
    close(analysis_progress)
    analysis_progress <<- NULL
  }
}

resume_analysis_progress <- function() {
  analysis_progress <<- txtProgressBar(
    min = 0,
    max = analysis_progress_total,
    initial = analysis_progress_step,
    style = 3
  )
}

read_original_workbook <- function(path) {
  # Prefer a temporary short path for Windows, but fall back to the source file
  # when a temporary copy is unavailable (for example, due to a file lock).
  temporary_directory <- tempfile("original_soccer_data_")
  dir.create(temporary_directory)
  short_path <- file.path(temporary_directory, "data.xlsx")
  workbook_path <- if (file.copy(path, short_path)) {
    short_path
  } else {
    message(
      "Temporary workbook copy unavailable; reading source file directly."
    )
    path
  }

  message(
    "Workbook sheets: ",
    paste(excel_sheets(workbook_path), collapse = ", ")
  )
  read_excel(workbook_path)
}

validate_wide_data <- function(data) {
  expected_measurements <- as.vector(outer(
    c("ref_td", "ref_hir", "ref_spr", "player_td", "player_hir", "player_spr"),
    seq(5, 90, 5),
    paste,
    sep = "_"
  ))
  expected <- c("Match", "id", expected_measurements)

  if (!setequal(names(data), expected)) {
    missing <- setdiff(expected, names(data))
    extra <- setdiff(names(data), expected)
    stop(
      "Unexpected workbook schema. Missing: ",
      paste(missing, collapse = ", "),
      "; extra: ",
      paste(extra, collapse = ", ")
    )
  }
  if (anyDuplicated(data$Match)) {
    stop("Match identifiers must be unique in the original wide data.")
  }
  if (anyNA(data)) {
    stop("The original workbook contains missing values.")
  }
  invisible(data)
}

prepare_analysis_data <- function(data) {
  data |>
    mutate(id = paste0("ID_", str_pad(as.numeric(factor(id)), 2, pad = "0"))) |>
    pivot_longer(
      cols = -c(Match, id),
      names_sep = "_",
      names_to = c("Role", "Component", "Time"),
      values_to = "Distance"
    ) |>
    mutate(
      Role = recode(Role, ref = "Referee", player = "Player"),
      Component = recode(
        Component,
        td = "Total Distance",
        hir = "High Intensity Running",
        spr = "Sprinting"
      ),
      Time = as.integer(Time)
    ) |>
    pivot_wider(names_from = Component, values_from = Distance) |>
    mutate(
      `High Speed Activity` = `High Intensity Running` + Sprinting,
      `Low Speed Activity` = `Total Distance` - `High Speed Activity`
    ) |>
    select(
      Match,
      id,
      Role,
      Time,
      `Low Speed Activity`,
      `High Speed Activity`
    ) |>
    pivot_longer(
      cols = c(`Low Speed Activity`, `High Speed Activity`),
      names_to = "Variable",
      values_to = "Distance"
    ) |>
    mutate(
      # Convert 5-minute epoch totals (m) to metres per minute
      Distance = Distance / 5,
      Match = factor(Match),
      id = factor(id),
      Role = factor(Role, levels = c("Player", "Referee")),
      Variable = factor(
        Variable,
        levels = c("Low Speed Activity", "High Speed Activity")
      ),
      subject_id = interaction(id, Role, drop = TRUE),
      Half = factor(
        if_else(Time <= 45, "First", "Second"),
        levels = c("First", "Second")
      ),
      Time_half = if_else(Half == "First", Time, Time - 45L),
      H1 = as.numeric(Half == "First"),
      H2 = as.numeric(Half == "Second"),
      RefH1 = as.numeric(Role == "Referee" & Half == "First"),
      RefH2 = as.numeric(Role == "Referee" & Half == "Second"),
      PlayerH1 = as.numeric(Role == "Player" & Half == "First"),
      PlayerH2 = as.numeric(Role == "Player" & Half == "Second")
    ) |>
    arrange(Match, Role, Variable, Time)
}

validate_analysis_data <- function(data, expected_matches = NULL) {
  key_duplicates <- data |>
    count(Match, Role, Variable, Time) |>
    filter(n != 1)
  series_sizes <- data |>
    count(Match, Role, Variable) |>
    filter(n != 18)

  if (nrow(key_duplicates) > 0 || nrow(series_sizes) > 0) {
    stop("Analysis data do not contain one complete 18-epoch series per key.")
  }
  if (anyNA(data$Distance) || any(data$Distance < 0)) {
    stop("Distance must be complete and non-negative.")
  }
  if (
    !is.null(expected_matches) && n_distinct(data$Match) != expected_matches
  ) {
    stop("Unexpected number of matches in analysis data.")
  }
  invisible(data)
}

print_data_checks <- function(wide, long, label) {
  section(paste("DATA CHECKS:", label))
  print(dim(wide))
  print(head(wide))
  print(
    long |>
      summarise(
        rows = n(),
        matches = n_distinct(Match),
        referee_ids = n_distinct(id),
        subjects = n_distinct(subject_id),
        missing_distance = sum(is.na(Distance)),
        minimum_distance = min(Distance),
        maximum_distance = max(Distance)
      )
  )
  print(
    long |>
      summarise(
        mean = mean(Distance),
        sd = sd(Distance),
        median = median(Distance),
        zero_fraction = mean(Distance == 0),
        .by = c(Variable, Role)
      )
  )
  print(
    long |>
      filter(Role == "Referee", Variable == "Low Speed Activity") |>
      distinct(Match, id) |>
      count(id, name = "matches") |>
      arrange(desc(matches))
  )
}

prepare_replicate_data <- function(synthetic_all, index) {
  replicate_data <- synthetic_all |>
    filter(.data$replicate == index)
  if (nrow(replicate_data) == 0L) {
    stop("No synthetic data found for replicate index ", index, ".")
  }
  if (
    !all(
      c("Match", "id", "Role", "Time", "Variable", "Distance") %in%
        names(replicate_data)
    )
  ) {
    stop("Synthetic replicate is missing required analysis columns.")
  }

  replicate_data <- replicate_data |>
    mutate(
      Match = factor(Match),
      id = factor(id),
      Role = factor(Role, levels = c("Player", "Referee")),
      Variable = factor(
        Variable,
        levels = c("Low Speed Activity", "High Speed Activity")
      ),
      Half = factor(
        if_else(Time <= 45, "First", "Second"),
        levels = c("First", "Second")
      ),
      Time_half = if_else(Half == "First", Time, Time - 45L),
      H1 = as.numeric(Half == "First"),
      H2 = as.numeric(Half == "Second"),
      RefH1 = as.numeric(Role == "Referee" & Half == "First"),
      RefH2 = as.numeric(Role == "Referee" & Half == "Second"),
      PlayerH1 = as.numeric(Role == "Player" & Half == "First"),
      PlayerH2 = as.numeric(Role == "Player" & Half == "Second"),
      subject_id = interaction(id, Role, drop = TRUE)
    ) |>
    select(-replicate) |>
    arrange(Match, Role, Variable, Time)

  validate_analysis_data(replicate_data)
  replicate_data
}

checkpoint_signature <- function(...) {
  signature_path <- tempfile("model_checkpoint_signature_", fileext = ".rds")
  on.exit(unlink(signature_path), add = TRUE)
  saveRDS(list(...), signature_path)
  unname(tools::md5sum(signature_path))
}

fit_smooth_model <- function(
  formula,
  data,
  family,
  engine = c("bam", "gam"),
  method = NULL,
  discrete = NULL,
  select = TRUE,
  control = gam.control(maxit = 100)
) {
  engine <- match.arg(engine)
  if (is.null(method)) {
    method <- if (engine == "bam") "fREML" else "REML"
  }

  if (engine == "bam" && !identical(method, "fREML")) {
    stop("BAM fitting in this helper requires method = 'fREML'.")
  }
  if (engine == "gam" && !method %in% c("REML", "ML")) {
    stop("GAM fitting in this helper requires method = 'REML' or 'ML'.")
  }
  if (is.null(discrete)) {
    discrete <- engine == "bam"
  }
  if (engine != "bam" && isTRUE(discrete)) {
    stop("discrete = TRUE is only available with BAM.")
  }

  fitter <- if (engine == "bam") mgcv::bam else mgcv::gam
  arguments <- list(
    formula = formula,
    data = data,
    family = family,
    method = method,
    select = select,
    control = control
  )
  if (engine == "bam") {
    arguments$discrete <- discrete
  }
  do.call(fitter, arguments)
}

checkpoint_model <- function(path, signature, fit) {
  if (!is.null(path) && file.exists(path)) {
    checkpoint <- readRDS(path)
    if (is.list(checkpoint) && identical(checkpoint$signature, signature)) {
      message("Loading model checkpoint: ", path)
      return(checkpoint$model)
    }
    message("Refitting stale model checkpoint: ", path)
  }

  model <- fit()
  if (!is.null(path)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    temporary_path <- tempfile(
      pattern = paste0(basename(path), "-"),
      tmpdir = dirname(path),
      fileext = ".tmp"
    )
    saveRDS(list(signature = signature, model = model), temporary_path)
    if (!file.rename(temporary_path, path)) {
      unlink(temporary_path)
      stop("Could not save model checkpoint: ", path)
    }
    message("Saved model checkpoint: ", path)
  }
  model
}

run_parallel_fits <- function(
  items,
  fit_one,
  workers = 2L,
  labels = as.character(items),
  report_progress = TRUE
) {
  workers <- min(as.integer(workers), length(items), future::availableCores())

  if (workers <= 1L) {
    return(purrr::map(items, function(item) {
      result <- fit_one(item)
      if (report_progress) {
        advance_analysis_progress()
      }
      result
    }))
  }

  old_plan <- future::plan()
  progress_was_open <- report_progress && !is.null(analysis_progress)
  on.exit(
    {
      future::plan(old_plan)
      if (progress_was_open && is.null(analysis_progress)) {
        resume_analysis_progress()
      }
    },
    add = TRUE
  )

  if (progress_was_open) {
    close_analysis_progress()
  }
  future::plan(future::multisession, workers = workers)

  results <- progressr::with_progress(
    {
      progress <- progressr::progressor(steps = length(items))
      future.apply::future_lapply(
        seq_along(items),
        function(index) {
          result <- fit_one(items[[index]])
          progress(message = paste(labels[[index]], "complete"))
          result
        },
        future.seed = TRUE,
        future.packages = c("dplyr", "mgcv")
      )
    },
    handlers = progressr::handler_txtprogressbar,
    cleanup = TRUE
  )

  if (progress_was_open) {
    resume_analysis_progress()
    advance_analysis_progress(length(items))
  }
  results
}

run_synthetic_fits_parallel <- function(
  synthetic_all,
  m,
  workers,
  seed,
  checkpoint_dir = NULL,
  engine = "bam",
  method = NULL,
  discrete = NULL
) {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::multisession, workers = workers)

  progressr::with_progress(
    {
      progress <- progressr::progressor(steps = m)
      future.apply::future_lapply(
        seq_len(m),
        function(replicate) {
          result <- fit_dataset(
            filter(synthetic_all, replicate == !!replicate) |>
              select(-replicate),
            source = "Hierarchy-aware synthetic",
            replicate = replicate,
            save_diagnostic_files = TRUE,
            diagnostic_dir = file.path(
              synthetic_dir,
              "diagnostics",
              paste0("replicate_", replicate)
            ),
            report_progress = FALSE,
            verbose = FALSE,
            checkpoint_dir = if (is.null(checkpoint_dir)) {
              NULL
            } else {
              file.path(checkpoint_dir, paste0("replicate_", replicate))
            },
            engine = engine,
            method = method,
            discrete = discrete
          )
          progress(message = paste("replicate", replicate, "complete"))
          result
        },
        future.seed = seed,
        future.packages = c("tidyverse", "mgcv", "broom")
      )
    },
    handlers = progressr::handler_txtprogressbar,
    cleanup = TRUE
  )
}

mean_formula <- function(
  kind = c("common", "deviation", "independent"),
  k = 5
) {
  kind <- match.arg(kind)
  base <- "Distance ~ Role * Half"
  random <- paste0(
    " + s(subject_id, bs = 're')",
    " + s(Time_half, subject_id, bs = 'fs', k = 4, m = 1)",
    " + s(Match, bs = 're')"
  )

  smooth <- switch(
    kind,
    common = paste0(
      " + s(Time_half, by = H1, bs = 'cr', k = ",
      k,
      ", id = 1)",
      " + s(Time_half, by = H2, bs = 'cr', k = ",
      k,
      ", id = 2)"
    ),
    deviation = paste0(
      " + s(Time_half, by = H1, bs = 'cr', k = ",
      k,
      ", id = 1)",
      " + s(Time_half, by = H2, bs = 'cr', k = ",
      k,
      ", id = 2)",
      " + s(Time_half, by = RefH1, bs = 'cr', k = ",
      k,
      ", id = 3)",
      " + s(Time_half, by = RefH2, bs = 'cr', k = ",
      k,
      ", id = 4)"
    ),
    independent = paste0(
      " + s(Time_half, by = PlayerH1, bs = 'cr', k = ",
      k,
      ", id = 1)",
      " + s(Time_half, by = RefH1, bs = 'cr', k = ",
      k,
      ", id = 2)",
      " + s(Time_half, by = PlayerH2, bs = 'cr', k = ",
      k,
      ", id = 3)",
      " + s(Time_half, by = RefH2, bs = 'cr', k = ",
      k,
      ", id = 4)"
    )
  )

  as.formula(paste0(base, smooth, random))
}

fit_candidate_models <- function(
  data,
  outcome,
  k = 5,
  report_progress = TRUE,
  workers = 1L,
  checkpoint_dir = NULL,
  engine = "bam",
  method = NULL,
  discrete = NULL
) {
  outcome_data <- data |>
    filter(Variable == outcome) |>
    droplevels()

  if (outcome == "Low Speed Activity" && any(outcome_data$Distance <= 0)) {
    stop("Tweedie(log) low-speed models require strictly positive distances.")
  }

  kinds <- c("common", "deviation", "independent")
  fit_one <- function(kind) {
    family <- tw(link = "log")
    checkpoint_path <- if (is.null(checkpoint_dir)) {
      NULL
    } else {
      file.path(
        checkpoint_dir,
        paste0(
          str_replace_all(tolower(outcome), " ", "_"),
          "_",
          kind,
          "_",
          engine,
          "_",
          method %||% if (engine == "bam") "fREML" else "REML",
          ".rds"
        )
      )
    }
    model_formula <- mean_formula(kind, k)
    actual_method <- method %||% if (engine == "bam") "fREML" else "REML"
    actual_discrete <- if (is.null(discrete)) engine == "bam" else discrete
    checkpoint_model(
      checkpoint_path,
      checkpoint_signature(
        "candidate",
        engine,
        actual_method,
        actual_discrete,
        outcome,
        family$family,
        family$link,
        kind,
        k,
        gam.control(maxit = 100),
        outcome_data
      ),
      function() {
        fit_smooth_model(
          formula = model_formula,
          data = outcome_data,
          family = family,
          engine = engine,
          method = method,
          discrete = discrete,
          control = gam.control(maxit = 100)
        )
      }
    )
  }

  fits <- run_parallel_fits(
    kinds,
    fit_one,
    workers = workers,
    labels = paste(outcome, kinds),
    report_progress = report_progress
  )
  rlang::set_names(fits, kinds)
}

fit_model <- function(
  data,
  outcome,
  formula,
  engine = "bam",
  method = NULL,
  discrete = NULL,
  checkpoint_path = NULL
) {
  outcome_data <- data |>
    filter(Variable == outcome) |>
    droplevels()
  if (outcome == "Low Speed Activity" && any(outcome_data$Distance <= 0)) {
    stop("Tweedie(log) low-speed models require strictly positive distances.")
  }
  family <- tw(link = "log")

  actual_method <- method %||% if (engine == "bam") "fREML" else "REML"
  if (!is.null(checkpoint_path)) {
    checkpoint_path <- sub(
      "\\.rds$",
      paste0("_", engine, "_", actual_method, ".rds"),
      checkpoint_path
    )
  }

  checkpoint_model(
    checkpoint_path,
    checkpoint_signature(
      "fit_model",
      engine,
      actual_method,
      if (is.null(discrete)) engine == "bam" else discrete,
      outcome,
      family$family,
      family$link,
      formula,
      gam.control(maxit = 100),
      outcome_data
    ),
    function() {
      model_formula <- formula
      fit_smooth_model(
        formula = model_formula,
        data = outcome_data,
        family = family,
        engine = engine,
        method = method,
        discrete = discrete,
        control = gam.control(maxit = 100)
      )
    }
  )
}

fit_dataset <- function(
  data,
  source,
  replicate = NA_integer_,
  save_diagnostic_files = FALSE,
  print_diagnostics = FALSE,
  diagnostic_dir = restricted_dir,
  report_progress = TRUE,
  verbose = TRUE,
  candidate_workers = 1L,
  checkpoint_dir = NULL,
  engine = "bam",
  method = NULL,
  discrete = NULL
) {
  outcomes <- levels(data$Variable)
  result <- map(outcomes, function(outcome) {
    if (verbose) {
      model_label <- paste("MODELS:", source, outcome)
      if (!is.na(replicate)) {
        model_label <- paste(model_label, "replicate", replicate)
      }
      section(model_label)
    }
    candidates <- fit_candidate_models(
      data,
      outcome,
      report_progress = report_progress,
      workers = candidate_workers,
      checkpoint_dir = if (is.null(checkpoint_dir)) {
        NULL
      } else {
        file.path(checkpoint_dir, "candidates")
      },
      engine = engine,
      method = method,
      discrete = discrete
    )
    evidence <- model_evidence(candidates)
    final <- select_final_model(candidates)
    is_bam <- inherits(final, "bam")
    model_metadata <- tibble(
      engine = if (is_bam) "bam" else "gam",
      method = final$method,
      discrete = if (is_bam) {
        if (is.null(discrete)) TRUE else isTRUE(discrete)
      } else {
        FALSE
      },
      family = final$family$family,
      link = final$family$link
    )
    if (verbose) {
      print(evidence)
      print(summary(final))
      print(k_diagnostics(final))
      print(gam.vcomp(final))
    }

    if (save_diagnostic_files || print_diagnostics) {
      if (save_diagnostic_files) {
        dir.create(diagnostic_dir, recursive = TRUE, showWarnings = FALSE)
      }
      prefix <- file.path(
        diagnostic_dir,
        paste0(
          str_replace_all(tolower(outcome), " ", "_"),
          if (is.na(replicate)) {
            "_original"
          } else {
            paste0("_replicate_", replicate)
          }
        )
      )
      save_diagnostics(
        final,
        data,
        outcome,
        prefix,
        save = save_diagnostic_files
      )
    }

    list(
      outcome = outcome,
      candidates = candidates,
      evidence = evidence |>
        mutate(
          source = source,
          replicate = replicate,
          outcome = outcome,
          !!!model_metadata,
          .before = 1
        ),
      final = final,
      estimates = extract_model_results(final, outcome, source, replicate) |>
        mutate(!!!model_metadata, .before = 1),
      predictions = predict_population(final, data, outcome) |>
        mutate(source = source, replicate = replicate, !!!model_metadata),
      contrasts = role_contrasts(final, data, outcome) |>
        mutate(source = source, replicate = replicate, !!!model_metadata),
      variance = variance_components(final, source, replicate) |>
        mutate(outcome = outcome, !!!model_metadata, .after = replicate)
    )
  })
  names(result) <- outcomes
  result
}

run_synthetic_replicate <- function(
  synthetic_all,
  index,
  save_diagnostic_files = TRUE,
  diagnostic_dir = NULL,
  report_progress = TRUE,
  verbose = TRUE,
  candidate_workers = 1L,
  checkpoint_dir = NULL,
  engine = "bam",
  method = NULL,
  discrete = NULL,
  fit_function = fit_dataset
) {
  if (
    length(index) != 1L ||
      !is.numeric(index) ||
      !is.finite(index) ||
      index < 1 ||
      index != floor(index)
  ) {
    stop("index must be one positive integer synthetic replicate index.")
  }
  if (!"replicate" %in% names(synthetic_all)) {
    stop("synthetic_all must contain a 'replicate' column.")
  }

  replicate_data <- prepare_replicate_data(synthetic_all, index)

  if (is.null(diagnostic_dir)) {
    diagnostic_dir <- file.path(
      synthetic_dir,
      "diagnostics",
      paste0("replicate_", index)
    )
  }
  if (is.null(checkpoint_dir)) {
    checkpoint_dir <- file.path(
      synthetic_dir,
      "checkpoints",
      "single_replicate",
      paste0("replicate_", index)
    )
  }

  fit_function(
    replicate_data,
    source = "Hierarchy-aware synthetic",
    replicate = as.integer(index),
    save_diagnostic_files = save_diagnostic_files,
    diagnostic_dir = diagnostic_dir,
    report_progress = report_progress,
    verbose = verbose,
    candidate_workers = candidate_workers,
    checkpoint_dir = checkpoint_dir,
    engine = engine,
    method = method,
    discrete = discrete
  )
}

prepare_player_explanatory_data <- function(data, outcome) {
  data |>
    filter(Variable == outcome) |>
    select(Match, id, Time, Half, Time_half, H1, H2, Role, Distance) |>
    pivot_wider(names_from = Role, values_from = Distance) |>
    rename(
      referee_id = id,
      referee_distance = Referee,
      player_distance = Player
    ) |>
    mutate(
      referee_id = factor(referee_id),
      match_order = as.integer(Match)
    ) |>
    droplevels()
}

player_explanatory_formula <- function(include_player = FALSE, k = 5) {
  base <- paste0(
    "referee_distance ~ Half",
    " + s(Time_half, by = H1, bs = 'cr', k = ",
    k,
    ", id = 1)",
    " + s(Time_half, by = H2, bs = 'cr', k = ",
    k,
    ", id = 2)",
    " + s(match_order, bs = 'cr', k = 10)",
    " + s(referee_id, bs = 're')",
    " + s(Time_half, referee_id, bs = 'fs', k = 4, m = 1)",
    " + s(Match, bs = 're')"
  )
  player_terms <- paste0(
    " + s(player_distance, by = H1, bs = 'cr', k = ",
    k,
    ", id = 3)",
    " + s(player_distance, by = H2, bs = 'cr', k = ",
    k,
    ", id = 4)"
  )

  as.formula(paste0(base, if (include_player) player_terms else ""))
}

player_explanatory_comparison <- function(
  data,
  outcome,
  k = 5,
  workers = 1L,
  report_progress = TRUE,
  checkpoint_dir = NULL
) {
  paired_data <- prepare_player_explanatory_data(data, outcome)
  if (
    outcome == "Low Speed Activity" &&
      any(
        paired_data$referee_distance <= 0 |
          paired_data$player_distance <= 0
      )
  ) {
    stop("Tweedie(log) low-speed models require strictly positive distances.")
  }

  specifications <- c(base = FALSE, `base + player movement` = TRUE)
  fits <- run_parallel_fits(
    specifications,
    function(include_player) {
      family <- tw(link = "log")
      formula <- player_explanatory_formula(include_player, k)

      checkpoint_path <- if (is.null(checkpoint_dir)) {
        NULL
      } else {
        file.path(
          checkpoint_dir,
          paste0(
            str_replace_all(tolower(outcome), " ", "_"),
            "_",
            if (include_player) "with_player" else "base",
            ".rds"
          )
        )
      }
      if (!is.null(checkpoint_path)) {
        checkpoint_path <- sub(
          "\\.rds$",
          "_gam_ML.rds",
          checkpoint_path
        )
      }
      checkpoint_model(
        checkpoint_path,
        checkpoint_signature(
          "player_explanatory",
          "gam",
          "ML",
          outcome,
          include_player,
          k,
          gam.control(maxit = 100),
          paired_data
        ),
        function() {
          model_formula <- formula
          fit_smooth_model(
            formula = model_formula,
            data = paired_data,
            family = family,
            engine = "gam",
            method = "ML",
            discrete = FALSE,
            control = gam.control(maxit = 100)
          )
        }
      )
    },
    workers = workers,
    labels = paste(outcome, names(specifications)),
    report_progress = report_progress
  ) |>
    rlang::set_names(names(specifications))

  base_model <- fits[["base"]]
  player_model <- fits[["base + player movement"]]
  comparison <- anova(base_model, player_model, test = "Chisq") |>
    as.data.frame()
  p_column <- grep("^Pr\\(", names(comparison), value = TRUE)
  p_value <- if (length(p_column) == 1L) {
    comparison[[p_column]][[nrow(comparison)]]
  } else {
    NA_real_
  }
  base_deviance <- comparison$`Resid. Dev`[[1]]
  player_deviance <- comparison$`Resid. Dev`[[2]]
  likelihood_ratio_df <- comparison$Df[[nrow(comparison)]]
  likelihood_ratio_deviance <- comparison$Deviance[[nrow(comparison)]]

  tibble(
    outcome = outcome,
    engine = "gam",
    method = "ML",
    discrete = FALSE,
    comparison = "Base vs base + half-specific player-movement smooths",
    base_AIC = AIC(base_model),
    player_AIC = AIC(player_model),
    delta_AIC_player_vs_base = AIC(player_model) - AIC(base_model),
    base_deviance = base_deviance,
    player_deviance = player_deviance,
    incremental_deviance_explained = (base_deviance - player_deviance) /
      base_deviance,
    likelihood_ratio_df = likelihood_ratio_df,
    likelihood_ratio_deviance = likelihood_ratio_deviance,
    likelihood_ratio_p_value = p_value,
    inference_note = paste(
      "ML comparison; chi-squared p-value is approximate for penalized smooths.",
      "Incremental deviance explained is the reduction in residual deviance",
      "relative to the base model."
    )
  )
}

linear_mean_formula <- function(
  kind = c("common", "deviation", "independent")
) {
  kind <- match.arg(kind)
  base <- "Distance ~ Role * Half"
  random <- paste0(
    " + s(subject_id, bs = 're')",
    " + s(Time_half, subject_id, bs = 'fs', k = 4, m = 1)",
    " + s(Match, bs = 're')"
  )
  time_terms <- switch(
    kind,
    common = " + Time_half:H1 + Time_half:H2",
    deviation = paste0(
      " + Time_half:H1 + Time_half:H2",
      " + Time_half:RefH1 + Time_half:RefH2"
    ),
    independent = paste0(
      " + Time_half:PlayerH1 + Time_half:RefH1",
      " + Time_half:PlayerH2 + Time_half:RefH2"
    )
  )
  as.formula(paste0(base, time_terms, random))
}

linearity_sensitivity <- function(
  data,
  outcome,
  kind,
  workers = 1L,
  checkpoint_dir = NULL
) {
  specifications <- list(
    `half-specific linear` = list(
      formula = linear_mean_formula(kind),
      method = "ML",
      checkpoint_name = "half_specific_linear"
    ),
    `half-specific smooth` = list(
      formula = mean_formula(kind, k = 5),
      method = "ML",
      checkpoint_name = "half_specific_smooth"
    )
  )
  # ML is used because the linear and smooth models have different fixed effects.
  fits <- run_parallel_fits(
    specifications,
    function(specification) {
      fit_model(
        data,
        outcome,
        specification$formula,
        engine = "gam",
        method = specification$method,
        discrete = FALSE,
        checkpoint_path = if (is.null(checkpoint_dir)) {
          NULL
        } else {
          file.path(
            checkpoint_dir,
            paste0(
              str_replace_all(tolower(outcome), " ", "_"),
              "_",
              specification$checkpoint_name,
              ".rds"
            )
          )
        }
      )
    },
    workers = workers,
    labels = paste(outcome, names(specifications))
  )
  tibble(
    model = names(specifications),
    AIC = purrr::map_dbl(fits, AIC)
  ) |>
    mutate(
      delta_AIC = AIC - min(AIC),
      akaike_weight = exp(-0.5 * delta_AIC) / sum(exp(-0.5 * delta_AIC)),
      evidence_ratio_vs_best = max(akaike_weight) / akaike_weight,
      outcome = outcome,
      selected_structure = kind,
      engine = "gam",
      method = "ML",
      discrete = FALSE,
      .before = 1
    )
}

basis_sensitivity <- function(
  data,
  outcome,
  kind,
  fitted_k = 5,
  doubled_k = 9,
  workers = 1L,
  checkpoint_dir = NULL
) {
  basis_dimensions <- c(3L, fitted_k, doubled_k)
  # Compare a constrained basis, the fitted basis, and the largest useful k.
  # There are nine distinct epochs per half, so nine is the largest useful k.
  fits <- run_parallel_fits(
    basis_dimensions,
    function(k) {
      fit_model(
        data,
        outcome,
        mean_formula(kind, k = k),
        checkpoint_path = if (is.null(checkpoint_dir)) {
          NULL
        } else {
          file.path(
            checkpoint_dir,
            paste0(
              str_replace_all(tolower(outcome), " ", "_"),
              "_k_",
              k,
              ".rds"
            )
          )
        }
      )
    },
    workers = workers,
    labels = paste(outcome, "k =", basis_dimensions)
  )

  if (!is.null(checkpoint_dir)) {
    walk2(fits, basis_dimensions, function(fit, k) {
      output <- capture_gam_check(fit)
      writeLines(
        output,
        file.path(
          checkpoint_dir,
          paste0(
            str_replace_all(tolower(outcome), " ", "_"),
            "_k_",
            k,
            "_gam_check.txt"
          )
        )
      )
    })
  }

  purrr::map2_dfr(fits, basis_dimensions, function(fit, k) {
    k_diagnostics(fit) |>
      mutate(k = k)
  }) |>
    mutate(
      outcome = outcome,
      selected_structure = kind,
      engine = "bam",
      method = "fREML",
      discrete = TRUE,
      .before = 1
    )
}

model_evidence <- function(models) {
  tibble(
    model = names(models),
    AIC = map_dbl(models, AIC)
  ) |>
    mutate(
      delta_AIC = AIC - min(AIC),
      akaike_weight = exp(-0.5 * delta_AIC) / sum(exp(-0.5 * delta_AIC)),
      evidence_ratio_vs_best = max(akaike_weight) / akaike_weight
    ) |>
    arrange(AIC)
}

select_final_model <- function(models) {
  evidence <- model_evidence(models)
  models[[evidence$model[[1]]]]
}

prediction_grid <- function(data) {
  crossing(
    Role = factor(c("Player", "Referee"), levels = levels(data$Role)),
    Half = factor(c("First", "Second"), levels = levels(data$Half)),
    # Add half-boundary extrapolation without changing the contrast time grid.
    Time_half = c(2.5, seq(5, 45, by = 1), 47.5)
  ) |>
    mutate(
      Time = if_else(Half == "First", Time_half, Time_half + 45),
      H1 = as.numeric(Half == "First"),
      H2 = as.numeric(Half == "Second"),
      RefH1 = as.numeric(Role == "Referee" & Half == "First"),
      RefH2 = as.numeric(Role == "Referee" & Half == "Second"),
      PlayerH1 = as.numeric(Role == "Player" & Half == "First"),
      PlayerH2 = as.numeric(Role == "Player" & Half == "Second"),
      subject_id = factor(
        levels(data$subject_id)[[1]],
        levels = levels(data$subject_id)
      ),
      Match = factor(levels(data$Match)[[1]], levels = levels(data$Match))
    )
}

predict_population <- function(model, data, outcome) {
  grid <- prediction_grid(data)
  random_terms <- c("s(subject_id)", "s(Time_half,subject_id)", "s(Match)")
  prediction <- predict(
    model,
    newdata = grid,
    type = "link",
    se.fit = TRUE,
    exclude = random_terms
  )

  fit_link <- as.numeric(prediction$fit)
  se_link <- as.numeric(prediction$se.fit)

  grid |>
    mutate(
      estimate_link = fit_link,
      variance_link = se_link^2,
      estimate = exp(estimate_link),
      lower = exp(estimate_link - z_975 * se_link),
      upper = exp(estimate_link + z_975 * se_link),
      outcome = outcome
    )
}

role_contrasts <- function(model, data, outcome) {
  grid <- prediction_grid(data) |>
    filter(Time_half %in% seq(5, 45, 5))
  random_terms <- c("s(subject_id)", "s(Time_half,subject_id)", "s(Match)")
  design <- predict(
    model,
    newdata = grid,
    type = "lpmatrix",
    exclude = random_terms
  )

  index <- grid |>
    mutate(row = row_number()) |>
    select(row, Role, Half, Time_half, Time)
  pairs <- index |>
    distinct(Half, Time_half, Time) |>
    arrange(Half, Time_half)

  contrast_rows <- map_dfr(seq_len(nrow(pairs)), function(i) {
    key <- pairs[i, ]
    player_row <- index |>
      filter(Half == key$Half, Time_half == key$Time_half, Role == "Player") |>
      pull(row)
    referee_row <- index |>
      filter(Half == key$Half, Time_half == key$Time_half, Role == "Referee") |>
      pull(row)
    difference <- design[referee_row, ] - design[player_row, ]
    estimate <- drop(difference %*% coef(model))
    variance <- drop(difference %*% vcov(model) %*% difference)
    tibble(
      Half = key$Half,
      Time_half = key$Time_half,
      Time = key$Time,
      estimate_link = estimate,
      variance_link = variance,
      ratio = exp(estimate),
      lower = exp(estimate - z_975 * sqrt(variance)),
      upper = exp(estimate + z_975 * sqrt(variance)),
      outcome = outcome
    )
  })

  contrast_rows
}

extract_model_results <- function(
  model,
  outcome,
  source,
  replicate = NA_integer_
) {
  parametric <- broom::tidy(model, parametric = TRUE) |>
    transmute(
      source,
      replicate,
      outcome,
      component = "parametric",
      term,
      estimate,
      std.error,
      statistic,
      p.value
    )

  smooth_table <- as.data.frame(summary(model)$s.table) |>
    rownames_to_column("term") |>
    as_tibble()
  statistic_column <- setdiff(
    names(smooth_table),
    c("term", "edf", "Ref.df", "p-value")
  )[[1]]
  p_value_column <- "p-value"
  smooth <- smooth_table |>
    transmute(
      source,
      replicate,
      outcome,
      component = "smooth",
      term,
      estimate = .data[["edf"]],
      std.error = NA_real_,
      statistic = .data[[statistic_column]],
      p.value = .data[[p_value_column]]
    )

  bind_rows(parametric, smooth)
}

within_series_acf <- function(model, data, outcome, lag_max = 5) {
  outcome_data <- data |>
    filter(Variable == outcome) |>
    arrange(Match, Role, Time) |>
    mutate(residual = as.numeric(residuals(model, type = "deviance")))

  outcome_data |>
    group_by(Match, Role) |>
    group_modify(function(.x, .y) {
      acf_values <- acf(
        .x$residual,
        lag.max = lag_max,
        plot = FALSE,
        na.action = na.pass
      )
      tibble(lag = seq_len(lag_max), acf = as.numeric(acf_values$acf)[-1])
    }) |>
    ungroup()
}

k_diagnostics <- function(model, seed = 7183, n_rep = 400) {
  # k.check() simulates; seed it, then restore the caller's RNG state.
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(
      assign(".Random.seed", old_seed, envir = globalenv()),
      add = TRUE
    )
  } else {
    on.exit(
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        rm(".Random.seed", envir = globalenv())
      },
      add = TRUE
    )
  }
  set.seed(seed)
  as.data.frame(k.check(model, n.rep = n_rep)) |>
    rownames_to_column("term") |>
    as_tibble()
}

capture_gam_check <- function(model) {
  diagnostic_plot <- tempfile(fileext = ".pdf")
  grDevices::pdf(diagnostic_plot)
  on.exit(
    {
      grDevices::dev.off()
      unlink(diagnostic_plot)
    },
    add = TRUE
  )

  invisible(capture.output(mgcv::gam.check(model, k.rep = 400, rep = 0)))
}

variance_components <- function(model, source, replicate = NA_integer_) {
  variance <- gam.vcomp(model)
  variance_table <- if (is.list(variance) && !is.null(variance$vc)) {
    variance$vc
  } else {
    variance
  }
  as.data.frame(variance_table) |>
    rownames_to_column("term") |>
    as_tibble() |>
    mutate(source = source, replicate = replicate, .before = 1)
}

response_ratio_contrast <- function(model, data, n = 250L) {
  grid <- crossing(
    Role = factor(c("Player", "Referee"), levels = levels(data$Role)),
    Half = factor(c("First", "Second"), levels = levels(data$Half)),
    Time_half = seq(2.5, 47.5, length.out = n)
  ) |>
    mutate(
      Time = if_else(Half == "First", Time_half, Time_half + 45),
      H1 = as.numeric(Half == "First"),
      H2 = as.numeric(Half == "Second"),
      RefH1 = as.numeric(Role == "Referee" & Half == "First"),
      RefH2 = as.numeric(Role == "Referee" & Half == "Second"),
      PlayerH1 = as.numeric(Role == "Player" & Half == "First"),
      PlayerH2 = as.numeric(Role == "Player" & Half == "Second"),
      subject_id = factor(
        levels(data$subject_id)[[1]],
        levels = levels(data$subject_id)
      ),
      Match = factor(levels(data$Match)[[1]], levels = levels(data$Match))
    )
  design <- predict(
    model,
    newdata = grid,
    type = "lpmatrix",
    exclude = c("s(subject_id)", "s(Time_half,subject_id)", "s(Match)")
  )
  covariance <- vcov(model)
  key <- paste(grid$Half, grid$Time_half)
  referee <- which(grid$Role == "Referee")
  player <- match(key[referee], key[grid$Role == "Player"]) |>
    (\(i) which(grid$Role == "Player")[i])()
  difference <- design[referee, , drop = FALSE] - design[player, , drop = FALSE]
  log_ratio <- drop(difference %*% coef(model))
  standard_error <- sqrt(rowSums((difference %*% covariance) * difference))

  grid[referee, ] |>
    transmute(
      Half,
      Time_half,
      Time,
      Time_midpoint = Time - 2.5,
      estimate = 100 * (exp(log_ratio) - 1),
      lower = 100 * (exp(log_ratio - z_975 * standard_error) - 1),
      upper = 100 * (exp(log_ratio + z_975 * standard_error) - 1),
      statistically_different = if_else(lower > 0 | upper < 0, "Yes", "No")
    )
}

wide_synthesis_input <- function(wide) {
  wide |>
    mutate(id = factor(id)) |>
    select(-Match) |>
    as.data.frame()
}

synthesise_hierarchy <- function(wide, m = 10, seed = 7183) {
  input <- wide_synthesis_input(wide)
  structure <- syn(input, m = 0, print.flag = FALSE)
  methods <- structure$method
  methods["id"] <- "sample"
  methods[names(methods) != "id"] <- "cart"
  predictors <- structure$predictor.matrix
  predictors[, "id"] <- 1
  predictors["id", ] <- 0
  diag(predictors) <- 0

  syn(
    input,
    m = m,
    method = methods,
    predictor.matrix = predictors,
    visit.sequence = names(input),
    seed = seed,
    proper = TRUE,
    print.flag = TRUE
  )
}

synthesis_to_long <- function(synthesis) {
  map2_dfr(synthesis$syn, seq_along(synthesis$syn), function(data, replicate) {
    data |>
      mutate(Match = row_number(), .before = 1) |>
      prepare_analysis_data() |>
      mutate(replicate = replicate)
  })
}

synthesise_legacy <- function(long, m = 10, seed = 7183) {
  input <- long |>
    transmute(
      Match = as.integer(Match),
      id,
      Role,
      Time,
      Variable,
      Distance
    ) |>
    as.data.frame()
  structure <- syn(input, m = 0, print.flag = FALSE)
  methods <- structure$method
  methods[c("Match", "id")] <- "sample"
  methods[c("Role", "Time", "Variable", "Distance")] <- "cart"
  predictors <- structure$predictor.matrix
  predictors[, c("Match", "id")] <- 0

  syn(
    input,
    m = m,
    method = methods,
    predictor.matrix = predictors,
    seed = seed + 1,
    proper = TRUE,
    print.flag = TRUE
  )
}

utility_summary <- function(original, synthetic) {
  bind_rows(
    original |>
      mutate(dataset = "Original"),
    synthetic |>
      mutate(dataset = "Synthetic")
  ) |>
    summarise(
      n = n(),
      mean = mean(Distance),
      sd = sd(Distance),
      median = median(Distance),
      q10 = quantile(Distance, 0.1),
      q90 = quantile(Distance, 0.9),
      zero_fraction = mean(Distance == 0),
      .by = c(dataset, Variable, Role)
    )
}

fully_synthetic_pool <- function(estimates, m = nrow(estimates)) {
  if (nrow(estimates) < 2) {
    stop("At least two successful synthetic estimates are required.")
  }
  q_bar <- mean(estimates$estimate)
  within <- mean(estimates$variance)
  between <- var(estimates$estimate)
  total_raw <- (1 + 1 / m) * between - within
  # A negative estimate can occur when synthesis variability is very small.
  total <- max(total_raw, within / m, .Machine$double.eps)
  df_raw <- if (between > 0) {
    (m - 1) * (1 - within / ((1 + 1 / m) * between))^2
  } else {
    Inf
  }
  df <- if (is.finite(df_raw)) max(df_raw, 1) else Inf
  critical <- if (is.finite(df)) qt(0.975, df = df) else z_975

  tibble(
    estimate = q_bar,
    within_variance = within,
    between_variance = between,
    total_variance_raw = total_raw,
    total_variance = total,
    df = df,
    lower = q_bar - critical * sqrt(total),
    upper = q_bar + critical * sqrt(total),
    p_value = 2 * pt(-abs(q_bar / sqrt(total)), df = df)
  )
}

pool_prediction_grid <- function(predictions) {
  predictions |>
    group_by(outcome, Role, Half, Time_half, Time) |>
    group_modify(function(.x, .y) {
      pooled <- fully_synthetic_pool(
        transmute(.x, estimate = estimate_link, variance = variance_link),
        m = n_distinct(.x$replicate)
      )
      pooled |>
        mutate(
          estimate_response = exp(estimate),
          lower_response = exp(lower),
          upper_response = exp(upper)
        )
    }) |>
    ungroup()
}

submission_figure <- function(raw, predictions, model, outcome) {
  player_colour <- "#FEE715"
  referee_colour <- "#101820"
  role_colours <- c(Player = player_colour, Referee = referee_colour)
  role_labels <- c(Player = "Players", Referee = "Referee")
  role_labeller <- as_labeller(role_labels)
  # Five-minute epochs are plotted at their midpoints; the half-time gap
  # gives each side its own tick labelled 45 minutes.
  halftime_gap <- 10
  time_breaks <- c(
    0,
    15,
    30,
    45,
    45 + halftime_gap,
    60 + halftime_gap,
    75 + halftime_gap,
    90 + halftime_gap
  )
  time_labels <- c(0, 15, 30, 45, 45, 60, 75, 90)
  plot_data <- raw |>
    filter(Variable == .env$outcome) |>
    mutate(
      Time_midpoint = Time - 2.5,
      Time_plot = if_else(
        Half == "Second",
        Time_midpoint + halftime_gap,
        Time_midpoint
      )
    )
  prediction_data <- predictions |>
    filter(.data$outcome == .env$outcome) |>
    mutate(
      Time_midpoint = Time - 2.5,
      Time_plot = if_else(
        Half == "Second",
        Time_midpoint + halftime_gap,
        Time_midpoint
      )
    ) |>
    arrange(Role, Half, Time_plot)
  is_low_speed <- outcome == "Low Speed Activity"
  histogram_x_limit <- if (is_low_speed) c(0, 160) else c(0, 40)
  raw_y_limit <- if (is_low_speed) c(0, 160) else c(0, 40)
  model_y_limit <- if (is_low_speed) c(95, 125) else c(7, 13)
  histogram_binwidth <- if (is_low_speed) 6 else 2
  halftime_axis <- list(
    annotate(
      "segment",
      x = 0,
      xend = 45,
      y = -Inf,
      yend = -Inf,
      linewidth = 0.5
    ),
    annotate(
      "segment",
      x = 45 + halftime_gap,
      xend = 90 + halftime_gap,
      y = -Inf,
      yend = -Inf,
      linewidth = 0.5
    ),
    theme(axis.line.x = element_blank())
  )

  raw_panel <- ggplot(plot_data, aes(Time_plot, Distance, colour = Role)) +
    geom_jitter(alpha = 0.2) +
    stat_summary(
      aes(group = Role),
      fun = mean,
      geom = "point",
      shape = 23,
      size = 3.5,
      colour = "darkred",
      fill = "white"
    ) +
    facet_wrap(vars(Role), labeller = role_labeller) +
    scale_colour_manual(values = role_colours) +
    scale_x_continuous(breaks = time_breaks, labels = time_labels) +
    coord_cartesian(xlim = c(0, 90 + halftime_gap), ylim = raw_y_limit) +
    labs(x = "Elapsed Match Time (minutes)", y = "Metres/min") +
    halftime_axis +
    theme(legend.position = "none")

  histogram_panel <- ggplot(plot_data, aes(Distance)) +
    geom_histogram(
      aes(fill = Role, colour = Role),
      binwidth = histogram_binwidth,
      position = "identity"
    ) +
    # Referee (black) fill is more transparent so the overlapping players show through.
    scale_fill_manual(
      values = c(
        Player = scales::alpha(player_colour, 0.8),
        Referee = scales::alpha(referee_colour, 0.4)
      ),
      labels = role_labels,
      name = NULL
    ) +
    scale_colour_manual(
      values = role_colours,
      labels = role_labels,
      name = NULL
    ) +
    coord_cartesian(xlim = histogram_x_limit) +
    labs(x = "Metres/min", y = "Count") +
    theme(
      axis.line.x = element_line(),
      legend.position = "inside",
      legend.position.inside = c(0.95, 0.7),
      legend.justification = c(1, 1),
      legend.text = element_text(size = 14)
    )

  model_panel <- ggplot(
    prediction_data,
    aes(Time_plot, estimate, colour = Role, fill = Role)
  ) +
    geom_ribbon(
      aes(ymin = lower, ymax = upper, group = interaction(Role, Half)),
      alpha = 0.2,
      colour = "black",
      linewidth = 0.5,
      linetype = "dashed"
    ) +
    geom_line(aes(group = interaction(Role, Half)), linewidth = 1.1) +
    scale_colour_manual(
      values = role_colours,
      labels = role_labels,
      name = NULL
    ) +
    scale_fill_manual(
      values = role_colours,
      labels = role_labels,
      name = NULL
    ) +
    scale_x_continuous(breaks = time_breaks, labels = time_labels) +
    coord_cartesian(
      xlim = c(0, 90 + halftime_gap),
      ylim = model_y_limit
    ) +
    labs(x = "Elapsed Match Time (minutes)", y = "Metres/min") +
    halftime_axis +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.5, 0.99),
      legend.justification = c(0.5, 1),
      legend.direction = "horizontal",
      legend.text = element_text(size = 14)
    )

  figure <- (raw_panel + histogram_panel) /
    (model_panel + plot_spacer())

  figure +
    plot_annotation(tag_levels = "A") &
    theme(
      panel.border = element_blank(),
      axis.line.y = element_line(),
      strip.text = element_text(size = 16, colour = "black", face = "bold"),
      strip.background = element_rect(
        colour = "black",
        fill = "cornsilk",
        linewidth = 1
      ),
      plot.tag = element_text(face = "bold", size = 14),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14, colour = "black")
    )
}

overlay_ratio_figure <- function(raw, fits) {
  halftime_gap <- 10
  time_breaks <- c(0, 15, 30, 45, 55, 70, 85, 100)
  time_labels <- c(0, 15, 30, 45, 45, 60, 75, 90)
  outcome_colours <- c(
    "Low Speed Activity" = "#6BAED6",
    "High Speed Activity" = "#F4777F"
  )
  outcome_labels <- c(
    "Low Speed Activity" = "Low speed",
    "High Speed Activity" = "High speed"
  )
  ratio_data <- imap(fits, function(fit, outcome) {
    response_ratio_contrast(fit$final, raw |> filter(Variable == outcome)) |>
      mutate(outcome = outcome)
  }) |>
    list_rbind() |>
    mutate(
      outcome = factor(outcome, levels = names(outcome_colours)),
      Time_plot = if_else(
        Half == "Second",
        Time_midpoint + halftime_gap,
        Time_midpoint
      )
    )

  ggplot(
    ratio_data,
    aes(Time_plot, estimate, group = interaction(outcome, Half))
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
    geom_ribbon(
      aes(ymin = lower, ymax = upper, fill = outcome),
      alpha = 0.5
    ) +
    geom_line(aes(colour = outcome), linewidth = 1.4) +
    scale_fill_manual(
      values = outcome_colours,
      labels = outcome_labels,
      name = NULL
    ) +
    scale_colour_manual(
      values = colorspace_darken(outcome_colours),
      labels = outcome_labels,
      name = NULL
    ) +
    scale_x_continuous(breaks = time_breaks, labels = time_labels) +
    scale_y_continuous(labels = \(x) paste0(x, "%")) +
    coord_cartesian(xlim = c(0, 90 + halftime_gap)) +
    annotate(
      "segment",
      x = 0,
      xend = 45,
      y = -Inf,
      yend = -Inf,
      linewidth = 0.5
    ) +
    annotate(
      "segment",
      x = 45 + halftime_gap,
      xend = 90 + halftime_gap,
      y = -Inf,
      yend = -Inf,
      linewidth = 0.5
    ) +
    labs(
      x = "Elapsed Match Time (minutes)",
      y = "Referee vs Player difference (%)"
    ) +
    theme(
      axis.line.x = element_blank(),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14, colour = "black"),
      legend.position = "inside",
      legend.position.inside = c(0.98, 0.02),
      legend.justification = c(1, 0),
      legend.text = element_text(size = 14)
    )
}

colorspace_darken <- function(colours, amount = 0.35) {
  rgb_values <- grDevices::col2rgb(colours) / 255
  darkened <- rgb_values * (1 - amount)
  setNames(
    grDevices::rgb(darkened[1, ], darkened[2, ], darkened[3, ]),
    names(colours)
  )
}

plot_trajectory <- function(raw, predictions, outcome, title) {
  raw_plot <- raw |>
    filter(Variable == outcome) |>
    mutate(Time_midpoint = Time - 2.5)
  prediction_plot <- predictions |>
    mutate(Time_midpoint = Time - 2.5)
  axis_limits <- tibble(
    Half = factor(
      c("First", "First", "Second", "Second"),
      levels = levels(raw_plot$Half)
    ),
    Time_midpoint = c(0, 45, 45, 90)
  )

  ggplot(raw_plot, aes(Time_midpoint, Distance, colour = Role)) +
    # Train each half to its full interval while plotting bins at their midpoints.
    geom_blank(
      data = axis_limits,
      aes(x = Time_midpoint),
      inherit.aes = FALSE
    ) +
    geom_point(
      alpha = 0.08,
      position = position_jitter(width = 0.7, height = 0)
    ) +
    geom_ribbon(
      data = prediction_plot,
      aes(x = Time_midpoint, ymin = lower, ymax = upper, fill = Role),
      inherit.aes = FALSE,
      alpha = 0.18,
      colour = NA
    ) +
    geom_line(
      data = prediction_plot,
      aes(x = Time_midpoint, y = estimate, group = Role),
      linewidth = 0.9
    ) +
    # Separate panels create a visible axis gap at the half-time break.
    facet_grid(
      cols = vars(Half),
      scales = "free_x",
      space = "free_x",
      labeller = as_labeller(c(First = "First half", Second = "Second half"))
    ) +
    scale_x_continuous(
      breaks = c(0, 25, 45, 70, 90),
      expand = expansion(mult = 0)
    ) +
    scale_colour_manual(
      values = c(Player = "#101820", Referee = "#FEE715")
    ) +
    scale_fill_manual(
      values = c(Player = "#101820", Referee = "#FEE715")
    ) +
    labs(
      x = "Elapsed match time (minutes)",
      y = "Distance (m/min)",
      title = title
    ) +
    theme(
      panel.spacing.x = unit(1.2, "lines"),
      strip.text = element_blank(),
      strip.background = element_blank()
    )
}

save_diagnostics <- function(model, data, outcome, prefix, save = TRUE) {
  acf_data <- within_series_acf(model, data, outcome)
  if (save) {
    write_csv(acf_data, paste0(prefix, "_within_series_acf.csv"))
    write_csv(k_diagnostics(model), paste0(prefix, "_k_check.csv"))
  }

  acf_plot <- ggplot(acf_data, aes(factor(lag), acf)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Within-series lag",
      y = "Residual autocorrelation",
      title = paste("Residual ACF:", outcome)
    )
  print(acf_plot)
  if (save) {
    ggsave(
      paste0(prefix, "_acf.png"),
      acf_plot,
      width = 16,
      height = 10,
      units = "cm",
      dpi = 300
    )
  }

  # Simulated quantile residuals are informative for non-Gaussian families.
  qq_plot <- gratia::qq_plot(
    model,
    method = "simulate",
    n_simulate = 100,
    seed = seed_synthesis
  )
  print(qq_plot)
  if (save) {
    ggsave(
      paste0(prefix, "_simulated_qq.png"),
      qq_plot,
      width = 16,
      height = 10,
      units = "cm",
      dpi = 300
    )
  }
}

# Step 1: prepare one dataset for modelling.
#
# prep_dataset() is the single entry point for the model-building step. It
# accepts any of:
#   * the restricted original workbook (.xlsx, wide format)
#   * a long-format synthetic CSV with a `replicate` column (e.g.
#     osf/data.syn.csv); `replicate` selects which one
#   * a single-replicate CSV with no `replicate` column (e.g. osf/data.syn.csv)
# and returns a "prepared_dataset": the analysis-ready long data plus the
# metadata that identifies it. Nothing is read from or written to globals.
# Requires the helper modules (R/analysis_helpers.R) to be sourced.

new_prepared_dataset <- function(
  data,
  source,
  replicate = NA_integer_,
  input_path = NA_character_,
  seed = NA_real_
) {
  structure(
    list(
      data = data,
      source = source,
      replicate = as.integer(replicate),
      metadata = list(
        input_path = input_path,
        input_md5 = if (!is.na(input_path) && file.exists(input_path)) {
          unname(tools::md5sum(input_path))
        } else {
          NA_character_
        },
        seed = seed,
        prepared_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
        r_version = R.version.string,
        package_versions = vapply(
          c("mgcv", "gratia", "synthpop", "dplyr", "tidyr"),
          function(pkg) as.character(utils::packageVersion(pkg)),
          character(1)
        )
      )
    ),
    class = "prepared_dataset"
  )
}

# Step 1a: synthesise datasets from the restricted original.
#
# prep_synthetic() synthesises `m` hierarchy-aware replicates and writes one
# prepared_dataset .rds per replicate (same schema as prep_dataset()), the
# public long-format CSVs, and the utility measures. It does not fit models.
# The original data are never written to `output_dir`.
# Requires the helper modules (R/analysis_helpers.R) and R/prep_dataset.R.

generate_valid_synthesis <- function(
  wide,
  m,
  seed,
  max_attempts = 20L
) {
  for (attempt in 0:(max_attempts - 1L)) {
    attempt_seed <- seed + attempt
    synthesis <- synthesise_hierarchy(wide, m = m, seed = attempt_seed)
    synthetic_all <- synthesis_to_long(synthesis)
    invalid <- synthetic_all |>
      summarise(
        invalid = anyNA(Distance) |
          any(Distance < 0, na.rm = TRUE) |
          any(Variable == "Low Speed Activity" & Distance <= 0, na.rm = TRUE),
        .by = replicate
      ) |>
      filter(invalid) |>
      pull(replicate)
    if (length(invalid) == 0L) {
      message("Using valid hierarchy-aware synthesis seed: ", attempt_seed)
      return(list(synthetic_all = synthetic_all, seed = attempt_seed))
    }
    message(
      "Retrying synthesis: invalid replicate(s) ",
      paste(invalid, collapse = ", "),
      " with seed ",
      attempt_seed + 1L
    )
  }
  stop(
    "Could not generate valid synthetic replicates in ",
    max_attempts,
    " attempts."
  )
}

synthesis_utility <- function(original, synthetic_all, m, seed) {
  legacy <- synthesise_legacy(original, m = m, seed = seed)
  legacy_utility <- map2_dfr(
    legacy$syn,
    seq_along(legacy$syn),
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
  primary_utility <- map_dfr(seq_len(m), function(replicate) {
    utility_summary(
      original,
      filter(synthetic_all, replicate == !!replicate)
    ) |>
      mutate(replicate = replicate, synthesis_method = "Hierarchy-aware wide")
  })

  simple <- function(x) {
    as.data.frame(transmute(x, Role, Time, Variable, Distance))
  }
  general <- map(seq_len(m), function(replicate) {
    utility.gen(
      simple(filter(synthetic_all, replicate == !!replicate)),
      simple(original),
      method = "logit"
    )
  })
  list(
    summary = bind_rows(primary_utility, legacy_utility),
    general = general
  )
}

# Step 2: build and save the models for ONE prepared dataset.
#
#   prepared <- prep_dataset("data.orginal.xlsx")            # or a synthetic
#   prepared <- prep_dataset("osf/data.syn.csv", replicate = 3)
#   build_models(prepared, output_dir = "outputs/.../replicate_03")
#
# The same function handles the original data and every synthetic replicate.
# It writes `<output_dir>/models.rds`, a "model_bundle" holding the data, all
# fitted models, the sensitivity analyses, the player-movement analyses and the
# metadata needed to audit the run. Step 3 (R/run_analysis.R) reads only this.
#
# Checkpointing is opt-in: set `checkpoint_dir` to cache individual fits. Each
# checkpoint stores a signature that includes the data and model specification,
# so a stale fit is refitted rather than reused. With the default (NULL) every
# run fits from scratch.
# Requires the helper modules (R/analysis_helpers.R).

add_player_distance <- function(data, referee, player = referee) {
  players <- data |>
    filter(Variable == player, Role == "Player") |>
    select(Match, id, Time, Half, pd = Distance)
  data |>
    filter(Variable == referee) |>
    left_join(players, by = c("Match", "id", "Time", "Half")) |>
    mutate(player_distance = if_else(Role == "Referee", pd, 0)) |>
    select(-pd)
}

fit_deviance_bam <- function(formula, data, power) {
  suppressWarnings(mgcv::bam(
    formula,
    data = data,
    family = mgcv::Tweedie(p = power, link = "log"),
    method = "fREML",
    discrete = TRUE
  ))
}

referee_deviance <- function(model, data) {
  sum(residuals(model, type = "deviance")[data$Role == "Referee"]^2)
}

player_deviance_formula <- function(base) {
  update(
    base,
    . ~ . +
      s(player_distance, by = RefH1, bs = "cr", k = 5, id = 5) +
      s(player_distance, by = RefH2, bs = "cr", k = 5, id = 6)
  )
}

# Joint Wald test of the player smooths (approximate for penalised terms)
player_deviance_wald <- function(model) {
  idx <- unlist(purrr::map(
    model$smooth,
    \(s) if (grepl("player_distance", s$label)) s$first.para:s$last.para
  ))
  b <- coef(model)[idx]
  stat <- drop(t(b) %*% MASS::ginv(model$Vp[idx, idx]) %*% b)
  df <- sum(model$edf[idx])
  c(wald = stat, edf = df, p = pchisq(stat, df, lower.tail = FALSE))
}

player_deviance_estimate <- function(data, base, power, wald = FALSE) {
  m0 <- fit_deviance_bam(base, data, power)
  m1 <- fit_deviance_bam(player_deviance_formula(base), data, power)
  d0 <- referee_deviance(m0, data)
  d1 <- referee_deviance(m1, data)
  out <- c(
    null = d0,
    full = d1,
    reduction = d0 - d1,
    pct = 100 * (d0 - d1) / d0
  )
  if (wald) {
    out <- c(out, player_deviance_wald(m1))
  }
  out
}

# Matches are nested in referees, so whole referees are resampled.
resample_referees <- function(data) {
  ids <- unique(as.character(data$id))
  draw <- sample(ids, length(ids), replace = TRUE)
  purrr::map_dfr(seq_along(draw), \(i) {
    data |>
      filter(id == draw[i]) |>
      mutate(
        Match = factor(paste(i, Match, sep = "_")),
        subject_id = factor(paste(i, subject_id, sep = "_"))
      )
  })
}

# Referee-movement deviance reduction from adding player movement, using each
# outcome's selected final model. `pairs = "same"` compares like with like
# (High/High, Low/Low); "all" also crosses speeds. `n_boot = 0` gives point
# estimates only; otherwise referee-level cluster bootstrap intervals.
player_deviance_analysis <- function(
  data,
  fits,
  pairs = c("same", "all"),
  n_boot = 0L,
  workers = 1L,
  seed = NULL
) {
  pairs <- match.arg(pairs)
  outcomes <- levels(data$Variable)
  powers <- purrr::map_dbl(rlang::set_names(outcomes), \(o) {
    as.numeric(sub(
      "^Tweedie\\(p=([0-9.]+)\\)$",
      "\\1",
      family(fits[[o]]$final)$family
    ))
  })
  bases <- purrr::map(rlang::set_names(outcomes), \(o) formula(fits[[o]]$final))
  grid <- if (pairs == "same") {
    tibble(referee = outcomes, player = outcomes)
  } else {
    tidyr::expand_grid(referee = outcomes, player = outcomes)
  }

  if (n_boot > 0L && workers > 1L) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = workers)
  }

  purrr::pmap_dfr(grid, function(referee, player) {
    paired <- add_player_distance(data, referee, player)
    stopifnot(!anyNA(paired$player_distance))
    base <- bases[[referee]]
    power <- powers[[referee]]
    point <- player_deviance_estimate(paired, base, power, wald = TRUE)

    boot <- NULL
    if (n_boot > 0L) {
      boot <- future.apply::future_sapply(
        seq_len(n_boot),
        \(b) {
          tryCatch(
            player_deviance_estimate(
              resample_referees(paired),
              base,
              power
            )[c("reduction", "pct")],
            error = \(e) c(reduction = NA_real_, pct = NA_real_)
          )
        },
        future.seed = seed %||% TRUE,
        future.packages = c("mgcv", "tidyverse")
      )
    }
    reduction_ci <- if (is.null(boot)) {
      c(NA_real_, NA_real_)
    } else {
      quantile(boot["reduction", ], c(0.025, 0.975), na.rm = TRUE)
    }
    pct_ci <- if (is.null(boot)) {
      c(NA_real_, NA_real_)
    } else {
      quantile(boot["pct", ], c(0.025, 0.975), na.rm = TRUE)
    }
    tibble(
      referee_outcome = referee,
      player_outcome = player,
      null_dev = point[["null"]],
      full_dev = point[["full"]],
      reduction = point[["reduction"]],
      reduction_lower = unname(reduction_ci[1]),
      reduction_upper = unname(reduction_ci[2]),
      pct_reduction = point[["pct"]],
      pct_lower = unname(pct_ci[1]),
      pct_upper = unname(pct_ci[2]),
      wald_stat = point[["wald"]],
      wald_df = point[["edf"]],
      wald_p_value = point[["p"]],
      n_boot = as.integer(n_boot),
      boot_failed = if (is.null(boot)) {
        NA_integer_
      } else {
        sum(is.na(boot["reduction", ]))
      }
    )
  })
}

# Step 3: tidy per-dataset analysis tables from ONE model bundle.
#
#   run_analysis("outputs/.../replicate_03/models.rds",
#                output_dir = "outputs/.../replicate_03/tables")
#
# Works on the bundle from build_models() for the original data or for any
# synthetic replicate, and writes the same files with the same columns in each
# case. Tables only: no figures (see R/plots.R) and no pooling (step 4a).
# Every table carries `source` and `replicate` so that step 4a can stack them.
# Requires the helper modules (R/analysis_helpers.R).

analysis_tables <- function(bundle) {
  stopifnot(inherits(bundle, "model_bundle"))
  from_fits <- function(element) {
    purrr::map_dfr(bundle$fits, element)
  }
  list(
    model_evidence = from_fits("evidence"),
    model_estimates = from_fits("estimates"),
    predictions = from_fits("predictions"),
    role_contrasts = from_fits("contrasts"),
    variance_components = from_fits("variance"),
    linearity_sensitivity = bundle$linearity,
    basis_dimension_sensitivity = bundle$basis_sensitivity,
    player_explanatory_deviance = bundle$player_explanatory,
    player_deviance = bundle$player_deviance
  )
}
