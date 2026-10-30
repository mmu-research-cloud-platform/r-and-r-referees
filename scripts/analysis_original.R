# Restricted original-data analysis: tables and figures
#
# Run from the project root after scripts/models_original.R. Loads the saved models
# and writes to outputs/original_restricted/, which must not be published.
library(tidyverse)
library(mgcv)
library(gratia)
library(broom)
library(patchwork)

options(scipen = 999)
theme_set(theme_classic())

z_975 <- qnorm(0.975)

source(file.path("R", "analysis_helpers.R"))

# Synthetic (.csv) runs reuse the in-memory objects from models_original.R
# and write nothing to disk
save_outputs <- !grepl("\\.csv$", original_file)
model_dir <- file.path(restricted_dir, "models")
if (save_outputs) {
  original <- readRDS(file.path(model_dir, "original_data.rds"))
  original_fits <- readRDS(file.path(model_dir, "original_fits.rds"))
  original_linearity <- readRDS(file.path(model_dir, "original_linearity.rds"))
  original_basis_sensitivity <- readRDS(
    file.path(model_dir, "original_basis_sensitivity.rds")
  )
  original_player_explanatory <- readRDS(
    file.path(model_dir, "original_player_explanatory.rds")
  )
} else {
  stopifnot(exists("original_fits"), exists("original_linearity"))
}

original_evidence <- map_dfr(original_fits, "evidence")
original_estimates <- map_dfr(original_fits, "estimates")
original_predictions <- map_dfr(original_fits, "predictions")
original_contrasts <- map_dfr(original_fits, "contrasts")
original_variance <- map_dfr(original_fits, "variance")

print(original_linearity)
print(original_basis_sensitivity)
print(original_player_explanatory)

if (save_outputs) {
  write_csv(original_evidence, file.path(restricted_dir, "model_evidence.csv"))
  write_csv(
    original_estimates,
    file.path(restricted_dir, "model_estimates.csv")
  )
  write_csv(original_predictions, file.path(restricted_dir, "predictions.csv"))
  write_csv(original_contrasts, file.path(restricted_dir, "role_contrasts.csv"))
  write_csv(
    original_variance,
    file.path(restricted_dir, "variance_components.csv")
  )
  write_csv(
    original_linearity,
    file.path(restricted_dir, "linearity_sensitivity.csv")
  )
  write_csv(
    original_basis_sensitivity,
    file.path(restricted_dir, "basis_dimension_sensitivity.csv")
  )
  write_csv(
    original_player_explanatory,
    file.path(restricted_dir, "player_explanatory_deviance.csv")
  )
  write_csv(
    readRDS(file.path(model_dir, "original_player_deviance.rds")),
    file.path(restricted_dir, "player_deviance_figure_models.csv")
  )
}

original_figure_1 <- submission_figure(
  original,
  original_predictions,
  original_fits[["Low Speed Activity"]]$final,
  "Low Speed Activity"
)
original_figure_2 <- submission_figure(
  original,
  original_predictions,
  original_fits[["High Speed Activity"]]$final,
  "High Speed Activity"
)
print(original_figure_1)
print(original_figure_2)
original_figure_3 <- overlay_ratio_figure(original, original_fits)
print(original_figure_3)
if (save_outputs) {
  ggsave(
    file.path(restricted_dir, "Figure 3.png"),
    original_figure_3,
    width = 20,
    height = 12,
    units = "cm",
    dpi = 300
  )
  ggsave(
    file.path(restricted_dir, "Figure 1.png"),
    original_figure_1,
    width = 35,
    height = 20,
    units = "cm",
    dpi = 300
  )
  ggsave(
    file.path(restricted_dir, "Figure 2.png"),
    original_figure_2,
    width = 35,
    height = 25,
    units = "cm",
    dpi = 300
  )
}

original_figure <- wrap_plots(
  map(levels(original$Variable), function(outcome) {
    plot_trajectory(
      original,
      filter(original_predictions, outcome == !!outcome),
      outcome,
      paste("Original data:", outcome)
    )
  }),
  ncol = 1
)
print(original_figure)
if (save_outputs) {
  ggsave(
    file.path(restricted_dir, "original_broken_half_trajectories.png"),
    original_figure,
    width = 20,
    height = 24,
    units = "cm",
    dpi = 300
  )
}

section("ORIGINAL-DATA RESULTS")
print(original_evidence)
print(original_estimates)
print(original_contrasts)
print(original_variance)
print(original_player_explanatory)

section("ORIGINAL-DATA MODEL CHECKS (gam.check AND k.check)")
original_gam_checks <- map(original_fits, \(fit) capture_gam_check(fit$final))
walk(original_gam_checks, \(check_text) writeLines(check_text))
original_k_checks <- map_dfr(original_fits, \(fit) {
  k_diagnostics(fit$final) |> mutate(outcome = fit$outcome, .before = 1)
})
print(original_k_checks)
if (save_outputs) {
  walk2(
    original_gam_checks,
    names(original_gam_checks),
    \(check_text, outcome) {
      writeLines(
        check_text,
        file.path(
          restricted_dir,
          paste0(str_replace_all(tolower(outcome), " ", "_"), "_gam_check.txt")
        )
      )
    }
  )
  write_csv(original_k_checks, file.path(restricted_dir, "k_check.csv"))
}

if (save_outputs) {
  writeLines(
    capture.output(sessionInfo()),
    file.path(restricted_dir, "sessionInfo.txt")
  )
}
if (save_outputs) {
  message(
    "Completed original-data analysis. Restricted outputs: ",
    restricted_dir
  )
} else {
  message("Completed synthetic-data analysis. Nothing saved to disk.")
}
