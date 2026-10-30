# Synthetic-data analysis: pooling, public outputs, and figures
#
# Run from the project root after scripts/models_synthetic.R. Loads the saved
# synthetic models and writes public-safe outputs to osf/ and
# outputs/synthetic/.
library(tidyverse)
library(mgcv)
library(gratia)
library(synthpop)
library(broom)
library(patchwork)

options(scipen = 999)
theme_set(theme_classic())

m_synthetic <- 10
z_975 <- qnorm(0.975)

source(file.path("R", "analysis_helpers.R"))

model_dir <- file.path(synthetic_dir, "models")
original <- readRDS(file.path(model_dir, "original_data.rds"))
synthetic_all <- readRDS(file.path(model_dir, "synthetic_all.rds"))
synthetic_fits <- readRDS(file.path(model_dir, "synthetic_fits.rds"))
synthetic_linearity <- readRDS(file.path(model_dir, "synthetic_linearity.rds"))
synthetic_basis_sensitivity <- readRDS(
  file.path(model_dir, "synthetic_basis_sensitivity.rds")
)
utility_all <- readRDS(file.path(model_dir, "utility_all.rds"))
primary_utility_gen <- readRDS(file.path(model_dir, "primary_utility_gen.rds"))

print(synthetic_linearity)
print(synthetic_basis_sensitivity)
print(utility_all)
print(primary_utility_gen)

synthetic_evidence <- map_dfr(synthetic_fits, \(x) map_dfr(x, "evidence"))
synthetic_estimates <- map_dfr(synthetic_fits, \(x) map_dfr(x, "estimates"))
synthetic_predictions <- map_dfr(synthetic_fits, \(x) map_dfr(x, "predictions"))
synthetic_contrasts <- map_dfr(synthetic_fits, \(x) map_dfr(x, "contrasts"))
synthetic_variance <- map_dfr(synthetic_fits, \(x) map_dfr(x, "variance"))

pooled_predictions <- pool_prediction_grid(synthetic_predictions)
pooled_contrasts <- synthetic_contrasts |>
  group_by(outcome, Half, Time_half, Time) |>
  group_modify(function(.x, .y) {
    fully_synthetic_pool(
      transmute(.x, estimate = estimate_link, variance = variance_link),
      m = n_distinct(.x$replicate)
    ) |>
      mutate(
        ratio = exp(estimate),
        ratio_lower = exp(lower),
        ratio_upper = exp(upper)
      )
  }) |>
  ungroup()

synthetic_model_summary <- synthetic_evidence |>
  summarise(
    mean_delta_AIC = mean(delta_AIC),
    median_delta_AIC = median(delta_AIC),
    minimum_delta_AIC = min(delta_AIC),
    maximum_delta_AIC = max(delta_AIC),
    mean_akaike_weight = mean(akaike_weight),
    selected_count = sum(delta_AIC == 0),
    .by = c(outcome, model)
  )

synthetic_player_deviance <- map_dfr(seq_len(m_synthetic), \(replicate) {
  player_deviance_analysis(
    filter(synthetic_all, replicate == !!replicate) |> select(-replicate),
    synthetic_fits[[replicate]],
    pairs = "same"
  ) |>
    mutate(replicate = replicate, .before = 1)
}) |>
  rename(outcome = referee_outcome)

write_csv(
  synthetic_player_deviance,
  file.path(synthetic_dir, "player_deviance_by_replicate.csv")
)
write_csv(
  synthetic_evidence,
  file.path(synthetic_dir, "model_evidence_by_replicate.csv")
)
write_csv(
  synthetic_model_summary,
  file.path(synthetic_dir, "model_evidence_summary.csv")
)
write_csv(
  synthetic_estimates,
  file.path(synthetic_dir, "model_estimates_by_replicate.csv")
)
write_csv(
  synthetic_predictions,
  file.path(synthetic_dir, "predictions_by_replicate.csv")
)
write_csv(
  pooled_predictions,
  file.path(synthetic_dir, "predictions_pooled.csv")
)
write_csv(
  synthetic_contrasts,
  file.path(synthetic_dir, "role_contrasts_by_replicate.csv")
)
write_csv(
  pooled_contrasts,
  file.path(synthetic_dir, "role_contrasts_pooled.csv")
)
write_csv(
  synthetic_variance,
  file.path(synthetic_dir, "variance_components_by_replicate.csv")
)
write_csv(
  synthetic_linearity,
  file.path(synthetic_dir, "linearity_sensitivity_by_replicate.csv")
)
write_csv(
  synthetic_basis_sensitivity,
  file.path(synthetic_dir, "basis_dimension_sensitivity_by_replicate.csv")
)
write_csv(utility_all, file.path("osf", "utility_summary.csv"))
write_csv(
  synthetic_model_summary,
  file.path("osf", "model_evidence_summary.csv")
)
write_csv(pooled_contrasts, file.path("osf", "pooled_role_contrasts.csv"))

replicate_one <- filter(synthetic_all, replicate == 1)

utility_plot <- bind_rows(
  original |> mutate(dataset = "Original"),
  replicate_one |> mutate(dataset = "Synthetic replicate 1")
) |>
  ggplot(aes(Distance, colour = dataset, fill = dataset)) +
  geom_density(alpha = 0.2) +
  facet_grid(Role ~ Variable, scales = "free") +
  labs(x = "Distance (m)", y = "Density", colour = NULL, fill = NULL)
ggsave(
  file.path("osf", "original_vs_synthetic_replicate_1.png"),
  utility_plot,
  width = 20,
  height = 14,
  units = "cm",
  dpi = 300
)

section("SYNTHETIC-DATA RESULTS")
print(synthetic_model_summary)
print(pooled_predictions)
print(pooled_contrasts)
print(synthetic_variance)
writeLines(
  capture.output(sessionInfo()),
  file.path(synthetic_dir, "sessionInfo.txt")
)
message(
  "Completed synthetic-data analysis. Public-safe outputs: osf and ",
  synthetic_dir
)
