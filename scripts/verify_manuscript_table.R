# Compare manuscript-reported values with the original-analysis outputs.
#
# Run from the project root after scripts/models_original.R, scripts/analysis_original.R
# (player deviance is built in models_original.R). Reads restricted_dir (default
# outputs/original_restricted) and writes manuscript_value_check.csv there.
# k-index and diagnostic p-values are recomputed with k.check(), which is
# stochastic, so they are checked with a tolerance and seed reported.
library(tidyverse)
library(mgcv)

if (!exists("restricted_dir")) {
  restricted_dir <- file.path("outputs", "original_restricted")
}
low <- "Low Speed Activity"
high <- "High Speed Activity"
read_out <- \(f) read_csv(file.path(restricted_dir, f), show_col_types = FALSE)

evidence <- read_out("model_evidence.csv")
linearity <- read_out("linearity_sensitivity.csv")
basis <- read_out("basis_dimension_sensitivity.csv")
contrasts <- read_out("role_contrasts.csv")
deviance <- read_out("player_deviance_figure_models.csv")
fits <- readRDS(file.path(restricted_dir, "models", "original_fits.rds"))
data <- readRDS(file.path(restricted_dir, "models", "original_data.rds"))

ev <- \(o, m, col) evidence[[col]][evidence$outcome == o & evidence$model == m]
lin <- \(o, m) {
  linearity$delta_AIC[linearity$outcome == o & linearity$model == m]
}
edf <- \(o, term) {
  basis$edf[basis$outcome == o & basis$term == term & basis$k == 5]
}
dev <- \(r, col) {
  deviance[[col]][deviance$referee_outcome == r & deviance$player_outcome == r]
}

set.seed(7183)
kc <- \(o, terms) {
  check <- k.check(fits[[o]]$final, subsample = 5000, n.rep = 400)
  list(
    index = check[terms, "k-index"],
    p = check[terms, "p-value"]
  )
}
low_terms <- c("s(Time_half):H1", "s(Time_half):H2")
high_terms <- c("s(Time_half):RefH1", "s(Time_half):RefH2")
k_low <- kc(low, low_terms)
k_high <- kc(high, high_terms)

role_ratio <- contrasts |>
  filter(outcome == low, Half == "First") |>
  pull(ratio) |>
  unique()

# tolerance = half a unit in the last reported digit
checks <- tribble(
  ~label                               , ~reported , ~computed                                , ~tolerance , ~source                             ,
  "Low: role-deviation dAIC"           ,   3.51    , ev(low, "deviation", "delta_AIC")        , 0.005      , "model_evidence.csv"                ,
  "Low: independent dAIC"              ,  12.37    , ev(low, "independent", "delta_AIC")      , 0.005      , "model_evidence.csv"                ,
  "Low: referee vs player (% higher)"  ,   6       , 100 * (role_ratio - 1)                   , 0.5        , "role_contrasts.csv"                ,
  "Low: first-half EDF"                ,   3.52    , edf(low, "s(Time_half):H1")              , 0.005      , "basis_dimension_sensitivity.csv"   ,
  "Low: second-half EDF"               ,   3.71    , edf(low, "s(Time_half):H2")              , 0.005      , "basis_dimension_sensitivity.csv"   ,
  "Low: first-half k-index"            ,   1.006   , k_low$index[1]                           , 0.0005     , "k.check (seeded)"                  ,
  "Low: second-half k-index"           ,   1.006   , k_low$index[2]                           , 0.0005     , "k.check (seeded)"                  ,
  "Low: first-half k.check p"          ,   0.695   , k_low$p[1]                               , 0.0005     , "k.check (seeded)"                  ,
  "Low: second-half k.check p"         ,   0.652   , k_low$p[2]                               , 0.0005     , "k.check (seeded)"                  ,
  "Low: smooth vs linear dAIC"         ,  55.07    , lin(low, "half-specific linear")         , 0.005      , "linearity_sensitivity.csv"         ,
  "High: independent dAIC"             ,   1.20    , ev(high, "independent", "delta_AIC")     , 0.005      , "model_evidence.csv"                ,
  "High: selected weight"              ,   0.639   , ev(high, "deviation", "akaike_weight")   , 0.0005     , "model_evidence.csv"                ,
  "High: independent weight"           ,   0.350   , ev(high, "independent", "akaike_weight") , 0.0005     , "model_evidence.csv"                ,
  "High: common dAIC"                  ,   8.10    , ev(high, "common", "delta_AIC")          , 0.005      , "model_evidence.csv"                ,
  "High: common weight"                ,   0.011   , ev(high, "common", "akaike_weight")      , 0.0005     , "model_evidence.csv"                ,
  "High: half 1 referee smooth EDF"    ,   2.59    , edf(high, "s(Time_half):RefH1")          , 0.005      , "basis_dimension_sensitivity.csv"   ,
  "High: half 2 referee smooth EDF"    ,   2.81    , edf(high, "s(Time_half):RefH2")          , 0.005      , "basis_dimension_sensitivity.csv"   ,
  "High: first-half k-index"           ,   0.981   , k_high$index[1]                          , 0.0005     , "k.check (seeded)"                  ,
  "High: second-half k-index"          ,   0.981   , k_high$index[2]                          , 0.0005     , "k.check (seeded)"                  ,
  "High: first-half k.check p"         ,   0.340   , k_high$p[1]                              , 0.0005     , "k.check (seeded)"                  ,
  "High: second-half k.check p"        ,   0.332   , k_high$p[2]                              , 0.0005     , "k.check (seeded)"                  ,
  "High: smooth vs linear dAIC"        ,   2.13    , lin(high, "half-specific smooth")        , 0.005      , "linearity_sensitivity.csv"         ,
  "Matches (n)"                        , 236       , n_distinct(data$Match)                   , 0          , "original_data.rds"                 ,
  "Smooth basis dimension k"           ,   5       , max(basis$k[basis$k == 5])               , 0          , "basis_dimension_sensitivity.csv"   ,
  "GAMM spline basis dimension K"      ,   9       , max(basis$k)                             , 0          , "basis_dimension_sensitivity.csv"   ,
  "Low: deviance reduction (%)"        ,  62.6     , dev(low, "pct_reduction")                , 0.05       , "player_deviance_figure_models.csv" ,
  "Low: deviance reduction lower (%)"  ,  59.2     , dev(low, "pct_lower")                    , 0.05       , "player_deviance_figure_models.csv" ,
  "Low: deviance reduction upper (%)"  ,  66.7     , dev(low, "pct_upper")                    , 0.05       , "player_deviance_figure_models.csv" ,
  "High: deviance reduction (%)"       ,  29.6     , dev(high, "pct_reduction")               , 0.05       , "player_deviance_figure_models.csv" ,
  "High: deviance reduction lower (%)" ,  26.9     , dev(high, "pct_lower")                   , 0.05       , "player_deviance_figure_models.csv" ,
  "High: deviance reduction upper (%)" ,  31.9     , dev(high, "pct_upper")                   , 0.05       , "player_deviance_figure_models.csv"
) |>
  mutate(
    difference = computed - reported,
    match = abs(difference) <= tolerance
  )

write_csv(checks, file.path(restricted_dir, "manuscript_value_check.csv"))
checks |> filter(!match)
