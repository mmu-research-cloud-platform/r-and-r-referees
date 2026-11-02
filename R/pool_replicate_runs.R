# Pools independent per-replicate runs of scripts/models_original.R (one
# outputs/synthetic/replicate_<i>/models/ directory each) into the synthetic
# column of the supplementary comparison table. Requires R/analysis_helpers.R.

pool_replicate_runs <- function(synthetic_dir, replicates = 1:10) {
  low <- "Low Speed Activity"
  high <- "High Speed Activity"
  model_dir <- \(i) file.path(synthetic_dir, paste0("replicate_", i), "models")

  runs <- lapply(replicates, function(i) {
    list(
      fits = readRDS(file.path(model_dir(i), "original_fits.rds")),
      linearity = readRDS(file.path(model_dir(i), "original_linearity.rds")),
      deviance = readRDS(file.path(
        model_dir(i),
        "original_player_deviance.rds"
      ))
    )
  })

  evidence <- dplyr::bind_rows(lapply(seq_along(runs), function(k) {
    dplyr::bind_rows(lapply(runs[[k]]$fits, `[[`, "evidence")) |>
      dplyr::mutate(replicate = replicates[[k]])
  }))
  linearity <- dplyr::bind_rows(lapply(seq_along(runs), function(k) {
    dplyr::mutate(runs[[k]]$linearity, replicate = replicates[[k]])
  }))
  deviance <- dplyr::bind_rows(lapply(seq_along(runs), function(k) {
    dplyr::mutate(runs[[k]]$deviance, replicate = replicates[[k]])
  })) |>
    dplyr::rename(outcome = referee_outcome)
  # Same fixed structures and k.check settings as the supplementary table:
  # low = common, high = deviation (not each replicate's own selected model,
  # whose smooth names differ and would yield NA).
  # k.check is stochastic: seed per replicate and outcome so results do not
  # depend on loop order (must match supplementary-material.qmd).
  diagnostics <- dplyr::bind_rows(lapply(seq_along(runs), function(k) {
    dplyr::bind_rows(lapply(c(low, high), function(o) {
      set.seed(7183 + 10 * replicates[[k]] + (o == high))
      structure <- if (o == low) "common" else "deviation"
      terms <- if (o == low) {
        c("s(Time_half):H1", "s(Time_half):H2")
      } else {
        c("s(Time_half):RefH1", "s(Time_half):RefH2")
      }
      check <- mgcv::k.check(
        runs[[k]]$fits[[o]]$candidates[[structure]],
        subsample = 5000,
        n.rep = 400
      )
      stopifnot(all(terms %in% rownames(check)))
      tibble::tibble(
        replicate = replicates[[k]],
        outcome = o,
        half = c(1, 2),
        edf = check[terms, "edf"],
        k_index = check[terms, "k-index"],
        p = check[terms, "p-value"]
      )
    }))
  }))

  # Pooled referee vs player effect (low-speed, common-structure model)
  low_role <- dplyr::bind_rows(lapply(runs, function(run) {
    model <- run$fits[[low]]$candidates[["common"]]
    tibble::tibble(
      estimate = unname(coef(model)["RoleReferee"]),
      variance = unname(vcov(model)["RoleReferee", "RoleReferee"])
    )
  }))
  pooled_role <- fully_synthetic_pool(low_role)

  mean_of <- \(x, digits) sprintf(paste0("%.", digits, "f"), mean(x))
  range_of <- \(x) paste(sprintf("%.3f", range(x)), collapse = " to ")
  pooled_evidence <- evidence |>
    dplyr::summarise(AIC = mean(AIC), .by = c(outcome, model)) |>
    dplyr::mutate(
      delta_AIC = AIC - min(AIC),
      akaike_weight = exp(-0.5 * delta_AIC) / sum(exp(-0.5 * delta_AIC)),
      .by = outcome
    )
  ev <- \(o, m, col, d) {
    sprintf(
      paste0("%.", d, "f"),
      pooled_evidence[[col]][
        pooled_evidence$outcome == o & pooled_evidence$model == m
      ]
    )
  }
  pct <- \(x) 100 * expm1(x)
  referee_text <- sprintf(
    "%.1f [%.1f, %.1f]",
    pct(pooled_role$estimate),
    pct(pooled_role$lower),
    pct(pooled_role$upper)
  )
  lin <- \(o, m, other) {
    w <- linearity |>
      dplyr::filter(outcome == o) |>
      dplyr::select(replicate, model, AIC) |>
      tidyr::pivot_wider(names_from = model, values_from = AIC)
    mean_of(w[[m]] - w[[other]], 2)
  }
  dg <- \(o, h, col, f) {
    f(diagnostics[[col]][diagnostics$outcome == o & diagnostics$half == h])
  }
  dev_text <- \(o) {
    x <- deviance$pct_reduction[deviance$outcome == o]
    sprintf("Mean %.1f (range %.1f to %.1f)", mean(x), min(x), max(x))
  }

  statistics <- c(
    "Low: role-deviation dAIC" = ev(low, "deviation", "delta_AIC", 2),
    "Low: independent dAIC" = ev(low, "independent", "delta_AIC", 2),
    "Low: referee vs player (% higher)" = referee_text,
    "Low: first-half EDF" = dg(low, 1, "edf", \(x) mean_of(x, 2)),
    "Low: second-half EDF" = dg(low, 2, "edf", \(x) mean_of(x, 2)),
    "Low: first-half k-index" = dg(low, 1, "k_index", \(x) mean_of(x, 3)),
    "Low: second-half k-index" = dg(low, 2, "k_index", \(x) mean_of(x, 3)),
    "Low: first-half k.check p" = dg(low, 1, "p", range_of),
    "Low: second-half k.check p" = dg(low, 2, "p", range_of),
    "Low: smooth vs linear dAIC" = lin(
      low,
      "half-specific linear",
      "half-specific smooth"
    ),
    "High: independent dAIC" = ev(high, "independent", "delta_AIC", 2),
    "High: selected weight" = ev(high, "deviation", "akaike_weight", 3),
    "High: independent weight" = ev(high, "independent", "akaike_weight", 3),
    "High: common dAIC" = ev(high, "common", "delta_AIC", 2),
    "High: common weight" = ev(high, "common", "akaike_weight", 3),
    "High: half 1 referee smooth EDF" = dg(high, 1, "edf", \(x) mean_of(x, 2)),
    "High: half 2 referee smooth EDF" = dg(high, 2, "edf", \(x) mean_of(x, 2)),
    "High: first-half k-index" = dg(high, 1, "k_index", \(x) mean_of(x, 3)),
    "High: second-half k-index" = dg(high, 2, "k_index", \(x) mean_of(x, 3)),
    "High: first-half k.check p" = dg(high, 1, "p", range_of),
    "High: second-half k.check p" = dg(high, 2, "p", range_of),
    "High: smooth vs linear dAIC" = lin(
      high,
      "half-specific smooth",
      "half-specific linear"
    ),
    "Low: deviance reduction (%)" = dev_text(low),
    "High: deviance reduction (%)" = dev_text(high)
  )

  list(
    table = tibble::tibble(
      Statistic = gsub("dAIC", "\u0394AIC", names(statistics), fixed = TRUE),
      `Synthetic (pooled)` = unname(statistics)
    ),
    pooled_role = pooled_role
  )
}
