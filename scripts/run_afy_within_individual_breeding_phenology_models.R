#!/usr/bin/env Rscript

# AFY-only refit of the existing within-/among-individual phenology models.
# The qmd and the unrestricted fitted models are deliberately left unchanged.
legacy_library <- "/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library"
if (!requireNamespace("cmdstanr", quietly = TRUE) && dir.exists(legacy_library)) {
  .libPaths(c(.libPaths(), legacy_library))
}
suppressPackageStartupMessages({
  library(brms)
  library(cmdstanr)
  library(dplyr)
  library(posterior)
  library(here)
})

log_message <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste0(..., collapse = ""), "\n", sep = "")
  flush.console()
}
model_choice <- commandArgs(trailingOnly = TRUE)
stopifnot(length(model_choice) <= 1L)
if (length(model_choice)) stopifnot(model_choice %in% c("first", "nest", "last"))
report_prefix <- if (length(model_choice)) {
  paste0("afy_within_individual_", model_choice, "_")
} else "afy_within_individual_"
report_path <- function(suffix) here("out", paste0(report_prefix, suffix))

dat_breeding <- readRDS(here("data/moult_breeding_KK_data.rds")) %>%
  mutate(rings_comb_raw = as.character(rings_comb), rings_comb = rings_comb_simp)

# Preserve the photograph-date reference used by the existing models. This
# keeps the time scale and priors comparable when restricting the sample.
date_scaling_ref <- dat_breeding %>%
  distinct(score, date_J, rings_comb, sex, season) %>%
  tidyr::drop_na(score, date_J, sex, rings_comb, season) %>%
  group_by(season) %>%
  summarise(
    date_J_mean_ref = mean(date_J), date_J_sd_ref = sd(date_J),
    .groups = "drop"
  )

predictors <- c(
  "first_breeding_date_J_snum", "last_nest_first_obs_J_snum",
  "last_breeding_date_J_snum"
)
specs <- tibble::tibble(
  key = c("first", "nest", "last"),
  predictor = predictors,
  label = c("First observed breeding date", "Final nest date", "Last observed breeding date"),
  output_file = c(
    "sex_first_breed_within_individual_afy_mod_.rds",
    "sex_last_nest_within_individual_afy_mod_.rds",
    "sex_last_breed_within_individual_afy_mod_.rds"
  ),
  seed = c(261L, 262L, 263L)
)
if (length(model_choice)) specs <- specs %>% filter(key == model_choice)

# Keep the original common complete-case sample definition, then require AFY.
# The stored breeding-date predictors retain their original seasonal centering.
dat_afy <- dat_breeding %>%
  filter(age_class == "AFY") %>%
  select(score, date_J, rings_comb, sex, season, age_class, age_class_source,
         all_of(predictors)) %>%
  distinct() %>%
  left_join(date_scaling_ref, by = "season") %>%
  mutate(date_J_scaled = (date_J - date_J_mean_ref) / date_J_sd_ref) %>%
  tidyr::drop_na(score, date_J, sex, rings_comb, season, all_of(predictors)) %>%
  mutate(across(c(rings_comb, sex, season, age_class, age_class_source),
                ~ if (is.factor(.x)) droplevels(.x) else factor(.x))) %>%
  as.data.frame()

bird_seasons <- dat_afy %>% distinct(rings_comb, season)
repeat_counts <- bird_seasons %>% count(rings_comb, name = "n_seasons")
stopifnot(
  nrow(dat_afy) == 1045L, n_distinct(dat_afy$rings_comb) == 47L,
  nrow(bird_seasons) == 86L, sum(repeat_counts$n_seasons > 1L) == 28L,
  all(dat_afy$age_class == "AFY")
)
write.csv(repeat_counts, report_path("repeat_counts.csv"), row.names = FALSE)

scaling <- dat_afy %>% group_by(season) %>%
  summarise(n = n(), mean_ref = first(date_J_mean_ref), sd_ref = first(date_J_sd_ref),
            .groups = "drop")
mean_date_sd <- weighted.mean(scaling$sd_ref, scaling$n)
write.csv(scaling, report_path("date_scaling.csv"), row.names = FALSE)

build_dataset <- function(predictor) {
  # Calculate individual means once per retained adult bird-season, never once
  # per photograph, and never using excluded FY/uncertain-age observations.
  phenology <- dat_afy %>% select(rings_comb, season, all_of(predictor)) %>% distinct()
  stopifnot(nrow(phenology) == nrow(bird_seasons))
  phenology <- phenology %>% group_by(rings_comb) %>%
    mutate(
      n_phenology_seasons = n(),
      breeding_mean_i = mean(.data[[predictor]]),
      breeding_dev_is = .data[[predictor]] - breeding_mean_i
    ) %>% ungroup()
  checks <- phenology %>% group_by(rings_comb) %>%
    summarise(mean_dev = mean(breeding_dev_is), .groups = "drop")
  stopifnot(max(abs(checks$mean_dev)) < 1e-10)
  dat_afy %>% left_join(phenology, by = c("rings_comb", "season", predictor)) %>%
    as.data.frame()
}

moult_priors <- c(
  prior(normal(5.5, 1), nlpar = "baseline", class = "b", coef = "Intercept"),
  prior(normal(0, 1), nlpar = "baseline", class = "b"),
  prior(normal(0, 0.5), nlpar = "t0", class = "b", coef = "Intercept"),
  prior(normal(0, 0.5), nlpar = "t0", class = "b"),
  prior(normal(1, 0.5), nlpar = "k", lb = 0),
  prior(exponential(2), nlpar = "baseline", class = "sd", group = "rings_comb"),
  prior(exponential(2), nlpar = "baseline", class = "sd", group = "rings_comb:season"),
  prior(exponential(2), nlpar = "t0", class = "sd", group = "rings_comb"),
  prior(exponential(2), nlpar = "t0", class = "sd", group = "rings_comb:season")
)
model_formula <- bf(
  score ~ 1 + (baseline - 1) * (1 - 1 / (1 + exp(-k * (date_J_scaled - t0)))),
  baseline ~ sex + (1 | i | rings_comb) + (1 | rings_comb:season),
  t0 ~ sex + season + breeding_mean_i + breeding_dev_is +
    (1 | i | rings_comb) + (1 | rings_comb:season),
  k ~ 1, nl = TRUE
)

summaries <- list()
diagnostics <- list()
predictive_checks <- list()
last_fit <- NULL
for (i in seq_len(nrow(specs))) {
  spec <- specs[i, ]
  dat_i <- build_dataset(spec$predictor)
  path_i <- here("out", spec$output_file)
  if (file.exists(path_i)) {
    model <- readRDS(path_i)
    stopifnot(isTRUE(all.equal(model$data, dat_i)), identical(model$predictor, spec$predictor))
    fit <- model$fitted_model
    log_message("Using saved AFY fit: ", spec$label)
  } else {
    log_message("Fitting ", spec$label, " | photographs=", nrow(dat_i),
                " | birds=", n_distinct(dat_i$rings_comb), " | bird-seasons=", nrow(bird_seasons))
    fit_args <- list(
      data = dat_i, chains = 4, cores = 4, iter = 10000, warmup = 5000,
      control = list(adapt_delta = 0.999, max_treedepth = 15),
      backend = "cmdstanr", seed = spec$seed, refresh = 1000
    )
    if (is.null(last_fit)) {
      fit <- do.call(brm, c(list(formula = model_formula, family = gaussian(),
                              prior = moult_priors,
                              stan_model_args = list(cpp_options = list(PRECOMPILED_HEADERS = FALSE))),
                           fit_args))
    } else {
      # Generic mean/deviation column names let all three models share the
      # exact Stan program, avoiding unnecessary repeated compilation.
      fit_args$data <- NULL
      fit <- do.call(update, c(list(object = last_fit, newdata = dat_i,
                                   recompile = FALSE), fit_args))
    }
    model <- list(
      fitted_model = fit, data = dat_i, predictor = spec$predictor,
      sample_restriction = "Confidently AFY bird-seasons only",
      date_scaling_reference = date_scaling_ref,
      midpoint_days_conversion = mean_date_sd,
      individual_means_calculated_after_afy_filter = TRUE,
      session_info = sessionInfo()
    )
    saveRDS(model, path_i)
    log_message("Saved ", path_i)
  }
  last_fit <- fit

  post <- as_draws_df(fit)
  among <- post$b_t0_breeding_mean_i * mean_date_sd
  within <- post$b_t0_breeding_dev_is * mean_date_sd
  summarise_effect <- function(x, component) {
    tibble::tibble(
      predictor = spec$predictor, label = spec$label, component = component,
      median = median(x), ci_lower = unname(quantile(x, 0.025)),
      ci_upper = unname(quantile(x, 0.975)), pr_positive = mean(x > 0),
      units = "Midpoint delay in days per one-day breeding-date delay",
      photographs = nrow(dat_i), birds = n_distinct(dat_i$rings_comb),
      bird_seasons = nrow(bird_seasons), repeated_birds = sum(repeat_counts$n_seasons > 1)
    )
  }
  summaries[[i]] <- bind_rows(
    summarise_effect(among, "Among individuals"),
    summarise_effect(within, "Within individuals"),
    summarise_effect(among - within, "Among minus within")
  )
  ds <- posterior::summarise_draws(as_draws_array(fit), "rhat", "ess_bulk", "ess_tail")
  nuts <- brms::nuts_params(fit)
  diagnostics[[i]] <- tibble::tibble(
    predictor = spec$predictor, max_rhat = max(ds$rhat, na.rm = TRUE),
    min_bulk_ess = min(ds$ess_bulk, na.rm = TRUE), min_tail_ess = min(ds$ess_tail, na.rm = TRUE),
    divergences = sum(nuts$Value[nuts$Parameter == "divergent__"]),
    max_treedepth = max(nuts$Value[nuts$Parameter == "treedepth__"]),
    treedepth_limit_hits = sum(nuts$Value[nuts$Parameter == "treedepth__"] >= 15)
  )
  set.seed(spec$seed + 1000L)
  yrep <- posterior_predict(fit, ndraws = 400)
  y <- dat_i$score
  ppc_stats <- list(mean = mean, sd = sd, q10 = function(x) unname(quantile(x, 0.1)),
                    q90 = function(x) unname(quantile(x, 0.9)))
  predictive_checks[[i]] <- bind_rows(lapply(names(ppc_stats), function(stat) {
    fun <- ppc_stats[[stat]]
    pred <- apply(yrep, 1, fun)
    tibble::tibble(predictor = spec$predictor, statistic = stat, observed = fun(y),
      predicted_median = median(pred), predicted_lower = unname(quantile(pred, 0.025)),
      predicted_upper = unname(quantile(pred, 0.975)))
  }))
  write.csv(bind_rows(summaries), report_path("effects.csv"), row.names = FALSE)
  write.csv(bind_rows(diagnostics), report_path("diagnostics.csv"), row.names = FALSE)
  write.csv(bind_rows(predictive_checks), report_path("predictive_checks.csv"), row.names = FALSE)
  log_message("Finished ", spec$label)
  print(summaries[[i]] %>% select(label, component, median, ci_lower, ci_upper))
  print(diagnostics[[i]])
}
log_message("All AFY fits complete; qmd unchanged")
