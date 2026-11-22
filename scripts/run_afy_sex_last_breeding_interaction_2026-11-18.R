#!/usr/bin/env Rscript

# Supplementary midpoint interaction in the original 47-bird AFY sample.
# This script writes only its own analysis directory. It does not modify the
# primary models, source data, manuscript, rebuttal, Quarto, or rendered HTML.
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

main <- function() {
  report_dir <- here("out", "afy_sex_last_breeding_interaction_2026-11-18")
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  path <- function(name) file.path(report_dir, name)
  log_con <- file(path("run.log"), open = "at")
  sink(log_con, split = TRUE)
  on.exit({ sink(); close(log_con) }, add = TRUE)
  log_message <- function(...) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), " | ",
        paste0(..., collapse = ""), "\n", sep = "")
    flush.console()
  }

  source_path <- here("out", "sex_last_breed_within_individual_afy_mod_.rds")
  protected_paths <- c(
    source_path, here("data", "moult_breeding_KK_data.rds"),
    here("banded_dotterel_moult.qmd"), here("banded_dotterel_moult.html")
  )
  before_hash <- tools::md5sum(protected_paths)
  write.csv(data.frame(path = names(before_hash), md5 = unname(before_hash)),
            path("input_hashes_before.csv"), row.names = FALSE)
  source_model <- readRDS(source_path)
  dat <- source_model$data
  stopifnot(identical(levels(dat$sex), c("F", "M")),
            nrow(dat) == 1045L, n_distinct(dat$rings_comb) == 47L,
            all(dat$age_class == "AFY"),
            source_model$predictor == "last_breeding_date_J_snum")

  phenology <- dat %>%
    distinct(rings_comb, sex, season, last_breeding_date_J_snum,
             breeding_mean_i, breeding_dev_is)
  stopifnot(nrow(phenology) == 86L,
            !anyDuplicated(phenology[c("rings_comb", "season")]))
  centering_checks <- phenology %>% group_by(rings_comb) %>%
    summarise(n_seasons = n(),
              mean_error = max(abs(breeding_mean_i -
                                    mean(last_breeding_date_J_snum))),
              deviation_error = max(abs(breeding_dev_is -
                                         (last_breeding_date_J_snum - breeding_mean_i))),
              mean_deviation = mean(breeding_dev_is), .groups = "drop")
  stopifnot(max(centering_checks$mean_error) < 1e-10,
            max(centering_checks$deviation_error) < 1e-10,
            max(abs(centering_checks$mean_deviation)) < 1e-10,
            sum(centering_checks$n_seasons > 1L) == 28L)
  birds <- phenology %>% distinct(rings_comb, sex) %>%
    left_join(centering_checks, by = "rings_comb")
  sample_counts <- birds %>% group_by(sex) %>%
    summarise(birds = n(), bird_seasons = sum(n_seasons),
              repeated_birds = sum(n_seasons > 1L),
              .groups = "drop")
  design <- model.matrix(~ sex * (breeding_mean_i + breeding_dev_is) + season,
                         data = phenology)
  stopifnot(qr(design)$rank == ncol(design))
  write.csv(sample_counts, path("sample_counts.csv"), row.names = FALSE)
  write.csv(centering_checks, path("centering_checks.csv"), row.names = FALSE)
  write.csv(phenology, path("bird_season_predictors.csv"), row.names = FALSE)
  log_message("Data verified: 47 AFY birds, 86 bird-seasons, 1,045 photographs; ",
              "fixed-effect design rank ", qr(design)$rank, "/", ncol(design))
  print(sample_counts)

  interaction_formula <- bf(
    score ~ 1 + (baseline - 1) *
      (1 - 1 / (1 + exp(-k * (date_J_scaled - t0)))),
    baseline ~ sex + (1 | i | rings_comb) + (1 | rings_comb:season),
    t0 ~ sex * (breeding_mean_i + breeding_dev_is) + season +
      (1 | i | rings_comb) + (1 | rings_comb:season),
    k ~ 1, nl = TRUE
  )
  # Keep formula environments free of this function's fit/data objects when
  # serialising the specification and result.
  environment(interaction_formula$formula) <- .GlobalEnv
  for (nm in names(interaction_formula$pforms)) {
    if (inherits(interaction_formula$pforms[[nm]], "formula")) {
      environment(interaction_formula$pforms[[nm]]) <- .GlobalEnv
    }
  }
  # Use exactly the user-specified priors in the original AFY fit. Generic
  # midpoint coefficient priors also cover the two added sex interactions.
  interaction_priors <- source_model$fitted_model$prior %>%
    filter(source == "user")
  seed <- 264L
  sampler_settings <- list(chains = 4L, iter = 10000L, warmup = 5000L,
                           adapt_delta = 0.999, max_treedepth = 15L, seed = seed)
  saveRDS(list(formula = interaction_formula, priors = interaction_priors,
               sampler_settings = sampler_settings,
               midpoint_days_conversion = source_model$midpoint_days_conversion,
               source_model = source_path), path("specification.rds"))
  writeLines(capture.output(print(interaction_formula)), path("formula.txt"))
  writeLines(capture.output(print(interaction_priors)), path("priors.txt"))

  model_path <- path("model.rds")
  if (file.exists(model_path)) {
    result <- readRDS(model_path)
    stopifnot(identical(result$data, dat),
              identical(result$sampler_settings, sampler_settings),
              identical(result$formula, interaction_formula))
    fit <- result$fitted_model
    log_message("Using existing fit in this analysis directory")
  } else {
    dir.create(path("cmdstan"), showWarnings = FALSE)
    log_message("Compiling and sampling the supplementary AFY interaction model")
    fit <- brm(
      formula = interaction_formula, data = dat, family = gaussian(),
      prior = interaction_priors,
      chains = sampler_settings$chains, cores = 4,
      iter = sampler_settings$iter, warmup = sampler_settings$warmup,
      control = list(adapt_delta = sampler_settings$adapt_delta,
                     max_treedepth = sampler_settings$max_treedepth),
      backend = "cmdstanr", seed = seed, refresh = 1000,
      output_dir = path("cmdstan"),
      stan_model_args = list(cpp_options = list(PRECOMPILED_HEADERS = FALSE))
    )
    result <- list(
      fitted_model = fit, data = dat, formula = interaction_formula,
      predictor = source_model$predictor,
      sample_restriction = "Original 47 confidently AFY birds / 86 bird-seasons",
      date_scaling_reference = source_model$date_scaling_reference,
      midpoint_days_conversion = source_model$midpoint_days_conversion,
      individual_means_calculated_after_afy_filter = TRUE,
      means_and_deviations_identical_to_primary_model = identical(dat, source_model$data),
      sampler_settings = sampler_settings, source_model = source_path,
      session_info = sessionInfo()
    )
    saveRDS(result, model_path)
    log_message("Saved fitted model: ", model_path)
  }

  post <- as_draws_df(fit)
  find_interaction <- function(predictor) {
    candidates <- names(post)[grepl("^b_t0_", names(post)) &
                                grepl("sexM", names(post), fixed = TRUE) &
                                grepl(predictor, names(post), fixed = TRUE)]
    stopifnot(length(candidates) == 1L)
    candidates
  }
  mean_interaction <- find_interaction("breeding_mean_i")
  dev_interaction <- find_interaction("breeding_dev_is")
  day_scale <- source_model$midpoint_days_conversion
  female_among <- post$b_t0_breeding_mean_i * day_scale
  female_within <- post$b_t0_breeding_dev_is * day_scale
  sex_among_difference <- post[[mean_interaction]] * day_scale
  sex_within_difference <- post[[dev_interaction]] * day_scale
  male_among <- female_among + sex_among_difference
  male_within <- female_within + sex_within_difference
  effects <- list(
    "Among individuals: female" = female_among,
    "Among individuals: male" = male_among,
    "Within individuals: female" = female_within,
    "Within individuals: male" = male_within,
    "Among-individual slope: male minus female" = sex_among_difference,
    "Within-individual slope: male minus female" = sex_within_difference,
    "Among minus within: female" = female_among - female_within,
    "Among minus within: male" = male_among - male_within
  )
  summarise_effect <- function(x, label) {
    data.frame(effect = label, median = median(x),
               ci_lower = unname(quantile(x, 0.025)),
               ci_upper = unname(quantile(x, 0.975)),
               pr_positive = mean(x > 0),
               units = "Days of molt midpoint change per one-day breeding-date change")
  }
  effect_summary <- bind_rows(lapply(names(effects), function(nm) {
    summarise_effect(effects[[nm]], nm)
  }))
  write.csv(effect_summary, path("effects.csv"), row.names = FALSE)
  effect_ten_days <- effect_summary %>%
    mutate(across(c(median, ci_lower, ci_upper), ~ .x * 10),
           units = "Days of molt midpoint change per ten-day breeding-date change")
  write.csv(effect_ten_days, path("effects_per_ten_days.csv"), row.names = FALSE)

  draw_array <- as_draws_array(fit)
  parameter_diagnostics <- posterior::summarise_draws(draw_array,
                                                      "rhat", "ess_bulk", "ess_tail")
  write.csv(as.data.frame(parameter_diagnostics), path("parameter_diagnostics.csv"),
            row.names = FALSE)
  np <- brms::nuts_params(fit)
  energy_diagnostics <- lapply(split(np[np$Parameter == "energy__", ],
                                     np$Chain[np$Parameter == "energy__"]), function(x) {
    data.frame(chain = x$Chain[1], ebfmi = mean(diff(x$Value)^2) / var(x$Value))
  }) %>% bind_rows()
  write.csv(energy_diagnostics, path("energy_diagnostics.csv"), row.names = FALSE)
  diagnostics <- data.frame(
    chains = sampler_settings$chains,
    posterior_draws = nrow(post),
    max_rhat = max(parameter_diagnostics$rhat, na.rm = TRUE),
    min_bulk_ess = min(parameter_diagnostics$ess_bulk, na.rm = TRUE),
    min_tail_ess = min(parameter_diagnostics$ess_tail, na.rm = TRUE),
    divergences = sum(np$Value[np$Parameter == "divergent__"]),
    max_treedepth = max(np$Value[np$Parameter == "treedepth__"]),
    treedepth_limit_hits = sum(np$Value[np$Parameter == "treedepth__"] >= 15),
    min_ebfmi = min(energy_diagnostics$ebfmi)
  )
  write.csv(diagnostics, path("diagnostics.csv"), row.names = FALSE)
  fixed_terms <- c("b_t0_breeding_mean_i", "b_t0_breeding_dev_is",
                   mean_interaction, dev_interaction,
                   "b_t0_sexM", "b_k_Intercept")
  stopifnot(length(grep("^b_k_", names(post))) == 1L)
  write.csv(as.data.frame(parameter_diagnostics %>% filter(variable %in% fixed_terms)),
            path("key_parameter_diagnostics.csv"), row.names = FALSE)

  # Same global posterior predictive summaries used for the primary AFY fits.
  set.seed(seed + 1000L)
  yrep <- posterior_predict(fit, ndraws = 400)
  stats <- list(mean = mean, sd = sd,
                q10 = function(x) unname(quantile(x, 0.1)),
                q90 = function(x) unname(quantile(x, 0.9)))
  ppc_summary <- bind_rows(lapply(names(stats), function(nm) {
    values <- apply(yrep, 1, stats[[nm]])
    data.frame(statistic = nm, observed = stats[[nm]](dat$score),
               predicted_median = median(values),
               predicted_lower = unname(quantile(values, 0.025)),
               predicted_upper = unname(quantile(values, 0.975)))
  }))
  write.csv(ppc_summary, path("predictive_checks.csv"), row.names = FALSE)
  sex_ppc <- bind_rows(lapply(levels(dat$sex), function(sx) {
    z <- dat$sex == sx
    values <- rowMeans(yrep[, z, drop = FALSE])
    data.frame(sex = sx, observed_mean = mean(dat$score[z]),
               predicted_median = median(values),
               predicted_lower = unname(quantile(values, 0.025)),
               predicted_upper = unname(quantile(values, 0.975)))
  }))
  write.csv(sex_ppc, path("predictive_checks_by_sex.csv"), row.names = FALSE)

  png(path("traceplots.png"), width = 1800, height = 1500, res = 160)
  par(mfrow = c(3, 2), mar = c(3, 4, 3, 1))
  for (nm in fixed_terms) {
    chain_matrix <- matrix(as.numeric(draw_array[, , nm, drop = FALSE]),
                           nrow = dim(draw_array)[1], ncol = dim(draw_array)[2])
    matplot(chain_matrix, type = "l", lty = 1,
            col = c("#4477AA80", "#EE667780", "#22883380", "#AA337780"),
            xlab = "Post-warmup iteration", ylab = "Coefficient", main = nm)
  }
  dev.off()
  png(path("posterior_predictive_distribution.png"), width = 1500, height = 1000,
      res = 160)
  breaks <- seq(0.5, 7.5, by = 1)
  observed <- hist(dat$score, breaks = breaks, plot = FALSE)$counts / nrow(dat)
  predicted <- t(apply(yrep, 1, function(x) {
    vapply(seq(1, 7), function(j) mean(x >= j - 0.5 & x < j + 0.5), numeric(1))
  }))
  lower <- apply(predicted, 2, quantile, 0.025)
  upper <- apply(predicted, 2, quantile, 0.975)
  plot(1:7, observed, pch = 16, ylim = c(0, max(upper, observed) * 1.08),
       xlab = "Rufous-band score", ylab = "Proportion of photographs")
  arrows(1:7, lower, 1:7, upper, angle = 90, code = 3, length = 0.04,
         col = "#4477AA")
  points(1:7, apply(predicted, 2, median), pch = 1, col = "#4477AA", cex = 1.2)
  legend("topright", c("Observed", "Posterior predictive median and 95% interval"),
         pch = c(16, 1), col = c("black", "#4477AA"), bty = "n")
  dev.off()

  after_hash <- tools::md5sum(protected_paths)
  write.csv(data.frame(path = names(after_hash), md5 = unname(after_hash),
                       unchanged = unname(after_hash == before_hash)),
            path("input_hashes_after.csv"), row.names = FALSE)
  writeLines(capture.output(sessionInfo()), path("session_info.txt"))
  writeLines(capture.output(summary(fit)), path("model_summary.txt"))
  log_message("Posterior effects")
  print(effect_summary, row.names = FALSE)
  log_message("Sampler diagnostics")
  print(diagnostics, row.names = FALSE)
  log_message("Predictive checks")
  print(ppc_summary, row.names = FALSE)
  if (!all(after_hash == before_hash)) {
    warning("One or more protected input files changed during this run; see hashes.")
  }
  log_message("Complete. Results remain separate from the Quarto and manuscript.")
}

main()
