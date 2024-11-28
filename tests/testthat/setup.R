

empty_directory <- function(x) {
  unlink(x, recursive = TRUE, force = TRUE)
  dir.create(x)
}

test_with_dir <- function(desc, ...) {
  withr <- TAD:::load_package("withr")

  new <- tempfile()
  empty_directory(new)
  withr$with_dir(
    new = new,
    code = {
      tmp <- capture.output(
        testthat::test_that(desc = desc, ...)
      )
    }
  )
  invisible(tmp)

}

datasets <- list(
  good1 = list(
    params = list(
      weights = TAD::AB[, 5:102],
      weights_factor = TAD::AB[, c("Year", "Plot", "Treatment", "Bloc")],
      trait_data = log(TAD::trait[["SLA"]]),
      aggregation_factor_name = c("Year", "Bloc"),
      statistics_factor_name = c("Treatment"),
      regenerate_abundance_df = TRUE,
      regenerate_weighted_moments_df = TRUE,
      regenerate_stat_per_obs_df = TRUE,
      regenerate_stat_per_rand_df = TRUE,
      randomization_number = 20,
      seed = 1312,
      significativity_threshold = c(0.05, 0.95),
      lin_mod = "lm",
      slope_distance = TAD::CONSTANTS$SKEW_UNIFORM_SLOPE_DISTANCE,
      intercept_distance = TAD::CONSTANTS$SKEW_UNIFORM_INTERCEPT_DISTANCE,
      abundance_file = NULL,
      weighted_moments_file = NULL,
      stat_per_obs_file = NULL,
      stat_per_rand_file = NULL,
      stat_skr_param_file = NULL
    ),
    weights = data.frame(sp1 = c(1, 0), sp2 = c(2, 8), sp3 = c(0, 2)),
    aggreg_factor = data.frame(plot = c("plot1", "plot2")),
    randomization_number = 3,
    generate_random_matrix_result = list(
      data.frame(
        number = as.integer(c(0, 0, 1, 1, 2, 2, 3, 3)),
        index1 = as.numeric(c(1, 0, 1, 0, 2, 0, 2, 0)),
        index2 = as.numeric(c(2, 8, 2, 2, 1, 2, 1, 2)),
        index3 = as.numeric(c(0, 2, 0, 8, 0, 8, 0, 8))
      ), data.frame(
        number = as.integer(c(0, 0, 1, 1, 2, 2, 3, 3)),
        index1 = as.numeric(c(1, 0, 2, 0, 1, 0, 1, 0)),
        index2 = as.numeric(c(2, 8, 1, 8, 2, 8, 2, 2)),
        index3 = as.numeric(c(0, 2, 0, 2, 0, 2, 0, 8))
      ), data.frame(
        number = as.integer(c(0, 0, 1, 1, 2, 2, 3, 3)),
        index1 = as.numeric(c(1, 0, 1, 0, 1, 0, 1, 0)),
        index2 = as.numeric(c(2, 8, 2, 8, 2, 8, 2, 2)),
        index3 = as.numeric(c(0, 2, 0, 2, 0, 2, 0, 8))
      ), data.frame(
        number = as.integer(c(0, 0, 1, 1, 2, 2, 3, 3)),
        index1 = as.numeric(c(1, 0, 2, 0, 1, 0, 2, 0)),
        index2 = as.numeric(c(2, 8, 1, 8, 2, 8, 1, 2)),
        index3 = as.numeric(c(0, 2, 0, 2, 0, 2, 0, 8))
      ), data.frame(
        number = as.integer(c(0, 0, 1, 1, 2, 2, 3, 3)),
        index1 = as.numeric(c(1, 0, 2, 0, 2, 0, 2, 0)),
        index2 = as.numeric(c(2, 8, 1, 8, 1, 2, 1, 2)),
        index3 = as.numeric(c(0, 2, 0, 2, 0, 8, 0, 8))
      ), data.frame(
        number = as.integer(c(0, 0, 1, 1, 2, 2, 3, 3)),
        index1 = as.numeric(c(1, 0, 1, 0, 1, 0, 2, 0)),
        index2 = as.numeric(c(2, 8, 2, 8, 2, 2, 1, 2)),
        index3 = as.numeric(c(0, 2, 0, 2, 0, 8, 0, 8))
      ), data.frame(
        number = as.integer(c(0, 0, 1, 1, 2, 2, 3, 3)),
        index1 = as.numeric(c(1, 0, 2, 0, 1, 0, 1, 0)),
        index2 = as.numeric(c(2, 8, 1, 8, 2, 2, 2, 2)),
        index3 = as.numeric(c(0, 2, 0, 2, 0, 8, 0, 8))
      ), data.frame(
        number = as.integer(c(0, 0, 1, 1, 2, 2, 3, 3)),
        index1 = as.numeric(c(1, 0, 2, 0, 2, 0, 2, 0)),
        index2 = as.numeric(c(2, 8, 1, 2, 1, 8, 1, 8)),
        index3 = as.numeric(c(0, 2, 0, 8, 0, 2, 0, 2))
      ), data.frame(
        number = as.integer(c(0, 0, 1, 1, 2, 2, 3, 3)),
        index1 = as.numeric(c(1, 0, 2, 0, 1, 0, 1, 0)),
        index2 = as.numeric(c(2, 8, 1, 8, 2, 8, 2, 8)),
        index3 = as.numeric(c(0, 2, 0, 2, 0, 2, 0, 2))
      ), data.frame(
        number = as.integer(c(0, 0, 1, 1, 2, 2, 3, 3)),
        index1 = as.numeric(c(1, 0, 1, 0, 1, 0, 1, 0)),
        index2 = as.numeric(c(2, 8, 2, 8, 2, 8, 2, 8)),
        index3 = as.numeric(c(0, 2, 0, 2, 0, 2, 0, 2))
      )
    )
  ),
  bad1 = list(
    weights1 = data.frame(sp1 = c(1, 0), sp2 = c(2, 8), sp3 = c(0, 2)),
    aggreg_factor = data.frame(plot = c("plot1"))
  ),
  good2 = list(
    param = list(),
    results = list(
      abundance_df = TAD::abundance_dataframe,
      filtering = TAD::filtered_abundances,
      weighted_moments_dataframe = TAD::weighted_moments_dataframe,
      stat_per_obs_dataframe = TAD::stat_per_obs_dataframe,
      stat_per_rand_dataframe = TAD::stat_per_rand_dataframe,
      skr_ses_dataframe = TAD::skr_ses_dataframe
    )
  )
)

get_bad_parameters <- function(...) {
  bad_params <- list(...)
  good_params <- datasets$good1$params
  for (bad_param in names(bad_params)) {
    good_params[[bad_param]] <- bad_params[[bad_param]]
  }
  return(good_params)
}

datasets$good2$param$abundance_df <- list(
  weights = (weights <- TAD::AB[, 5:102]),
  abundance_file = (abundance_file <- NULL),
  weights_factor = (
    weights_factor <- TAD::AB[, c("Year", "Plot", "Treatment", "Bloc")]
  ),
  aggregation_factor_name = c("Year", "Bloc"),
  regenerate_abundance_df = TRUE,
  randomization_number = (randomization_number <- 20),
  seed = 1312
)

datasets$good2$param$filtering <- list(
  abundance_df = datasets$good2$results$abundance_df,
  weights = weights,
  weights_factor = weights_factor,
  trait_data = log(TAD::trait[["SLA"]])
)

datasets$good2$param$weighted_moments <- list(
  weights_factor = datasets$good2$results$filtering$weights_factor,
  trait_data = datasets$good2$results$filtering$trait_data,
  weighted_moments_file = NULL,
  regenerate_weighted_moments_df = TRUE,
  abundance_df = datasets$good2$results$filtering$abundance_df,
  randomization_number = randomization_number,
  slope_distance = TAD::CONSTANTS$SKEW_UNIFORM_SLOPE_DISTANCE,
  intercept_distance = TAD::CONSTANTS$SKEW_UNIFORM_INTERCEPT_DISTANCE
)


datasets$good2$param$stat_per_obs_dataframe <- list(
  weights_factor = datasets$good2$results$filtering$weights_factor,
  stat_per_obs_file = NULL,
  regenerate_stat_per_obs_df = TRUE,
  weighted_moments = datasets$good2$results$weighted_moments,
  randomization_number = randomization_number,
  significativity_threshold = (significativity_threshold <- c(0.05, 0.95))
)

datasets$good2$param$stat_per_rand_dataframe <- list(
  weights = datasets$good2$results$filtering$weights,
  stat_per_rand_file = NULL,
  regenerate_stat_per_rand_df = TRUE,
  statistics_factor_name = (statistics_factor_name <- c("Treatment")),
  weights_factor = datasets$good2$results$filtering$weights_factor,
  randomization_number = randomization_number,
  weighted_moments = datasets$good2$results$weighted_moments,
  abundance_df = datasets$good2$results$filtering$abundance_df,
  lin_mod = "lm"
)

datasets$good2$param$skr_ses_dataframe <- list(
  statistics_factor_name = statistics_factor_name,
  significativity_threshold = significativity_threshold,
  skr_param = datasets$good2$results$stat_per_rand_dataframe,
  stat_skr_param_file = NULL
)
