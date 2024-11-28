## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----test best strat----------------------------------------------------------

## increase the randomization_number to at least 1000 to have
## better results
## our analysis parameters:
tad_analysis_parameter <- list(
  weights = TAD::AB[, c(5:102)],
  weights_factor = TAD::AB[, c("Year", "Plot", "Treatment", "Bloc")],
  trait_data = log(TAD::trait[["SLA"]]),
  aggregation_factor_name = c("Year", "Bloc"),
  statistics_factor_name = c("Treatment"),
  regenerate_abundance_df = TRUE,
  regenerate_weighted_moments_df = TRUE,
  regenerate_stat_per_obs_df = TRUE,
  regenerate_stat_per_rand_df = TRUE,
  seed = 1312,
  significativity_threshold = c(0.05, 0.95)
)

## We will try different strategies, with different number of randomisations
## with a fixed number of weights (98)
strategies <- list(
  sequencial = future::sequential,
  multisession = future::multisession
)
results_string <- list()

## We run the TAD with 10 and then with 1000 randomisations, with
## multiprocessing and without multiprocessing to see the difference
for (randomization_number in c(10, 1000)) {

  tad_analysis_parameter$randomization_number <- randomization_number

  for (strat in names(strategies)) {

    ## We set the strategy
    future::plan(strategies[[strat]])

    time_before <- proc.time()[[1]]
    do.call(TAD::launch_analysis_tad, tad_analysis_parameter)
    ellapsed_time <- proc.time()[[1]] - time_before

    results_string[[length(results_string) + 1]] <- sprintf(
      "[%s rand - %12s] The TAD Analysis took %s seconds.",
      as.character(randomization_number),
      as.character(strat),
      as.character(ellapsed_time)
    )

  }
  ## Always reset the strategy to sequential after your processing
  future::plan(future::sequential)
}

cat(paste(results_string, collapse = "\n"), "\n")

