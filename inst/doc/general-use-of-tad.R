## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----single core processing---------------------------------------------------

library(future)
plan(multisession)
weights <- data.frame(
  sp1 = c(1, 0),
  sp2 = c(2, 8),
  sp3 = c(0, 2)
)
aggregation_factor <- data.frame(
  plots = c("plot1", "plot2")
)

time_before <- proc.time()[[1]]
## This will run in singleprocess mode
str(TAD::generate_random_matrix(
  weights = weights,
  aggregation_factor = aggregation_factor,
  randomization_number = 500
))

## ----multiprocessing----------------------------------------------------------

library(future)
plan(multisession)
weights <- data.frame(
  sp1 = c(1, 0),
  sp2 = c(2, 8),
  sp3 = c(0, 2)
)
aggregation_factor <- data.frame(
  plots = c("plot1", "plot2")
)

time_before <- proc.time()[[1]]
## This will run in multiprocessing mode
str(TAD::generate_random_matrix(
  weights = weights,
  aggregation_factor = aggregation_factor,
  randomization_number = 500
))

## ----cleanup------------------------------------------------------------------
plan(sequential)

## ----tad analysis-------------------------------------------------------------

with_parallelism <- function(x) {
  future::plan(future::multisession)
  on.exit(future::plan(future::sequential))
  force(x)
}

# weights <- TAD::AB[, c(5:102)]
weights <- TAD::AB[, c(5:30)]
with_parallelism(
  result <- TAD::launch_analysis_tad(
    weights = weights,
    weights_factor = TAD::AB[, c("Year", "Plot", "Treatment", "Bloc")],
  trait_data = log(TAD::trait[["SLA"]])[seq_len(ncol(weights))],
    aggregation_factor_name = c("Year", "Bloc"),
    statistics_factor_name = c("Treatment"),
    regenerate_abundance_df = TRUE,
    regenerate_weighted_moments_df = TRUE,
    regenerate_stat_per_obs_df = TRUE,
    regenerate_stat_per_rand_df = TRUE,
    randomization_number = 100,
    seed = 1312,
    significativity_threshold = c(0.05, 0.95)
  )
)

str(result)

