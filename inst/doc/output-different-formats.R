## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------

weights <- TAD::AB[, 5:20]
weights_factor <- TAD::AB[, c("Year", "Plot", "Treatment", "Bloc")]
trait_data <- log(TAD::trait[["SLA"]][seq_len(ncol(weights))])
aggregation_factor_name <- c("Year", "Bloc")
statistics_factor_name <- c("Treatment")
regenerate_abundance_df <- TRUE
regenerate_weighted_moments_df <- TRUE
regenerate_stat_per_obs_df <- TRUE
regenerate_stat_per_rand_df <- TRUE
regenerate_stat_skr_df <- TRUE
randomization_number <- 20
seed <- 1312
significativity_threshold <- c(0.025, 0.975)
lin_mod <- "lm"
slope_distance <- 1
intercept_distance <- 1.86

produce_results <- function(
  abundance_file,
  weighted_moments_file,
  stat_per_obs_file,
  stat_per_rand_file,
  stat_skr_param_file
) {

  TAD::launch_analysis_tad(
    weights = weights,
    weights_factor = weights_factor,
    trait_data = trait_data,
    randomization_number = randomization_number,
    aggregation_factor_name = aggregation_factor_name,
    statistics_factor_name = statistics_factor_name,
    seed = seed,
    abundance_file = abundance_file,
    weighted_moments_file = weighted_moments_file,
    stat_per_obs_file = stat_per_obs_file,
    stat_per_rand_file = stat_per_rand_file,
    stat_skr_param_file = stat_skr_param_file,
    regenerate_abundance_df = regenerate_abundance_df,
    regenerate_weighted_moments_df = regenerate_weighted_moments_df,
    regenerate_stat_per_obs_df = regenerate_stat_per_obs_df,
    regenerate_stat_per_rand_df = regenerate_stat_per_rand_df,
    regenerate_stat_skr_df = regenerate_stat_skr_df,
    significativity_threshold = significativity_threshold,
    lin_mod = lin_mod,
    slope_distance = slope_distance,
    intercept_distance = intercept_distance
  )
}

## ----generate CSV outputs-----------------------------------------------------

## We don't especially need multiprocessing
future::plan(future::sequential)

## We define the outputs.
csv_files <- c(
  abundance_csv_file <- "abundance_file.csv",
  weighted_moments_csv_file <- "weighted_moments_file.csv",
  stat_per_obs_csv_file <- "stat_per_obs_file.csv",
  stat_per_rand_csv_file <- "stat_per_rand_file.csv",
  stat_skr_param_csv_file <- "stat_skr_param_file.csv"
)

## We run the analysis and provide the paths to write the results to.
results <- produce_results(
  abundance_file = abundance_csv_file,
  weighted_moments_file = weighted_moments_csv_file,
  stat_per_obs_file = stat_per_obs_csv_file,
  stat_per_rand_file = stat_per_rand_csv_file,
  stat_skr_param_file = stat_skr_param_csv_file
)

## Let's see if all the files have been created: we should
##   see five CSV files here
list.files(pattern = "*.csv", full.names = TRUE)

## ----CSV abundance outputs----------------------------------------------------
abundance_from_csv <- TAD::load_abundance_dataframe(
  abundance_csv_file
)
head(abundance_from_csv[
  c(1:3),
  c(colnames(abundance_from_csv) %in% colnames(abundance_from_csv)[1:8])
])

## ----CSV weighted_moments output----------------------------------------------
weighted_moments_from_csv <- TAD::load_weighted_moments(
  weighted_moments_csv_file,
  factor_names = c("Year", "Plot", "Bloc")
)
head(weighted_moments_from_csv, n = 3)

## ----CSV stat_per_obs output--------------------------------------------------
stat_per_obs_from_csv <- TAD::load_statistics_per_obs(
  stat_per_obs_csv_file,
  factor_names = c("Year", "Plot", "Bloc")
)
head(stat_per_obs_from_csv, n = 3)

## ----CSV stat_per_rand output-------------------------------------------------
stat_per_rand_from_csv <- TAD::load_statistics_per_random(
  stat_per_rand_csv_file,
  factor_names = c("Treatment")
)
head(stat_per_rand_from_csv, n = 3)

## ----CSV stat_skr_param output------------------------------------------------
stat_skr_param_from_csv <- TAD::load_stat_skr_param(
  stat_skr_param_csv_file,
  character_names = c("Treatment")
)
head(stat_skr_param_from_csv, n = 3)

warnings()

## ----generate tsv outputs-----------------------------------------------------

## We don't want multiprocessing
future::plan(future::sequential)

## We define the outputs.
tsv_files <- c(
  abundance_tsv_file <- "abundance_file.tsv",
  weighted_moments_tsv_file <- "weighted_moments_file.tsv",
  stat_per_obs_tsv_file <- "stat_per_obs_file.tsv",
  stat_per_rand_tsv_file <- "stat_per_rand_file.tsv",
  stat_skr_param_tsv_file <- "stat_skr_param_file.tsv"
)

## We run the analysis and provide the paths to write the results to.
results <- produce_results(
  abundance_file = abundance_tsv_file,
  weighted_moments_file = weighted_moments_tsv_file,
  stat_per_obs_file = stat_per_obs_tsv_file,
  stat_per_rand_file = stat_per_rand_tsv_file,
  stat_skr_param_file = stat_skr_param_tsv_file
)

## Let's see if all the files have been created: we should
##   see five tsv files here
list.files(pattern = "*.tsv", full.names = TRUE)

## ----tsv abundance outputs----------------------------------------------------
abundance_from_tsv <- TAD::load_abundance_dataframe(
  abundance_tsv_file
)
head(abundance_from_tsv[
  c(1:3),
  c(colnames(abundance_from_tsv) %in% colnames(abundance_from_tsv)[1:8])
])

## ----tsv weighted_moments output----------------------------------------------
weighted_moments_from_tsv <- TAD::load_weighted_moments(
  weighted_moments_tsv_file,
  factor_names = c("Year", "Plot", "Bloc")
)
head(weighted_moments_from_tsv, n = 3)

## ----tsv stat_per_obs output--------------------------------------------------
stat_per_obs_from_tsv <- TAD::load_statistics_per_obs(
  stat_per_obs_tsv_file,
  factor_names = c("Year", "Plot", "Bloc")
)
head(stat_per_obs_from_tsv, n = 3)

## ----tsv stat_per_rand output-------------------------------------------------
stat_per_rand_from_tsv <- TAD::load_statistics_per_random(
  stat_per_rand_tsv_file,
  factor_names = c("Treatment")
)
head(stat_per_rand_from_tsv, n = 3)

## ----tsv stat_skr_param output------------------------------------------------
stat_skr_param_from_tsv <- TAD::load_stat_skr_param(
  stat_skr_param_tsv_file,
  character_names = c("Treatment")
)
head(stat_skr_param_from_tsv, n = 3)

## ----tsv identical to csv outputs---------------------------------------------
print(identical(abundance_from_tsv, abundance_from_csv))
print(identical(weighted_moments_from_tsv, weighted_moments_from_csv))
print(identical(stat_per_obs_from_tsv, stat_per_obs_from_csv))
print(identical(stat_per_rand_from_tsv, stat_per_rand_from_csv))
print(identical(stat_skr_param_from_tsv, stat_skr_param_from_csv))

## ----generate rda outputs-----------------------------------------------------

## We don't want multiprocessing
future::plan(future::sequential)

## We define the outputs.
rda_files <- c(
  abundance_rda_file <- "abundance_file.rda",
  weighted_moments_rda_file <- "weighted_moments_file.rda",
  stat_per_obs_rda_file <- "stat_per_obs_file.rda",
  stat_per_rand_rda_file <- "stat_per_rand_file.rda",
  stat_skr_param_rda_file <- "stat_skr_param_file.rda"
)

## We run the analysis and provide the paths to write the results to.
results <- produce_results(
  abundance_file = abundance_rda_file,
  weighted_moments_file = weighted_moments_rda_file,
  stat_per_obs_file = stat_per_obs_rda_file,
  stat_per_rand_file = stat_per_rand_rda_file,
  stat_skr_param_file = stat_skr_param_rda_file
)

## Let's see if all the files have been created: we should
##   see five rda files here
list.files(pattern = "*.rda", full.names = TRUE)

## ----rda abundance outputs----------------------------------------------------
abundance_from_rda <- TAD::load_abundance_dataframe(
  abundance_rda_file
)
head(abundance_from_rda[
  c(1:3),
  c(colnames(abundance_from_rda) %in% colnames(abundance_from_rda)[1:8])
])

## ----rda weighted_moments output----------------------------------------------
weighted_moments_from_rda <- TAD::load_weighted_moments(
  weighted_moments_rda_file
)
head(weighted_moments_from_rda, n = 3)

## ----rda stat_per_obs output--------------------------------------------------
stat_per_obs_from_rda <- TAD::load_statistics_per_obs(
  stat_per_obs_rda_file
)
head(stat_per_obs_from_rda, n = 3)

## ----rda stat_per_rand output-------------------------------------------------
stat_per_rand_from_rda <- TAD::load_statistics_per_random(
  stat_per_rand_rda_file
)
head(stat_per_rand_from_rda, n = 3)

## ----rda stat_skr_param output------------------------------------------------
stat_skr_param_from_rda <- TAD::load_stat_skr_param(
  stat_skr_param_rda_file
)
head(stat_skr_param_from_rda, n = 3)

## ----rda identical to csv outputs---------------------------------------------
print(all.equal(abundance_from_rda, abundance_from_csv))
print(all.equal(weighted_moments_from_rda, weighted_moments_from_csv))
print(all.equal(stat_per_obs_from_rda, stat_per_obs_from_csv))
print(all.equal(stat_per_rand_from_rda, stat_per_rand_from_csv))
print(all.equal(stat_skr_param_from_rda, stat_skr_param_from_csv))
print(all.equal(abundance_from_rda, abundance_from_csv))
print(all.equal(weighted_moments_from_rda, weighted_moments_from_csv))
print(all.equal(stat_per_obs_from_rda, stat_per_obs_from_csv))
print(all.equal(stat_per_rand_from_rda, stat_per_rand_from_csv))
print(all.equal(stat_skr_param_from_rda, stat_skr_param_from_csv))

## ----include = FALSE----------------------------------------------------------
file.remove(csv_files)
file.remove(rda_files)
file.remove(tsv_files)

