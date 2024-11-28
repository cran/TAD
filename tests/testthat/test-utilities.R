
test_with_dir("Saving rds files works", {
  file <- "./test1.rds"
  obj <- list(1, 2, list(3, 4))
  TAD:::save_depending_on_format(file, "obj")
  testthat::expect_true(file.exists(file))
  testthat::expect_false(dir.exists(file))
  testthat::expect_identical(readRDS(file = file), obj)
})


test_with_dir("Saving rda files works", {
  file <- "./test2.rda"
  obj <- list(1, 2, list(3, 4))
  TAD:::save_depending_on_format(file, "obj")
  testthat::expect_true(file.exists(file))
  testthat::expect_false(dir.exists(file))
  load(file, envir = (env <- new.env()))
  testthat::expect_identical(env$obj, obj)
})


test_with_dir("Saving tsv files works", {
  file <- "./test3.tsv"
  abundance_df <- datasets$good1$generate_random_matrix_result[[1]]
  TAD::save_abundance_dataframe(file, abundance_df)
  testthat::expect_true(file.exists(file))
  testthat::expect_false(dir.exists(file))
  result <- TAD:::load_tad_table(
    path = file,
    sep = "\t",
    table_name = "abundance_df"
  )
  testthat::expect_identical(result, abundance_df)
})


test_with_dir("Saving csv files works", {
  file <- "./test4.csv"
  abundance_df <- datasets$good1$generate_random_matrix_result[[1]]
  TAD::save_abundance_dataframe(file, abundance_df)
  testthat::expect_true(file.exists(file))
  testthat::expect_false(dir.exists(file))
  result <- TAD:::load_tad_table(
    path = file,
    sep = ",",
    table_name = "abundance_df"
  )
  testthat::expect_identical(result, abundance_df)
})


test_with_dir("Loading rds files works", {
  file <- "./test5.rds"
  abundance_df <- datasets$good1$generate_random_matrix_result[[1]]
  TAD::save_abundance_dataframe(file, abundance_df)
  testthat::expect_identical(
    TAD::load_abundance_dataframe(path = file),
    abundance_df
  )
})


test_with_dir("Loading rda files works", {
  file <- "./test6.rda"
  abundance_df <- datasets$good1$generate_random_matrix_result[[1]]
  TAD::save_abundance_dataframe(file, abundance_df)
  testthat::expect_identical(
    TAD::load_abundance_dataframe(path = file),
    abundance_df
  )
})


test_with_dir("Loading tsv files works", {
  file <- "./test7.tsv"
  abundance_df <- datasets$good1$generate_random_matrix_result[[1]]
  TAD::save_abundance_dataframe(file, abundance_df)
  testthat::expect_identical(
    TAD::load_abundance_dataframe(path = file),
    abundance_df
  )
})


test_with_dir("Loading csv files works", {
  file <- "./test8.csv"
  abundance_df <- datasets$good1$generate_random_matrix_result[[1]]
  TAD::save_abundance_dataframe(file, abundance_df)
  testthat::expect_identical(
    TAD::load_abundance_dataframe(path = file),
    abundance_df
  )
})


test_with_dir("Saving and loading abundance works", {

  file <- "./abundances.csv"
  abd_df <- datasets$good2$results$abundance_df
  TAD::save_abundance_dataframe(path = file, object = abd_df)
  testthat::expect_identical(
    TAD::load_abundance_dataframe(path = file),
    abd_df,
    tolerance = 10e-15
  )
  file <- "./abundances2.csv"
  abundance_df <- datasets$good2$results$abundance_df
  TAD::save_abundance_dataframe(path = file)
  testthat::expect_identical(
    TAD::load_abundance_dataframe(path = file),
    abundance_df,
    tolerance = 10e-15
  )
})


test_with_dir("Saving and loading weighted moments works", {

  file <- "./weighted_moments.csv"
  abd_df <- datasets$good2$results$weighted_moments_dataframe
  TAD::save_weighted_moments(path = file, object = abd_df)
  testthat::expect_identical(
    TAD::load_weighted_moments(
      path = file,
      factor_names = colnames(
        datasets$good2$param$weighted_moments$weights_factor
      )
    ),
    abd_df,
    tolerance = 10e-15
  )

  file <- "./weighted_moments2.csv"
  weighted_moments <- datasets$good2$results$weighted_moments_dataframe
  TAD::save_weighted_moments(path = file)
  testthat::expect_identical(
    TAD::load_weighted_moments(
      path = file,
      factor_names = colnames(
        datasets$good2$param$weighted_moments$weights_factor
      )
    ),
    weighted_moments,
    tolerance = 10e-15
  )

})


test_with_dir("Saving and loading statistics per obs works", {

  file <- "./statistics_per_obs.csv"
  abd_df <- datasets$good2$results$stat_per_obs_dataframe
  TAD::save_statistics_per_obs(path = file, object = abd_df)
  testthat::expect_identical(
    TAD::load_statistics_per_obs(
      path = file,
      factor_names = colnames(
        datasets$good2$param$weighted_moments$weights_factor
      )
    ),
    abd_df,
    tolerance = 10e-15
  )

  file <- "./statistics_per_obs2.csv"
  statistics_per_observation <- datasets$good2$results$stat_per_obs_dataframe
  TAD::save_statistics_per_obs(path = file)
  testthat::expect_identical(
    TAD::load_statistics_per_obs(
      path = file,
      factor_names = colnames(
        datasets$good2$param$weighted_moments$weights_factor
      )
    ),
    statistics_per_observation,
    tolerance = 10e-15
  )

})


test_with_dir("Saving and loading statistics_per_random works", {

  file <- "./statistics_per_random.csv"
  abd_df <- datasets$good2$results$stat_per_rand_dataframe
  TAD::save_statistics_per_random(path = file, object = abd_df)
  testthat::expect_identical(
    TAD::load_statistics_per_random(path = file),
    abd_df,
    tolerance = 10e-15
  )

  file <- "./statistics_per_random2.csv"
  statistics_per_random <- datasets$good2$results$stat_per_rand_dataframe
  TAD::save_statistics_per_random(path = file)
  testthat::expect_identical(
    TAD::load_statistics_per_random(path = file),
    statistics_per_random,
    tolerance = 10e-15
  )

})


test_with_dir("Saving and loading stat_skr_param works", {

  file <- "./stat_skr_param.csv"
  abd_df <- datasets$good2$results$skr_ses_dataframe
  param <- datasets$good2$param
  TAD::save_stat_skr_param(path = file, object = abd_df)
  testthat::expect_identical(
    TAD::load_stat_skr_param(
      path = file,
      character_names = param$stat_per_rand_dataframe$statistics_factor_name
    ),
    abd_df,
    tolerance = 10e-15
  )

  file <- "./stat_skr_param2.csv"
  stat_skr_param <- datasets$good2$results$skr_ses_dataframe
  TAD::save_stat_skr_param(path = file)
  testthat::expect_identical(
    TAD::load_stat_skr_param(
      path = file,
      character_names = param$stat_per_rand_dataframe$statistics_factor_name
    ),
    stat_skr_param,
    tolerance = 10e-15
  )

})
