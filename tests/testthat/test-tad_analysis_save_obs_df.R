

testthat::test_that("Stats per observation dataframes works", {
  stat_per_obs_dataframe <- do.call(
    TAD:::save_obs_df,
    datasets$good2$param$stat_per_obs_dataframe
  )
  testthat::expect_equal(
    stat_per_obs_dataframe,
    datasets$good2$results$stat_per_obs_dataframe
  )
})
