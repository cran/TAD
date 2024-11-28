

testthat::test_that("Stats per random dataframes works", {
  stat_per_rand_dataframe <- do.call(
    TAD:::get_stat_per_rand,
    datasets$good2$param$stat_per_rand_dataframe
  )
  testthat::expect_equal(
    stat_per_rand_dataframe,
    datasets$good2$results$stat_per_rand_dataframe
  )
})
