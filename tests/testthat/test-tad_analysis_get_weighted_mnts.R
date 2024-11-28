

testthat::test_that("Stats per random dataframes works", {
  weighted_moments <- do.call(
    TAD:::get_weighted_mnts,
    datasets$good2$param$weighted_moments
  )
  testthat::expect_equal(
    weighted_moments,
    datasets$good2$results$weighted_moments
  )
})
