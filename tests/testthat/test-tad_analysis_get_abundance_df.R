

testthat::test_that("Abundance dataframes works", {
  abundance_df <- do.call(
    TAD:::get_abundance_df,
    datasets$good2$param$abundance_df
  )
  testthat::expect_equal(
    abundance_df,
    datasets$good2$results$abundance_df
  )
})
