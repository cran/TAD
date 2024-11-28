
testthat::test_that("Error generate_random_matrix", {
  data <- datasets$bad1
  testthat::expect_error(
    object = TAD::generate_random_matrix(
      weights = data$weights,
      aggregation_factor = data$aggreg_factor,
      randomization_number = 3
    ),
    regexp = paste0(
      "weights and aggregation_factor must have",
      " the same number of rows !"
    )
  )
})

testthat::test_that(
  paste0(
    "[x10] generate_random_matrix seed's parameters",
    " fixes the content"
  ), {
    data <- datasets$good1
    results <- list()
    for (seed in 1:10) {
      results[[seed]] <- TAD::generate_random_matrix(
        weights = data$weights,
        aggregation_factor = data$aggreg_factor,
        randomization_number = data$randomization_number,
        seed = seed
      )
    }
    testthat::expect_equal(
      object = results,
      expected = data$generate_random_matrix_result
    )
  }
)

testthat::test_that(
  paste0(
    "generate_random_matrix null seed",
    " produce any result"
  ), {
    data <- datasets$good1
    results <- list()
    for (seed in 1:10) {
      results[[seed]] <- TAD::generate_random_matrix(
        weights = data$weights,
        aggregation_factor = data$aggreg_factor,
        randomization_number = data$randomization_number
      )
    }
    testthat::expect_equal(
      object = nrow(results),
      expected = nrow(data$generate_random_matrix_result)
    )
    testthat::expect_equal(
      object = ncol(results),
      expected = ncol(data$generate_random_matrix_result)
    )
    testthat::expect_equal(
      object = colnames(results),
      expected = colnames(data$generate_random_matrix_result)
    )
    testthat::expect_equal(
      object = rownames(results),
      expected = rownames(data$generate_random_matrix_result)
    )
  }
)
