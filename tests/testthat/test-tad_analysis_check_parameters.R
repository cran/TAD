

testthat::test_that("Good parameters are not caught", {
  testthat::expect_no_error(
    do.call(
      TAD:::check_parameters,
      get_bad_parameters()
    )
  )
})

testthat::test_that("Bad parameters are caught for weight", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(weights = 42)
    ),
    regexp = "Bad type for weights. Got numeric, expected data.frame",
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(weights = NULL)
    ),
    regexp = "Bad type for weights. Got NULL, expected data.frame",
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(weights = matrix())
    ),
    regexp = "Bad type for weights. Got matrix, expected data.frame",
    fixed = TRUE
  )
})

testthat::test_that("Bad parameters are caught for weights_factor", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(weights_factor = 42)
    ),
    regexp = "Bad type for weights_factor. Got numeric, expected data.frame",
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(weights_factor = NULL)
    ),
    regexp = "Bad type for weights_factor. Got NULL, expected data.frame",
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(weights_factor = matrix())
    ),
    regexp = "Bad type for weights_factor. Got matrix, expected data.frame",
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(weights_factor = data.frame(x = c(1, 2, 3)))
    ),
    regexp = paste(
      "Bad type for weights_factor[[factor_no]].",
      "Got numeric, expected factor"
    ),
    fixed = TRUE
  )
})

testthat::test_that("Bad parameters are caught for trait_data", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(trait_data = list())
    ),
    regexp = "Bad type for trait_data. Got list, expected numeric",
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(trait_data = NULL)
    ),
    regexp = "Bad type for trait_data. Got NULL, expected numeric",
    fixed = TRUE
  )
})

testthat::test_that("Bad parameters are caught for randomization_number", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(randomization_number = list())
    ),
    regexp = "Bad type for randomization_number. Got list, expected numeric",
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(randomization_number = NULL)
    ),
    regexp = "Bad type for randomization_number. Got NULL, expected numeric",
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(randomization_number = c(1, 2))
    ),
    regexp = paste(
      "Bad value for randomization_number: Expected to be",
      "one numeric, greater than zero, but got <numeric(2)> 1, 2"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(randomization_number = -14)
    ),
    regexp = paste(
      "Bad value for randomization_number: Expected to be",
      "one numeric, greater than zero, but got <numeric(1)> -14"
    ),
    fixed = TRUE
  )
})

testthat::test_that("Bad parameters are caught for aggregation_factor_name", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(aggregation_factor_name = list())
    ),
    regexp = paste(
      "Bad type for aggregation_factor_name.",
      "Got list, expected character"
    ),
    fixed = TRUE
  )
  testthat::expect_no_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(aggregation_factor_name = NULL)
    )
  )
})

testthat::test_that("Bad parameters are caught for statistics_factor_name", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(statistics_factor_name = list())
    ),
    regexp = paste(
      "Bad type for statistics_factor_name.",
      "Got list, expected character"
    ),
    fixed = TRUE
  )
  testthat::expect_no_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(statistics_factor_name = NULL)
    )
  )
})

testthat::test_that("Bad parameters are caught for seed", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(seed = list())
    ),
    regexp = paste(
      "Bad type for seed.",
      "Got list, expected any of double, numeric, logical or null"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(seed = c(1, 2, 3))
    ),
    regexp = paste(
      "Bad value for seed: Expected to be a vector of one element,",
      "but got <numeric(3)> 1, 2, 3"
    ),
    fixed = TRUE
  )
  testthat::expect_no_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(seed = NULL)
    )
  )
})

testthat::test_that("Bad parameters are caught for abundance_file", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(abundance_file = list())
    ),
    regexp = paste(
      "Bad type for abundance_file.",
      "Got list, expected character or null"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(abundance_file = c("1", "2", "3"))
    ),
    regexp = paste(
      "Bad value for abundance_file: Expected to be a vector of",
      "one element or null, but got <character(3)> 1, 2, 3"
    ),
    fixed = TRUE
  )
  testthat::expect_no_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(abundance_file = NULL)
    )
  )
})

testthat::test_that("Bad parameters are caught for weighted_moments_file", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(weighted_moments_file = list())
    ),
    regexp = paste(
      "Bad type for weighted_moments_file.",
      "Got list, expected character or null"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(weighted_moments_file = c("1", "2", "3"))
    ),
    regexp = paste(
      "Bad value for weighted_moments_file: Expected to be a vector of",
      "one element or null, but got <character(3)> 1, 2, 3"
    ),
    fixed = TRUE
  )
  testthat::expect_no_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(weighted_moments_file = NULL)
    )
  )
})

testthat::test_that("Bad parameters are caught for stat_per_obs_file", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(stat_per_obs_file = list())
    ),
    regexp = paste(
      "Bad type for stat_per_obs_file.",
      "Got list, expected character or null"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(stat_per_obs_file = c("1", "2", "3"))
    ),
    regexp = paste(
      "Bad value for stat_per_obs_file: Expected to be a vector of",
      "one element or null, but got <character(3)> 1, 2, 3"
    ),
    fixed = TRUE
  )
  testthat::expect_no_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(stat_per_obs_file = NULL)
    )
  )
})

testthat::test_that("Bad parameters are caught for stat_per_rand_file", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(stat_per_rand_file = list())
    ),
    regexp = paste(
      "Bad type for stat_per_rand_file.",
      "Got list, expected character or null"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(stat_per_rand_file = c("1", "2", "3"))
    ),
    regexp = paste(
      "Bad value for stat_per_rand_file: Expected to be a vector of",
      "one element or null, but got <character(3)> 1, 2, 3"
    ),
    fixed = TRUE
  )
  testthat::expect_no_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(stat_per_rand_file = NULL)
    )
  )
})

testthat::test_that("Bad parameters are caught for stat_skr_param_file", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(stat_skr_param_file = list())
    ),
    regexp = paste(
      "Bad type for stat_skr_param_file.",
      "Got list, expected character or null"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(stat_skr_param_file = c("1", "2", "3"))
    ),
    regexp = paste(
      "Bad value for stat_skr_param_file: Expected to be a vector of",
      "one element or null, but got <character(3)> 1, 2, 3"
    ),
    fixed = TRUE
  )
  testthat::expect_no_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(stat_skr_param_file = NULL)
    )
  )
})

testthat::test_that("Bad parameters are caught for regenerate_abundance_df", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(regenerate_abundance_df = list())
    ),
    regexp = paste(
      "Bad type for regenerate_abundance_df.",
      "Got list, expected logical"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(regenerate_abundance_df = c(TRUE, TRUE, FALSE))
    ),
    regexp = paste(
      "Bad value for regenerate_abundance_df: Expected to be a vector of",
      "one element, but got <logical(3)> TRUE, TRUE, FALSE"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(regenerate_abundance_df = NULL),
    ),
    regexp = paste(
      "Bad type for regenerate_abundance_df.",
      "Got NULL, expected logical"
    ),
    fixed = TRUE
  )
})

testthat::test_that("Bad parameters are caught for regen_weighted_momts_df", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(regenerate_weighted_moments_df = list())
    ),
    regexp = paste(
      "Bad type for regenerate_weighted_moments_df.",
      "Got list, expected logical"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(regenerate_weighted_moments_df = c(TRUE, TRUE, FALSE))
    ),
    regexp = paste(
      "Bad value for regenerate_weighted_moments_df: Expected to be",
      "a vector of one element, but got <logical(3)> TRUE, TRUE, FALSE"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(regenerate_weighted_moments_df = NULL),
    ),
    regexp = paste(
      "Bad type for regenerate_weighted_moments_df.",
      "Got NULL, expected logical"
    ),
    fixed = TRUE
  )
})

testthat::test_that("Bad parameters are caught for regen_stat_per_obs_df", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(regenerate_stat_per_obs_df = list())
    ),
    regexp = paste(
      "Bad type for regenerate_stat_per_obs_df.",
      "Got list, expected logical"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(regenerate_stat_per_obs_df = c(TRUE, TRUE, FALSE))
    ),
    regexp = paste(
      "Bad value for regenerate_stat_per_obs_df: Expected to be a vector of",
      "one element, but got <logical(3)> TRUE, TRUE, FALSE"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(regenerate_stat_per_obs_df = NULL),
    ),
    regexp = paste(
      "Bad type for regenerate_stat_per_obs_df.",
      "Got NULL, expected logical"
    ),
    fixed = TRUE
  )
})

testthat::test_that("Bad parameters are caught for regen_stat_per_rand_df", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(regenerate_stat_per_rand_df = list())
    ),
    regexp = paste(
      "Bad type for regenerate_stat_per_rand_df.",
      "Got list, expected logical"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(regenerate_stat_per_rand_df = c(TRUE, TRUE, FALSE))
    ),
    regexp = paste(
      "Bad value for regenerate_stat_per_rand_df: Expected to be a vector of",
      "one element, but got <logical(3)> TRUE, TRUE, FALSE"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(regenerate_stat_per_rand_df = NULL),
    ),
    regexp = paste(
      "Bad type for regenerate_stat_per_rand_df.",
      "Got NULL, expected logical"
    ),
    fixed = TRUE
  )
})

testthat::test_that("Bad parameters are caught for significativity_threshold", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(significativity_threshold = list())
    ),
    regexp = paste(
      "Bad type for significativity_threshold.",
      "Got list, expected numeric"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(significativity_threshold = NULL)
    ),
    regexp = paste(
      "Bad type for significativity_threshold.",
      "Got NULL, expected numeric"
    ),
    fixed = TRUE
  )
  testthat::expect_no_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(significativity_threshold = c(0, 1))
    )
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(significativity_threshold = c(0.3, 0.5, 0.7))
    ),
    regexp = paste(
      "Bad value for significativity_threshold: Expected to be",
      "exactly two numeric, like this: c(lower, upper)",
      "lower < upper,",
      "and with lower >= 0 and upper <= 1,",
      "but got <numeric(3)> 0.3, 0.5, 0.7"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(significativity_threshold = c(0.7, 0.5))
    ),
    regexp = paste(
      "Bad value for significativity_threshold: Expected to be",
      "exactly two numeric, like this: c(lower, upper)",
      "lower < upper,",
      "and with lower >= 0 and upper <= 1,",
      "but got <numeric(2)> 0.7, 0.5"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(significativity_threshold = c(0.5, 1.5))
    ),
    regexp = paste(
      "Bad value for significativity_threshold: Expected to be",
      "exactly two numeric, like this: c(lower, upper)",
      "lower < upper,",
      "and with lower >= 0 and upper <= 1,",
      "but got <numeric(2)> 0.5, 1.5"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(significativity_threshold = c(-0.5, 0.5))
    ),
    regexp = paste(
      "Bad value for significativity_threshold: Expected to be",
      "exactly two numeric, like this: c(lower, upper)",
      "lower < upper,",
      "and with lower >= 0 and upper <= 1,",
      "but got <numeric(2)> -0.5, 0.5"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(significativity_threshold = c(0.5, 0.5))
    ),
    regexp = paste(
      "Bad value for significativity_threshold: Expected to be",
      "exactly two numeric, like this: c(lower, upper)",
      "lower < upper,",
      "and with lower >= 0 and upper <= 1,",
      "but got <numeric(2)> 0.5, 0.5"
    ),
    fixed = TRUE
  )
})

testthat::test_that("Bad parameters are caught for lin_mod", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(lin_mod = list())
    ),
    regexp = paste(
      "Bad type for lin_mod.",
      "Got list, expected character"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(lin_mod = NULL)
    ),
    regexp = paste(
      "Bad type for lin_mod.",
      "Got NULL, expected character"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(lin_mod = c("lm", "mblm"))
    ),
    regexp = paste(
      "Bad value for lin_mod: Expected to be exactly one of lm, mblm,",
      "but got <character(2)> lm, mblm"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(lin_mod = c("lm", "lm"))
    ),
    regexp = paste(
      "Bad value for lin_mod: Expected to be exactly one of lm, mblm,",
      "but got <character(2)> lm, lm"
    ),
    fixed = TRUE
  )
})

testthat::test_that("Bad parameters are caught for slope_distance", {
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(slope_distance = list())
    ),
    regexp = paste(
      "Bad type for slope_distance.",
      "Got list, expected numeric"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(slope_distance = NULL)
    ),
    regexp = paste(
      "Bad type for slope_distance.",
      "Got NULL, expected numeric"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(slope_distance = c(14.5, 147.2))
    ),
    regexp = paste(
      "Bad value for slope_distance: Expected to be one",
      "numeric, greater than zero, but got <numeric(2)> 14.5, 147.2"
    ),
    fixed = TRUE
  )
  testthat::expect_error(
    object = do.call(
      TAD:::check_parameters,
      get_bad_parameters(slope_distance = -1.7)
    ),
    regexp = paste(
      "Bad value for slope_distance: Expected to be one",
      "numeric, greater than zero, but got <numeric(1)> -1.7"
    ),
    fixed = TRUE
  )
})
