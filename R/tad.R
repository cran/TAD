#' @importFrom stats sd
#' @importFrom doFuture %dofuture%
#' @importFrom foreach foreach
#' @importFrom utils read.table write.table
#' @importFrom methods is
NULL

#nolint start: object_name_linter object_length_linter

#' @title The CONSTANTS constant
#' @description
#' Provides a set of constants to prevent typo and provide some defauts values
#' to functions in the TAD.
#' Among those constants are:
#'   - SKEW_UNIFORM_SLOPE_DISTANCE
#'   - SKEW_UNIFORM_INTERCEPT_DISTANCE
#'   - DEFAULT_SIGNIFICATIVITY_THRESHOLD
#'   - DEFAULT_LIN_MOD
#'   - DEFAULT_SLOPE_DISTANCE
#'   - DEFAULT_INTERCEPT_DISTANCE
#' @export
CONSTANTS <- list(
  SKEW_UNIFORM_SLOPE_DISTANCE=1,
  SKEW_UNIFORM_INTERCEPT_DISTANCE=1.86,
  DEFAULT_SIGNIFICATIVITY_THRESHOLD=c(0.025, 0.975),
  DEFAULT_LIN_MOD="lm"
)
CONSTANTS$DEFAULT_SLOPE_DISTANCE <- CONSTANTS$SKEW_UNIFORM_SLOPE_DISTANCE
CONSTANTS$DEFAULT_INTERCEPT_DISTANCE <- CONSTANTS$SKEW_UNIFORM_INTERCEPT_DISTANCE

#nolint end

lockBinding("CONSTANTS", environment())


#' @title Generate random matrix
#' @description Generate and save random matrix
#' @concept tad
#' @param weights the dataframe of weights, one row correspond to a
#'   series of observation
#' @param aggregation_factor the dataframe of factor to take into account for
#'   the randomization
#' @param randomization_number the number of random abundance matrix to
#'   generate
#' @param seed the seed of the pseudo random number generator
#' @return a data.frame of randomization_number observations
#' @examples
#' aggregation_factor_name <- c("Year", "Bloc")
#' weights_factor = TAD::AB[, c("Year", "Plot", "Treatment", "Bloc")]
#' aggregation_factor <- as.data.frame(
#'   weights_factor[, aggregation_factor_name]
#' )
#' random_matrix <- TAD::generate_random_matrix(
#'   weights = TAD::AB[, 5:102],
#'   aggregation_factor = aggregation_factor,
#'   randomization_number = 100,
#'   seed = 1312
#' )
#' head(random_matrix)
#' @export
generate_random_matrix <- function(
  weights,
  aggregation_factor = NULL,
  randomization_number,
  seed = NULL
) {
  # Construct the id for aggregation
  if (!is.null(aggregation_factor)) {
    if (nrow(weights) != nrow(aggregation_factor)) {
      stop(
        "weights and aggregation_factor must",
        " have the same number of rows !"
      )
    }
    if (is.data.frame(aggregation_factor)) {
      aggregation_id <- apply(aggregation_factor, 1, paste, collapse = "_")
    } else {
      aggregation_id <- aggregation_factor
    }
  } else {
    # not working with empty id
    aggregation_id <- rep(x = "_", times = nrow(weights))
  }
  # Construct a list which contains for each aggregation factor the
  # valid weight index, i.e. the sum of weight is not equal to 0
  aggregation_factor_index_list <- list()
  for (ag_factor in unique(aggregation_id)) {
    aggregation_factor_index_list[[ag_factor]] <- as.vector(which(
      colSums(weights[which(aggregation_id == ag_factor), ]) != 0
    ))
  }
  # Set seed for the Pseudo Random number Generator

  futur_option <- list()
  if (is.null(seed)) {
    futur_option$seed <- TRUE
  } else {
    futur_option$seed <- seed
  }
  rand_number <- 0
  # Generation of the random matrix
  weights_df <- foreach( ## nolint
    rand_number = c(0:randomization_number),
    .combine = "rbind",
    .options.future = futur_option
  ) %dofuture% {  ## nolint
    # Creation of the dataframe which receive the random
    # weights (regarding aggregation factor)
    dataframe_to_return <- data.frame(matrix(
      data = 0,
      nrow = nrow(weights),
      ncol = ncol(weights) + 1
    ))
    colnames(dataframe_to_return) <- c(
      "number",
      paste0("index", seq_len(ncol(weights)))
    )
    dataframe_to_return$number <- rand_number

    # if rand_number = 0, put the original weights data,
    # otherwise weights are shuffle randomly regarding valid index
    if (rand_number == 0) {
      dataframe_to_return[
        seq_len(nrow(weights)),
        2:ncol(dataframe_to_return)
      ] <- weights
    } else {
      for (weights_line_number in seq_len(nrow(weights))){
        index <- aggregation_factor_index_list[[
          aggregation_id[weights_line_number]
        ]]
        dataframe_to_return[weights_line_number, 1 + index] <- weights[
          weights_line_number,
          sample(index, replace = FALSE)
        ]
      }
    }
    return(dataframe_to_return)
  }
  return(weights_df)
}

#' @title parameters checkings
#' @description Checks all parameters of the TAD and raises errors if
#'   parameters' values are incoherent.
#' @inheritParams launch_analysis_tad
#' @keywords internal
check_parameters <- function(
  weights = NULL,
  weights_factor = NULL,
  trait_data = NULL,
  randomization_number = NULL,
  aggregation_factor_name = NULL,
  statistics_factor_name = NULL,
  seed = NULL,
  abundance_file = NULL,
  weighted_moments_file = NULL,
  stat_per_obs_file = NULL,
  stat_per_rand_file = NULL,
  stat_skr_param_file = NULL,
  regenerate_abundance_df = NULL,
  regenerate_weighted_moments_df = NULL,
  regenerate_stat_per_obs_df = NULL,
  regenerate_stat_per_rand_df = NULL,
  significativity_threshold = NULL,
  lin_mod = NULL,
  slope_distance = NULL,
  intercept_distance = NULL,
  csv_tsv_load_parameters = NULL
) {
  # preliminary test on input data
  check_parameter_type(weights, "data.frame")
  check_parameter_type(weights_factor, "data.frame")
  for (factor_no in seq_along(weights_factor)) {
    check_parameter_type(weights_factor[[factor_no]], "factor")
  }
  check_parameter_type(trait_data, "numeric")
  check_parameter_value(
    value = nrow(weights),
    checker = function(x) x == nrow(weights_factor),
    expected_description = "the same value as nrow(weights_factor)"
  )
  check_parameter_value(
    value = ncol(weights),
    checker = function(x) x == length(trait_data),
    expected_description = sprintf(
      "the same value as length(trait_data<%s>)", length(trait_data)
    )
  )
  check_parameter_type(randomization_number, "numeric")
  check_parameter_value(
    value = randomization_number,
    checker = function(x) length(x) == 1 && x > 0,
    expected_description = "one numeric, greater than zero"
  )
  check_parameter_type(aggregation_factor_name, "character", or_null = TRUE)
  check_parameter_type(statistics_factor_name, "character", or_null = TRUE)
  check_parameter_type(seed, c("double", "numeric", "logical"), or_null = TRUE)
  check_parameter_value(
    value = seed,
    checker = function(x) length(x) == 1,
    expected_description = "a vector of one element"
  )
  check_parameter_type(abundance_file, "character", or_null = TRUE)
  check_parameter_value(
    value = abundance_file,
    checker = function(x) length(x) == 1,
    expected_description = "a vector of one element or null"
  )
  check_parameter_type(weighted_moments_file, "character", or_null = TRUE)
  check_parameter_value(
    value = weighted_moments_file,
    checker = function(x) length(x) == 1,
    expected_description = "a vector of one element or null"
  )
  check_parameter_type(stat_per_obs_file, "character", or_null = TRUE)
  check_parameter_value(
    value = stat_per_obs_file,
    checker = function(x) length(x) == 1,
    expected_description = "a vector of one element or null"
  )
  check_parameter_type(stat_per_rand_file, "character", or_null = TRUE)
  check_parameter_value(
    value = stat_per_rand_file,
    checker = function(x) length(x) == 1,
    expected_description = "a vector of one element or null"
  )
  check_parameter_type(stat_skr_param_file, "character", or_null = TRUE)
  check_parameter_value(
    value = stat_skr_param_file,
    checker = function(x) length(x) == 1,
    expected_description = "a vector of one element or null"
  )
  check_parameter_type(regenerate_abundance_df, "logical")
  check_parameter_value(
    value = regenerate_abundance_df,
    checker = function(x) length(x) == 1,
    expected_description = "a vector of one element"
  )
  check_parameter_type(regenerate_weighted_moments_df, "logical")
  check_parameter_value(
    value = regenerate_weighted_moments_df,
    checker = function(x) length(x) == 1,
    expected_description = "a vector of one element"
  )
  check_parameter_type(regenerate_stat_per_obs_df, "logical")
  check_parameter_value(
    value = regenerate_stat_per_obs_df,
    checker = function(x) length(x) == 1,
    expected_description = "a vector of one element"
  )
  check_parameter_type(regenerate_stat_per_rand_df, "logical")
  check_parameter_value(
    value = regenerate_stat_per_rand_df,
    checker = function(x) length(x) == 1,
    expected_description = "a vector of one element"
  )
  check_parameter_type(significativity_threshold, "numeric")
  check_parameter_value(
    value = significativity_threshold,
    checker = function(x) {
      return(
        length(x) == 2
        && x[[1]] < x[[2]]
        && x[[1]] >= 0
        && x[[2]] <= 1
      )
    },
    expected_description = paste(
      "exactly two numeric, like this: c(lower, upper)",
      "lower < upper,",
      "and with lower >= 0 and upper <= 1"
    )
  )
  check_parameter_type(lin_mod, "character")
  check_parameter_value(
    value = lin_mod,
    checker = function(x) length(x) == 1 && x %in% c("lm", "mblm"),
    expected_description = "exactly one of lm, mblm"
  )
  check_parameter_type(slope_distance, "numeric")
  check_parameter_value(
    value = slope_distance,
    checker = function(x) length(x) == 1 && x > 0,
    expected_description = "one numeric, greater than zero"
  )
}

#' @title abundance generation
#' @inheritParams launch_analysis_tad
#' @keywords internal
get_abundance_df <- function(
  weights,
  weights_factor,
  randomization_number,
  abundance_file,
  regenerate_abundance_df,
  aggregation_factor_name,
  seed
) {
  # Generate or load random matrix
  if (
    is.null(abundance_file)
    || (
      !is.null(abundance_file)
      && !file.exists(abundance_file)
    ) || regenerate_abundance_df
  ) {

    if (is.null(aggregation_factor_name)) {
      aggregation_factor <- NULL
    } else {
      aggregation_factor <- as.data.frame(
        weights_factor[, aggregation_factor_name]
      )
    }
    abundance_df <- generate_random_matrix(
      weights = weights,
      aggregation_factor = aggregation_factor,
      randomization_number = randomization_number,
      seed = seed
    )
    # save the result
    if (!is.null(abundance_file)) {
      save_abundance_dataframe(abundance_file)
    }
  } else {
    abundance_df <- load_abundance_dataframe(abundance_file)
  }
  return(abundance_df)
}

#' @title input filter
#' @inheritParams launch_analysis_tad
#' @keywords internal
filter_na_empty <- function(
  abundance_df,
  # abundance_df = abundance_df,
  weights,
  # weights = weights,
  weights_factor,
  # weights_factor = weights_factor,
  trait_data
  # trait_data = trait_data
) {

  # Remove the species which have no trait value
  species_to_remove <- which(is.na(trait_data))

  if (length(species_to_remove) != 0) {
    trait_data <- trait_data[-species_to_remove]
    weights <- weights[, -species_to_remove]
    abundance_df <- abundance_df[, -(1 + species_to_remove)]
  }

  # Remove the observation with a total abundance of 0
  weights_to_remove <- which(rowSums(weights) == 0)

  if (length(weights_to_remove) != 0) {
    weights_factor <- weights_factor[-weights_to_remove, ]
    weights <- weights[-weights_to_remove, ]
  }

  abundance_to_remove <- which(
    rowSums(abundance_df[, 2:ncol(abundance_df)]) == 0
  )

  if (length(abundance_to_remove) != 0) {
    abundance_df <- abundance_df[-abundance_to_remove, ]
  }
  return(list(
    abundance_df = abundance_df,
    weights = weights,
    weights_factor = weights_factor,
    trait_data = trait_data
  ))
}

#' @title weighted moments generation
#' @inheritParams launch_analysis_tad
#' @keywords internal
get_weighted_mnts <- function(
  weights_factor,
  trait_data,
  weighted_moments_file,
  regenerate_weighted_moments_df,
  abundance_df,
  randomization_number,
  slope_distance,
  intercept_distance,
  factor_names = NULL
) {
  # Generate or load moments dataframe
  if (
    is.null(weighted_moments_file)
    || (
      !is.null(weighted_moments_file)
      && !file.exists(weighted_moments_file)
    ) || regenerate_weighted_moments_df
  ) {
    # Compute for each line the weighted mean, variance, skewness,
    # kurtosis and distance to lower boundary
    weighted_moments_list <- weighted_mvsk(
      data = trait_data,
      weights = as.matrix(
        abundance_df[, c(2:(length(trait_data) + 1))]
      )
    )

    # Create a dataframe with the weighted moments and save it
    weighted_moments <- data.frame(matrix(
      data = NA,
      ncol = 0,
      nrow = nrow(abundance_df)
    ))
    weighted_moments$number <- abundance_df$number
    weighted_moments$mean <- weighted_moments_list[["mean"]]
    weighted_moments$variance <- weighted_moments_list[["variance"]]
    weighted_moments$skewness <- weighted_moments_list[["skewness"]]
    weighted_moments$kurtosis <- weighted_moments_list[["kurtosis"]]
    rm(weighted_moments_list)
    distance_law <- (
      weighted_moments$kurtosis - (
        slope_distance
        * weighted_moments$skewness
        * weighted_moments$skewness
        + intercept_distance
      )
    )
    weighted_moments$distance_law <- distance_law
    weighted_moments <- cbind(
      weights_factor[
        rep(
          x = seq_len(nrow(weights_factor)),
          times = randomization_number + 1
        ),
        ,
        drop = FALSE
      ],
      weighted_moments
    )

    if (!is.null(weighted_moments_file)) {
      save_weighted_moments(weighted_moments_file)
    }
  } else {
    weighted_moments <- load_weighted_moments(
      weighted_moments_file,
      factor_names = factor_names
    )
  }
  return(weighted_moments)
}

#' @title observations genration/save/load
#' @inheritParams launch_analysis_tad
#' @keywords internal
save_obs_df <- function(
  weights_factor,
  stat_per_obs_file,
  regenerate_stat_per_obs_df,
  weighted_moments,
  randomization_number,
  significativity_threshold
) {
  # Generate or load statistics per observation dataframe
  if (
    is.null(stat_per_obs_file)
    || (
      !is.null(stat_per_obs_file)
      && !file.exists(stat_per_obs_file)
    ) || regenerate_stat_per_obs_df
  ) {
    # compute statistics for null model for mean/var/skewness/kurtosis
    statistics_per_observation <- weights_factor

    for (i in seq_len(nrow(statistics_per_observation))) {
      statistics_per_observation[
        i,
        c((ncol(weights_factor) + 1):(ncol(weights_factor) + 4))
      ] <- null_model_distribution_stats(
        observed_value = weighted_moments$mean[i],
        random_values = weighted_moments$mean[
          c(1:randomization_number) * nrow(statistics_per_observation) + i
        ],
        significance_threshold = significativity_threshold
      )
      statistics_per_observation[
        i,
        c((ncol(weights_factor) + 5):(ncol(weights_factor) + 8))
      ] <- null_model_distribution_stats(
        observed_value = weighted_moments$variance[i],
        random_values = weighted_moments$variance[
          c(1:randomization_number) * nrow(statistics_per_observation) + i
        ],
        significance_threshold = significativity_threshold
      )
      statistics_per_observation[
        i,
        (ncol(weights_factor) + 9):(ncol(weights_factor) + 12)
      ] <- null_model_distribution_stats(
        observed_value = weighted_moments$skewness[i],
        random_values = weighted_moments$skewness[
          c(1:randomization_number) * nrow(statistics_per_observation) + i
        ],
        significance_threshold = significativity_threshold
      )
      statistics_per_observation[
        i,
        (ncol(weights_factor) + 13):(ncol(weights_factor) + 16)
      ] <- null_model_distribution_stats(
        observed_value = weighted_moments$kurtosis[i],
        random_values = weighted_moments$kurtosis[
          c(1:randomization_number) * nrow(statistics_per_observation) + i
        ],
        significance_threshold = significativity_threshold
      )
    }
    common_col_name <- c(
      "standardized_observed",
      "standardized_min_quantile",
      "standardized_max_quantile",
      "significance"
    )
    colnames(statistics_per_observation) <- c(
      colnames(weights_factor),
      paste0(common_col_name, "mean"),
      paste0(common_col_name, "variance"),
      paste0(common_col_name, "skewness"),
      paste0(common_col_name, "kurtosis")
    )

    if (!is.null(stat_per_obs_file)) {
      save_statistics_per_obs(stat_per_obs_file)
    }
  } else {
    statistics_per_observation <- load_statistics_per_obs(stat_per_obs_file)
  }
  return(statistics_per_observation)
}

#' @title stats per random generation
#' @inheritParams launch_analysis_tad
#' @keywords internal
generate_stat_per_rand <- function(
  weights,
  statistics_factor_name,
  weights_factor,
  weighted_moments,
  randomization_number,
  abundance_df,
  lin_mod
) {
  if (lin_mod == "lm") {
    lin_function <- stats::lm
  } else if (lin_mod == "mblm") {
    lin_function <- mblm::mblm
  } else {
    stop(sprintf(
      paste0(
        "Internal error: generate_stat_per_rand had to manage with %s value",
        " of lin_mod parameter but it did not. Please, fill an issue",
        " assigned to the devs."
      ),
      lin_mod
    ))
  }
  # Construct the id for statistics
  if (!is.null(statistics_factor_name)) {
    statistics_id <- apply(
      as.data.frame(weights_factor[, statistics_factor_name]),
      1,
      paste,
      collapse = "_"
    )
  } else {
    statistics_id <- rep(x = "_", times = nrow(weights_factor))
  }

  # Construct a list which contains for each statistics factor the
  # species which are valid, i.e. the sum of abundance is not equal to 0
  statistics_factor_species_list <- list()
  for (stat_factor in unique(statistics_id)) {
    statistics_factor_species_list[[stat_factor]] <- as.vector(which(
      colSums(weights[which(statistics_id == stat_factor), ]) != 0
    ))
  }

  # Generate the analysis per null model regarding the
  # factor given in parameter
  statistics_per_random <- data.frame(matrix(
    data = NA,
    nrow = (
      (randomization_number + 1)
      * length(statistics_factor_species_list)
    ),
    ncol = 0
  ))
  length_factor <- length(names(statistics_factor_species_list))
  abundance_df$skewness <- weighted_moments$skewness
  abundance_df$kurtosis <- weighted_moments$kurtosis
  abundance_df$distance_law <- weighted_moments$distance_law
  for (i in c(0:randomization_number)) {
    for (j in seq_len(length_factor)) {
      index <- i * length_factor + j

      statistics_per_random$number[index] <- i

      statistics_per_random[index, statistics_factor_name] <- weights_factor[
        which(statistics_id == names(statistics_factor_species_list)[j])[1],
        statistics_factor_name
      ]

      df_to_analyze <- abundance_df[
        which(abundance_df$number == i),
      ]
      df_to_analyze <- df_to_analyze[
        which(statistics_id == names(statistics_factor_species_list)[j]),
      ]
      y <- df_to_analyze$kurtosis
      x <- df_to_analyze$skewness ^ 2  # nolint: object_usage_linter
      dist_law <- df_to_analyze$distance_law ^ 2


      fit <- lin_function(y ~ x)
      statistics_per_random$slope[index] <- fit$coefficients[2]
      statistics_per_random$intercept[index] <- fit$coefficients[1]
      statistics_per_random$rsquare[index] <- 1 - (
        mean(stats::residuals(fit) ^ 2, na.rm = TRUE)
        / stats::var(y, na.rm = TRUE)
      )
      statistics_per_random$tad_stab[index] <- sqrt(mean(
        fit$residuals ^ 2,
        na.rm = TRUE
      ))
      statistics_per_random$distance_to_family[index] <- sqrt(
        mean(dist_law, na.rm = TRUE)
      )
      statistics_per_random$cv_distance_to_family[index] <- (
        sd(dist_law, na.rm = TRUE) * 100
        / mean(dist_law, na.rm = TRUE)
      )
    }
  }
  return(statistics_per_random)
}

#' @title stats per random genration/save/load
#' @keywords internal
get_stat_per_rand <- function(
  weights,
  stat_per_rand_file,
  regenerate_stat_per_rand_df,
  statistics_factor_name,
  weights_factor,
  randomization_number,
  weighted_moments,
  abundance_df,
  lin_mod
) {
  # Generate or load statistics per random dataframe
  if (
    is.null(stat_per_rand_file)
    || (
      !is.null(stat_per_rand_file)
      && !file.exists(stat_per_rand_file)
    ) || regenerate_stat_per_rand_df
  ) {
    statistics_per_random <- generate_stat_per_rand(
      weights = weights,
      statistics_factor_name = statistics_factor_name,
      weights_factor = weights_factor,
      weighted_moments = weighted_moments,
      randomization_number = randomization_number,
      abundance_df = abundance_df,
      lin_mod = lin_mod
    )
    if (!is.null(stat_per_rand_file)) {
      save_statistics_per_random(stat_per_rand_file)
    }
  } else {
    statistics_per_random <- load_statistics_per_random(stat_per_rand_file)
  }
  return(statistics_per_random)
}

#' @title skr ses genration/save/load
#' @keywords internal
build_skr_ses <- function( #nolint
  statistics_factor_name,
  significativity_threshold,
  stat_per_rand_file = NULL,
  skr_param = NULL,
  stat_skr_param_file = NULL,
  regenerate_ses_skr = FALSE,
  skew_uniform = FALSE
) {
  if (
    ! is.null(stat_skr_param_file)
    && file.exists(stat_skr_param_file)
    && ! regenerate_ses_skr
  ) {
    return(load_stat_skr_param(
      stat_skr_param_file,
      factor_names = statistics_factor_name
    ))
  }
  if (is.null(skr_param)) {
    if (is.null(stat_per_rand_file)) {
      stop(
        "Neither the stat_per_rand_file or the skr_param",
        " was provided. Please, provide one of those."
      )
    }
    skr_param <- load_statistics_per_random(stat_per_rand_file)
  }
  ses_skr <- data.frame()
  for (i in unique(skr_param[[statistics_factor_name]])) {
    zero_skreu_param <- skr_param[
      skr_param[[statistics_factor_name]] == i
      & skr_param$number == 0,
    ]
    greater_zero_skreu_param <- skr_param[
      skr_param$number > 0,
    ]
    rand_matrixes <- list()
    for (parameter in c(
      "slope",
      "intercept",
      "rsquare",
      "tad_stab",
      "distance_to_family",
      "cv_distance_to_family"
    )) {
      rand_matrixes[[parameter]] <- null_model_distribution_stats(
        observed_value = zero_skreu_param[[parameter]],
        random_values = greater_zero_skreu_param[[parameter]],
        significance_threshold = significativity_threshold
      )
    }
    tmp_result <- data.frame(
      slope_ses = rand_matrixes$slope[[1]][1],
      slope_signi = rand_matrixes$slope[[4]][1],
      intercept_ses = rand_matrixes$intercept[[1]][1],
      intercept_signi = rand_matrixes$intercept[[4]][1],
      rsquare_ses = rand_matrixes$rsquare[[1]][1],
      rsquare_signi = rand_matrixes$rsquare[[4]][1],
      tad_stab_ses = rand_matrixes$tad_stab[[1]][1],
      tad_stab_signi = rand_matrixes$tad_stab[[4]][1]
    )
    if (skew_uniform) {
      tmp_result$tad_eve_ses <- rand_matrixes$distance_to_family[[1]][1]
      tmp_result$tad_eve_signi <- rand_matrixes$distance_to_family[[4]][1]
      tmp_result$cv_tad_eve_ses <- rand_matrixes$cv_distance_to_family[[1]][1]
      tmp_result$cv_tad_eve_signi <- rand_matrixes$cv_distance_to_family[[4]][1]
    } else {
      tmp_result$distance_to_family_ses <- (
        rand_matrixes$distance_to_family[[1]][1]
      )
      tmp_result$distance_to_family_signi <- (
        rand_matrixes$distance_to_family[[4]][1]
      )
      tmp_result$cv_distance_to_family_ses <- (
        rand_matrixes$cv_distance_to_family[[1]][1]
      )
      tmp_result$cv_distance_to_family_signi <- (
        rand_matrixes$cv_distance_to_family[[4]][1]
      )
    }
    tmp_result$statistics_factor_name <- i
    ses_skr <- rbind(ses_skr, tmp_result)
  }
  names(ses_skr)[
    names(ses_skr) == "statistics_factor_name"
  ] <- statistics_factor_name

  if (! is.null(stat_skr_param_file)) {
    save_stat_skr_param(
      stat_skr_param_file,
      object = skr_custom_uniform_names(ses_skr)
    )
  }
  return(ses_skr)
}

#' @title Launch the analysis
#' @description Launch distribution analysis
#' @concept tad
#' @param weights the dataframe of weights, one row correspond to a
#'   series of observation
#' @param weights_factor the dataframe which contains the different
#'   factor linked to the weights
#' @param trait_data a vector of the data linked to the different factor
#' @param randomization_number the number of random abundance matrix to
#'   generate
#' @param aggregation_factor_name vector of factor name for the
#'   generation of random matrix
#' @param statistics_factor_name vector of factor name for the
#'   computation of statistics for each generated matrix
#' @param seed the seed of the pseudo random number generator
#' @param abundance_file the path and name of the RDS file to
#'   load/save the dataframe which
#'   contains the observed data and the generated matrix
#' @param weighted_moments_file the path and name of the RDS file to
#'   load/save the dataframe which
#'   contains the calculated moments
#' @param stat_per_obs_file the path and name of the RDS file to
#'   load/save the dataframe which
#'   contains the statistics for each observed row regarding the random ones
#' @param stat_per_rand_file the path and name of the RDS file to
#'   load/save the dataframe which
#'   contains the statistics for each random matrix generated
#' @param regenerate_abundance_df boolean to specify if the
#'   abundance dataframe is computed again
#' @param regenerate_weighted_moments_df boolean to specify if
#'   the weighted moments dataframe is computed again
#' @param regenerate_stat_per_obs_df boolean to specify if
#'   the statistics per observation dataframe is computed again
#' @param regenerate_stat_per_rand_df boolean to specify if
#'   the statistics per random matrix dataframe is computed again
#' @param regenerate_stat_skr_df boolean to specify if
#'   the stats SKR dataframe is computed again
#' @param significativity_threshold the significance threshold to
#'   consider that the observed value is in the randomized value
#' @param lin_mod Indicates the type of linear model to use for
#'   (SKR): choose "lm" or "mblm"
#' @param slope_distance slope of the theoretical distribution
#'   law (default: slope = 1 intercept = 1.86 skew-uniform distribution family)
#' @param intercept_distance intercept of the theoretical distribution
#'   law (default: slope = 1 intercept = 1.86 skew-uniform distribution family)
#' @param stat_skr_param_file default=NULL You can provide the output to write
#'   the SKR statistics results to.
#' @param csv_tsv_load_parameters a list of parameters for each data structure
#'   we want to load. Each element must be named after the data structure
#'   we want to load.
#' @return A \code{list} of the 9 following named elements:
#' \itemize{
#'   \item raw_abundance_df
#'   \item filtered_weights
#'   \item filtered_weights_factor
#'   \item filtered_trait_data
#'   \item abundance_df
#'   \item weighted_moments
#'   \item statistics_per_observation
#'   \item stat_per_rand
#'   \item ses_skr
#' }
#'
#' @examples
#'
#'   output_path <- file.path(tempdir(), "outputs")
#'   dir.create(output_path)
#'   results <- TAD::launch_analysis_tad(
#'     weights = TAD::AB[, 5:102],
#'     weights_factor = TAD::AB[, c("Year", "Plot", "Treatment", "Bloc")],
#'     trait_data = log(TAD::trait[["SLA"]]),
#'     aggregation_factor_name = c("Year", "Bloc"),
#'     statistics_factor_name = (statistics_factor_name <- c("Treatment")),
#'     regenerate_abundance_df = TRUE,
#'     regenerate_weighted_moments_df = TRUE,
#'     regenerate_stat_per_obs_df = TRUE,
#'     regenerate_stat_per_rand_df = TRUE,
#'     weighted_moments_file = file.path(output_path, "weighted_moments.csv"),
#'     stat_per_obs_file = file.path(output_path, "stat_per_obs.csv"),
#'     stat_per_rand_file = file.path(output_path, "stat_per_rand.csv"),
#'     stat_skr_param_file = file.path(output_path, "stat_skr_param.csv"),
#'     randomization_number = 20,
#'     seed = 1312,
#'     significativity_threshold = c(0.05, 0.95),
#'     lin_mod = "lm",
#'     slope_distance = (
#'       slope_distance <- TAD::CONSTANTS$SKEW_UNIFORM_SLOPE_DISTANCE
#'     ),
#'     intercept_distance = (
#'       intercept_distance <- TAD::CONSTANTS$SKEW_UNIFORM_INTERCEPT_DISTANCE
#'     )
#'   )
#'   moments_graph <- TAD::moments_graph(
#'     moments_df = results$weighted_moments,
#'     statistics_per_observation = results$statistics_per_observation,
#'     statistics_factor_name = statistics_factor_name,
#'     statistics_factor_name_breaks = c("Mown_Unfertilized", "Mown_NPK"),
#'     statistics_factor_name_col = c("#1A85FF", "#D41159"),
#'     output_path = file.path(output_path, "moments_graph.jpeg"),
#'     dpi = 100
#'   )
#'   skr_graph <- TAD::skr_graph(
#'     moments_df = results$weighted_moments,
#'     statistics_factor_name = statistics_factor_name,
#'     statistics_factor_name_breaks = c("Mown_Unfertilized", "Mown_NPK"),
#'     statistics_factor_name_col = c("#1A85FF", "#D41159"),
#'     output_path = file.path(output_path, "skr_graph.jpeg"),
#'     slope_distance = slope_distance,
#'     intercept_distance = intercept_distance,
#'     dpi = 100
#'   )
#'   skr_param_graph <- TAD::skr_param_graph(
#'     skr_param = results$ses_skr,
#'     statistics_factor_name = statistics_factor_name,
#'     statistics_factor_name_breaks = c("Mown_Unfertilized", "Mown_NPK"),
#'     statistics_factor_name_col = c("#1A85FF", "#D41159"),
#'     slope_distance = slope_distance,
#'     intercept_distance = intercept_distance,
#'     save_skr_param_graph = file.path(output_path, "skr_param_graph.jpeg"),
#'     dpi = 100
#'   )
#'
#'   unlink(output_path, recursive = TRUE, force = TRUE)
#'
#' @export
launch_analysis_tad <- function(
  weights,
  weights_factor,
  trait_data,
  randomization_number,
  aggregation_factor_name = NULL,
  statistics_factor_name = NULL,
  seed = NULL,
  abundance_file = NULL,
  weighted_moments_file = NULL,
  stat_per_obs_file = NULL,
  stat_per_rand_file = NULL,
  stat_skr_param_file = NULL,
  regenerate_abundance_df = FALSE,
  regenerate_weighted_moments_df = FALSE,
  regenerate_stat_per_obs_df = FALSE,
  regenerate_stat_per_rand_df = FALSE,
  regenerate_stat_skr_df = FALSE,
  significativity_threshold = CONSTANTS$DEFAULT_SIGNIFICATIVITY_THRESHOLD,
  lin_mod = CONSTANTS$DEFAULT_LIN_MOD,
  slope_distance = CONSTANTS$DEFAULT_SLOPE_DISTANCE,
  intercept_distance = CONSTANTS$DEFAULT_INTERCEPT_DISTANCE,
  csv_tsv_load_parameters = list()
) {
  for (name in (c(
    ABUNDANCE_DF_NAME,
    WEIGHTED_MOMENTS_NAME,
    STATISTICS_PER_OBSERVATION_NAME,
    STATISTICS_PER_RANDOM_NAME,
    STAT_SKR_PARAM_NAME
  ))) {
    if (is.null(csv_tsv_load_parameters[[name]])) {
      csv_tsv_load_parameters[[name]] <- list()
    }
  }
  check_parameters(
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
    significativity_threshold = significativity_threshold,
    lin_mod = lin_mod,
    slope_distance = slope_distance,
    intercept_distance = intercept_distance,
    csv_tsv_load_parameters = csv_tsv_load_parameters
  )

  abundance_df <- get_abundance_df(
    weights = weights,
    weights_factor = weights_factor,
    randomization_number = randomization_number,
    abundance_file = abundance_file,
    regenerate_abundance_df = regenerate_abundance_df,
    aggregation_factor_name = aggregation_factor_name,
    seed = seed
  )
  raw_abundance_df <- abundance_df

  results <- filter_na_empty(
    abundance_df = abundance_df,
    weights = weights,
    weights_factor = weights_factor,
    trait_data = trait_data
  )
  abundance_df <- results$abundance_df
  weights <- results$weights
  weights_factor <- results$weights_factor
  trait_data <- results$trait_data

  weighted_moments <- get_weighted_mnts(
    weights_factor = weights_factor,
    trait_data = trait_data,
    weighted_moments_file = weighted_moments_file,
    regenerate_weighted_moments_df = regenerate_weighted_moments_df,
    abundance_df = abundance_df,
    randomization_number = randomization_number,
    slope_distance = slope_distance,
    intercept_distance = intercept_distance,
    factor_names = aggregation_factor_name
  )

  statistics_per_observation <- save_obs_df(
    weights_factor = weights_factor,
    stat_per_obs_file = stat_per_obs_file,
    regenerate_stat_per_obs_df = regenerate_stat_per_obs_df,
    weighted_moments = weighted_moments,
    randomization_number = randomization_number,
    significativity_threshold = significativity_threshold
  )

  stat_per_rand <- get_stat_per_rand(
    weights = weights,
    stat_per_rand_file = stat_per_rand_file,
    regenerate_stat_per_rand_df = regenerate_stat_per_rand_df,
    statistics_factor_name = statistics_factor_name,
    weights_factor = weights_factor,
    randomization_number = randomization_number,
    weighted_moments = weighted_moments,
    abundance_df = abundance_df,
    lin_mod = lin_mod
  )

  ses_skr <- build_skr_ses(
    statistics_factor_name = statistics_factor_name,
    significativity_threshold = significativity_threshold,
    skr_param = stat_per_rand,
    stat_skr_param_file = stat_skr_param_file,
    regenerate_ses_skr = regenerate_stat_skr_df,
    skew_uniform = (
      slope_distance == CONSTANTS$SKEW_UNIFORM_SLOPE_DISTANCE
      && intercept_distance == CONSTANTS$SKEW_UNIFORM_INTERCEPT_DISTANCE
    )
  )

  return(list(
    raw_abundance_df = raw_abundance_df,
    filtered_weights = weights,
    filtered_weights_factor = weights_factor,
    filtered_trait_data = trait_data,
    abundance_df = abundance_df,
    weighted_moments = weighted_moments,
    statistics_per_observation = statistics_per_observation,
    stat_per_rand = stat_per_rand,
    ses_skr = ses_skr
  ))
}

#' @title Compute the weighted mean, variance, skewness and kurtosis
#' @description Compute the weighted mean, variance, skewness and kurtosis
#'   of data with given weights
#' @concept Statistics
#' @param data the data
#' @param weights the vector or matrix of weights corresponding to the
#'   data (each row corresponding to an
#' iteration of data)
#'
#' @return the list of weighted mean, variance, skewness and
#'   kurtosis of the data
#' @examples
#'
#' weighted_mvsk(
#'   data = c(1, 2, 3),
#'   weights = matrix(data = c(1, 1, 1, 2, 1, 3), nrow = 2, ncol = 3)
#' )
#'
#' @export
weighted_mvsk <- function(data, weights) {

  if (is.vector(weights)) {
    if (length(data) != length(weights)) {
      stop(
        "Impossible to compute the weighted mean, variance,",
        " skewness and kurtosis for two vector of different size"
      )
    } else {
      weights <- matrix(data = weights, nrow = 1, ncol = length(data))
    }
  } else if (length(data) != ncol(weights)) {
    stop(
      "Impossible to compute the weighted mean, variance,",
      " skewness and kurtosis for data with incorrect size,",
      " data and weights must have the same column numbers !"
    )
  }
  data <- matrix(data = data, nrow = 1, ncol = length(data))
  weights <- weights / rowSums(weights)
  mean <- (weights %*% t(data))
  diff_data_mean <- (
    data[rep(x = 1, times = nrow(mean)), ]
    - mean[, rep(x = 1, times = ncol(data))]
  )
  mean <- mean[, 1]
  variance <- rowSums(diff_data_mean ^ 2 * weights)
  diff_data_mean_on_sd <- diff_data_mean / sqrt(variance)
  skewness <- rowSums(diff_data_mean_on_sd ^ 3 * weights)
  kurtosis <- rowSums(diff_data_mean_on_sd ^ 4 * weights)
  return(list(
    mean = mean,
    variance = variance,
    skewness = skewness,
    kurtosis = kurtosis
  ))
}

#' @title Compare a value to random values
#' @description Compute different statistics (standardized by
#'   the distribution of random  values).
#' @concept Statistics
#' @param observed_value the observed value
#' @param random_values the random Values
#' @param significance_threshold the array of values used to compute the
#'   quantile (c(0.025, 0.975) by default)
#' @param remove_nas boolean - tells weither to remoe NAs or not
#' @return a list corresponding to :
#' - the observed value
#' - quantile values (minimum significance threshold)
#' - quantile values (maximum significance threshold)
#' - significance (observed value not in quantile values)
#' @examples
#'
#' null_model_distribution_stats(
#'   observed_value = 2,
#'   random_values = c(1, 4, 5, 6, 8),
#'   significance_threshold = c(0.025, 0.975)
#' )
#'
#' @export
null_model_distribution_stats <- function(
  observed_value,
  random_values,
  significance_threshold = c(0.05, 0.95),
  remove_nas = TRUE
) {
  mean_random <- mean(random_values, na.rm = remove_nas)
  sd_random <- stats::sd(random_values, na.rm = remove_nas)
  standardized_observed  <- (observed_value - mean_random) / sd_random
  quant <- stats::quantile(
    x = random_values,
    probs = significance_threshold,
    na.rm = remove_nas
  )
  return(list(
    standardized_observed,
    (quant[[1]] - mean_random) / sd_random,
    (quant[[2]] - mean_random) / sd_random,
    observed_value > quant[[2]] || observed_value < quant[[1]]
  ))
}
