#' Example dataset containing some traits
#'
#' @format ## `trait`
#' A data.frame with two columns and 98 rows:
#' \describe{
#'   \item{Species}{chr sp1, sp2, ...}
#'   \item{SLA}{num 15.4, 32, 32.5, ...}
#' }
#' @keywords internal
"trait"

#' Example dataset containing some traits
#'
#' @format ## `AB`
#' A data.frame with 102 columns and 96 rows:
#' \describe{
#'   \item{Plot}{Factor w/ 8 levels "4","6","11",..: 2 4 6 8 2 4 6 8 2 ...}
#'   \item{Year}{Factor w/ 12 levels "2010","2011",..: 1 1 1 1 2 2 2 2 3 3 ...}
#'   \item{Treatment}{Factor w/ 2 levels "Mown_NPK",..: 1 1 1 1 1 1 1 1 1 ...}
#'   \item{Bloc}{Factor w/ 2 levels "1","2": 1 1 2 2 1 1 2 2 1 }
#'   \item{SP1}{num 0, 0.84, 0, 9.15, 0, ...}
#'   \item{SP3}{num  0, 0.84, 0, 0, 0, ...}
#'   ...
#' }
#' @keywords internal
"AB"

#' Example dataset of abundance_dataframe
#'
#' @format ## `abundance_dataframe
#' A data.frame with 2016 rows and 99 columns:
#' \describe{
#'   \item{number}{int  0 0 0 0 0 0 0 0 0 0 ...}
#'   \item{index1}{num  0 0.84 0 9.15 0 ...}
#'   \item{index2}{num  0 0.84 0 0 0 ...}
#'   \item{index3}{num  0 0 0 0 0 0 0 0 0 0 ...}
#'   ...
#' }
#' Example output of get_abundance_df
#' @keywords internal
"abundance_dataframe"

#' Example dataset of stat_per_obs_dataframe
#'
#' @format ## `stat_per_obs_dataframe`
#' A data.frame - it contains the provided factors, and the following
#' rows - each has 96 obs:
#' \describe{
#'   \item{standardized_observedmean}{num  -0.847 -0.888 ...}
#'   \item{standardized_min_quantilemean}{num  -1.95 -1.58 ...}
#'   \item{standardized_max_quantilemean}{num  1.49 1.18 ...}
#'   \item{significancemean}{logi  FALSE FALSE ...}
#'   \item{standardized_observedvariance}{num  -0.635 -0.746 ...}
#'   \item{standardized_min_quantilevariance}{num  -0.844 -1.183 ...}
#'   \item{standardized_max_quantilevariance}{num  1.07 2.23 ...}
#'   \item{significancevariance}{logi  FALSE FALSE ...}
#'   \item{standardized_observedskewness}{num  0.904 1.458 ...}
#'   \item{standardized_min_quantileskewness}{num  -1.21 -1.56 ...}
#'   \item{standardized_max_quantileskewness}{num  1.23 1.43 ...}
#'   \item{significanceskewness}{logi  FALSE TRUE ...}
#'   \item{standardized_observedkurtosis}{num  1.61 3.17 ...}
#'   \item{standardized_min_quantilekurtosis}{num  -0.654 -1.436 ...}
#'   \item{standardized_max_quantilekurtosis}{num  0.996 1.717 ...}
#'   \item{significancekurtosis}{logi  TRUE TRUE ...}
#' }
#' Example output of get_stat_per_rand
#' @keywords internal
"stat_per_obs_dataframe"

#' Example dataset of stat_per_rand_dataframe
#'
#' @format ## `stat_per_rand_dataframe`
#' A data.frame - of 8 columns and 42 rows.
#' \describe{
#' \item{number}{int  0 0 1 1 2 2 3 3 4 4 ...}
#' \item{Treatment}{Factor w/ 2 levels}
#' \item{slope}{num  0.826 0.832 1.406 ...}
#' \item{intercept}{num  4.54 2.72 2.46 ...}
#' \item{rsquare}{num  0.201 0.216 ...}
#' \item{tad_stab}{num  1.856 0.49 ...}
#' \item{distance_to_family}{num  3.063 ...}
#' \item{cv_distance_to_family}{num  144 ...}
#' }
#' Example output of stat_per_rand_dataframe
#' @keywords internal
"stat_per_rand_dataframe"

#' Example dataset of weighted_moments_dataframe
#'
#' @format ## `weighted_moments_dataframe`
#' A data.frame - todo = describe the df content:
#' Example output of weighted_moments_dataframe
#' @keywords internal
"weighted_moments_dataframe"

#' Example dataset of skr_ses
#'
#' @format ## `skr_ses`
#' A data.frame - todo = describe the df content:
#' Example output of skr_ses
#' @keywords internal
"skr_ses_dataframe"

#' Example dataset of filtred results just after abundances generation
#' @keywords internal
"filtered_abundances"
