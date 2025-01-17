% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tad.R
\name{check_parameters}
\alias{check_parameters}
\title{parameters checkings}
\usage{
check_parameters(
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
)
}
\arguments{
\item{weights}{the dataframe of weights, one row correspond to a
series of observation}

\item{weights_factor}{the dataframe which contains the different
factor linked to the weights}

\item{trait_data}{a vector of the data linked to the different factor}

\item{randomization_number}{the number of random abundance matrix to
generate}

\item{aggregation_factor_name}{vector of factor name for the
generation of random matrix}

\item{statistics_factor_name}{vector of factor name for the
computation of statistics for each generated matrix}

\item{seed}{the seed of the pseudo random number generator}

\item{abundance_file}{the path and name of the RDS file to
load/save the dataframe which
contains the observed data and the generated matrix}

\item{weighted_moments_file}{the path and name of the RDS file to
load/save the dataframe which
contains the calculated moments}

\item{stat_per_obs_file}{the path and name of the RDS file to
load/save the dataframe which
contains the statistics for each observed row regarding the random ones}

\item{stat_per_rand_file}{the path and name of the RDS file to
load/save the dataframe which
contains the statistics for each random matrix generated}

\item{stat_skr_param_file}{default=NULL You can provide the output to write
the SKR statistics results to.}

\item{regenerate_abundance_df}{boolean to specify if the
abundance dataframe is computed again}

\item{regenerate_weighted_moments_df}{boolean to specify if
the weighted moments dataframe is computed again}

\item{regenerate_stat_per_obs_df}{boolean to specify if
the statistics per observation dataframe is computed again}

\item{regenerate_stat_per_rand_df}{boolean to specify if
the statistics per random matrix dataframe is computed again}

\item{significativity_threshold}{the significance threshold to
consider that the observed value is in the randomized value}

\item{lin_mod}{Indicates the type of linear model to use for
(SKR): choose "lm" or "mblm"}

\item{slope_distance}{slope of the theoretical distribution
law (default: slope = 1 intercept = 1.86 skew-uniform distribution family)}

\item{intercept_distance}{intercept of the theoretical distribution
law (default: slope = 1 intercept = 1.86 skew-uniform distribution family)}

\item{csv_tsv_load_parameters}{a list of parameters for each data structure
we want to load. Each element must be named after the data structure
we want to load.}
}
\description{
Checks all parameters of the TAD and raises errors if
parameters' values are incoherent.
}
\keyword{internal}
