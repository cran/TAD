
#' @title check_parameter_type
#' @keywords internal
check_parameter_type <- function(value, expected, or_null = FALSE) {
  name <- deparse(substitute(value))
  if (or_null) {
    fmt <- "Bad type for %s. Got %s, expected %s or null"
  } else {
    fmt <- "Bad type for %s. Got %s, expected %s"
  }
  if (is.null(value) || missing(value)) {
    if (or_null) {
      is_good_type <- TRUE
      type_str <- ""
    } else {
      is_good_type <- FALSE
      type_str <- expected
    }
  } else if (length(expected) > 1) {
    is_good_type <- any(sapply(expected, function(x) is(value, x)))
    type_str <- paste("any of", paste(expected, collapse = ", "))
  } else {
    is_good_type <- is(value, expected)
    type_str <- expected
  }
  if (! is_good_type) {
    stop(sprintf(
      fmt,
      name,
      class(value),
      type_str
    ))
  }
}

#' @title check_parameter_value
#' @keywords internal
check_parameter_value <- function(
  value,
  checker,
  expected_description,
  fmt = NULL
) {
  name <- deparse(substitute(value))
  if (is.null(value) || missing(value)) {
    str_value <- "NULL"
    str_length <- 0
    str_type <- "NULL"
    checker <- function(...) TRUE
  } else {
    if (length(value > 1)) {
      str_value <- paste(value, collapse = ", ")
    } else {
      str_value <- as.character(value)
    }
    str_length <- length(value)
    str_type <- class(value)
  }
  if (is.null(fmt)) {
    fmt <- "Bad value for %s: Expected to be %s, but got <%s(%s)> %s"
  }
  if (!checker(value)) {
    stop(sprintf(
      fmt,
      name,
      expected_description,
      str_type,
      str_length,
      str_value
    ))
  }
}

#' @title load_package
#' @keywords internal
load_package <- function(package_name) {
  if (!suppressMessages(
    require(
      package_name,
      quietly = TRUE,
      character.only = TRUE
    )
  )) {
    stop(sprintf(
      paste(
        "Please install %s to run graphs generation function.",
        "To make all the package's functionnalities available,you can run:",
        "devtools::install_dev_deps('tad')",
        "in R console, or directly from a terminal:",
        "R -q -e \"devtools::install_dev_deps('tad')\"",
        sep = "\n"
      ),
      package_name
    ))
  }
  return(as.environment(sprintf("package:%s", package_name)))
}

#' @keywords internal
rename_df_columns <- function(dataframe, ...) {
  transforms <- list(...)
  column_names <- colnames(dataframe)
  for (original in names(transforms)) {
    column_names[column_names == original] <- transforms[[original]]
  }
  colnames(dataframe) <- column_names
  return(dataframe)
}
