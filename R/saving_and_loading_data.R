

#nolint start: object_name_linter

ABUNDANCE_DF_NAME <- "abundance_df"
WEIGHTED_MOMENTS_NAME <- "weighted_moments"
STATISTICS_PER_OBSERVATION_NAME <- "statistics_per_observation"
STATISTICS_PER_RANDOM_NAME <- "statistics_per_random"
STAT_SKR_PARAM_NAME <- "stat_skr_param"

#nolint end


#' @title save_abundance_dataframe
#' @description
#' This function provides a secured way to save abundance_dataframe
#' dataframe.
#' The more generic functionprovided by TAD `save_depending_on_format` expects
#' saves object using their name, but saves nothing if the provided name is not
#' correct, or mya even save an unwanted object.
#' This function provides a way to verify the object you want to save, and so,
#' it is more secured.
#' @param path the path of the file to load
#' @param object the object to save
#' @return NULL - called for side effects
#' @export
save_abundance_dataframe <- function(path, object = NULL) {
  if (is.null(object)) {
    object <- parent.frame()[[ABUNDANCE_DF_NAME]]
  }
  if (is.null(object) || ! is.data.frame(object)) {
    stop(sprintf(
      paste0(
        "Could not find the abundance dataframe to save. ",
        "Please, create a `%s` variable, or provide ",
        "the dataframe with the `object` parameter."
      ),
      ABUNDANCE_DF_NAME
    ))
  }
  assign(ABUNDANCE_DF_NAME, object)
  return(save_depending_on_format(file = path, ABUNDANCE_DF_NAME))
}

#' @title save_weighted_moments
#' @description
#' This function provides a secured way to save weighted_moments
#' dataframe.
#' The more generic functionprovided by TAD `save_depending_on_format` expects
#' saves object using their name, but saves nothing if the provided name is not
#' correct, or mya even save an unwanted object.
#' This function provides a way to verify the object you want to save, and so,
#' it is more secured.
#' @param path the path of the file to load
#' @param object the object to save
#' @return NULL - called for side effects
#' @export
save_weighted_moments <- function(path, object = NULL) {
  if (is.null(object)) {
    object <- parent.frame()[[WEIGHTED_MOMENTS_NAME]]
  }
  if (is.null(object) || ! is.data.frame(object)) {
    stop(sprintf(
      paste0(
        "Could not find the weighted moments dataframe to save. ",
        "Please, create a `%s` variable, or provide ",
        "the dataframe with the `object` parameter."
      ),
      WEIGHTED_MOMENTS_NAME
    ))
  }
  assign(WEIGHTED_MOMENTS_NAME, object)
  return(save_depending_on_format(file = path, WEIGHTED_MOMENTS_NAME))
}

#' @title save_statistics_per_obs
#' @description
#' This function provides a secured way to save statistics_per_obs
#' dataframe.
#' The more generic functionprovided by TAD `save_depending_on_format` expects
#' saves object using their name, but saves nothing if the provided name is not
#' correct, or mya even save an unwanted object.
#' This function provides a way to verify the object you want to save, and so,
#' it is more secured.
#' @param path the path of the file to load
#' @param object the object to save
#' @return NULL - called for side effects
#' @export
save_statistics_per_obs <- function(path, object = NULL) {
  if (is.null(object)) {
    object <- parent.frame()[[STATISTICS_PER_OBSERVATION_NAME]]
  }
  if (is.null(object) || ! is.data.frame(object)) {
    stop(sprintf(
      paste0(
        "Could not find the statistics per observation dataframe to save. ",
        "Please, create a `%s` variable, or provide ",
        "the dataframe with the `object` parameter."
      ),
      STATISTICS_PER_OBSERVATION_NAME
    ))
  }
  assign(STATISTICS_PER_OBSERVATION_NAME, object)
  return(save_depending_on_format(file = path, STATISTICS_PER_OBSERVATION_NAME))
}

#' @title save_statistics_per_random
#' @description
#' This function provides a secured way to save statistics_per_random
#' dataframe.
#' The more generic functionprovided by TAD `save_depending_on_format` expects
#' saves object using their name, but saves nothing if the provided name is not
#' correct, or mya even save an unwanted object.
#' This function provides a way to verify the object you want to save, and so,
#' it is more secured.
#' @param path the path of the file to load
#' @param object the object to save
#' @return NULL - called for side effects
#' @export
save_statistics_per_random <- function(path, object = NULL) {
  if (is.null(object)) {
    object <- parent.frame()[[STATISTICS_PER_RANDOM_NAME]]
  }
  if (is.null(object) || ! is.data.frame(object)) {
    stop(sprintf(
      paste0(
        "Could not find the statistics per random dataframe to save. ",
        "Please, create a `%s` variable, or provide ",
        "the dataframe with the `object` parameter."
      ),
      STATISTICS_PER_RANDOM_NAME
    ))
  }
  assign(STATISTICS_PER_RANDOM_NAME, object)
  return(save_depending_on_format(file = path, STATISTICS_PER_RANDOM_NAME))
}

#' @title save_stat_skr_param
#' @description
#' This function provides a secured way to save stat_skr_param
#' dataframe.
#' The more generic functionprovided by TAD `save_depending_on_format` expects
#' saves object using their name, but saves nothing if the provided name is not
#' correct, or mya even save an unwanted object.
#' This function provides a way to verify the object you want to save, and so,
#' it is more secured.
#' @param path the path of the file to load
#' @param object the object to save
#' @return NULL - called for side effects
#' @export
save_stat_skr_param <- function(path, object = NULL) {
  if (is.null(object)) {
    object <- parent.frame()[[STAT_SKR_PARAM_NAME]]
  }
  if (is.null(object) || ! is.data.frame(object)) {
    stop(sprintf(
      paste0(
        "Could not find the statistics skr parameters dataframe to save. ",
        "Please, create a `%s` variable, or provide ",
        "the dataframe with the `object` parameter."
      ),
      STAT_SKR_PARAM_NAME
    ))
  }
  assign(STAT_SKR_PARAM_NAME, object)
  return(save_depending_on_format(file = path, STAT_SKR_PARAM_NAME))
}

#' @title save_depending_on_format
#' @description
#' Saves data in TSV, CSV, RDA or RDS, depending on the file's extension.
#' CSV and TSVs are saved the same way:
#' with double quotes (ascii character 34, or 0x22 " ),
#' EOL is a newline (ascii character 10, or 0x0A),
#' NAs are saved as "NA",
#' decimal point are latin point (ascii character 46, or 0x2E . ),
#' row names and colnames are added and the encoding is UTF-8.
#' The only thing that changes is the separator:
#' A comma (ascii character 44, or 0x2C , ) for CSV files and
#' a tabulation (ascii character 9, or 0x09) for TSV files.
#' @param file the path of the file to save
#' @param var_name the name of the object to save
#' @param compress compression algo for rds/rda, defaults to bzip2
#' @return NULL - called for side effects
#' @keywords internal
save_depending_on_format <- function(file, var_name, compress = "bzip2") {
  accepted_extensions <- c("rda", "rds", "csv", "tsv")
  if (grepl("rds$", file, ignore.case = TRUE)) {
    saveRDS(
      object = parent.frame()[[var_name]],
      file = file,
      compress = compress
    )
  } else if (grepl("rda$", file, ignore.case = TRUE)) {
    save(
      list = c(var_name),
      file = file,
      envir = parent.frame(),
      compress = compress
    )
  } else if (grepl("csv$", file, ignore.case = TRUE)) {
    write.table(
      parent.frame()[[var_name]],
      file = file,
      append = FALSE,
      quote = TRUE,
      sep = ",",
      eol = "\n",
      na = "NA",
      dec = ".",
      row.names = TRUE,
      col.names = TRUE,
      qmethod = c("double"),
      fileEncoding = "UTF-8"
    )
  } else if (grepl("tsv$", file, ignore.case = TRUE)) {
    write.table(
      parent.frame()[[var_name]],
      file = file,
      append = FALSE,
      quote = TRUE,
      sep = "\t",
      eol = "\n",
      na = "NA",
      dec = ".",
      row.names = TRUE,
      col.names = TRUE,
      qmethod = c("double"),
      fileEncoding = "UTF-8"
    )
  } else {
    stop(sprintf(
      paste0(
        "Could not determine format from path: %s. Please",
        " provide an extension within: %s."
      ),
      file,
      paste(accepted_extensions, collapse = ", ")
    ))
  }
  return(NULL)
}

#' @title load_depending_on_format
#' @param path the path of the file to load
#' @param env_attr the name of the object to load
#' @param ... only used in case of CSV loading - passed to load_tad_table
#'   see load_tad_table$`...` for more informations
#' @keywords internal
load_depending_on_format <- function(
  path,
  env_attr = NULL,
  ...
) {
  accepted_extensions <- c("rda", "rds", "csv", "tsv")
  if (grepl("rds$", path, ignore.case = TRUE)) {
    result <- readRDS(file = path)
  } else if (grepl("rda$", path, ignore.case = TRUE)) {
    load(path, envir = (env <- new.env()))
    if (is.null(env_attr)) {
      result <- env
    } else {
      result <- env[[env_attr]]
    }
  } else if (grepl("csv$", path, ignore.case = TRUE)) {
    return(load_tad_table(path = path, table_name = env_attr, sep = ",", ...))
  } else if (grepl("tsv$", path, ignore.case = TRUE)) {
    return(load_tad_table(path = path, table_name = env_attr, sep = "\t", ...))
  } else {
    stop(sprintf(
      paste0(
        "Could not determine format from path: %s. Please",
        " provide an extension within: %s."
      ),
      file,
      paste(accepted_extensions, collapse = ", ")
    ))
  }
  return(result)
}

#' @title load_tad_table
#' @param path the path to the file to load
#' @param sep the separator used for this csv/tsv file
#' @param table_name the name of the object to load
#' @param factor_names provide the name of factor columns. A `as.factor`
#'   function will be applied to the provided columns.
#' @param character_names provide the name of character columns. A
#'   `as.character` function will be applied to the provided columns.
#' @param ... a set of functions to apply to the result's attribute.
#'   if `a` = as.numeric in `...`, then `result$a <- as.numeric(result$a)`
#'   any other function than as.numeric can be used
#' @keywords internal
load_tad_table <- function(
  path,
  sep,
  table_name = NULL,
  factor_names = NULL,
  character_names = NULL,
  ...
) {

  result <- read.table(
    file = path,
    header = TRUE,
    check.names = FALSE,
    comment.char = "",
    fileEncoding = "UTF-8",
    sep = sep,
    quote = "\"",
    dec = ".",
    stringsAsFactors = TRUE
  )

  for (c in colnames(result)) {
    if (all(is.integer(result[, c]))) {
      result[, c] <- as.double(result[, c])
    }
  }

  if (any(
    table_name == c(
      ABUNDANCE_DF_NAME,
      STATISTICS_PER_OBSERVATION_NAME,
      STATISTICS_PER_RANDOM_NAME,
      STAT_SKR_PARAM_NAME
    )
  )) {
    row.names(result) <- as.integer(seq_len(nrow(result)))
  }

  operations <- get_loading_operations_for(
    table_name,
    factor_names,
    character_names
  )

  operations <- do.call(list, c(list(...), operations))
  item_names <- names(operations)
  colnames <- colnames(result)
  for (index in seq_along(item_names)) {
    attr_name <- item_names[[index]]
    if (! (attr_name %in% colnames)) {
      warning(sprintf(
        "Unknown attribute %s in table %s loaded from %s.",
        attr_name,
        (if (!is.null(table_name)) table_name else "UnknowTableType"),
        normalizePath(path, mustWork = FALSE)
      ))
      next
    }
    result[, attr_name] <- operations[[index]](result[, attr_name])
  }
  return(result)
}


get_loading_operations_for <- function(
  table_name,
  factor_names,
  character_names
) {
  operations <- list()

  if (table_name == ABUNDANCE_DF_NAME) {
    operations[["number"]] <- as.integer
  } else if (table_name == WEIGHTED_MOMENTS_NAME) {
    operations[["number"]] <- as.integer
  } else if (table_name == STATISTICS_PER_OBSERVATION_NAME) {
  } else if (table_name == STATISTICS_PER_RANDOM_NAME) {
    operations[["distance_to_family"]] <- as.double
    operations[["cv_distance_to_family"]] <- as.double
  } else if (table_name == STAT_SKR_PARAM_NAME) {
    operations[["distance_to_family_ses"]] <- as.double
    operations[["cv_distance_to_family_ses"]] <- as.double
  } else {
    warning(
      "Load TAD table called with no table_name or unknow provided. ",
      "Some post-loading ",
      "processing may not be applied and the resulting dataframe may ",
      "slightly differ from the object previously saved."
    )
  }

  if (! is.null(factor_names)) {
    for (factor_name in factor_names) {
      operations[[factor_name]] <- as.factor
    }
  }
  if (! is.null(character_names)) {
    for (character_name in character_names) {
      operations[[character_name]] <- as.character
    }
  }

  return(operations)
}


#' @title load_abundance_dataframe
#' @param path the path to the file to load
#' @param ... a set of parameters provided to load_depending_on_format
#'   may contain some operations to apply to format/cast CSV or TSV data
#'   which are almost typeless by default
#' @return an abundance dataframe, with the column `number` caster
#' into integers and rownames casted into integers.
#' @export
load_abundance_dataframe <- function(path, ...) {
  return(load_depending_on_format(
    path = path,
    env_attr = ABUNDANCE_DF_NAME,
    ...
  ))
}

#' @title load_weighted_moments
#' @param path the path to the file to load
#' @param ... a set of parameters provided to load_depending_on_format
#'   may contain some operations to apply to format/cast CSV or TSV data
#'   which are almost typeless by default
#' @return a weighted moments dataframe with the column `number` caster
#' into integers and rownames casted into integers.
#' @export
load_weighted_moments <- function(path, ...) {
  return(load_depending_on_format(
    path = path,
    env_attr = WEIGHTED_MOMENTS_NAME,
    ...
  ))
}

#' @title load_statistics_per_obs
#' @param path the path to the file to load
#' @param ... a set of parameters provided to load_depending_on_format
#'   may contain some operations to apply to format/cast CSV or TSV data
#'   which are almost typeless by default
#' @return a stats par observations dataframe with rownames casted
#' into integers.
#' @export
load_statistics_per_obs <- function(path, ...) {
  return(load_depending_on_format(
    path = path,
    env_attr = STATISTICS_PER_OBSERVATION_NAME,
    ...
  ))
}

#' @title load_statistics_per_random
#' @param path the path to the file to load
#' @param ... a set of parameters provided to load_depending_on_format
#'   may contain some operations to apply to format/cast CSV or TSV data
#'   which are almost typeless by default
#' @return a stats per randon dataframe with distance_to_family and
#' cv_distance_to_family casted into doubles and with rownames casted
#' into integers.
#' @export
load_statistics_per_random <- function(path, ...) {
  return(load_depending_on_format(
    path = path,
    env_attr = STATISTICS_PER_RANDOM_NAME,
    ...
  ))
}

#' @title load_stat_skr_param
#' @param path the path to the file to load
#' @param ... a set of parameters provided to load_depending_on_format
#'   may contain some operations to apply to format/cast CSV or TSV data
#'   which are almost typeless by default
#' @return a stats SKR parameters dataframe with distance_to_family_ses and
#' cv_distance_to_family_ses casted into doubles and with rownames casted
#' into integers.
#' @export
load_stat_skr_param <- function(path, ...) {
  object <- load_depending_on_format(
    path = path,
    env_attr = STAT_SKR_PARAM_NAME,
    ...
  )
  return(skr_standard_uniform_names(object))
}

#' @title skr_custom_uniform_names
#' @keywords internal
skr_custom_uniform_names <- function(skr) {
  return(rename_df_columns(
    skr,
    tad_eve_ses = "TADeve_ses",
    tad_eve_signi = "TADeve_signi",
    cv_tad_eve_ses = "cv_TADeve_ses",
    cv_tad_eve_signi = "cv_TADeve_signi"
  ))
}

#' @title skr_standard_uniform_names
#' @keywords internal
skr_standard_uniform_names <- function(skr) {
  return(rename_df_columns(
    skr,
    TADeve_ses = "tad_eve_ses",
    TADeve_signi = "tad_eve_signi",
    cv_TADeve_ses = "cv_tad_eve_ses",
    cv_TADeve_signi = "cv_tad_eve_signi"
  ))
}
