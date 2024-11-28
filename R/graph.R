
#' @title moments_graph
#' @description
#' Graph of the distributions' moments (mean, variance, skewness and
#' kurtosis) compared to null model
#' @param moments_df Moments data frame (mean, variance, skewness, kurtosis)
#' @param statistics_per_observation SES of the Moments data frame and
#'   significance compared to null model
#' @param statistics_factor_name column of data use for colors discrimination
#' @param statistics_factor_name_breaks vector of factor levels of the
#'   statistics_factor_name, same dimension than statistics_factor_name_col
#' @param statistics_factor_name_col vector of colors, same dimension than
#'   statistics_factor_name_breaks
#' @param output_path The path to save the graph
#' @param dpi The dpi number to use when we generate png/jpg graph
#' @return A graph instance
#' @examples
#'
#' \donttest{
#'
#'   results <- TAD::launch_analysis_tad(
#'     weights = TAD::AB[, 5:102],
#'     weights_factor = TAD::AB[, c("Year", "Plot", "Treatment", "Bloc")],
#'     trait_data = log(TAD::trait[["SLA"]]),
#'     aggregation_factor_name = c("Year", "Bloc"),
#'     statistics_factor_name = (statistics_factor_name <- c("Treatment")),
#'     randomization_number = 100
#'   )
#'
#'   # if you want to display the graph
#'   graph <- TAD::moments_graph(
#'     moments_df = results$weighted_moments,
#'     statistics_per_observation = results$statistics_per_observation,
#'     statistics_factor_name = statistics_factor_name,
#'     statistics_factor_name_breaks = c("Mown_Unfertilized", "Mown_NPK"),
#'     statistics_factor_name_col = c("#1A85FF", "#D41159")
#'   )
#'
#'   plot(graph)
#'
#'   # if you want to save the graph as a file
#'   # either jpg, jpeg, png or svg are
#'   output_path <- file.path(tempdir(), "outputs")
#'   dir.create(output_path)
#'   TAD::moments_graph(
#'     moments_df = results$weighted_moments,
#'     statistics_per_observation = results$statistics_per_observation,
#'     statistics_factor_name = statistics_factor_name,
#'     statistics_factor_name_breaks = c("Mown_Unfertilized", "Mown_NPK"),
#'     statistics_factor_name_col = c("#1A85FF", "#D41159"),
#'     output_path = file.path(output_path, "moment_graph.png")
#'   )
#'
#'   unlink(output_path, recursive = TRUE, force = TRUE)
#'
#' }
#'
#' @export
moments_graph <- function(
  moments_df,
  statistics_per_observation,
  statistics_factor_name,
  statistics_factor_name_breaks = NULL,
  statistics_factor_name_col = NULL,
  output_path = NULL,
  dpi = 600
) {

  if (! is.null(output_path)) {
    if (grepl(".svg$", output_path, ignore.case = TRUE)) {
      load_package("svglite")
    }
  }

  ggplot2 <- load_package("ggplot2")
  ggpubr <- load_package("ggpubr")
  dplyr <- load_package("dplyr")
  rlang <- load_package("rlang")
  grid <- load_package("grid")

  if (is.null(statistics_factor_name_col)) {
    gr_devices <- load_package("grDevices")
    statistics_factor_name_col <- gr_devices$palette()
  }

  .data <- NULL

  boxplot_1 <- (
    ggplot2$ggplot()
    + ggplot2$geom_boxplot(
      data = dplyr$filter(moments_df, .data$number > 0),
      ggplot2$aes(x = "Mean", y = mean),
      col = "black",
      fill = "lightgrey",
      alpha = 0.4
    )
    + ggplot2$geom_point(
      data = dplyr$filter(moments_df, .data$number == 0),
      ggplot2$aes(
        x = "Mean",
        y = mean,
        col = !!rlang$sym(statistics_factor_name),
        fill = !!rlang$sym(statistics_factor_name)
      ),
      shape = 21,
      size = 4,
      alpha = 0.4,
      position = "jitter"
    )
    + ggplot2$scale_fill_manual(
      values = statistics_factor_name_col,
      limits = statistics_factor_name_breaks
    )
    + ggplot2$scale_color_manual(
      values = statistics_factor_name_col,
      limits = statistics_factor_name_breaks
    )
    + ggplot2$theme_bw()
    + ggplot2$labs(y = "Moments")
    + ggplot2$theme(
      plot.title = ggplot2$element_blank(),
      axis.text.y = ggplot2$element_text(size = 20),
      axis.title.y = ggplot2$element_text(size = 30),
      axis.text.x = ggplot2$element_blank(),
      axis.title.x = ggplot2$element_blank(),
      legend.title = ggplot2$element_text(size = 30, face = "bold"),
      legend.text = ggplot2$element_text(size = 30),
      legend.key.size = grid$unit(1.5, "cm")
    )
  )
  boxplot_2 <- (
    ggplot2$ggplot()
    + ggplot2$geom_boxplot(
      data = dplyr$filter(moments_df, .data$number > 0),
      ggplot2$aes(x = "Variance", y = .data$variance),
      col = "black",
      fill = "lightgrey",
      alpha = 0.4
    )
    + ggplot2$geom_point(
      data = dplyr$filter(moments_df, .data$number == 0),
      ggplot2$aes(
        x = "Variance",
        y = .data$variance,
        col = !!rlang$sym(statistics_factor_name),
        fill = !!rlang$sym(statistics_factor_name)
      ),
      shape = 21,
      size = 4,
      alpha = 0.4,
      position = "jitter"
    )
    + ggplot2$scale_fill_manual(
      values = statistics_factor_name_col,
      limits = statistics_factor_name_breaks
    )
    + ggplot2$scale_color_manual(
      values = statistics_factor_name_col,
      limits = statistics_factor_name_breaks
    )
    + ggplot2$theme_bw()
    + ggplot2$labs(y = "Moments")
    + ggplot2$theme(
      plot.title = ggplot2$element_blank(),
      axis.text.y = ggplot2$element_text(size = 20),
      axis.title.y = ggplot2$element_blank(),
      axis.text.x = ggplot2$element_blank(),
      axis.title.x = ggplot2$element_blank(),
      legend.title = ggplot2$element_text(size = 30, face = "bold"),
      legend.text = ggplot2$element_text(size = 30),
      legend.key.size = grid$unit(1.5, "cm")
    )
  )
  boxplot_3 <- (
    ggplot2$ggplot()
    + ggplot2$geom_boxplot(
      data = dplyr$filter(moments_df, .data$number > 0),
      ggplot2$aes(x = "Skewness", y = .data$skewness),
      col = "black",
      fill = "lightgrey",
      alpha = 0.4
    )
    + ggplot2$geom_point(
      data = dplyr$filter(moments_df, .data$number == 0),
      ggplot2$aes(
        x = "Skewness",
        y = .data$skewness,
        col = !!rlang$sym(statistics_factor_name),
        fill = !!rlang$sym(statistics_factor_name)
      ),
      shape = 21,
      size = 4,
      alpha = 0.4,
      position = "jitter"
    )
    + ggplot2$scale_fill_manual(
      values = statistics_factor_name_col,
      limits = statistics_factor_name_breaks
    )
    + ggplot2$scale_color_manual(
      values = statistics_factor_name_col,
      limits = statistics_factor_name_breaks
    )
    + ggplot2$theme_bw()
    + ggplot2$labs(y = "Moments")
    + ggplot2$theme(
      plot.title = ggplot2$element_blank(),
      axis.text.y = ggplot2$element_text(size = 20),
      axis.title.y = ggplot2$element_blank(),
      axis.text.x = ggplot2$element_blank(),
      axis.title.x = ggplot2$element_blank(),
      legend.title = ggplot2$element_text(size = 30, face = "bold"),
      legend.text = ggplot2$element_text(size = 30),
      legend.key.size = grid$unit(1.5, "cm")
    )
  )
  boxplot_4 <- (
    ggplot2$ggplot()
    + ggplot2$geom_boxplot(
      data = dplyr$filter(moments_df, .data$number > 0),
      ggplot2$aes(x = "Kurtosis", y = .data$kurtosis),
      col = "black",
      fill = "lightgrey",
      alpha = 0.4
    )
    + ggplot2$geom_point(
      data = dplyr$filter(moments_df, .data$number == 0),
      ggplot2$aes(
        x = "Kurtosis",
        y = .data$kurtosis,
        col = !!rlang$sym(statistics_factor_name),
        fill = !!rlang$sym(statistics_factor_name)
      ),
      shape = 21,
      size = 4,
      alpha = 0.4,
      position = "jitter"
    )
    + ggplot2$scale_fill_manual(
      values = statistics_factor_name_col,
      limits = statistics_factor_name_breaks
    )
    + ggplot2$scale_color_manual(
      values = statistics_factor_name_col,
      limits = statistics_factor_name_breaks
    )
    + ggplot2$theme_bw()
    + ggplot2$labs(y = "Moments")
    + ggplot2$theme(
      plot.title = ggplot2$element_blank(),
      axis.text.y = ggplot2$element_text(size = 20),
      axis.title.y = ggplot2$element_blank(),
      axis.text.x = ggplot2$element_blank(),
      axis.title.x = ggplot2$element_blank(),
      legend.title = ggplot2$element_text(size = 30, face = "bold"),
      legend.text = ggplot2$element_text(size = 30),
      legend.key.size = grid$unit(1.5, "cm")
    )
  )

  abline_1 <- (
    ggplot2$ggplot()
    + ggplot2$geom_abline(
      intercept = 0,
      slope = 0,
      color = "grey",
      linewidth = 1,
      linetype = "dashed"
    )
    + ggplot2$geom_point(
      data = dplyr$filter(
        statistics_per_observation,
        .data$significancemean == "TRUE"
      ),
      ggplot2$aes(
        x = "Mean",
        y = .data$standardized_observedmean,
        col = !!rlang$sym(statistics_factor_name),
        fill = !!rlang$sym(statistics_factor_name)
      ),
      shape = 21,
      size = 4,
      alpha = 0.8,
      position = "jitter"
    )
    + ggplot2$geom_point(
      data = dplyr$filter(
        statistics_per_observation,
        .data$significancemean == "FALSE"
      ),
      ggplot2$aes(
        x = "Mean",
        y = .data$standardized_observedmean,
        col = !!rlang$sym(statistics_factor_name),
        fill = !!rlang$sym(statistics_factor_name)
      ),
      shape = 21,
      size = 4,
      alpha = 0.2,
      position = "jitter"
    )
    + ggplot2$scale_fill_manual(
      values = statistics_factor_name_col,
      limits = statistics_factor_name_breaks
    )
    + ggplot2$scale_color_manual(
      values = statistics_factor_name_col,
      limits = statistics_factor_name_breaks
    )
    + ggplot2$theme_bw()
    + ggplot2$labs(y = paste0("SES ", "Moments"))
    + ggplot2$theme(
      plot.title = ggplot2$element_blank(),
      axis.text.y = ggplot2$element_text(size = 20),
      axis.title.y = ggplot2$element_text(size = 30),
      axis.text.x = ggplot2$element_text(size = 20),
      axis.title.x = ggplot2$element_blank(),
      legend.title = ggplot2$element_text(size = 30, face = "bold"),
      legend.text = ggplot2$element_text(size = 30),
      legend.key.size = grid$unit(1.5, "cm")
    )
  )
  abline_2 <- (
    ggplot2$ggplot()
    + ggplot2$geom_abline(
      intercept = 0,
      slope = 0,
      color = "grey",
      linewidth = 1,
      linetype = "dashed"
    )
    + ggplot2$geom_point(
      data = dplyr$filter(
        statistics_per_observation,
        .data$significancevariance == "TRUE"
      ),
      ggplot2$aes(
        x = "Variance",
        y = .data$standardized_observedvariance,
        col = !!rlang$sym(statistics_factor_name),
        fill = !!rlang$sym(statistics_factor_name)
      ),
      shape = 21,
      size = 4,
      alpha = 0.8,
      position = "jitter"
    )
    + ggplot2$geom_point(
      data = dplyr$filter(
        statistics_per_observation,
        .data$significancevariance == "FALSE"
      ),
      ggplot2$aes(
        x = "Variance",
        y = .data$standardized_observedvariance,
        col = !!rlang$sym(statistics_factor_name),
        fill = !!rlang$sym(statistics_factor_name)
      ),
      shape = 21,
      size = 4,
      alpha = 0.2,
      position = "jitter"
    )
    + ggplot2$scale_fill_manual(
      values = statistics_factor_name_col,
      limits = statistics_factor_name_breaks
    )
    + ggplot2$scale_color_manual(
      values = statistics_factor_name_col,
      limits = statistics_factor_name_breaks
    )
    + ggplot2$theme_bw()
    + ggplot2$labs()
    + ggplot2$theme(
      plot.title = ggplot2$element_blank(),
      axis.text.y = ggplot2$element_text(size = 20),
      axis.title.y = ggplot2$element_blank(),
      axis.text.x = ggplot2$element_text(size = 20),
      axis.title.x = ggplot2$element_blank(),
      legend.title = ggplot2$element_text(size = 30, face = "bold"),
      legend.text = ggplot2$element_text(size = 30),
      legend.key.size = grid$unit(1.5, "cm")
    )
  )
  abline_3 <- (
    ggplot2$ggplot()
    + ggplot2$geom_abline(
      intercept = 0,
      slope = 0,
      color = "grey",
      linewidth = 1,
      linetype = "dashed"
    )
    + ggplot2$geom_point(
      data = dplyr$filter(
        statistics_per_observation,
        .data$significanceskewness == "TRUE"
      ),
      ggplot2$aes(
        x = "Skewness",
        y = .data$standardized_observedskewness,
        col = !!rlang$sym(statistics_factor_name),
        fill = !!rlang$sym(statistics_factor_name)
      ),
      shape = 21,
      size = 4,
      alpha = 0.8,
      position = "jitter"
    )
    + ggplot2$geom_point(
      data = dplyr$filter(
        statistics_per_observation,
        .data$significanceskewness == "FALSE"
      ),
      ggplot2$aes(
        x = "Skewness",
        y = .data$standardized_observedskewness,
        col = !!rlang$sym(statistics_factor_name),
        fill = !!rlang$sym(statistics_factor_name)
      ),
      shape = 21,
      size = 4,
      alpha = 0.2,
      position = "jitter"
    )
    + ggplot2$scale_fill_manual(
      values = statistics_factor_name_col,
      limits = statistics_factor_name_breaks
    )
    + ggplot2$scale_color_manual(
      values = statistics_factor_name_col,
      limits = statistics_factor_name_breaks
    )
    + ggplot2$theme_bw()
    + ggplot2$labs()
    + ggplot2$theme(
      plot.title = ggplot2$element_blank(),
      axis.text.y = ggplot2$element_text(size = 20),
      axis.title.y = ggplot2$element_blank(),
      axis.text.x = ggplot2$element_text(size = 20),
      axis.title.x = ggplot2$element_blank(),
      legend.title = ggplot2$element_text(size = 30, face = "bold"),
      legend.text = ggplot2$element_text(size = 30),
      legend.key.size = grid$unit(1.5, "cm")
    )
  )
  abline_4 <- (
    ggplot2$ggplot()
    + ggplot2$geom_abline(
      intercept = 0,
      slope = 0,
      color = "grey",
      linewidth = 1,
      linetype = "dashed"
    )
    + ggplot2$geom_point(
      data = dplyr$filter(
        statistics_per_observation,
        .data$significancekurtosis == "TRUE"
      ),
      ggplot2$aes(
        x = "Kurtosis",
        y = .data$standardized_observedkurtosis,
        col = !!rlang$sym(statistics_factor_name),
        fill = !!rlang$sym(statistics_factor_name)
      ),
      shape = 21,
      size = 4,
      alpha = 0.8,
      position = "jitter"
    )
    + ggplot2$geom_point(
      data = dplyr$filter(
        statistics_per_observation,
        .data$significancekurtosis == "FALSE"
      ),
      ggplot2$aes(
        x = "Kurtosis",
        y = .data$standardized_observedkurtosis,
        col = !!rlang$sym(statistics_factor_name),
        fill = !!rlang$sym(statistics_factor_name)
      ),
      shape = 21,
      size = 4,
      alpha = 0.2,
      position = "jitter"
    )
    + ggplot2$scale_fill_manual(
      values = statistics_factor_name_col,
      limits = statistics_factor_name_breaks
    )
    + ggplot2$scale_color_manual(
      values = statistics_factor_name_col,
      limits = statistics_factor_name_breaks
    )
    + ggplot2$theme_bw()
    + ggplot2$labs()
    + ggplot2$theme(
      plot.title = ggplot2$element_blank(),
      axis.text.y = ggplot2$element_text(size = 20),
      axis.title.y = ggplot2$element_blank(),
      axis.text.x = ggplot2$element_text(size = 20),
      axis.title.x = ggplot2$element_blank(),
      legend.title = ggplot2$element_text(size = 30, face = "bold"),
      legend.text = ggplot2$element_text(size = 30),
      legend.key.size = grid$unit(1.5, "cm")
    )
  )

  moment_graph <- ggpubr$ggarrange(
    boxplot_1,
    boxplot_2,
    boxplot_3,
    boxplot_4,
    abline_1,
    abline_2,
    abline_3,
    abline_4,
    ncol = 4,
    nrow = 2,
    common.legend = TRUE,
    legend = "bottom"
  )

  if (! is.null(output_path)) {
    ggplot2$ggsave(
      output_path,
      moment_graph,
      dpi = dpi,
      width = 15,
      height = 8
    )
  }
  return(moment_graph)
}


#' @title skr_graph
#' @description
#' Graph of the SKR, compared to null model
#' @param moments_df moments data frame (mean, variance, skewness, kurtosis)
#' @param statistics_factor_name column of data use for colors discrimination
#' @param statistics_factor_name_breaks vector of factor levels of the
#'   statistics_factor_name, same dimension than statistics_factor_name_col
#' @param statistics_factor_name_col vector of colors, same dimension than
#'   statistics_factor_name_breaks
#' @param output_path The path to save the graph
#' @param slope_distance slope of the theoretical distribution
#'   law (default: slope = 1 intercept = 1.86 skew-uniform)
#' @param intercept_distance intercept of the theoretical distribution
#'   law (default: slope = 1 intercept = 1.86 skew-uniform)
#' @param dpi The dpi number to use when we generate png/jpg graph
#' @return A graph instance
#' @examples
#'
#' \donttest{
#'
#' results <- TAD::launch_analysis_tad(
#'   weights = TAD::AB[, 5:102],
#'   weights_factor = TAD::AB[, c("Year", "Plot", "Treatment", "Bloc")],
#'   trait_data = log(TAD::trait[["SLA"]]),
#'   aggregation_factor_name = c("Year", "Bloc"),
#'   statistics_factor_name = (statistics_factor_name <- c("Treatment")),
#'   randomization_number = 100,
#'   slope_distance = (
#'     slope_distance <- TAD::CONSTANTS$SKEW_UNIFORM_SLOPE_DISTANCE
#'   ),
#'   intercept_distance = (
#'     intercept_distance <- TAD::CONSTANTS$SKEW_UNIFORM_INTERCEPT_DISTANCE
#'   )
#' )
#'
#' graph <- TAD::skr_graph(
#'   moments_df = results$weighted_moments,
#'   statistics_factor_name = statistics_factor_name,
#'   statistics_factor_name_breaks = c("Mown_Unfertilized", "Mown_NPK"),
#'   statistics_factor_name_col = c("#1A85FF", "#D41159"),
#'   slope_distance = slope_distance,
#'   intercept_distance = intercept_distance
#' )
#'
#' plot(graph)
#'
#' output_path <- file.path(tempdir(), "outputs")
#' dir.create(output_path)
#' TAD::skr_graph(
#'   moments_df = results$weighted_moments,
#'   statistics_factor_name = statistics_factor_name,
#'   statistics_factor_name_breaks = c("Mown_Unfertilized", "Mown_NPK"),
#'   statistics_factor_name_col = c("#1A85FF", "#D41159"),
#'   slope_distance = slope_distance,
#'   intercept_distance = intercept_distance,
#'   dpi = 200,
#'   output_path = file.path(output_path, "moment_graph.png")
#' )
#'
#' unlink(output_path, recursive = TRUE, force = TRUE)
#'
#' }
#'
#' @export
skr_graph <- function(
  moments_df,
  statistics_factor_name,
  statistics_factor_name_breaks = NULL,
  statistics_factor_name_col = NULL,
  slope_distance = CONSTANTS$SKEW_UNIFORM_SLOPE_DISTANCE,
  intercept_distance = CONSTANTS$SKEW_UNIFORM_INTERCEPT_DISTANCE,
  output_path = NULL,
  dpi = 600
) {
  if (! is.null(output_path)) {
    if (grepl(".svg$", output_path, ignore.case = TRUE)) {
      load_package("svglite")
    }
  }

  ggplot2 <- load_package("ggplot2")
  dplyr <- load_package("dplyr")
  rlang <- load_package("rlang")

  if (is.null(statistics_factor_name_col)) {
    gr_devices <- load_package("grDevices")
    statistics_factor_name_col <- gr_devices$palette()
  }

  .data <- NULL

  skr_graph <- (
    ggplot2$ggplot()
    + ggplot2$geom_point(
      data = dplyr$filter(moments_df, .data$number > 0),
      ggplot2$aes(x = .data$skewness**2, y = .data$kurtosis),
      shape = 21,
      size = 2,
      alpha = 0.4,
      col = "#D3D3D3",
      fill = "#D3D3D3"
    )
    + ggplot2$geom_smooth(
      data = dplyr$filter(moments_df, .data$number > 0),
      ggplot2$aes(
        x = .data$skewness**2,
        y = .data$kurtosis,
        group = .data$number
      ),
      col = "#D3D3D3",
      fill = "#D3D3D3",
      se = FALSE,
      method = "lm",
      formula = y ~ x,
      linetype = 1,
      linewidth = 0.5,
      alpha = 0.1
    )
    + ggplot2$geom_abline(
      intercept = intercept_distance,
      slope = slope_distance,
      linetype = "dashed",
      linewidth = 2
    )
    + ggplot2$geom_point(
      data = dplyr$filter(moments_df, .data$number == 0),
      ggplot2$aes(
        x = .data$skewness**2,
        y = .data$kurtosis,
        fill = !!rlang$sym(statistics_factor_name)
      ),
      shape = 21, size = 6, alpha = 0.4
    )
    + ggplot2$geom_smooth(
      data = dplyr$filter(moments_df, .data$number == 0),
      ggplot2$aes(
        x = .data$skewness**2,
        y = .data$kurtosis,
        col = !!rlang$sym(statistics_factor_name),
        fill = !!rlang$sym(statistics_factor_name)
      ),
      se = FALSE,
      method = "lm",
      formula = y ~ x,
      linetype = 1,
      linewidth = 2,
      alpha = 0.1
    )
    + ggpubr::stat_regline_equation(
      data = dplyr$filter(moments_df, .data$number == 0),
      ggplot2$aes(
        .data$skewness**2,
        y = .data$kurtosis,
        col = !!rlang$sym(statistics_factor_name)
      ),
      alpha = 1,
      size = 8
    )
    + ggplot2$scale_fill_manual(
      limits = statistics_factor_name_breaks,
      values = statistics_factor_name_col
    )
    + ggplot2$scale_color_manual(
      limits = statistics_factor_name_breaks,
      values = statistics_factor_name_col
    )
    + ggplot2$xlim(0, 10)
    + ggplot2$ylim(0, 20)
    + ggplot2$theme_bw()
    + ggplot2$labs(
      x = paste0(
        "Skewness",
        "\u00b2" # compatible character "squared"
      ),
      y = "Kurtosis"
    )
    + ggplot2$theme(
      legend.position = "bottom",
      plot.title = ggplot2$element_blank(),
      axis.text.y = ggplot2$element_text(size = 35),
      axis.title.y = ggplot2$element_text(size = 40),
      axis.title.x = ggplot2$element_text(size = 40),
      axis.text.x = ggplot2$element_text(size = 35)
    )
  )

  if (! is.null(output_path)) {
    ggplot2$ggsave(
      output_path,
      skr_graph,
      dpi = dpi,
      height = 10,
      width = 10
    )
  }
  return(skr_graph)
}


#' skr_param_graph
#' @description
#' Graph of the parameters computed from the SKR, compared to null model
#' @param skr_param SES of SKR parameters data frame (SES and Significance)
#' @param statistics_factor_name column of data use for colors discrimination
#' @param statistics_factor_name_breaks vector of factor levels of the
#'   statistics_factor_name, same dimension than statistics_factor_name_col
#' @param statistics_factor_name_col vector of colors, same dimension than
#'   statistics_factor_name_breaks
#' @param slope_distance slope of the theoretical distribution
#'   law (default: slope = 1 intercept = 1.86 skew-uniform distribution family)
#' @param intercept_distance intercept of the theoretical distribution
#'   law (default: slope = 1 intercept = 1.86 skew-uniform distribution family)
#' @param save_skr_param_graph The path to save the graph
#' @param dpi The dpi number to use when we generate png/jpg graph
#' @return A graph instance
#' @examples
#'
#' \donttest{
#'
#'   results <- TAD::launch_analysis_tad(
#'     weights = TAD::AB[, 5:102],
#'     weights_factor = TAD::AB[, c("Year", "Plot", "Treatment", "Bloc")],
#'     trait_data = log(TAD::trait[["SLA"]]),
#'     aggregation_factor_name = c("Year", "Bloc"),
#'     statistics_factor_name = (statistics_factor_name <- c("Treatment")),
#'     randomization_number = 100,
#'     slope_distance = (
#'       slope_distance <- TAD::CONSTANTS$SKEW_UNIFORM_SLOPE_DISTANCE
#'     ),
#'     intercept_distance = (
#'       intercept_distance <- TAD::CONSTANTS$SKEW_UNIFORM_INTERCEPT_DISTANCE
#'     )
#'   )
#'
#'   # if you want to display the graph
#'   graph <- TAD::skr_param_graph(
#'     skr_param = results$ses_skr,
#'     statistics_factor_name = statistics_factor_name,
#'     statistics_factor_name_breaks = c("Mown_Unfertilized", "Mown_NPK"),
#'     statistics_factor_name_col = c("#1A85FF", "#D41159"),
#'     slope_distance = slope_distance,
#'     intercept_distance = intercept_distance
#'   )
#'
#'   plot(graph)
#'
#'   output_path <- file.path(tempdir(), "outputs")
#'   dir.create(output_path)
#'
#'   # if you want to save the graph as a file
#'   # either jpg, jpeg, png or svg are
#'   TAD::skr_param_graph(
#'     skr_param = results$ses_skr,
#'     statistics_factor_name = statistics_factor_name,
#'     statistics_factor_name_breaks = c("Mown_Unfertilized", "Mown_NPK"),
#'     statistics_factor_name_col = c("#1A85FF", "#D41159"),
#'     slope_distance = slope_distance,
#'     intercept_distance = intercept_distance,
#'     save_skr_param_graph = file.path(output_path, "skr_param_graph.jpeg"),
#'     dpi = 300
#'   )
#'
#'   unlink(output_path, recursive = TRUE, force = TRUE)
#'
#' }
#'
#' @export
skr_param_graph <- function(
  skr_param,
  statistics_factor_name,
  statistics_factor_name_breaks = NULL,
  statistics_factor_name_col = NULL,
  slope_distance = CONSTANTS$SKEW_UNIFORM_SLOPE_DISTANCE,
  intercept_distance = CONSTANTS$SKEW_UNIFORM_INTERCEPT_DISTANCE,
  save_skr_param_graph = NULL,
  dpi = 600
) {

  if (! is.null(save_skr_param_graph)) {
    if (grepl(".svg$", save_skr_param_graph, ignore.case = TRUE)) {
      load_package("svglite")
    }
  }

  if (is.null(statistics_factor_name_col)) {
    gr_devices <- load_package("grDevices")
    statistics_factor_name_col <- gr_devices$palette()
  }

  ggplot2 <- load_package("ggplot2")
  dplyr <- load_package("dplyr")
  rlang <- load_package("rlang")

  if (
    slope_distance == CONSTANTS$SKEW_UNIFORM_SLOPE_DISTANCE
    ## todo better float check
    && intercept_distance == CONSTANTS$SKEW_UNIFORM_INTERCEPT_DISTANCE
  ) {
    title_dist_law <- "TADeve"
  } else {
    title_dist_law <- "distance to \nfamily"
  }

  .data <- NULL

  graph <- (
    ggplot2$ggplot()
    + ggplot2$geom_abline(
      intercept = 0,
      slope = 0,
      color = "grey",
      linewidth = 1,
      linetype = "dashed"
    )
    + ggplot2$geom_point(
      data = dplyr$filter(skr_param, .data$slope_signi == TRUE),
      ggplot2$aes(
        x =  "Slope",
        y = .data$slope_ses,
        fill = !!rlang$sym(statistics_factor_name)
      ),
      alpha = 0.8,
      size = 6,
      color = "black",
      shape = 21
    )
    + ggplot2$geom_point(
      data = dplyr$filter(skr_param, .data$slope_signi == FALSE),
      ggplot2$aes(
        x =  "Slope",
        y = .data$slope_ses,
        fill = !!rlang$sym(statistics_factor_name)
      ),
      alpha = 0.2,
      size = 6,
      color = "black",
      shape = 21
    )
  )
  graph <- (
    graph
    + ggplot2$geom_point(
      data = dplyr$filter(skr_param, .data$intercept_signi == TRUE),
      ggplot2$aes(
        x =  "Intercept",
        y = .data$intercept_ses,
        fill = !!rlang$sym(statistics_factor_name)
      ),
      alpha = 0.8,
      size = 6,
      color = "black",
      shape = 21
    )
    + ggplot2$geom_point(
      data = dplyr$filter(skr_param, .data$intercept_signi == FALSE),
      ggplot2$aes(
        x =  "Intercept",
        y = .data$intercept_ses,
        fill = !!rlang$sym(statistics_factor_name)
      ),
      alpha = 0.2,
      size = 6,
      color = "black",
      shape = 21
    )
  )
  graph <- (
    graph
    + ggplot2$geom_point(
      data = dplyr$filter(skr_param, .data$rsquare_signi == TRUE),
      ggplot2$aes(
        x =  paste0("R", "\u00b2"), # compatible character "squared"
        y = .data$rsquare_ses,
        fill = !!rlang$sym(statistics_factor_name)
      ),
      alpha = 0.8,
      size = 6,
      color = "black",
      shape = 21
    )
    + ggplot2$geom_point(
      data = dplyr$filter(skr_param, .data$rsquare_signi == FALSE),
      ggplot2$aes(
        x =  paste0("R", "\u00b2"), # compatible character "squared"
        y = .data$rsquare_ses,
        fill = !!rlang$sym(statistics_factor_name)
      ),
      alpha = 0.2,
      size = 6,
      color = "black",
      shape = 21
    )
  )
  if ("tad_eve_ses" %in% colnames(skr_param)) {
    graph <- skr_param_graph_tad_eve(
      graph,
      skr_param,
      title_dist_law,
      statistics_factor_name
    )
  } else {
    graph <- skr_param_graph_dist_to_family(
      graph,
      skr_param,
      title_dist_law,
      statistics_factor_name
    )
  }
  graph <- (
    graph
    + ggplot2$geom_point(
      data = dplyr$filter(skr_param, .data$tad_stab_signi == TRUE),
      ggplot2$aes(
        x =  "TADstab",
        y = .data$tad_stab_ses,
        fill = !!rlang$sym(statistics_factor_name)
      ),
      alpha = 0.8,
      size = 6,
      color = "black",
      shape = 21
    )
    + ggplot2$geom_point(
      data = dplyr$filter(skr_param, .data$tad_stab_signi == FALSE),
      ggplot2$aes(
        x =  "TADstab",
        y = .data$tad_stab_ses,
        fill = !!rlang$sym(statistics_factor_name)
      ),
      alpha = 0.2,
      size = 6,
      color = "black",
      shape = 21
    )
  )
  graph <- (
    graph
    + ggplot2$scale_x_discrete(
      limits = c(
        "Slope",
        "Intercept",
        paste0("R", "\u00b2"), # compatible character "squared"
        "TADstab",
        title_dist_law,
        paste0("CV ", title_dist_law)
      )
    )
    + ggplot2$scale_fill_manual(
      limits = statistics_factor_name_breaks,
      values = statistics_factor_name_col
    )
    + ggplot2$scale_color_manual(
      limits = statistics_factor_name_breaks,
      values = statistics_factor_name_col
    )
    + ggplot2$theme_bw()
    + ggplot2$labs(title = paste0("Parameters of the SKR"), y = "SES")
    + ggplot2$theme(
      legend.position = "bottom",
      plot.title = ggplot2$element_text(
        size = 16,
        face = "bold",
        hjust = 0.5
      ),
      axis.text.y = ggplot2$element_text(size = 10),
      axis.title.y = ggplot2$element_text(size = 12, face = "bold"),
      axis.title.x = ggplot2$element_blank(),
      axis.text.x = ggplot2$element_text(size = 10, face = "bold")
    )
  )

  if (! is.null(save_skr_param_graph)) {
    ggplot2$ggsave(
      save_skr_param_graph,
      graph,
      dpi = dpi,
      width = 15,
      height = 5
    )
  }
  return(graph)
}

skr_param_graph_tad_eve <- function(
  graph,
  skr_param,
  title_dist_law,
  statistics_factor_name
) {
  return(
    skr_param_graph_generic(
      graph,
      skr_custom_uniform_names(skr_param),
      title_dist_law,
      statistics_factor_name,
      "TADeve",
      "cv_TADeve"
    )
  )
}

skr_param_graph_dist_to_family <- function(
  graph,
  skr_param,
  title_dist_law,
  statistics_factor_name
) {
  return(
    skr_param_graph_generic(
      graph,
      skr_param,
      title_dist_law,
      statistics_factor_name,
      "distance_to_family",
      "cv_distance_to_family"
    )
  )
}


skr_param_graph_generic <- function(
  graph,
  skr_param,
  title_dist_law,
  statistics_factor_name,
  colname_mean,
  colname_cv
) {
  ggplot2 <- load_package("ggplot2")
  dplyr <- load_package("dplyr")
  rlang <- load_package("rlang")
  .data <- NULL
  graph <- (
    graph
    + ggplot2$geom_point(
      data = dplyr$filter(
        skr_param,
        .data[[sprintf("%s_signi", colname_mean)]] == TRUE
      ),
      ggplot2$aes(
        x = title_dist_law,
        y = .data[[sprintf("%s_ses", colname_mean)]],
        fill = !!rlang$sym(statistics_factor_name)
      ),
      alpha = 0.8,
      size = 6,
      color = "black",
      shape = 21
    )
    + ggplot2$geom_point(
      data = dplyr$filter(
        skr_param,
        .data[[sprintf("%s_signi", colname_mean)]] == FALSE
      ),
      ggplot2$aes(
        x = title_dist_law,
        y = .data[[sprintf("%s_ses", colname_mean)]],
        fill = !!rlang$sym(statistics_factor_name)
      ),
      alpha = 0.2,
      size = 6,
      color = "black",
      shape = 21
    )
  )
  graph <- (
    graph
    + ggplot2$geom_point(
      data = dplyr$filter(
        skr_param,
        .data[[sprintf("%s_signi", colname_cv)]] == TRUE
      ),
      ggplot2$aes(
        x = paste0("cv ", title_dist_law),
        y = .data[[sprintf("%s_ses", colname_cv)]],
        fill = !!rlang$sym(statistics_factor_name)
      ),
      alpha = 0.8,
      size = 6,
      color = "black",
      shape = 21
    )
    + ggplot2$geom_point(
      data = dplyr$filter(
        skr_param,
        .data[[sprintf("%s_signi", colname_cv)]] == FALSE
      ),
      ggplot2$aes(
        x = paste0("cv ", title_dist_law),
        y = .data[[sprintf("%s_ses", colname_cv)]],
        fill = !!rlang$sym(statistics_factor_name)
      ),
      alpha = 0.2,
      size = 6,
      color = "black",
      shape = 21
    )
  )
}
