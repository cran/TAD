
test_with_dir("Moments Graphs", {

  testthat::expect_no_error(
    invisible(moments_graph(
      moments_df = TAD::weighted_moments_dataframe,
      statistics_per_observation = TAD::stat_per_obs_dataframe,
      statistics_factor_name = c("Treatment"),
      statistics_factor_name_breaks = c("Mown_Unfertilized", "Mown_NPK"),
      statistics_factor_name_col = c("#1A85FF", "#D41159")
    ))
  )

})
