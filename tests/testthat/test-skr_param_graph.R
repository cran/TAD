
test_with_dir("SKR Graphs", {
  testthat::expect_no_error(
    skr_param_graph(
      skr_param = TAD::skr_ses_dataframe,
      statistics_factor_name = c("Treatment"),
      statistics_factor_name_breaks = c("Mown_Unfertilized", "Mown_NPK"),
      statistics_factor_name_col = c("#1A85FF", "#D41159")
    )
  )

})
