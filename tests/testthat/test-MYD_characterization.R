test_that("MYD_characterization", {
  res <- calculate_KBDI(tab_clim)
  res1 <- identify_peak_drought_days(res)
  res2 <- identify_min_kbdi_between_two_drought_peaks(res, res1)
  drought_features_table <- initialize_drought_features_table(res)

  DF <- Drought_features_extraction(kbdi_data = res,
                                    minDmax = res2,
                                    drought_features_table = drought_features_table,
                                    save_to_csv = F,
                                    plot_features_extraction = F)

  MYD_features_calculation <- MYD_characterization(kbdi_data = res,
                                                   minDmax = res2,
                                                   drought_features_data = DF,
                                                   save_to_csv = FALSE)

  expect_equal(26.7414, MYD_features_calculation$RP.I.mean[1], tolerance = 1e-3)
  expect_equal(275, MYD_features_calculation$Low.D.Duration[1])

  outt <- res
  colnames(outt) <- c('yr', 'month', 'day','DF', 'KBDI')

  expect_error(MYD_characterization(kbdi_data = outt, minDmax = res2, drought_features_data = DF, save_to_csv = F),
                                           "The KBDI data must contain the following columns: 'year', 'month', 'day', and 'KBDI'")

})

