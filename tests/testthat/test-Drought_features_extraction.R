test_that("Drought_features_extraction", {
  res <- calculate_KBDI(tab_clim)
  res1 <- identify_peak_drought_days(res)
  res2 <- identify_min_kbdi_between_two_drought_peaks(res, res1)
  drought_features_table <- initialize_drought_features_table(res)

  DF <- Drought_features_extraction(kbdi_data = res,
                                    minDmax = res2,
                                    drought_features_table = drought_features_table,
                                    save_to_csv = F,
                                    plot_features_extraction = F)

  expect_equal(4, DF$RP.num[1])
  expect_equal(126, DF$Low.D.Onset[1])

  outt <- res
  colnames(outt) <- c('yr', 'month', 'day','DF', 'KBDI')

  expect_error(Drought_features_extraction(kbdi_data = outt,minDmax = res2, drought_features_table = drought_features_table, save_to_csv = F,
  plot_features_extraction = F), "The KBDI data must contain the following columns: 'year', 'month', 'day', and 'KBDI'")

})

