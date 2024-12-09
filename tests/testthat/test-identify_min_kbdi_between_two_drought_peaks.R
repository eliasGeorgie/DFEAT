test_that("identify_min_kbdi_between_two_drought_peaks", {
  res <- calculate_KBDI(tab_clim)
  res1 <- identify_peak_drought_days(res)
  res2 <- identify_min_kbdi_between_two_drought_peaks (res, res1)
  expect_equal(412, res2$minDmax_DOY[1])

  outt <- res1
  outt <- outt[2:dim(outt)[1],]

  expect_error( identify_min_kbdi_between_two_drought_peaks(res, outt), "Mismatch in years between kbdi_data and peak_drought_data:\n")

})

