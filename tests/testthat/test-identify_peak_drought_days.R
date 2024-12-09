
test_that("identify_peak_drought_days", {
  res <- calculate_KBDI(tab_clim)
  res1 <- identify_peak_drought_days(res)

  expect_equal(292, res1$Peak.KBDI.DOY[1])

  outt <- res
  colnames(outt) <- c('yr', 'month', 'day','DF', 'KBDI')

  expect_error( identify_peak_drought_days(outt), "The data must contain columns: 'year', 'month', 'day', and 'KBDI'")

})

