

# Alternatively, you can capture all messages in a single call
test_that("calculate_KBDI", {
  res <- calculate_KBDI(tab_clim)
  expect_equal(0.641, res$KBDI[1], tolerance = 1e-3)

 outt <- tab_clim
 colnames(outt) <- colnames(tab_clim) <- c('yr', 'month', 'day', 'prec', 'Tmean', 'Tmax', 'Tmin', 'PET_monteith')

 expect_error( calculate_KBDI(tab_clim), "The data must contain columns: 'year', 'month', and 'day'")

})

