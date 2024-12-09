test_that("check_columns_names", {
  res <- check_columns_names(tab_clim)
  expect_message(check_columns_names(tab_clim), 'Extra columns found: tmean, tmin, pet_monteith')
})


