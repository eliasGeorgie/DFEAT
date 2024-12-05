# Alternatively, you can capture all messages in a single call
test_that("check_missing_values", {
  expect_message(
    check_missing_values(tab_clim),
    "No missing values (NA) detected",
    fixed = TRUE
  )

  out <- tab_clim
  out$Tmean[1] <- NA
  expect_warning(
    check_missing_values(out),
    "Missing values are detected",
    fixed = TRUE
  )
})
