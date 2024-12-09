
test_that("check_years_days_and_start", {
  expect_message(
    check_years_days_and_start(tab_clim),
    "The years are in chronological order",
    fixed = TRUE
  )
  expect_message(
    check_years_days_and_start(tab_clim),
    "All years have 365 or 366 days",
    fixed = TRUE
  )
  expect_message(
    check_years_days_and_start(tab_clim),
    "All years start on January 1st",
    fixed = TRUE
  )

  tab_clim <- tab_clim[-367,]
  expect_warning(check_years_days_and_start(tab_clim),
                 "Some years do not start on January 1st",
                 fixed = TRUE)
})



