
test_that("initialize_drought_features_table", {
  res <- calculate_KBDI(tab_clim)
  drought_features_table <- initialize_drought_features_table(res)

  expect_equal(dim(drought_features_table), c(60, 21))

  expect_equal(colnames(drought_features_table), c(
    'Drying.rate', 'Wetting.rate', 'Intercept.drying', 'Intercept.wetting', 'Peak.KBDI',
    'Peak.KBDI.DOY', 'RP.num', 'RP.I.mean', 'RP.sd', 'Low.D.Onset', 'Mod.D.Onset',
    'Extreme.D.Onset', 'Low.D.Offset', 'Mod.D.Offset', 'Extreme.D.Offset',
    'Low.D.Duration', 'Mod.D.Duration', 'Extreme.D.Duration',
    'Low.D.S', 'Mod.D.S', 'Extreme.D.S'
  ))

  expect_true(all(is.na(drought_features_table)))

  expect_true(all(rownames(drought_features_table) == paste("Hydro.Year.", 1960:2019, sep = "")))
})

