# End-to-end checks on the synthetic example file.

test_that("the example file runs and gives coherent, reproducible output", {
  prep <- prepare_data(read_transect_file(example_path))
  s <- list(B = 30, global_draws = 50, seed = 42)
  r1 <- run_analysis(prep, s)
  r2 <- run_analysis(prep, s)

  expect_gte(nrow(r1$sites), 3)
  expect_true(all(r1$sites$class %in% CLASSES$class))
  expect_true(all(r1$sites$class_support >= 0 & r1$sites$class_support <= 1))
  expect_true(all(r1$sites$feas_low <= r1$sites$feas_high))
  expect_true(all(r1$sites$urg_low <= r1$sites$urg_high))
  # point scores are relative: standardised components average zero across sites
  expect_equal(mean(r1$sites$feasibility), 0, tolerance = 1e-8)
  # the same seed gives the same result
  expect_identical(r1$sites$class_support, r2$sites$class_support)
  expect_identical(r1$sites$feas_low, r2$sites$feas_low)
  # the equal-weights benchmark is reported with the other scenarios (v2.4.0)
  expect_true("Equal weights (benchmark)" %in% names(r1$sensitivity$presets))
  expect_true("Equal weights (benchmark)" %in% r1$sensitivity$by_scenario$scenario)
  st <- r1$sensitivity$by_site$stability_rank_preserving
  expect_true(all(st >= 0 & st <= 1))
})

test_that("a saved settings file reproduces the settings", {
  prep <- prepare_data(read_transect_file(example_path))
  s <- modifyList(DEFAULT_SETTINGS, list(B = 20, seed = 7, min_transects = 4))
  w <- apply_preset("Marine heatwave (during or after)")
  f <- tempfile(fileext = ".csv")
  utils::write.csv(settings_table(s, w), f, row.names = FALSE)
  back <- read_settings(f)
  expect_equal(back$weights$weight, w$weight)
  expect_equal(back$settings$seed, 7)
  expect_equal(back$settings$min_transects, 4)
})
