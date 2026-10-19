# Tests for the optional bleaching-event mode (R/event.R), v2.5.0.

event_settings <- list(
  event_before_from = "2024-06-01", event_before_to = "2024-06-30",
  event_use_after = TRUE, event_after_from = "2025-09-01", event_after_to = "2025-09-30"
)

test_that("event comparison reports the change from before to the latest phase", {
  prep <- prepare_data(read_transect_file(example_path))
  ev <- event_comparison(prep$df, event_phases_from_settings(event_settings), B = 50, seed = 1)
  expect_true(all(c("site", "coral_change_pp", "coral_change_low", "coral_change_high") %in% names(ev$sites)))

  # sites surveyed in both phases have a change; the others carry a note
  both <- ev$sites$transects_reference > 0 & !is.na(ev$sites$compared_with)
  expect_true(any(both))
  expect_false(any(is.na(ev$sites$coral_change_pp[both])))
  expect_true(all(ev$sites$note[!both] != ""))

  # the point estimate is the difference of the phase means
  d <- prep$df[!is.na(prep$df$coral), ]
  s <- ev$sites$site[both][1]
  in_phase <- function(from, to) d$site == s & d$date >= as.Date(from) & d$date <= as.Date(to)
  expected <- 100 * (mean(d$coral[in_phase("2025-09-01", "2025-09-30")]) -
                     mean(d$coral[in_phase("2024-06-01", "2024-06-30")]))
  expect_equal(ev$sites$coral_change_pp[ev$sites$site == s], expected)
  expect_true(all(ev$sites$coral_change_low <= ev$sites$coral_change_high, na.rm = TRUE))
})

test_that("event comparison needs 'before' and at least one later phase", {
  prep <- prepare_data(read_transect_file(example_path))
  only_before <- event_phases_from_settings(list(event_before_from = "2024-06-01", event_before_to = "2024-06-30"))
  expect_error(event_comparison(prep$df, only_before, B = 10), "during or after")
  no_before <- event_phases_from_settings(list(event_use_after = TRUE,
                                               event_after_from = "2025-09-01", event_after_to = "2025-09-30"))
  expect_error(event_comparison(prep$df, no_before, B = 10), "before")
})

test_that("the standard analysis is unaffected by event settings", {
  prep <- prepare_data(read_transect_file(example_path))
  s1 <- list(B = 20, global_draws = 20, seed = 3)
  s2 <- c(s1, event_settings)
  expect_identical(run_analysis(prep, s1)$sites$class, run_analysis(prep, s2)$sites$class)
})

test_that("event settings are saved and reloaded", {
  s <- modifyList(DEFAULT_SETTINGS, event_settings)
  f <- tempfile(fileext = ".csv")
  utils::write.csv(settings_table(s, DEFAULT_WEIGHTS), f, row.names = FALSE)
  back <- read_settings(f)$settings
  expect_equal(back$event_before_from, as.Date("2024-06-01"))
  expect_true(back$event_use_after)
  expect_false(back$event_use_during)
})

test_that("without 'before', change is measured from 'during' and flagged as an underestimate", {
  prep <- prepare_data(read_transect_file(example_path))
  ph <- event_phases_from_settings(list(
    event_use_during = TRUE, event_during_from = "2024-06-01", event_during_to = "2024-06-30",
    event_use_after = TRUE, event_after_from = "2025-09-01", event_after_to = "2025-09-30"))
  ev <- event_comparison(prep$df, ph, B = 20, seed = 2)
  expect_equal(ev$reference, "during")
  expect_true(all(ev$sites$reference_phase == "during"))
  expect_true(any(grepl("underestimates", ev$notes)))
  only_during <- event_phases_from_settings(list(
    event_use_during = TRUE, event_during_from = "2024-06-01", event_during_to = "2024-06-30"))
  expect_error(event_comparison(prep$df, only_during, B = 10), "after")
})

test_that("'before' can be switched off even when its dates are filled in (as in the app)", {
  prep <- prepare_data(read_transect_file(example_path))
  ph <- event_phases_from_settings(list(
    event_use_before = FALSE, event_before_from = "2024-06-01", event_before_to = "2024-06-30",
    event_use_during = TRUE, event_during_from = "2024-06-01", event_during_to = "2024-06-30",
    event_use_after = TRUE, event_after_from = "2025-09-01", event_after_to = "2025-09-30"))
  expect_true(is.na(ph$from[ph$phase == "before"]))
  ev <- event_comparison(prep$df, ph, B = 20, seed = 2)
  expect_equal(ev$reference, "during")
})
