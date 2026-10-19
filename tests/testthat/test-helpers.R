# Unit tests for the small helpers that every score depends on.
# Written 19 October 2026 to replace the original test suite, which was not found.

test_that("zscore standardises and treats missing values as the mean", {
  expect_equal(zscore(c(1, 2, 3)), c(-1, 0, 1))
  expect_equal(zscore(c(1, NA, 3)), c(-1, 0, 1) / sqrt(2))
  expect_equal(zscore(c(5, 5, 5)), c(0, 0, 0))   # no variation: drops out
  expect_equal(zscore(c(1, NA)), c(0, 0))        # fewer than two values: drops out
})

test_that("shannon gives log(k) for k equally common taxa and NA for empty transects", {
  counts <- rbind(c(5, 5, 0), c(2, 2, 2), c(0, 0, 0))
  h <- shannon(counts)
  expect_equal(h[1:2], c(log(2), log(3)))
  expect_true(is.na(h[3]))
})

test_that("survey periods run Mar-May, Jun-Aug, Sep-Nov and Dec-Feb by default", {
  d <- as.Date(c("2025-03-01", "2025-05-31", "2025-02-28", "2026-01-15", "2025-12-01"))
  expect_equal(period_start(d), as.Date(c("2025-03-01", "2025-03-01", "2024-12-01", "2025-12-01", "2025-12-01")))
  expect_equal(period_label(as.Date("2025-12-01")), "Dec 2025 to Feb 2026")
})

test_that("classes follow the signs of Feasibility and Urgency", {
  expect_equal(classify(c(1, -1, 1, -1), c(1, 1, -1, -1)),
               c("Immediate restoration", "Triage and foundational support",
                 "Proactive reinforcement", "Monitoring only"))
  expect_equal(classify(0, 0), "Immediate restoration")   # zero goes to the higher class
})

test_that("dates are read day first and implausible readings are rejected", {
  expect_equal(parse_dates(c("05/01/2025", "2025-01-05", "31/12/2024")),
               as.Date(c("2025-01-05", "2025-01-05", "2024-12-31")))
  expect_true(is.na(parse_dates("13/45/2024")))
  expect_equal(parse_dates("05/01/2025", day_first = FALSE), as.Date("2025-05-01"))
})

test_that("numbers with decimal commas and missing-value codes are read", {
  expect_equal(to_number(c("0,45", "1.5", "", "NA", "abc")), c(0.45, 1.5, NA, NA, NA))
})

test_that("month names are read in English and Spanish", {
  expect_equal(month_number(c("Jan", "enero", "3", "Sept", "13")), c(1L, 1L, 3L, 9L, NA))
})

test_that("default weights are within their allowed ranges and directions are fixed", {
  w <- DEFAULT_WEIGHTS
  expect_true(all(w$weight >= w$min & w$weight <= w$max))
  expect_true(all(w$sign %in% c(-1, 1)))
  expect_false(any(duplicated(w$id)))
})

test_that("CCA is not part of the algal-dominance PCA (v2.4.0)", {
  expect_false("cca" %in% PCA_SETS$composition)
  expect_true(all(c("coral", "turf", "cyanobacteria", "macroalgae") %in% PCA_SETS$composition))
  # CCA counts in Feasibility instead, as a positive indicator
  cca <- DEFAULT_WEIGHTS[DEFAULT_WEIGHTS$id == "f_cca", ]
  expect_equal(c(cca$sign, cca$weight), c(1, 1))
  expect_true("mean_cca" %in% Z_VARS)
})

test_that("presets change weights but never directions", {
  for (p in c(names(PRESETS), names(SENSITIVITY_SCENARIOS))) {
    w <- apply_preset(p)
    expect_identical(w$sign, DEFAULT_WEIGHTS$sign)
    expect_true(all(w$weight >= w$min & w$weight <= w$max))
  }
  expect_equal(apply_preset("Disease outbreak")$weight[DEFAULT_WEIGHTS$id == "u_disease"], 3)
})
