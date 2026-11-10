# Tests for the Reef Check import (v2.6.0). Fixtures are synthetic.
fixture <- function(f) file.path(project_dir, "tests", "testthat", "fixtures", f)
rc_example <- file.path(project_dir, "data", "example_reefcheck_segments.csv")

test_that("point codes are split into category and bleaching/disease flags", {
  pc <- reefcheck_parse_codes(c("HC", "hc/b", "HC/D ", "RC", "RCTA", "xx", ""))
  expect_equal(pc$code, c("HC", "HC", "HC", "RC", "RC", NA, NA))
  expect_equal(pc$sub[5], "RCTA")
  expect_equal(pc$bleached, c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(pc$diseased, c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))
})

test_that("segment counts become proportions, and rock is never read as turf", {
  counts <- data.frame(site = "A", HC = 10, NIA = 6, RC = 16, RB = 4, SD = 4, hc_bleached = 5, hc_disease = 1)
  tr <- reefcheck_counts_to_transects(counts)
  expect_equal(tr$coral, 10 / 40)
  expect_equal(tr$macroalgae, 6 / 40)
  expect_equal(tr$rubble, 4 / 40)
  expect_equal(tr$bleached, 5 / 10)
  expect_equal(tr$disease, 1 / 10)
  expect_false("turf" %in% names(tr))
  expect_length(attr(tr, "messages"), 0)
})

test_that("rock with turf and rock with coralline algae are used as turf and CCA", {
  counts <- data.frame(site = "A", HC = 10, NIA = 6, RCTA = 12, RCCA = 4, RB = 4, SD = 4)
  tr <- reefcheck_counts_to_transects(counts)
  expect_equal(tr$turf, 12 / 40)
  expect_equal(tr$cca, 4 / 40)
})

test_that("segments without 40 points are reported", {
  tr <- reefcheck_counts_to_transects(data.frame(site = "A", HC = 10, RC = 20))
  expect_true(any(grepl("40 points", attr(tr, "messages"))))
  expect_equal(tr$coral, 10 / 30)
})

test_that("the tool's own files are not mistaken for Reef Check data", {
  expect_true(is.na(reefcheck_table_type(read_transect_file(example_path))))
  expect_equal(reefcheck_table_type(read_transect_file(rc_example)), "segments")
  expect_equal(reefcheck_table_type(read_transect_file(fixture("reefcheck_points.csv"))), "points")
})

test_that("a point table is counted per segment", {
  imp <- import_transect_files(fixture("reefcheck_points.csv"))
  expect_true(imp$reefcheck)
  a1 <- imp$df[imp$df$site == "Alpha" & imp$df$transect == "1", ]
  expect_equal(a1$coral, 16 / 40)       # HC + HC/B + HC/D
  expect_equal(a1$bleached, 4 / 16)
  expect_equal(a1$disease, 2 / 16)
  expect_equal(nrow(imp$df), 6)
  expect_true(any(grepl("Turf-dominated", imp$messages)))
})

test_that("a Reef Check Excel data sheet is read, including the bleaching and disease count rows", {
  skip_if_not_installed("readxl")
  f <- fixture("Paradise 14-03-25 line m.xlsx")
  tr <- read_reefcheck_sheet(f)
  expect_equal(nrow(tr), 4)
  expect_equal(unique(tr$site), "Paradise Reef")
  expect_equal(unique(as.Date(tr$date)), as.Date("2025-03-14"))
  expect_equal(tr$coral, c(16, 12, 8, 20) / 40)
  expect_equal(tr$bleached, c(4 / 16, 6 / 12, 2 / 8, 10 / 20))
  expect_equal(tr$disease, c(1 / 16, 0, 2 / 8, 0))
})

test_that("the full analysis runs on Reef Check data, without turf, CCA, diversity or PC1", {
  imp <- import_transect_files(rc_example)
  p <- prepare_data(imp$df)
  p$messages <- c(imp$messages, p$messages)
  r <- run_analysis(p, list(B = 30, global_draws = 30, seed = 1))
  expect_equal(nrow(r$sites), 8)
  expect_false(isTRUE(r$pca$ok))
  expect_true(all(r$sites$class %in% CLASSES$class))
  expect_true(any(grepl("Reef Check data", r$notes)))
})

test_that("several files in the tool's format are combined", {
  imp <- import_transect_files(c(example_path, example_path), c("a.csv", "b.csv"))
  expect_false(imp$reefcheck)
  expect_equal(nrow(imp$df), 2 * nrow(read_transect_file(example_path)))
})

test_that("Reef Check's database export is read: HC/B and HC/D count as coral, soft coral does not", {
  imp <- import_transect_files(fixture("reefcheck_export_substrate.csv"))
  expect_true(imp$reefcheck)
  d <- imp$df
  expect_equal(nrow(d), 15)                       # 16 segments, one without data
  expect_equal(unique(as.Date(d$date)), as.Date("2025-03-14"))
  r1 <- d[d$site == "Reef One", ][1, ]
  raw <- read.csv(fixture("reefcheck_export_substrate.csv"), check.names = FALSE)
  s1 <- raw[raw$reef_name == "Reef One" & raw$segment_code == "S1", ]
  hc <- sum(as.numeric(s1$total[s1$substrate_code %in% c("HC", "HC/B", "HC/D")]))
  expect_equal(r1$coral, hc / 40)                 # SC not included
  expect_equal(r1$bleached, 2 / hc)
  expect_true(any(grepl("no data", imp$messages)))
})
