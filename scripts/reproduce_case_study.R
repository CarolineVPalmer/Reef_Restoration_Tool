# =============================================================================
# reproduce_case_study.R -- Isla del Caño case study (manuscript Results)
#
# Re-runs the case study with the saved settings file and writes every table
# and figure used in the manuscript to outputs/case_study/. It then compares
# the main tables with the reference outputs produced on 30 September 2026
# (outputs/reference_v2.3.0/; held with the case-study data, not in the public
# repository) and reports any difference.
#
# Run from the project folder (the one containing app.R):
#   source("scripts/reproduce_case_study.R")
#
# STATUS: rewritten on 19 October 2026 because the original script was not
# found. It has not yet been run. Run it once and check the comparison report
# at the end before relying on it.
#
# VERSIONS: the reference outputs were made with v2.3.0. To check that the
# restructuring changed nothing, run this script on git tag v2.3.0 first: every
# table should then be "identical" (Table3 and the classes-with-and-without-trend
# table may differ in column layout only). From v2.4.0 (CCA removed from the
# algal-dominance PCA, minimum-transect flag 3, equal-weights benchmark) the
# report will show differences; these are expected, and the new outputs replace
# the reference for the manuscript.
#
# Expected run time: a few minutes (B = 1000 bootstrap replicates, plus one
# run per survey period for the classes-by-period table).
# =============================================================================

stopifnot(file.exists("app.R"), dir.exists("R"))
for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f)

# The cleaned copy (empty spreadsheet rows removed; see the data repository's
# cleaning_log.txt) is used if present; results are identical because the tool
# ignores rows without a site name.
data_file <- file.path("data", "case_study", "prevsub2026a_clean.csv")
if (!file.exists(data_file)) data_file <- file.path("data", "case_study", "prevsub2026a.csv")
settings_file <- file.path("data", "case_study", "settings_case_study.csv")
ref_dir       <- file.path("outputs", "reference_v2.3.0")
out_dir       <- file.path("outputs", "case_study")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_file)) {
  stop("Case-study data not found at ", data_file,
       ". The data are not in the public repository until their release is agreed; see README.")
}

out <- function(name) file.path(out_dir, name)
save_csv <- function(x, name) utils::write.csv(x, out(name), row.names = FALSE)
save_png <- function(p, name, width = 8, height = 7) {
  if (!is.null(p)) ggplot2::ggsave(out(name), p, width = width, height = height, dpi = 300, bg = "white")
}

# ---- 1. Main analysis (most recent survey period) -----------------------------
st   <- read_settings(settings_file)
prep <- prepare_data(read_transect_file(data_file), day_first = isTRUE(st$settings$day_first))
r    <- run_analysis(prep, st$settings, st$weights)

save_csv(site_table(r, digits = 3), "Table2_site_metrics_and_scores.csv")
save_csv(merge(r$sensitivity$by_site, r$sensitivity$presets, by = "site"), "Table3_weight_sensitivity_by_site.csv")
save_csv(r$sensitivity$by_scenario, "Table3b_weight_sensitivity_by_scenario.csv")
save_csv(r$donors, "Table4_donor_shortlist.csv")
if (!is.null(r$trends$table))     save_csv(r$trends$table, "Table5_site_trends.csv")
if (!is.null(r$trends$reef_mean)) save_csv(r$trends$reef_mean, "Table5b_reef_wide_change.csv")
save_csv(r$diagnostics, "TableS1_diagnostics.csv")
if (!is.null(r$pca$loadings))     save_csv(r$pca$loadings, "TableS_PCA_loadings.csv")
if (!is.null(r$trajectories))     save_csv(r$trajectories, "TableS_trajectories.csv")
save_csv(settings_table(r$settings, r$weights_requested), "settings_case_study.csv")

save_png(plot_decision_plane(r, base_size = 12), "Fig3_decision_plane.png")
save_png(plot_trend_scores(r, base_size = 12), "Fig4a_trend_scores.png", height = 6)
save_png(plot_trend_heatmap(r, base_size = 12), "Fig4b_trend_by_indicator.png", width = 9, height = 7)
save_png(plot_pca(r, base_size = 12), "FigS1_PCA_biplot.png")
save_png(plot_trajectories(r, base_size = 11), "FigS2_trajectories.png", width = 11, height = 9)

# ---- 2. Classes with and without the trend --------------------------------------
s_no_trend <- modifyList(st$settings, list(include_trends = FALSE))
r_no_trend <- run_analysis(prep, s_no_trend, st$weights)
cmp_trend <- data.frame(site = r$sites$site,
                        class_with_trend = r$sites$class,
                        class_without_trend = r_no_trend$sites$class[match(r$sites$site, r_no_trend$sites$site)])
cmp_trend$changed <- cmp_trend$class_with_trend != cmp_trend$class_without_trend
save_csv(cmp_trend, "TableS_classes_with_and_without_trend.csv")

# ---- 3. Classes in each survey period --------------------------------------------
# Each survey period is used as the snapshot in turn (a survey window covering
# the period). Periods with fewer than three sites cannot be scored and are skipped.
dfp <- apply_filters(prep$df, modifyList(DEFAULT_SETTINGS, st$settings))$df
p_starts <- sort(unique(period_start(dfp$date, st$settings$period_months, st$settings$period_start_month)))
by_period <- list()
for (k in seq_along(p_starts)) {
  p0 <- p_starts[k]
  p1 <- seq(p0, by = "month", length.out = st$settings$period_months + 1)[st$settings$period_months + 1] - 1
  s_k <- modifyList(st$settings, list(snapshot_mode = "window", window_from = p0, window_to = p1))
  r_k <- tryCatch(run_analysis(prep, s_k, st$weights), error = function(e) NULL)
  if (is.null(r_k)) next
  lab <- period_label(p0, st$settings$period_months)
  by_period[[lab]] <- data.frame(site = r_k$sites$site,
                                 value = sprintf("%s (%.0f%%, n=%d)", r_k$sites$pathway,
                                                 100 * r_k$sites$class_support, r_k$sites$n_transects))
}
all_sites <- sort(unique(dfp$site))
cls_tab <- data.frame(site = all_sites)
for (lab in names(by_period)) {
  x <- by_period[[lab]]
  cls_tab[[lab]] <- ifelse(is.na(match(all_sites, x$site)), "", x$value[match(all_sites, x$site)])
}
save_csv(cls_tab, "TableS_classes_by_survey_period.csv")

# ---- 4. Run log --------------------------------------------------------------------
s <- r$settings
q <- r$quadrant_counts
log_lines <- c(
  sprintf("Reef Restoration Prioritisation Tool v%s -- case study run %s", s$version, s$run_at),
  paste0("Data file: ", basename(data_file)),
  paste0("Snapshot: ", r$snapshot$description),
  sprintf("Sites scored: %d; transects: %d; not scored: %s", nrow(r$sites), sum(r$sites$n_transects),
          paste(r$snapshot$not_scored, collapse = ", ")),
  sprintf("Feasibility-Urgency Spearman rho: %.2f", r$rho),
  paste0("Sites per class: ", paste(sprintf("%s %s = %d", CLASSES$pathway, names(q), as.integer(q)), collapse = "; ")),
  sprintf("Borderline (interval crosses zero): %d of %d; median class support %.0f%%",
          sum(r$sites$borderline != "No"), nrow(r$sites), 100 * stats::median(r$sites$class_support)),
  sprintf("Weight sensitivity: median stability one-at-a-time %.0f%%, global %.0f%%, rank-preserving %.0f%%",
          100 * stats::median(r$sensitivity$by_site$stability_one_at_a_time),
          100 * stats::median(r$sensitivity$by_site$stability_global),
          100 * stats::median(r$sensitivity$by_site$stability_rank_preserving)),
  sprintf("Equal-weights benchmark: %d of %d classes changed",
          sum(r$sensitivity$presets[["Equal weights (benchmark)"]] != r$sites$class), nrow(r$sites)),
  sprintf("Classes changed by the trend: %d", sum(cmp_trend$changed)),
  "", "Notes:", paste0("- ", r$notes),
  "", "Formulas:", paste0("- ", methods_text(r)),
  "", utils::capture.output(utils::sessionInfo()))
writeLines(log_lines, out("run_log.txt"))

# ---- 5. Compare with the reference outputs (30 September 2026) ---------------------
compare_tables <- function(name, key = "site", tol = 1e-3) {
  a_path <- out(name); b_path <- file.path(ref_dir, name)
  if (!file.exists(a_path) || !file.exists(b_path)) return(sprintf("%-45s not compared (file missing)", name))
  a <- utils::read.csv(a_path, stringsAsFactors = FALSE)
  b <- utils::read.csv(b_path, stringsAsFactors = FALSE)
  if (!identical(sort(names(a)), sort(names(b)))) return(sprintf("%-45s DIFFERENT columns", name))
  if (key %in% names(a)) { a <- a[order(a[[key]]), ]; b <- b[order(b[[key]]), ] }
  if (nrow(a) != nrow(b)) return(sprintf("%-45s DIFFERENT number of rows (%d vs %d)", name, nrow(a), nrow(b)))
  bad <- character(0)
  for (v in names(a)) {
    x <- a[[v]]; y <- b[[v]]
    same <- if (is.numeric(x) && is.numeric(y)) isTRUE(all(abs(x - y) <= tol | (is.na(x) & is.na(y)))) else
      identical(as.character(x), as.character(y))
    if (!same) bad <- c(bad, v)
  }
  if (length(bad)) sprintf("%-45s DIFFERENT in: %s", name, paste(bad, collapse = ", ")) else sprintf("%-45s identical", name)
}
report <- vapply(c("Table2_site_metrics_and_scores.csv", "Table3_weight_sensitivity_by_site.csv",
                   "Table3b_weight_sensitivity_by_scenario.csv", "Table4_donor_shortlist.csv",
                   "Table5_site_trends.csv", "Table5b_reef_wide_change.csv", "TableS1_diagnostics.csv",
                   "TableS_PCA_loadings.csv", "TableS_classes_by_survey_period.csv",
                   "TableS_classes_with_and_without_trend.csv"),
                 compare_tables, character(1), USE.NAMES = FALSE)
writeLines(report, out("comparison_with_reference.txt"))
cat("\nComparison with the reference outputs (30 September 2026):\n", paste0(report, "\n"), sep = "")
cat("\nOutputs written to ", normalizePath(out_dir), "\n", sep = "")
