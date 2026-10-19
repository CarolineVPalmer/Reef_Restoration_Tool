# =============================================================================
# snapshot.R -- Reef Restoration Prioritisation Tool
#
# Filtering: data period and snapshot (the single time point used for classes).
# Moved verbatim from analysis.R (v2.3.0); roxygen headers added.
# =============================================================================

# -----------------------------------------------------------------------------
# 4. Filtering: data period and snapshot
# -----------------------------------------------------------------------------
#' Keep surveys inside the data period and transects with a coral cover value.
#'
#' @param df Cleaned data (`prepare_data()$df`).
#' @param s Settings list.
#' @return A list with the filtered data (`df`) and notes.
apply_filters <- function(df, s) {
  notes <- character(0)
  from <- as_date_or_na(s$data_from); to <- as_date_or_na(s$data_to)
  if (any(!is.na(df$date)) && (!is.na(from) || !is.na(to))) {
    keep <- !is.na(df$date)
    if (!is.na(from)) keep <- keep & df$date >= from
    if (!is.na(to))   keep <- keep & df$date <= to
    if (sum(!keep)) notes <- c(notes, sprintf("Data period %s to %s: %d row(s) outside the period (or undated) were excluded.",
                                             ifelse(is.na(from), "start", format(from)), ifelse(is.na(to), "end", format(to)), sum(!keep)))
    df <- df[keep, , drop = FALSE]
  }
  df <- df[!is.na(df$coral), , drop = FALSE]
  if (nrow(df) == 0) stop("No transects remain in the data period.")
  list(df = df, notes = notes)
}

#' Select the snapshot: the single time point at which sites are compared.
#'
#' @param df Filtered data.
#' @param s Settings list (`snapshot_mode`, period and window settings).
#' @return A list with the snapshot data, a logical index into `df`, a description, the mode, sites not scored and notes.
#'   Stops if fewer than three sites are present, because relative scores need at least three sites.
choose_snapshot <- function(df, s) {
  # The single time point used for classes, intervals and donor ranks.
  notes <- character(0)
  all_sites <- sort(unique(df$site))
  has_dates <- any(!is.na(df$date))
  mode <- if (!has_dates) "all" else s$snapshot_mode
  if (!has_dates && s$snapshot_mode != "all")
    notes <- c(notes, "No survey dates available: all transects were pooled for each site.")
  in_snap <- rep(FALSE, nrow(df))
  if (mode == "period") {
    ps <- period_start(df$date, s$period_months, s$period_start_month)
    last <- max(ps, na.rm = TRUE)
    in_snap <- !is.na(ps) & ps == last
    desc <- sprintf("Most recent survey period, %s", period_label(last, s$period_months))
  } else if (mode == "latest") {
    last <- tapply(df$date, df$site, function(d) if (all(is.na(d))) NA else max(d, na.rm = TRUE))
    last <- as.Date(last, origin = "1970-01-01")
    in_snap <- !is.na(df$date) & df$date == last[df$site]
    rng <- range(last, na.rm = TRUE)
    desc <- sprintf("Most recent survey at each site (%s to %s)", format(rng[1]), format(rng[2]))
    if (as.numeric(diff(rng)) > 92)
      notes <- c(notes, sprintf("The most recent surveys span %d days (%s to %s). Sites surveyed months apart may differ because of season or events rather than location; consider a survey window.",
                                as.integer(diff(rng)), format(rng[1]), format(rng[2])))
  } else if (mode == "window") {
    wf <- as_date_or_na(s$window_from); wt <- as_date_or_na(s$window_to)
    if (is.na(wf) || is.na(wt)) stop("Choose the start and end of the survey window.")
    in_snap <- !is.na(df$date) & df$date >= wf & df$date <= wt
    desc <- sprintf("Survey window %s to %s (all surveys in the window pooled)", format(wf), format(wt))
  } else {
    in_snap <- rep(TRUE, nrow(df))
    desc <- "All transects in the data period pooled for each site"
    if (has_dates && as.numeric(diff(range(df$date, na.rm = TRUE))) > 183)
      notes <- c(notes, "All data were pooled across more than six months; within-site variation then includes change over time.")
  }
  d <- df[in_snap, , drop = FALSE]
  missing <- setdiff(all_sites, unique(d$site))
  if (length(missing)) notes <- c(notes, sprintf("Not surveyed in the snapshot and not scored: %s.", paste(missing, collapse = ", ")))
  if (length(unique(d$site)) < 3) stop("Fewer than three sites in the snapshot: at least three are needed to compare sites.")
  list(df = d, in_snap = in_snap, description = desc, mode = mode, not_scored = missing, notes = notes)
}
