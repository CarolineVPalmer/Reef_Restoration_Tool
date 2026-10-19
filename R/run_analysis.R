# =============================================================================
# run_analysis.R -- Reef Restoration Prioritisation Tool
#
# Full analysis: point estimates, bootstrap, trends, donor shortlist, diagnostics.
# Moved verbatim from analysis.R (v2.3.0); roxygen headers added.
# =============================================================================

# -----------------------------------------------------------------------------
# 8. Full analysis
# -----------------------------------------------------------------------------
#' Warnings when survey methods (e.g. transects and photogrammetry) are mixed.
#'
#' @param d Snapshot data.
#' @param dfp Data for the whole data period.
#' @return Character vector of notes.
method_notes <- function(d, dfp) {
  # Warnings when survey methods (e.g. transects vs photogrammetry) are mixed.
  if (!"method" %in% names(d)) return(character(0))
  out <- character(0)
  m_snap <- sort(unique(stats::na.omit(d$method)))
  if (length(m_snap) > 1) {
    by <- vapply(m_snap, function(k) paste(sort(unique(d$site[d$method %in% k])), collapse = ", "), "")
    out <- c(out, sprintf("Survey methods differ within the snapshot (%s). Sites are compared directly, so differences between methods may appear as differences between sites.",
                          paste(sprintf("%s: %s", m_snap, by), collapse = "; ")))
  }
  n_m <- tapply(dfp$method, dfp$site, function(v) length(unique(stats::na.omit(v))))
  changed <- names(n_m)[!is.na(n_m) & n_m > 1]
  if (length(changed))
    out <- c(out, sprintf("The survey method changed over time at: %s. Trends and trajectories at these sites may partly reflect the change of method; ideally run both methods side by side for at least one survey round.",
                          paste(changed, collapse = ", ")))
  out
}

#' Run the full analysis.
#'
#' @param prep Output of `prepare_data()`.
#' @param s Settings list; missing entries take `DEFAULT_SETTINGS`.
#' @param w Weights table (default `DEFAULT_WEIGHTS`).
#' @param progress Optional function(value, message) for progress reporting.
#' @return A list with site results, donor shortlist, PCA, diagnostics, notes, Feasibility-Urgency correlation, class counts,
#'   weight sensitivity, trends, trajectories, snapshot description, bootstrap matrices, weights and settings.
#'   The random seed is set from `s$seed`, so a run is reproducible from its settings file.
run_analysis <- function(prep, s = DEFAULT_SETTINGS, w = DEFAULT_WEIGHTS, progress = NULL) {
  s <- modifyList(DEFAULT_SETTINGS, s)
  tick <- function(x, msg) if (is.function(progress)) progress(x, msg)
  notes <- prep$messages
  f <- apply_filters(prep$df, s); notes <- c(notes, f$notes)
  dfp <- f$df
  snap <- choose_snapshot(dfp, s); notes <- c(notes, snap$notes)
  d <- snap$df
  sites <- sort(unique(d$site))
  notes <- c(notes, method_notes(d, dfp))
  pca_vars <- PCA_SETS[[s$pca_set]]

  # indicators available in this snapshot
  w_eff <- w
  unavailable <- character(0)
  for (k in seq_len(nrow(w_eff))) {
    ind <- w_eff$indicator[k]
    raw <- sub("^mean_", "", ind)
    if (w_eff$group[k] %in% c("Feasibility", "Urgency", "Donor") && ind %in% Z_VARS && !ind %in% c("pc1", "trend", "cv_coral", "mean_coral")) {
      # an indicator is used only if it was recorded at more than half of the sites
      # (otherwise imputing the mean for the rest would be meaningless)
      site_has <- if (raw %in% names(d)) tapply(d[[raw]], d$site, function(v) any(!is.na(v))) else FALSE
      if (mean(site_has) <= 0.5) {
        if (w_eff$weight[k] > 0) unavailable <- c(unavailable, w_eff$label[k])
        w_eff$weight[k] <- 0
      }
    }
  }
  unavailable <- unique(unavailable)
  if (length(unavailable)) notes <- c(notes, sprintf("Recorded at half of the sites or fewer in the snapshot, so given no weight: %s.", paste(unavailable, collapse = ", ")))

  # trends
  trend_used <- FALSE; tr <- NULL; trend_vec <- NULL
  if (isTRUE(s$include_trends)) {
    if (!any(!is.na(dfp$date))) {
      notes <- c(notes, "The file has no dates: trends not used.")
    } else {
      tr <- trend_setup(dfp, w_eff, s)
      if (tr$n_eligible >= 3) {
        trend_used <- TRUE
        tr$slopes <- trend_slopes(dfp, tr$period, tr$tmid, tr)
        tr$score <- trend_score(tr$slopes, tr)
        trend_vec <- unname(tr$score[sites])
        n_in <- sum(sites %in% names(tr$score))
        notes <- c(notes, sprintf("Trend adjustment used: %d of the %d scored sites have >= %d survey periods spanning >= %d days; the others get a neutral trend (0).",
                                  n_in, length(sites), s$trend_min_periods, s$trend_min_days))
      } else {
        notes <- c(notes, sprintf("Only %d site(s) have >= %d survey periods spanning >= %d days (3 needed): trends not used.",
                                  tr$n_eligible, s$trend_min_periods, s$trend_min_days))
      }
    }
  }
  if (!trend_used) w_eff$weight[w_eff$indicator == "trend"] <- 0

  # point estimates
  tick(0.1, "Scoring sites")
  m0 <- unit_metrics(d, d$site, sites)
  sc <- score_units(m0, w_eff, pca_vars, trend_vec)
  m <- sc$m; names(m)[names(m) == "unit"] <- "site"
  notes <- c(notes, sc$pca$note)

  # bootstrap: transects are resampled within each survey (site x date) across
  # the whole data period; the full calculation (standardisation, PCA, indices,
  # class, donor rank and, if used, trend slopes) is repeated each time.
  tick(0.2, "Bootstrap")
  set.seed(s$seed)
  strata <- paste(dfp$site, ifelse(is.na(dfp$date), "", format(dfp$date)))
  idx <- split(seq_len(nrow(dfp)), strata)
  B <- s$B
  bF <- bU <- bD <- matrix(NA_real_, length(sites), B)
  bC <- matrix(NA_character_, length(sites), B)
  el_sites <- if (trend_used) rownames(tr$slopes) else character(0)
  bT <- matrix(NA_real_, length(el_sites), B, dimnames = list(el_sites, NULL))
  bS <- bT
  pc1_ok <- logical(B)
  for (b in seq_len(B)) {
    ii <- unlist(lapply(idx, function(i) i[sample.int(length(i), length(i), replace = TRUE)]), use.names = FALSE)
    snap_rows <- ii[snap$in_snap[ii]]
    db <- dfp[snap_rows, , drop = FALSE]
    tv <- NULL
    if (trend_used) {
      sl <- trend_slopes(dfp[ii, , drop = FALSE], tr$period[ii], tr$tmid[ii], tr)
      ts <- trend_score(sl, tr)
      bT[, b] <- ts[el_sites]; bS[, b] <- sl[el_sites, if ("coral" %in% colnames(sl)) "coral" else 1]
      tv <- unname(ts[sites])
    }
    r <- score_units(unit_metrics(db, db$site, sites), w_eff, pca_vars, tv)
    bF[, b] <- r$m$feasibility; bU[, b] <- r$m$urgency; bD[, b] <- r$m$donor_score; bC[, b] <- r$m$class
    pc1_ok[b] <- r$pca$ok
    if (b %% 20 == 0) tick(0.2 + 0.6 * b / B, sprintf("Bootstrap %d/%d", b, B))
  }
  q <- function(M, p) apply(M, 1, stats::quantile, probs = p, na.rm = TRUE, names = FALSE)
  m$feas_low <- q(bF, 0.025); m$feas_high <- q(bF, 0.975)
  m$urg_low  <- q(bU, 0.025); m$urg_high  <- q(bU, 0.975)
  m$class_support <- rowMeans(bC == m$class)
  m$borderline <- ifelse(m$feas_low < 0 & m$feas_high > 0 & m$urg_low < 0 & m$urg_high > 0, "Both axes",
                  ifelse(m$feas_low < 0 & m$feas_high > 0, "Feasibility",
                  ifelse(m$urg_low < 0 & m$urg_high > 0, "Urgency", "No")))
  m$below_min <- m$n_transects < s$min_transects
  m$alerts <- site_alerts(m)
  m$depth_m <- if ("depth" %in% names(dfp)) vapply(m$site, function(si) {
      x <- dfp$depth[dfp$site == si & !is.na(dfp$depth) & dfp$depth > 0]
      if (!length(x)) NA_character_ else if (min(x) == max(x)) format(min(x)) else paste(min(x), max(x), sep = "-") }, character(1)) else NA_character_
  m$pathway <- CLASSES$pathway[match(m$class, CLASSES$class)]
  m$recommendation <- CLASSES$recommendation[match(m$class, CLASSES$class)]
  m$trend_status <- if (!is.null(tr)) tr$info$status[match(m$site, tr$info$site)] else "not requested"
  m$trend_contribution <- if (trend_used) sc$Z[, "trend"] * sum(weight_vector(w_eff, "Urgency")["trend"]) else 0

  # trend table: rates of change in natural units, with bootstrap intervals
  if (!is.null(tr)) {
    tt <- tr$info[, c("site", "n_periods", "span_days", "status")]
    if (trend_used) {
      sl <- tr$slopes
      unit_mult <- ifelse(colnames(sl) %in% c(COVER_VARS, PREVALENCE_VARS), 100, 1)
      sl <- sweep(sl, 2, unit_mult, "*")
      colnames(sl) <- paste0(colnames(sl), ifelse(unit_mult == 100, "_pp_per_yr", "_per_yr"))
      k <- match(tt$site, rownames(sl))
      tt <- cbind(tt, sl[k, , drop = FALSE])
      if ("coral" %in% tr$vars) {
        tt$coral_pp_per_yr_low  <- 100 * apply(bS, 1, stats::quantile, 0.025, na.rm = TRUE, names = FALSE)[k]
        tt$coral_pp_per_yr_high <- 100 * apply(bS, 1, stats::quantile, 0.975, na.rm = TRUE, names = FALSE)[k]
      }
      tt$trend_score <- tr$score[tt$site]
      tt$trend_score_low  <- apply(bT, 1, stats::quantile, 0.025, na.rm = TRUE, names = FALSE)[k]
      tt$trend_score_high <- apply(bT, 1, stats::quantile, 0.975, na.rm = TRUE, names = FALSE)[k]
      tt$direction <- ifelse(is.na(tt$trend_score), "",
                      ifelse(tt$trend_score_low > 0, "deteriorating relative to other sites",
                      ifelse(tt$trend_score_high < 0, "improving relative to other sites", "no clear difference")))
      tt$urgency_contribution <- m$trend_contribution[match(tt$site, m$site)]
      tt$in_snapshot <- tt$site %in% sites
    }
    rownames(tt) <- NULL
    tr$table <- tt
    if (trend_used) {
      mult <- ifelse(colnames(tr$slopes) %in% c(COVER_VARS, PREVALENCE_VARS), 100, 1)
      tr$reef_mean <- data.frame(indicator = colnames(tr$slopes),
                                 mean_change_per_yr = colMeans(tr$slopes, na.rm = TRUE) * mult,
                                 units = ifelse(mult == 100, "percentage points", "H' units"), row.names = NULL)
    }
  }

  # donor shortlist
  rank_b <- apply(bD, 2, function(x) rank(-x, ties.method = "min"))
  K <- min(s$top_n, length(sites))
  m$donor_rank <- rank(-m$donor_score, ties.method = "min")
  m$donor_topK_support <- rowMeans(rank_b <= K)
  m$donor_rank_low  <- apply(rank_b, 1, stats::quantile, 0.025, names = FALSE, type = 1)
  m$donor_rank_high <- apply(rank_b, 1, stats::quantile, 0.975, names = FALSE, type = 1)
  donors <- m[order(m$donor_rank), c("site", "depth_m", "donor_rank", "donor_rank_low", "donor_rank_high", "donor_topK_support",
                                     "donor_score", "n_transects", "mean_coral", "mean_shannon", "cv_coral",
                                     "mean_bleached", "mean_disease", "class", "alerts", "below_min")]
  rownames(donors) <- NULL

  # relationship between indices and quadrant counts
  rho <- if (length(sites) >= 3) suppressWarnings(stats::cor(m$feasibility, m$urgency, method = "spearman")) else NA_real_
  counts <- table(factor(m$class, levels = CLASSES$class))

  # sensitivity to weights
  tick(0.85, "Weight sensitivity")
  sens <- weight_sensitivity(sc$Z, m$class, w_eff, s, trend_used, sites = m$site)

  # trajectories (descriptive)
  traj <- tryCatch(site_trajectories(dfp, w_eff, s), error = function(e) NULL)

  # diagnostics per site
  diag <- do.call(rbind, lapply(sites, function(si) {
    ds <- d[d$site == si, , drop = FALSE]; da <- dfp[dfp$site == si, , drop = FALSE]
    used <- function(ind) any(w_eff$weight[w_eff$indicator == ind] > 0)
    miss <- c(if (used("mean_shannon") && all(is.na(ds$shannon))) "diversity",
              if (used("mean_bleached") && "bleached" %in% names(ds) && all(is.na(ds$bleached))) "bleaching",
              if (used("mean_disease") && "disease" %in% names(ds) && all(is.na(ds$disease))) "disease",
              if (used("mean_predation") && "predation" %in% names(ds) && all(is.na(ds$predation))) "predation",
              if (used("cv_coral") && is.na(m$cv_coral[m$site == si])) "patchiness (CV)",
              if (is.na(m$pc1[m$site == si]) && sc$pca$ok) "algal dominance (PC1)")
    data.frame(site = si,
               transects_in_snapshot = sum(!is.na(ds$coral)),
               surveys_in_snapshot = length(unique(ds$date[!is.na(ds$date)])),
               snapshot_first = if (any(!is.na(ds$date))) format(min(ds$date, na.rm = TRUE)) else NA,
               snapshot_last  = if (any(!is.na(ds$date))) format(max(ds$date, na.rm = TRUE)) else NA,
               transects_in_period = nrow(da),
               surveys_in_period = length(unique(da$date[!is.na(da$date)])),
               depth_range = if ("depth" %in% names(ds) && any(!is.na(ds$depth))) paste(range(ds$depth, na.rm = TRUE), collapse = "-") else NA,
               missing_indicators = if (length(miss)) paste(miss, collapse = ", ") else "",
               note = if (sum(!is.na(ds$coral)) < s$min_transects) sprintf("Fewer than %d transects: interpret with caution", s$min_transects) else "",
               stringsAsFactors = FALSE)
  }))
  if (any(diag$missing_indicators != ""))
    notes <- c(notes, sprintf("Missing indicators were set to the mean of the sites (neutral) for: %s.",
                              paste(sprintf("%s (%s)", diag$site[diag$missing_indicators != ""],
                                            diag$missing_indicators[diag$missing_indicators != ""]), collapse = "; ")))
  if (!all(pc1_ok) && sc$pca$ok) notes <- c(notes, sprintf("PC1 was rejected in %.0f%% of bootstrap replicates (contributing nothing in those).", 100 * mean(!pc1_ok)))

  tick(1, "Done")
  list(sites = m, donors = donors, pca = sc$pca, diagnostics = diag, notes = unique(notes[notes != ""]),
       rho = rho, quadrant_counts = counts, sensitivity = sens, trends = tr, trajectories = traj,
       snapshot = list(description = snap$description, mode = snap$mode, not_scored = snap$not_scored),
       boot = list(feasibility = bF, urgency = bU, class = bC, donor = bD),
       weights_used = w_eff, weights_requested = w,
       settings = c(s, list(trend_used = trend_used, version = APP_VERSION,
                            run_at = format(Sys.time(), "%Y-%m-%d %H:%M"),
                            r_version = R.version.string)))
}
