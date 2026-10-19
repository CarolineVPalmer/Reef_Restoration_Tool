# =============================================================================
# trajectories_alerts.R -- Reef Restoration Prioritisation Tool
#
# Descriptive trajectories and absolute alerts.
# Moved verbatim from analysis.R (v2.3.0); roxygen headers added.
# =============================================================================

# -----------------------------------------------------------------------------
# 7. Trajectories (descriptive): site x period scores on a common scale
# -----------------------------------------------------------------------------
#' Score every site in every survey period on a common scale (descriptive only).
#'
#' @param df Data for the whole data period.
#' @param w Weights table.
#' @param s Settings list.
#' @return Data frame of site-period scores and classes, or `NULL`. Indicators missing from more than 20% of site-periods are
#'   left out (listed in `attr(, "dropped")`) so that apparent change is not driven by gaps in recording.
site_trajectories <- function(df, w, s) {
  d <- df[!is.na(df$date), , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  d$period <- period_start(d$date, s$period_months, s$period_start_month)
  if (length(unique(d$period)) < 2) return(NULL)
  unit <- paste(d$site, format(d$period), sep = " | ")
  m <- unit_metrics(d, unit)
  w0 <- w; w0$weight[w0$indicator == "trend"] <- 0
  # indicators missing from more than 20% of site-periods are left out of the
  # trajectories, so that change is not driven by gaps in recording
  dropped <- character(0)
  for (k in which(w0$group %in% c("Feasibility", "Urgency") & w0$weight > 0)) {
    ind <- w0$indicator[k]
    if (ind %in% names(m) && !ind %in% c("mean_coral", "cv_coral") && mean(is.na(m[[ind]])) > 0.2) {
      w0$weight[k] <- 0; dropped <- c(dropped, w0$label[k])
    }
  }
  sc <- score_units(m, w0, PCA_SETS[[s$pca_set]])
  out <- sc$m
  out$site <- sub(" \\| .*$", "", out$unit)
  out$period_start <- as.Date(sub("^.* \\| ", "", out$unit))
  out$period <- period_label(out$period_start, s$period_months)
  out <- out[order(out$site, out$period_start), c("site", "period_start", "period", "n_transects", "mean_coral",
                                                  "mean_shannon", "mean_bleached", "mean_disease", "pc1",
                                                  "feasibility", "urgency", "class")]
  rownames(out) <- NULL
  attr(out, "dropped") <- unique(dropped)
  out
}

#' Absolute alerts (`FLAG_RULES`) for each site.
#'
#' @param m Site metrics.
#' @return Character vector of alert labels, "" where none apply.
site_alerts <- function(m) {
  # absolute alerts (FLAG_RULES) for each site, as a short text
  hits <- sapply(seq_len(nrow(FLAG_RULES)), function(k) {
    x <- m[[FLAG_RULES$variable[k]]]
    if (is.null(x)) return(rep(FALSE, nrow(m)))
    !is.na(x) & if (FLAG_RULES$above[k]) x > FLAG_RULES$threshold[k] else x < FLAG_RULES$threshold[k]
  })
  if (is.null(dim(hits))) hits <- matrix(hits, nrow = nrow(m))
  apply(hits, 1, function(h) paste(FLAG_RULES$short[h], collapse = "; "))
}

#' One line per alert type, with the number and names of the sites affected.
#'
#' @param r Result of `run_analysis()`.
#' @return Character vector.
alert_summary <- function(r) {
  # one line per alert type: how many sites, which ones
  m <- r$sites
  out <- character(0)
  for (k in seq_len(nrow(FLAG_RULES))) {
    hit <- grepl(FLAG_RULES$short[k], m$alerts, fixed = TRUE)
    if (any(hit)) out <- c(out, sprintf("%d of %d sites: %s (%s)", sum(hit), nrow(m), FLAG_RULES$meaning[k],
                                        paste(m$site[hit], collapse = ", ")))
  }
  out
}
