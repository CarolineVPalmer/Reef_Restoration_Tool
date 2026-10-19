# =============================================================================
# trends.R -- Reef Restoration Prioritisation Tool
#
# Trends: per-site rates of change over the whole data period.
# Moved verbatim from analysis.R (v2.3.0); roxygen headers added.
# =============================================================================

# -----------------------------------------------------------------------------
# 6. Trends: per-site rates of change over the whole data period
#
# Each indicator is averaged per survey period (e.g. Mar-May, Jun-Aug ...), so
# every period counts once however many surveys it had, and a straight line is
# fitted to the period means against time. The slope is the change per year.
# Only sites with >= trend_min_periods periods spanning >= trend_min_days are
# eligible. Slopes are standardised across eligible sites and combined with the
# Trend weights (sign fixed: + = getting worse) into a trend score, which enters
# Urgency with weight u_trend. Non-eligible sites get a neutral trend (0).
# -----------------------------------------------------------------------------
#' Decide which sites are eligible for trends and prepare survey-period times.
#'
#' @param df Data for the whole data period.
#' @param w Weights table.
#' @param s Settings list.
#' @return A list with the trend variables, per-site eligibility, the period of each row and its midpoint in years.
#'   Sites need at least `trend_min_periods` periods spanning `trend_min_days`, and at least three sites must be eligible.
trend_setup <- function(df, w, s) {
  vars <- intersect(unique(w$indicator[w$group == "Trend" & w$weight > 0]), names(df))
  sites <- sort(unique(df$site))
  per <- period_start(df$date, s$period_months, s$period_start_month)
  info <- data.frame(site = sites, n_periods = 0L, span_days = 0, eligible = FALSE, stringsAsFactors = FALSE)
  for (i in seq_along(sites)) {
    k <- df$site == sites[i] & !is.na(df$date)
    if (!any(k)) next
    info$n_periods[i] <- length(unique(per[k]))
    info$span_days[i] <- as.numeric(diff(range(df$date[k])))
  }
  info$eligible <- info$n_periods >= s$trend_min_periods & info$span_days >= s$trend_min_days
  info$status <- ifelse(info$eligible, "used",
                        sprintf("not eligible (%d period(s) over %d days)", info$n_periods, round(info$span_days)))
  if (sum(info$eligible) < 3 && any(info$eligible)) {
    info$status[info$eligible] <- "eligible, but fewer than 3 eligible sites: not used"
    info$eligible <- FALSE
  }
  # time of each period = its midpoint, in years
  tmid <- (as.numeric(per) + 30.44 * s$period_months / 2) / 365.25
  list(vars = vars, info = info, period = per, tmid = tmid, n_eligible = sum(info$eligible),
       tw = w[w$group == "Trend" & w$indicator %in% vars, ])
}

#' Ordinary least-squares slope (per year) of survey-period means for each eligible site and indicator.
#'
#' @param df Data.
#' @param period Period start of each row.
#' @param tmid Period midpoint (years) of each row.
#' @param setup Output of `trend_setup()`.
#' @return Matrix of slopes (rows = eligible sites). Averaging within periods first means each period counts once,
#'   however many surveys it contained; a slope needs at least three period means.
trend_slopes <- function(df, period, tmid, setup) {
  # slopes (per year) of period means, for eligible sites; rows = eligible sites
  el <- setup$info$site[setup$info$eligible]
  vars <- setup$vars
  out <- matrix(NA_real_, length(el), length(vars), dimnames = list(el, vars))
  keep <- df$site %in% el & !is.na(period)
  if (!any(keep) || !length(vars)) return(out)
  g <- paste(df$site[keep], format(period[keep]), sep = "|")
  X <- as.matrix(df[keep, vars, drop = FALSE])
  sums <- rowsum(ifelse(is.na(X), 0, X), g, reorder = TRUE)
  cnts <- rowsum(1 * !is.na(X), g, reorder = TRUE)
  means <- sums / ifelse(cnts > 0, cnts, NA)
  gt <- tapply(tmid[keep], g, `[`, 1)[rownames(means)]
  gs <- sub("\\|.*$", "", rownames(means))
  for (si in el) {
    r <- gs == si
    for (v in vars) {
      y <- means[r, v]; t <- gt[r]; ok <- !is.na(y)
      if (sum(ok) >= 3 && length(unique(t[ok])) >= 3) {
        tt <- t[ok] - mean(t[ok])
        out[si, v] <- sum(tt * (y[ok] - mean(y[ok]))) / sum(tt^2)
      }
    }
  }
  out
}

#' Combine standardised slopes into one trend score (+ = deteriorating relative to the other sites; Eq. 5).
#'
#' @param slopes Matrix from `trend_slopes()`.
#' @param setup Output of `trend_setup()`.
#' @return Named numeric vector, one value per eligible site.
trend_score <- function(slopes, setup) {
  # weighted sum of standardised slopes (+ = deteriorating), for eligible sites
  if (!nrow(slopes)) return(setNames(numeric(0), character(0)))
  tw <- setup$tw
  total <- rep(0, nrow(slopes))
  for (k in seq_len(nrow(tw))) total <- total + tw$sign[k] * tw$weight[k] * zscore(slopes[, tw$indicator[k]])
  setNames(total, rownames(slopes))
}
