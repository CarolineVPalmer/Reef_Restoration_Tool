# =============================================================================
# utils.R -- Reef Restoration Prioritisation Tool
#
# Small helpers: text cleaning, number and date parsing, standardisation, diversity, survey periods.
# Moved verbatim from analysis.R (v2.3.0); roxygen headers added.
# =============================================================================

# -----------------------------------------------------------------------------
# 2. Small helpers
# -----------------------------------------------------------------------------
#' Normalise text: remove non-breaking spaces and byte-order marks, trim and collapse white space.
#'
#' @param x Vector coerced to character.
#' @return Character vector.
clean_text <- function(x) {
  x <- as.character(x)
  x <- gsub("\u00a0|\ufeff", " ", x)          # non-breaking spaces, byte-order marks
  gsub("\\s+", " ", trimws(x))
}

#' Convert text to numbers, accepting decimal commas and common missing-value codes.
#'
#' @param x Vector (numeric vectors are returned unchanged).
#' @return Numeric vector; unreadable values become `NA`.
to_number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- gsub("\\s", "", clean_text(x))
  x[x %in% c("", "NA", "na", "N/A", "-", ".")] <- NA
  # decimal commas (e.g. "0,45") when there is no dot
  x <- ifelse(grepl("^-?[0-9]*,[0-9]+$", x), sub(",", ".", x), x)
  suppressWarnings(as.numeric(x))
}

#' Standardise a variable across units (sites or site-periods).
#'
#' @param x Numeric vector.
#' @return Z-scores. Missing values are set to 0 (the mean of the units, i.e. neutral); a variable with fewer than two
#'   values or no variation returns all zeros, so it drops out of the indices. This is why every score is
#'   relative to the units analysed.
zscore <- function(x) {
  # Standardise across units. Missing values become 0 (= the mean of the units
  # analysed, i.e. neutral). No usable variation -> all 0 (indicator drops out).
  ok <- !is.na(x)
  if (sum(ok) < 2) return(rep(0, length(x)))
  s <- sd(x[ok])
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  z <- (x - mean(x[ok])) / s
  z[!ok] <- 0
  z
}

#' Mean that returns `NA` (not `NaN`) when every value is missing.
#'
#' @param v Numeric vector.
#' @return A single number.
mean_or_na <- function(v) if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)

#' Shannon diversity H' (natural log) for each row of a matrix of colony counts per taxon.
#'
#' @param counts Numeric matrix, one row per transect and one column per taxon.
#' @return Numeric vector; `NA` where no colonies were counted, so that missing diversity is not read as zero.
shannon <- function(counts) {
  # Shannon H' per row of a count matrix (natural log). NA when no colonies were counted.
  tot <- rowSums(counts)
  p <- counts / ifelse(tot > 0, tot, NA)
  h <- -rowSums(ifelse(p > 0, p * log(p), 0))
  h[!(tot > 0)] <- NA
  h
}

#' Parse dates value by value, allowing mixed formats and Spanish month names.
#'
#' @param x Character vector of dates.
#' @param day_first Logical; read 05/01/2025 as 5 January (TRUE) or 1 May (FALSE).
#' @return A `Date` vector; values outside 1950-2100 are rejected as implausible.
parse_dates <- function(x, day_first = TRUE) {
  # Each value parsed on its own (mixed formats allowed); implausible years rejected.
  x <- clean_text(x)
  for (i in seq_along(MONTHS_ES)) x <- gsub(MONTHS_ES[i], month.name[i], x, ignore.case = TRUE)
  # %B and %b read month names in the computer's language. Spanish names have
  # just been translated to English, so parse in the "C" (English) locale;
  # otherwise these dates fail on computers set to Spanish (fixed in v2.4.0).
  old_locale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_locale), add = TRUE)
  invisible(Sys.setlocale("LC_TIME", "C"))
  slash <- if (day_first) c("%d/%m/%Y", "%m/%d/%Y", "%d/%m/%y") else c("%m/%d/%Y", "%d/%m/%Y", "%m/%d/%y")
  fmts <- c("%Y-%m-%d", "%Y/%m/%d", slash, "%d-%m-%Y", "%d.%m.%Y",
            "%d %B %Y", "%d %b %Y", "%d-%b-%Y", "%d-%b-%y", "%Y-%b-%d", "%Y-%B-%d")
  out <- as.Date(rep(NA_character_, length(x)))
  for (f in fmts) {
    todo <- is.na(out) & !is.na(x) & x != ""
    if (!any(todo)) break
    d <- suppressWarnings(as.Date(x[todo], format = f))
    yr <- as.integer(format(d, "%Y"))
    ok <- !is.na(d) & yr >= 1950 & yr <= 2100
    out[which(todo)[ok]] <- d[ok]
  }
  out
}

#' Convert month names or numbers (English or Spanish, full or abbreviated) to 1-12.
#'
#' @param m Vector of month values.
#' @return Integer vector; `NA` where the month cannot be read.
month_number <- function(m) {
  m <- tolower(clean_text(m))
  n <- suppressWarnings(as.integer(m))
  by_name <- match(m, tolower(month.name))
  by_es   <- match(m, MONTHS_ES)
  by_abb  <- match(substr(m, 1, 3), tolower(month.abb))
  by_es3  <- match(substr(m, 1, 3), substr(MONTHS_ES, 1, 3))
  out <- ifelse(!is.na(n), n, ifelse(!is.na(by_name), by_name, ifelse(!is.na(by_es), by_es,
                ifelse(!is.na(by_abb), by_abb, by_es3))))
  out[!is.na(out) & (out < 1 | out > 12)] <- NA
  out
}

#' First day of the survey period that contains each date.
#'
#' @param date `Date` vector.
#' @param months Period length in months.
#' @param start_month Month in which the first period of the year starts.
#' @return `Date` vector. With `months = 3` and `start_month = 3` the periods are Mar-May, Jun-Aug, Sep-Nov and Dec-Feb,
#'   which follow the seasons used in the Isla del Caño monitoring.
period_start <- function(date, months = 3, start_month = 3) {
  # First day of the survey period containing each date. With months = 3 and
  # start_month = 3 the periods are Mar-May, Jun-Aug, Sep-Nov and Dec-Feb.
  tot <- as.integer(format(date, "%Y")) * 12 + as.integer(format(date, "%m")) - 1
  off <- start_month - 1
  st <- floor((tot - off) / months) * months + off
  as.Date(sprintf("%04d-%02d-01", st %/% 12, st %% 12 + 1))
}

#' Human-readable label for survey periods, e.g. "Dec 2025 to Feb 2026".
#'
#' @param start `Date` vector of period starts.
#' @param months Period length in months.
#' @return Character vector.
period_label <- function(start, months = 3) {
  lab <- character(length(start))
  for (i in seq_along(start)) {
    if (is.na(start[i])) { lab[i] <- NA; next }
    e <- seq(start[i], by = "month", length.out = months)[months]
    lab[i] <- if (months == 1) format(start[i], "%b %Y") else
      paste0(format(start[i], "%b %Y"), " to ", format(e, "%b %Y"))
  }
  lab
}

#' Read a single value as a `Date`, returning `NA` for empty input.
#'
#' @param x Value from a settings list or input control.
#' @return A single `Date`.
as_date_or_na <- function(x) {
  if (length(x) == 0 || is.null(x)) return(as.Date(NA))
  if (inherits(x, "Date")) return(x[1])
  x <- as.character(x[1])
  if (is.na(x) || x %in% c("", "NA")) return(as.Date(NA))
  suppressWarnings(as.Date(x))
}
