# =============================================================================
# data_input.R -- Reef Restoration Prioritisation Tool
#
# Reading and validating the input file.
# Moved verbatim from analysis.R (v2.3.0); roxygen headers added.
# =============================================================================

# -----------------------------------------------------------------------------
# 3. Reading and validating the input file
# -----------------------------------------------------------------------------
#' Read a comma- or semicolon-separated transect file.
#'
#' @param path Path to the file.
#' @return A data frame. Semicolon files are read with decimal commas, as saved by Excel in Spanish and many other locales.
read_transect_file <- function(path) {
  # Reads comma- or semicolon-separated files (the latter with decimal commas,
  # as saved by Excel in Spanish, Portuguese and many European locales).
  first <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
  semi <- length(first) && lengths(regmatches(first, gregexpr(";", first))) >
                           lengths(regmatches(first, gregexpr(",", first)))
  df <- utils::read.csv(path, sep = if (semi) ";" else ",", dec = if (semi) "," else ".",
                        stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM",
                        na.strings = c("NA", ""), strip.white = TRUE)
  df
}

#' Match column names, remove unusable rows, convert units and calculate transect diversity.
#'
#' @param df_raw Data frame from `read_transect_file()`.
#' @param day_first Logical, passed to `parse_dates()`.
#' @return A list with the cleaned data (`df`), messages reporting every correction (`messages`), whether dates are
#'   available (`has_dates`), the taxon columns used (`tot_cols`) and the variables found (`variables`).
prepare_data <- function(df_raw, day_first = TRUE) {
  msgs <- character(0)
  nm <- clean_text(names(df_raw))
  low <- tolower(nm)
  for (a in names(COLUMN_ALIASES)) {
    hit <- which(low == a)
    if (length(hit) && !(COLUMN_ALIASES[[a]] %in% low)) {
      low[hit[1]] <- COLUMN_ALIASES[[a]]
      msgs <- c(msgs, sprintf("Column '%s' was used as '%s'.", nm[hit[1]], COLUMN_ALIASES[[a]]))
    }
  }
  known <- c("site", "date", "year", "month", "method", COVER_VARS, PREVALENCE_VARS, OTHER_NUM_VARS, "caulerpa")
  for (k in known) { hit <- which(low == k); if (length(hit)) nm[hit[1]] <- k }
  names(df_raw) <- nm

  if (!"site" %in% nm) stop("The file must contain a column named 'site'.")
  if (!"coral" %in% nm) stop("The file must contain a 'coral' column (live coral cover).")
  tot_cols <- grep("^tot_", nm, value = TRUE, ignore.case = TRUE)

  # rows without a usable site name (blank, 'NA', or '0' from spreadsheet padding)
  site <- clean_text(df_raw$site)
  bad_site <- is.na(site) | site == "" | toupper(site) %in% c("NA", "0", "#N/A", "#REF!")
  if (any(bad_site)) msgs <- c(msgs, sprintf("%d row(s) without a valid site name (blank, 'NA' or '0') were ignored.", sum(bad_site)))
  df <- df_raw[!bad_site, , drop = FALSE]
  site <- site[!bad_site]
  if (nrow(df) == 0) stop("No rows with a valid site name were found.")

  # merge site names that differ only in capitalisation or spacing
  key <- tolower(site)
  canon <- tapply(site, key, function(x) names(sort(table(x), decreasing = TRUE))[1])
  nvar <- tapply(site, key, function(x) length(unique(x)))
  if (any(nvar > 1)) msgs <- c(msgs, sprintf("Site names differing only in capitalisation were merged: %s.",
                                             paste(canon[nvar > 1], collapse = ", ")))
  out <- data.frame(site = unname(canon[key]), stringsAsFactors = FALSE)
  out$.row <- which(!bad_site)   # row in the original file (for tracing and comparisons)
  # optional survey method (e.g. "transect", "photogrammetry"): used only for warnings
  if ("method" %in% names(df)) {
    mt <- tolower(clean_text(df$method))
    mt[is.na(mt) | mt == "" | toupper(mt) %in% c("NA", "N/A")] <- NA
    out$method <- mt
  }

  # numeric variables
  num_vars <- intersect(c(COVER_VARS, PREVALENCE_VARS, OTHER_NUM_VARS, "caulerpa"), names(df))
  for (v in num_vars) {
    raw <- df[[v]]
    x <- to_number(raw)
    lost <- sum(is.na(x) & !is.na(raw) & clean_text(raw) != "" & !toupper(clean_text(raw)) %in% c("NA", "N/A"))
    if (lost > 0) msgs <- c(msgs, sprintf("%d value(s) in '%s' were not numeric and were treated as missing.", lost, v))
    out[[v]] <- x
  }
  for (v in intersect(c(COVER_VARS, PREVALENCE_VARS, "caulerpa"), names(out))) {
    mx <- suppressWarnings(max(out[[v]], na.rm = TRUE))
    if (is.finite(mx) && mx > 1.5) {
      out[[v]] <- out[[v]] / 100
      msgs <- c(msgs, sprintf("'%s' looked like a percentage and was converted to a proportion.", v))
    }
    if (any(out[[v]] < 0 | out[[v]] > 1, na.rm = TRUE))
      msgs <- c(msgs, sprintf("'%s' has values outside 0-1 after conversion; check the file.", v))
  }
  if ("caulerpa" %in% names(out)) {
    if (!"macroalgae" %in% names(out)) out$macroalgae <- NA_real_
    both_na <- is.na(out$macroalgae) & is.na(out$caulerpa)
    out$macroalgae <- ifelse(both_na, NA, rowSums(cbind(out$macroalgae, out$caulerpa), na.rm = TRUE))
    out$caulerpa <- NULL
    msgs <- c(msgs, "Caulerpa cover was added to macroalgae.")
  }

  # dates: 'date' column first, then year + month (first day of the month)
  date <- as.Date(rep(NA_character_, nrow(df)))
  if ("date" %in% names(df)) {
    date <- parse_dates(df$date, day_first)
    raw <- clean_text(df$date)
    bad <- is.na(date) & !is.na(raw) & raw != ""
    if (any(bad)) msgs <- c(msgs, sprintf("%d date(s) could not be read or were implausible (e.g. '%s').",
                                          sum(bad), raw[bad][1]))
  }
  if (all(c("year", "month") %in% names(df))) {
    yr <- suppressWarnings(as.integer(to_number(df$year)))
    mo <- month_number(df$month)
    ok <- !is.na(yr) & !is.na(mo) & yr >= 1950 & yr <= 2100
    ym <- as.Date(ifelse(ok, sprintf("%04d-%02d-01", yr, mo), NA))
    fill <- is.na(date) & !is.na(ym)
    if (any(fill)) msgs <- c(msgs, sprintf("%d date(s) were taken from the year and month columns.", sum(fill)))
    date[fill] <- ym[fill]
  }
  out$date <- date
  has_dates <- any(!is.na(out$date))

  # coral diversity (Shannon H') from colony counts in Tot_* columns
  if (length(tot_cols)) {
    tax <- sapply(tot_cols, function(v) to_number(df[[v]]))
    if (is.null(dim(tax))) tax <- matrix(tax, nrow = nrow(df))
    no_tax <- rowSums(!is.na(tax)) == 0
    tax[is.na(tax)] <- 0
    out$shannon <- shannon(tax)
    out$shannon[no_tax] <- NA
    n_miss <- sum(is.na(out$shannon))
    if (n_miss > 0) msgs <- c(msgs, sprintf("%d transect(s) had no coral taxon counts; diversity is missing for these (not set to zero).", n_miss))
  } else {
    out$shannon <- NA_real_
    msgs <- c(msgs, "No coral taxon columns (Tot_*) found: coral diversity is not used.")
  }

  n_coral_na <- sum(is.na(out$coral))
  if (n_coral_na) msgs <- c(msgs, sprintf("%d row(s) had no coral cover value and are not counted as transects.", n_coral_na))

  list(df = out, messages = msgs, has_dates = has_dates, tot_cols = tot_cols,
       variables = intersect(c(COVER_VARS, PREVALENCE_VARS, OTHER_NUM_VARS, "shannon"), names(out)))
}
