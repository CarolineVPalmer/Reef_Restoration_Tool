# =============================================================================
# settings_io.R -- Reef Restoration Prioritisation Tool
#
# Settings files (for reproducibility).
# Moved verbatim from analysis.R (v2.3.0); roxygen headers added.
# =============================================================================

# -----------------------------------------------------------------------------
# 10. Settings files (for reproducibility)
# -----------------------------------------------------------------------------
#' Settings and weights as a two-column table that can be saved and reloaded.
#'
#' @param s Settings list.
#' @param w Weights table.
#' @return Data frame with columns type, id, group and value.
settings_table <- function(s, w) {
  s <- modifyList(DEFAULT_SETTINGS, s)
  opt <- s[names(DEFAULT_SETTINGS)]
  opt <- vapply(opt, function(x) if (inherits(x, "Date")) format(x) else if (length(x) == 0 || is.na(x[1])) "" else as.character(x[1]), character(1))
  rbind(data.frame(type = "option", id = names(opt), group = "", value = unname(opt), stringsAsFactors = FALSE),
        data.frame(type = "weight", id = w$id, group = w$group, value = as.character(w$weight), stringsAsFactors = FALSE))
}

DATE_KEYS <- c("data_from", "data_to", "window_from", "window_to",
               "event_before_from", "event_before_to", "event_during_from", "event_during_to",
               "event_after_from", "event_after_to")

#' Read a settings file written by `settings_table()` (or an older weights-only file).
#'
#' @param path Path to the CSV file.
#' @return A list with `settings` and `weights`; weights are clamped to their allowed range.
read_settings <- function(path) {
  t <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
  if (!all(c("type", "id", "value") %in% names(t))) {
    # older settings files (weights only: id, weight)
    if (all(c("id", "weight") %in% names(t))) t <- data.frame(type = "weight", id = t$id, value = t$weight)
    else stop("Not a valid settings file.")
  }
  s <- DEFAULT_SETTINGS
  o <- t[t$type == "option", ]
  for (i in seq_len(nrow(o))) {
    k <- o$id[i]; v <- o$value[i]
    if (!k %in% names(s)) next
    def <- DEFAULT_SETTINGS[[k]]
    s[[k]] <- if (is.na(v) || v == "") NA else if (k %in% DATE_KEYS) as.Date(v) else
      if (is.logical(def)) as.logical(v) else
      if (is.numeric(def)) as.numeric(v) else v
  }
  w <- DEFAULT_WEIGHTS
  wt <- t[t$type == "weight", ]
  hit <- match(wt$id, w$id)
  ok <- !is.na(hit)
  val <- suppressWarnings(as.numeric(wt$value[ok]))
  w$weight[hit[ok]] <- pmin(pmax(val, w$min[hit[ok]]), w$max[hit[ok]])
  list(settings = s, weights = w)
}
