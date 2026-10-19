# =============================================================================
# event.R -- Reef Restoration Prioritisation Tool
#
# Optional bleaching-event mode (v2.5.0): compares surveys before, during and
# after an event, site by site, with bootstrap intervals. It is descriptive: it
# reports observed change and does not forecast coral loss, because no
# relationship between bleaching and later mortality has been validated for the
# reefs the tool is meant for. It runs on whatever phases exist: "before" plus
# "during" (bleaching and change so far), "before" plus "after" (losses), or all
# three. If there are no surveys before the event, "during" is used as the
# reference (v2.5.1): many teams start surveying only once bleaching is under
# way. Change is then measured from the first survey of the event, so losses
# before that survey are missed and the change shown is an underestimate. It is
# independent of the standard analysis, which is unchanged and does not need it.
# =============================================================================

EVENT_PHASES <- c("before", "during", "after")
EVENT_VARS <- c("coral", "turf", "cca", "bleached", "disease")

#' Event phases from a settings list.
#'
#' @param s Settings list (see `DEFAULT_SETTINGS`).
#' @return Data frame with columns phase, from and to; phases not in use have
#'   `NA` dates. "During" and "after" are used only if switched on.
event_phases_from_settings <- function(s) {
  s <- modifyList(DEFAULT_SETTINGS, s)
  use <- c(TRUE, isTRUE(s$event_use_during), isTRUE(s$event_use_after))
  from <- c(as_date_or_na(s$event_before_from), as_date_or_na(s$event_during_from),
            as_date_or_na(s$event_after_from))
  to <- c(as_date_or_na(s$event_before_to), as_date_or_na(s$event_during_to),
          as_date_or_na(s$event_after_to))
  from[!use] <- NA
  to[!use] <- NA
  data.frame(phase = EVENT_PHASES, from = from, to = to, stringsAsFactors = FALSE)
}

#' Assign each transect to an event phase by its date.
#'
#' @param df Cleaned transect data with a `date` column.
#' @param phases Data frame from `event_phases_from_settings()`.
#' @return Character vector of phases (`NA` outside every window). Where windows
#'   overlap, the earlier phase wins and a note is attached as `attr(, "note")`.
assign_event_phase <- function(df, phases) {
  ph <- rep(NA_character_, nrow(df))
  overlap <- FALSE
  for (k in seq_len(nrow(phases))) {
    if (is.na(phases$from[k]) || is.na(phases$to[k])) next
    inside <- !is.na(df$date) & df$date >= phases$from[k] & df$date <= phases$to[k]
    if (any(inside & !is.na(ph))) overlap <- TRUE
    ph[inside & is.na(ph)] <- phases$phase[k]
  }
  if (overlap) {
    attr(ph, "note") <- "Some survey dates fall in more than one phase window; they were counted in the earlier phase."
  }
  ph
}

#' Site means of the event indicators in each phase.
#'
#' @param d Transect data with a `phase` column.
#' @param sites Sites to report, in order.
#' @param vars Indicators available in the data.
#' @return Named list (one element per phase present) of data frames with the
#'   number of transects and the mean of each indicator per site.
event_phase_means <- function(d, sites, vars) {
  out <- list()
  for (p in intersect(EVENT_PHASES, unique(d$phase))) {
    x <- d[d$phase == p, , drop = FALSE]
    f <- factor(x$site, levels = sites)
    m <- data.frame(site = sites, n = as.vector(table(f)), stringsAsFactors = FALSE)
    for (v in vars) m[[v]] <- vapply(split(x[[v]], f), mean_or_na, numeric(1))
    out[[p]] <- m
  }
  out
}

#' Change in each indicator from the reference phase to the latest phase.
#'
#' The reference is "before" when it exists, otherwise "during". For each site
#' the comparison phase is the latest later phase in which the site was surveyed
#' ("after" if possible), so the tool works while the event is still under way.
#'
#' @param pm List from `event_phase_means()`.
#' @param vars Indicators available.
#' @param ref Reference phase, "before" or "during".
#' @return Data frame: latest phase per site and change (latest minus reference)
#'   for each indicator, as proportions.
event_changes <- function(pm, vars, ref = "before") {
  before <- pm[[ref]]
  later <- EVENT_PHASES[seq(match(ref, EVENT_PHASES) + 1, length(EVENT_PHASES))]
  latest_phase <- rep(NA_character_, nrow(before))
  for (p in later) {
    if (!is.null(pm[[p]])) latest_phase[pm[[p]]$n > 0] <- p
  }
  out <- data.frame(site = before$site, latest_phase = latest_phase, stringsAsFactors = FALSE)
  for (v in vars) {
    latest <- rep(NA_real_, nrow(before))
    for (p in later) {
      use <- !is.na(latest_phase) & latest_phase == p
      if (any(use)) latest[use] <- pm[[p]][[v]][use]
    }
    out[[paste0(v, "_latest")]] <- latest
    out[[paste0(v, "_change")]] <- latest - before[[v]]
  }
  out
}

#' Compare surveys before, during and after a bleaching event.
#'
#' @param df Cleaned transect data (`prepare_data()$df`); all dates are used,
#'   whatever the data period of the standard analysis.
#' @param phases Data frame from `event_phases_from_settings()`.
#' @param B Bootstrap replicates for the intervals.
#' @param seed Random seed, so the intervals can be reproduced.
#' @param severe Bleaching prevalence above which the severe-bleaching alert is
#'   shown (the same 50% threshold as the standard analysis).
#' @return A list with the site table, the reef-wide summary, notes and the
#'   phases used. Cover and prevalence are reported in percent and percentage
#'   points.
event_comparison <- function(df, phases, B = 200, seed = 123, severe = 0.5) {
  used <- phases[!is.na(phases$from) & !is.na(phases$to), , drop = FALSE]
  ref <- if ("before" %in% used$phase) "before" else "during"
  if (!ref %in% used$phase) {
    stop("Choose the dates of surveys made before the event, or during it if there were none before.")
  }
  if (ref == "before" && nrow(used) < 2) {
    stop("Choose the dates of surveys made during or after the event, as well as before it.")
  }
  if (ref == "during" && !"after" %in% used$phase) {
    stop("Without surveys before the event, choose the dates of surveys made after it to compare with those during it.")
  }
  if (any(used$to < used$from)) stop("In each phase, the end date must be after the start date.")

  df <- df[!is.na(df$date) & !is.na(df$coral), , drop = FALSE]
  ph <- assign_event_phase(df, used)
  notes <- if (is.null(attr(ph, "note"))) character(0) else attr(ph, "note")
  d <- df[!is.na(ph), , drop = FALSE]
  d$phase <- ph[!is.na(ph)]
  if (!any(d$phase == ref)) stop(sprintf("No surveys fall within the '%s' dates.", ref))
  if (!any(d$phase != ref)) stop("No surveys fall within the dates of the later phase(s).")
  if (ref == "during") {
    notes <- c(notes, paste("No surveys before the event were given, so change is measured from the surveys during it.",
                            "Coral lost before those surveys is missed, so losses shown are underestimates."))
  }

  sites <- sort(unique(d$site))
  vars <- intersect(EVENT_VARS, names(d))
  pm <- event_phase_means(d, sites, vars)
  ch <- event_changes(pm, vars, ref)

  # Bootstrap: transects resampled within each survey (site x date x phase), as
  # in the standard analysis, so the intervals reflect variation among transects.
  set.seed(seed)
  strata <- split(seq_len(nrow(d)), paste(d$site, d$date, d$phase))
  boot_coral <- matrix(NA_real_, length(sites), B)
  boot_reef <- rep(NA_real_, B)
  for (b in seq_len(B)) {
    ii <- unlist(lapply(strata, function(i) i[sample.int(length(i), length(i), replace = TRUE)]),
                 use.names = FALSE)
    cb <- event_changes(event_phase_means(d[ii, , drop = FALSE], sites, vars), vars, ref)$coral_change
    boot_coral[, b] <- cb
    boot_reef[b] <- mean(cb, na.rm = TRUE)
  }
  q <- function(x, p) if (all(is.na(x))) NA_real_ else stats::quantile(x, p, na.rm = TRUE, names = FALSE)

  before <- pm[[ref]]
  pct <- function(x) 100 * x
  tab <- data.frame(
    site = sites,
    reference_phase = ref,
    transects_reference = before$n,
    compared_with = ch$latest_phase,
    coral_reference_pct = pct(before$coral),
    coral_latest_pct = pct(ch$coral_latest),
    coral_change_pp = pct(ch$coral_change),
    coral_change_low = pct(apply(boot_coral, 1, q, 0.025)),
    coral_change_high = pct(apply(boot_coral, 1, q, 0.975)),
    stringsAsFactors = FALSE
  )
  # relative loss is shown only where there was coral to lose
  tab$coral_relative_change_pct <- ifelse(!is.na(before$coral) & before$coral > 0,
                                          100 * ch$coral_change / before$coral, NA_real_)
  for (v in intersect(c("turf", "cca"), vars)) tab[[paste0(v, "_change_pp")]] <- pct(ch[[paste0(v, "_change")]])
  if ("bleached" %in% vars) {
    during <- pm[["during"]]
    tab$bleached_during_pct <- if (is.null(during)) NA_real_ else pct(during$bleached)
    tab$severe_bleaching <- !is.na(tab$bleached_during_pct) & tab$bleached_during_pct > 100 * severe
  }
  tab$change <- ifelse(is.na(tab$coral_change_low), "",
                ifelse(tab$coral_change_high < 0, "clear loss",
                ifelse(tab$coral_change_low > 0, "clear gain", "no clear change")))
  tab$note <- ifelse(tab$transects_reference == 0, sprintf("not surveyed in the '%s' phase", ref),
              ifelse(is.na(tab$compared_with), "not surveyed in a later phase", ""))
  missing_ref <- tab$site[tab$transects_reference == 0]
  if (length(missing_ref)) {
    notes <- c(notes, sprintf("No surveys in the '%s' phase at: %s. Change cannot be calculated for these sites.",
                              ref, paste(missing_ref, collapse = ", ")))
  }

  ok <- !is.na(ch$coral_change)
  reef <- data.frame(
    sites_compared = sum(ok),
    reference_phase = ref,
    coral_reference_pct = pct(mean(before$coral[ok])),
    coral_latest_pct = pct(mean(ch$coral_latest[ok])),
    coral_change_pp = pct(mean(ch$coral_change[ok])),
    coral_change_low = pct(q(boot_reef, 0.025)),
    coral_change_high = pct(q(boot_reef, 0.975)),
    sites_with_clear_loss = sum(tab$change == "clear loss"),
    sites_with_severe_bleaching = if ("severe_bleaching" %in% names(tab)) sum(tab$severe_bleaching) else NA_integer_
  )
  notes <- c(notes, "Changes are observed differences between survey phases; they are not forecasts.")
  list(sites = tab, reef = reef, notes = notes, phases = used, reference = ref, B = B, seed = seed)
}

#' Plot the change in coral cover at each site, with 95% bootstrap intervals.
#'
#' @param ev Result of `event_comparison()`.
#' @param base_size Base font size.
#' @return A ggplot object, or `NULL` if no site could be compared.
plot_event_change <- function(ev, base_size = 13) {
  t <- ev$sites[!is.na(ev$sites$coral_change_pp), , drop = FALSE]
  if (!nrow(t)) return(NULL)
  t$site <- factor(t$site, levels = t$site[order(t$coral_change_pp)])
  t$change <- factor(ifelse(t$change == "", "no clear change", t$change),
                     levels = c("clear loss", "no clear change", "clear gain"))
  ggplot2::ggplot(t, ggplot2::aes(coral_change_pp, site, colour = change)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = coral_change_low, xmax = coral_change_high),
                           orientation = "y", width = 0, linewidth = 0.8) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_colour_manual(values = c("clear loss" = "#D55E00", "no clear change" = "#999999",
                                            "clear gain" = "#0072B2"), name = NULL, drop = FALSE) +
    ggplot2::labs(x = sprintf("Change in live coral cover since the '%s' surveys (percentage points)", ev$reference), y = NULL,
                  caption = "Bars: 95% bootstrap intervals. Observed change, not a forecast.") +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(legend.position = "bottom")
}
