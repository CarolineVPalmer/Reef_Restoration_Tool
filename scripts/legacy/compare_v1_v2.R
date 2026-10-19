# =============================================================================
# compare_v1_v2.R -- run the original (v1) and revised (v2) tool side by side
#
# HOW TO RUN
#   1. Save this file in the same folder as your transect CSV.
#   2. Open it in RStudio and click "Source" (top right of the script pane).
#   Results print in the Console, the plot appears in the Plots pane, and
#   comparison_v1_v2.csv, comparison_v1_v2_donors.csv, comparison_v1_v2.png,
#   trajectories_v2.csv and trajectories_v2.png are saved in the same folder.
# =============================================================================

# ---- settings you can change -------------------------------------------------
data_file         <- NULL   # e.g. "IslaDelCano_transects.csv"; NULL = the only CSV in the folder
dates_day_first   <- TRUE   # TRUE if dates are written like 25/01/2025
data_from         <- as.Date("2024-04-01")           # surveys before this are excluded (both versions)
snapshot_window   <- as.Date(c("2024-12-01", "2025-02-28"))  # the survey scored by both versions;
                                                     # NULL = v2 uses the most recent survey at each site
period_months      <- 3   # trajectory periods (v2 only)
period_start_month <- 3   # 3 = Mar-May, Jun-Aug, Sep-Nov, Dec-Feb
v2_include_trends <- FALSE  # v2 trend adjustment (only sites with 3+ periods over 365+ days); v1 always used trends when there were 2+ dates
v2_pca_as_v1      <- FALSE  # TRUE = v2 PCA also uses bleaching, disease, predators (as v1)
min_transects     <- 5
# ------------------------------------------------------------------------------

# Use the folder this script is saved in as the working directory (RStudio)
try({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getSourceEditorContext()$path
    if (nzchar(p)) setwd(dirname(p))
  }
}, silent = TRUE)
suppressPackageStartupMessages({
  library(shiny); library(ggplot2); library(dplyr); library(tidyr); library(tibble)
  library(vegan); library(FactoMineR); library(factoextra)
})

# =============================================================================
# VERSION 2 calculations
# =============================================================================
APP_VERSION <- "2.2.0"

# -----------------------------------------------------------------------------
# 1. Default weights (single source of truth)
#    'sign' is fixed (ecological direction); users change only 'weight' (>= 0)
# -----------------------------------------------------------------------------
DEFAULT_WEIGHTS <- tibble::tribble(
  ~id,          ~group,        ~indicator,     ~sign, ~weight, ~min, ~max, ~label,
  "f_coral",    "Feasibility", "mean_coral",     1,    3.0,   0,    6,   "Live coral cover",
  "f_shannon",  "Feasibility", "mean_shannon",   1,    2.0,   0,    4,   "Coral diversity (H')",
  "f_cv",       "Feasibility", "cv_coral",      -1,    1.0,   0,    3,   "Coral patchiness (CV)",
  "f_disease",  "Feasibility", "mean_disease",  -1,    0.5,   0,    3,   "Disease",
  "f_bleached", "Feasibility", "mean_bleached", -1,    0.5,   0,    3,   "Bleaching",
  "f_pred",     "Feasibility", "mean_predators",-1,    0.5,   0,    3,   "Coral predators",
  "u_bleached", "Urgency",     "mean_bleached",  1,    2.0,   0,    4,   "Bleaching",
  "u_disease",  "Urgency",     "mean_disease",   1,    2.0,   0,    4,   "Disease",
  "u_cv",       "Urgency",     "cv_coral",       1,    1.0,   0,    3,   "Coral patchiness (CV)",
  "u_coral",    "Urgency",     "mean_coral",    -1,    1.0,   0,    3,   "Live coral cover",
  "u_pc1",      "Urgency",     "pc1",            1,    1.5,   0,    3,   "Algal dominance (PC1)",
  "d_coral",    "Donor",       "mean_coral",     1,    3.0,   0,    6,   "Live coral cover",
  "d_shannon",  "Donor",       "mean_shannon",   1,    2.0,   0,    4,   "Coral diversity (H')",
  "d_bleached", "Donor",       "mean_bleached", -1,    2.0,   0,    4,   "Bleaching",
  "d_disease",  "Donor",       "mean_disease",  -1,    2.0,   0,    4,   "Disease",
  "d_cv",       "Donor",       "cv_coral",      -1,    1.0,   0,    3,   "Coral patchiness (CV)",
  "t_coral",    "Trend",       "coral",         -1,    4.0,   0,    6,   "Coral cover trend",
  "t_cca",      "Trend",       "cca",           -1,    2.0,   0,    4,   "CCA trend",
  "t_shannon",  "Trend",       "shannon",       -1,    2.0,   0,    4,   "Diversity trend",
  "t_turf",     "Trend",       "turf",           1,    2.0,   0,    4,   "Turf trend",
  "t_macro",    "Trend",       "macroalgae",     1,    2.0,   0,    4,   "Macroalgae trend",
  "t_cyano",    "Trend",       "cyanobacteria",  1,    3.0,   0,    4,   "Cyanobacteria trend",
  "t_bleached", "Trend",       "bleached",       1,    2.0,   0,    4,   "Bleaching trend",
  "t_disease",  "Trend",       "disease",        1,    2.0,   0,    4,   "Disease trend",
  "t_pred",     "Trend",       "predators",      1,    2.0,   0,    4,   "Predator trend"
)

# Presets: changes relative to the defaults, each with a one-line justification
PRESETS <- list(
  "Default (literature-based)" = list(changes = c(),
    why = "Weights as reported in the paper, based on resilience-assessment literature."),
  "Disease outbreak" = list(changes = c(f_disease = 1.5, u_disease = 3, d_disease = 3),
    why = "Gives disease more influence during an outbreak (e.g. SCTLD or white syndromes)."),
  "Post-bleaching event" = list(changes = c(f_bleached = 1, u_bleached = 3, d_bleached = 3),
    why = "Gives bleaching more influence immediately after a marine heatwave."),
  "Transient algae" = list(changes = c(u_pc1 = 0.5),
    why = "Reduces the weight of algal dominance where algal cover is seasonal or short-lived."),
  "Minimal overlap" = list(changes = c(u_coral = 0, u_cv = 0, f_bleached = 0, f_disease = 0, f_pred = 0),
    why = "Removes shared indicators so Feasibility reflects condition only and Urgency pressure only."))

apply_preset <- function(name, base = DEFAULT_WEIGHTS) {
  ch <- PRESETS[[name]]$changes
  if (length(ch)) base$weight[match(names(ch), base$id)] <- unname(ch)
  base
}

CLASSES <- tibble::tribble(
  ~class,                              ~pathway, ~recommendation,
  "Immediate restoration",             "A", "Relatively good condition under acute pressure: carry out a detailed site-suitability assessment, then active restoration where conditions allow.",
  "Triage and foundational support",   "B", "Poorer condition under pressure: identify and manage limiting pressures first (e.g. substrate stability, herbivory, local threats) before outplanting.",
  "Proactive reinforcement",           "C", "Relatively good condition, lower pressure: candidate for nurseries, donor material and reinforcement.",
  "Monitoring only",                   "D", "Poorer condition, lower pressure: allow natural recovery and re-survey; reassess if pressure increases."
)

# Composition variables used for the community-context PCA
PCA_VARS_COMPOSITION <- c("coral", "turf", "cca", "cyanobacteria", "macroalgae")
PCA_VARS_ORIGINAL    <- c("coral", "turf", "cca", "cyanobacteria", "bleached", "macroalgae", "disease", "predators")
BENTHIC_VARS <- c("coral", "turf", "cca", "cyanobacteria", "bleached", "macroalgae", "disease", "predators")

# -----------------------------------------------------------------------------
# 2. Helpers
# -----------------------------------------------------------------------------
scale_safe <- function(x, centre = NULL, spread = NULL) {
  # z-score across sites; returns 0 (neutral) when there is no usable variation
  # (when centre/spread are supplied, e.g. in the bootstrap, x can be a single site)
  fixed <- !is.null(centre) && !is.null(spread)
  if (is.null(centre)) centre <- mean(x, na.rm = TRUE)
  if (is.null(spread)) spread <- sd(x, na.rm = TRUE)
  if ((!fixed && sum(!is.na(x)) < 2) || is.na(spread) || spread == 0) return(rep(0, length(x)))
  z <- (x - centre) / spread
  z[is.na(z)] <- 0
  z
}

parse_dates <- function(x, day_first = TRUE) {
  # Parses mixed date strings element by element; rejects implausible years.
  x <- trimws(as.character(x))
  slash <- if (day_first) c("%d/%m/%Y", "%m/%d/%Y") else c("%m/%d/%Y", "%d/%m/%Y")
  fmts <- c("%Y-%m-%d", "%Y/%m/%d", slash, "%d-%m-%Y", "%d.%m.%Y",
            "%d %B %Y", "%d %b %Y", "%Y-%b-%d", "%Y-%B-%d")
  out <- as.Date(rep(NA_character_, length(x)))
  for (f in fmts) {
    todo <- is.na(out) & !is.na(x) & x != ""
    if (!any(todo)) break
    d <- suppressWarnings(as.Date(x[todo], format = f))
    ok <- !is.na(d) & as.integer(format(d, "%Y")) >= 1950 & as.integer(format(d, "%Y")) <= 2100
    out[which(todo)[ok]] <- d[ok]
  }
  out
}

month_number <- function(m) {
  m <- trimws(as.character(m))
  n <- suppressWarnings(as.integer(m))
  by_name <- match(tolower(m), tolower(month.name))
  by_abb  <- match(tolower(substr(m, 1, 3)), tolower(month.abb))
  ifelse(!is.na(n), n, ifelse(!is.na(by_name), by_name, by_abb))
}

# -----------------------------------------------------------------------------
# 3. Read and validate the input file
# -----------------------------------------------------------------------------
prepare_data <- function(df_raw, day_first = TRUE) {
  msgs <- character(0)
  names(df_raw) <- trimws(names(df_raw))
  lower <- tolower(names(df_raw))
  # standardise key column names (case-insensitive)
  for (nm in c("site", "date", "year", "month", BENTHIC_VARS)) {
    hit <- which(lower == nm)
    if (length(hit) >= 1) names(df_raw)[hit[1]] <- nm
  }
  # common alternative column names
  aliases <- c(diseased = "disease", predation = "predators", predator = "predators", bleaching = "bleached")
  for (a in names(aliases)) {
    if (a %in% names(df_raw) && !aliases[[a]] %in% names(df_raw)) {
      names(df_raw)[names(df_raw) == a] <- aliases[[a]]
      msgs <- c(msgs, sprintf("Column '%s' was used as '%s'.", a, aliases[[a]]))
    }
  }
  if (!"site" %in% names(df_raw)) stop("The file must contain a column named 'site'.")
  if (!"coral" %in% names(df_raw)) stop("The file must contain a 'coral' column (live coral cover).")
  tot_cols <- grep("^Tot_", names(df_raw), value = TRUE)
  if (length(tot_cols) == 0) stop("The file must contain coral taxon columns starting with 'Tot_'.")

  df <- df_raw %>% mutate(site = gsub("\\s+", " ", trimws(as.character(site)))) %>% filter(!is.na(site), site != "")
  # merge site names that differ only in capitalisation (e.g. "Barco Somero" / "Barco somero")
  key <- tolower(df$site)
  canon <- tapply(df$site, key, function(x) names(sort(table(x), decreasing = TRUE))[1])
  variants <- tapply(df$site, key, function(x) length(unique(x)))
  if (any(variants > 1)) {
    msgs <- c(msgs, sprintf("Site names differing only in capitalisation were merged: %s.",
                            paste(canon[variants > 1], collapse = ", ")))
  }
  df$site <- unname(canon[key])

  # numeric coercion, with a count of values that could not be read
  num_cols <- c(intersect(BENTHIC_VARS, names(df)), tot_cols)
  for (v in num_cols) {
    before <- sum(!is.na(df[[v]]) & df[[v]] != "")
    df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
    lost <- before - sum(!is.na(df[[v]]))
    if (lost > 0) msgs <- c(msgs, sprintf("%d value(s) in '%s' were not numeric and were treated as missing.", lost, v))
  }
  # proportions: accept 0-1 or 0-100
  for (v in intersect(setdiff(BENTHIC_VARS, "predators"), names(df))) {
    if (max(df[[v]], na.rm = TRUE) > 1.5) {
      df[[v]] <- df[[v]] / 100
      msgs <- c(msgs, sprintf("'%s' looked like a percentage and was converted to a proportion.", v))
    }
  }
  # taxon counts: transects with no taxon data get no diversity value (not zero);
  # within a transect with some taxon data, missing taxa are treated as zero
  tax <- as.matrix(df[, tot_cols])
  no_tax <- rowSums(!is.na(tax)) == 0 | rowSums(tax, na.rm = TRUE) == 0
  tax[is.na(tax)] <- 0

  # dates
  if ("date" %in% names(df)) {
    raw <- df$date
    df$date <- parse_dates(raw, day_first)
    bad <- sum(is.na(df$date) & !is.na(raw) & trimws(as.character(raw)) != "")
    if (bad > 0) msgs <- c(msgs, sprintf("%d date(s) could not be read.", bad))
  }
  if (all(c("year", "month") %in% names(df))) {
    ym <- as.Date(sprintf("%04d-%02d-01", as.integer(df$year), month_number(df$month)))
    if (!"date" %in% names(df)) df$date <- ym else df$date[is.na(df$date)] <- ym[is.na(df$date)]
  }

  df$shannon <- vegan::diversity(tax, index = "shannon")
  df$shannon[no_tax] <- NA
  if (any(no_tax)) msgs <- c(msgs, sprintf("%d transect(s) had no coral taxon data; diversity was left missing for these (not set to zero).", sum(no_tax)))
  list(df = df, tot_cols = tot_cols, messages = msgs)
}

# -----------------------------------------------------------------------------
# 4. Site-level metrics
# -----------------------------------------------------------------------------
site_metrics <- function(df) {
  has <- function(v) v %in% names(df)
  df %>%
    group_by(site) %>%
    summarise(
      n_transects    = n(),
      mean_coral     = mean(coral, na.rm = TRUE),
      cv_coral       = { m <- mean(coral, na.rm = TRUE); s <- sd(coral, na.rm = TRUE)
                         if (is.na(s) || m <= 0) 0 else s / m },
      mean_shannon   = mean(shannon, na.rm = TRUE),
      mean_bleached  = if (has("bleached"))  mean(bleached,  na.rm = TRUE) else 0,
      mean_disease   = if (has("disease"))   mean(disease,   na.rm = TRUE) else 0,
      mean_predators = if (has("predators")) mean(predators, na.rm = TRUE) else 0,
      .groups = "drop"
    ) %>%
    mutate(across(c(mean_bleached, mean_disease, mean_predators), ~ ifelse(is.nan(.x), 0, .x)),
           mean_shannon = ifelse(is.nan(mean_shannon), NA_real_, mean_shannon))
}

run_pca <- function(df, sites, vars) {
  vars <- intersect(vars, names(df))
  empty <- list(ok = FALSE, pc1 = setNames(rep(NA_real_, length(sites)), sites),
                note = "", res = NULL, means = NULL)
  if (!"coral" %in% vars || length(vars) < 3) { empty$note <- "Too few benthic variables for PCA."; return(empty) }
  means <- df %>% group_by(site) %>%
    summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>% drop_na()
  keep <- vapply(means[vars], function(x) sd(x) > 0, logical(1))
  vars <- vars[keep]
  if (nrow(means) < 3 || !"coral" %in% vars) { empty$note <- "Fewer than 3 sites with complete data, or no variation in coral."; return(empty) }
  m <- as.data.frame(means[, vars]); rownames(m) <- means$site
  res <- FactoMineR::PCA(m, graph = FALSE, ncp = min(5, length(vars)))
  load <- res$var$coord[, 1]
  algal <- intersect(c("turf", "cyanobacteria", "macroalgae"), names(load))
  accepted <- length(algal) > 0 && any(sign(load[algal]) != sign(load["coral"]))
  if (!accepted) { empty$note <- "PC1 rejected: coral does not load opposite to any algal group."; empty$res <- res; empty$means <- means; return(empty) }
  scores <- res$ind$coord[, 1]
  if (load["coral"] > 0) scores <- -scores   # higher PC1 = more algal-dominated
  pc1 <- setNames(rep(NA_real_, length(sites)), sites)
  pc1[names(scores)] <- scores
  list(ok = TRUE, pc1 = pc1, res = res, means = means,
       note = sprintf("PC1 accepted (%.0f%% of variance); oriented so higher = more algal-dominated.", res$eig[1, 2]))
}


index_scores <- function(m, w, group) {
  ww <- w %>% filter(group == !!group)
  out <- rep(0, nrow(m))
  for (i in seq_len(nrow(ww))) out <- out + ww$sign[i] * ww$weight[i] * m[[paste0("z_", ww$indicator[i])]]
  out
}

classify <- function(feas, urg) {
  ifelse(feas >= 0 & urg >= 0, "Immediate restoration",
  ifelse(feas <  0 & urg >= 0, "Triage and foundational support",
  ifelse(feas >= 0 & urg <  0, "Proactive reinforcement", "Monitoring only")))
}

Z_VARS <- c("mean_coral", "cv_coral", "mean_shannon", "mean_bleached", "mean_disease", "mean_predators", "pc1")

add_z <- function(m, stats = NULL) {
  for (v in Z_VARS) {
    x <- m[[v]]
    if (v == "pc1") x[is.na(x)] <- 0
    m[[paste0("z_", v)]] <- if (is.null(stats)) scale_safe(x) else scale_safe(x, stats[[v]]["mean"], stats[[v]]["sd"])
  }
  m
}

# -----------------------------------------------------------------------------
# 5. Time handling: data period, snapshot, survey periods
# -----------------------------------------------------------------------------
has_dates <- function(df) "date" %in% names(df) && any(!is.na(df$date))

filter_dates <- function(df, from = NULL, to = NULL) {
  # keep surveys within [from, to]; rows without a date are dropped when a limit is set
  if (!has_dates(df) || (is.null(from) && is.null(to))) return(list(df = df, dropped = 0))
  keep <- !is.na(df$date)
  if (!is.null(from)) keep <- keep & df$date >= as.Date(from)
  if (!is.null(to))   keep <- keep & df$date <= as.Date(to)
  list(df = df[keep, , drop = FALSE], dropped = sum(!keep))
}

select_snapshot <- function(df, mode = c("recent", "window"), window = NULL, recent_days = 45) {
  mode <- match.arg(mode)
  if (!has_dates(df)) return(list(df = df, label = "all data (no dates)"))
  if (mode == "window" && !is.null(window)) {
    d <- df[!is.na(df$date) & df$date >= as.Date(window[1]) & df$date <= as.Date(window[2]), , drop = FALSE]
    return(list(df = d, label = sprintf("survey window %s to %s", format(as.Date(window[1])), format(as.Date(window[2])))))
  }
  d <- df %>% filter(!is.na(date)) %>% group_by(site) %>%
    filter(date >= max(date) - recent_days) %>% ungroup()
  list(df = d, label = sprintf("most recent survey at each site (surveys within %d days of the site's latest date)", recent_days))
}

assign_periods <- function(df, period_months = 3, origin = NULL) {
  if (is.null(origin)) origin <- min(df$date, na.rm = TRUE)
  origin <- as.Date(format(as.Date(origin), "%Y-%m-01"))
  mi <- (as.integer(format(df$date, "%Y")) - as.integer(format(origin, "%Y"))) * 12 +
        (as.integer(format(df$date, "%m")) - as.integer(format(origin, "%m")))
  k <- mi %/% period_months
  start <- seq(origin, by = "month", length.out = max(k, na.rm = TRUE) * period_months + period_months)
  st <- start[k * period_months + 1]
  en <- start[k * period_months + period_months]
  df$period <- k
  df$period_label <- if (period_months == 1) format(st, "%b %Y") else
    ifelse(format(st, "%Y") == format(en, "%Y"), paste0(format(st, "%b"), "-", format(en, "%b %Y")),
           paste0(format(st, "%b %Y"), "-", format(en, "%b %Y")))
  df
}

site_trends <- function(df, w, sites) {
  # slopes per year for the given sites; trend score standardised across those sites
  tw <- w %>% filter(group == "Trend", indicator %in% names(df))
  vars <- unique(tw$indicator)
  sl <- df %>% filter(!is.na(date), site %in% sites) %>% mutate(t = as.numeric(date) / 365.25) %>%
    group_by(site) %>%
    summarise(across(all_of(vars), ~ if (n_distinct(t) > 1 && sum(!is.na(.x)) > 1)
      unname(coef(lm(.x ~ t))[2]) else NA_real_), .groups = "drop")
  total <- rep(0, nrow(sl))
  for (i in seq_len(nrow(tw))) total <- total + tw$sign[i] * tw$weight[i] * scale_safe(sl[[tw$indicator[i]]])
  tibble(site = sl$site, trend_score = scale_safe(total))
}

# -----------------------------------------------------------------------------
# 6. Scores for one set of units (sites, or site x period)
# -----------------------------------------------------------------------------
score_units <- function(df, w, pca_vars) {
  m <- site_metrics(df)
  pca <- run_pca(df, m$site, pca_vars)
  m$pc1 <- unname(pca$pc1[m$site])
  stats <- lapply(setNames(Z_VARS, Z_VARS), function(v) {
    x <- m[[v]]; if (v == "pc1") x[is.na(x)] <- 0
    c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
  })
  m <- add_z(m)
  m$feasibility  <- index_scores(m, w, "Feasibility")
  m$urgency_base <- index_scores(m, w, "Urgency")
  list(m = m, pca = pca, stats = stats)
}

# -----------------------------------------------------------------------------
# 7. Full analysis
# -----------------------------------------------------------------------------
run_analysis <- function(prep, w = DEFAULT_WEIGHTS,
                         data_from = NULL, data_to = NULL,
                         snapshot = c("recent", "window"), window = NULL, recent_days = 45,
                         period_months = 3, period_start_month = NULL,
                         include_trends = FALSE, trend_min_periods = 3, trend_min_span = 365,
                         min_transects = 5, B = 200, seed = 123,
                         pca_vars = PCA_VARS_COMPOSITION) {
  snapshot <- match.arg(snapshot)
  notes <- prep$messages

  # 7.1 data period
  fd <- filter_dates(prep$df, data_from, data_to)
  df_all <- fd$df
  if (fd$dropped > 0) notes <- c(notes, sprintf("%d transect(s) outside the data period (or without a date) were excluded.", fd$dropped))
  if (nrow(df_all) == 0) stop("No surveys fall within the selected data period.")

  # 7.2 snapshot
  snap <- select_snapshot(df_all, snapshot, window, recent_days)
  df <- snap$df
  if (nrow(df) == 0) stop("No surveys fall within the snapshot window.")
  missing_sites <- setdiff(unique(df_all$site), unique(df$site))
  if (length(missing_sites)) notes <- c(notes, sprintf("Not surveyed in the snapshot, so not scored: %s.", paste(missing_sites, collapse = ", ")))
  notes <- c(notes, paste0("Snapshot: ", snap$label, "."))

  if (snapshot == "recent" && has_dates(df)) {
    latest <- df %>% group_by(site) %>% summarise(d = max(date), .groups = "drop")
    if (as.numeric(diff(range(latest$d))) > 90)
      notes <- c(notes, sprintf("Most recent surveys differ between sites (%s to %s); sites are compared at different times. Consider a survey window instead.",
                                format(min(latest$d)), format(max(latest$d))))
  }

  # period origin: first day of period_start_month on or before the first survey
  period_origin <- NULL
  if (has_dates(df_all)) {
    first <- min(df_all$date, na.rm = TRUE)
    psm <- if (is.null(period_start_month)) as.integer(format(first, "%m")) else as.integer(period_start_month)
    yr <- as.integer(format(first, "%Y"))
    period_origin <- as.Date(sprintf("%d-%02d-01", yr, psm))
    if (period_origin > first) period_origin <- as.Date(sprintf("%d-%02d-01", yr - 1, psm))
  }

  sc <- score_units(df, w, pca_vars)
  m <- sc$m; pca <- sc$pca; stats <- sc$stats
  notes <- c(notes, pca$note)

  # 7.3 survey periods and trend eligibility (from all data in the data period)
  dated <- has_dates(df_all)
  n_dates <- if (dated) n_distinct(df_all$date[!is.na(df_all$date)]) else 0
  span_days <- if (n_dates > 1) as.numeric(diff(range(df_all$date, na.rm = TRUE))) else 0
  per_site <- tibble(site = m$site, n_periods = 0L, span_days = 0, trend_eligible = FALSE)
  if (dated) {
    dp <- assign_periods(df_all %>% filter(!is.na(date)), period_months, period_origin)
    ps <- dp %>% group_by(site) %>%
      summarise(n_periods = n_distinct(period), span_days = as.numeric(max(date) - min(date)), .groups = "drop")
    per_site <- tibble(site = m$site) %>% left_join(ps, by = "site") %>%
      mutate(n_periods = coalesce(n_periods, 0L), span_days = coalesce(span_days, 0),
             trend_eligible = n_periods >= trend_min_periods & span_days >= trend_min_span)
  }
  m <- m %>% left_join(per_site, by = "site")

  # 7.4 trend adjustment (only for eligible sites)
  m$trend_score <- 0
  trend_used <- FALSE
  if (include_trends) {
    elig <- m$site[m$trend_eligible]
    if (length(elig) >= 2) {
      tr <- site_trends(df_all, w, elig)
      m <- m %>% select(-trend_score) %>% left_join(tr, by = "site") %>% mutate(trend_score = coalesce(trend_score, 0))
      trend_used <- TRUE
      notes <- c(notes, sprintf("Trend adjustment applied to %d site(s) with at least %d survey periods over at least %d days: %s.",
                                length(elig), trend_min_periods, trend_min_span, paste(elig, collapse = ", ")))
    } else {
      notes <- c(notes, sprintf("Trend adjustment requested but fewer than two sites have at least %d survey periods over at least %d days; not applied. See the Trajectories tab for change through time.",
                                trend_min_periods, trend_min_span))
    }
  }
  m$trend_note <- ifelse(m$trend_eligible, ifelse(trend_used, "applied", "eligible (not requested)"),
                         sprintf("not eligible (%d period(s), %d days)", m$n_periods, round(m$span_days)))
  m$urgency <- m$urgency_base + m$trend_score
  m$class <- classify(m$feasibility, m$urgency)

  # 7.5 bootstrap CIs on the snapshot: resample transects within site; PC1 and trend held at site values
  set.seed(seed)
  boot <- lapply(seq_len(nrow(m)), function(i) {
    d <- df[df$site == m$site[i], , drop = FALSE]
    if (nrow(d) < 2) return(c(fl = m$feasibility[i], fh = m$feasibility[i], ul = m$urgency[i], uh = m$urgency[i]))
    reps <- replicate(B, {
      bm <- site_metrics(d[sample(nrow(d), replace = TRUE), , drop = FALSE])
      bm$pc1 <- m$pc1[i]
      bm <- add_z(bm, stats)
      c(index_scores(bm, w, "Feasibility"), index_scores(bm, w, "Urgency") + m$trend_score[i])
    })
    c(fl = unname(quantile(reps[1, ], .025)), fh = unname(quantile(reps[1, ], .975)),
      ul = unname(quantile(reps[2, ], .025)), uh = unname(quantile(reps[2, ], .975)))
  })
  b <- do.call(rbind, boot)
  m$feas_low <- b[, "fl"]; m$feas_high <- b[, "fh"]; m$urg_low <- b[, "ul"]; m$urg_high <- b[, "uh"]
  m$borderline <- with(m, ifelse(feas_low < 0 & feas_high > 0 & urg_low < 0 & urg_high > 0, "Both axes",
                          ifelse(feas_low < 0 & feas_high > 0, "Feasibility",
                          ifelse(urg_low < 0 & urg_high > 0, "Urgency", "No"))))
  m$below_min <- m$n_transects < min_transects
  m <- m %>% left_join(CLASSES, by = "class")

  # 7.6 donor scores (snapshot data)
  dm <- add_z(site_metrics(df) %>% mutate(pc1 = 0))
  dm$donor_score <- index_scores(dm, w, "Donor")
  donors <- dm %>% select(site, n_transects, mean_coral, cv_coral, mean_shannon, mean_bleached, mean_disease, donor_score) %>%
    arrange(desc(donor_score))
  m <- m %>% left_join(donors %>% select(site, donor_score), by = "site")

  # 7.7 trajectories: site x period scores, standardised across all site-periods
  traj <- NULL; traj_note <- "Trajectories need dated surveys in at least two periods."
  if (dated) {
    dp <- assign_periods(df_all %>% filter(!is.na(date)), period_months, period_origin)
    if (n_distinct(dp$period) >= 2) {
      dp$unit <- paste(dp$site, dp$period, sep = " || ")
      lab <- dp %>% distinct(unit, site, period, period_label)
      tsc <- score_units(dp %>% mutate(site = unit), w, pca_vars)
      traj <- tsc$m %>% rename(unit = site) %>% left_join(lab, by = "unit") %>%
        mutate(urgency = urgency_base, class = classify(feasibility, urgency),
               below_min = n_transects < min_transects) %>%
        select(site, period, period_label, n_transects, below_min, mean_coral, mean_bleached, mean_disease,
               feasibility, urgency, class) %>%
        arrange(site, period)
      traj_note <- sprintf("Scores for each site in each %d-month period, standardised across all site-periods (zero = average over all sites and periods), so movement reflects change at the site. %s",
                           period_months, tsc$pca$note)
    }
  }

  # 7.8 summaries
  rho <- if (nrow(m) >= 3) suppressWarnings(cor(m$feasibility, m$urgency, method = "spearman")) else NA_real_
  diag <- df %>% group_by(site) %>%
    summarise(snapshot_transects = n(),
              snapshot_dates = if (has_dates(df)) paste(format(sort(unique(date))), collapse = ", ") else NA_character_,
              .groups = "drop") %>%
    left_join(m %>% select(site, mean_shannon, pc1, borderline, n_periods, span_days, trend_note), by = "site") %>%
    mutate(pca_accepted = pca$ok, below_min = snapshot_transects < min_transects,
           note = ifelse(below_min, sprintf("Fewer than %d transects in snapshot: interpret with caution", min_transects), ""))

  list(sites = m, donors = donors, pca = pca, diagnostics = diag, notes = notes,
       rho = rho, quadrant_counts = table(factor(m$class, levels = CLASSES$class)),
       trajectories = traj, trajectory_note = traj_note,
       settings = list(weights = w, data_from = data_from, data_to = data_to, snapshot = snap$label,
                       period_months = period_months, period_origin = period_origin, include_trends = trend_used,
                       trend_min_periods = trend_min_periods, trend_min_span = trend_min_span,
                       min_transects = min_transects, B = B, seed = seed,
                       pca_vars = intersect(pca_vars, names(df)),
                       n_dates = n_dates, span_days = span_days, version = APP_VERSION,
                       run_at = format(Sys.time(), "%Y-%m-%d %H:%M")),
       stats = stats)
}

# -----------------------------------------------------------------------------
# 8. Weight sensitivity (point estimates; fast)
# -----------------------------------------------------------------------------
weight_sensitivity <- function(res, w = res$settings$weights, factor = 0.5) {
  m <- res$sites
  base <- m$class
  fu <- w %>% filter(group %in% c("Feasibility", "Urgency"))
  scen <- list()
  for (i in seq_len(nrow(fu))) {
    for (f in c(1 - factor, 1 + factor)) {
      ww <- w; ww$weight[ww$id == fu$id[i]] <- ww$weight[ww$id == fu$id[i]] * f
      scen[[sprintf("%s x%.1f", fu$id[i], f)]] <- ww
    }
  }
  scen[["Minimal overlap"]] <- apply_preset("Minimal overlap", w)
  cls <- sapply(scen, function(ww) classify(index_scores(m, ww, "Feasibility"),
                                              index_scores(m, ww, "Urgency") + m$trend_score))
  cls <- matrix(cls, nrow = nrow(m), dimnames = list(m$site, names(scen)))
  by_site <- tibble(site = m$site, class = base, stability = rowMeans(cls == base))
  by_site$most_common_alternative <- sapply(seq_len(nrow(m)), function(k) {
    alt <- cls[k, cls[k, ] != base[k]]
    if (length(alt) == 0) "None" else names(sort(table(alt), decreasing = TRUE))[1]
  })
  by_scenario <- tibble(scenario = colnames(cls), sites_changed = colSums(cls != base))
  list(by_site = by_site, by_scenario = by_scenario, n_scenarios = ncol(cls))
}

# =============================================================================
# VERSION 1 (original 0525 app), calculations copied unchanged, without Shiny.
# Bugs are kept deliberately so the comparison shows what changed.
# =============================================================================
v1_analysis <- function(df_raw, topN = 5) {
  if (!"site" %in% names(df_raw)) stop("v1: no 'site' column")
  df_raw <- df_raw %>% mutate(site = trimws(as.character(site)))
  date_cols <- names(df_raw)[tolower(names(df_raw)) == "date"]
  if (length(date_cols) > 1) {
    df_raw <- df_raw %>% select(-all_of(setdiff(date_cols, date_cols[1]))) %>% rename(date = all_of(date_cols[1]))
  } else if (length(date_cols) == 1 && date_cols != "date") {
    df_raw <- df_raw %>% rename(date = all_of(date_cols))
  }
  if ("date" %in% names(df_raw)) {
    df_raw <- df_raw %>% mutate(date = as.Date(date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%d-%m-%Y", "%Y-%b-%d", "%Y-%B-%d")))
  }
  if ("date" %in% names(df_raw) && any(is.na(df_raw$date)) && all(c("year", "month") %in% names(df_raw))) {
    df_raw <- df_raw %>% mutate(date = if_else(is.na(date), as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%B-%d"), date))
  }
  if (!("date" %in% names(df_raw)) && all(c("year", "month") %in% names(df_raw))) {
    df_raw <- df_raw %>% mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%B-%d"))
  }
  expected_benthic <- c("coral","turf","cca","cyanobacteria","bleached","macroalgae","disease","predators")
  present_benthic <- expected_benthic[expected_benthic %in% names(df_raw)]
  if (length(present_benthic) > 0) df_raw <- df_raw %>% mutate(across(all_of(present_benthic), ~ as.numeric(.x)))
  df <- df_raw

  tot_cols <- grep("^Tot_", names(df), value = TRUE)
  if (length(tot_cols) == 0) stop("v1: no Tot_ columns")
  df <- df %>% mutate(shannon = diversity(select(., all_of(tot_cols)), index = "shannon"))

  benthic_vars  <- c("coral", "turf", "cca", "cyanobacteria", "bleached")
  optional_vars <- c("macroalgae", "disease", "predators")
  all_vars      <- c(benthic_vars, optional_vars)
  included_vars <- all_vars[all_vars %in% names(df)]

  df$PC1 <- NA_real_; pca_ok <- FALSE
  df_pca_plot <- df %>% group_by(site) %>%
    summarise(across(all_of(included_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>% drop_na()
  if (nrow(df_pca_plot) >= 2) {
    pca_res  <- PCA(df_pca_plot %>% select(-site), graph = FALSE)
    loadings <- get_pca_var(pca_res)$coord[, 1]
    algal    <- intersect(c("turf", "cyanobacteria", "macroalgae"), names(loadings))
    if ("coral" %in% names(loadings) && any(loadings[algal] * loadings["coral"] < 0)) {
      pca_ok <- TRUE
      flip   <- loadings["coral"] > 0
      df_clean <- df %>% select(all_of(included_vars)) %>% drop_na()
      scores   <- get_pca_ind(PCA(df_clean, graph = FALSE))$coord[, 1]
      if (flip) scores <- -scores
      df$PC1[match(rownames(df_clean), rownames(df))] <- scores
    }
  }

  scale_safe1 <- function(x) {
    x_non_na <- x[!is.na(x)]
    if (length(x_non_na) < 2 || length(unique(x_non_na)) <= 1) rep(0, length(x)) else as.numeric(scale(x))
  }
  site_scores <- df %>% group_by(site) %>%
    summarise(
      n_transects    = n(),
      mean_coral     = mean(coral, na.rm = TRUE),
      cv_coral       = ifelse(mean(coral, na.rm = TRUE) > 0, sd(coral, na.rm = TRUE)/mean(coral, na.rm = TRUE), 0),
      mean_shannon   = mean(shannon, na.rm = TRUE),
      pc1            = if (pca_ok) mean(PC1, na.rm = TRUE) else NA_real_,
      mean_disease   = if ("disease"   %in% names(df)) mean(disease,   na.rm = TRUE) else 0,
      mean_bleached  = if ("bleached"  %in% names(df)) mean(bleached,  na.rm = TRUE) else 0,
      mean_predators = if ("predators" %in% names(df)) mean(predators, na.rm = TRUE) else 0,
      .groups = "drop") %>%
    mutate(
      z_mean_coral = scale_safe1(mean_coral), z_cv_coral = scale_safe1(cv_coral),
      z_mean_shannon = scale_safe1(mean_shannon), z_pc1 = scale_safe1(ifelse(is.na(pc1), 0, pc1)),
      z_mean_disease = scale_safe1(mean_disease), z_mean_bleached = scale_safe1(mean_bleached),
      z_mean_pred = scale_safe1(mean_predators),
      feasibility  = 3*z_mean_coral + 2*z_mean_shannon - 1*z_cv_coral - 0.5*z_mean_disease - 0.5*z_mean_bleached - 0.5*z_mean_pred,
      urgency_base = 2*z_mean_bleached + 2*z_mean_disease + 1*z_cv_coral - 1*z_mean_coral + 1.5*z_pc1,
      trend_score  = 0)

  if ("date" %in% names(df) && n_distinct(df$date) > 1) {
    df <- df %>% mutate(date_numeric = as.numeric(date))
    trend_vars <- c(benthic_vars, optional_vars, "shannon", "predators")
    weights <- c(coral = -4, turf = 2, cca = -2, cyanobacteria = 3, bleached = 2, macroalgae = 2, disease = 2, shannon = -2, predators = 2)
    valid_trend_vars <- trend_vars[trend_vars %in% names(df)]
    trends <- df %>% group_by(site) %>%
      summarise(across(all_of(valid_trend_vars), ~ if (length(unique(date_numeric)) > 1) coef(lm(.x ~ date_numeric))[2] else NA_real_,
                       .names = "trend_{.col}"), .groups = "drop")
    z_trend_mat <- trends %>% select(site)
    for (v in valid_trend_vars) {
      sv <- trends[[paste0("trend_", v)]]
      z_trend_mat[[paste0("z_trend_", v)]] <- if (all(is.na(sv)) || is.na(sd(sv, na.rm = TRUE)) || sd(sv, na.rm = TRUE) == 0) 0 else (sv - mean(sv, na.rm = TRUE)) / sd(sv, na.rm = TRUE)
    }
    ws <- rep(0, nrow(z_trend_mat))
    for (v in valid_trend_vars) ws <- ws + weights[[v]] * ifelse(is.na(z_trend_mat[[paste0("z_trend_", v)]]), 0, z_trend_mat[[paste0("z_trend_", v)]])
    z_trend_mat$trend_score <- if (nrow(z_trend_mat) > 1 && sd(ws) > 0) (ws - mean(ws)) / sd(ws) else 0
    site_scores <- site_scores %>% select(-trend_score) %>% left_join(z_trend_mat %>% select(site, trend_score), by = "site") %>%
      mutate(trend_score = coalesce(trend_score, 0))
  }
  site_scores <- site_scores %>% mutate(urgency = urgency_base + trend_score)

  orig <- function(v) c(mean(v, na.rm = TRUE), sd(v, na.rm = TRUE))
  os <- list(mc = orig(site_scores$mean_coral), cv = orig(site_scores$cv_coral), ms = orig(site_scores$mean_shannon),
             p1 = orig(site_scores$pc1[!is.na(site_scores$pc1)]), md = orig(site_scores$mean_disease),
             mb = orig(site_scores$mean_bleached), mp = orig(site_scores$mean_predators))
  zz <- function(x, s) if (!is.na(s[2]) && s[2] > 0) (x - s[1]) / s[2] else 0   # (v1 would stop here if sd is NA)
  boot_one <- function(d) {
    sh <- diversity(d %>% select(all_of(included_vars)) %>% as.matrix(), index = "shannon")   # v1 bug kept: benthic columns
    mc <- mean(d$coral, na.rm = TRUE)
    cvc <- ifelse(mc > 0, sd(d$coral, na.rm = TRUE) / mc, 0)
    md <- if ("disease" %in% names(d)) mean(d$disease, na.rm = TRUE) else 0
    mb <- if ("bleached" %in% names(d)) mean(d$bleached, na.rm = TRUE) else 0
    mp <- if ("predators" %in% names(d)) mean(d$predators, na.rm = TRUE) else 0
    z_mc <- zz(mc, os$mc); z_cv <- zz(cvc, os$cv); z_ms <- zz(mean(sh, na.rm = TRUE), os$ms); z_p1 <- zz(0, os$p1)
    z_md <- zz(md, os$md); z_mb <- zz(mb, os$mb); z_mp <- zz(mp, os$mp)
    c(3*z_mc + 2*z_ms - z_cv - 0.5*z_md - 0.5*z_mb - 0.5*z_mp, 2*z_mb + 2*z_md + z_cv - z_mc + 1.5*z_p1)
  }
  set.seed(123); B <- 200
  ci <- t(sapply(site_scores$site, function(s) {
    d <- df %>% filter(site == s)
    r <- replicate(B, boot_one(d[sample(nrow(d), replace = TRUE), ]))
    c(quantile(r[1, ], c(.025, .975), na.rm = TRUE), quantile(r[2, ], c(.025, .975), na.rm = TRUE))
  }))
  site_scores$feas_low <- ci[, 1]; site_scores$feas_high <- ci[, 2]
  site_scores$urg_low  <- ci[, 3]; site_scores$urg_high  <- ci[, 4]

  site_scores$class <- with(site_scores, ifelse(feasibility >= 0 & urgency >= 0, "Immediate restoration",
                                          ifelse(feasibility < 0 & urgency >= 0, "Triage and foundational support",
                                          ifelse(feasibility < 0 & urgency < 0, "Monitoring only", "Proactive reinforcement"))))

  latest <- df
  if ("date" %in% names(df) && any(!is.na(df$date))) latest <- df %>% group_by(site) %>% filter(date == max(date, na.rm = TRUE)) %>% ungroup()
  donors <- latest %>% group_by(site) %>%
    summarise(mean_coral = mean(coral, na.rm = TRUE),
              cv_coral = ifelse(mean(coral, na.rm = TRUE) > 0, sd(coral, na.rm = TRUE)/mean(coral, na.rm = TRUE), 0),
              mean_shannon = mean(shannon, na.rm = TRUE),
              mean_bleached = if ("bleached" %in% names(latest)) mean(bleached, na.rm = TRUE) else 0,
              mean_disease  = if ("disease"  %in% names(latest)) mean(disease,  na.rm = TRUE) else 0, .groups = "drop") %>%
    mutate(donor_score = 3*scale_safe1(mean_coral) + 2*scale_safe1(mean_shannon) - 2*scale_safe1(mean_bleached) -
             2*scale_safe1(mean_disease) - 1*scale_safe1(cv_coral)) %>% arrange(desc(donor_score))
  site_scores <- site_scores %>% left_join(donors %>% select(site, donor_score), by = "site")
  list(sites = site_scores, donors = donors, pca_ok = pca_ok, dates = if ("date" %in% names(df)) df$date else NULL)
}

# =============================================================================
# RUN BOTH VERSIONS AND COMPARE
# =============================================================================
if (is.null(data_file)) {
  csvs <- setdiff(list.files(pattern = "\\.csv$", ignore.case = TRUE),
                  c("comparison_v1_v2.csv", "comparison_v1_v2_donors.csv", "trajectories_v2.csv"))
  if (length(csvs) != 1) stop("Set data_file at the top of this script. CSV files in this folder: ", paste(csvs, collapse = ", "))
  data_file <- csvs
}
cat("Using data file:", data_file, "\n\n")
raw <- read.csv(data_file, stringsAsFactors = FALSE, check.names = FALSE)

prep <- prepare_data(raw, day_first = dates_day_first)

# Version 1 is given only the rows version 2 scores (same data period and snapshot window),
# with dates read correctly, so the comparison is about the calculations, not the date bug.
date_col <- names(raw)[tolower(names(raw)) == "date"][1]
raw_dates <- if (!is.na(date_col)) parse_dates(raw[[date_col]], dates_day_first) else as.Date(rep(NA, nrow(raw)))
keep <- !is.na(raw_dates) & raw_dates >= data_from
if (!is.null(snapshot_window)) keep <- keep & raw_dates >= snapshot_window[1] & raw_dates <= snapshot_window[2]
raw_v1 <- raw[keep, , drop = FALSE]
if (!is.na(date_col)) raw_v1[[date_col]] <- format(raw_dates[keep])   # ISO dates, which v1 reads correctly
rownames(raw_v1) <- NULL
cat(sprintf("Version 1 uses %d of %d transects (%s).\n\n", nrow(raw_v1), nrow(raw),
            if (is.null(snapshot_window)) paste("from", format(data_from)) else paste(format(snapshot_window), collapse = " to ")))

v1 <- tryCatch(v1_analysis(raw_v1), error = function(e) { message("Version 1 stopped with an error: ", conditionMessage(e)); NULL })
v2 <- run_analysis(prep, data_from = data_from,
                   snapshot = if (is.null(snapshot_window)) "recent" else "window", window = snapshot_window,
                   period_months = period_months, period_start_month = period_start_month,
                   include_trends = v2_include_trends,
                   min_transects = min_transects, B = 200, seed = 123,
                   pca_vars = if (v2_pca_as_v1) PCA_VARS_ORIGINAL else PCA_VARS_COMPOSITION)
sens <- weight_sensitivity(v2)

cat("---- Version 2 notes ----\n"); cat(paste("-", v2$notes), sep = "\n"); cat("\n")
if (!is.null(v1) && !is.null(v1$dates)) {
  cat("Dates scored by version 1:", paste(format(range(v1$dates, na.rm = TRUE)), collapse = " to "),
      "| version 2 snapshot:", v2$settings$snapshot, "\n\n")
}

r2 <- function(x) round(x, 2)
cmp <- v2$sites %>%
  transmute(site, n_transects,
            v2_class = class, v2_feasibility = r2(feasibility), v2_feas_CI = sprintf("%.2f to %.2f", feas_low, feas_high),
            v2_urgency = r2(urgency), v2_urg_CI = sprintf("%.2f to %.2f", urg_low, urg_high),
            v2_borderline = borderline, v2_below_min = below_min)
if (!is.null(v1)) {
  cmp <- v1$sites %>%
    transmute(site, v1_class = class, v1_feasibility = r2(feasibility), v1_feas_CI = sprintf("%.2f to %.2f", feas_low, feas_high),
              v1_urgency = r2(urgency), v1_urg_CI = sprintf("%.2f to %.2f", urg_low, urg_high),
              v1_trend_used = r2(trend_score)) %>%
    right_join(cmp, by = "site") %>%
    mutate(class_changed = ifelse(v1_class == v2_class, "", "CHANGED"),
           v1_feas_inside_own_CI = ifelse(v1$sites$feasibility[match(site, v1$sites$site)] >= v1$sites$feas_low[match(site, v1$sites$site)] &
                                             v1$sites$feasibility[match(site, v1$sites$site)] <= v1$sites$feas_high[match(site, v1$sites$site)], "yes", "NO")) %>%
    select(site, n_transects, v1_class, v2_class, class_changed, v1_feasibility, v2_feasibility, v1_urgency, v2_urgency,
           v1_feas_CI, v2_feas_CI, v1_urg_CI, v2_urg_CI, v1_feas_inside_own_CI, v1_trend_used, v2_borderline, v2_below_min)
}
cmp <- cmp %>% left_join(sens$by_site %>% transmute(site, v2_weight_stability = sprintf("%.0f%%", 100 * stability)), by = "site")

cat("---- Site classes and scores: version 1 vs version 2 ----\n")
print(as.data.frame(cmp), row.names = FALSE)
cat(sprintf("\nVersion 2: Feasibility-Urgency Spearman rho = %.2f\n", v2$rho))

donor_cmp <- v2$donors %>% transmute(site, v2_rank = row_number(), v2_donor_score = r2(donor_score))
if (!is.null(v1)) donor_cmp <- v1$donors %>% transmute(site, v1_rank = row_number(), v1_donor_score = r2(donor_score)) %>% full_join(donor_cmp, by = "site")
cat("\n---- Donor ranking ----\n"); print(as.data.frame(donor_cmp), row.names = FALSE)

write.csv(cmp, "comparison_v1_v2.csv", row.names = FALSE)
write.csv(donor_cmp, "comparison_v1_v2_donors.csv", row.names = FALSE)

# side-by-side decision planes
pl <- v2$sites %>% transmute(site, feasibility, urgency, feas_low, feas_high, urg_low, urg_high, class, version = "Version 2 (revised)")
if (!is.null(v1)) pl <- bind_rows(v1$sites %>% transmute(site, feasibility, urgency, feas_low, feas_high, urg_low, urg_high, class,
                                                         version = "Version 1 (original)"), pl)
p <- ggplot(pl, aes(feasibility, urgency, colour = class)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") + geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = feas_low, xmax = feas_high), height = 0) + geom_errorbar(aes(ymin = urg_low, ymax = urg_high), width = 0) +
  geom_point(size = 2.5) + geom_text(aes(label = site), vjust = -0.9, size = 3.2, show.legend = FALSE) +
  facet_wrap(~ version) +
  scale_colour_manual(values = c("Immediate restoration" = "#2E6B45", "Triage and foundational support" = "#C0392B",
                                 "Proactive reinforcement" = "#2C7FB8", "Monitoring only" = "#7F7F7F"), name = NULL) +
  labs(x = "Feasibility", y = "Urgency", caption = "Bars: 95% bootstrap intervals") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
print(p)
ggsave("comparison_v1_v2.png", p, width = 11, height = 5.5, dpi = 200)

# ---- version 2 trajectories (all surveys in the data period) ----------------
cat("\n---- Version 2 trajectories ----\n", v2$trajectory_note, "\n")
if (!is.null(v2$trajectories)) {
  tr <- v2$trajectories
  write.csv(tr, "trajectories_v2.csv", row.names = FALSE)
  last <- tr %>% group_by(site) %>% filter(period == max(period)) %>% ungroup()
  first <- tr %>% group_by(site) %>% filter(period == min(period)) %>% ungroup()
  pt <- ggplot(tr, aes(feasibility, urgency, colour = site)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") + geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_path(arrow = arrow(length = unit(0.1, "inches"), type = "closed"), linewidth = 0.7) +
    geom_point(data = first, shape = 1, size = 2) + geom_point(data = last, size = 2.5) +
    facet_wrap(~ site) +
    labs(x = "Feasibility", y = "Urgency",
         title = sprintf("Site trajectories, %d-month periods", period_months),
         caption = "Open circle: first period; filled: latest. Zero = average over all sites and periods.") +
    theme_minimal(base_size = 11) + theme(legend.position = "none")
  print(pt)
  ggsave("trajectories_v2.png", pt, width = 11, height = 8, dpi = 200)
  print(as.data.frame(tr %>% select(site, period_label, n_transects, feasibility, urgency, class) %>%
                        mutate(across(where(is.numeric), ~ round(.x, 2)))), row.names = FALSE)
}
cat("\nSaved in your working directory: comparison_v1_v2.csv, comparison_v1_v2_donors.csv, comparison_v1_v2.png, trajectories_v2.csv, trajectories_v2.png\n")
