# =============================================================================
# plots_tables.R -- Reef Restoration Prioritisation Tool
#
# Tables, figures and the formula text shown in the app.
# Moved verbatim from analysis.R (v2.3.0); roxygen headers added.
# =============================================================================

# -----------------------------------------------------------------------------
# 11. Tables and figures
# -----------------------------------------------------------------------------
#' Site results table for display and export.
#'
#' @param r Result of `run_analysis()`.
#' @param digits Decimal places.
#' @return Data frame.
site_table <- function(r, digits = 2) {
  m <- r$sites
  out <- data.frame(site = m$site, depth_m = m$depth_m, n_transects = m$n_transects,
                    coral = m$mean_coral, H = m$mean_shannon, CV_coral = m$cv_coral, turf = m$mean_turf, cca = m$mean_cca,
                    bleached = m$mean_bleached, disease = m$mean_disease, predation = m$mean_predation, rubble = m$mean_rubble, sand = m$mean_sand,
                    PC1 = m$pc1, feasibility = m$feasibility, feas_low = m$feas_low, feas_high = m$feas_high,
                    urgency = m$urgency, urg_low = m$urg_low, urg_high = m$urg_high,
                    class = m$class, pathway = m$pathway, class_support = m$class_support, alerts = m$alerts,
                    borderline = m$borderline, below_min = m$below_min, trend_status = m$trend_status,
                    stringsAsFactors = FALSE)
  if (r$settings$trend_used) out$trend_contribution <- m$trend_contribution
  num <- vapply(out, is.numeric, logical(1))
  out[num] <- lapply(out[num], round, digits)
  out
}

#' Feasibility-Urgency decision plane with 95% bootstrap intervals.
#'
#' @param r Result of `run_analysis()`.
#' @param base_size Base font size.
#' @return A ggplot object.
plot_decision_plane <- function(r, base_size = 13) {
  s <- r$sites
  s$flag <- ifelse(s$borderline != "No" | s$below_min, paste0(s$site, "*"), s$site)
  s$data <- ifelse(s$below_min, "Fewer transects than the minimum", "Adequate")
  s$class <- factor(paste0(s$pathway, ". ", s$class), levels = paste0(CLASSES$pathway, ". ", CLASSES$class))
  cols <- setNames(CLASS_COLOURS[CLASSES$class], levels(s$class))
  lab <- if (requireNamespace("ggrepel", quietly = TRUE))
    ggrepel::geom_text_repel(ggplot2::aes(label = flag), size = base_size / 3.4, show.legend = FALSE, seed = 1, min.segment.length = 0.3) else
    ggplot2::geom_text(ggplot2::aes(label = flag), vjust = -1, size = base_size / 3.4, show.legend = FALSE)
  ggplot2::ggplot(s, ggplot2::aes(feasibility, urgency, colour = class)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = feas_low, xmax = feas_high), orientation = "y", width = 0, alpha = 0.7) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = urg_low, ymax = urg_high), width = 0, alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(shape = data), size = 3, stroke = 1.1) +
    lab +
    ggplot2::scale_shape_manual(values = c("Adequate" = 16, "Fewer transects than the minimum" = 1), name = NULL, drop = TRUE,
                                guide = if (any(s$below_min)) "legend" else "none") +
    ggplot2::scale_colour_manual(values = cols, name = NULL, drop = FALSE) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2, override.aes = list(shape = 16))) +
    ggplot2::labs(x = "Feasibility (relative condition)", y = "Urgency (relative pressure)",
                  caption = "Bars: 95% bootstrap intervals. Zero lines = mean of the sites analysed.\n* borderline (interval crosses zero) or below the minimum number of transects.") +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(legend.position = "bottom", legend.box = "vertical")
}

#' Site trajectories on the decision plane, one panel per site.
#'
#' @param r Result of `run_analysis()`.
#' @param base_size Base font size.
#' @return A ggplot object, or `NULL`.
plot_trajectories <- function(r, base_size = 13) {
  t <- r$trajectories
  if (is.null(t) || nrow(t) == 0) return(NULL)
  t <- t[order(t$site, t$period_start), ]
  last <- do.call(rbind, lapply(split(t, t$site), function(x) x[nrow(x), ]))
  ggplot2::ggplot(t, ggplot2::aes(feasibility, urgency, group = site)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_path(colour = "grey55", arrow = ggplot2::arrow(length = ggplot2::unit(0.18, "cm"), type = "closed")) +
    ggplot2::geom_point(ggplot2::aes(colour = class), size = 2) +
    ggplot2::geom_text(data = last, ggplot2::aes(label = site), size = base_size / 4, vjust = -0.9) +
    ggplot2::scale_colour_manual(values = CLASS_COLOURS, name = NULL, drop = FALSE) +
    ggplot2::facet_wrap(~ site) +
    ggplot2::labs(x = "Feasibility (relative to all site-periods)", y = "Urgency (relative to all site-periods)",
                  caption = "Descriptive only: each site-period scored on a common scale; arrows run from earlier to later periods.") +
    ggplot2::theme_minimal(base_size = base_size) + ggplot2::theme(legend.position = "bottom")
}

#' Trend scores with 95% bootstrap intervals.
#'
#' @param r Result of `run_analysis()`.
#' @param base_size Base font size.
#' @return A ggplot object, or `NULL`.
plot_trend_scores <- function(r, base_size = 13) {
  t <- r$trends$table
  if (is.null(t) || !"trend_score" %in% names(t)) return(NULL)
  t <- t[!is.na(t$trend_score), ]
  t$site <- factor(t$site, levels = t$site[order(t$trend_score)])
  short <- c("deteriorating relative to other sites" = "Deteriorating", "no clear difference" = "No clear difference",
             "improving relative to other sites" = "Improving")
  t$direction <- factor(short[ifelse(t$direction == "", "no clear difference", t$direction)], levels = unname(short))
  ggplot2::ggplot(t, ggplot2::aes(trend_score, site, colour = direction)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = trend_score_low, xmax = trend_score_high), orientation = "y", width = 0, linewidth = 0.8) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_colour_manual(values = c("Deteriorating" = "#D55E00", "No clear difference" = "#999999",
                                            "Improving" = "#0072B2"), name = "Relative to the other sites:", drop = FALSE) +
    ggplot2::labs(x = "Trend score (+ = deteriorating relative to the other sites)", y = NULL,
                  caption = "Bars: 95% bootstrap intervals; a bar that does not cross zero shows a clear difference.\nSites without enough survey periods are not shown.") +
    ggplot2::theme_minimal(base_size = base_size) + ggplot2::theme(legend.position = "bottom")
}

#' Rate of change of each indicator at each site, coloured relative to the other sites.
#'
#' @param r Result of `run_analysis()`.
#' @param base_size Base font size.
#' @return A ggplot object, or `NULL`.
plot_trend_heatmap <- function(r, base_size = 13) {
  # rate of change per indicator, standardised across sites and signed so that
  # red = getting worse, blue = getting better
  tr <- r$trends
  if (is.null(tr) || is.null(tr$slopes) || !nrow(tr$slopes)) return(NULL)
  sl <- tr$slopes; tw <- tr$tw
  labs <- c(coral = "Coral cover", cca = "CCA", shannon = "Diversity (H')", turf = "Turf", macroalgae = "Macroalgae",
            cyanobacteria = "Cyanobacteria", bleached = "Bleaching", disease = "Disease", predation = "Predation")
  rows <- list()
  for (v in colnames(sl)) {
    sg <- tw$sign[match(v, tw$indicator)]
    z <- zscore(sl[, v]) * sg
    mult <- if (v %in% c(COVER_VARS, PREVALENCE_VARS)) 100 else 1
    rows[[v]] <- data.frame(site = rownames(sl), indicator = labs[v], worse = z,
                            label = ifelse(is.na(sl[, v]), "", formatC(sl[, v] * mult, format = "f", digits = if (mult == 100) 1 else 2)), row.names = NULL)
  }
  h <- do.call(rbind, rows)
  # bottom row: average change across the eligible sites (reef-wide change, which
  # the relative trend score does not show)
  avg_lab <- "Mean of these sites"
  mn <- colMeans(sl, na.rm = TRUE)
  h <- rbind(h, data.frame(site = avg_lab, indicator = labs[colnames(sl)], worse = NA_real_,
                           label = ifelse(colnames(sl) %in% c(COVER_VARS, PREVALENCE_VARS), sprintf("%.1f", mn * 100), sprintf("%.2f", mn)),
                           row.names = NULL))
  h$indicator <- factor(h$indicator, levels = labs[colnames(sl)])
  ord <- c(avg_lab, names(sort(tr$score)))
  h$site <- factor(h$site, levels = ord)
  ggplot2::ggplot(h, ggplot2::aes(indicator, site, fill = pmax(-2.5, pmin(2.5, worse)))) +
    ggplot2::geom_tile(colour = "white", linewidth = 1) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = base_size / 4.2, colour = "grey15") +
    ggplot2::scale_fill_gradient2(low = "#0072B2", mid = "#F2F2F2", high = "#D55E00", midpoint = 0, limits = c(-2.5, 2.5),
                                  na.value = "#D9D9D9", name = "Compared with\nthe other sites\n(red = worse,\nblue = better)") +
    ggplot2::labs(x = NULL, y = NULL,
                  caption = paste0("Numbers: change per year from survey-period means (percentage points for cover and prevalence;\n",
                                   "H' units for diversity). Colour: that change relative to the other sites. Grey row: mean of the sites,\n",
                                   "i.e. change across the reef. Sites ordered by trend score, most deteriorating at the top.")) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
                   plot.caption = ggplot2::element_text(hjust = 0), plot.caption.position = "plot")
}

#' Biplot of the benthic-composition PCA used for algal dominance.
#'
#' @param r Result of `run_analysis()`.
#' @param base_size Base font size.
#' @return A ggplot object, or `NULL`.
plot_pca <- function(r, base_size = 13) {
  p <- r$pca
  if (is.null(p$scores)) return(NULL)
  sc <- p$scores; ld <- p$loadings
  k <- max(abs(c(sc$PC1, sc$PC2))) / max(abs(c(ld$PC1, ld$PC2))) * 0.8
  lab <- if (requireNamespace("ggrepel", quietly = TRUE)) ggrepel::geom_text_repel else ggplot2::geom_text
  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, colour = "grey80") + ggplot2::geom_vline(xintercept = 0, colour = "grey80") +
    ggplot2::geom_segment(data = ld, ggplot2::aes(x = 0, y = 0, xend = PC1 * k, yend = PC2 * k),
                          colour = "#9A6B1F", arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"))) +
    ggplot2::geom_text(data = ld, ggplot2::aes(PC1 * k * 1.1, PC2 * k * 1.1, label = variable), colour = "#9A6B1F") +
    ggplot2::geom_point(data = sc, ggplot2::aes(PC1, PC2), size = 2.5) +
    lab(data = sc, ggplot2::aes(PC1, PC2, label = unit), size = base_size / 3.6) +
    ggplot2::labs(x = sprintf("PC1 (%.0f%%; higher = more algal-dominated)", 100 * p$var_exp[1]),
                  y = sprintf("PC2 (%.0f%%)", 100 * p$var_exp[2]), title = "Site means: benthic composition") +
    ggplot2::theme_minimal(base_size = base_size)
}

#' Plain-text formulas and settings for the run, shown in the app's Methods tab.
#'
#' @param r Result of `run_analysis()`.
#' @return Character vector.
methods_text <- function(r) {
  w <- r$weights_used
  eq <- function(g) {
    ww <- w[w$group == g & w$weight > 0, ]
    paste(sprintf("%s %s\u00b7z(%s)", ifelse(ww$sign > 0, "+", "\u2212"), format(ww$weight, trim = TRUE), ww$indicator), collapse = " ")
  }
  s <- r$settings
  c(sprintf("Feasibility = %s", eq("Feasibility")),
    sprintf("Urgency = %s", eq("Urgency")),
    sprintf("Donor score = %s", eq("Donor")),
    if (s$trend_used) sprintf("trend score = %s, where each slope is the change per year in survey-period means, standardised across the %d eligible sites (>= %d periods spanning >= %d days); other sites 0.",
                              eq("Trend"), r$trends$n_eligible, s$trend_min_periods, s$trend_min_days),
    sprintf("z = standardised across the %d sites in the snapshot (%s); missing values are set to 0 (the mean).", nrow(r$sites), r$snapshot$description),
    sprintf("Bootstrap: %d replicates, seed %d; transects resampled within each survey (site x date) and the full calculation (standardisation, PCA, indices, class, donor ranks%s) repeated.", s$B, s$seed, if (s$trend_used) ", trend slopes" else ""),
    sprintf("PCA variables: %s. App version %s; %s.", paste(PCA_SETS[[s$pca_set]], collapse = ", "), s$version, s$r_version))
}
