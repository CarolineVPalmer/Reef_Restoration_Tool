# =============================================================================
# indices.R -- Reef Restoration Prioritisation Tool
#
# Unit-level metrics, algal-dominance PCA, standardisation, composite indices and classes.
# Moved verbatim from analysis.R (v2.3.0); roxygen headers added.
# =============================================================================

# -----------------------------------------------------------------------------
# 5. Unit-level metrics (a unit is a site, or a site x period for trajectories)
# -----------------------------------------------------------------------------
#' Summarise transects into unit-level descriptors (a unit is a site, or a site x period).
#'
#' @param d Transect data.
#' @param unit Vector giving the unit of each row.
#' @param levels Units to return, in order.
#' @return Data frame with one row per unit: transect count, mean and CV of coral cover, and the means of the other indicators.
unit_metrics <- function(d, unit, levels = sort(unique(unit))) {
  f <- factor(unit, levels = levels)
  gm <- function(v) if (!v %in% names(d)) rep(NA_real_, nlevels(f)) else
    vapply(split(d[[v]], f), mean_or_na, numeric(1))
  cor_split <- split(d$coral, f)
  data.frame(
    unit            = levels,
    n_transects     = vapply(cor_split, function(v) sum(!is.na(v)), numeric(1)),
    mean_coral      = vapply(cor_split, mean_or_na, numeric(1)),
    cv_coral        = vapply(cor_split, function(v) { v <- v[!is.na(v)]
                        if (length(v) < 2 || mean(v) <= 0) NA_real_ else sd(v) / mean(v) }, numeric(1)),
    mean_shannon    = gm("shannon"),
    mean_bleached   = gm("bleached"),
    mean_disease    = gm("disease"),
    mean_predation  = gm("predation"),
    mean_sand       = gm("sand"),
    mean_rubble     = gm("rubble"),
    mean_rugosity   = gm("rugosity"),
    mean_herbivores = gm("herbivores"),
    mean_turf       = gm("turf"),
    mean_cca        = gm("cca"),
    mean_cyanobacteria = gm("cyanobacteria"),
    mean_macroalgae = gm("macroalgae"),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

#' Algal-dominance axis: first principal component of benthic composition across units.
#'
#' @param m Unit metrics.
#' @param vars Composition variables (`PCA_SETS`).
#' @return A list. PC1 is accepted only if coral loads opposite to at least one algal group, and is oriented so that higher
#'   values mean more algal-dominated; otherwise `ok = FALSE` and algal dominance contributes nothing.
pc1_scores <- function(m, vars) {
  # PCA on unit means (standardised variables). PC1 is used as 'algal dominance'
  # only if coral loads opposite to at least one algal group; it is oriented so
  # that higher values = more algal-dominated.
  res <- list(ok = FALSE, pc1 = rep(NA_real_, nrow(m)), note = "", loadings = NULL, scores = NULL, var_exp = NULL)
  cols <- paste0("mean_", vars)
  cols <- cols[cols %in% names(m)]
  X <- as.matrix(m[, cols, drop = FALSE])
  keep_col <- colSums(!is.na(X)) == nrow(X) | colMeans(!is.na(X)) >= 0.5
  X <- X[, keep_col, drop = FALSE]
  rows <- stats::complete.cases(X)
  X <- X[rows, , drop = FALSE]
  if (nrow(X) >= 2) X <- X[, apply(X, 2, function(x) sd(x) > 0), drop = FALSE]
  if (nrow(X) < 3 || ncol(X) < 3 || !"mean_coral" %in% colnames(X)) {
    res$note <- "PC1 not used: fewer than three sites with complete composition data, or too few varying variables."
    return(res)
  }
  p <- stats::prcomp(X, center = TRUE, scale. = TRUE)
  load <- p$rotation[, 1] * p$sdev[1]                 # correlations of variables with PC1
  flip <- if (load["mean_coral"] > 0) -1 else 1
  load <- load * flip
  sc <- p$x[, 1] * flip
  algal <- intersect(paste0("mean_", c("turf", "cyanobacteria", "macroalgae")), names(load))
  names(load) <- sub("^mean_", "", names(load))
  res$loadings <- data.frame(variable = names(load), PC1 = load,
                             PC2 = p$rotation[, 2] * p$sdev[2], row.names = NULL)
  res$scores <- data.frame(unit = m$unit[rows], PC1 = sc, PC2 = p$x[, 2], row.names = NULL)
  res$var_exp <- p$sdev^2 / sum(p$sdev^2)
  if (!any(load[sub("^mean_", "", algal)] > 0)) {
    res$note <- "PC1 rejected: coral does not load opposite to any algal group; algal dominance contributes nothing."
    return(res)
  }
  res$ok <- TRUE
  res$pc1[rows] <- sc
  res$note <- sprintf("PC1 accepted (%.0f%% of variance); oriented so that higher values = more algal-dominated.", 100 * res$var_exp[1])
  res
}

#' Signed weights (direction x magnitude) for one index.
#'
#' @param w Weights table.
#' @param group "Feasibility", "Urgency", "Donor" or "Trend".
#' @return Named numeric vector.
weight_vector <- function(w, group) {
  ww <- w[w$group == group, ]
  setNames(ww$sign * ww$weight, ww$indicator)
}

#' Standardise every scorable descriptor across units.
#'
#' @param m Unit metrics.
#' @return Matrix of z-scores with columns `Z_VARS`.
z_matrix <- function(m) {
  Z <- sapply(Z_VARS, function(v) if (v %in% names(m)) zscore(m[[v]]) else rep(0, nrow(m)))
  if (is.null(dim(Z))) Z <- matrix(Z, nrow = nrow(m), dimnames = list(NULL, Z_VARS))
  Z
}

#' Weighted sum of standardised descriptors (Eqs 2-4 in the manuscript).
#'
#' @param Z Matrix from `z_matrix()`.
#' @param wv Signed weights from `weight_vector()`.
#' @return Numeric vector of index values.
index_from_z <- function(Z, wv) as.vector(Z[, names(wv), drop = FALSE] %*% wv)

#' Assign management classes from the signs of Feasibility and Urgency.
#'
#' @param feas Feasibility scores.
#' @param urg Urgency scores.
#' @return Character vector of class names. Zero is the mean of the sites analysed, so classes are relative priorities;
#'   a score of exactly zero is assigned to the higher class.
classify <- function(feas, urg) {
  ifelse(feas >= 0 & urg >= 0, "Immediate restoration",
  ifelse(feas <  0 & urg >= 0, "Triage and foundational support",
  ifelse(feas >= 0 & urg <  0, "Proactive reinforcement", "Monitoring only")))
}

#' Point scores for a set of units: PCA, standardisation, indices and classes.
#'
#' @param m Unit metrics.
#' @param w Weights table.
#' @param pca_vars Composition variables for the PCA.
#' @param trend Raw trend score per unit, or `NULL`.
#' @return A list with the scored units (`m`), the z-score matrix (`Z`) and the PCA result (`pca`).
score_units <- function(m, w, pca_vars, trend = NULL) {
  # Point scores for a set of units. 'trend' = raw trend score per unit (or NULL).
  pca <- pc1_scores(m, pca_vars)
  m$pc1 <- if (pca$ok) pca$pc1 else NA_real_
  m$trend <- if (is.null(trend)) NA_real_ else trend
  Z <- z_matrix(m)
  m$feasibility <- index_from_z(Z, weight_vector(w, "Feasibility"))
  m$urgency     <- index_from_z(Z, weight_vector(w, "Urgency"))
  m$donor_score <- index_from_z(Z, weight_vector(w, "Donor"))
  m$class <- classify(m$feasibility, m$urgency)
  list(m = m, Z = Z, pca = pca)
}
