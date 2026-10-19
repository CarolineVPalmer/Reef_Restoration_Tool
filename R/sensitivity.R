# =============================================================================
# sensitivity.R -- Reef Restoration Prioritisation Tool
#
# Weight sensitivity (point estimates; indicator z-scores held fixed).
# Moved verbatim from analysis.R (v2.3.0); roxygen headers added.
# =============================================================================

# -----------------------------------------------------------------------------
# 9. Weight sensitivity (point estimates; indicator z-scores held fixed)
# -----------------------------------------------------------------------------
#' Sensitivity of the classes to the weights, with indicator values held fixed.
#'
#' @param Z z-score matrix of the snapshot sites.
#' @param base_class Classes under the weights in use.
#' @param w Weights in use.
#' @param s Settings list.
#' @param trend_used Logical.
#' @param sites Site names.
#' @return A list: stability per site (one weight at a time, and all weights varied together), changes per scenario
#'   (including an equal-weights benchmark, which is not counted in the stability figures),
#'   stability under random weights that keep the order of the default weights,
#'   and classes under every reef context and the Minimal overlap scenario.
weight_sensitivity <- function(Z, base_class, w, s, trend_used = FALSE, sites = seq_along(base_class)) {
  fu <- w[w$group %in% c("Feasibility", "Urgency") & w$weight > 0, ]
  cls_for <- function(ww) classify(index_from_z(Z, weight_vector(ww, "Feasibility")),
                                   index_from_z(Z, weight_vector(ww, "Urgency")))
  scen <- list()
  for (i in seq_len(nrow(fu))) for (fct in c(1 - s$sens_factor, 1 + s$sens_factor)) {
    ww <- w; ww$weight[ww$id == fu$id[i]] <- ww$weight[ww$id == fu$id[i]] * fct
    scen[[sprintf("%s (%s) x%.1f", fu$label[i], fu$group[i], fct)]] <- ww
  }
  mo <- w; ch <- SENSITIVITY_SCENARIOS[["Minimal overlap"]]$changes; mo$weight[match(names(ch), mo$id)] <- 0
  scen[["Minimal overlap (shared indicators removed)"]] <- mo
  cls <- sapply(scen, cls_for)
  if (is.null(dim(cls))) cls <- matrix(cls, nrow = length(base_class))
  by_scenario <- data.frame(scenario = names(scen), sites_changed = colSums(cls != base_class),
                            sites_changed_names = apply(cls != base_class, 2, function(x) paste(sites[x], collapse = ", ")),
                            stringsAsFactors = FALSE, row.names = NULL)

  # global: all Feasibility and Urgency weights varied together, each x U(1-f, 1+f)
  set.seed(s$seed + 1)
  G <- s$global_draws
  gcls <- matrix(NA_character_, length(base_class), G)
  for (g in seq_len(G)) {
    ww <- w
    sel <- ww$group %in% c("Feasibility", "Urgency")
    ww$weight[sel] <- ww$weight[sel] * stats::runif(sum(sel), 1 - s$sens_factor, 1 + s$sens_factor)
    gcls[, g] <- cls_for(ww)
  }

  # Rank-preserving weights (v2.4.0). The literature supports the ORDER of the
  # indicators (e.g. coral cover above patchiness) more firmly than their exact
  # magnitudes, which are judgement. Each draw gives the Feasibility and the
  # Urgency indicators in use new random weights that keep the order of the
  # default weights; indicators with equal default weights may swap order.
  # Weights are drawn as sorted uniform values, as in stochastic multicriteria
  # acceptability analysis with ordinal information (Lahdelma et al. 1998),
  # and rescaled to the default total (which does not affect the classes).
  # A separate seed keeps the global results above unchanged.
  rank_draw <- function(ww) {
    for (grp in c("Feasibility", "Urgency")) {
      k <- which(ww$group == grp & ww$weight > 0)
      if (length(k) < 2) next
      v <- sort(stats::runif(length(k)), decreasing = TRUE)
      o <- order(-ww$weight[k], stats::runif(length(k)))   # ties broken at random
      new <- numeric(length(k))
      new[o] <- v
      ww$weight[k] <- new * sum(ww$weight[k]) / sum(new)
    }
    ww
  }
  set.seed(s$seed + 2)
  rcls <- matrix(NA_character_, length(base_class), G)
  for (g in seq_len(G)) rcls[, g] <- cls_for(rank_draw(w))
  # classes under each preset
  all_sets <- c(PRESETS, SENSITIVITY_SCENARIOS)
  pres <- sapply(names(all_sets), function(p) {
    # preset changes applied to the weights in use; indicators given no weight
    # (e.g. not recorded) stay at zero
    ww <- w; ch <- all_sets[[p]]$changes
    if (length(ch)) { hit <- match(names(ch), ww$id); ww$weight[hit] <- ifelse(ww$weight[hit] > 0, unname(ch), 0) }
    cls_for(ww)
  })
  if (is.null(dim(pres))) pres <- matrix(pres, nrow = length(base_class), dimnames = list(NULL, names(all_sets)))
  pres <- data.frame(site = sites, pres, check.names = FALSE, stringsAsFactors = FALSE)

  # Equal weights (v2.4.0): every Feasibility and Urgency indicator in use gets
  # weight 1. This is a benchmark for whether the expert-assigned magnitudes
  # matter at all: if classes hold, they are driven by the data rather than by
  # the weights. Indicators switched off or not recorded stay at zero.
  eq <- w
  eq_sel <- eq$group %in% c("Feasibility", "Urgency") & eq$weight > 0
  eq$weight[eq_sel] <- 1
  eq_cls <- cls_for(eq)
  pres[["Equal weights (benchmark)"]] <- eq_cls
  by_scenario <- rbind(by_scenario, data.frame(
    scenario = "Equal weights (benchmark)", sites_changed = sum(eq_cls != base_class),
    sites_changed_names = paste(sites[eq_cls != base_class], collapse = ", "), stringsAsFactors = FALSE))

  alt <- function(k, M) { a <- M[k, M[k, ] != base_class[k]]
    if (length(a) == 0) "None" else names(sort(table(a), decreasing = TRUE))[1] }
  by_site <- data.frame(site = sites, class = base_class,
                        stability_one_at_a_time = rowMeans(cls == base_class),
                        stability_global = rowMeans(gcls == base_class),
                        stability_rank_preserving = rowMeans(rcls == base_class),
                        most_common_alternative = vapply(seq_along(base_class), alt, character(1), M = cbind(cls, gcls)),
                        stringsAsFactors = FALSE)
  list(by_site = by_site, by_scenario = by_scenario, presets = pres,
       n_scenarios = ncol(cls), n_global = G, factor = s$sens_factor)
}
