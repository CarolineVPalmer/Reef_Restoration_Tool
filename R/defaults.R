# =============================================================================
# defaults.R -- Reef Restoration Prioritisation Tool
#
# Defaults: settings, weights, presets, alert thresholds, classes and recognised variables.
# These objects are the single source of truth for every score; see
# docs/design_decisions_v2.3.0.md and Default_weights_justification.docx.
# Moved verbatim from analysis.R (v2.3.0); roxygen headers added.
# =============================================================================

APP_VERSION <- "2.5.2"

# -----------------------------------------------------------------------------
# 1. Defaults
# -----------------------------------------------------------------------------

# Analysis settings. Dates are NA = "use everything available".
DEFAULT_SETTINGS <- list(
  day_first          = TRUE,     # dates such as 05/01/2025 read as 5 January
  data_from          = NA,       # exclude surveys before this date (e.g. "2024-04-01")
  data_to            = NA,       # exclude surveys after this date
  snapshot_mode      = "period", # most recent survey "period"; "latest" survey per site; a survey "window"; or "all" data pooled
  window_from        = NA,
  window_to          = NA,
  min_transects      = 3,        # sites with fewer transects in the snapshot are flagged. Three is the
                                 # usual effort of NGO and agency teams; more transects give more confident
                                 # classes (bootstrap class support shows how confident). Was 5 before v2.4.0.
  top_n              = 5,        # length of the donor shortlist
  B                  = 200,      # bootstrap replicates
  seed               = 123,
  pca_set            = "composition", # or "extended" (adds bleaching, disease, predation, as in v1)
  include_trends     = TRUE,     # used only for sites that meet the two rules below
  period_months      = 3,        # survey period length (snapshot, trajectories and trends)
  period_start_month = 3,        # 3 = periods Mar-May, Jun-Aug, Sep-Nov, Dec-Feb
  trend_min_periods  = 3,        # trend used only for sites with >= this many periods ...
  trend_min_days     = 365,      # ... spanning at least this many days
  sens_factor        = 0.5,      # one-at-a-time sensitivity: each weight x (1 -/+ factor)
  global_draws       = 1000,     # global sensitivity: random weight sets
  # Optional bleaching-event mode (v2.5.0; see R/event.R). Not used by the
  # standard analysis. Each phase is used only when switched on. With "before",
  # the comparison works while an event is under way; without it (v2.5.2), change
  # is measured from "during" to "after", e.g. where monitoring began only once
  # bleaching was visible.
  event_use_before   = TRUE,
  event_before_from  = NA,
  event_before_to    = NA,
  event_use_during   = FALSE,
  event_during_from  = NA,
  event_during_to    = NA,
  event_use_after    = FALSE,
  event_after_from   = NA,
  event_after_to     = NA
)

# Weights: the single source of truth for every score.
# 'sign' is fixed (ecological direction); users change only 'weight' (>= 0).
# Indicators with a default weight of 0 are optional: they are used only if the
# column is present in the data and the user gives them a weight.
# Justification for every weight: Default_weights_justification.docx
# v2.4.0: crustose coralline algae (CCA) added to Feasibility (+, weight 1,
# range 0-3). CCA induces coral settlement and supports post-settlement
# survival, so a reef with more CCA has better recovery potential than one with
# the same coral cover but more turf. Weight 1 mirrors turf (-1). At Isla del
# Caño CCA is nearly independent of coral cover (site-survey rho = -0.02), so it
# adds information the other indicators do not carry.
DEFAULT_WEIGHTS <- data.frame(
  id = c("f_coral", "f_shannon", "f_cv", "f_turf", "f_cca", "f_rubble", "f_disease", "f_bleached", "f_pred", "f_rugosity",
         "u_bleached", "u_disease", "u_pred", "u_cv", "u_coral", "u_pc1", "u_herbivores", "u_trend",
         "d_coral", "d_shannon", "d_bleached", "d_disease", "d_cv",
         "t_coral", "t_cca", "t_shannon", "t_turf", "t_macro", "t_cyano", "t_bleached", "t_disease", "t_pred"),
  group = c(rep("Feasibility", 10), rep("Urgency", 8), rep("Donor", 5), rep("Trend", 9)),
  indicator = c("mean_coral", "mean_shannon", "cv_coral", "mean_turf", "mean_cca", "mean_rubble", "mean_disease", "mean_bleached", "mean_predation", "mean_rugosity",
                "mean_bleached", "mean_disease", "mean_predation", "cv_coral", "mean_coral", "pc1", "mean_herbivores", "trend",
                "mean_coral", "mean_shannon", "mean_bleached", "mean_disease", "cv_coral",
                "coral", "cca", "shannon", "turf", "macroalgae", "cyanobacteria", "bleached", "disease", "predation"),
  sign   = c( 1,  1, -1, -1,  1, -1, -1, -1, -1,  1,
              1,  1,  1,  1, -1,  1, -1,  1,
              1,  1, -1, -1, -1,
             -1, -1, -1,  1,  1,  1,  1,  1,  1),
  weight = c(3, 2, 1, 1, 1, 1, 0.5, 0.5, 0.5, 0,
             2, 2, 1, 1, 1, 1.5, 0, 1,
             3, 2, 2, 2, 1,
             4, 2, 2, 2, 2, 3, 2, 2, 2),
  min    = 0,
  max    = c(6, 4, 3, 3, 3, 3, 3, 3, 3, 3,
             4, 4, 3, 3, 3, 3, 3, 4,
             6, 4, 4, 4, 3,
             6, 4, 4, 4, 4, 4, 4, 4, 4),
  label  = c("Live coral cover", "Coral diversity (H')", "Coral patchiness (CV)", "Turf algae",
             "Crustose coralline algae (CCA)", "Rubble",
             "Disease", "Bleaching", "Predation (colonies with scars)", "Rugosity (optional)",
             "Bleaching", "Disease", "Predation (colonies with scars)", "Coral patchiness (CV)", "Live coral cover",
             "Algal dominance (PC1: turf/algae vs coral)", "Herbivores (optional)", "Trend score (if trends used)",
             "Live coral cover", "Coral diversity (H')", "Bleaching", "Disease", "Coral patchiness (CV)",
             "Coral cover trend", "CCA trend", "Diversity trend", "Turf trend", "Macroalgae trend",
             "Cyanobacteria trend", "Bleaching trend", "Disease trend", "Predation trend"),
  stringsAsFactors = FALSE
)

# Presets (starting points; changes relative to the general default), each with a one-line reason
PRESETS <- list(
  "General (default)" = list(changes = c(),
    why = "Literature-based weights for any reef with routine benthic transects (see Default_weights_justification.docx)."),
  "Low-diversity or marginal reef" = list(changes = c(f_shannon = 1, d_shannon = 1),
    why = "Where few coral taxa occur (e.g. Eastern Tropical Pacific, high-latitude reefs), diversity reflects community type more than condition, so it counts less."),
  "Disease outbreak" = list(changes = c(f_disease = 1.5, u_disease = 3, d_disease = 3),
    why = "Gives disease more influence during an outbreak (e.g. stony coral tissue loss disease, white syndromes)."),
  "Algae or sediment dominated" = list(changes = c(f_turf = 2, u_pc1 = 2.5),
    why = "Where turf, sediment-laden turf or macroalgae limit recovery (e.g. runoff, landslides, overfishing of herbivores)."),
  "Marine heatwave (during or after)" = list(changes = c(u_bleached = 3, d_bleached = 3),
    why = "Bleaching counts more as a pressure and in donor choice (sites where corals resist bleaching are the best source of heat-tolerant stock); its weight in Feasibility is not raised because some bleaching is temporary.")
)

# Scenario used only in the weight-sensitivity analysis (not offered as a preset)
SENSITIVITY_SCENARIOS <- list(
  "Minimal overlap" = list(changes = c(u_coral = 0, u_cv = 0, u_pc1 = 0, f_bleached = 0, f_disease = 0, f_pred = 0),
    why = "Removes shared indicators so Feasibility reflects condition only and Urgency pressure only.")
)

#' Apply a reef-context preset or sensitivity scenario to a weights table.
#'
#' @param name Name of an element of `PRESETS` or `SENSITIVITY_SCENARIOS`.
#' @param base Weights table to modify (default `DEFAULT_WEIGHTS`).
#' @return The weights table with the preset's weights substituted. Directions are never changed.
apply_preset <- function(name, base = DEFAULT_WEIGHTS) {
  ch <- c(PRESETS, SENSITIVITY_SCENARIOS)[[name]]$changes
  if (length(ch)) base$weight[match(names(ch), base$id)] <- unname(ch)
  base
}

# Absolute alerts shown beside the relative classes (site means in the snapshot).
# Relative scores cannot show when every site is degraded; these can.
# Coral < 10%: collapse level (Darling et al. 2019). Bleaching > 50% of colonies: 'severe'
# in the ReefBase severity scale (Virgen-Urcelay & Donner 2023). Rubble > 30%: rule of thumb.
FLAG_RULES <- data.frame(
  variable  = c("mean_coral", "mean_bleached", "mean_rubble"),
  above     = c(FALSE, TRUE, TRUE),
  threshold = c(0.10, 0.50, 0.30),
  short     = c("coral < 10%", "bleaching > 50%", "rubble > 30%"),
  meaning   = c("Coral cover below 10%: near-collapse level; natural recovery unlikely and not a donor source.",
                "Severe bleaching (> 50% of colonies; ReefBase category): act on local stressors, protect nurseries, hold outplanting.",
                "Extensive rubble (> 30% cover): stabilise the substrate before outplanting."),
  stringsAsFactors = FALSE
)

CLASSES <- data.frame(
  class = c("Immediate restoration", "Triage and foundational support", "Proactive reinforcement", "Monitoring only"),
  pathway = c("A", "B", "C", "D"),
  recommendation = c(
    "Relatively good condition under relatively high pressure: carry out a detailed site-suitability assessment, then active restoration where conditions allow.",
    "Poorer condition under relatively high pressure: identify and manage limiting pressures first (e.g. substrate stability, herbivory, local threats) before outplanting.",
    "Relatively good condition under lower pressure: candidate for nurseries, donor material and reinforcement.",
    "Poorer condition under lower pressure: allow natural recovery and re-survey; reassess if pressure increases."),
  stringsAsFactors = FALSE
)
# Okabe-Ito colours, distinguishable with the common forms of colour blindness
# (v2.4.0; previously a red-green pair separated classes A and B). Class
# letters (A-D) are shown in the decision-plane legend; points could also be
# given class-specific shapes if colour alone proves insufficient.
CLASS_COLOURS <- c("Immediate restoration" = "#009E73", "Triage and foundational support" = "#D55E00",
                   "Proactive reinforcement" = "#0072B2", "Monitoring only" = "#999999")

# Input variables recognised by name (case-insensitive). All cover and
# prevalence variables are proportions (0-1); 0-100 is converted automatically.
COVER_VARS      <- c("coral", "turf", "cca", "cyanobacteria", "macroalgae", "rubble", "sand")
PREVALENCE_VARS <- c("bleached", "disease", "predation")   # proportion of coral colonies affected
OTHER_NUM_VARS  <- c("rugosity", "herbivores", "depth")    # any units (they are standardised)
COLUMN_ALIASES  <- c(diseased = "disease", predators = "predation", predator = "predation",
                     bleaching = "bleached", cyano = "cyanobacteria", crustose_coralline_algae = "cca",
                     herbivore = "herbivores", herbivory = "herbivores")
# Variables for the algal-dominance PCA. Crustose coralline algae (CCA) are left
# out (v2.4.0): CCA facilitates coral settlement, so it is not part of algal
# dominance, and in v2.3.0 it loaded with the algae and raised Urgency. CCA now
# enters Feasibility directly, with a positive sign (see DEFAULT_WEIGHTS).
# The "extended" set reproduces version 1 and is kept unchanged for comparison.
PCA_SETS <- list(composition = c("coral", "turf", "cyanobacteria", "macroalgae"),
                 extended    = c("coral", "turf", "cca", "cyanobacteria", "macroalgae", "bleached", "disease", "predation"))

# Site-level quantities that can be standardised and weighted
Z_VARS <- c("mean_coral", "cv_coral", "mean_shannon", "mean_turf", "mean_cca", "mean_bleached", "mean_disease", "mean_predation",
            "pc1", "mean_rubble", "mean_rugosity", "mean_herbivores", "trend")

MONTHS_ES <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto",
               "septiembre", "octubre", "noviembre", "diciembre")
