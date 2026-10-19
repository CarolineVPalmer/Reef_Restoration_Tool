# =============================================================================
# event_case_study_isla_del_cano.R -- post hoc test of the bleaching-event mode
#
# Runs the event mode on the 2023-24 El Nino bleaching at Isla del Cano and
# compares it with Palmer et al. (2025, PeerJ 13:e20088), using the same
# periods: "during bleaching" (March-August 2024) and "after disturbance"
# (December 2024 - February 2025). Most sites have no surveys before the event,
# so "during" is the reference and losses before the first survey are missed.
# This is a consistency check on the same surveys, not an independent test.
#
# Run from the project folder:  source("scripts/event_case_study_isla_del_cano.R")
# Writes to outputs/event_case_study/.
# =============================================================================

stopifnot(file.exists("app.R"), dir.exists("R"))
for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f)

data_file <- file.path("data", "case_study", "prevsub2026a_clean.csv")
if (!file.exists(data_file)) data_file <- file.path("data", "case_study", "prevsub2026a.csv")
out_dir <- file.path("outputs", "event_case_study")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out <- function(name) file.path(out_dir, name)

prep <- prepare_data(read_transect_file(data_file), day_first = TRUE)
settings <- list(
  event_use_during = TRUE, event_during_from = "2024-03-01", event_during_to = "2024-08-31",
  event_use_after = TRUE, event_after_from = "2024-12-01", event_after_to = "2025-02-28"
)
phases <- event_phases_from_settings(settings)
ev <- event_comparison(prep$df, phases, B = 1000, seed = 123)

utils::write.csv(ev$sites, out("event_sites.csv"), row.names = FALSE)
utils::write.csv(ev$reef, out("event_reef.csv"), row.names = FALSE)
writeLines(ev$notes, out("event_notes.txt"))
p <- plot_event_change(ev, base_size = 12)
if (!is.null(p)) ggplot2::ggsave(out("event_coral_change.png"), p, width = 8, height = 6, dpi = 300, bg = "white")

# Comparison with Palmer et al. (2025): mean cover across the nine sites they
# analysed, in each phase, and the relative change. Values reported in that
# paper: coral 19.3% to 11.5% (about -40%); turf about +71%.
palmer_sites <- c("Ancla", "Barco Profundo", "Barco Somero", "Chorro", "Cueva", "Esquina",
                  "Este Intermedio", "San Josecito", "Tina")
d <- prep$df[!is.na(prep$df$coral) & !is.na(prep$df$date), ]
d$phase <- assign_event_phase(d, phases)
d <- d[!is.na(d$phase) & d$site %in% palmer_sites, ]
both <- intersect(unique(d$site[d$phase == "during"]), unique(d$site[d$phase == "after"]))
cmp <- do.call(rbind, lapply(c("coral", "turf", "cca", "bleached"), function(v) {
  if (!v %in% names(d)) return(NULL)
  m <- tapply(d[[v]], list(d$site, d$phase), mean, na.rm = TRUE)[both, c("during", "after"), drop = FALSE]
  data.frame(indicator = v, sites = length(both),
             during_pct = 100 * mean(m[, "during"], na.rm = TRUE),
             after_pct = 100 * mean(m[, "after"], na.rm = TRUE),
             relative_change_pct = 100 * (mean(m[, "after"], na.rm = TRUE) / mean(m[, "during"], na.rm = TRUE) - 1))
}))
cmp$palmer_2025 <- c(coral = "19.3% to 11.5% (about -40%)", turf = "about +71%", cca = "", bleached = "")[cmp$indicator]
utils::write.csv(cmp, out("palmer2025_comparison.csv"), row.names = FALSE)
writeLines(c(sprintf("Sites in both phases (of Palmer et al.'s nine): %s", paste(sort(both), collapse = ", ")),
             utils::capture.output(print(cmp, row.names = FALSE)),
             "", utils::capture.output(print(ev$sites[, c("site", "transects_reference", "compared_with",
                                                          "coral_reference_pct", "coral_latest_pct",
                                                          "coral_change_pp", "coral_change_low",
                                                          "coral_change_high", "change")], row.names = FALSE)),
             "", utils::capture.output(print(ev$reef, row.names = FALSE))),
           out("summary.txt"))
cat(readLines(out("summary.txt")), sep = "\n")
