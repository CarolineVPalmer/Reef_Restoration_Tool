# Changes

## Version 2.5.1 (in development)

- A "Running…" message now appears as soon as Run analysis (or Compare) is clicked and stays until the results are ready. In the browser version the page cannot update during the calculation, so the progress bar alone looked as if nothing was happening.
- **Bug fix:** clicking Run analysis on the Start page did nothing until another tab was opened, because the switch to the Overview happened inside the (lazily computed) analysis. The app now switches to the Overview on click, which starts the analysis.
- Event mode works without surveys before the event: "during" becomes the reference and the results say that losses before the first survey are missed (approved 19 Oct 2026). Many teams start surveying only once bleaching is under way.
- `scripts/event_case_study_isla_del_cano.R`: post hoc test of the event mode on the 2023–24 event, using the periods of Palmer et al. (2025).

## Version 2.5.0 (in development)

- **Optional bleaching-event mode** (new tab "Bleaching event (optional)"; `R/event.R`). Users give the dates of surveys before the event (required) and during and/or after it; the tool reports, per site, bleaching during the event and the change in coral, turf and CCA cover since before the event, with 95% bootstrap intervals (transects resampled within surveys), and the change across the reef. It works with "before" plus either later phase, so it can be used while an event is under way. It describes observed change and does not forecast coral loss. It uses all dates in the file, is run separately and does not change the standard analysis (tested). Event dates are saved in the settings file.

## Version 2.4.0 (19 October 2026)

Changes to defaults agreed on 19 October 2026. Results differ from 2.3.0, so the case study must be re-run.

- **Crustose coralline algae (CCA) moved from the algal-dominance PCA into Feasibility.** CCA facilitates coral settlement and is not part of algal dominance; the PC1 acceptance rule already treated only turf, cyanobacteria and macroalgae as algae. In the case-study data CCA had loaded with the algae and raised Urgency. The `extended` (version 1) set is unchanged. CCA now counts in Feasibility as a positive indicator (weight 1, range 0–3): it induces coral settlement and supports recruit survival, so it indicates recovery potential that coral cover alone does not show; at Isla del Caño it is nearly independent of coral cover. CCA cover is shown in the site table.
- **Minimum-transect flag lowered from 5 to 3**, the usual effort of NGO and agency teams. Classes from three transects are less certain, which the bootstrap class support reports; help text recommends five or six where possible.
- **Equal-weights benchmark** added to the weight-sensitivity output (classes with every Feasibility and Urgency indicator in use weighted 1). Reported beside the reef contexts; not counted in the stability figures.
- **Rank-preserving weight sensitivity**: random weights that keep the order of the default weights (1,000 draws), reported as a third stability measure. Tests whether classes depend on the exact magnitudes or only on the ordering of indicators.
- **Colour-blind-safe palette** (Okabe–Ito) for classes and trend plots; previously classes A and B were green and red.
- **Bug fix:** dates with month names (English or Spanish) failed on computers set to a non-English language, because `%B` follows the computer's locale. Month names are now parsed in the C locale.
- `geom_errorbarh()` (deprecated in ggplot2 4.0) replaced by `geom_errorbar(orientation = "y")`.
- Tests for the PCA variable set and the equal-weights benchmark.
- Automatic check on GitHub (`.github/workflows/R-check.yml`): parses every file, runs the unit tests and the example analysis on each push.
- Case-study check: runs in the private data repository (Restoration-Tool-Data), which holds a synced copy of this code, so the monitoring data and derived results never enter this repository and no access key is needed.

## Repository set-up (19 October 2026; calculations as 2.3.0)

No change to any calculation, default, weight or threshold: results should be identical to v2.3.0.

- `analysis.R` split into one file per stage in `R/`, moved verbatim (checked line by line), with roxygen headers added to all 40 functions.
- `app.R` now loads `R/` and reads the example file from `data/`.
- Added `scripts/reproduce_case_study.R`, which re-runs the case study and compares it with the reference outputs of 30 September 2026. It replaces the original script, which was not found, and has not yet been run.
- The 2.3.0 test file (`tests/test_analysis.R`, 72 checks) and the exploratory scripts `transect_effort_check.R` and `hindcast_check.R` listed below were not found; the tests in `tests/testthat/` are new.
- Added unit tests (`tests/`), `README.md`, `LICENSE`, `CITATION.cff`, `DESCRIPTION` (for renv) and `.gitignore`.
- Design notes moved to `docs/`.
- `scripts/legacy/compare_v1_v2.R` (tool v2.2.0 alongside version 1) kept for provenance; it produced `outputs/reference_v2.3.0/comparison_v1_v2.csv`.

## Version 2.3.0 (30 September 2026): release for publication

The folder contained version 2.1.0 of the app. The data period, snapshot, trajectories and
trend-eligibility features described for version 2.2 in the handoff notes were not in that file;
they are implemented here, together with the fixes below.

### Bugs found in 2.1.0 (all fixed)

| # | Problem | Effect on `prevsub2026.csv` | Fix |
|---|---|---|---|
| 1 | The byte-order mark at the start of the file made the first column `﻿date`, so the `date` column was not recognised. | All dates came from `year` + `month` (first of the month); "Abril" failed. | File read with BOM removed; names cleaned of BOMs and non-breaking spaces. |
| 2 | Spreadsheet padding rows with site = `0` (439 rows, dated 1900) were treated as a real site. | A fake site "0" (coral 3%) was scored and shifted every z-score. | Rows without a valid site name (blank, `NA`, `0`) are dropped and counted. |
| 3 | No data period or snapshot: all surveys since 2019 were pooled. | Pre-bleaching surveys mixed with 2024–26 data; within-site CV included change over time. | Data period and snapshot (latest / window / all) added. |
| 4 | A site with no bleaching, disease or predation records got a mean of **0** (i.e. "no stress"). | Missing data scored as the best possible value. | Missing values stay missing and are set to the across-site mean (neutral) after standardisation, and are listed per site. |
| 5 | CV of coral cover was set to 0 for sites with one transect. | One transect scored as perfectly uniform (favourable). | CV missing (neutral) when fewer than 2 transects. |
| 6 | `n_transects` counted rows even when coral cover was missing. | Transect numbers overstated. | Only rows with coral cover are counted. |
| 7 | Spanish month names were not recognised. | Undated rows. | English and Spanish month names accepted. |
| 8 | Donor scores always used the latest survey, even when classes used another period. | Donors and classes came from different data. | Donors use the same snapshot as the classes. |
| 9 | Caulerpa (a macroalga) was recorded in its own column and ignored. | Algal cover understated at sites with Caulerpa. | A `caulerpa` column is added to macroalgae (reported in the notes). |
| 10 | Diversity missing at most sites (e.g. `Tot_*` columns empty from June 2025) was imputed as average for all of them. | Diversity silently dropped or driven by one site. | Indicators recorded at half of the sites or fewer are given no weight, with a note. |
| 11 | The trend score was standardised to SD 1 and added to an Urgency score with SD of about 3–5. | The trend's influence was fixed and undocumented. | Explicit weight `u_trend` (default 1) in the Urgency weights. |

### Methodological changes

- **Joint bootstrap.** Version 2.1 resampled one site at a time against fixed means and SDs, so
  intervals ignored uncertainty in the zero lines, and PC1 was held fixed. Now transects are
  resampled within all sites at once and the whole calculation is repeated (standardisation, PCA,
  indices, class, donor rank). This propagates PC1 uncertainty (a limitation listed in the outline).
- **Class support**: % of bootstrap replicates that give the site its reported class.
- **Donor rank intervals** and **top-K support**.
- **Global weight sensitivity**: all Feasibility and Urgency weights varied together (each × U(0.5, 1.5),
  1000 draws), alongside the one-at-a-time ±50% scenarios; classes under each preset are also reported.
- **Trend adjustment**: slopes of survey-period means per year; only for sites
  with ≥ 3 periods over ≥ 365 days; needs ≥ 3 eligible sites; others get a neutral trend.
- **Trajectories**: site × period scores standardised across all site-periods; indicators missing in
  more than 20% of site-periods are left out so gaps in recording do not look like change.
- **Optional indicators** (default weight 0): rubble (Feasibility −), rugosity (Feasibility +),
  herbivores (Urgency −), for the limiting factors discussed in the paper.
- **PCA** uses base R `prcomp` on standardised site means (numerically the same as FactoMineR);
  vegan, FactoMineR, factoextra, dplyr and tidyr are no longer needed. Only shiny and ggplot2 are required.

### Updates after review (30 September 2026, same version)

- **Default snapshot = most recent survey period** (3-month periods by default), so sites are always
  compared at the same time. Other options: survey window, most recent survey per site, all data.
- **Depth filter removed.** Depth is a site characteristic, not condition or pressure; it is shown
  beside the results (site scores, recommendations, donors) instead.
- **Trends on by default and redesigned:** slopes of survey-period means (each period counts once),
  computed in closed form; trend slopes are re-estimated in every bootstrap replicate (transects
  resampled within each survey), giving intervals for the trend score and the coral-cover rate of
  change; results in natural units (percentage points per year); a reef-wide summary (mean slope
  across sites) because the trend score is relative; new Trends tab with a trend-score plot and an
  indicator-by-site heat map; trend download.
- Bootstrap now stratified by survey (site × date) over the whole data period.
- **Turf algae added to Feasibility** (sign −, weight 1, range 0–3): turf limits coral settlement, recruit
  and outplant survival and is the dominant post-bleaching state at Isla del Caño. PC1 relabelled
  "Algal dominance (PC1: turf/algae vs coral)". The Minimal overlap preset now also sets PC1 to zero in
  Urgency, so that turf counts only once (as a condition) in that scenario. Turf is shown in the site table.

### Updates after review 2 (30 September 2026)

- **Predation** is the proportion of coral colonies with predation scars. The internal column is now
  `predation` (the old name `predators` is still accepted), and predation now also enters **Urgency**
  (weight 1), like bleaching and disease, which are also proportions of colonies affected.
- **Rubble** now counts in Feasibility by default (sign −, weight 1): it is recorded in most benthic
  protocols, is nearly independent of coral and turf, and unstable rubble kills recruits and outplants.
- **Sand** is read and shown as a site characteristic (not scored), like depth.
- **Presets reduced to five reef contexts:** General (default), Low-diversity or marginal reef, Disease
  outbreak, Algae or sediment dominated, Marine heatwave (during or after). "Transient algae" removed;
  "Minimal overlap" kept only as a sensitivity scenario.
- **Absolute alerts** beside the relative classes: coral < 10%, bleaching > 50% of colonies,
  rubble > 30%; shown in the Overview, Recommendations, site and donor tables.
- **Simpler interface:** basic sidebar (data, surveys, reef context, run); weights, trend settings,
  options and extra downloads under "Show advanced settings"; specialist tabs under "More".

### Interface and reproducibility

- `app.R` (interface) and `analysis.R` (calculations) are separate, so the paper's results come from
  `reproduce_case_study.R` using exactly the code in the app.
- Settings file now stores every option (data period, snapshot, depth filter, bootstrap, trends) as
  well as the weights, and can be loaded back into the app. Old weights-only files still load.
- Every download starts with a header (version, time, R version, snapshot, weights, seed).
- Separate downloads: site results, donors, sensitivity, trajectories, settings, decision-plane PNG.
- Semicolon-separated files with decimal commas are read (common in Latin American Excel exports).
- Unit tests (`tests/test_analysis.R`, 72 checks, including recovery of a known rate of change, alerts and presets) and a synthetic example covering all four classes.

### Update 3 (30 September 2026)

- Optional `method` column (e.g. transect, photogrammetry): the tool warns when methods differ between
  sites in the snapshot or change over time at a site. Scores are unchanged. README section on using
  photogrammetry data. Unit tests: 72 checks.
- Exploratory checks added as supplementary scripts: `transect_effort_check.R` and `hindcast_check.R`.

- Severe-bleaching alert threshold changed from > 30% to > 50% of colonies, the "severe" category of
  the ReefBase severity scale used in the global bleaching database (Virgen-Urcelay & Donner 2023). At
  Isla del Caño case study, note that "bleached" in the
  Isla del Caño data includes pale colonies (P + PB + MB + B).

## Version 2.0–2.1 (from version 1, 0525)

Fixed in 2.0 and still in place: bootstrap diversity from the coral taxa rather than benthic
columns; a single PCA on site means with correct orientation; no crash when PC1 is rejected;
duplicate predator trend removed; element-wise date parsing; one weights table used everywhere;
analysis in one reactive; PCA biplot with site names; British spelling; adjustable weights with fixed
directions, presets, save/load settings, weight-sensitivity tab, minimum-transect flag, borderline
flag, overview tab, methods tab; class letters follow the manuscript (A Immediate, B Triage,
C Proactive, D Monitoring); PCA on composition only by default (option to add bleaching, disease
and predation as in version 1).

## 2.0.0 (version 1 "0525" to version 2)

Original changelog, restored 19 October 2026. Headings demoted one level; text unchanged.

Files: `app.R` (interface), `analysis.R` (all calculations), `example_transects.csv` (synthetic example data, not real survey data). Keep all three in the same folder.

### Bugs fixed

| # | Problem in version 1 | Effect | Fix |
|---|---|---|---|
| 1 | Bootstrap recalculated Shannon diversity from the benthic cover columns (coral, turf, CCA…) instead of the coral taxa (`Tot_*`). | Feasibility intervals were not centred on the reported scores and could exclude them. | Bootstrap uses the transect-level H′ from the taxa, the same as the point estimate. |
| 2 | PC1 acceptance was tested on a PCA of site means, but scores came from a second PCA on transects, and were matched back by row names that `drop_na()` resets. | PC1 could be mis-oriented or attached to the wrong transects when any row had missing data. | One PCA on site means; site PC1 taken directly from it and oriented so higher = more algal-dominated. |
| 3 | When PC1 was rejected, the bootstrap evaluated `if (NA > 0)`. | The app stopped with an error. | Missing spread handled; PC1 contributes zero when rejected. |
| 4 | In the bootstrap, PC1 was set to 0 but still standardised against the site mean. | Urgency intervals shifted away from the point estimate. | PC1 (and any trend score) held at each site's value in the bootstrap, so intervals reflect transect variation only. |
| 5 | `predators` appeared twice in the trend variables. | Predator trend counted double. | Duplicates removed. |
| 6 | `as.Date(..., tryFormats)` chose one format from the first value and applied it to all; "12/01/2025" was read as year 0012. | Silent wrong dates; mixed formats failed. | Each date parsed separately; day-first/month-first option; implausible years rejected; unreadable dates reported. |
| 7 | Summary table converted `date` even when no date column existed. | Error for files without dates. | Removed; date checks are conditional. |
| 8 | Site with one transect: SD = NA, so CV and all scores became NA. | Site dropped silently. | CV = 0 and site flagged below the minimum transect number. |
| 9 | Weights were typed in three places (indices, bootstrap, donor). | Easy to change one and not the others. | One weights table used everywhere. |
| 10 | Outputs were defined inside `observeEvent`. | Hard to maintain; outputs re-registered on every run. | Analysis in one `eventReactive`; outputs read from it. |
| 11 | PCA plot labelled sites by row number. | Sites not identifiable. | Biplot with site names and variable loadings (Fig S1). |
| 12 | `library(rmarkdown)` loaded but unused; "Prioritization" spelling. | Tidiness. | Removed; British spelling to match the paper. |

### New features (to meet the manuscript scope)

- **Adjustable weights**: sliders for Feasibility, Urgency, Donor and Trend weights, with fixed directions (+/−), allowed ranges, reset to defaults, and a warning when weights differ from defaults by more than a factor of two.
- **Presets**: Default, Disease outbreak, Post-bleaching event, Transient algae, Minimal overlap; each with a one-line justification.
- **Save and load settings**: settings file (weights and options) for reproducibility; results file includes a header with version, date, weights, trend setting, bootstrap B and seed.
- **Weight-sensitivity tab**: each Feasibility and Urgency weight ×0.5 and ×1.5 in turn, plus Minimal overlap; stability (% scenarios with the same class) per site, and which scenarios change classes.
- **Trends off by default**, with a warning when surveys span less than a year.
- **Minimum transects** (default 5): sites below are flagged in the plot and tables.
- **Borderline flag**: sites whose 95% interval crosses zero on either axis.
- **Overview tab**: sites, transects, date span, Feasibility–Urgency Spearman ρ, quadrant counts, and data warnings.
- **Relative-score messages** throughout (zero = mean of the sites analysed).
- **Classes and pathways aligned with the manuscript** (A Immediate restoration, B Triage and foundational support, C Proactive reinforcement, D Monitoring only), with recommendations matching Fig 1.
- **Methods and weights tab**: prints the exact formulas used in each run.
- **Input checks**: case-insensitive column names; percentages converted to proportions; non-numeric values and missing taxon counts reported; required columns (`site`, `coral`, `Tot_*`) checked with clear messages.
- **Example file download** (synthetic data).

### Decisions for you

1. **PCA variables.** Version 1 included bleaching, disease and predators in the PCA. Version 2 uses composition only (coral, turf, CCA, cyanobacteria, macroalgae) because bleaching and disease already enter Urgency directly, so including them in PC1 counts them twice. A checkbox restores the version 1 behaviour. Choose one and state it in the Methods.
2. **Class letters.** Version 1 used A = Triage, B = Immediate, C = Monitoring, D = Proactive. Version 2 follows the manuscript figure (A = Immediate, B = Triage, C = Proactive, D = Monitoring). Keep whichever you prefer, but use the same in app, figures and text.
3. **Resource links.** Version 1 linked each class to a web page (Reef Resilience, IUCN, CORDAP). I could not check these from here, so they are not in version 2. If you want links, check each URL works and add them to `CLASSES` in `analysis.R`.
4. **Results will differ from version 1** because of bugs 1–4. Re-run Isla del Caño with version 2 and use those results in the paper.

### Still to do

- Run on the real Isla del Caño data and compare with version 1.
- Test on a second (or simulated) dataset for the "general-purpose" claim.
- Deploy to Posit Connect Cloud; archive on Zenodo with the example files.
- Optional: Spanish-language interface for managers in the Eastern Tropical Pacific.
