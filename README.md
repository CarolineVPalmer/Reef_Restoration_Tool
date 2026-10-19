# Reef Restoration Prioritisation Tool

A free, point-and-click R Shiny tool that turns routine coral reef transect data into **relative** restoration priorities for the sites a team monitors. For each site it calculates a Feasibility index (relative reef condition), an Urgency index (relative pressure) and a donor-site score. It places sites on a Feasibility–Urgency decision plane with bootstrap uncertainty, links each management class to a broad intervention pathway, and reports absolute alerts beside the relative classes.

The tool is a first-pass screening step before detailed site-suitability assessment. It is not a measure of restoration success and has not been validated against restoration outcomes.

Version: 2.5.2 (see `CHANGES.md`). Manuscript: *An accessible reef management tool for prioritising coral restoration in data-limited and low-resource settings* [in preparation].

## Use it in your browser

**https://carolinevpalmer.github.io/Reef_Restoration_Tool/**

The tool runs entirely inside your web browser: nothing to install, and the data you upload stay on your own computer. The first visit downloads the software (about half a minute on a fast connection, a few minutes on a slow one); later visits are faster.

## Quick start (in R, for offline use)

1. Install R (≥ 4.3) and RStudio.
2. Open `reef-restoration-tool.Rproj` (or the project folder) in RStudio.
3. Restore the package versions used for the paper:
   ```r
   install.packages("renv")
   renv::restore()
   ```
   If there is no `renv.lock` yet (see *Maintainer setup*), run `install.packages(c("shiny", "ggplot2", "ggrepel"))` instead.
4. Open `app.R` and click **Run App**, or run `shiny::runApp()` in the console.
5. Upload a transect CSV, or download the synthetic example file from the app (`data/example_transects.csv`).

## Input data

One row per transect (or photogrammetry plot). Required columns: `site` and `coral` (live coral cover). Recommended columns: `date` (or `year` and `month`); `turf`, `cca`, `cyanobacteria`, `macroalgae` and `rubble` cover; `bleached`, `disease` and `predation` (proportion of coral colonies affected); and colony counts per coral taxon in columns starting `Tot_` (for diversity). Optional columns: `rugosity`, `herbivores`, `depth` and `sand` (shown beside the results but not scored), and `method`. Cover and prevalence can be given as proportions or percentages. Comma- and semicolon-separated files are accepted.

## Repository structure

```
app.R                     Shiny interface (UI and server)
R/                        All calculations, one file per stage
  defaults.R              Default settings, weights, presets, alerts, classes
  utils.R                 Parsing, standardisation, diversity, survey periods
  data_input.R            Reading and validating the input file
  snapshot.R              Data period and snapshot
  indices.R               Site descriptors, PCA, Feasibility/Urgency/Donor, classes
  trends.R                Trend slopes and trend score
  trajectories_alerts.R   Trajectories and absolute alerts
  run_analysis.R          Full analysis, including the bootstrap
  sensitivity.R           Weight sensitivity
  settings_io.R           Saving and reloading settings
  plots_tables.R          Figures, tables and formula text
data/
  example_transects.csv   Synthetic example (no real sites)
  case_study/             Isla del Caño data and settings (not public yet; see below)
scripts/
  reproduce_case_study.R  Re-runs the case study and checks it against the reference outputs
tests/                    testthat unit tests
docs/                     Design decisions
```

## Reproducing the case study

From the project folder in R:

```r
source("scripts/reproduce_case_study.R")   # writes outputs/case_study/ and a comparison report
source("tests/testthat.R")                 # runs the unit tests
```

The case-study settings (data from 1 April 2024, most recent survey period as the snapshot, trends on, minimum of three transects, B = 1000, seed 123) are stored in `data/case_study/settings_case_study.csv` and can also be loaded in the app.

## Data availability

The Isla del Caño monitoring data are excluded from the public repository (`.gitignore`) until their release has been agreed with the data owners and the permit conditions have been checked. [Decision needed before the first public release: archive the data with the code on Zenodo, or deposit them separately and link to them.]

## Maintainer setup (once)

```r
install.packages("renv")
renv::init(bare = TRUE)
renv::install(c("shiny", "ggplot2", "ggrepel", "testthat", "rstudioapi"))
renv::snapshot()   # writes renv.lock; commit it
```

## Releases and DOI

Each GitHub release is archived on Zenodo, which issues a DOI. Connect the repository at zenodo.org (Account › GitHub) before making the first release, then create release `v2.3.0` on GitHub. Citation metadata are in `CITATION.cff`.

## Citation

See `CITATION.cff`. [Add the manuscript reference and the Zenodo DOI after release.]

## Licence

Code: MIT (see `LICENSE`). [Confirm. Data, when released: CC BY 4.0 is suggested.]
