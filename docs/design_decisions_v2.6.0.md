# Design decisions v2.6.0 (2 November 2026)

This record amends `design_decisions_v2.5.0.md`. Everything not listed here is unchanged.

| Topic | Decision | Reason |
|---|---|---|
| Reef Check import | Added: Excel data sheets, segment tables and point tables are converted to the tool's transect format; one 20 m segment = one transect | Many practitioner teams use the Reef Check protocol; retyping their data is the main barrier to using the tool. Segments keep the within-site structure needed for patchiness and the bootstrap. |
| Category mapping | HC → coral; NIA → macroalgae; RB → rubble; SD → sand; RCTA/RCCA → turf/CCA where recorded; RC, SC, SP, SI, OT, RKC not used | Follows Reef Check definitions (turf and coralline algae are recorded as rock). Reading rock as turf would apply the turf penalty to bare rock and coralline algae. |
| Bleaching and disease | Share of hard-coral points marked HC/B or HC/D, or from the sheet's count rows | Reef Check records these per point, not per colony; the difference is stated in the user guide. Recording them per site and date instead of per transect changed no classes in the Isla del Caño test. |
| Indices and weights | Unchanged; unavailable indicators get zero weight, as for any file | The existing rule (an indicator recorded at half of the sites or fewer is not used) handles Reef Check data without a separate weighting scheme. |
| Warning | Shown on the Overview with every Reef Check analysis | Isla del Caño reduced to Reef Check categories: 8/11 classes kept (most recent period), 7/13 (Dec 2024 – Feb 2025); a turf-dominated site (Barco Profundo) was rated more favourably; class support rose although accuracy fell. |

Still open: test with real Reef Check exports from the central database (layout not yet seen) and with completed field sheets from a partner team before the release is announced.
