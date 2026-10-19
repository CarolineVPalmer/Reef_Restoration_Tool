# Design decisions v2.5.0 (19 October 2026)

Amends the v2.4.0 design decisions (see `CHANGES.md`); everything else is unchanged.

| Topic | Decision | Reason |
|---|---|---|
| Bleaching-event mode | Added as an optional tab; approved by Caroline 19 Oct 2026. The standard tool is unchanged and fully usable without it (tested: identical classes with and without event settings). | Managers need to compare reefs before, during and after the 2026–27 event; one tool rather than a second app keeps maintenance and translation manageable. |
| What it reports | Per site: bleaching during the event (with the > 50% alert), change in coral, turf and CCA cover from before to the latest phase (after if surveyed, otherwise during), 95% bootstrap intervals for coral change, and change across the reef. | Observed change, with uncertainty, is what the data support. |
| Missing phases | "Before" is required; "during" and "after" are each optional, so the comparison runs mid-event. Sites missing a phase are listed with a note. | Most teams will not yet have after-event data. |
| No forecasting | The mode does not predict coral loss. | No validated bleaching–mortality relationship for these reefs; a wrong forecast could misdirect effort. Revisit after the event with before/after data. |
| Dates | The event comparison uses all dates in the file, independent of the standard analysis's data period. | Pre-event baselines often predate the data period chosen for the current screen. |
| Hosting | Main option: in-browser version (Shinylive) on GitHub Pages; download-and-run in R as backup. Code repository to be made public at launch. Approved 19 Oct 2026, subject to the speed test. | Free, no usage cap, no server to maintain, and users' data never leave their device. |
