# Design decisions v2.7.0 (25 November 2026)

This record amends `design_decisions_v2.6.0.md`. Everything not listed here is unchanged.

Status: proposed by Caroline, Lucy and Chris; to be merged once all three have approved the wording.

| Topic | Decision | Reason |
|---|---|---|
| Class labels | A Assess for active restoration; B Address barriers to recovery; C Protect and maintain; D Monitor and reassess (previously Immediate restoration; Triage and foundational support; Proactive reinforcement; Monitoring only) | "Immediate" overstated what a relative class supports: in the Isla del Caño case study the only class A site had 19% class support and moved to B under the Disease outbreak context, and the severe-bleaching alert already says to hold outplanting. "Triage" can read as abandoning a site (Buckley, 2016); the paper keeps the term for the tool as a whole, defined as ordering action under constraint. |
| Class descriptions | One-line summary (tables, plot, exports) plus a full description (help text, Table S2), edited from Lucy's draft | The full text gives managers the hedged guidance they need; tables and legends need one line. |
| Class D wording | States that low relative Urgency does not mean recovery is under way, and points to the coral-cover alert | A site whose coral has collapsed can score low on Urgency because little coral remains to bleach (San Josecito, December 2024 – February 2025). |
| Links to alerts | A refers to the severe-bleaching alert (hold outplanting); B to the rubble alert (stabilise first); D to the coral-cover alert | Relative classes and absolute alerts are meant to be read together. |
| Letters, boundaries, colours | Unchanged: A–D keep their quadrants; zero lines; score of exactly zero to the higher class; Okabe–Ito colours, now keyed by letter | No result changes; renaming cannot break colours in future. The v1 letter order (A Triage, B Immediate, C Monitoring, D Proactive) is not used. |
| Spanish | Proposed labels: Evaluar para restauración activa; Abordar las barreras a la recuperación; Proteger y mantener; Monitorear y reevaluar | To be checked by a native speaker before use in the dashboard; the Shiny app is English-only at present. |
