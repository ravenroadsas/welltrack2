# WellTrack 2.0: essential preview

This is a trimmed, **read-only** version of the WellTrack 2.0 mockup, made for a first session with end users. It shows the core idea (one shared record per intervention, moving through the decision thread with clear gates) without showing the full roadmap or suggesting that a working tool is days away.

The full mockup is on branch `claude/welltrack-2-shiny-mockup-3w7t0d`. Both branches share the same process logic, YAML config and data layer. Only the UI differs.

```r
pkgload::load_all(); run_app()     # or shiny::runApp()
devtools::test()
```

## What users see

| Tab | Content |
|---|---|
| **Pipeline** (home) | Board with one column per step. Each card shows class, incremental potential, days in step vs target, and how much of the next gate's evidence is in place. Filters: field, type, text. |
| **Opportunity** | Header (current / reservoir / realizable potential, cost), decision-thread stepper, and four sections: Overview, Technical assurance (discipline status cards), Gates (D1/D2/D3 criteria met/open), Readiness. A small "Case status" panel shows the next gate and its open items. |
| **Decisions** | Cases waiting for D1/D2/D3, with the evidence package and the possible outcomes. No decision form. |
| **Process Stats** | Three KPIs (median lead time, active cases, % over target) and two charts: time per step vs target, and work in progress. |
| **New opportunity** | The framing form, with a side panel showing the resulting class and required disciplines. **Nothing is saved.** |

A permanent banner says: *"EARLY PREVIEW: a concept to collect your feedback, not a working tool. Data is fictitious, screens will change, and nothing you enter is saved."*

## What is deliberately not shown (vs. the full mockup)

- **Writing data.** No decision signing, evidence confirmation, stream/workstream updates or saved opportunities.
- **My Work** inbox and the role-based exception lists.
- **Automation signals:** criteria automation modes (auto/assisted/manual), "auto" master-data badges, auto-filled form fields, the assistant / "Ask" box.
- **Advanced analysis:** reservoir → realizable waterfall, WPA-by-exception triggers, economics (NPV/IRR/consistency check), execution vs baseline, risks and change control, history/audit trail.
- **Advanced stats:** working vs waiting time, flow segments, bottleneck heat map, lead-time trend, value realization, deferred-oil KPI.
- **Admin:** config viewer, permission matrix, user table, activity logging and process mining. Activity logging is removed entirely from this branch, so nothing about users is recorded.

These are held back on purpose, so later releases deliver visible progress.

## Presenter notes (managing expectations)

- Open with the banner: this is a **concept to react to**, built on fictitious data. It is not a beta.
- Ask for feedback on **the flow and the information**, not on features: *Are these the right steps and gates? Is anything missing in the card / case header? Who should own each item?*
- When asked "when can we use it?", answer with the **sequence**, not a date: validate the process → connect real data sources → enable data entry for a pilot group → extend. Each step needs their input and IT/data work.
- Note requests in a backlog, and avoid promising features in the room.
- Questions worth asking:
  1. Do the six stages and three decisions match how you work?
  2. What should a card on the board show at a glance?
  3. Which gate criteria are missing or unnecessary for your discipline?
  4. What time targets per step are realistic?
