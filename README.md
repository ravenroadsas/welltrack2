# WellTrack 2.0: UI mockup

A Shiny app, built as an R package, for the **decision-centric Well Intervention process**: workover, well service and initial completion.

> **Status: UI/UX mockup for review.** Data is synthetic. A few interactions marked **LIVE** write to an in-memory DuckDB so the flow can be tried: recording a decision, confirming gate evidence, updating a stream or workstream, and creating an opportunity. Everything else is read-only.

```r
# run locally
pkgload::load_all(); run_app()          # or: shiny::runApp()  (uses app.R)
devtools::test()                        # 162 unit tests
```

Use the user selector at the top right to **switch accounts**. The same screens change by role: try `Laura Gómez` (Asset Manager) to sign a D2, `Diana Mejía` (ALS) to update a stream, or `Juan Morales` (Integrator).

---

## 1. The process the UI follows

The UI follows the **decision thread** from the process document. It does not mirror the organizational swimlanes.

```
S1 Opportunity → ◆D1 Pursue? → S2 Technical Assurance → S3 Value & Investment → ◆D2 Invest?
  → S4 Prepare & Readiness → ◆D3 Execute? → S5 Execute → S6 Verify Value & Learn
```

* **Stage**, **state**, **decision status** and **readiness** are stored as separate concepts (doc §29).
* Every case shows the same **stepper**. Stages are circles, decisions are diamonds, and each step shows how many days it took.
* For each case, the app works out the next gate and how ready its evidence is (%), and lists blockers with their owner and next action. This is the "case status" panel, modelled on the example status response in doc §34.

## 2. Screens (tabs)

| Tab | Purpose | Main users |
|---|---|---|
| **My Work** | Inbox that shows only what needs *this person's* action (streams, workstreams, risks, decisions, SLA breaches), plus exceptions and a pipeline-at-a-glance chart. | everyone |
| **Pipeline** | Board with one column per step (decision columns highlighted) and a table view. Cards show class, incremental bopd, days in step vs SLA, and next-gate readiness. Filters: field, type, class, text, over-SLA. | integrator, management |
| **Opportunity** | The shared object. Header with reservoir → realizable → incremental potential, cost and NPV. Sub-tabs: Overview, Technical assurance (stream cards + potential waterfall + WPA-by-exception), Value & investment (economics consistency check), Gates & decisions (criteria checklist with automation mode), Readiness (parallel workstreams, WRR at T-8), Execution & value (actual vs D2 baseline), Risks & changes, History (audit trail). | all disciplines |
| **Decisions** | Queue of D1/D2/D3 cases (mine / all) and the selected case's **decision package**. A form whose outcomes come from config: rationale is mandatory, conditions are optional, and the record stores approver, date, baseline and resulting state. | decision authorities |
| **Process Stats** | **Time per process**: median vs SLA per step, working vs waiting time, flow segments (Opp→D1, D1→D2, D2→RTE, RTE→Exec), lead-time trend, bottleneck heat map (by class/type/field), WIP ageing, value realization, table view. | management, process owner |
| **Admin** | Process config viewer (steps, SLA, criteria, automation mix, classes, streams), account types and permission matrix, users, **activity log & process mining**, raw YAML. | admin |
| **New opportunity** (button) | Stage 1 framing form **generated from config**. Fields marked `auto` are pre-filled from master data. A live preview shows class, required streams, D1 authority and SLA. | contributors, integrator |

Screenshots: [`docs/screenshots/`](docs/screenshots).

## 3. Account types (5)

Roles are combinable (for example, the integrator is also a contributor). Discipline and authority level are **attributes** of a user, not extra roles. This keeps the role list short while still giving discipline-scoped editing and authority-scoped approvals.

| Account type | Can | Scoped by |
|---|---|---|
| **Viewer** | read everything | n/a |
| **Contributor** | originate opportunities; edit *their discipline's* assurance streams and readiness workstreams; register risks; record execution | `discipline` (Reservoir, Ops Eng, ALS, Production, Integrity, Field Ops, Regulatory, Finance, Supply Chain) |
| **Integrator** (Surveillance / WPA) | everything a contributor can do on any discipline, plus confirm gate evidence, change state, prepare decision packages, run value review | n/a |
| **Decision Authority** | sign D1/D2/D3 within their authority | `authority` per gate and class (config `decisions.*.authority`) |
| **Administrator** | config, users, activity logs | n/a |

On **Posit Connect**, `session$user` and `session$groups` identify the person. Connect groups map to roles (`roles.*.connect_groups`). The user table (discipline, authority) is a CSV for now and will move to a **pin**.

## 4. Configuration (adjust the process without code)

`inst/config/process.yml` is the single source of truth for:

* steps, SLA per class, and states mapped to steps;
* gates: outcomes → resulting state, and **criteria with an automation mode**;
* intervention types, **complexity classes** and classification rules (thresholds);
* assurance streams (with `applies_when` conditions), readiness workstreams, the WRR offset;
* WPA exception triggers, change-control materiality thresholds;
* the framing form fields, roles and permissions, activity-to-phase mapping, KPI targets.

`wt_validate_config()` checks the file at startup. For example, it stops the app if a decision leads to an unknown state, or if an `auto` criterion has no check. Rules are structured (`{field, op, value}` or `{fn: name}`) and never `eval()`'d, so a config file cannot execute code.

### Progressive automation

Every gate criterion is `manual` → `assisted` → `auto`:

* **manual**: a person ticks it and attaches evidence;
* **assisted**: the app evaluates it from data and proposes a result; a person confirms (shown as *SUGGESTED*);
* **auto**: the app evaluates it.

Moving a criterion to the next level means changing its `mode` in YAML (and adding a check function in `wt_check_registry()` if it is new). The Admin tab shows the auto/assisted/manual mix per gate as an **automation coverage** indicator.

Items tagged `# TBV` in the YAML are the "to be validated" list in doc §40 (authorities, thresholds, mandatory disciplines, WPA triggers, RTE checklist…).

## 5. Analyses as gate evidence

Users can run analyses inside the case to fulfil a gate requirement. The result is stored as **evidence** on the criterion.

**An analysis is defined once and called by id from any process.** `inst/config/analyses.yml` is a catalog shared by all processes. Each entry is like a function signature: what case data it needs (`inputs`), which values it produces (`outputs`), who may run it (`disciplines`), and where it runs (`kind`: `module` in this app, `app` on Connect, `report`, or `external` tool). A process links analyses to criteria:

```yaml
- {id: d2_baseline, label: Production baseline analysed (decline evidence), mode: auto, classes: [B, C],
   check: {fn: evidence_submitted, analysis: decline_curve}, analyses: [decline_curve]}
```

The same `decline_curve` is used by D1 `d1_info` and D2 `d2_baseline`. Another process would reference it the same way. Unknown ids stop the app at startup.

**Implementation of an in-app analysis** (`kind: module`):
- `R/analysis_decline.R` has a pure compute function, `wt_an_decline_fit()`. It is tested and callable from scripts, scheduled reports or an API, and returns `params`, `outputs`, `data_ref` and `summary`.
- It also has a thin Shiny module (inputs + chart) around that function.
- It is registered in `wt_analysis_registry()` (`R/analyses.R`).

**Evidence record** (`evidence` table, built by `wt_evidence_record()`): case, gate, criterion, analysis id and version, params, outputs (JSON), data reference, summary, status (`submitted` / `superseded`), user and time. Every analysis kind writes the same record. The gate check `evidence_submitted` reads it, and evidence counts only for the criterion it was attached to.

**UX: one sub-tab, however many analyses:**
- **Opportunity → Analysis** (workbench). Left: the analyses this process links to the case's criteria, current gate first, with evidence status. Centre: the analysis. Right: its outputs and **Submit as evidence** (LIVE), plus the evidence already attached.
- **"Analyze" button** on each gate criterion that has analyses. It opens the workbench with that analysis and criterion selected.
- **Decision package** lists the analysis results attached to the gate.
- **`kind: app`** analyses (example: `nodal_quicklook`) show a deep link `…?opp=…&criterion=…` to the separate Connect app. That app writes the same evidence record back. In the mockup the URL is a placeholder.
- **Admin** lists the catalog and which criteria use each analysis.

**Adding an analysis:** add a catalog entry and reference it from a criterion. For an in-app analysis, also add a compute function, a module and one registry line. The case and gate screens do not change.

## 6. Data tracked per opportunity

Relational model (DuckDB in dev). All SQL is in `R/data_access.R` and all access goes through DBI, so moving to SQL Server or PostgreSQL means changing the driver in `wt_db_connect()`.

| Table | Content |
|---|---|
| `opportunity` | framing, well context, class, step + state, reservoir/realizable/incremental potential, cost ± uncertainty, NPV/IRR/payout, economic basis, baseline version, planned & actual execution, value realization, lesson |
| `step_history` | entered/exited per step, waiting share, recycle flag. **Drives every time statistic.** |
| `stream_status` | assurance stream per case: status, owner, constraint (bopd), note |
| `workstream_status` | readiness workstream per case: status, owner, due date |
| `gate_check` | manual/assisted evidence confirmations (who, when, evidence) |
| `decision` | gate, outcome, authority, decided_by/at, baseline, rationale, conditions, superseded |
| `risk` | category, P×C, mitigation, owner, due, status |
| `change_request` | post-D2 changes, materiality (accept / revalidate / reapprove D2), status |
| `evidence` | analysis results attached to gate criteria (see §5) |
| `production_history` | monthly oil rate per well (input of the decline analysis; source system later) |
| `activity_log` | raw UI events mapped to process phases (process mining) |

## 7. Value KPIs (shown in Process Stats)

| KPI | Why it shows the value of the process |
|---|---|
| Median lead time (opportunity → executed) vs target by class | the main speed outcome |
| Opp→D1, D1→D2, D2→RTE, RTE→Exec | locates where time is lost (doc §37 flow efficiency) |
| Working vs waiting time per step | quantifies hand-off and queue waste |
| **Deferred oil from delays** = Σ (days over SLA × incremental bopd) of open steps | puts a **barrel value on process delay** |
| % cases managed by exception (no WPA meeting) | measures the shift from meetings to asynchronous assurance |
| Value realization = actual / promised oil (and cost ratio) | checks whether D2 promises hold |
| Recycle rate after D2, % over SLA, open blockers | decision quality and process health |

## 8. Process mining

`www/activity.js` captures input changes and navigation **in the browser** and sends them to the server in batches every 5 s. `wt_map_activity_phase()` maps raw ids to readable phases using the config (`^opp-stream_` → *Technical assurance*, `^decisions-decide` → *Record decision*, …). `wt_activity_eventlog()` collapses consecutive events into activity instances (case = opportunity, activity = phase, resource = user, start/end). The Admin tab shows time by phase and a directly-follows matrix, and exports a CSV ready for bupaR / pm4py / Celonis.

## 9. Performance: moving work out of the Shiny R process

1. **Database**: step statistics and readiness move into SQL views (`wt_db_step_stats()` shows the pattern with DuckDB `quantile_cont`). The app then reads pre-aggregated rows.
2. **Scheduled jobs on Connect** (Quarto/R Markdown): nightly retrieval of well, production and ALS master data; recomputation of the portfolio summary, SLA breaches and reminders, written to tables or pins.
3. **API** (plumber on Connect): readiness and status-summary endpoints that the LLM assistant and other systems can call, so the logic is not tied to a Shiny session.
4. **Client side**: charts render in the browser (echarts), the board is static HTML with JS click handlers, and the activity log is buffered in JS.
5. **Shared connection pool** across sessions, with a file or enterprise database once multi-process.

## 10. Code layout

```
inst/config/process.yml   process definition (YAML)
inst/config/analyses.yml  analysis catalog shared by all processes
inst/config/users.csv     user → roles/discipline/authority (→ pin)
inst/app/www/             welltrack.css (industrial theme), activity.js (logger)
R/config.R                load/validate config, accessors
R/process_logic.R         rules, classification, gates, transitions, WPA, status summary
R/work_items.R            portfolio summary, My Work items
R/stats.R                 durations, lead times, KPIs
R/activity_log.R          phase mapping, event log, transitions
R/analyses.R              analysis registry, case analyses, evidence record, deep links
R/analysis_<id>.R         one file per in-app analysis: compute function + Shiny module
R/users.R                 user resolution (Connect), permissions
R/data_access.R           ALL database access (DBI)
R/mock_data.R             synthetic portfolio for the mockup
R/ui_helpers.R            callouts, KPI tiles, badges, stepper, board
R/tab_<tab>_ui.R / _server.R   one pair per main tab
R/mod_newopp.R            config-driven framing form
tests/testthat/           unit tests for all non-UI functions
app.R                     Posit Connect entry point
```

## 11. Questions for the mockup review

1. Are **five account types** right? Should Planner/Regulatory become a separate type, or stay contributors with a discipline?
2. Should the home page stay **My Work** (inbox), or should it be the **Pipeline** board for some roles?
3. Gate criteria and their initial automation mode: which manual items can start as *assisted*?
4. Classification thresholds (150 / 800 kUSD, 100 bopd), SLA per step and class, WPA triggers.
5. Should D1 be decided individually or in a weekly batch (a "D1 board" view)?
6. Which source systems feed the `auto` fields (well master, production, ALS, SAP)?
7. Which extra Stats breakdowns are needed (by rig, by originator, by discipline queue time)?
