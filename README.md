# realtimeclinreview

An open-source R framework for **real-time clinical data review**: map raw EDC and vendor
extracts into a collected data model, run a library of checks, and review results in a
Shiny app and an Excel workbook, with every screen reproducible "as of" any review date.

> **Status:** early design + working prototype. Private until licensing/ownership checks are done.
> **All data in this repository are simulated.** No real trial data, ever.

## Repository layout

| Path | What it is |
| --- | --- |
| `docs/design.md` | Framework design: data model, as-of engine, derivations, check and display libraries, review state, outputs, pipeline, validation, roadmap |
| `core/` | Core package `rtcrcore` (working name): everything that decides what a reviewer sees. Validated once |
| `app/` | App package `rtcrapp` (working name): Shiny display modules that present core results |
| `studies/example/` | Example study (simulated): `study.yml`, visit schedule, `mapping_spec.xlsx`, one extract |
| `renv.lock` | Pinned R package versions for the whole repository |
| `prototype/` | Working modular Shiny app on a simulated single-arm rare-dermatology study (113 passing tests). Source of the patterns the packages will generalize |
| `prototype/specs/Review_Data_Spec.xlsx` | Example mapping specification (sources → review datasets, derivations, checks, traceability) |
| `demo/data_review_demo.html` | Self-contained offline HTML demo (open in a browser) |
| `demo/single_file_app.R` | Original single-file Shiny mockup |

## Run the review app (example study)

```r
renv::install(c("./core", "./app"))
rtcrapp::run_app("studies/example")     # screens come from study.yml `displays`
```

The app opens on **My review**, an inbox: subjects with new or changed data since your last sign-off, or a new clinical flag, on the left; the selected subject, new data first, on the right. Press **Reviewed, next** (or the m key) to mark it and move on. Every other screen is one click away in the sidebar; clicking a subject anywhere opens it in a side panel. Review date, changes since, site and a subject search sit above every screen.

Review checks (new and changed AEs, SAEs, low compliance, missed and overdue visits, study rules and data issues) are rows in `studies/<study>/check_spec.xlsx`, which the review team edits: add a row, change a condition, or set STATUS to retired. New studies start from the canned set (`rtcrcore::write_check_spec()`). Programmer checks on loading and mapping always run and show only on the Data load page.

Deep links: `?tab=<display id>` opens a screen, `?subject=<id>` opens the subject panel. The **Review workbook** button downloads the whole review as Excel: contents, one sheet per check, and a filterable patient profile.

## Development setup

From the repository root (renv activates automatically via `.Rprofile`):

```r
renv::restore()                       # install pinned packages
renv::install(c("./core", "./app"))   # local packages (not in the lockfile)
testthat::test_local("core")
testthat::test_local("app")
```

## Run the prototype

```r
setwd("prototype")
install.packages(c("shiny", "bslib", "dplyr", "tidyr", "DT", "plotly", "writexl", "openxlsx", "testthat"))
shiny::runApp()
testthat::test_dir("tests/testthat")
```

## Plan (version 1)

Two packages, per `docs/design.md`:

1. **Core package** (validated once): connectors (Medrio export, generic CSV/SAS), mapping-spec
   compiler (Excel → YAML), collected data model with standard header, as-of engine and change log,
   standard derivations (ABLFL/BASE/CHG, ADY, TRTEMFL, AVISIT, compliance, PK timing), check registry,
   review-state API (DBI: SQLite/Postgres), Excel workbook writer.
2. **App package**: generic Shiny display modules driven by form types and `study.yml`.

### Next steps

- [x] Scaffold `core/` and `app/` packages (usethis, testthat, renv)
- [x] Port standard header + as-of engine from `prototype/R/snapshot.R`
- [x] Mapping-spec schema and compiler; convert the prototype study to `studies/example/`
- [x] Port checks into the registry format (ID, tier, applies-to, timing, severity, owner)
- [x] Standard derivations with unit tests (reference-range flags and unit conversion still to do)
- [x] Review state (marks, sign-off, append-only audit trail); Postgres path written but not yet tested against a server
- [x] App package: shell and display modules (what's new, visit status grid, over-time chart, event timeline, listing, check results, subject profile) with review marks and Excel export
- [x] Operational displays: site scorecard, coding review, lock readiness, from the EDC query and form status reports (simulated in the example by `dev/simulate_example_ops.R`)
- [x] Reviewer-first layout: sidebar navigation, My review inbox, subject side panel with reviewed & next, site page, queries, sign-off (demonstration only until SSO re-authentication) and audit trail
- [x] Check spec for the review team: canned review checks per study, add, change and retire checks in Excel; programmer checks on the Data load page only
- [ ] SSO re-authentication for sign-off; roles from the login (version 2)
- [ ] Get a dummy Medrio export to settle record keys and versioning

## License

Apache License 2.0. See `LICENSE`.
