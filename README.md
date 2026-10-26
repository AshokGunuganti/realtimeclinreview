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
- [ ] Standard derivations with unit tests
- [ ] Review state (marks, sign-off, append-only audit trail)
- [ ] Get a dummy Medrio export to settle record keys and versioning

## License

Apache License 2.0. See `LICENSE`.
