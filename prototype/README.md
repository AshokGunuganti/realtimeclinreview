# Phase 3 Clinical Data Review (modular R Shiny)

Real-time clinical data review for a single-arm, baseline-controlled rare-dermatology study:
what changed since the last refresh, efficacy, safety and PK, data quality, site performance,
dosing compliance, subject profiles and source data checks, with Excel export throughout.

**All data are SIMULATED.** Nothing here is real trial data.

## Run it

```r
install.packages(c("shiny", "bslib", "dplyr", "tidyr", "DT", "plotly", "writexl", "openxlsx", "testthat"))
shiny::runApp()            # from this folder
```

Install packages from CRAN. Some Linux distribution builds of shiny/DT strip bundled JavaScript
(date picker, DT styles).

Run the tests (113 checks: round trip, data checks, every module, spec consistency):

```r
testthat::test_dir("tests/testthat")
```

## How it is built

```
data/source/            raw files as delivered (EDC form extracts, imaging vendor, PK lab, query log)
config/visit_schedule.csv   visit schedule: folders, nominal weeks, windows, vendor visit labels
R/adapters.R            read + type sources, enforce required columns     (spec: Sources, Source Variables)
R/derive.R              review datasets RVSUBJ, RVVISIT, RVEFF, RVCOMP, RVAE, RVPK, RVQRY
                                                                          (spec: Review Datasets, Variable Mapping)
R/snapshot.R            as-of view for a review date + change log         (spec: Derivation Rules DR-001..017)
R/summaries.R           screen-level summaries (site status, flags, CIs)
R/checks.R              source data checks SC-001..011                    (spec: Data Checks)
R/mod_*.R               one Shiny module per screen; mod_table.R = reusable table card with Excel export
app.R                   wires modules together
specs/                  mapping specification (xlsx) and the scripts that build it
data-raw/               simulation: truth engine + writer for the raw source files
tests/testthat/         round-trip, checks, module and spec-consistency tests
```

Only `R/adapters.R` knows source file formats. To onboard the real study:

1. Point `APP_CFG$source_dir` (R/config.R) at the extract location.
2. Update `SOURCE_SPECS` and the parsers in `R/adapters.R` to the study's EDC field OIDs and vendor layouts.
3. Update `config/visit_schedule.csv` from the protocol schedule of assessments.
4. Run the tests. The round-trip test is simulation-specific; the check, module and spec tests still apply.
5. Rebuild the spec (needs Python 3 with openpyxl): `Rscript specs/export_spec_inputs.R && python3 specs/build_spec.py specs/inputs`.

## Review date and refreshes

Every record carries an availability date (EDC audit timestamp or vendor transfer date). The app
shows what was available as of the review date. In production the review date is the extract date.
In this demo the simulated files run to 31 Dec 2026, the default review date is 22 Oct 2026, and
"Simulate next weekly refresh" steps forward a week so you can see new and changed records appear.

## Regenerate the simulated sources

```
Rscript data-raw/simulate_sources.R
```

Realistic features in the simulated sources: Rave-style dates and audit timestamps, missed visits
recorded as "visit not done", out-of-window visits, central MCSS corrections arriving as new vendor
rows, MedDRA coding lagging AE entry, PK results as text with "<0.100" for BLQ, a run-in withdrawal,
and a query log with open/answer/close timestamps.

## Open items before real use

See the "Open Items" sheet in the spec: EDC system and OIDs, vendor transfer agreements, access to
blinded central reads, MedDRA version, refresh schedule and hosting, validation level, thresholds.
