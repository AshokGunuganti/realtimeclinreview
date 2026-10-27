# Real-Time Clinical Data Review Framework: Design

Working design, October 2026

## Purpose and scope

The framework lets a small study team review an ongoing trial from raw EDC and vendor data every week, so issues are found while sites can still fix them, not at database lock. A new study is set up by configuration, not by rewriting code.

It is the open-source successor to *Real Time Clinical Trial Oversight with SAS* (PharmaSUG 2017, DA01). That paper's three tools map directly onto this design:

| 2017 tool (SAS) | Framework equivalent |
| --- | --- |
| Data entry timeliness metrics by site | Site performance display, with the same three timeliness anchors |
| Programmatic edit-check workbook | Check library plus the Excel review workbook |
| Graphical patient profiles (PDF) | Subject profile display plus PDF export (v2) |

**In scope:** data management review, clinical and medical review, safety and PK monitoring, site operations. Each review covers one study; one installation can host several.

**Out of scope:** submission datasets and outputs, cross-study pooling, statistical analysis. SDTM is an optional export, not a dependency.

## Design principles

Three goals decide every trade-off: a **small team can maintain it**, it is **fit for purpose for regulatory compliance** (validated once, traceable, auditable), and a **new study is mostly configuration**, not new code.

1. **Stay close to how data is collected.** Data is kept one table per eCRF form or vendor file, at collection grain, with CDASH variable names. Nothing is split into SDTM domains and pieced back together.
2. **Share record identity with the EDC.** Every record keeps the EDC's own form and record keys, so queries, audit changes, review marks and EDC links all point at the same record.
3. **Everything is as-of.** Every record carries the date it became available and a version. Any screen can be shown as it stood on any review date.
4. **Configure, don't code.** Mapping, checks and displays are driven by a study's spec and config. Code changes only when a genuinely new capability is needed.
5. **Build once, reuse per study.** A form library of standard forms, with their checks and displays, makes each new study mostly configuration.
6. **One pipeline, many outputs.** All logic (derivations, checks, as-of views, role filtering) lives in the validated core package; the app, the Excel workbook and PDF profiles only present its results, so they always agree.
7. **Review needs an owner.** The framework records who reviewed what and when, not just what the data shows.
8. **No real data in the repository.** Examples and tests use simulated data only.

## Architecture overview

A scheduled pipeline runs the core package on each extract: it builds the versioned collected data model, then the as-of views, derivations and checks, and caches the results. The app, the Excel workbook and PDF profiles only present those results, so they never disagree.

*(Architecture diagram: see the layer list below; an SVG will be added under docs/.)*

The validation boundary is drawn by package, not by where code runs. The core package holds everything that decides what a reviewer sees (connectors, mapping, as-of engine, derivations, checks, the review-state API and output writers) and is validated. The app package only renders screens and records review actions through the core API.

## Collected data model

The collected data model (CDM) is the single source of truth: one table per eCRF form or vendor file, one row per form instance or log line, fields horizontal as on the CRF. Every table starts with the same standard header.

**Standard header (required on every CDM table)**

| Variable | Meaning | Example |
| --- | --- | --- |
| STUDYID | Study identifier | DERM-301 |
| SITEID | Site number | 105 |
| SUBJID | Subject identifier | 105-002 |
| FORMOID | Form or vendor file identifier | AE, IGA, VENDOR\_PK |
| FOLDER | EDC folder or vendor visit label (blank for log and subject forms) | WK04 |
| VISITNUM | Visit number from the visit schedule | 3 |
| RECID | Record key from the source: EDC record or log line ID, or vendor row key | 48211 |
| ENTRYDT | Date first entered or received | 2026-10-20 |
| UPDDT | Date last changed | 2026-10-24 |
| AVAILDT | Date and time (UTC) this version became available, with an extract sequence number for same-day runs | 2026-10-24 |
| VERSION | Version of the record, 1 = original | 2 |
| DELFL | Optional. TRUE on a version that records a deletion (a tombstone); tables without it have no deletions | FALSE |

`FORMOID + RECID + VERSION` is unique. After the header come the collected fields under CDASH names (`AETERM, AESTDAT, AESEV`). Coded fields keep both the code and its decode, and dates are converted to ISO 8601. Any field the reviewer needs from the source is kept, even if it has no SDTM home.

**Record keys.** Each form declares its key strategy in the spec: the source record ID where the EDC provides a stable one, otherwise a natural key (for example SUBJID + FOLDER + FORMOID + repeat number) or a vendor key. A key crosswalk table carries subject renumbering, site transfers and re-screening (linked by a person ID), so history and review marks follow the person. Grids and repeating sections become one row per line, with the line number in the key. Unscheduled visits take the previous scheduled VISITNUM plus .01, .02 and so on.

**Dates and coding.** System timestamps are stored in UTC. Collected dates and times stay local, with each site's time zone in the site table, so dose-to-sample times are correct. MedDRA and WHODrug coding is kept as its own source joined by key, with the dictionary version as a column; a re-code or dictionary upgrade is flagged as a coding change and does not reset review marks.

**Form types.** Each form declares a type in config. Generic checks and displays work off the type, not the form name.

| Form type | Grain | Examples | What it gets for free |
| --- | --- | --- | --- |
| Subject | One row per subject | Demographics, eligibility, disposition | Enrollment counts, subject header |
| Visit | One row per subject per visit | Visit date, vitals, investigator assessments | Visit status grid, over-time charts, timeliness |
| Log | One row per entry | AEs, con meds, medical history, deviations | Timeline, listing, change tracking |
| Vendor findings | One row per result | Central reads, PK, central labs, ePRO | Over-time charts, threshold alerts, reconciliation |

**Versioning.** A change in the source creates a new version, never an overwrite. Versions come from the best evidence a source offers: record-level entry and update timestamps, or an audit log, where the source provides them; vendor corrections usually arrive as new rows. Where a source has neither, the pipeline compares each extract with the previous one: a record first seen in this extract is new, a changed value is a new version, and AVAILDT is that extract's timestamp. Change detection compares only mapped, reviewer-relevant fields, so a new export column or a relabelled decode does not mark every record as changed. This works with any EDC that can export, which keeps new studies and new systems cheap to add. The as-of engine picks the current version for any review date.

## Study configuration and mapping spec

A study is a folder of configuration plus its extracts; nothing in the package is study-specific.

```
studies/<study>/
  study.yml            sources, thresholds, roles, enabled checks and displays
  visit_schedule.csv   visits, nominal days, windows, EDC folders, vendor visit labels
  mapping_spec.xlsx    source field -> CDM variable, one row per field (compiled to mapping.yml)
  extracts/            raw files or API cache, one folder per extract date
```

**Mapping spec.** Data managers and programmers author it in Excel; the pipeline compiles it to YAML and validates it against a schema before running. Each row: target form, target variable, source, source field(s), transform, transform arguments, required flag, notes.

**Spec layout.** Three sheets. *Forms*: FORMOID, form type (subject, visit, log, vendor, or report for operational EDC reports), source file, record key (source fields), versioning (`single`: one row per key; `rows`: several rows per key are successive versions, such as vendor corrections, ordered by AVAILDT) and description. *Mapping*: one row per target variable in output order; arguments are written `name=value; name=value`; `filter` and `pivot` rows reshape the source before any field is mapped and leave the variable blank. *Codelists*: codelist, code, decode. FORMOID, RECID (key fields joined with `|`) and VERSION are filled by the mapper, never by the spec. Values a transform cannot use go to the parse log with form, variable, source row and subject; rows with no AVAILDT are dropped and logged.

**Transform vocabulary.** Deliberately small, so the spec stays readable and every transform is unit-tested once. A spec check warns when a study leans on derive for more than a configured share of fields, a sign the library is missing a transform:

| Transform | Does | Example argument |
| --- | --- | --- |
| direct | Copy as is |  |
| date | Parse a date or datetime to ISO, keeping partial dates | DD MON YYYY |
| decode | Code and decode via a codelist | IGA |
| recode | Map source values to new values, optionally conditional | Y/N to Yes/No |
| visit | Folder or vendor label to VISITNUM via the schedule | VENDORVIS |
| number | Numeric conversion, with a result-text rule | `<x` sets a below-limit flag |
| filter | Keep or drop rows | drop test sites |
| lookup | Join a value from a reference table | site time zone |
| pivot | Long to wide or wide to long, for vendor files and grids | one row per analyte |
| concat | Join fields | sep = " " |
| constant | Fixed value | FORMOID = VENDOR\_PK |
| derive | Named, registered R function for anything else | study\_day |

**Connectors.** A connector turns one source system into landing tables with the standard header filled. Version 1 ships a Medrio connector and a generic CSV/SAS connector; CDISC ODM follows, covering most other EDCs. Each connector records a parse log for anything it could not read.

**Medrio connector (version 1).** Built on scheduled exports, not the API. Medrio's public API is designed mainly for writing data (data entry, subjects, users, bulk upload) and reads only one subject's form at a time, so it is not suited to pulling a whole study. Its exports fit this design closely:

| Medrio output | Used for |
| --- | --- |
| Relational data export (one table per form, one record per row; SAS or tab-delimited) | The collected data model, mostly by renaming |
| Coded fields exported as two variables (formatted value and code) | Code and decode kept together |
| Data dictionary (Excel: forms, fields, codelists, visits, edit checks) | Seeds the mapping spec and visit schedule for each new study |
| Query report and query aging | Query status and aging on every record |
| Data audit log report | Entry and change timestamps; required in version 1 for timeliness metrics |
| Scheduled visits and missing forms reports | Cross-check of visit status; with the form status report (PI signature, SDV, freeze, lock), the lock-readiness view |

The exports are dropped into a secure folder on a schedule and read from there. An ODM connector through Medrio's API is a later upgrade if Medrio enables ODM export for the study. An API user needs the API Access and PHI permissions; single sign-on users cannot use the API, so it needs a dedicated service account, and tokens expire within an hour. Sources: [Medrio OpenAPI reference](https://community.medrio.com/documentation/other-products2/openapi-reference), [UCSF Medrio features](https://prevention.ucsf.edu/sites/prevention.ucsf.edu/files/inline-files/Medrio%20Features%20Slideshow%2020200128.pdf).

**study.yml** also holds the review thresholds (for example a PK alert level, a compliance flag, an entry lag target), which roles see which forms, which library checks and displays are switched on, and the test sites and subjects to exclude. It carries a schema\_version; the core ships migration functions whenever the schema changes.

## As-of engine and change log

The as-of engine reconstructs what the data looked like on any review date D, using only the standard header. That makes it generic: it never needs to know what a form contains.

- **Snapshot(D):** for each record, the highest VERSION with AVAILDT on or before D. Records first available after D are hidden.
- **Change log(since, D):** compares Snapshot(since) with Snapshot(D) and classifies every record as **New** (first version in the window), **Changed** (a later version in the window, with old and new values of each changed field), **Deleted** (the form was extracted in full but the record is absent; a missing or truncated file never produces Deleted) or **Unchanged**. A later version whose compared fields are all equal (for example only UPDDT moved) counts as Unchanged, so review marks are not reset needlessly; compared fields are every column except RECID, VERSION, AVAILDT, ENTRYDT, UPDDT and DELFL. A record created and deleted inside the window is not reported.
- **Deletions:** a deletion is stored as a new version with DELFL = TRUE, never by removing rows, so the record can still be shown as it stood on earlier dates. The pipeline writes one only when the rule above holds. Snapshot(D) hides a record whose current version is a deletion.
- **Derived status:** visit status, query status, days overdue and similar values are computed against D, never stored.

In production D is the latest extract; for investigations D can be any past date, to replay what reviewers saw. "Since" defaults to the reviewer's last sign-off for that review type, or the previous extract if none, so every "new" count is defined. The statuses New, Changed, Deleted and Unchanged feed every output: the app's "what's new" screen, row flags in the Excel workbook, and the reset of review marks. Snapshots and derivations for each extract are computed by the pipeline and cached; only ad-hoc replay dates are computed on the fly.

## Standard derivations

Cross-form derivations such as baseline, study day and treatment-emergent flags run in the validated core on the as-of snapshot, after mapping, and are cached by the pipeline per extract. They cannot be mapping transforms: they need data from other forms (first dose comes from the exposure form) and must move when that data arrives late or is corrected.

Derivations are registered like checks, unit-tested once in the core package, and configured per study in study.yml. Names follow ADaM conventions so programmers recognize them.

| Derivation | What it does | Configured in study.yml |
| --- | --- | --- |
| Reference dates | First and last dose, enrollment, withdrawal and last contact per subject | Which form and field supplies each |
| Subject status | Screen failure, on study, discontinued or completed; stops expected visits after discontinuation | Disposition form and codes |
| Period (APERIOD) | Assigns records to screening, run-in, treatment and follow-up | Period boundaries |
| Study day (ADY) | Days from first dose for any date, with no day 0 | Reference date |
| Baseline flag (ABLFL) and baseline value (BASE) | Selects the baseline record per subject and parameter | Rule per parameter: last non-missing on or before first dose, a named visit, or the mean of run-in values |
| Post-baseline flag, change (CHG), percent change (PCHG) | Change from baseline for post-baseline records | Which parameters |
| Analysis visit (AVISIT) | Assigns unscheduled and out-of-window visits to nominal visits | Window rules from the visit schedule |
| Treatment-emergent flag (TRTEMFL) | Event starts on or after first dose, or worsens after it | Lag window, if any |
| On-treatment flag | Record between first dose and last dose plus n days | n |
| Dosing compliance | Applications recorded against prescribed per diary interval, missed doses | Prescribed frequency, diary form |
| PK timing | Time since last dose, nominal against actual sampling window, below-limit handling | Window per PK visit, BLQ rule |
| Partial date imputation | Completes partial dates for derivations only, keeping the original value and an imputation flag | Imputation rule |
| Prior and concomitant medication flags | Classifies medications against first dose | Reference date |
| Worst post-baseline | Highest severity, worst score or extreme value per subject and parameter | Direction per parameter |
| Reference range flags | High, low and clinically significant flags from a ranges table keyed by lab, sex, age band and effective date | Ranges table and thresholds |
| Unit conversion | Converts results to standard units | Conversion table |

**Derivation types.** As with checks, each derivation is a configured instance of a registered type, run in study.yml order on the as-of snapshot, so later derivations can use earlier results (impute a partial AE start date, then derive study day and TRTEMFL from it). Subject-level results go to a derived subject table, ADSL (reference dates with a `<name>SRC` pointer to the source record, EOSSTT, EOSDT). Analysis visit targets count from the first visit on the same anchor in the visit schedule, so a run-in visit can anchor on enrollment and treatment visits on first dose; `keep_scheduled` keeps records collected at a scheduled visit on that visit and moves only unscheduled ones. Checks can run on the derived snapshot.

**Rules.** Derivations are recomputed for every review date, so a corrected dose date or baseline value updates the flags automatically. Each derived value points back to its source records (BASE names the record it came from), so a reviewer can click through. Derived variables sit beside collected ones; collected values are never overwritten.

## Check library

Checks are registered, not hard-coded. Each check is metadata plus one function, and every check returns findings in the same shape, so the app, the workbook and review state handle them identically.

**Two audiences.** *Review checks* look at the study data being collected: new and changed AEs, serious and severe AEs, low dosing compliance, missed, overdue and out-of-window visits, discontinuations, study rules such as a PK alert, and data issues sites must fix. They are what reviewers see on screen and in the workbook. *Programmer checks* (the structural tier, STR-001 to STR-006) test that the extract loaded and mapped cleanly. They always run, but appear only on the Data load page; reviewers see a one-line banner when one fails or a check cannot run, because data may then be missing. They never go into the review workbook.

**Check spec.** Review checks live in a check spec workbook (`check_spec.xlsx`, named by study.yml `check_spec`), separate from the mapping spec, so the review team owns them. Every study starts from the canned set (`canned_checks()`, written by `write_check_spec()`), which uses standard collected variable names; the team then adds rows, changes forms, fields, conditions and thresholds, and retires rows, without programming. One row per check: ID, name, description, category (Safety, Dosing, Visits, Disposition, Efficacy, Data), tier (`clinical` for medical review, `data` for data issues), type, form, field, conditions in plain text (`AESER == Y; AESEV in MODERATE, SEVERE`; `$name` reads a study threshold), other settings (`name=value; ...`), message, severity, owner, status (`active` or `retired`) and the last change (on, by, reason). Rows are retired, never deleted, so the sheet is the history; the file itself is versioned with the study. A row the app cannot read (an unknown type, a mistyped condition, a duplicate ID) is skipped and listed on the Data load page, and a check that fails on the data (a field the form does not have) shows as *not run* with the reason; neither stops the other checks. The workbook carries a Check types help sheet and a How to sheet, with drop-down lists for type, tier, severity, owner and status.

**New, changed and back for review.** Every check flags each record that meets its condition on the review date, so "new AE" is the AE check's findings that are new since the earlier date, and the app shows them first. A finding's hash covers the fields the check watches (for AEs: term, dates, severity, seriousness, causality, outcome), so a worsened AE comes back for review and an edit to an unrelated field does not. The review queue holds clinical flags new or changed in the window, plus any reviewed flag whose data has changed since.

**Check metadata**

| Field | Purpose |
| --- | --- |
| ID | Stable identifier, e.g. `XF-AE-001` |
| Name and description | Shown in the app and on the workbook contents sheet |
| Category | Grouping for screens and the workbook: Safety, Dosing, Visits, Disposition, Efficacy, Data, Data load |
| Tier | Structural, data, cross-form, reconciliation or clinical (below) |
| Applies to | Form type or named forms the check needs |
| Timing | Load (once per extract) or as-of (re-run for each review date) |
| Severity | Error, warning or info |
| Parameters | Values set in study.yml, e.g. a threshold |
| Owner | Who acts on findings: data management, clinical, safety, sites |

**Tiers**

1. **Structural:** required fields present, dates parse, keys unique, subjects exist, visits match the schedule.
2. **Cross-form:** consistency across forms the EDC cannot easily check, such as an AE starting before first dose, a con med with no matching indication, or a visit entered without its assessment. These were the core of the 2017 edit-check workbook.
3. **Reconciliation:** a generic check type that matches two sources by key within a date tolerance (PK samples against dosing and visits, central reads and diary entries against visits) and reports unmatched or mismatched records. SAE reconciliation with the safety database follows in version 2.
4. **Clinical:** study-configured rules a reviewer wants flagged, such as a worsening score, a value over a threshold or low dosing compliance.

**Findings shape.** Every finding has its own key: check ID + subject + anchor (the source record's `FORMOID + RECID`, or subject + visit for missing-data findings such as an overdue visit) + field. It also carries the source VERSION, the value, a short message and a hash of only the fields the check depends on. Findings inherit EDC links and New/Changed status, and their review marks reset only when a relevant field changes.

**Check types and checks.** A check type is one generic, unit-tested function with declared parameters; a check is a configured instance of a type with the metadata above. The library ships nine types: `record` (every record that meets the conditions, with the fields to watch), `visit_status` (scheduled visits that are missed, overdue or out of window on the review date), `parse_log` (values the mapping could not use, by transform), `subject_exists`, `missing_value`, `date_order`, `range` (bounds are numbers or other fields), `flag_when` and `reconcile` (match two forms by key, optionally only at visits a schedule column flags, with a date tolerance, one or both ways). Types that select records take `when`, a list of conditions (`field`, `op`, `value`) that must all hold. A parameter written `$name` reads the study's threshold `name`. Structural checks STR-001 to STR-006 ship ready-made and always run with a check spec. Each check runs on its own: one that fails is reported as *not run* and the rest carry on.

A study without a check spec can still list its checks in study.yml (`checks`), and a study can register its own check types in a study folder, without touching the package.

## Display library

Screens are built from generic Shiny modules that work off form types, configured per study in study.yml and filtered by the user's role in the core. Each module declares the form types and fields it needs, and the app only shows a screen when that data exists, so a study with no PK simply has no PK view.

**Layout.** The app is organised around the review a reviewer has to get through, not around the order screens were built, and kept quiet: white ground, one blue for actions, colour only where it carries meaning. A sidebar lists every screen under a few headings, so any screen is one click away: My review and All changes at the top, then Subjects (subject list, visit status, full profile), Safety (adverse events, PK, dosing, clinical flags), Efficacy, Data quality (data checks, queries, coding), Operations (sites, lock readiness) and Close-out (sign-off, audit trail, data load). On a phone the sidebar folds away. Above every screen sit its title and one row of controls: review date, changes since, site, a subject search and the review workbook.

**My review** is an inbox: the subjects with new, changed or deleted clinical data in the review window, or a clinical flag new in it, are listed on the left (still to review first, with chips for what changed and which flags fired), and the selected subject is on the right, new data first. **Reviewed, next** marks the subject and opens the one below it.

**Subject panel.** On every other screen, a subject ID (a table, the timeline, the search box, `?subject=`) opens the subject in a side panel over the current screen, so closing it returns the reviewer to the same place. The panel and the inbox show the same view: what is new first: clinical flags, then each form's new, changed and deleted records with changed values highlighted (the earlier value on hover), then unresolved queries, data findings and all of the subject's data by form, with a timeline by study day. **Reviewed, next** marks every item of the subject still to review (each record and clinical flag gets its own mark, with the chosen outcome and comment) and opens the next subject in the queue; keys m (reviewed, next), n and p move through the queue without the mouse.

**Review cycle and sign-off.** Medical review works subject by subject. Review marks on records and flags give each subject's status in the window (reviewed when every item has a current mark), and so the queue's progress. When nothing in the window is left to review, the reviewer signs off the cycle for the whole study: the sign-off records the scope hash of every item reviewed, and its review date becomes the default "changes since" for the next cycle. A per-subject "medical review complete" before database lock is a separate close-out step. Signing needs re-authentication through the hosting platform's single sign-on; until that is connected, only simulated studies can sign, as a demonstration.

**Roles (version 2).** Version 1 gives every user the same layout. Roles will come from the login: when a user is granted access they get a role, and the role sets the home queue (for example data managers land on open data findings, CRAs on their site), the default filters and the sign-off type. The sidebar configuration does not change.

| Module | Works on | Shows |
| --- | --- | --- |
| Review queue | All forms | My review: subjects with new data or open clinical flags in the window, progress, and the record and finding changes |
| What's new | All forms | New, changed and deleted records since the last review, by priority |
| Subject list | Subject data | Every subject with status, reference dates, review status and open items |
| Visit status grid | Visit forms | Subject by visit: complete, out of window, overdue, missed; no expected visits after discontinuation |
| Over-time chart | Visit and vendor findings | A chosen parameter by visit: individual lines, mean with CI, reference bands |
| Event timeline | Log forms | Events by study day, colored by a chosen field (e.g. severity) |
| Listing | Any form | Filterable table with record status and review marks |
| Site scorecard | All forms | Entry timeliness, overdue visits, query rate and time to close, flags per site |
| Subject profile | All forms | One subject across every form, on a time axis anchored on first dose |
| Check results | Check library | Findings by check, with counts and review status |
| Coding review | Coded log forms | Verbatim term against coded term, with consistency across subjects |
| Lock readiness | All forms | Per subject and form: open queries, missing data, PI signature, SDV, freeze and lock status |
| Queries | Query report | Every EDC query with its status on the review date, unresolved and oldest first |
| Sign-off, audit trail | Review store | Sign-off of the review cycle and its history; every mark and sign-off, newest first |
| Stack | Other modules | Several modules on one page, e.g. the AE timeline above the AE listing |

**Configuration.** study.yml `displays` lists the screens in order: each has an `id`, a `module`, a `title`, an optional `group` (the sidebar heading it sits under) and module `params` (for example `visit_grid` takes the visit form, its date field, the not-done flag and a fallback anchor such as planned Day 1 for subjects not yet dosed). The core supplies everything a screen computes (`review_data()` per review date, `visit_status()`), so the app only presents. Screens accept deep links: `?tab=<id>` and `?subject=<id>`. The site scorecard links each site to the site's own page (its subjects, next and overdue visits, unresolved queries, forms awaiting SDV), which sets Site for every screen. The review workbook exports the whole review for the review date and site in one Excel file: a contents sheet linking to one sheet per check (description, settings, findings with review status), a patient profile sheet with every subject's data one row per field (filter on Subject; derived fields labelled), and an About sheet; each export is logged.

**Operational reports.** The EDC query report and form status report are mapped like any source, with form type `report`, which keeps them out of clinical screens (what's new, subject profile). Their status is derived against the review date from their dates (opened, answered, closed; SDV, PI signature, freeze, lock), and dates after the review date count as not yet happened, so an earlier review date shows the status as it stood then. The query report needs the queried form's record key fields so each query links to its record; listings then show unresolved queries per record (study.yml `app: queries` names the report). Lock readiness counts findings from data checks, not clinical review flags; vendor data has no EDC form status, so a subject counts as locked when every EDC form is locked and the vendor data has no blockers. Coding review compares normalized verbatim terms, flags a verbatim coded to more than one term, and flags codes from an older dictionary version than the latest in use. To confirm with Medrio: whether the form status report carries the SDV, signature, freeze and lock dates, or only the current status.

**Example configuration** for an over-time chart: parameter = IGA from the IGA form, x = nominal week, shade the run-in, show mean and 95% CI, minimum n = 3.

A study that needs something genuinely specific registers a **plug-in module** in its study folder, using the same contract. Every module shares the same building blocks: subject links that open the profile, a review-mark column and an Excel button.

## Review state

Review state is the record of sponsor oversight: who reviewed what, when and with what outcome. Review marks and sign-off are GxP electronic records, so they follow 21 CFR Part 11 and EU Annex 11; the 2017 workbook could only infer review from query status.

- **Review marks** on a record or a finding, with an optional comment and an outcome (no action, query raised in the EDC, escalated). A mark resets only when a field it depends on changes.
- **Sign-off (version 1)** of a review cycle, for example the weekly medical review of an extract. It records name, UTC time, the meaning of the signature and a hash of the scope reviewed, after re-authentication through single sign-on.
- **Audit trail**, append-only: every mark, sign-off and change, with user, UTC time, old and new values. The application account can insert but never update or delete. Views and exports go to a separate access log.
- **Queries stay in the EDC.** The framework shows query status from the EDC report and links to the record; it does not raise queries itself.

**Insert-only store.** Every table is insert-only, enforced by database triggers (and, in production, by granting the application role only INSERT and SELECT). Setting, changing or clearing a mark adds a row; the current mark is the latest row per target. A mark stores the hash of the values it depends on (a finding's DEPHASH, or a record's hash of the fields the change log compares), and shows as *needs re-review* when that hash no longer matches. Saving a mark can name the mark the reviewer was looking at, so a stale screen cannot silently overwrite a colleague's mark. Sign-off stores user, printed name, UTC time, meaning and an order-independent hash of the scope (keys and hashes reviewed), and refuses a re-authentication older than five minutes; the single sign-on itself is the app's job. Comparing the findings of the last sign-off's review date with today's classifies each as New, Changed, Persisting or Resolved.

**Storage.** Written through R's standard database layer (DBI). `review_store` in study.yml selects Postgres for production, with backups and database roles, or SQLite for demos only. The app and its database are the single record of review; every export shows marks read-only.

## Outputs

**Excel review workbook.** Produced by one writer in the core package, whether run by the pipeline or from an Excel button in the app, so it always matches the app. Every export is scoped to the requesting user's role, carries a watermark (user, time, data cut) and is written to the access log. Its structure follows the 2017 edit-check workbook:

| Sheet | Contents |
| --- | --- |
| About | Study, review date, changes-since date, filters, data sources and extract dates, generation time |
| Contents | One hyperlinked row per sheet: check or listing name, description, owner, finding count, new count |
| One sheet per check | Findings with autofilter and frozen header; first column links to the record in the EDC |
| One sheet per listing | Monitored forms (AEs, visits, PK and so on) at collection grain |
| Site metrics | Entry timeliness by site, query aging, overdue visits, with charts |

Every data row carries the same status columns: record status (New, Changed, Deleted or Unchanged since the last review), changed fields with old values, query status from the EDC, and review mark and comment (read-only). New and changed rows are highlighted so a reviewer can filter straight to them.

**Timeliness anchors.** As in the 2017 paper, entry timeliness is measured three ways: first data point entered for a visit, all visit data entered, and all critical data entered. Critical fields are flagged per form in study.yml. True entry times come from the EDC audit log, a version 1 requirement; where only extract dates exist, lag is shown at weekly precision and labelled with its basis (audit log, record timestamp or extract).

**EDC links.** Built from a URL template in study.yml with the record keys filled in. Medrio's documentation shows no record-level link format, so unless Medrio confirms one, the column shows site, subject, visit and form, enough to go straight to the record in Medrio.

**PDF patient profiles (version 2).** One bookmarked page per subject, generated for a chosen review set (all, a site, or subjects with SAEs, withdrawals, worsening scores, low compliance or threshold alerts). Each page shows a header with demographics and status, then dosing, efficacy, AEs by severity, con meds and PK on one time axis anchored on first dose.

## Pipeline, deployment and access

**Batch pipeline, thin app.** A scheduled pipeline (built with the `targets` package) runs connectors, mapping, versioning and load-time checks, then writes the CDM and findings as parquet files per extract. The app only reads those files and computes as-of views. This keeps the app fast, makes the pipeline the thing that is validated, and lets the same results feed the workbook and profiles.

**Extract intake.** Each extract arrives with a manifest of expected files, row counts and checksums. A run is quarantined, and the previous data stays live, if a file is missing, a form's row count drops by more than a configured share, or a required column disappears.

**Run log and alerts.** Every run records its status, extract, package versions (pinned per study with renv) and hashes of the spec and config. The app and every export show "data as of" and the last successful run; a failed or quarantined run emails the study's data manager. One documented command reruns or backfills an extract.

**Spec changes mid-study.** A mapping spec change triggers a deterministic rebuild of the version history from the stored extracts. Review marks carry over by finding key and field hash, so only records whose relevant values change need review again.

**Refresh.** Nightly or weekly per study, set in study.yml. Each run keeps its extract folder, so any past review can be reproduced exactly.

**Multi-study.** One app instance serves all studies; a study selector picks the config folder. A later portfolio view compares operational metrics (enrollment, entry timeliness, open queries) across studies using only the standard header.

**Hosting.** Posit Connect or an equivalent internal server, behind company single sign-on. Data stays in the company's own storage; no public hosting for patient-level data.

**Close-out.** At database lock, `archive_study()` writes a locked bundle for the trial master file: extracts, collected data, review database, audit trail, config and package versions, with a manifest and checksums. The study then becomes read-only.

**Access, blinding and privacy.** Roles in study.yml are applied in the core, so the app, the workbook and PDFs are all role-scoped. The mapping spec flags fields as `blinded` or `phi`; they are hidden or masked unless a role is granted them, and aggregate efficacy displays are limited to a named role. Each study sets a retention and distribution policy for exported files.

## Testing and validation

The package is tested in five layers, all run in continuous integration:

1. **Round trip:** a simulated study is written out as raw source files, read back through connectors and mapping, and must reproduce the simulation exactly.
2. **Corruption tests:** source files are deliberately broken (bad dates, orphan subjects, unknown visits, missing columns, truncated files) and intake and structural checks must catch each one.
3. **Unit tests:** every transform, derivation, library check and the as-of engine against hand-built cases.
4. **Spec consistency:** the mapping spec must cover every source field and every CDM variable, and check IDs must match between spec and registry.
5. **Contract tests:** header-only fixtures of each real export layout (no data) fail the build if Medrio or a vendor changes its format.

**Validation.** The open-source package is not validated for any sponsor. Each sponsor validates its installation, risk-based, for its intended use: the system that records data review, not a read-only viewer. The package ships requirements and functional spec templates, a risk assessment, a traceability matrix from requirements to tests, and `validate_install()` for installation and operational qualification.

**Change control.** Study configuration (study.yml, visit schedule, mapping spec) lives in git and changes only through a reviewed pull request; its version is stamped on every run. Study-specific code (derive functions, custom checks, plug-in modules) ships with its own tests in the study folder, run in continuous integration, which keeps "validate the core once" true. The package follows semantic versioning, documents its public contracts for checks, derivations and modules, and gives an upgrade checklist for running studies.

## Roadmap and open decisions

**Version 1:** collected data model with record keys and crosswalk; Medrio and CSV/SAS connectors with extract intake checks; mapping spec compiler; as-of engine and change log; standard derivations; structural, cross-form, reconciliation and clinical checks; display modules (review queue with subject panel, what's new, subject list, visit status, over-time chart, event timeline, listing, site scorecard and site page, subject profile, check results, queries, coding review, lock readiness); review marks, sign-off and audit trail; role-scoped Excel workbook; run log, alerts and study archive; validation templates; a simulated example study and documentation site.

**Version 2:** roles from the login, each with its own home queue and defaults; PDF patient profiles; CDISC ODM connector; SAE reconciliation with the safety database; weekly digest emails and risk indicators with thresholds (KRIs, QTLs); cross-study portfolio view; optional SDTM export, possibly through sdtm.oak.

**Open decisions**

- [x] Package split, decided: two packages. A core package (connectors, mapping, as-of engine, checks, Excel workbook) that changes rarely and is validated once, and an app package with the Shiny modules that can evolve without re-validating the core
- [x] Mapping spec authoring, decided: Excel only, compiled to YAML by the pipeline. One source of truth that data managers can read and review; the YAML is generated, never edited by hand
- [ ] Medrio, questions for Medrio or the DM vendor: (1) can the relational export be scheduled and delivered to SFTP or cloud storage; (2) does it include a record ID and record-level created/modified timestamps per row; (3) is ODM export available through the API, and does it include audit records; (4) can the query report and data audit log be exported on a schedule; (5) is there a URL format to open a specific subject or form; (6) who owns the Medrio license and the API service account, the sponsor or the DM vendor
- [x] Review state storage, decided: both from version 1 through one database layer (DBI), chosen by `review_store` in study.yml. SQLite is the default for demos and single users; Postgres for team use, where several reviewers write at the same time. Remaining: who hosts Postgres (IT or a managed cloud database)
- [x] License, decided: Apache 2.0 (permissive, with an explicit patent grant)
- [ ] Project name: deferred, working name for now

* [ ] Open-source governance: CONTRIBUTING and GOVERNANCE files, release channel (r-universe, then CRAN), pharmaverse alignment; no client or employer names in any public material

**Questions to answer before coding**

1. What does a real Medrio export contain: a stable record ID per row, created and modified timestamps, deleted-record handling, and a scheduled audit-log export? Get a dummy-study export; keys, versioning and timeliness depend on it.
2. Are review marks and sign-off the formal record of data and medical review shown to inspectors? This sets the Part 11 scope.
3. Which package owns each layer, and what, if anything, runs only in the Shiny process?
4. Who approves study configuration and study-specific code, given outsourced data management and programming?
5. Which data are blinded or PHI, which roles see them, and may exported files leave the hosted environment?
