skip_if_not(dir.exists(study_dir), "example study not available")
skip_if_not_installed("RSQLite")

test_that("library display modules are registered", {
  expect_true(all(c("whats_new", "visit_grid", "over_time", "timeline", "listing", "review_summary", "check_catalogue",
                    "subject_profile", "site_scorecard", "coding_review", "lock_readiness") %in% display_modules()))
})

test_that("displays whose data is missing are left out", {
  rd <- review_cache(example_study())(as.Date("2026-12-31"))
  cfg <- list(list(id = "a", module = "listing", params = list(form = "AE")),
              list(id = "b", module = "listing", params = list(form = "CM")),
              list(id = "c", module = "over_time", params = list(form = "PK", value = "NOPE")),
              list(id = "d", module = "timeline", params = list(form = "AE", day = "ASTDY", label = "AETERM")))
  expect_equal(names(resolve_displays(cfg, rd)), c("a", "d"))
  expect_error(resolve_displays(list(list(id = "x", module = "nope")), rd), "unknown module 'nope'")
})

test_that("the example study's app builds with every configured screen", {
  app <- rtcr_app(study_dir, user = "tester", store = list(driver = "sqlite", path = ":memory:"))
  expect_s3_class(app, "shiny.appobj")
  st <- example_study()
  displays <- resolve_displays(st$config$displays, review_cache(st)(st$extract_date))
  expect_equal(length(displays), length(st$config$displays))
  html <- as.character(app_ui(st, displays, st$config$app, c("101", "102")))
  for (t in c("Action items", "Checks", "Visit status", "mCSS over time", "AE listing",
              "Subject profile", "Sites", "AE coding", "Lock readiness", "Sign-off", "Audit trail")) {
    expect_match(html, t)
  }
  expect_false(grepl("tab=review|tab=flags|tab=changes", html))         # folded into Checks, or out for now
})

test_that("the sidebar lists every screen under a closed group, in the order the groups first appear", {
  st <- example_study()
  displays <- resolve_displays(st$config$displays, review_cache(st)(st$extract_date))
  html <- as.character(app_ui(st, displays, st$config$app, "101"))
  groups <- regmatches(html, gregexpr('<details class="side-sec">\\s*<summary class="side-group"[^>]*>[^<]+</summary>',
                                      html))[[1]]
  expect_match(groups[2], "--area:#d55181")                              # Safety's accent
  expect_equal(gsub("<[^>]+>|\\s{2,}|\n", "", groups), c("Study conduct", "Safety", "Efficacy", "Data quality",
                                                         "Operations", "Close-out"))
  expect_false(grepl("<details[^>]*open", html))                         # all start collapsed
  ungrouped <- sub('<details class="side-sec".*', "", html)
  expect_match(ungrouped, "tab=actions")                                  # ungrouped screens stay visible
  expect_match(ungrouped, "tab=checks")
  expect_match(ungrouped, 'class="side-link side-featured" href="\\?tab=profile"')   # its own tab, highlighted
  expect_false(grepl("tab=mcss", ungrouped))
  links <- regmatches(html, gregexpr('class="side-link[^"]*" href="\\?tab=[^"]+"', html))[[1]]
  expect_equal(sub('.*tab=', "", sub('"$', "", links)), names(displays))
  expect_match(html, "bslib-sidebar-layout")
  expect_false(grepl("navbar-nav", html))                                # no top menu bar
  for (id in c("asof", "since", "site", "find", "workbook", "panel-oc", "page_title")) {
    expect_match(html, sprintf('id="%s"', id))
  }
})

test_that("what's new lists record and finding changes in the window", {
  ctx <- test_ctx()
  shiny::testServer(whatsnew_server, args = list(ctx = ctx, params = list(priority = list("AE"))), {
    d <- changes()
    expect_gt(nrow(d), 0)
    expect_true(all(d$RECSTAT %in% c("New", "Changed", "Deleted")))
    expect_equal(d$FORMOID[1], "AE")                        # priority form first
    expect_false(anyNA(d$DEPHASH[d$RECSTAT != "Deleted"]))
    x <- rec_tbl()
    expect_true(all(c("Status", "Form", "Subject", "Review", ".KEY", ".DEPHASH") %in% names(x)))
    expect_match(x$Review[1], "not reviewed")
    expect_s3_class(findings(), "data.frame")
  })
})

test_that("listings head their columns with labels, not variable names", {
  ctx <- test_ctx()
  shiny::testServer(listing_server, args = list(ctx = ctx, params = list(form = "AE")), {
    x <- tbl()
    expect_true(all(c("Reported term", "Severity", "Start study day", "Treatment emergent") %in% names(x)))
    expect_false(any(c("AETERM", "AESEV", "ASTDY") %in% names(x)))
    expect_true(".KEY" %in% names(x))
  })
  expect_equal(names(label_columns(data.frame(AESEV = 1), list())), "AESEV")      # a study without labels
  expect_equal(changed_text(1, data.frame(RECID = 1, VARIABLE = "AESEV", OLD = "MILD", NEW = "MODERATE"),
                            ctx$study$labels), "Severity: MILD \u2192 MODERATE")
})

test_that("marking selected rows writes review marks and refreshes the table", {
  ctx <- test_ctx()
  shiny::testServer(listing_server, args = list(ctx = ctx, params = list(form = "AE")), {
    n0 <- nrow(tbl())
    session$setInputs(`tbl-tbl_rows_selected` = c(1, 2), `tbl-outcome` = "query raised",
                      `tbl-comment` = "Check onset date")
    session$setInputs(`tbl-mark` = 1)
    m <- rtcrcore::current_marks(ctx$con, ctx$study$id, "record")
    expect_equal(nrow(m), 2)
    expect_equal(unique(m$OUTCOME), "query raised")
    expect_equal(unique(m$USERID), "tester")
    x <- tbl()
    expect_equal(nrow(x), n0)
    expect_equal(sum(grepl("reviewed\\b", x$Review) & !grepl("not reviewed", x$Review)), 2)
    expect_match(x$`Marked by`[x$.KEY %in% m$TARGET_KEY][1], "tester: query raised \\(Check onset date\\)")
  })
})

test_that("the summary sends a check to its tab and keeps only checks with no tab", {
  ctx <- test_ctx()
  shiny::testServer(summary_server, args = list(ctx = ctx, params = list(exclude_tiers = list("structural"))), {
    s <- summary()
    expect_setequal(unique(findings()$CHECKID), intersect(c("DT-FUT", "DS-DISC"), s$ID[s$Findings > 0]))
    session$setInputs(pick = "AE-SEV")                                     # has a tab: opens it, picked there
    expect_equal(ctx$check_focus()$tab, "ae")
    expect_equal(ctx$check_focus()$check, "AE-SEV")
    session$setInputs(pick = "DS-DISC")                                    # no tab: its findings stay here
    expect_true(all(findings()$CHECKID == "DS-DISC"))
    expect_equal(nrow(findings()), s$Findings[s$ID == "DS-DISC"])
    session$setInputs(all = 1)
    expect_false("AE-SEV" %in% findings()$CHECKID)
  })
})

test_that("each check links to the first tab that shows its form", {
  st <- example_study()
  tabs <- check_tab_map(st, st$config$displays)
  expect_equal(unname(tabs[c("AE-SEV", "AE-COD", "EX-COMP", "CL-PK-001", "CL-IGA-001", "VS-MISS")]),
               c("ae", "ae", "diary", "pk", "iga", "visits"))
  expect_true(all(is.na(tabs[c("DS-DISC", "DT-FUT")])))                    # no screen shows DS; DT-FUT has no form
  expect_false(any(grepl("^STR-", names(tabs))))                         # programmer checks never link
  coding <- Filter(function(d) d$id == "coding", st$config$displays)[[1]]
  expect_true(all(c("AE-SEV", "AE-COD") %in% display_checks(st, coding)))  # every screen with the form carries them
  profile <- Filter(function(d) d$id == "profile", st$config$displays)[[1]]
  expect_length(display_checks(st, profile), 0)                           # the profile is the reviewer's own view
})

test_that("a tab's checks card lists its checks and is picked from a check link", {
  ctx <- test_ctx()
  shiny::testServer(form_checks_server, args = list(ctx = ctx, check_ids = c("EX-COMP", "DM-DRY-001"), tab = "diary"), {
    expect_true(all(tbl()$Check %in% c("EX-COMP", "DM-DRY-001")))
    expect_match(output$chips$html, "All checks")
    expect_match(output$chips$html, "Low dosing compliance")
    ctx$check_focus(list(tab = "diary", check = "EX-COMP", at = Sys.time()))
    session$flushReact()
    expect_equal(picked(), "EX-COMP")
    expect_true(all(tbl()$Check == "EX-COMP"))
    ctx$check_focus(list(tab = "pk", check = "CL-PK-001", at = Sys.time()))  # another tab's check: ignored
    session$flushReact()
    expect_equal(picked(), "EX-COMP")
    session$setInputs(pick = "")
    expect_null(picked())
  })
})

test_that("the checks catalogue lists active checks with their change record and tab", {
  ctx <- test_ctx()
  cat <- shiny::isolate(check_catalogue(ctx$study, ctx$data(), character(), ctx$check_tabs, ctx$tab_titles))
  expect_false(any(grepl("^STR-", cat$ID)))
  expect_true(all(cat$Status == "active"))
  ex <- cat[cat$ID == "EX-COMP", ]
  expect_equal(ex$Tab, "Dosing diary")
  expect_equal(ex$Description, "Dosing interval with compliance below the study threshold")
  expect_equal(ex$Reason, "Standard check")
  expect_true(is.na(cat$Tab[cat$ID == "DS-DISC"]))

  # A retired row in the spec stays listed, with its reason.
  st <- ctx$study
  extra <- st$check_spec$table[st$check_spec$table$ID == "EX-COMP", ]
  extra$ID <- "EX-COMP-OLD"
  extra$STATUS <- "retired"
  extra$REASON <- "Threshold moved to 80%"
  st$check_spec$table <- rbind(st$check_spec$table, extra)
  cat2 <- shiny::isolate(check_catalogue(st, ctx$data(), character(), ctx$check_tabs, ctx$tab_titles))
  old <- cat2[cat2$ID == "EX-COMP-OLD", ]
  expect_equal(old$Status, "retired")
  expect_equal(old$Reason, "Threshold moved to 80%")

  shiny::testServer(catalogue_server, args = list(ctx = ctx, params = list()), {
    html <- output$groups$html
    expect_match(html, "Open Dosing diary")
    expect_match(html, "Findings on the review summary")
    expect_false(grepl("accordion-collapse collapse show", html))
    session$setInputs(open = "CL-PK-001")
    expect_equal(ctx$check_focus()$tab, "pk")
  })
})

test_that("new and updated data is the first check, across every clinical form", {
  ctx <- test_ctx()
  shiny::testServer(summary_server, args = list(ctx = ctx, params = list(exclude_tiers = list("structural"))), {
    fc <- form_counts()
    expect_equal(fc$Form, clinical_forms(ctx$study))
    expect_false(any(c("QUERY", "FSTAT") %in% fc$Form))                  # operational reports left out
    expect_gt(sum(fc$Total), 0)
    html <- output$groups$html
    expect_lt(regexpr("New and updated data", html), regexpr("Safety", html))   # first area
    expect_match(output$headline$html, "new or updated records")

    f <- fc$Form[fc$Total > 0][1]
    session$setInputs(pick_form = f)
    expect_equal(output$view, paste0("form:", f))
    session$setInputs(all = 1)
    expect_equal(output$view, "findings")

    # Findings say when their record is itself new or updated.
    x <- tbl()
    keys <- changed_keys()
    on_changed <- paste0(findings()$FORMOID, ":", findings()$RECID) %in% names(keys)
    expect_true(any(on_changed))
    expect_true(all(nzchar(x$Record[on_changed])))
    expect_true(all(x$Record[!on_changed] == ""))
  })
})

test_that("changed records list the other checks that flag them", {
  ctx <- test_ctx()
  shiny::testServer(listing_server, args = list(ctx = ctx, params = list(form = "AE", show = "changes", flagged_by = TRUE)), {
    x <- tbl()
    expect_true("Also flagged by" %in% names(x))
    expect_true(all(x$RECSTAT != "Unchanged"))
    expect_true(any(grepl("AE-NEW", x$`Also flagged by`)))
  })
  shiny::isolate({
    rd <- ctx$data()
    f <- rd$findings[!is.na(rd$findings$FORMOID), ][1, ]
    key <- paste0(f$FORMOID, ":", f$RECID)
    expect_match(flagged_by(rd, key), f$CHECKID)
    expect_equal(flagged_by(rd, "AE:nope"), "")
  })
})

test_that("the checks screen groups every review check by area, all closed at first", {
  ctx <- test_ctx()
  shiny::testServer(summary_server, args = list(ctx = ctx, params = list(exclude_tiers = list("structural"))), {
    s <- summary()
    expect_true(all(c("clinical", "data") %in% s$Tier))
    expect_false(any(s$Tier == "structural"))
    g <- check_groups(s)
    expect_equal(names(g)[1:2], c("Safety", "Efficacy"))
    expect_setequal(unlist(lapply(g, function(x) x$ID)), s$ID)
    expect_true(all(c("CL-IGA-001", "XF-IGA-001", "RC-MCSS-001") %in% c(g$Efficacy$ID, g$Data$ID)))
    html <- output$groups$html
    expect_match(html, "accordion")
    expect_false(grepl("accordion-collapse collapse show", html))       # every area starts collapsed
    expect_match(output$headline$html, "\\d+ checks")
  })
  expect_equal(names(check_groups(data.frame(Category = c("Zeta", "Safety", "Alpha"), New = 0, Findings = 0))),
               c("Safety", "Alpha", "Zeta"))
})

test_that("visit grid has one row per subject and one column per visit", {
  ctx <- test_ctx(asof = "2026-10-22")
  shiny::testServer(visitgrid_server, args = list(ctx = ctx, params = list(
    form = "SV", date = "VISDAT", not_done = list(field = "VISND", value = "Yes"),
    fallback = list(TRTSDT = "PLANDY1"))), {
    g <- grid()
    expect_equal(nrow(g), nrow(ctx$data()$adsl))
    expect_equal(ncol(g), 2 + nrow(ctx$study$schedule))
    expect_true(all(unlist(g[-(1:2)]) %in% c(names(status_colours), NA)))
    o <- overdue()
    expect_equal(nrow(o), sum(status()$STATUS == "Overdue / not entered"))
    expect_true(all(o$`Days overdue` > 0))
  })
})

test_that("site filter narrows every screen", {
  ctx <- test_ctx(site = "101")
  rd <- shiny::isolate(ctx$data())
  expect_true(all(rd$adsl$SITEID == "101"))
  expect_true(all(vapply(rd$tables, function(t) all(t$SITEID == "101"), logical(1))))
  expect_equal(sum(rd$summary$N), nrow(rd$findings))
})

test_that("over-time summary gives mean and 95% CI only with enough values", {
  d <- data.frame(SUBJID = c("a", "b", "c", "a"), VISITNUM = c(1, 1, 1, 2), Y = c(1, 2, 3, 5))
  s <- visit_summary(d, min_n = 3)
  expect_equal(s$N, c(3, 1))
  expect_equal(s$MEAN, c(2, 5))
  expect_equal(s$LO[1], 2 - stats::qt(0.975, 2) * 1 / sqrt(3))
  expect_true(is.na(s$LO[2]))
})

test_that("Excel export writes the data and an About sheet, and logs the export", {
  ctx <- test_ctx()
  f <- tempfile(fileext = ".xlsx")
  shiny::isolate(write_export(list(Test = data.frame(A = 1:2)), f, ctx))
  expect_equal(openxlsx::getSheetNames(f), c("Test", "About"))
  about <- openxlsx::read.xlsx(f, "About")
  expect_equal(about$Value[about$Item == "User"], "tester")
  expect_equal(about$Value[about$Item == "Review date"], "2026-12-31")
})

test_that("subject profile follows subject links", {
  ctx <- test_ctx()
  shiny::testServer(profile_server, args = list(ctx = ctx, params = list(
    lanes = list(list(type = "events", form = "AE", day = "ASTDY", label = "AEDECOD")))), {
    ctx$goto_subject("102-001")
    session$flushReact()
    session$setInputs(subject = "102-001")
    expect_equal(subject(), "102-001")
    expect_match(output$header$html, "102-001")
  })
})

test_that("what's new and the profile leave out operational reports", {
  st <- example_study()
  expect_false(any(c("QUERY", "FSTAT") %in% clinical_forms(st)))
  expect_true(all(c("AE", "SV") %in% clinical_forms(st)))
  ctx <- test_ctx()
  shiny::testServer(whatsnew_server, args = list(ctx = ctx, params = list()), {
    expect_false(any(changes()$FORMOID %in% c("QUERY", "FSTAT")))
  })
})

test_that("listings show unresolved queries on the queried record", {
  ctx <- test_ctx()
  q <- shiny::isolate(ctx$queries())
  open <- q[q$STATUS != "Closed" & q$QRYFORM == "SV", ]
  shiny::testServer(listing_server, args = list(ctx = ctx, params = list(form = "SV")), {
    x <- tbl()
    flagged <- x$.KEY[x$Queries != ""]
    expect_setequal(flagged, unique(open$TARGET))
    expect_match(x$Queries[x$.KEY == open$TARGET[1]], "1 open")
  })
})

sc_params <- list(visits = list(form = "SV", date = "VISDAT", not_done = list(field = "VISND", value = "Yes"),
                                fallback = list(TRTSDT = "PLANDY1")), query_age = 30, title = "Site scorecard")

test_that("site scorecard has one row per site, linking to the site's page", {
  ctx <- test_ctx()
  shiny::testServer(sites_server, args = list(ctx = ctx, params = sc_params), {
    s <- score()
    expect_setequal(s$SITEID, unique(ctx$data()$adsl$SITEID))
    expect_true(all(s$STATUS %in% c("Needs attention", "Watch", "On track")))
    expect_equal(sum(s$OPENQ), nrow(queries()))
    expect_true(all(diff(queries()$`Age (d)`) <= 0))                     # oldest first
    expect_match(tbl()$Site[1], "goto_site")
    expect_false(one())
  })
})

test_that("with one site the sites screen is that site's page", {
  ctx <- test_ctx(site = "111")
  shiny::testServer(sites_server, args = list(ctx = ctx, params = c(sc_params, form_status = "FSTAT")), {
    expect_true(one())
    x <- subj()
    expect_equal(nrow(x), nrow(ctx$data()$adsl))
    expect_true(all(c("Next visit", "Overdue", "Missed", "Unresolved queries", "Awaiting SDV", "Open findings") %in%
                      names(x)))
    expect_equal(sum(x$`Unresolved queries`), nrow(queries()))
    expect_equal(nrow(overdue()), sum(visits()$STATUS == "Overdue / not entered"))
  })
  v <- data.frame(SUBJID = c("1", "1", "1", "2"), VISIT = c("A", "B", "C", "A"),
                  STATUS = c("Overdue / not entered", "Not yet due", "Not yet due", "Missed"),
                  TARGDT = as.Date(c("2026-01-01", "2026-03-01", "2026-02-01", "2026-01-01")))
  x <- site_subjects(data.frame(SUBJID = c("1", "2"), EOSSTT = c("ONGOING", "DISCONTINUED")), v)
  expect_match(x$`Next visit`[1], "^C")                                   # earliest target date
  expect_equal(x$Overdue, c("A", ""))
  expect_equal(x$Missed, c(0, 1))
})

test_that("coding review lists inconsistent terms first and filters records by term", {
  ctx <- test_ctx()
  params <- list(form = "AE", verbatim = "AETERM", coded = "AEDECOD", soc = "AESOC", version = "MEDDRAV",
                 date = "AECODDT", title = "AE coding")
  shiny::testServer(coding_server, args = list(ctx = ctx, params = params), {
    t <- terms()
    expect_match(t$Consistent[1], "Inconsistent")
    expect_equal(t$Verbatim[1], "ITCHY SKIN AT GEL SITE")
    expect_equal(nrow(recs()), nrow(ctx$data()$tables$AE))
    session$setInputs(`terms-tbl_rows_selected` = 1)
    r <- recs()
    expect_equal(nrow(r), t$Records[1])
    expect_true(all(grepl("^AE:", r$.KEY)))
    expect_false(anyNA(r$.DEPHASH))
    expect_true(all(c("Flag", "Review", "Version") %in% names(r)))
  })
})

test_that("lock readiness rolls forms up to subjects", {
  ctx <- test_ctx()
  shiny::testServer(lock_server, args = list(ctx = ctx, params = list(form_status = "FSTAT")), {
    x <- lr()
    expect_false(any(x$FORMOID %in% c("QUERY", "FSTAT")))
    s <- subjects()
    expect_equal(nrow(s), length(unique(x$SUBJID)))
    expect_equal(s$STATUS[s$SUBJID == "109-001"], "Locked")             # withdrew in run-in
    expect_equal(sum(x$OPENQ), sum(shiny::isolate(ctx$queries())$STATUS != "Closed"))
    i <- which(s$SUBJID == "109-001")
    session$setInputs(`subj-tbl_rows_selected` = i)
    expect_equal(nrow(ftbl()), sum(x$SUBJID == "109-001"))
  })
})

test_that("review workbook has contents, a sheet per check and a filterable patient profile", {
  ctx <- test_ctx(site = "111")
  rd <- shiny::isolate(ctx$data())
  f <- rd$findings[1, ]
  rtcrcore::set_mark(ctx$con, ctx$study$id, "finding", f$FINDINGKEY, "tester", "escalated", comment = "Call site",
                     dephash = f$DEPHASH, subjid = f$SUBJID)
  path <- withr::local_tempfile(fileext = ".xlsx")
  review_workbook(rd, ctx$study, path, shiny::isolate(about_table(ctx)), ctx$con)

  review <- rd$summary[rd$summary$TIER != "structural", ]                 # programmer checks stay out
  sheets <- openxlsx::getSheetNames(path)
  expect_equal(sheets, c("Contents", review$ID, "Patient profile", "About"))
  expect_false(any(grepl("^STR-", sheets)))
  toc <- openxlsx::read.xlsx(path, "Contents", startRow = 4)
  expect_equal(toc$Findings[seq_len(nrow(review))], review$N)
  expect_equal(toc$Category[seq_len(nrow(review))], review$CATEGORY)
  expect_match(openxlsx::read.xlsx(path, "Contents", rows = 2, colNames = FALSE)[[1]],
               sprintf("site 111\\. %d checks, %d findings\\.", nrow(review), sum(review$N)))
  info <- openxlsx::read.xlsx(path, "AE-SAE", rows = 2:8, colNames = FALSE)
  expect_match(info[[2]][info[[1]] == "Last changed"], "canned set")

  chk <- openxlsx::read.xlsx(path, f$CHECKID, startRow = 11)
  expect_equal(nrow(chk), sum(rd$findings$CHECKID == f$CHECKID))
  expect_equal(chk$Review[1], "reviewed")
  expect_equal(chk$Marked.by[1], "tester: escalated (Call site)")
  empty <- setdiff(review$ID, rd$findings$CHECKID)[1]
  expect_match(openxlsx::read.xlsx(path, empty, rows = 11, colNames = FALSE)[[1]], "No findings")

  p <- openxlsx::read.xlsx(path, "Patient profile")
  expect_setequal(unique(p$Subject), rd$adsl$SUBJID)
  expect_false(any(p$Form %in% c("QUERY", "FSTAT")))
  ae <- rd$tables$AE
  expect_equal(sum(p$Form == "AE"), nrow(ae) * (ncol(ae) - sum(names(ae) %in% c(rtcrcore::cdm_header_vars(), "DELFL"))))
  expect_true(all(p$Source[p$Form == "AE" & p$Field == "ASTDY"] == "Derived"))
  expect_true(all(p$Source[p$Form == "AE" & p$Field == "AETERM"] == "Collected"))
  wb <- openxlsx::loadWorkbook(path)
  expect_match(wb$worksheets[[which(names(wb) == "Patient profile")]]$autoFilter, "A1:L1")
})

test_that("sheet names are legal, unique and do not clash with fixed sheets", {
  n <- check_sheet_names(c("A[1]:b*?/\\c", "Contents", "X", "X", strrep("y", 40)))
  expect_equal(n, c("A_1__b____c", "Contents_", "X", "X_1", strrep("y", 27)))
  expect_true(all(nchar(n) <= 31))
})
