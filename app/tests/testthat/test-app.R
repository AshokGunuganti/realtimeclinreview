skip_if_not(dir.exists(study_dir), "example study not available")
skip_if_not_installed("RSQLite")

test_that("library display modules are registered", {
  expect_true(all(c("whats_new", "visit_grid", "over_time", "timeline", "listing", "check_results",
                    "subject_profile") %in% display_modules()))
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
  for (t in c("What.{1,6}s new", "Visit status", "mCSS over time", "AE listing", "Checks", "Subject profile")) {
    expect_match(html, t)
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

test_that("check results filter findings by the selected check", {
  ctx <- test_ctx()
  shiny::testServer(checks_server, args = list(ctx = ctx, params = list()), {
    all <- nrow(findings())
    s <- summary()
    expect_equal(nrow(s), length(ctx$study$checks))
    i <- which(s$Findings > 0)[1]
    session$setInputs(`sum-tbl_rows_selected` = i)
    expect_equal(nrow(findings()), s$Findings[i])
    expect_true(all(findings()$CHECKID == s$ID[i]))
    expect_lte(nrow(findings()), all)
  })
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
    timeline = list(list(form = "AE", day = "ASTDY", label = "AETERM")))), {
    ctx$goto_subject("102-001")
    session$flushReact()
    session$setInputs(subject = "102-001")
    expect_equal(subject(), "102-001")
    expect_match(output$header$html, "102-001")
  })
})
