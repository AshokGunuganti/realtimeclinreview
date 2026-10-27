# Review queue, subject panel, sign-off and the screens added with the menus.

skip_if_not(dir.exists(study_dir), "example study not available")
skip_if_not_installed("RSQLite")

test_that("record changes cover clinical forms only, with record hashes for live records", {
  st <- example_study()
  ch <- record_changes(st, as.Date("2026-10-22"), as.Date("2026-12-31"))
  expect_true(all(ch$FORMOID %in% clinical_forms(st)))
  expect_true(all(ch$RECSTAT %in% c("New", "Changed", "Deleted")))
  h <- record_hash_table(st, "AE", as.Date("2026-12-31"))
  ae <- ch[ch$FORMOID == "AE", ]
  expect_equal(ae$DEPHASH, h$DEPHASH[match(ae$KEY, h$TARGET_KEY)])      # same hash as listings use
  one <- record_changes(st, as.Date("2026-10-22"), as.Date("2026-12-31"), "111")
  expect_true(all(one$SITEID == "111"))
  expect_equal(nrow(record_changes(st, as.Date("2026-12-31"), as.Date("2026-12-31"))), 0)
})

items_fixture <- function() {
  data.frame(TYPE = c("record", "record", "record", "finding", "record"),
             SUBJID = c("A", "A", "B", "C", "D"), SITEID = "1",
             FORMOID = c("SV", "AE", "SV", "PK", "SV"), KEY = paste0("k", 1:5), DEPHASH = "h", VERSION = 1,
             RECSTAT = c("New", "Changed", "New", NA, "New"), CHECKID = c(NA, NA, NA, "CL-PK-001", NA),
             REVIEW_STATUS = c("not reviewed", "reviewed", "reviewed", "not reviewed", "not reviewed"),
             MARK_USER = c(NA, "u1", "u2", NA, NA), MARK_AT = c(NA, "2026-12-01", "2026-12-02", NA, NA),
             stringsAsFactors = FALSE)
}

test_that("a window with nothing to review gives an empty queue", {
  ctx <- test_ctx(since = "2026-12-31")
  shiny::isolate({
    it <- ctx$review_items()
    expect_equal(nrow(it[it$TYPE == "record", ]), 0)
    expect_true(all(it$TYPE == "finding"))
    empty <- queue_items(ctx$changes(), ctx$data()$findings[0, ], ctx$con, ctx$study$id)
    expect_equal(nrow(empty), 0)
    expect_equal(nrow(subject_queue(empty, NULL)), 0)
  })
})

test_that("the subject queue puts flagged and priority subjects first, reviewed last", {
  q <- subject_queue(items_fixture(), NULL, priority = c("AE", "PK"))
  expect_equal(q$SUBJID, c("C", "A", "D", "B"))
  expect_equal(q$STATUS, c("To review", "To review", "To review", "Reviewed"))
  expect_equal(q$CHANGES[q$SUBJID == "A"], "AE 1; SV 1")                 # priority form first
  expect_equal(q$FLAGS[1], "CL-PK-001")
  expect_equal(q$REVIEWED[q$SUBJID == "A"], 1)
  expect_equal(q$LASTBY[q$SUBJID == "B"], "u2")
  expect_equal(nrow(subject_queue(items_fixture()[0, ], NULL)), 0)
})

test_that("next in queue walks subjects to review, wrapping round", {
  q <- subject_queue(items_fixture(), NULL, priority = c("AE", "PK"))     # C, A, D to review; B reviewed
  expect_equal(next_in_queue(q), "C")
  expect_equal(next_in_queue(q, "C"), "A")
  expect_equal(next_in_queue(q, "D"), "C")                                # wraps
  expect_equal(next_in_queue(q, "B"), "C")                                # reviewed subject: next to review
  expect_equal(next_in_queue(q, "A", -1), "C")
  expect_equal(next_in_queue(q, "D", 1, to_review = FALSE), "B")          # browsing includes reviewed
  q$STATUS <- "Reviewed"
  expect_null(next_in_queue(q, "A"))
})

test_that("study day has no day 0", {
  expect_equal(study_day(as.Date(c("2026-01-01", "2026-01-02", "2025-12-31", NA)), as.Date("2026-01-01")),
               c(1, 2, -1, NA))
})

test_that("my review is an inbox: list on the left, the subject on the right, mark and move on", {
  ctx <- test_ctx()
  shiny::testServer(review_server, args = list(ctx = ctx, params = list()), {
    q0 <- ctx$review_queue()
    expect_gt(nrow(q0), 1)
    expect_equal(subj(), q0$SUBJID[1])                                    # first to review is shown
    expect_match(output$progress$html, sprintf("0 of %d subjects reviewed", nrow(q0)))
    expect_match(output$list$html, sprintf("To review \\(%d\\)", nrow(q0)))
    expect_match(output$head$html, q0$SUBJID[1])
    expect_match(output$body$html, "All data")
    session$setInputs(pick = q0$SUBJID[3])
    expect_equal(subj(), q0$SUBJID[3])
    session$setInputs(outcome = "no action", comment = "")
    session$setInputs(mark = 1)
    q <- ctx$review_queue()
    expect_equal(q$STATUS[q$SUBJID == q0$SUBJID[3]], "Reviewed")
    expect_equal(subj(), q0$SUBJID[4])                                    # next still to review
    expect_match(output$list$html, "Reviewed \\(1\\)")
    q <- ctx$review_queue()
    session$setInputs(prev = 1)
    expect_equal(subj(), q$SUBJID[match(q0$SUBJID[4], q$SUBJID) - 1])     # the one above in the list
    session$setInputs(pick = "not-a-subject")
    expect_equal(subj(), q0$SUBJID[1])                                    # falls back to the first to review
  })
  empty <- test_ctx(since = "2026-12-31")
  shiny::testServer(review_server, args = list(ctx = empty, params = list()), {
    if (!nrow(empty$review_queue())) {
      expect_null(subj())
      expect_match(output$body$html, "No new or changed data")
    }
  })
})

test_that("the subject panel marks every item of the subject and moves to the next", {
  ctx <- test_ctx()
  shiny::testServer(subject_panel_server, args = list(ctx = ctx, timeline = list(list(form = "AE", day = "ASTDY",
                                                                                      label = "AETERM"))), {
    q0 <- ctx$review_queue()
    first <- q0$SUBJID[1]
    open(first)
    expect_equal(ctx$selected_subject(), first)
    expect_match(output$head$html, first)
    expect_match(output$body$html, "All data")
    n_items <- sum(ctx$review_items()$SUBJID == first)
    session$setInputs(outcome = "no action", comment = "")
    session$setInputs(mark = 1)
    m <- rtcrcore::current_marks(ctx$con, ctx$study$id)
    expect_equal(nrow(m), n_items)
    expect_true(all(m$SUBJID == first & m$USERID == "tester"))
    q <- ctx$review_queue()
    expect_equal(q$STATUS[q$SUBJID == first], "Reviewed")
    expect_equal(subj(), q0$SUBJID[2])                                    # next subject to review
    session$setInputs(prev = 1)
    expect_equal(subj(), first)                                           # browsing reaches reviewed subjects
    session$setInputs(`next` = 1)
    expect_equal(subj(), q0$SUBJID[2])
  })
})

test_that("the panel shows clinical flags and highlights changed values", {
  ctx <- test_ctx()
  shiny::isolate({
    f <- clinical_findings(ctx$data())
    expect_gt(nrow(f), 0)
    html <- as.character(panel_body(ctx, f$SUBJID[1], list()))
    expect_match(html, "Clinical flags")
    expect_match(html, f$CHECKID[1])
    ch <- ctx$changes()
    s <- ch$SUBJID[ch$RECSTAT == "Changed"][1]
    html <- as.character(panel_body(ctx, s, list()))
    expect_match(html, '<mark title="was: ')
    expect_match(as.character(panel_head(ctx, s)), s)
  })
})

test_that("stacked displays render and run each item", {
  ctx <- test_ctx()
  params <- list(items = list(list(module = "timeline", title = "AE timeline",
                                   params = list(form = "AE", day = "ASTDY", label = "AETERM", color = "AESEV")),
                              list(module = "listing", title = "AE listing", params = list(form = "AE"))))
  html <- as.character(stack_ui("ae", params, example_study()))
  expect_match(html, "AE timeline")
  expect_match(html, 'id="ae-i2-tbl-tbl"')
  rd <- review_cache(example_study())(as.Date("2026-12-31"))
  expect_true(stack_needs(params, rd))
  expect_false(stack_needs(list(items = list(list(module = "listing", params = list(form = "CM")))), rd))
  expect_error(stack_items(list(items = list(list(module = "nope")))), "unknown module 'nope'")
  shiny::testServer(stack_server, args = list(ctx = ctx, params = params), {
    expect_true(TRUE)                                                     # both servers start
  })
})

test_that("check results can show only some tiers", {
  ctx <- test_ctx()
  shiny::testServer(checks_server, args = list(ctx = ctx, params = list(tiers = list("clinical"))), {
    expect_true(all(summary()$Tier == "clinical"))
    expect_true(all(findings()$CHECKID %in% summary()$ID))
  })
  shiny::testServer(checks_server, args = list(ctx = ctx, params = list(exclude_tiers = list("clinical"))), {
    expect_false(any(summary()$Tier == "clinical"))
    expect_gt(nrow(summary()), 0)
  })
})

test_that("subject list has every subject with review status and open items", {
  ctx <- test_ctx()
  shiny::testServer(subjects_server, args = list(ctx = ctx, params = list(title = "Subjects")), {
    x <- tbl()
    expect_equal(nrow(x), nrow(ctx$data()$adsl))
    expect_true(all(c("Study day", "Review", "Clinical flags", "Data findings", "Unresolved queries") %in% names(x)))
    expect_equal(sum(x$`Clinical flags`), nrow(clinical_findings(ctx$data())))
  })
})

test_that("queries list unresolved queries first, oldest first", {
  ctx <- test_ctx()
  shiny::testServer(queries_server, args = list(ctx = ctx, params = list(form = "QUERY", title = "Queries")), {
    x <- q()
    un <- x$STATUS != "Closed"
    expect_true(all(un[seq_len(sum(un))]))
    expect_true(all(diff(x$AGE[un]) <= 0))
    expect_equal(nrow(tbl()), nrow(ctx$queries()))
  })
})

test_that("sign-off waits until everything is reviewed, then records the scope", {
  ctx <- test_ctx(since = "2026-12-20")
  shiny::testServer(signoff_server, args = list(ctx = ctx, params = list()), {
    expect_match(output$status$html, 'disabled')
    shiny::isolate(for (s in ctx$review_queue()$SUBJID) mark_subject(ctx, ctx$review_items(), s))
    ctx$bump_marks()
    session$flushReact()
    expect_equal(sum(ctx$review_queue()$STATUS == "To review"), 0)
    expect_false(grepl('disabled', output$status$html))
    session$setInputs(sign = 1, name = "Test Reviewer", meaning = "I have reviewed the data in this review window")
    session$setInputs(confirm = 1)
    s <- rtcrcore::signoffs(ctx$con, ctx$study$id)
    expect_equal(nrow(s), 1)
    expect_equal(s$ASOF, "2026-12-31")
    expect_equal(s$SCOPE_N, nrow(ctx$review_items()))
    expect_equal(nrow(hist()), 1)
  })
  shiny::testServer(audit_server, args = list(ctx = ctx, params = list()), {
    x <- tbl()
    expect_equal(x$Event[1], "sign-off")                                  # newest first
    expect_true(any(x$Event == "mark set"))
  })
})

test_that("programmer checks stay off review screens and show on the data load page", {
  ctx <- test_ctx()
  shiny::testServer(checks_server, args = list(ctx = ctx, params = list(tiers = list("data", "cross-form",
                                                                                     "reconciliation"))), {
    expect_false(any(grepl("^STR-", summary()$ID)))
    expect_true(all(c("Category", "New", "Result") %in% names(summary())))
  })
  shiny::testServer(dataload_server, args = list(ctx = ctx, params = list()), {
    expect_true(all(grepl("^STR-", summary()$ID)))
    expect_equal(nrow(spec()), 0)
  })
  shiny::isolate({
    rd <- ctx$data()
    expect_null(load_banner(ctx$study, rd))                               # clean example load
    rd$summary$N[rd$summary$ID == "STR-001"] <- 2L
    rd$summary$ERROR[rd$summary$ID == "AE-SEV"] <- "Form AE has no field AESEV."
    expect_equal(load_banner(ctx$study, rd), "2 data load error(s): some data may be missing; 1 check(s) not running.")
    st <- ctx$study
    st$check_spec$problems <- data.frame(ROW = 12L, ID = "X-1", PROBLEM = "Cannot read condition", stringsAsFactors = FALSE)
    p <- load_problems(st, rd)
    expect_equal(p$WHERE, c("Check spec, Checks row 12", "Check could not run"))
  })
})

test_that("the queue holds clinical flags new in the window, not every open flag", {
  ctx <- test_ctx(since = "2026-12-20")
  shiny::isolate({
    it <- ctx$review_items()
    flags <- it[it$TYPE == "finding", ]
    expect_true(all(flags$KEY %in% ctx$new_flags()))
    expect_lt(nrow(flags), nrow(clinical_findings(ctx$data())))
    all_open <- queue_items(ctx$changes(), clinical_findings(ctx$data()), ctx$con, ctx$study$id)
    expect_gt(sum(all_open$TYPE == "finding"), nrow(flags))
  })
})
