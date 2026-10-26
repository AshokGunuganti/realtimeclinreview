qry_fixture <- function() {
  q <- cdm_tbl("QUERY", c("Q1", "Q2", "Q3", "Q4"), c("101-001", "101-001", "102-001", "102-001"),
               availdt = c("2026-03-01", "2026-03-05", "2026-03-10", "2026-03-20"),
               QRYFORM = c("SV", "AE", "SV", "PK"), AESPID = c(NA, "2", NA, NA),
               QRYOPDT = as.Date(c("2026-03-01", "2026-03-05", "2026-03-10", "2026-03-20")),
               QRYANSDT = as.Date(c("2026-03-04", "2026-03-20", NA, NA)),
               QRYCLSDT = as.Date(c("2026-03-06", NA, NA, NA)))
  q$FOLDER <- c("WK04", "LOGAE", "WK08", NA)
  q
}
qry_mapping <- list(forms = list(SV = list(key = c("SUBJID", "FOLDER")), AE = list(key = c("SUBJID", "AESPID")),
                                 PK = list(key = c("SUBJID", "VISIT", "PCTEST"))))

test_that("query status comes from the dates on or before the review date", {
  q <- query_status(qry_fixture(), "2026-03-31", qry_mapping)
  expect_equal(q$STATUS, c("Closed", "Answered", "Open", "Open"))
  expect_equal(q$AGE, c(NA, 26, 21, 11))
  expect_equal(q$DAYSTOCLOSE, c(5, NA, NA, NA))
  expect_equal(q$TARGET, c("SV:101-001|WK04", "AE:101-001|2", "SV:102-001|WK08", NA))   # PK key not in report

  early <- query_status(qry_fixture(), "2026-03-05")
  expect_equal(early$STATUS[1:2], c("Answered", "Open"))               # closed 6 Mar, answered 20 Mar
  expect_true(is.na(early$QRYCLSDT[1]))
  expect_true(all(is.na(early$TARGET)))
})

fs_fixture <- function() {
  cdm_tbl("FSTAT", c("1|DAY1|SV", "1|WK04|SV", "1|LOGAE|AE", "2|DAY1|SV"), c("101-001", "101-001", "101-001", "102-001"),
          FSFORM = c("SV", "SV", "AE", "SV"),
          SDVDT = as.Date(c("2026-02-01", "2026-03-01", NA, "2026-02-10")),
          SIGNDT = as.Date(c("2026-02-05", "2026-04-10", NA, "2026-02-12")),
          FREEZEDT = as.Date(c("2026-02-10", NA, NA, "2026-02-20")),
          LOCKDT = as.Date(c(NA, NA, NA, "2026-03-01")))
}

test_that("form status is the latest step reached by the review date", {
  fs <- form_status(fs_fixture(), "2026-03-31")
  expect_equal(fs$STATUS, c("Frozen", "SDV complete", "Entered", "Locked"))
  expect_true(is.na(fs$SIGNDT[2]))                                      # signed 10 Apr, after the review date
  expect_equal(form_status(fs_fixture(), "2026-02-11")$STATUS, c("Frozen", "Entered", "Entered", "SDV complete"))
})

test_that("entry lag is entry date minus clinical date", {
  sv <- cdm_tbl("SV", c("a", "b", "c"), c("101-001", "101-002", "102-001"),
                availdt = c("2026-03-05", "2026-03-10", "2026-03-10"),
                VISDAT = as.Date(c("2026-03-01", "2026-03-10", NA)))
  l <- entry_lag(list(SV = sv), "SV", "VISDAT")
  expect_equal(l$LAG, c(4, 0))
  expect_equal(l$SITEID, c("101", "101"))
  expect_error(entry_lag(list(), "SV", "VISDAT"), "not in the data")
})

test_that("site scorecard scores sites and sorts the worst first", {
  adsl <- data.frame(SUBJID = c("101-001", "101-002", "102-001"), SITEID = c("101", "101", "102"),
                     EOSSTT = c("ONGOING", "DISCONTINUED", "ONGOING"))
  visits <- data.frame(SITEID = c("101", "101", "101", "102", "102", "102"),
                       STATUS = c("Complete", "Overdue / not entered", "Not yet due", "Complete", "Complete", "Missed"))
  lag <- data.frame(SITEID = c("101", "102", "102"), LAG = c(9, 2, 4))
  q <- query_status(qry_fixture(), "2026-05-01")
  q$SITEID <- c("101", "101", "102", "102")
  s <- site_scorecard(adsl, visits, lag, q, findings = data.frame(SITEID = "102"), lag_target = 5, query_age = 30)
  expect_equal(s$SITEID, c("101", "102"))
  r <- s[1, ]
  expect_equal(c(r$SUBJECTS, r$DISCONT, r$VISDUE, r$OVERDUE, r$MISSED, r$PCTWIN), c(2, 1, 2, 1, 0, 50))
  expect_equal(c(r$LAGMED, r$PCTLAG, r$QUERIES, r$OPENQ, r$OLDESTQ, r$MEDCLOSE), c(9, 0, 2, 1, 57, 5))
  expect_equal(r$SCORE, 2 + 2 + 1 + 1)                    # lag, overdue, under 85% in window, old query
  expect_equal(r$STATUS, "Needs attention")
  expect_equal(s$QRATE, c(20, 10))                        # queries per 10 entered visits
  expect_equal(s$FINDINGS, c(0, 1))
  expect_equal(s$STATUS[2], "Needs attention")            # missed, 67% in window, 2 queries over 30 days
  only <- site_scorecard(adsl)
  expect_equal(names(only), c("SITEID", "SUBJECTS", "DISCONT", "SCORE", "STATUS"))
  expect_true(all(only$STATUS == "On track"))
})

test_that("lock readiness lists blockers per subject and form", {
  tables <- list(
    SV = cdm_tbl("SV", c("1|DAY1", "1|WK04", "2|DAY1"), c("101-001", "101-001", "102-001")),
    AE = cdm_tbl("AE", "1|2", "101-001"),
    PK = cdm_tbl("PK", "1|WK04|DRUG", "101-001"))
  fs <- form_status(fs_fixture(), "2026-03-31")
  q <- query_status(qry_fixture(), "2026-03-31")
  f <- data.frame(SUBJID = "101-001", FORMOID = "PK")
  lr <- lock_readiness(tables, c("SV", "AE", "PK"), fs, q, f)
  get <- function(s, form) lr[lr$SUBJID == s & lr$FORMOID == form, ]
  expect_equal(nrow(lr), 4)
  expect_equal(get("101-001", "SV")$BLOCKERS, "signed 1 of 2")          # Q1 closed
  expect_equal(get("101-001", "AE")$BLOCKERS, "1 unresolved query; SDV 0 of 1; signed 0 of 1")
  expect_equal(get("101-001", "PK")$STATUS, "Not ready")                # vendor form: findings only
  expect_equal(get("101-001", "PK")$BLOCKERS, "1 open finding")
  expect_equal(get("102-001", "SV")$STATUS, "Locked")                   # the EDC's lock stands
  expect_equal(get("102-001", "SV")$BLOCKERS, "")
  lr2 <- lock_readiness(tables, c("SV", "AE", "PK"), fs, q)
  expect_equal(lr2$STATUS[lr2$SUBJID == "101-001" & lr2$FORMOID == "PK"], "Ready to lock")
  expect_equal(nrow(lock_readiness(tables, "CM")), 0)

  by_subj <- lock_by_subject(lr2)
  expect_equal(by_subj$SUBJID, c("101-001", "102-001"))                 # not ready first
  expect_equal(by_subj$STATUS, c("Not ready", "Locked"))
  expect_equal(by_subj$READY, c(1, 1))
  lr2$STATUS[lr2$SUBJID == "101-001"] <- c("Locked", "Locked", "Ready to lock")
  expect_equal(lock_by_subject(lr2)$STATUS[1], "Locked")               # vendor PK has no form status
  lr2$INSTANCES[lr2$FORMOID == "SV" & lr2$SUBJID == "101-001"] <- 0
  lr2$STATUS[lr2$SUBJID == "101-001"] <- "Ready to lock"
  expect_equal(lock_by_subject(lr2)$STATUS[1], "Ready to lock")
  expect_equal(nrow(lock_by_subject(lr2[0, ])), 0)
})

test_that("coding review flags uncoded, inconsistent and older-version records", {
  ae <- cdm_tbl("AE", c("a", "b", "c", "d", "e"), c("101-001", "101-002", "102-001", "102-002", "102-003"),
                AETERM = c("Itchy skin", "ITCHY  SKIN ", "itchy skin", "Headache", "Rash"),
                AEDECOD = c("Pruritus", "Application site pruritus", "Pruritus", "Headache", ""),
                MEDDRAV = c("29.1", "29.1", "29.1", "29.0", ""),
                AECODDT = as.Date(c("2026-03-01", "2026-03-02", "2026-04-15", "2026-02-01", NA)))
  cr <- coding_review(ae, "AETERM", "AEDECOD", version = "MEDDRAV", date = "AECODDT")
  flag <- stats::setNames(cr$records$FLAG, cr$records$RECID)
  expect_equal(unname(flag[c("a", "b", "c", "d", "e")]), c("Inconsistent", "Inconsistent", "Inconsistent", "Older version",
                                                           "Not coded"))
  expect_equal(cr$terms$VERBATIM[1], "ITCHY SKIN")                     # inconsistent first
  expect_equal(cr$terms$CODED[1], "Application site pruritus (1); Pruritus (2)")
  expect_false(cr$terms$CONSISTENT[1])

  early <- coding_review(ae, "AETERM", "AEDECOD", version = "MEDDRAV", date = "AECODDT", asof = "2026-03-31")
  expect_equal(early$records$FLAG[early$records$RECID == "c"], "Not coded")   # coded 15 Apr
})

test_that("example study has queries, form status and coding to review", {
  dir <- test_path("..", "..", "..", "studies", "example")
  skip_if_not(dir.exists(dir), "example study not available")
  s <- load_study(dir)
  expect_equal(s$mapping$forms$QUERY$type, "report")
  rd <- review_data(s, "2026-12-31")
  q <- query_status(rd$tables$QUERY, rd$asof, s$mapping)
  for (f in c("SV", "AE")) {
    t <- sub("^[A-Z]+:", "", q$TARGET[q$QRYFORM == f])
    expect_true(all(t %in% rd$tables[[f]]$RECID))                       # every query points at a record
  }
  expect_equal(sum(q$STATUS != "Closed"), 10)
  fs <- form_status(rd$tables$FSTAT, rd$asof)
  expect_equal(sum(fs$STATUS == "Locked"), 3)                           # withdrawn subject 109-001
  cr <- coding_review(rd$tables$AE, "AETERM", "AEDECOD", "AESOC", "MEDDRAV", "AECODDT", asof = rd$asof)
  expect_equal(cr$terms$VERBATIM[!cr$terms$CONSISTENT], "ITCHY SKIN AT GEL SITE")
})
