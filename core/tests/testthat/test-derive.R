# Two dosed subjects and one run-in withdrawal.
derive_fixture <- function() {
  list(
    DM = cdm_tbl("DM", c("101-001", "102-001", "103-001"), c("101-001", "102-001", "103-001"),
                 ENRLDAT = as.Date(c("2025-12-01", "2025-12-10", "2025-12-15"))),
    EX = cdm_tbl("EX", c("101-001|DAY1", "102-001|DAY1"), c("101-001", "102-001"), visitnum = 2,
                 EXSTDAT = as.Date(c("2026-02-01", "2026-02-10"))),
    DS = cdm_tbl("DS", "103-001", "103-001", DSDECOD = "WITHDRAWAL BY SUBJECT", DSSTDAT = as.Date("2026-01-05")),
    SV = cdm_tbl("SV", c("101-001|RUNIN", "101-001|DAY1", "101-001|WK04", "101-001|UNS1", "102-001|WK04"),
                 c("101-001", "101-001", "101-001", "101-001", "102-001"), visitnum = c(1, 2, 3, 99, 3),
                 VISDAT = as.Date(c("2025-12-02", "2026-02-01", "2026-03-10", "2026-03-30", "2026-03-10"))),
    MCSS = cdm_tbl("MCSS", c("101-001|RUN-IN", "101-001|BASELINE", "101-001|WEEK 4", "102-001|BASELINE", "102-001|WEEK 4"),
                   c("101-001", "101-001", "101-001", "102-001", "102-001"), visitnum = c(1, 2, 3, 2, 3),
                   PHOTODT = as.Date(c("2025-12-02", "2026-02-01", "2026-03-01", "2026-02-10", "2026-03-10")),
                   MCSS = c(14, 12, 9, 0, 3)),
    AE = cdm_tbl("AE", c("101-001|1", "101-001|2", "101-001|3", "102-001|1"), c("101-001", "101-001", "101-001", "102-001"),
                 AESTDAT = c("2026-01-20", "2026-02", "2026-03-05", NA), AESEV = c("MILD", "SEVERE", "MODERATE", "MILD")),
    DIARY = cdm_tbl("DIARY", c("101-001|WK04", "102-001|WK04"), c("101-001", "102-001"), visitnum = 3,
                    DRYSTDAT = as.Date(c("2026-02-01", "2026-02-10")), DRYENDAT = as.Date(c("2026-02-28", "2026-03-09")),
                    DOSPRESC = c(28, 28), DOSMISS = c(7, 0)),
    PK = cdm_tbl("PK", c("101-001|WEEK 4", "102-001|WEEK 4"), c("101-001", "102-001"), visitnum = 3,
                 PCDTC = c("2026-03-01T10:00", "2026-03-10T08:30"), PCSTRESN = c(0.4, NA), PCBLQFL = c(FALSE, TRUE),
                 PCLLOQ = c(0.1, 0.1)),
    CM = cdm_tbl("CM", c("101-001|1", "101-001|2", "101-001|3"), rep("101-001", 3),
                 CMSTDAT = as.Date(c("2025-06-01", "2026-01-01", "2026-03-01")),
                 CMENDAT = as.Date(c("2025-07-01", NA, NA)), CMONGO = c(NA, "Y", NA))
  )
}

ref_dates <- list(id = "REF", type = "reference_dates", params = list(subject_form = "DM", dates = list(
  list(name = "ENRLDT", from = list(list(form = "DM", field = "ENRLDAT"))),
  list(name = "TRTSDT", from = list(list(form = "EX", field = "EXSTDAT"))),
  list(name = "LSTCNTDT", pick = "last", from = list(list(form = "SV", field = "VISDAT"),
                                                     list(form = "MCSS", field = "PHOTODT"))))))

# Snapshots come back sorted by RECID; put rows back in fixture order for readable expectations.
fixture_order <- function(res, tables) {
  for (nm in names(res$tables)) {
    x <- res$tables[[nm]]
    res$tables[[nm]] <- x[match(intersect(unique(tables[[nm]]$RECID), x$RECID), x$RECID), , drop = FALSE]
  }
  res
}

derive1 <- function(..., tables = derive_fixture(), asof = "2026-12-31") {
  res <- derive_snapshot(tables, define_derivations(list(ref_dates, ...)), as.Date(asof),
                         schedule = data.frame(VISITNUM = 1:3, VISIT = c("Run-in", "Day 1", "Week 4"), NOMWK = c(-8, 0, 4),
                                               WINDOW = c(7, 0, 7), ANCHOR = c("ENRLDT", "TRTSDT", "TRTSDT")))
  fixture_order(res, tables)
}

test_that("library derivation types are registered on load", {
  expect_true(all(c("reference_dates", "subject_status", "study_day", "period", "baseline", "analysis_visit",
                    "treatment_emergent", "on_treatment", "compliance", "pk_timing", "impute_date", "worst",
                    "prior_concomitant") %in% derivation_types()))
})

test_that("derivation configuration is validated", {
  err <- tryCatch(define_derivations(list(
    list(id = "A", type = "baseline", params = list(form = "MCSS", value = "MCSS", colour = "red")),
    list(id = "B", type = "nope"), list(id = "B", type = "study_day", params = list(items = list())))),
    error = conditionMessage)
  expect_match(err, "A: parameter\\(s\\) required by baseline: rule")
  expect_match(err, "A: parameter\\(s\\) not used by baseline: colour")
  expect_match(err, "B: unknown derivation type 'nope'")
  expect_match(err, "B: defined more than once")
})

test_that("reference_dates builds ADSL with first/last dates and source records", {
  a <- derive1()$adsl
  expect_equal(a$SUBJID, c("101-001", "102-001", "103-001"))
  expect_equal(a$TRTSDT, as.Date(c("2026-02-01", "2026-02-10", NA)))
  expect_equal(a$TRTSDTSRC, c("EX:101-001|DAY1", "EX:102-001|DAY1", NA))
  expect_equal(a$LSTCNTDT[1], as.Date("2026-03-30"))
  expect_equal(a$LSTCNTDTSRC[1], "SV:101-001|UNS1")
  expect_equal(a$LSTCNTDT[2], as.Date("2026-03-10"))
})

test_that("reference dates follow the review date", {
  t <- lapply(derive_fixture(), function(x) { x$AVAILDT <- x$ENTRYDT <- as.Date("2026-01-01"); x })
  t$EX$AVAILDT[2] <- as.Date("2026-02-11")         # 102-001 dose record arrives late
  t$EX$ENTRYDT[2] <- as.Date("2026-02-11")
  expect_true(is.na(derive1(tables = t, asof = "2026-02-10")$adsl$TRTSDT[2]))
  expect_equal(derive1(tables = t, asof = "2026-02-11")$adsl$TRTSDT[2], as.Date("2026-02-10"))
})

test_that("subject_status classifies from the last disposition record", {
  r <- derive1(list(id = "ST", type = "subject_status",
                    params = list(form = "DS", field = "DSDECOD", date = "DSSTDAT", completed = "COMPLETED",
                                  screen_failure = "SCREEN FAILURE")))
  expect_equal(r$adsl$EOSSTT, c("ONGOING", "ONGOING", "DISCONTINUED"))
  expect_equal(r$adsl$DCSREAS[3], "WITHDRAWAL BY SUBJECT")
  expect_equal(r$adsl$EOSDT[3], as.Date("2026-01-05"))
  expect_equal(r$adsl$EOSSRC[3], "DS:103-001")
})

test_that("study_day has no day 0", {
  r <- derive1(list(id = "DY", type = "study_day",
                    params = list(items = list(list(form = "SV", field = "VISDAT", to = "ADY")))))
  sv <- r$tables$SV
  expect_equal(sv$ADY, c(-61L, 1L, 38L, 58L, 29L))
})

test_that("period assigns screening, run-in and treatment", {
  r <- derive1(list(id = "P", type = "period", params = list(
    boundaries = list(list(name = "Run-in", from = "ENRLDT"), list(name = "Treatment", from = "TRTSDT")),
    items = list(list(form = "SV", field = "VISDAT")))))
  expect_equal(r$tables$SV$APERIODC, c("Run-in", "Treatment", "Treatment", "Treatment", "Treatment"))
  t <- derive_fixture()
  t$SV$VISDAT[1] <- as.Date("2025-11-20")
  r <- derive1(tables = t, list(id = "P", type = "period", params = list(
    boundaries = list(list(name = "Run-in", from = "ENRLDT")), items = list(list(form = "SV", field = "VISDAT")))))
  expect_equal(r$tables$SV$APERIODC[1], "Screening")
})

test_that("baseline: last value on or before first dose, with change from baseline", {
  r <- derive1(list(id = "B", type = "baseline", params = list(form = "MCSS", value = "MCSS", date = "PHOTODT",
                                                              rule = "last_before_ref")))
  m <- r$tables$MCSS
  expect_equal(m$ABLFL, c(NA, "Y", NA, "Y", NA))
  expect_equal(m$BASE, c(12, 12, 12, 0, 0))
  expect_equal(m$BASESRC[1], "MCSS:101-001|BASELINE")
  expect_equal(m$APOSTFL, c("N", "N", "Y", "N", "Y"))
  expect_equal(m$CHG, c(NA, NA, -3, NA, 3))
  expect_equal(m$PCHG, c(NA, NA, -25, NA, NA))            # no percent change from a zero baseline
})

test_that("baseline: mean of pre-dose values, or a named visit", {
  r <- derive1(list(id = "B", type = "baseline", params = list(form = "MCSS", value = "MCSS", date = "PHOTODT",
                                                              rule = "mean_before_ref")))
  m <- r$tables$MCSS
  expect_equal(m$BASE[1:3], c(13, 13, 13))
  expect_equal(m$ABLFL[1:3], c("Y", "Y", NA))
  expect_equal(m$BASESRC[1], "MCSS:101-001|RUN-IN, MCSS:101-001|BASELINE")

  r <- derive1(list(id = "B", type = "baseline", params = list(form = "MCSS", value = "MCSS", rule = "visit",
                                                              visit = 1, suffix = "_RI")))
  m <- r$tables$MCSS
  expect_equal(m$BASE_RI[1:3], c(14, 14, 14))
  expect_equal(m$CHG_RI[1:3], c(NA, -2, -5))
  expect_true(all(is.na(m$BASE_RI[4:5])))                 # 102-001 has no run-in read
})

test_that("a corrected baseline value moves every change from baseline", {
  t <- derive_fixture()
  corr <- t$MCSS[2, ]
  corr$VERSION <- 2
  corr$AVAILDT <- corr$ENTRYDT <- as.Date("2026-06-01")
  corr$MCSS <- 10
  t$MCSS <- rbind(t$MCSS, corr)
  bl <- list(id = "B", type = "baseline", params = list(form = "MCSS", value = "MCSS", date = "PHOTODT",
                                                       rule = "last_before_ref"))
  before <- derive1(bl, tables = t, asof = "2026-05-31")$tables$MCSS
  after <- derive1(bl, tables = t, asof = "2026-06-01")$tables$MCSS
  expect_equal(before$CHG[before$RECID == "101-001|WEEK 4"], -3)
  expect_equal(after$CHG[after$RECID == "101-001|WEEK 4"], -1)
})

test_that("analysis_visit assigns nearest nominal visit and flags the window", {
  r <- derive1(list(id = "AV", type = "analysis_visit", params = list(form = "SV", field = "VISDAT")))
  sv <- r$tables$SV
  expect_equal(sv$AVISITN, c(1, 2, 3, 3, 3))
  expect_equal(sv$AVISIT[4], "Week 4")
  expect_equal(sv$ATDIFF, c(1L, 0L, 9L, 29L, 0L))           # Week 4 target = TRTSDT + 28
  expect_equal(sv$AWINFL, c("Y", "Y", "N", "N", "Y"))

  t <- derive_fixture()
  t$SV$VISDAT[3] <- as.Date("2026-02-10")                 # Week 4 visit done on day 10
  r <- derive1(tables = t, list(id = "AV", type = "analysis_visit", params = list(form = "SV", field = "VISDAT")))
  expect_equal(r$tables$SV$AVISITN[3], 2)                 # nearest: Day 1
  r <- derive1(tables = t, list(id = "AV", type = "analysis_visit",
                                params = list(form = "SV", field = "VISDAT", keep_scheduled = TRUE)))
  expect_equal(r$tables$SV$AVISITN[3:4], c(3, 3))         # kept on Week 4; unscheduled still moved
  expect_equal(r$tables$SV$ATDIFF[3], -19L)
})

test_that("treatment_emergent uses imputed partial dates", {
  r <- derive1(
    list(id = "IMP", type = "impute_date", params = list(form = "AE", field = "AESTDAT", to = "ASTDT",
                                                         not_before = "TRTSDT")),
    list(id = "TE", type = "treatment_emergent", params = list(form = "AE", start = "ASTDT")))
  ae <- r$tables$AE
  expect_equal(ae$ASTDT, as.Date(c("2026-01-20", "2026-02-01", "2026-03-05", NA)))  # Feb 2026 -> first dose
  expect_equal(ae$ASTDTF, c(NA, "D", NA, NA))
  expect_equal(ae$TRTEMFL, c("N", "Y", "Y", "Y"))                                    # missing start: conservative
})

test_that("impute_date last rule ends the month or year", {
  t <- derive_fixture()
  t$AE$AESTDAT <- c("2026-02", "2024", "2026-03-05", NA)
  r <- derive1(tables = t, list(id = "IMP", type = "impute_date",
                                params = list(form = "AE", field = "AESTDAT", rule = "last")))
  expect_equal(r$tables$AE$AESTDAT_IMP, as.Date(c("2026-02-28", "2024-12-31", "2026-03-05", NA)))
  expect_equal(r$tables$AE$AESTDAT_IMPF, c("D", "M", NA, NA))
})

test_that("on_treatment respects last dose plus n days", {
  t <- derive_fixture()
  t$EX <- rbind(t$EX, transform(t$EX[1, ], RECID = "101-001|LAST", EXSTDAT = as.Date("2026-03-01")))
  ref <- ref_dates
  ref$params$dates[[4]] <- list(name = "TRTEDT", pick = "last", from = list(list(form = "EX", field = "EXSTDAT")))
  r <- derive_snapshot(t, define_derivations(list(ref, list(id = "ON", type = "on_treatment",
    params = list(form = "SV", field = "VISDAT", end_ref = "TRTEDT", days = 14)))), as.Date("2026-12-31"))
  r <- fixture_order(r, t)
  expect_equal(r$tables$SV$ONTRTFL, c("N", "Y", "Y", "N", "N"))
})

test_that("compliance from prescribed doses or a daily frequency", {
  r <- derive1(list(id = "C", type = "compliance", params = list(form = "DIARY", prescribed = "DOSPRESC", missed = "DOSMISS")))
  expect_equal(r$tables$DIARY$COMPPCT, c(75, 100))
  expect_equal(r$tables$DIARY$DOSAPPL, c(21, 28))
  r <- derive1(list(id = "C", type = "compliance", params = list(form = "DIARY", missed = "DOSMISS", per_day = 2,
                                                                start = "DRYSTDAT", end = "DRYENDAT")))
  expect_equal(r$tables$DIARY$COMPPCT, c(88, 100))
})

test_that("pk_timing applies the BLQ rule and time since last dose", {
  r <- derive1(list(id = "PK", type = "pk_timing", params = list(
    form = "PK", value = "PCSTRESN", blq_flag = "PCBLQFL", blq_rule = "half_lloq", lloq = "PCLLOQ",
    time = "PCDTC", last_dose = list(form = "EX", field = "EXSTDAT"), nominal_hours = 672, window_hours = 24)))
  pk <- r$tables$PK
  expect_equal(pk$AVAL, c(0.4, 0.05))
  expect_equal(pk$TSLD, c(682, 680.5))
  expect_equal(pk$TSLDSRC, c("EX:101-001|DAY1", "EX:102-001|DAY1"))
  expect_equal(pk$PKWINFL, c("Y", "Y"))
})

test_that("worst picks one record per subject by order or value", {
  r <- derive1(list(id = "W", type = "worst", params = list(form = "AE", field = "AESEV",
                                                           order = list("MILD", "MODERATE", "SEVERE"))))
  expect_equal(r$tables$AE$WORSTFL, c(NA, "Y", NA, "Y"))
  r <- derive1(list(id = "W", type = "worst", params = list(form = "MCSS", field = "MCSS", direction = "min",
                                                           when = list(list(field = "VISITNUM", op = ">", value = 2)))))
  expect_equal(r$tables$MCSS$WORSTFL, c(NA, NA, "Y", NA, "Y"))
})

test_that("prior and concomitant medication flags", {
  r <- derive1(list(id = "CM", type = "prior_concomitant",
                    params = list(form = "CM", start = "CMSTDAT", end = "CMENDAT", ongoing = "CMONGO")))
  expect_equal(r$tables$CM$PREFL, c("Y", "Y", "N"))
  expect_equal(r$tables$CM$CONCOMFL, c("N", "Y", NA))
})

test_that("derivations never overwrite collected values", {
  expect_error(derive1(list(id = "DY", type = "study_day",
                            params = list(items = list(list(form = "SV", field = "VISDAT", to = "VISDAT"))))),
               "overwrite collected variable\\(s\\) on SV: VISDAT")
  expect_error(derive1(list(id = "C1", type = "compliance", params = list(form = "DIARY", prescribed = "DOSPRESC", missed = "DOSMISS")),
                       list(id = "C2", type = "compliance", params = list(form = "DIARY", prescribed = "DOSPRESC", missed = "DOSMISS"))),
               "already derived")
})

test_that("checks can use derived variables", {
  dv <- define_derivations(list(ref_dates,
    list(id = "IMP", type = "impute_date", params = list(form = "AE", field = "AESTDAT", to = "ASTDT", not_before = "TRTSDT")),
    list(id = "TE", type = "treatment_emergent", params = list(form = "AE", start = "ASTDT"))))
  ch <- define_checks(list(list(id = "C-TE", type = "flag_when", name = "TEAE", description = "d", tier = "clinical",
                                severity = "info", owner = "safety",
                                params = list(form = "AE", field = "AESEV", message = "Treatment-emergent AE",
                                              when = list(list(field = "TRTEMFL", op = "==", value = "Y"))))))
  res <- run_checks(derive_fixture(), ch, as.Date("2026-12-31"), derivations = dv)
  expect_equal(res$summary$N, 3L)
})
