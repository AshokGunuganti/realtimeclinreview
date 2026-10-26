# Hand-built AE log: record 1 is corrected, record 2 arrives later,
# record 3 is deleted, record 4 gets a new version with no data change.
ae_fixture <- function() {
  data.frame(
    STUDYID = "DERM-301", SITEID = "105", SUBJID = c("105-001", "105-001", "105-001", "105-002", "105-002", "105-003", "105-003"),
    FORMOID = "AE", FOLDER = NA_character_, VISITNUM = NA_real_,
    RECID = c(1, 1, 2, 3, 3, 4, 4),
    ENTRYDT = as.Date(c("2026-10-01", "2026-10-01", "2026-10-20", "2026-10-02", "2026-10-02", "2026-10-03", "2026-10-03")),
    UPDDT = as.Date(c(NA, "2026-10-15", NA, NA, "2026-10-18", NA, "2026-10-16")),
    AVAILDT = as.Date(c("2026-10-01", "2026-10-15", "2026-10-20", "2026-10-02", "2026-10-18", "2026-10-03", "2026-10-16")),
    VERSION = c(1, 2, 1, 1, 2, 1, 2),
    DELFL = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
    AETERM = c("Headache", "Headache", "Rash", "Nausea", "Nausea", "Pruritus", "Pruritus"),
    AESEV = c("Mild", "Moderate", "Mild", "Mild", "Mild", NA, NA),
    AESTDAT = as.Date(c("2026-09-30", "2026-09-30", "2026-10-19", "2026-10-01", "2026-10-01", "2026-10-02", "2026-10-02")),
    stringsAsFactors = FALSE
  )
}

test_that("as_of keeps the highest version available on the date", {
  ae <- ae_fixture()
  s <- as_of(ae, as.Date("2026-10-10"))
  expect_equal(s$RECID, c(1, 3, 4))
  expect_equal(s$VERSION, c(1, 1, 1))
  expect_equal(s$AESEV[s$RECID == 1], "Mild")

  s <- as_of(ae, "2026-10-31")
  expect_equal(s$RECID, c(1, 2, 4))              # 3 deleted
  expect_equal(s$AESEV[s$RECID == 1], "Moderate")
  expect_named(s, names(ae))
})

test_that("as_of includes versions available on the review date itself", {
  ae <- ae_fixture()
  expect_equal(as_of(ae, "2026-10-15")$VERSION[1], 2)
  expect_equal(as_of(ae, "2026-10-14")$VERSION[1], 1)
  expect_false(2 %in% as_of(ae, "2026-10-19")$RECID)
  expect_true(2 %in% as_of(ae, "2026-10-20")$RECID)
})

test_that("as_of hides deletions unless asked", {
  ae <- ae_fixture()
  expect_false(3 %in% as_of(ae, "2026-10-18")$RECID)
  s <- as_of(ae, "2026-10-18", include_deleted = TRUE)
  expect_true(s$DELFL[s$RECID == 3])
  expect_true(3 %in% as_of(ae, "2026-10-17")$RECID)
})

test_that("as_of works without a DELFL column and before any data", {
  ae <- ae_fixture()
  ae <- ae[!ae$DELFL, setdiff(names(ae), "DELFL")]
  expect_equal(as_of(ae, "2026-10-31")$RECID, c(1, 2, 3, 4))
  s <- as_of(ae, "2026-01-01")
  expect_equal(nrow(s), 0)
  expect_named(s, names(ae))
})

test_that("POSIXct availability compares to the end of a date-only review day", {
  x <- data.frame(FORMOID = "PK", RECID = c("A", "A"), VERSION = 1:2,
                  AVAILDT = as.POSIXct(c("2026-10-22 08:00:00", "2026-10-22 23:30:00"), tz = "UTC"),
                  PCORRES = c("1.2", "1.3"))
  expect_equal(as_of(x, "2026-10-22")$VERSION, 2L)
  expect_equal(as_of(x, as.POSIXct("2026-10-22 12:00:00", tz = "UTC"))$VERSION, 1L)
  expect_equal(nrow(as_of(x, "2026-10-21")), 0)
})

test_that("change_log classifies New, Changed, Deleted and Unchanged", {
  cl <- change_log(ae_fixture(), since = "2026-10-10", d = "2026-10-31")
  st <- stats::setNames(cl$RECSTAT, cl$RECID)
  expect_equal(st[["1"]], "Changed")
  expect_equal(st[["2"]], "New")
  expect_equal(st[["3"]], "Deleted")
  expect_equal(st[["4"]], "Unchanged")          # version 2 only moved UPDDT
  expect_equal(cl$CHGVARS[cl$RECID == 1], "AESEV")
  expect_equal(cl$PREVVER[cl$RECID == 1], 1)
  expect_true(is.na(cl$PREVVER[cl$RECID == 2]))
  expect_equal(cl$AETERM[cl$RECID == 3], "Nausea")  # deleted rows show last live values
})

test_that("change_log window is (since, d]", {
  ae <- ae_fixture()
  cl <- change_log(ae, since = "2026-10-15", d = "2026-10-20")
  expect_equal(cl$RECSTAT[cl$RECID == 1], "Unchanged")  # corrected on since itself
  expect_equal(cl$RECSTAT[cl$RECID == 2], "New")        # arrived on d itself
  expect_true(all(change_log(ae, "2026-10-31", "2026-10-31")$RECSTAT == "Unchanged"))
})

test_that("a record created and deleted inside the window is not reported", {
  x <- data.frame(FORMOID = "CM", RECID = 9, VERSION = 1:2,
                  AVAILDT = as.Date(c("2026-10-12", "2026-10-14")), DELFL = c(FALSE, TRUE),
                  CMTRT = "Aspirin")
  expect_equal(nrow(change_log(x, "2026-10-10", "2026-10-31")), 0)
})

test_that("missing-to-value and value-to-missing count as changes", {
  x <- data.frame(FORMOID = "VS", RECID = c(1, 1, 2, 2), VERSION = c(1, 2, 1, 2),
                  AVAILDT = as.Date(c("2026-10-01", "2026-10-12", "2026-10-01", "2026-10-12")),
                  SYSBP = c(NA, 120, 130, NA), DIABP = c(80, 80, NA, NA))
  cl <- change_log(x, "2026-10-05", "2026-10-31")
  expect_equal(cl$RECSTAT, c("Changed", "Changed"))
  expect_equal(cl$CHGVARS, c("SYSBP", "SYSBP"))
})

test_that("change_fields gives old and new values per changed field", {
  ae <- ae_fixture()
  ae$AESTDAT[2] <- as.Date("2026-09-29")
  cf <- change_fields(ae, "2026-10-10", "2026-10-31")
  expect_equal(nrow(cf), 2)
  expect_equal(cf$VARIABLE, c("AESEV", "AESTDAT"))
  expect_equal(cf$OLD, c("Mild", "2026-09-30"))
  expect_equal(cf$NEW, c("Moderate", "2026-09-29"))
  expect_equal(unique(cf$SUBJID), "105-001")
  expect_equal(unique(cf$PREVVER), 1)
  expect_equal(unique(cf$VERSION), 2)

  none <- change_fields(ae, "2026-10-31", "2026-10-31")
  expect_equal(nrow(none), 0)
  expect_named(none, c("FORMOID", "RECID", "SUBJID", "VARIABLE", "OLD", "NEW", "PREVVER", "VERSION"))
})

test_that("change_log rejects since after d", {
  expect_error(change_log(ae_fixture(), "2026-10-31", "2026-10-01"), "must not be after")
})

test_that("review dates are validated", {
  ae <- ae_fixture()
  expect_error(as_of(ae, "22/10/2026"), "must be a date")
  expect_error(as_of(ae, c("2026-10-01", "2026-10-02")), "single")
  expect_error(as_of(ae, NA), "single")
  expect_error(as_of(ae, 20261022), "Date, POSIXct")
})
