cdm_min <- function(...) {
  x <- data.frame(FORMOID = "AE", RECID = c(1, 1, 2), VERSION = c(1, 2, 1),
                  AVAILDT = as.Date(c("2026-10-01", "2026-10-05", "2026-10-02")),
                  AETERM = c("Rash", "Rash", "Headache"), stringsAsFactors = FALSE)
  mods <- list(...)
  for (n in names(mods)) x[[n]] <- mods[[n]]
  x
}

test_that("a well-formed table passes", {
  expect_silent(validate_cdm(cdm_min(), header = FALSE))
  expect_invisible(validate_cdm(cdm_min(), header = FALSE))
})

test_that("the full header is required by default", {
  expect_error(validate_cdm(cdm_min()), "missing header variable\\(s\\): STUDYID, SITEID, SUBJID")
  expect_error(validate_cdm(cdm_min()[, -2], header = FALSE), "RECID")
})

test_that("key problems are caught", {
  expect_error(validate_cdm(cdm_min(VERSION = c(1, 1, 1)), header = FALSE), "not unique")
  expect_error(validate_cdm(cdm_min(VERSION = c(1, 2.5, 1)), header = FALSE), "whole number")
  expect_error(validate_cdm(cdm_min(VERSION = c(0, 1, 1)), header = FALSE), "whole number")
  expect_error(validate_cdm(cdm_min(RECID = c(1, NA, 2)), header = FALSE), "must not be missing")
  expect_error(validate_cdm(cdm_min(AVAILDT = as.Date(c("2026-10-01", NA, "2026-10-02"))), header = FALSE),
               "AVAILDT is missing on 1")
  expect_error(validate_cdm(cdm_min(AVAILDT = c("2026-10-01", "2026-10-05", "2026-10-02")), header = FALSE),
               "Date or POSIXct")
  expect_error(validate_cdm(cdm_min(DELFL = c("N", "N", "Y")), header = FALSE), "DELFL")
})

test_that("a later version may not become available before an earlier one", {
  x <- cdm_min(AVAILDT = as.Date(c("2026-10-05", "2026-10-01", "2026-10-02")))
  expect_error(validate_cdm(x, header = FALSE), "AE / 1 version 2")
})
