vs_fixture <- function(asof = "2026-04-01") {
  adsl <- data.frame(SUBJID = c("1", "2", "3"), SITEID = "101",
                     ENRLDT = as.Date(c("2026-01-01", "2026-01-01", "2026-01-01")),
                     TRTSDT = as.Date(c("2026-02-01", NA, "2026-02-01")),
                     PLANDY1 = as.Date(c("2026-02-26", "2026-02-26", "2026-02-26")),
                     EOSSTT = c("ONGOING", "ONGOING", "DISCONTINUED"),
                     EOSDT = as.Date(c(NA, NA, "2026-02-15")))
  sched <- data.frame(VISITNUM = 1:4, VISIT = c("Run-in", "Day 1", "Week 4", "Week 8"), NOMWK = c(-8, 0, 4, 8),
                      WINDOW = c(7, 0, 7, 7), ANCHOR = c("ENRLDT", "TRTSDT", "TRTSDT", "TRTSDT"))
  sv <- cdm_tbl("SV", c("1|1", "1|2", "1|3", "2|1", "3|1", "3|2"), c("1", "1", "1", "2", "3", "3"),
                visitnum = c(1, 2, 3, 1, 1, 2),
                VISDAT = as.Date(c("2026-01-02", "2026-02-01", "2026-03-15", "2026-01-03", NA, "2026-02-01")),
                VISND = c(NA, NA, NA, NA, "Yes", NA))
  visit_status(list(SV = sv), adsl, sched, as.Date(asof), "SV", "VISDAT", not_done = list(field = "VISND", value = "Yes"),
               fallback = list(TRTSDT = "PLANDY1"))
}

test_that("visit status follows the rules in order", {
  v <- vs_fixture()
  st <- function(s, n) v$STATUS[v$SUBJID == s & v$VISITNUM == n]
  expect_equal(nrow(v), 12)
  expect_equal(st("1", 1), "Complete")
  expect_equal(st("1", 3), "Out of window")                 # target 1 Mar, done 15 Mar, window 7
  expect_equal(st("1", 4), "Not yet due")                   # target 29 Mar + 7 >= 1 Apr
  expect_equal(st("2", 2), "Overdue / not entered")         # not dosed: planned Day 1 26 Feb
  expect_equal(v$DAYSOVER[v$SUBJID == "2" & v$VISITNUM == 2], 34L)
  expect_equal(st("3", 1), "Missed")
  expect_equal(st("3", 3), "Discontinued")                  # target after end of study
  expect_equal(v$TARGDT[v$SUBJID == "1" & v$VISITNUM == 3], as.Date("2026-03-01"))
})

test_that("an overdue visit becomes due only after its window", {
  v <- vs_fixture("2026-03-05")
  expect_equal(v$STATUS[v$SUBJID == "2" & v$VISITNUM == 3], "Not yet due")   # 26 Feb + 28 + 7
  expect_equal(v$STATUS[v$SUBJID == "1" & v$VISITNUM == 4], "Not yet due")
})

test_that("load_study and review_data assemble the example study", {
  dir <- test_path("..", "..", "..", "studies", "example")
  skip_if_not(dir.exists(dir), "example study not available")
  st <- load_study(dir)
  expect_equal(st$id, "DERM-301-SIM")
  expect_equal(st$extract_date, as.Date("2026-12-31"))
  expect_true(all(c("DM", "SV", "AE", "PK") %in% names(st$tables)))
  rd <- review_data(st, "2026-10-22")
  expect_equal(rd$asof, as.Date("2026-10-22"))
  expect_true("PLANDY1" %in% names(rd$adsl))
  expect_true("TRTEMFL" %in% names(rd$tables$AE))
  expect_equal(nrow(rd$summary), length(st$checks))
})
