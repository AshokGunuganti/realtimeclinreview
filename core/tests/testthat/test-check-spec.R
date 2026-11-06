# Check spec: the review team's workbook of review checks.

spec_rows <- function(...) {
  rows <- canned_checks(changed_on = "2026-10-01")
  extra <- list(...)
  for (e in extra) {
    r <- rows[1, ]
    r[] <- NA
    for (n in names(e)) r[[n]] <- e[[n]]
    rows <- rbind(rows, r)
  }
  rows
}

write_tmp_spec <- function(rows, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".xlsx", .local_envir = env)
  write_check_spec(path, rows)
  path
}

test_that("the canned set reads back as valid check definitions", {
  spec <- read_check_spec(write_tmp_spec(canned_checks()))
  expect_equal(nrow(spec$problems), 0)
  ids <- vapply(spec$checks, `[[`, "", "id")
  expect_equal(ids, canned_checks()$ID)
  ch <- define_checks(c(library_checks(), spec$checks))
  expect_true(all(c("STR-001", "AE-SAE", "VS-MISS") %in% names(ch)))
  sae <- ch[["AE-SAE"]]
  expect_equal(sae$type, "record")
  expect_equal(sae$params$when, list(list(field = "AESER", op = "==", value = "Y")))
  expect_equal(sae$params$watch, c("AESEV", "AEREL", "AEOUT"))
  expect_equal(sae$changed$by, "canned set")
  vm <- ch[["VS-MISS"]]$params
  expect_equal(vm$date, "VISDAT")
  expect_equal(vm$not_done, list(list(field = "VISND", op = "==", value = "Yes")))
  expect_equal(ch[["AE-COD"]]$params$fields, "AEDECOD")
  ws <- openxlsx::getSheetNames(write_tmp_spec(canned_checks()))
  expect_equal(ws, c("Checks", "Check types", "How to"))
})

test_that("conditions read as field, operator and value", {
  expect_equal(parse_conditions("AESER == Y; PCSTRESN >= $pk_alert"),
               list(list(field = "AESER", op = "==", value = "Y"), list(field = "PCSTRESN", op = ">=", value = "$pk_alert")))
  expect_equal(parse_conditions("AESEV in MODERATE, SEVERE")[[1]]$value, c("MODERATE", "SEVERE"))
  expect_equal(parse_conditions("AESEV not in 'MILD'")[[1]], list(field = "AESEV", op = "not_in", value = "MILD"))
  expect_equal(parse_conditions("VISND is missing")[[1]], list(field = "VISND", op = "missing"))
  expect_null(parse_conditions(NA))
  expect_error(parse_conditions("AESER equals Y"), "Cannot read condition")
  expect_error(parse_conditions("AESER =="), "has no value")
})

test_that("retired rows do not run and bad rows are reported, not fatal", {
  rows <- spec_rows(
    list(ID = "X-1", NAME = "Bad op", DESCRIPTION = "d", TIER = "clinical", TYPE = "record", FORM = "AE",
         WHEN = "AESER equals Y", SEVERITY = "warning", OWNER = "safety", STATUS = "active"),
    list(ID = "X-2", NAME = "No type", DESCRIPTION = "d", TIER = "clinical", TYPE = "nope", FORM = "AE",
         SEVERITY = "warning", OWNER = "safety"),
    list(ID = "STR-009", NAME = "Reserved", DESCRIPTION = "d", TIER = "data", TYPE = "record", FORM = "AE",
         SEVERITY = "info", OWNER = "sites"),
    list(ID = "AE-SAE", NAME = "Duplicate", DESCRIPTION = "d", TIER = "clinical", TYPE = "record", FORM = "AE",
         SEVERITY = "info", OWNER = "safety"),
    list(ID = "X-3", NAME = "Wrong tier", DESCRIPTION = "d", TIER = "medical", TYPE = "record", FORM = "AE",
         SEVERITY = "info", OWNER = "safety"))
  rows$STATUS[rows$ID == "AE-SEV"] <- "retired"
  spec <- read_check_spec(write_tmp_spec(rows))
  ids <- vapply(spec$checks, `[[`, "", "id")
  expect_false("AE-SEV" %in% ids)                                         # retired
  expect_equal(spec$table$STATUS[spec$table$ID == "AE-SEV"], "retired")   # kept as history
  expect_equal(spec$problems$ID, c("X-1", "X-2", "STR-009", "AE-SAE", "X-3"))
  expect_equal(spec$problems$ROW, nrow(canned_checks()) + 1L + 1:5)              # sheet rows, after the header
  expect_match(spec$problems$PROBLEM[1], "Cannot read condition")
  expect_match(spec$problems$PROBLEM[2], "unknown check type 'nope'")
  expect_match(spec$problems$PROBLEM[3], "reserved")
  expect_match(spec$problems$PROBLEM[4], "another active row")
  expect_match(spec$problems$PROBLEM[5], "TIER must be")
})

test_that("record and visit status checks flag what reviewers look for", {
  study <- test_path("..", "..", "..", "studies", "example")
  skip_if_not(dir.exists(study), "example study not available")
  s <- load_study(study)
  expect_equal(nrow(s$check_spec$problems), 0)
  rd <- review_data(s, "2026-12-31")
  n <- stats::setNames(rd$summary$N, rd$summary$ID)
  expect_equal(unname(n["AE-NEW"]), nrow(rd$tables$AE))
  expect_equal(unname(n["AE-SAE"]), sum(rd$tables$AE$AESER == "Y"))
  expect_equal(unname(n["EX-COMP"]), sum(rd$tables$DIARY$COMPPCT < s$thresholds$compliance, na.rm = TRUE))
  v <- visit_status(rd$tables, rd$adsl, s$schedule, rd$asof, "SV", "VISDAT", not_done = list(field = "VISND", value = "Yes"),
                    fallback = list(TRTSDT = "PLANDY1"))
  expect_equal(unname(n[c("VS-MISS", "VS-OVER", "VS-WIN")]),
               c(sum(v$STATUS == "Missed"), sum(v$STATUS == "Overdue / not entered"), sum(v$STATUS == "Out of window")))
  over <- rd$findings[rd$findings$CHECKID == "VS-OVER", ]
  expect_true(all(startsWith(over$ANCHOR, "VISIT:")))                     # no record to anchor on
  expect_equal(rd$summary$CATEGORY[rd$summary$ID == "STR-001"], "Data load")
  expect_false(any(rd$summary$RESULT == "not run"))

  # A change to a watched field brings an AE back; a change elsewhere does not.
  ae <- s$tables$AE
  i <- which(ae$AESER == "Y")[1]
  bump <- function(field, value) {
    v <- ae[i, ]
    v[[field]] <- value
    v$VERSION <- v$VERSION + 1
    v$AVAILDT <- as.POSIXct("2026-12-30", tz = "UTC")
    s2 <- s
    s2$tables$AE <- rbind(ae, v)
    f <- review_data(s2, "2026-12-31")$findings
    f$DEPHASH[f$CHECKID == "AE-SAE" & f$RECID == ae$RECID[i]]
  }
  h0 <- rd$findings$DEPHASH[rd$findings$CHECKID == "AE-SAE" & rd$findings$RECID == ae$RECID[i]]
  expect_false(identical(bump("AEOUT", "FATAL"), h0))
  expect_identical(bump("MEDDRAV", "30.0"), h0)
})

test_that("a check that cannot run is reported and the rest still run", {
  study <- test_path("..", "..", "..", "studies", "example")
  skip_if_not(dir.exists(study), "example study not available")
  s <- load_study(study)
  bad <- list(id = "X-BAD", type = "record", name = "n", description = "d", tier = "clinical", timing = "asof",
              severity = "info", owner = "safety", params = list(form = "AE", field = "NOFIELD"))
  s$checks <- c(s$checks, list(`X-BAD` = bad))
  rd <- review_data(s, "2026-12-31")
  x <- rd$summary[rd$summary$ID == "X-BAD", ]
  expect_equal(x$RESULT, "not run")
  expect_match(x$ERROR, "no field NOFIELD")
  expect_gt(sum(rd$summary$N), 0)
})

test_that("dates after the data cut are flagged, header dates are not", {
  study <- test_path("..", "..", "..", "studies", "example")
  skip_if_not(dir.exists(study), "example study not available")
  s <- load_study(study)
  rd <- review_data(s, "2026-12-31")
  f <- rd$findings[rd$findings$CHECKID == "DT-FUT", ]
  ae <- rd$tables$AE
  expect_equal(sort(f$RECID), sort(ae$RECID[!is.na(ae$AECODDT) & ae$AECODDT > as.Date("2026-12-31")]))
  expect_true(all(f$FIELD == "AECODDT"))
  expect_match(f$MESSAGE[1], "after the data cut \\(2026-12-31\\)")
  expect_equal(sum(review_data(s, "2026-10-22")$findings$CHECKID == "DT-FUT"), 0)   # not yet entered then

  one <- list(id = "F-1", type = "future_date", name = "n", description = "d", tier = "data", timing = "asof",
              severity = "warning", owner = "sites", params = list(form = "AE", exclude = "AECODDT"))
  s$checks <- list(`F-1` = one)
  expect_equal(nrow(review_data(s, "2026-12-31")$findings), 0)                  # field left out
})

test_that("review data derives once and gives the same findings as deriving inside the checks", {
  study <- test_path("..", "..", "..", "studies", "example")
  skip_if_not(dir.exists(study), "example study not available")
  s <- load_study(study)
  rd <- review_data(s, "2026-10-22")
  ck <- run_checks(s$tables, s$checks, as.Date("2026-10-22"), s$issues, s$schedule, s$thresholds,
                   derivations = s$derivations)
  expect_equal(rd$findings, ck$findings)
  expect_equal(rd$summary, ck$summary)
})
