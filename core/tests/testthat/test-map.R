extract_fixture <- function() {
  dir <- tempfile("extract")
  dir.create(dir)
  utils::write.csv(data.frame(
    STUDYID = "S1", SITEID = c("101", "102", "999"), SUBJID = c("101-001", "102-001", "999-001"),
    SEX = c("M", "F", "M"), CREATED = c("2026-01-05", "2026-01-06", "2026-01-07")),
    file.path(dir, "dm.csv"), row.names = FALSE)
  utils::write.csv(data.frame(
    STUDYID = "S1", SUBJID = c("101-001", "101-001", "102-001", "102-001"),
    VISIT = c("WEEK 4", "WEEK 4", "WEEK 4", "WEEK 99"), PCTEST = "DRUG",
    PCORRES = c("0.20", "0.25", "<0.10", ""), PCORRESU = "ng/mL",
    TRANSDT = c("2026-02-01", "2026-02-10", "2026-02-01", "2026-02-01")),
    file.path(dir, "pk.csv"), row.names = FALSE)
  dir
}

schedule_fixture <- function() data.frame(VISITNUM = c(1, 2), FOLDER = c("DAY1", "WK04"), VENDORVIS = c("BASELINE", "WEEK 4"))

map_fixture <- function() {
  m <- compile_spec(write_spec())
  src <- read_extract(extract_fixture(), m)
  map_extract(m, src, schedule_fixture())
}

test_that("read_extract reads every needed file as text with blanks as NA", {
  m <- compile_spec(write_spec())
  src <- read_extract(extract_fixture(), m)
  expect_named(src, c("dm.csv", "pk.csv"))
  expect_type(src$pk.csv$PCORRES, "character")
  expect_true(is.na(src$pk.csv$PCORRES[4]))
  expect_error(read_extract(tempdir(), m), "missing from extract: dm.csv")
})

test_that("map_extract builds valid CDM tables with the header first", {
  r <- map_fixture()
  dm <- r$tables$DM
  expect_equal(names(dm)[1:11], cdm_header_vars())
  expect_equal(dm$SUBJID, c("101-001", "102-001"))     # test site 999 filtered out
  expect_equal(dm$SEXDCD, c("Male", "Female"))
  expect_equal(dm$RECID, c("101-001", "102-001"))
  expect_s3_class(dm$AVAILDT, "POSIXct")
  expect_s3_class(dm$ENTRYDT, "Date")
  expect_true(all(dm$VERSION == 1))
  expect_true(all(is.na(dm$FOLDER)) && all(is.na(dm$VISITNUM)))
  expect_silent(validate_cdm(dm))
})

test_that("vendor rows become versions ordered by availability", {
  pk <- map_fixture()$tables$PK
  a <- pk[pk$RECID == "101-001|WEEK 4", ]
  expect_equal(a$VERSION, c(1, 2))
  expect_equal(a$PCSTRESN, c(0.20, 0.25))
  expect_equal(pk$SITEID[pk$SUBJID == "102-001"][1], "102")
  expect_true(pk$PCBLQFL[pk$RECID == "102-001|WEEK 4"])
  expect_equal(pk$PCLABEL[1], "DRUG ng/mL")
  # Version 2 is current after it arrives.
  expect_equal(as_of(pk, "2026-02-10")$PCSTRESN[1], 0.25)
  expect_equal(as_of(pk, "2026-02-09")$PCSTRESN[1], 0.20)
})

test_that("unusable values go to the parse log, not silently", {
  iss <- map_fixture()$issues
  v <- iss[iss$FORMOID == "PK" & iss$VARIABLE == "VISITNUM", ]
  expect_equal(v$VALUE, "WEEK 99")
  expect_equal(v$SUBJID, "102-001")
  expect_equal(v$ROW, 4)
  expect_named(iss, c("FORMOID", "VARIABLE", "ROW", "SUBJID", "VALUE", "DETAIL"))
})

test_that("duplicate keys on a single-version form are logged and the later row kept", {
  f <- spec_frames()
  f$Forms$VERSIONING[2] <- "single"
  m <- compile_spec(write_spec(f))
  r <- map_extract(m, read_extract(extract_fixture(), m), schedule_fixture())
  pk <- r$tables$PK
  expect_equal(pk$PCSTRESN[pk$RECID == "101-001|WEEK 4"], 0.25)
  d <- r$issues[r$issues$VARIABLE == "RECID", ]
  expect_equal(d$ROW, 1)
  expect_match(d$DETAIL, "Duplicate record key")
})

test_that("rows without an availability date are dropped and logged", {
  dir <- extract_fixture()
  pk <- utils::read.csv(file.path(dir, "pk.csv"), colClasses = "character")
  pk$TRANSDT[3] <- ""
  utils::write.csv(pk, file.path(dir, "pk.csv"), row.names = FALSE)
  m <- compile_spec(write_spec())
  r <- map_extract(m, read_extract(dir, m), schedule_fixture())
  expect_equal(nrow(r$tables$PK), 3)
  expect_true(any(r$issues$VARIABLE == "AVAILDT" & r$issues$ROW == 3))
})

test_that("a missing required column stops the mapping", {
  dir <- extract_fixture()
  pk <- utils::read.csv(file.path(dir, "pk.csv"), colClasses = "character")
  pk$PCORRES <- NULL
  utils::write.csv(pk, file.path(dir, "pk.csv"), row.names = FALSE)
  m <- compile_spec(write_spec())
  expect_error(map_extract(m, read_extract(dir, m), schedule_fixture()), "missing required column\\(s\\): PCORRES")
})
