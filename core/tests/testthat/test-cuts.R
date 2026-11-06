skip_if_not_installed("RSQLite")

study_db <- function(env = parent.frame()) {
  con <- open_study_db(list(driver = "sqlite", path = ":memory:"))
  withr::defer(DBI::dbDisconnect(con), envir = env)
  con
}

# Writes a small cut folder: two files and a manifest.
make_cut <- function(root, label = "2027-01-05", ae_rows = 2, manifest_ae = ae_rows, study = "S1") {
  dir <- file.path(root, label)
  dir.create(dir, recursive = TRUE)
  ae <- data.frame(SUBJID = sprintf("101-%03d", seq_len(ae_rows)), AETERM = c("Rash, \"mild\"", rep("Nausea", ae_rows - 1)),
                   AESTDAT = c("", rep("01 JAN 2027", ae_rows - 1)), stringsAsFactors = FALSE)
  dm <- data.frame(SUBJID = c("101-001", "101-002"), SEX = c("F", "M"), stringsAsFactors = FALSE)
  utils::write.csv(ae, file.path(dir, "edc_ae.csv"), row.names = FALSE)
  utils::write.csv(dm, file.path(dir, "edc_dm.csv"), row.names = FALSE)
  utils::write.csv(data.frame(FILE = c("edc_ae.csv", "edc_dm.csv"), ROWS = c(manifest_ae, 2), SOURCE = "EDC",
                              STUDYID = study, EXTRACT_DT = label),
                   file.path(dir, "manifest.csv"), row.names = FALSE)
  dir
}

test_that("a cut is stored as delivered and reads back exactly", {
  con <- study_db()
  dir <- make_cut(withr::local_tempdir())
  r <- intake_cut(con, dir, "S1", "dm1")

  expect_equal(r$status, "loaded")
  expect_null(r$message)
  cuts <- list_cuts(con, "S1")
  expect_equal(nrow(cuts), 1)
  expect_equal(cuts$LABEL, "2027-01-05")
  expect_equal(cuts$EXTRACT_DT, "2027-01-05")
  expect_equal(cuts$N_ROWS, 4)

  back <- read_cut(con, r$cut_id)
  expect_named(back, c("edc_ae.csv", "edc_dm.csv"), ignore.order = TRUE)
  expect_identical(back[["edc_ae.csv"]], read_text_csv(file.path(dir, "edc_ae.csv")))
  expect_true(is.na(back[["edc_ae.csv"]]$AESTDAT[1]))
  expect_equal(back[["edc_ae.csv"]]$AETERM[1], "Rash, \"mild\"")
})

test_that("the same files are not stored twice", {
  con <- study_db()
  dir <- make_cut(withr::local_tempdir())
  first <- intake_cut(con, dir, "S1", "dm1")
  again <- intake_cut(con, dir, "S1", "dm1")

  expect_equal(again$status, "duplicate")
  expect_equal(again$cut_id, first$cut_id)
  expect_equal(nrow(list_cuts(con, "S1", status = NULL)), 1)
})

test_that("a cut that fails intake checks is recorded as rejected with no data", {
  con <- study_db()
  root <- withr::local_tempdir()
  bad_rows <- intake_cut(con, make_cut(root, "c1", ae_rows = 3, manifest_ae = 5), "S1", "dm1")
  wrong_study <- intake_cut(con, make_cut(root, "c2", study = "OTHER"), "S1", "dm1")
  no_manifest <- make_cut(root, "c3")
  unlink(file.path(no_manifest, "manifest.csv"))
  none <- intake_cut(con, no_manifest, "S1", "dm1")

  expect_equal(bad_rows$status, "rejected")
  expect_match(bad_rows$message, "edc_ae.csv: Manifest says 5 rows; the file has 3")
  expect_match(wrong_study$message, "not S1")
  expect_match(none$message, "No manifest.csv")
  expect_equal(nrow(list_cuts(con, "S1")), 0)
  expect_equal(nrow(list_cuts(con, "S1", status = NULL)), 3)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM raw_row")$n, 0)
  expect_error(read_cut(con, bad_rows$cut_id), "rejected on intake")
})

test_that("files the mapping needs must be in the cut", {
  con <- study_db()
  mapping <- list(forms = list(AE = list(source = "edc_ae.csv", fields = list()),
                               LB = list(source = "lab.csv", fields = list())))
  r <- intake_cut(con, make_cut(withr::local_tempdir()), "S1", "dm1", mapping = mapping)
  expect_equal(r$status, "rejected")
  expect_match(r$message, "missing: lab.csv")
})

test_that("extra files are stored and noted, not rejected", {
  con <- study_db()
  dir <- make_cut(withr::local_tempdir())
  writeLines(c("A", "1"), file.path(dir, "readme.csv"))
  r <- intake_cut(con, dir, "S1", "dm1")
  expect_equal(r$status, "loaded")
  expect_equal(r$files$PROBLEM[r$files$FILE == "readme.csv"], "Delivered but not in the manifest")
  expect_true("readme.csv" %in% names(read_cut(con, r$cut_id)))
})

test_that("pickup takes in new cut folders oldest first and skips loaded ones", {
  con <- study_db()
  inbox <- withr::local_tempdir()
  make_cut(inbox, "2027-01-05")
  first <- pickup_cuts(con, inbox, "S1")
  make_cut(inbox, "2027-01-12", ae_rows = 3)
  second <- pickup_cuts(con, inbox, "S1")

  expect_equal(first$STATUS, "loaded")
  expect_equal(second$LABEL, c("2027-01-05", "2027-01-12"))
  expect_equal(second$STATUS, c("duplicate", "loaded"))
  expect_equal(list_cuts(con, "S1")$LABEL, c("2027-01-05", "2027-01-12"))
  expect_equal(nrow(pickup_cuts(con, withr::local_tempdir(), "S1")), 0)
})

test_that("cut tables are append-only and intake is audited", {
  con <- study_db()
  intake_cut(con, make_cut(withr::local_tempdir()), "S1", "dm1")
  for (t in c("cut", "cut_file", "raw_row")) {
    expect_error(DBI::dbExecute(con, paste("UPDATE", t, "SET CUTID = 99")), "append-only")
    expect_error(DBI::dbExecute(con, paste("DELETE FROM", t)), "append-only")
  }
  trail <- audit_trail(con, "S1")
  expect_equal(trail$EVENT, "cut loaded")
  expect_equal(trail$USERID, "dm1")
})

test_that("raw values are kept untrimmed; the mapping view trims them", {
  con <- study_db()
  dir <- make_cut(withr::local_tempdir())
  writeLines(c("A,B", "\"  x \",\"\""), file.path(dir, "pad.csv"))
  r <- intake_cut(con, dir, "S1", "dm1")
  expect_equal(read_cut(con, r$cut_id, raw = TRUE)[["pad.csv"]]$A, "  x ")
  expect_equal(read_cut(con, r$cut_id, raw = TRUE)[["pad.csv"]]$B, "")
  expect_equal(read_cut(con, r$cut_id)[["pad.csv"]]$A, "x")
  expect_true(is.na(read_cut(con, r$cut_id)[["pad.csv"]]$B))
})

test_that("a repeat of a rejected cut is not recorded again", {
  con <- study_db()
  dir <- make_cut(withr::local_tempdir(), ae_rows = 3, manifest_ae = 5)
  first <- intake_cut(con, dir, "S1", "dm1")
  again <- intake_cut(con, dir, "S1", "dm1")
  expect_true(first$new)
  expect_false(again$new)
  expect_equal(again$status, "rejected")
  expect_match(again$message, paste0("Already rejected as cut ", first$cut_id))
  expect_equal(nrow(list_cuts(con, "S1", status = NULL)), 1)
})

test_that("an unreadable file rejects the cut instead of stopping intake", {
  con <- study_db()
  dir <- make_cut(withr::local_tempdir())
  dir.create(file.path(dir, "subfolder"))
  writeBin(as.raw(c(0x22, 0x61, 0x0a, 0x22)), file.path(dir, "edc_ae.csv"))
  r <- intake_cut(con, dir, "S1", "dm1")
  expect_equal(r$status, "rejected")
  expect_false("subfolder" %in% r$files$FILE)
})

test_that("the database refuses a second load of the same files", {
  con <- study_db()
  r <- intake_cut(con, make_cut(withr::local_tempdir()), "S1", "dm1")
  fp <- list_cuts(con, "S1")$FINGERPRINT
  expect_error(db_insert_id(con, "cut", list(STUDYID = "S1", LABEL = "x", FINGERPRINT = fp, N_FILES = 0L,
                                             N_ROWS = 0L, STATUS = "loaded", USERID = "dm1", RECEIVED_AT = "t"),
                            "CUTID"), "UNIQUE")
  expect_equal(nrow(list_cuts(con, "S1")), 1)
})

test_that("pickup reports a folder it cannot take in and carries on", {
  con <- study_db()
  inbox <- withr::local_tempdir()
  make_cut(inbox, "2027-01-12")
  local_mocked_bindings(intake_cut = function(con, d, ...) {
    if (basename(d) == "2027-01-05") stop("disk error")
    list(cut_id = 1L, status = "loaded", new = TRUE, message = NULL)
  })
  dir.create(file.path(inbox, "2027-01-05"))
  r <- pickup_cuts(con, inbox, "S1")
  expect_equal(r$STATUS, c("error", "loaded"))
  expect_equal(r$MESSAGE[1], "disk error")
})

test_that("queries are translated for Postgres placeholders", {
  pg <- structure(list(), class = "PqConnection")
  expect_equal(db_sql(pg, "SELECT * FROM cut WHERE A = ? AND B = ?"), "SELECT * FROM cut WHERE A = $1 AND B = $2")
  expect_equal(db_sql(pg, "SELECT 1"), "SELECT 1")
  expect_equal(db_sql(DBI::ANSI(), "A = ?"), "A = ?")
})

test_that("intake needs a user and an existing folder", {
  con <- study_db()
  expect_error(intake_cut(con, withr::local_tempdir(), "S1", ""), "user ID")
  expect_error(intake_cut(con, file.path(tempdir(), "nope"), "S1", "dm1"), "not found")
})

# Integration: the example study loads the same from the database as from its folder.
example <- test_path("..", "..", "..", "studies", "example")

test_that("the example study loads the same from the study database", {
  skip_if_not(dir.exists(example), "example study not available")
  work <- withr::local_tempdir()
  file.copy(example, work, recursive = TRUE)
  dir <- file.path(work, "example")
  cfg <- yaml::read_yaml(file.path(dir, "study.yml"))
  cfg$study_db <- list(driver = "sqlite", path = "study.sqlite")
  cfg$review_store <- cfg$study_db
  yaml::write_yaml(cfg, file.path(dir, "study.yml"))

  expect_message(r <- run_pickup(dir), "2026-12-31: loaded")
  expect_equal(r$STATUS, "loaded")
  expect_message(again <- run_pickup(dir), "duplicate")
  expect_equal(again$CUTID, r$CUTID)

  from_db <- load_study(dir)
  cfg$study_db <- NULL
  yaml::write_yaml(cfg, file.path(dir, "study.yml"))
  from_folder <- load_study(dir)

  expect_equal(from_db$cut_id, r$CUTID)
  expect_equal(from_db$extract_date, from_folder$extract_date)
  expect_identical(from_db$tables, from_folder$tables)
  expect_identical(from_db$issues, from_folder$issues)
})

# Postgres: runs only against a test database, configured as JSON in
# RTCR_TEST_POSTGRES, e.g. {"dbname":"rtcr_test","host":"localhost","user":"rtcr"}.
test_that("cuts work the same on Postgres", {
  pg <- Sys.getenv("RTCR_TEST_POSTGRES")
  skip_if(!nzchar(pg), "RTCR_TEST_POSTGRES not set")
  skip_if_not_installed("RPostgres")
  con <- open_study_db(c(list(driver = "postgres"), jsonlite::fromJSON(pg)))
  on.exit(DBI::dbDisconnect(con))
  study <- paste0("PGTEST-", as.integer(Sys.time()))
  dir <- make_cut(withr::local_tempdir(), study = study)
  r <- intake_cut(con, dir, study, "dm1")
  expect_equal(r$status, "loaded")
  expect_identical(read_cut(con, r$cut_id)[["edc_ae.csv"]], read_text_csv(file.path(dir, "edc_ae.csv")))
  expect_equal(intake_cut(con, dir, study, "dm1")$status, "duplicate")
  expect_error(DBI::dbExecute(con, "DELETE FROM raw_row"), "append-only")
})
