test_that("clean simulated sources raise no Error-level findings", {
  src <- read_sources(src_dir); rv <- derive_review(src, load_schedule(APP_CFG$schedule_file))
  ck <- run_checks(src, rv, review_snapshot(rv, as.Date("2026-10-22")))
  expect_equal(sum(ck$summary$N[ck$summary$SEVERITY == "Error"]), 0)
  expect_setequal(ck$summary$CHECK, CHECK_DEFS$CHECK)
})

test_that("corrupted sources are caught", {
  tmp <- tempfile(); dir.create(tmp); file.copy(list.files(src_dir, full.names = TRUE), tmp)
  ae <- read.csv(file.path(tmp, "edc_ae.csv"), colClasses = "character")
  ae$AESTDAT[1] <- "31/05/2026"                      # wrong date format  -> SC-002
  ae$SUBJID[2]  <- "999-999"                         # orphan subject     -> SC-003
  write.csv(ae, file.path(tmp, "edc_ae.csv"), row.names = FALSE)
  pk <- read.csv(file.path(tmp, "vendor_pk.csv"), colClasses = "character")
  pk$PCORRES[1] <- "HEMOLYZED"                       # bad result         -> SC-009
  pk$VISIT[2]   <- "WEEK 6"                          # unknown visit      -> SC-004
  write.csv(pk, file.path(tmp, "vendor_pk.csv"), row.names = FALSE)
  src <- read_sources(tmp); rv <- derive_review(src, load_schedule(APP_CFG$schedule_file))
  ck <- run_checks(src, rv, review_snapshot(rv, as.Date("2026-12-31")))
  hit <- ck$summary$CHECK[ck$summary$N > 0]
  expect_true(all(c("SC-002", "SC-003", "SC-004", "SC-009") %in% hit))
  sv <- read.csv(file.path(tmp, "edc_sv.csv")); sv$VISDAT <- NULL
  write.csv(sv, file.path(tmp, "edc_sv.csv"), row.names = FALSE)
  expect_error(read_sources(tmp), "SC-001")           # missing required column
})
