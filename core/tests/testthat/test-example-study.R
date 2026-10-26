# Integration: the simulated example study in the repository maps cleanly.
# Skipped where the repository is not around the package (e.g. R CMD check).
study <- test_path("..", "..", "..", "studies", "example")

test_that("example study spec compiles and its extract maps without issues", {
  skip_if_not(dir.exists(study), "example study not available")
  m <- compile_spec(file.path(study, "mapping_spec.xlsx"))
  sched <- utils::read.csv(file.path(study, "visit_schedule.csv"), stringsAsFactors = FALSE)
  ext <- file.path(study, "extracts", "2026-12-31")
  src <- read_extract(ext, m)
  r <- map_extract(m, src, sched)

  expect_equal(nrow(r$issues), 0)
  manifest <- utils::read.csv(file.path(ext, "manifest.csv"), stringsAsFactors = FALSE)
  rows <- stats::setNames(manifest$ROWS, manifest$FILE)
  for (id in names(m$forms)) {
    expect_equal(nrow(r$tables[[id]]), rows[[m$forms[[id]]$source]], label = id)
  }

  mcss <- r$tables$MCSS
  expect_equal(sum(mcss$VERSION > 1), sum(mcss$READSTAT == "CORRECTED"))
  expect_true(all(mcss$READSTAT[mcss$VERSION == 1] == "ORIGINAL"))

  iga <- r$tables$IGA
  expect_equal(iga$IGATXT, src$edc_iga.csv$IGATXT[match(iga$RECID, paste(src$edc_iga.csv$SUBJID, src$edc_iga.csv$FOLDER, sep = "|"))])
  expect_false(anyNA(r$tables$SV$VISITNUM))
})

example_run <- function(extract, asof) {
  st <- yaml::read_yaml(file.path(study, "study.yml"))
  m <- compile_spec(file.path(study, "mapping_spec.xlsx"))
  sched <- utils::read.csv(file.path(study, "visit_schedule.csv"), stringsAsFactors = FALSE)
  r <- map_extract(m, read_extract(extract, m), sched)
  run_checks(r$tables, define_checks(st$checks), as.Date(asof), r$issues, sched, st$thresholds)
}

test_that("example study checks run clean on the clean extract", {
  skip_if_not(dir.exists(study), "example study not available")
  res <- example_run(file.path(study, "extracts", "2026-12-31"), "2026-10-22")
  expect_equal(sum(res$summary$N[res$summary$SEVERITY == "error"]), 0)
  expect_false(anyDuplicated(res$findings$FINDINGKEY) > 0)
})

test_that("example study checks catch the prototype's corruptions", {
  skip_if_not(dir.exists(study), "example study not available")
  tmp <- tempfile("corrupt")
  dir.create(tmp)
  file.copy(list.files(file.path(study, "extracts", "2026-12-31"), full.names = TRUE), tmp)
  rd <- function(f) utils::read.csv(file.path(tmp, f), colClasses = "character")
  wr <- function(d, f) utils::write.csv(d, file.path(tmp, f), row.names = FALSE)
  ae <- rd("edc_ae.csv"); ae$AESTDAT[1] <- "31/05/2026"; ae$SUBJID[2] <- "999-999"; wr(ae, "edc_ae.csv")
  pk <- rd("vendor_pk.csv"); pk$PCORRES[1] <- "HEMOLYZED"; pk$VISIT[2] <- "WEEK 6"; wr(pk, "vendor_pk.csv")
  sv <- rd("edc_sv.csv"); sv$VISDAT[which(sv$FOLDER == "WK04")[1]] <- "01 JAN 2026"
  wr(rbind(sv, sv[5, ]), "edc_sv.csv")
  dry <- rd("edc_diary.csv"); dry$DOSMISS[3] <- "40"; wr(dry, "edc_diary.csv")
  wr(rd("edc_iga.csv")[-c(1, 2), ], "edc_iga.csv")

  res <- example_run(tmp, "2026-12-31")
  n <- stats::setNames(res$summary$N, res$summary$ID)
  expect_equal(unname(n[c("STR-001", "STR-002", "STR-003", "STR-004", "STR-005", "XF-IGA-001", "DM-DRY-001",
                          "XF-SV-001")]), c(1, 1, 1, 1, 1, 2, 1, 1))
})

test_that("example study derivations run and line up with the visit schedule", {
  skip_if_not(dir.exists(study), "example study not available")
  st <- yaml::read_yaml(file.path(study, "study.yml"))
  m <- compile_spec(file.path(study, "mapping_spec.xlsx"))
  sched <- utils::read.csv(file.path(study, "visit_schedule.csv"), stringsAsFactors = FALSE)
  r <- map_extract(m, read_extract(file.path(study, "extracts", "2026-12-31"), m), sched)
  d <- derive_snapshot(r$tables, define_derivations(st$derivations), as.Date("2026-10-22"), sched, st$thresholds)

  a <- d$adsl
  expect_equal(nrow(a), nrow(d$tables$DM))
  expect_equal(sum(a$EOSSTT == "DISCONTINUED"), 1)
  expect_true(all(is.na(a$TRTSDT) | a$TRTSDT >= a$ENRLDT))

  sv <- d$tables$SV
  done <- !is.na(sv$VISDAT)
  expect_equal(sv$AVISITN[done], sv$VISITNUM[done])          # keep_scheduled
  expect_true(all(sv$ADY[done & sv$VISITNUM == 2] == 1))     # Day 1 is first dose
  # Simulated run-in visits can fall up to 7 days before enrollment (Screening), never in Treatment.
  expect_true(all(sv$APERIODC[done & sv$VISITNUM == 1] %in% c("Screening", "Run-in")))
  expect_true(all(sv$APERIODC[done & sv$VISITNUM > 2] == "Treatment"))

  mc <- d$tables$MCSS
  expect_true(all(mc$ABLFL[mc$VISITNUM == 2] %in% "Y" | is.na(mc$BASE[mc$VISITNUM == 2])))
  post <- mc$APOSTFL %in% "Y"
  expect_equal(mc$CHG[post], mc$MCSS[post] - mc$BASE[post])
})
