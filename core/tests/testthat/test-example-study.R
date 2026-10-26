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
