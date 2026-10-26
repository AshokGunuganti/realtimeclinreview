# Round trip: raw sources -> adapters -> review layer must reproduce the truth simulation
local({
  old <- setwd(root); on.exit(setwd(old))
  source("data-raw/sim_engine.R", local = TRUE)
  sim  <<- simulate_study(); comp_truth <<- simulate_compliance(sim)
})
src <- read_sources(src_dir)
rv  <- derive_review(src, load_schedule(APP_CFG$schedule_file))
far <- as.Date("2026-12-31")
s   <- review_snapshot(rv, far)

test_that("subjects map from DM / EX / DS", {
  t <- sim$subj
  r <- rv$subj[match(t$subjid, rv$subj$SUBJID), ]
  expect_equal(r$SITEID, t$site); expect_equal(r$AGE, t$age); expect_equal(r$SEX, t$sex)
  expect_equal(r$ENRLDT, t$runin); expect_equal(r$TRTSDT, t$dose_date)
  expect_equal(sum(!is.na(r$WDDT)), sum(!t$dosed))
})

test_that("visits: dates, not-done flags, entry dates, windows", {
  t <- sim$vis |> dplyr::filter(dplyr::if_else(missed, target <= far, is.na(entry) | entry <= far))
  r <- rv$visit[match(paste(t$subjid, t$visitnum), paste(rv$visit$SUBJID, rv$visit$VISITNUM)), ]
  expect_equal(r$VISDT, t$vdate)
  expect_equal(r$VISNDFL, t$missed)
  expect_equal(r$TARGDT, t$target)
  ok <- !t$missed
  expect_equal(r$ENTRYDT[ok], t$entry[ok])
  expect_equal(r$OOWFL[ok], (abs(t$offset) > t$window & t$window > 0)[ok])
})

test_that("efficacy: IGA and corrected MCSS reproduce truth", {
  t <- sim$vis |> dplyr::filter(!missed, entry <= far)
  r <- s$visit[match(paste(t$subjid, t$visitnum), paste(s$visit$SUBJID, s$visit$VISITNUM)), ]
  expect_equal(r$MCSS, ifelse(!is.na(t$mod) & t$mod > far, t$mcss_prev, t$mcss))
  expect_equal(r$IGA[!is.na(t$iga)], t$iga[!is.na(t$iga)])
  expect_equal(sum(rv$eff$VERSION > 1), sum(!is.na(t$mod) & t$mod <= far))
})

test_that("AEs, PK and compliance map correctly", {
  t <- sim$ae |> dplyr::filter(entry <= far) |> dplyr::arrange(subjid, aeseq)
  expect_equal(nrow(s$ae), nrow(t))
  expect_equal(s$ae$AEDECOD, t$pt); expect_equal(s$ae$ASERFL, t$serious); expect_equal(s$ae$ARELFL, t$related)
  expect_equal(s$ae$ASTDY, as.integer(t$day)); expect_equal(s$ae$ASEV, t$sev)
  p <- sim$pk |> dplyr::filter(entry <= far) |> dplyr::arrange(subjid, week)
  expect_equal(s$pk$BLQFL, p$conc < APP_CFG$lloq)
  expect_equal(s$pk$AVAL[!s$pk$BLQFL], round(p$conc[!(p$conc < APP_CFG$lloq)], 3))
  ct <- comp_truth |> dplyr::left_join(sim$vis[, c("subjid", "visitnum", "entry")], by = c("subjid", "visitnum")) |>
    dplyr::filter(entry <= far) |> dplyr::arrange(subjid, visitnum)
  cr <- rv$comp |> dplyr::arrange(SUBJID, VISITNUM)
  expect_equal(nrow(cr), nrow(ct))
  expect_equal(cr$DOSPRESC, ct$days); expect_equal(cr$DOSMISS, ct$missed_doses)
  expect_equal(cr$COMPPCT, round(100 * (ct$days - ct$missed_doses) / ct$days))   # DR-008 from EDC counts
})

test_that("as-of logic: nothing visible after the review date", {
  a <- as.Date("2026-10-22"); s2 <- review_snapshot(rv, a)
  expect_true(all(s2$visit$ENTRYDT[s2$visit$ENTERED] <= a))
  expect_true(all(s2$ae$ENTRYDT <= a)); expect_true(all(s2$pk$AVAILDT <= a)); expect_true(all(s2$qry$OPENDT <= a))
  expect_true(all(s2$visit$STATUS %in% VISIT_STATUS))
})
