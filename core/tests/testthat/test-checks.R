# Small hand-built CDM: DM, EX (first dose), SV (visits), IGA, PK.
checks_fixture <- function() {
  list(
    DM = cdm_tbl("DM", c("101-001", "102-001"), c("101-001", "102-001")),
    EX = cdm_tbl("EX", c("101-001|DAY1", "102-001|DAY1"), c("101-001", "102-001"), visitnum = 2,
                 EXSTDAT = as.Date(c("2026-02-01", "2026-02-10"))),
    SV = cdm_tbl("SV", c("101-001|WK04", "101-001|WK08", "102-001|WK04", "103-001|WK04"),
                 c("101-001", "101-001", "102-001", "103-001"), visitnum = c(3, 4, 3, 3),
                 VISDAT = as.Date(c("2026-03-01", "2026-03-29", "2026-02-09", "2026-03-05")),
                 VISND = c(NA, NA, NA, "Yes")),
    IGA = cdm_tbl("IGA", c("101-001|WK04", "102-001|WK04"), c("101-001", "102-001"), visitnum = 3,
                  IGASCORE = c(2, -1)),
    PK = cdm_tbl("PK", c("101-001|WEEK 4", "102-001|WEEK 4"), c("101-001", "102-001"), visitnum = 3,
                 PCDTC = c("2026-03-01T10:00", "2026-02-12T09:00"), PCSTRESN = c(0.4, 1.8),
                 DOSPRESC = c(28, 28), DOSMISS = c(3, 30))
  )
}

sched_fixture <- function() data.frame(VISITNUM = 1:4, FOLDER = c("RUNIN", "DAY1", "WK04", "WK08"),
                                       IGAFL = c("N", "N", "Y", "Y"))

run1 <- function(def, tables = checks_fixture(), ...) {
  run_checks(tables, define_checks(list(def)), as.Date("2026-12-31"), schedule = sched_fixture(), ...)
}

test_that("library check types are registered on load", {
  expect_true(all(c("parse_log", "subject_exists", "missing_value", "date_order", "range", "flag_when",
                    "reconcile") %in% check_types()))
})

test_that("library checks switch on by ID and take overrides", {
  ch <- define_checks(list("STR-001", list(id = "STR-005", params = list(subject_form = "DEMOG"), severity = "warning")))
  expect_named(ch, c("STR-001", "STR-005"))
  expect_equal(ch$`STR-005`$params$subject_form, "DEMOG")
  expect_equal(ch$`STR-005`$severity, "warning")
  expect_equal(ch$`STR-001`$timing, "load")
})

test_that("check definitions are validated together", {
  err <- tryCatch(define_checks(list(
    list(id = "X-1", type = "range", name = "n", description = "d", tier = "fancy", severity = "error",
         owner = "clinical", params = list(form = "PK", fld = "x")),
    list(id = "X-2", type = "nope", name = "n", description = "d", tier = "clinical", timing = "asof",
         severity = "error", owner = "clinical"),
    "STR-001", "STR-001")), error = conditionMessage)
  expect_match(err, "X-1: tier must be one of")
  expect_match(err, "X-1: parameter\\(s\\) required by range: field")
  expect_match(err, "X-1: parameter\\(s\\) not used by range: fld")
  expect_match(err, "X-2: unknown check type 'nope'")
  expect_match(err, "STR-001: defined more than once")
})

test_that("findings have the standard shape and a unique key", {
  r <- run1("STR-005")
  f <- r$findings
  expect_named(f, c("FINDINGKEY", "CHECKID", "SEVERITY", "OWNER", "SUBJID", "SITEID", "FORMOID", "RECID",
                    "VISITNUM", "ANCHOR", "FIELD", "VERSION", "VALUE", "MESSAGE", "DEPHASH"))
  expect_equal(nrow(f), 1)
  expect_equal(f$FINDINGKEY, "STR-005|103-001|SV:103-001|WK04|SUBJID")
  expect_equal(f$SITEID, "103")
  expect_equal(f$VERSION, 1)
  expect_equal(r$summary$RESULT, "error")
  expect_equal(r$summary$N, 1L)
})

test_that("parse_log turns parse issues into findings", {
  iss <- data.frame(FORMOID = "PK", RECID = "101-001|WEEK 4", VARIABLE = "PCSTRESN", TRANSFORM = "number",
                    ROW = 1L, SUBJID = "101-001", VALUE = "HEMOLYZED", DETAIL = "Value is not a number")
  f <- run1("STR-002", issues = iss)$findings
  expect_equal(f$VALUE, "HEMOLYZED")
  expect_equal(f$ANCHOR, "PK:101-001|WEEK 4")
  expect_equal(nrow(run1("STR-001", issues = iss)$findings), 0)
})

test_that("missing_value respects when", {
  def <- list(id = "M-1", type = "missing_value", name = "n", description = "d", tier = "cross-form",
              severity = "info", owner = "data management",
              params = list(form = "SV", fields = "VISND", when = list(list(field = "VISITNUM", op = ">=", value = 3))))
  f <- run1(def)$findings
  expect_equal(sort(f$RECID), c("101-001|WK04", "101-001|WK08", "102-001|WK04"))
  expect_equal(unique(f$MESSAGE), "VISND is missing")
})

test_that("date_order flags visits not after first dose", {
  def <- list(id = "D-1", type = "date_order", name = "n", description = "d", tier = "cross-form",
              severity = "warning", owner = "sites",
              params = list(form = "SV", field = "VISDAT", ref_form = "EX", ref_field = "EXSTDAT", op = "after",
                            when = list(list(field = "VISITNUM", op = ">", value = 2))))
  f <- run1(def)$findings
  expect_equal(f$RECID, "102-001|WK04")
  expect_match(f$MESSAGE, "2026-02-09 is not after EX EXSTDAT 2026-02-10")
})

test_that("range uses numbers or other fields as bounds", {
  def <- list(id = "R-1", type = "range", name = "n", description = "d", tier = "cross-form",
              severity = "error", owner = "sites", params = list(form = "PK", field = "DOSMISS", min = 0, max = "DOSPRESC"))
  f <- run1(def)$findings
  expect_equal(f$SUBJID, "102-001")
  expect_equal(f$MESSAGE, "DOSMISS 30 is outside [0, 28]")
})

test_that("flag_when reads thresholds from study config", {
  def <- list(id = "C-1", type = "flag_when", name = "PK alert", description = "d", tier = "clinical",
              severity = "warning", owner = "safety",
              params = list(form = "PK", field = "PCSTRESN", message = "PK at or above alert level",
                            when = list(list(field = "PCSTRESN", op = ">=", value = "$pk_alert"))))
  expect_equal(run1(def, thresholds = list(pk_alert = 1.5))$findings$SUBJID, "102-001")
  expect_equal(nrow(run1(def, thresholds = list(pk_alert = 0.1))$findings), 2)
  expect_error(run1(def), "threshold 'pk_alert' is not set")
})

test_that("reconcile reports unmatched records, schedule flags and date gaps", {
  def <- list(id = "X-1", type = "reconcile", name = "n", description = "d", tier = "cross-form",
              severity = "warning", owner = "data management",
              params = list(form_a = "SV", form_b = "IGA", schedule_flag = "IGAFL",
                            when_a = list(list(field = "VISND", op = "missing"))))
  f <- run1(def)$findings
  expect_equal(f$RECID, "101-001|WK08")               # WK04 visits have IGA; 103 visit not done
  expect_equal(f$MESSAGE, "No matching IGA record")

  def$params <- list(form_a = "PK", form_b = "SV", date_a = "PCDTC", date_b = "VISDAT", tolerance = 1,
                     both_ways = TRUE)
  f <- run1(def)$findings
  gap <- f[f$FIELD == "PCDTC", ]
  expect_equal(gap$SUBJID, "102-001")
  expect_match(gap$MESSAGE, "differ by 3 day\\(s\\); tolerance 1")
  expect_true("SV:101-001|WK08" %in% f$ANCHOR)        # SV with no PK, reported both ways
})

test_that("as-of checks only see data available on the review date", {
  t <- checks_fixture()
  t$SV$AVAILDT[4] <- as.Date("2026-11-01")
  t$SV$ENTRYDT[4] <- as.Date("2026-11-01")
  ch <- define_checks(list("STR-005"))
  expect_equal(nrow(run_checks(t, ch, as.Date("2026-10-22"))$findings), 0)
  expect_equal(nrow(run_checks(t, ch, as.Date("2026-11-01"))$findings), 1)
})

test_that("DEPHASH changes only when the values a finding relies on change", {
  def <- list(id = "R-1", type = "range", name = "n", description = "d", tier = "cross-form",
              severity = "error", owner = "sites", params = list(form = "PK", field = "DOSMISS", min = 0, max = "DOSPRESC"))
  t <- checks_fixture()
  h1 <- run1(def, t)$findings$DEPHASH
  t$PK$PCSTRESN[2] <- 9
  expect_equal(run1(def, t)$findings$DEPHASH, h1)
  t$PK$DOSMISS[2] <- 31
  expect_false(identical(run1(def, t)$findings$DEPHASH, h1))
})

test_that("checks_table lists definitions for the spec sheet", {
  tb <- checks_table(define_checks(list("STR-001", "STR-005")))
  expect_equal(tb$ID, c("STR-001", "STR-005"))
  expect_equal(tb$APPLIES_TO, c("All forms", "DM"))
  expect_equal(tb$PARAMS[1], "transforms=date")
})

test_that("a study can register its own check type", {
  register_check_type("test_always", function(ctx, p) {
    t <- ctx$tables[[p$form]]
    data.frame(SUBJID = t$SUBJID, FORMOID = p$form, RECID = t$RECID, VISITNUM = NA, FIELD = "X",
               VALUE = "v", MESSAGE = "m")
  }, required = "form")
  def <- list(id = "S-1", type = "test_always", name = "n", description = "d", tier = "clinical",
              severity = "info", owner = "clinical", params = list(form = "DM"))
  expect_equal(nrow(run1(def)$findings), 2)
})
