# Run one field transform on a vector, collecting the parse log.
run_tf <- function(transform, ..., args = list(), ctx = list()) {
  x <- list(...)
  log <- list()
  n <- if (length(x)) length(x[[1]]) else 3
  ctx <- utils::modifyList(list(src = data.frame(i = seq_len(n)), codelists = list(), sources = list()), ctx)
  ctx$issue <- function(rows, values, detail) {
    log[[length(log) + 1]] <<- data.frame(ROW = rows, VALUE = as.character(values), DETAIL = detail)
  }
  value <- get(paste0("tf_", transform), envir = asNamespace("rtcrcore"))(x, args, ctx)
  list(value = value, log = do.call(rbind, log))
}

test_that("direct copies values", {
  expect_equal(run_tf("direct", c("a", NA))$value, c("a", NA))
})

test_that("date parses spec formats to Date, POSIXct or ISO text", {
  v <- c("12 JUN 2026 14:34:01", NA, "31 FEB 2026 10:00:00")
  r <- run_tf("date", v, args = list(format = "DD MON YYYY HH:MM:SS", type = "datetime"))
  expect_s3_class(r$value, "POSIXct")
  expect_equal(format(r$value[1], "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2026-06-12 14:34:01")
  expect_equal(r$log$ROW, 3)

  r <- run_tf("date", c("31 MAY 2026", "31-05-2026"), args = list(format = "DD MON YYYY"))
  expect_equal(r$value[1], as.Date("2026-05-31"))
  expect_true(is.na(r$value[2]))
  expect_match(r$log$DETAIL, "could not be parsed")

  r <- run_tf("date", c("2026-03-07T10:44", "2026-04-01"), args = list(format = "ISO", type = "text"))
  expect_equal(r$value, c("2026-03-07T10:44", "2026-04-01T00:00"))
})

test_that("date keeps partial dates as truncated ISO text", {
  r <- run_tf("date", c("UN JUN 2026", "UN UNK 2026", "05 JUN 2026", "UN XYZ 2026"),
              args = list(format = "DD MON YYYY", type = "text"))
  expect_equal(r$value, c("2026-06", "2026", "2026-06-05", NA))
  expect_equal(r$log$ROW, 4)
  r <- run_tf("date", c("2026-06", "2026"), args = list(format = "ISO", type = "text"))
  expect_equal(r$value, c("2026-06", "2026"))
  # Partial dates are not complete dates.
  expect_true(is.na(run_tf("date", "UN JUN 2026", args = list(format = "DD MON YYYY"))$value))
})

test_that("decode maps codes and logs unknown ones", {
  cl <- list(IGA = list(code = c("-1", "0", "1"), decode = c("WORSE", "NO CHANGE", "IMPROVED")))
  r <- run_tf("decode", c("1", "0", "7", NA), args = list(codelist = "IGA"), ctx = list(codelists = cl))
  expect_equal(r$value, c("IMPROVED", "NO CHANGE", NA, NA))
  expect_equal(r$log$VALUE, "7")
})

test_that("recode maps values, keeping or logging the rest", {
  r <- run_tf("recode", c("Y", "N", "U", NA), args = list(map = "Y:Yes, N:No"))
  expect_equal(r$value, c("Yes", "No", "U", NA))
  expect_null(r$log)
  r <- run_tf("recode", c("Y", "U"), args = list(map = "Y:Yes,N:No", default = "missing"))
  expect_equal(r$value, c("Yes", NA))
  expect_equal(r$log$VALUE, "U")
})

test_that("visit maps folders or vendor labels to VISITNUM", {
  sched <- data.frame(VISITNUM = c(1, 2), FOLDER = c("DAY1", "WK04"), VENDORVIS = c("BASELINE", "WEEK 4"))
  r <- run_tf("visit", c("WEEK 4", "BASELINE", "WEEK 99"), args = list(by = "VENDORVIS"), ctx = list(schedule = sched))
  expect_equal(r$value, c(2, 1, NA))
  expect_equal(r$log$VALUE, "WEEK 99")
  expect_error(run_tf("visit", "x", args = list(by = "NOPE"), ctx = list(schedule = sched)), "no column NOPE")
})

test_that("number converts and flags below-limit results", {
  r <- run_tf("number", c("0.127", "<0.100", "abc", NA), args = list(flag = "PCBLQFL"))
  expect_equal(r$value$value, c(0.127, NA, NA, NA))
  expect_equal(r$value$extra$PCBLQFL, c(FALSE, TRUE, FALSE, NA))
  expect_equal(r$log$VALUE, "abc")
  r <- run_tf("number", c("<0.1", "2"))
  expect_equal(r$value, c(NA, 2))
  expect_match(r$log$DETAIL, "no flag variable")
})

test_that("lookup joins a value from another source", {
  dm <- data.frame(SUBJID = c("101-001", "102-001", "102-001"), SITEID = c("101", "102", "102"))
  r <- run_tf("lookup", c("102-001", "101-001", "999-001"),
              args = list(table = "dm.csv", by = "SUBJID", value = "SITEID"), ctx = list(sources = list(dm.csv = dm)))
  expect_equal(r$value, c("102", "101", NA))
  expect_equal(r$log$VALUE, "999-001")

  dm$SITEID[3] <- "103"
  r <- run_tf("lookup", "102-001", args = list(table = "dm.csv", by = "SUBJID", value = "SITEID"),
              ctx = list(sources = list(dm.csv = dm)))
  expect_match(r$log$DETAIL, "More than one value")
})

test_that("concat joins non-missing fields", {
  r <- run_tf("concat", c("A", NA, NA), c("B", "C", NA), args = list(sep = "-"))
  expect_equal(r$value, c("A-B", "C", NA))
  expect_equal(run_tf("concat", "A", "B")$value, "A B")
})

test_that("constant fills every row", {
  expect_equal(run_tf("constant", args = list(value = "VENDOR_PK"))$value, rep("VENDOR_PK", 3))
})

test_that("derive calls a registered function", {
  register_derive("test_upper", function(x, args, ctx) toupper(x[[1]]))
  expect_equal(run_tf("derive", c("a", "b"), args = list(fn = "test_upper"))$value, c("A", "B"))
  expect_error(run_tf("derive", "a", args = list(fn = "nope")), "No derive function registered")
})

test_that("filter keeps or drops rows", {
  df <- data.frame(SITEID = c("101", "999", "102"), v = 1:3)
  expect_equal(step_filter(df, list(field = "SITEID", drop = "999, 998"))$v, c(1, 3))
  expect_equal(step_filter(df, list(field = "SITEID", keep = "102"))$v, 3)
  expect_error(step_filter(df, list(field = "NOPE", drop = "1")), "not in the source")
})

test_that("pivot reshapes wider and longer", {
  long <- data.frame(SUBJID = c("1", "1", "2"), TEST = c("A", "B", "A"), RES = c("x", "y", "z"))
  w <- step_pivot(long, list(direction = "wider", id = "SUBJID", names = "TEST", values = "RES"))
  expect_equal(w$A, c("x", "z"))
  expect_equal(w$B, c("y", NA))
  l <- step_pivot(w, list(direction = "longer", cols = "A, B", names_to = "TEST", values_to = "RES"))
  expect_equal(nrow(l), 4)
  expect_equal(l$RES[l$SUBJID == "1" & l$TEST == "B"], "y")
})
