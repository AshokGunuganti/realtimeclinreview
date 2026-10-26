test_that("a valid spec compiles and round-trips through YAML", {
  yml <- tempfile(fileext = ".yml")
  m <- compile_spec(write_spec(), yml)
  expect_equal(names(m$forms), c("DM", "PK"))
  expect_equal(m$forms$PK$key, c("SUBJID", "VISIT"))
  expect_equal(m$forms$DM$versioning, "single")
  expect_equal(m$forms$DM$steps[[1]]$transform, "filter")
  expect_length(m$forms$DM$fields, 6)
  expect_equal(m$forms$PK$fields[[9]]$args$sep, " ")
  expect_equal(m$codelists$SEX$decode, c("Male", "Female"))

  m2 <- read_mapping(yml)
  expect_equal(m2$forms$PK$fields[[2]]$args, m$forms$PK$fields[[2]]$args)
  expect_equal(m2$forms$DM$fields[[1]]$source, "STUDYID")
  expect_equal(m2$forms$PK$fields[[9]]$args$sep, " ")
})

test_that("parse_args splits name=value pairs", {
  expect_equal(parse_args("format=DD MON YYYY; type=datetime"), list(format = "DD MON YYYY", type = "datetime"))
  expect_equal(parse_args(NA), list())
  expect_equal(parse_args("map=Y:Yes,N:No"), list(map = "Y:Yes,N:No"))
  expect_error(parse_args("oops"), "name=value")
})

test_that("validation reports every problem at once", {
  f <- spec_frames()
  f$Forms$FORMTYPE[1] <- "listing"
  f$Mapping$TRANSFORM[f$Mapping$VARIABLE %in% "SEXDCD"] <- "translate"
  f$Mapping$ARGS[f$Mapping$VARIABLE %in% "PCSTRESN"] <- "flag=PCBLQFL; digits=2"
  f$Mapping <- f$Mapping[!(f$Mapping$FORMOID == "PK" & f$Mapping$VARIABLE %in% "VISITNUM"), ]
  err <- tryCatch(compile_spec(write_spec(f)), error = conditionMessage)
  expect_match(err, "Form DM: FORMTYPE must be one of")
  expect_match(err, "SEXDCD: unknown transform 'translate'")
  expect_match(err, "PCSTRESN: argument\\(s\\) not used by number: digits")
  expect_match(err, "Form PK: header variable\\(s\\) not mapped: VISITNUM")
})

test_that("validation catches argument and structure errors", {
  bad <- function(edit) {
    f <- spec_frames()
    f <- edit(f)
    tryCatch(compile_spec(write_spec(f)), error = conditionMessage)
  }
  expect_match(bad(function(f) { f$Mapping$ARGS[f$Mapping$VARIABLE %in% "SEXDCD"] <- "codelist=SEXX"; f }),
               "codelist SEXX is not on the Codelists sheet")
  expect_match(bad(function(f) { f$Mapping$ARGS[f$Mapping$VARIABLE %in% "AVAILDT"][1] <- "format=ISO; type=stamp"; f }),
               "date type must be")
  expect_match(bad(function(f) { f$Mapping$VARIABLE[f$Mapping$VARIABLE %in% "SEXDCD"] <- "SUBJID"; f }),
               "mapped more than once: SUBJID")
  expect_match(bad(function(f) { f$Mapping$VARIABLE[f$Mapping$VARIABLE %in% "SEXDCD"] <- "RECID"; f }),
               "filled automatically")
  expect_match(bad(function(f) { f$Mapping$ARGS[1] <- "field=SITEID"; f }), "filter needs keep or drop")
  expect_match(bad(function(f) { f$Mapping$SOURCE_FIELDS[f$Mapping$VARIABLE %in% "SEXDCD"] <- "SEX, AGE"; f }),
               "decode takes 1 source field\\(s\\), got 2")
  expect_match(bad(function(f) { f$Mapping$FORMOID[2] <- "XX"; f }), "not on the Forms sheet: XX")
  expect_match(bad(function(f) { f$Forms$VERSIONING[1] <- "audit"; f }), "VERSIONING must be")
  expect_match(bad(function(f) { f$Codelists <- f$Codelists[c(1, 1, 2), ]; f }), "duplicate codes")
})

test_that("a missing sheet or column is reported", {
  f <- spec_frames()
  expect_error(compile_spec(write_spec(f[c("Forms", "Mapping")])), "missing sheet\\(s\\): Codelists")
  f$Mapping$REQUIRED <- NULL
  expect_error(compile_spec(write_spec(f)), "Mapping is missing column\\(s\\): REQUIRED")
})

test_that("heavy use of derive raises a warning", {
  f <- spec_frames()
  f$Mapping$TRANSFORM[f$Mapping$VARIABLE %in% c("SEXDCD", "PCLABEL", "PCSTRESN")] <- "derive"
  f$Mapping$ARGS[f$Mapping$VARIABLE %in% c("SEXDCD", "PCLABEL", "PCSTRESN")] <- "fn=anything"
  expect_warning(compile_spec(write_spec(f), derive_share = 0.1), "use derive")
})
