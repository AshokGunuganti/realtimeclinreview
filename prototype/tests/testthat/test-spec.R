# The spec workbook must describe exactly what the code reads and produces
spec_path <- file.path(root, "specs", "Review_Data_Spec.xlsx")
rd <- function(s) openxlsx::read.xlsx(spec_path, sheet = s, check.names = FALSE, sep.names = " ")
split_vars <- function(x) trimws(unlist(strsplit(gsub("\\(.*?\\)", "", x), "/")))

test_that("every source column is specified, and nothing extra", {
  sv <- rd("Source Variables")
  for (k in names(SOURCE_SPECS)) {
    f <- SOURCE_SPECS[[k]]$file
    actual <- names(read.csv(file.path(src_dir, f), nrows = 1, check.names = FALSE))
    expect_setequal(sv$Variable[sv$File == f], actual)
    expect_setequal(sv$Variable[sv$File == f & sv$`Required (SC-001)` == "Y"], SOURCE_SPECS[[k]]$cols)
  }
})

test_that("every review dataset variable has a mapping row", {
  src <- read_sources(src_dir); rv <- derive_review(src, load_schedule(APP_CFG$schedule_file))
  vm <- rd("Variable Mapping")
  map <- c("subj" = "RVSUBJ", "visit" = "RVVISIT", "eff" = "RVEFF", "comp" = "RVCOMP", "ae" = "RVAE", "pk" = "RVPK", "qry" = "RVQRY")
  keys <- c("SUBJID", "SITEID", "VISIT", "VISITNUM")   # shared identifiers documented once
  for (n in names(map)) {
    spec_vars <- split_vars(vm$Variable[vm$Dataset %in% c(map[[n]], "RVSUBJ")])
    missing <- setdiff(names(rv[[n]]), c(spec_vars, keys))
    expect_equal(missing, character(0), info = map[[n]])
  }
  s <- review_snapshot(rv, as.Date("2026-10-22"))
  snap_vars <- split_vars(vm$Variable[vm$Dataset == "SNAPSHOT"])
  extra <- unique(c(setdiff(names(s$visit), names(rv$visit)), setdiff(names(s$subj), names(rv$subj)),
                    setdiff(names(s$ae), names(rv$ae)), setdiff(names(s$qry), names(rv$qry))))
  expect_equal(setdiff(extra, c(snap_vars, "WDDT", "KNOWN")), character(0))   # WDDT joined from RVSUBJ; KNOWN internal helper
})

test_that("rule and check IDs agree between code and spec", {
  code <- unlist(lapply(list.files(file.path(root, "R"), full.names = TRUE), readLines))
  code_dr <- unique(regmatches(code, gregexpr("DR-[0-9]{3}", code)) |> unlist())
  spec_dr <- rd("Derivation Rules")$`Rule ID`
  expect_setequal(code_dr, spec_dr)
  vm_dr <- unique(unlist(regmatches(rd("Variable Mapping")$Rule, gregexpr("(DR|SC)-[0-9]{3}", rd("Variable Mapping")$Rule))))
  expect_true(all(vm_dr %in% c(spec_dr, CHECK_DEFS$CHECK)))
  expect_setequal(rd("Data Checks")$`Check ID`, CHECK_DEFS$CHECK)
  vs <- rd("Visit Schedule"); cfg <- load_schedule(APP_CFG$schedule_file)
  expect_equal(vs$FOLDER, cfg$FOLDER); expect_equal(as.numeric(vs$WINDOW), cfg$WINDOW)
})
