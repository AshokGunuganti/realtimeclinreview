test_that("standard header has the design-doc variables in order", {
  h <- cdm_header_vars()
  expect_equal(h[1:3], c("STUDYID", "SITEID", "SUBJID"))
  expect_true(all(c("FORMOID", "RECID", "AVAILDT", "VERSION") %in% h))
  expect_false(anyDuplicated(h) > 0)
})
