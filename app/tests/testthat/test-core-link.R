test_that("app sees the core package", {
  expect_true("STUDYID" %in% rtcrcore::cdm_header_vars())
})
