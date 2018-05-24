context("reporting")

## Assume that we montagu set up locally
test_that("list", {
  location <- montagu_test_server()

  res <- montagu_reports_list(location = location)

  expect_is(res, "data.frame")
  expect_true(EXAMPLE %in% res$name)
})


test_that("versions", {
  location <- montagu_test_server()
  v <- montagu_reports_report_versions(EXAMPLE, location = location)
  expect_is(v, "character")
  expect_true(EXAMPLE_ID %in% v)
})


test_that("metadata", {
  location <- montagu_test_server()
  info <- montagu_reports_report_metadata(EXAMPLE, EXAMPLE_ID,
                                          location = location)
  expect_is(info, "list")
  expect_equal(info$id, EXAMPLE_ID)
  expect_equal(info$name, EXAMPLE)
})


test_that("download", {
  location <- montagu_test_server()
  path <- montagu_reports_report_download(EXAMPLE, EXAMPLE_ID,
                                          location = location, progress = FALSE)
  expect_true(file.exists(path))
})


test_that("artefact list", {
  location <- montagu_test_server()
  dat <- montagu_reports_report_artefact_list(EXAMPLE, EXAMPLE_ID, location = location)
  expect_is(names(dat), "character")
  expect_is(dat, "character")
})


test_that("run", {
  testthat::skip("Don't test this most of the time")
  location <- montagu_test_server()
  progress <- identical(environment(), .GlobalEnv)
  res <- montagu_reports_run(EXAMPLE, progress = progress, poll = 0.1,
                             location = location)
  v <- montagu_reports_report_versions(EXAMPLE, location = location)
  expect_true(res$id %in% v)
})
