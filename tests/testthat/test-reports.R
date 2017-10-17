context("reporting")

## Assume that we montagu set up locally
test_that("list", {
  montagu_reset()

  res <- montagu_reports_list()

  expect_is(res, "character")
  expect_true(EXAMPLE %in% res)
})

test_that("versions", {
  montagu_reset()

  v <- montagu_reports_report_versions(EXAMPLE)
  expect_is(v, "character")
  expect_true(EXAMPLE_ID %in% v)
})

test_that("metadata", {
  montagu_reset()
  info <- montagu_reports_report_metadata(EXAMPLE, EXAMPLE_ID)
})

test_that("download", {
  montagu_reset()
  path <- montagu_reports_report_download(EXAMPLE, EXAMPLE_ID)
  expect_true(file.exists(path))
})

test_that("artefact list", {
  montagu_reset()
  dat <- montagu_reports_report_artefact_list(EXAMPLE, EXAMPLE_ID)
  expect_is(names(dat), "character")
  expect_is(dat, "character")
})

test_that("run", {
  montagu_reset()
  progress <- identical(environment(), .GlobalEnv)
  res <- montagu_reports_run(EXAMPLE, progress = progress, poll = 0.1)
  v <- montagu_reports_report_versions(EXAMPLE)
  expect_true(res$id %in% v)
})
