context("remote")

test_that("check members", {
  remote <- montagu_orderly_remote("test_server", "host_name")

  expect_true("name" %in% names(remote))
  expect(is.character(remote$name))

  expect_true("location" %in% names(remote))
  expect(is.character(remote$location))
})

