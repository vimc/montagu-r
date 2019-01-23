context("diseases")

test_that("download disease list", {
  location <- montagu_test_server()
  dat <- montagu_disease_list(location = location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "name"))
})

test_that("download correct disease id", {
  location <- montagu_test_server()
  dat <- montagu_disease_by_id(disease_id = "YF", location = location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "name"))
  expect_equal(nrow(dat), 1)
  expect_true(dat$id == "YF")
})

test_that("download incorrect disease id", {
  location <- montagu_test_server()
  expect_error(montagu_disease_by_id(disease_id = "ZZZ", location = location),
               "Unknown disease with id 'ZZZ'")
})

test_that("download empty disease id", {
  location <- montagu_test_server()
  expect_error(montagu_disease_by_id(disease_id = "", location = location),
               "'disease_id' must be nonempty")
})