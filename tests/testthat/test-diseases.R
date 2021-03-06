context("diseases")

test_that("download disease list", {
  location <- montagu_test_server_user()
  dat <- montagu_diseases(location = location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "name"))
})

test_that("download correct disease id", {
  location <- montagu_test_server_user()
  dat <- montagu_disease(disease_id = "YF", location = location)
  expect_is(dat, "list")
  expect_equal(names(dat), c("id", "name"))
  expect_true(dat$id == "YF")
})

test_that("download incorrect disease id", {
  location <- montagu_test_server_user()
  expect_error(montagu_disease(disease_id = "ZZZ", location = location),
               "Unknown disease with id 'ZZZ'",
               class = "montagu_api_error")
})

test_that("download empty disease id", {
  location <- montagu_test_server_user()
  expect_error(montagu_disease(disease_id = "", location = location),
               "'disease_id' must be nonempty")
})