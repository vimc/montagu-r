context("models")

test_that("download models list", {
  location <- montagu_test_server_user()
  dat <- montagu_models(location = location)
  expect_is(dat, "data.frame")
  expect_equal(sort(names(dat)),
               c("citation", "description", "id", "modelling_group"))
})

test_that("download correct model id", {
  location <- montagu_test_server_user()
  dat <- montagu_model(model_id = "YFIC", location = location)
  expect_is(dat, "list")
  expect_equal(sort(names(dat)),
               c("citation", "description", "id", "modelling_group"))
  expect_equal(dat$id, "YFIC")
})

test_that("download incorrect model id", {
  location <- montagu_test_server_user()
  expect_error(montagu_model(model_id = "YFICZZZ", location = location),
               "Unknown model_id with id 'YFICZZZ'",
               class = "montagu_api_error")
})
