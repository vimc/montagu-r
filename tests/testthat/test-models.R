context("models")

test_that("download models list", {
  location <- montagu_test_server()
  dat <- montagu_models_list(location = location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "name", "citation", "modelling_group"))
})

test_that("download correct models id", {
  location <- montagu_test_server()
  dat <- montagu_disease_by_id(model_id = "123", location = location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "name", "citation", "modelling_group"))
  expect_equal(nrow(dat), 1)
  expect_true(dat$id == "123")
})

test_that("download incorrect disease id", {
  location <- montagu_test_server()
  expect_error(montagu_model_by_id(model_id = "ZZZ", location = location),
               "Unknown model with id 'ZZZ'")
})

test_that("download empty model id", {
  location <- montagu_test_server()
  expect_error(montagu_model_by_id(model_id = "", location = location),
               "'model_id' must be nonempty")
})