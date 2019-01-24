context("models")

test_that("download models list", {
  location <- montagu_test_server()
  dat <- montagu_models_list(location = location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "description", "citation", "modelling_group"))
})

test_that("download correct model id", {
  location <- montagu_test_server()
  dat <- montagu_model_by_id(model_id = "YFIC", location = location)
  expect_is(dat, "list")
  expect_equal(names(dat),
               c("id", "description", "citation", "modelling_group"))
  expect_equal(dat$id, "YFIC")
})

test_that("download incorrect model id", {
  location <- montagu_test_server()
  tryCatch({
    montagu_model_by_id(model_id = "YFICZZZ", location = location)
    fail(message = "Expect to fail, but did not")
  }, error = function(e) {
    expect_equal(class(e)[[1]], "montagu_api_error")
  })
})