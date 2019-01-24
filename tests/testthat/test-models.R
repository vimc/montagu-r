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
  expect_error(montagu_model_by_id(model_id = "YFICZZZ", location = location),
    paste0("Error running request:\n\t - unexpected-error: ",
           "An unexpected error occurred. Please contact support at ",
           "montagu-help@imperial.ac.uk and quote this code: (.*)"))
})
