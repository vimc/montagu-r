context("demographics")


test_that("demographic list is cached within session", {
  location <- montagu_location(montagu_test_server())
  location$reset_cache()
  touchstone_id <- "201710gavi-5"
  d <- montagu_demographics_list(touchstone_id, location = location)
  expect_equal(location$cache$list("demographics_list"), touchstone_id)
  expect_identical(d, location$cache$get(touchstone_id, "demographics_list"))
  ## Remove the token so that we can't possibly make a call; if we do
  ## make a call after this we'll get a message
  location$token <- NULL
  expect_message(
    d2 <- montagu_demographics_list(touchstone_id, location = location),
    NA)
})
