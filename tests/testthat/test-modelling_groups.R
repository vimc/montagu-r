context("modelling_groups")

test_that("download modelling_group list", {
  location <- montagu_test_server()
  dat <- montagu_modelling_groups(location = location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "description"))
})

test_that("download correct modelling_group id", {
  location <- montagu_test_server()
  dat <- montagu_modelling_group(modelling_group_id = "IC-Garske", location = location)
  expect_is(dat, "list")
  expect_equal(names(dat), c("id", "description"))
  expect_equal(dat$id, "IC-Garske")
})

test_that("download incorrect modelling_group id", {
  location <- montagu_test_server()
  expect_error(montagu_modelling_group(modelling_group_id = "IC-GarskeZZZ", location = location),
               "Unknown modelling-group with id 'IC-GarskeZZZ'")
})

test_that("download model list - incorrect modelling_group_id", {
  location <- montagu_test_server()
  expect_error(montagu_model_list(modelling_group_id = "IC-GarskeZZZ", location = location),
               "Unknown modelling-group with id 'IC-GarskeZZZ'")
})

test_that("download model list - result no models", {
  location <- montagu_test_server()
  dat <- montagu_model_list(modelling_group_id = "VIMC", location = location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "description", "citation", "modelling_group"))
  expect_equal(nrow(dat), 0)  
})

test_that("download model list - result one model", {
  location <- montagu_test_server()
  dat <- montagu_model_list(modelling_group_id = "IC-Garske", location = location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "description", "citation", "modelling_group"))
  expect_equal(nrow(dat), 1)  
})

test_that("download model list - result multiple models", {
  location <- montagu_test_server()
  dat <- montagu_model_list(modelling_group_id = "LSHTM-Clark", location = location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "description", "citation", "modelling_group"))
  expect_gt(nrow(dat), 1)  
})

test_that("download members - correct modelling_group_id", {
  location <- montagu_test_server()
  dat <- montagu_modelling_group_members(modelling_group_id = "VIMC", location = location)
  expect_is(dat, "character")
})
