context("modelling_groups")

test_that("download modelling_group list", {
  location <- montagu_test_server()
  dat <- montagu_modelling_group_list(location = location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "description"))
})

test_that("download correct modelling_group id", {
  location <- montagu_test_server()
  dat <- montagu_modelling_group_by_id(modelling_group_id = "IC-Garske", location = location)
  expect_is(dat, "list")
  expect_equal(names(dat), c("id", "description"))
  expect_equal(dat$id, "IC-Garske")
})

test_that("download incorrect modelling_group id", {
  location <- montagu_test_server()
  expect_error(montagu_modelling_group_by_id(modelling_group_id = "IC-GarskeZZZ", location = location),
               "Unknown modelling-group with id 'IC-GarskeZZZ'")
})

test_that("download model list - incorrect modelling_group_id", {
  location <- montagu_test_server()
  expect_error(montagu_model_list_by_modelling_group_id(modelling_group_id = "IC-GarskeZZZ", location = location),
               "Unknown modelling-group with id 'IC-GarskeZZZ'")
})

test_that("download model list - result no models", {
  location <- montagu_test_server()
  dat <- montagu_model_list_by_modelling_group_id(modelling_group_id = "VIMC", location = location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "description", "citation", "modelling_group"))
  expect_equal(nrow(dat), 0)  
})

test_that("download model list - result one model", {
  location <- montagu_test_server()
  dat <- montagu_model_list_by_modelling_group_id(modelling_group_id = "IC-Garske", location = location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "description", "citation", "modelling_group"))
  expect_equal(nrow(dat), 1)  
})

test_that("download model list - result multiple models", {
  location <- montagu_test_server()
  dat <- montagu_model_list_by_modelling_group_id(modelling_group_id = "LSHTM-Clark", location = location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "description", "citation", "modelling_group"))
  expect_gt(nrow(dat), 1)  
})

test_that("download members - correct modelling_group_id", {
  location <- montagu_test_server()
  dat <- montagu_members_by_modelling_group_id(modelling_group_id = "VIMC", location = location)
  expect_is(dat, "character")
})

# Insufficient permissions for these tests, so not working yet

test_that("add and remove user to/from modelling group", {
  location <- montagu_test_server()
  group <- "VIMC"
  username <- "abcde.fghijk"
  
  # Check user doesn't already exist
  members <- montagu_members_by_modelling_group_id(group, location)
  expect_false(username %in% members)
  
  # Add user
  montagu_add_user_to_modelling_group(group, username, location)
  members <- montagu_members_by_modelling_group_id(group, location)
  expect_true(username %in% members)
  
  # Add user again - expect "already-exists" error
  expect_error(montagu_add_user_to_modelling_group(group, username, location),
               "Not sure what error")

  # Remove the user
  montagu_remove_user_from_modelling_group(group, username, location)
  members <- montagu_members_by_modelling_group_id(group, location)
  expect_false(username %in% members)
  
  # Remove non-existent
  expect_error(montagu_remove_user_from_modelling_group(group, username, location),
               "Not sure what error")
  
})

# Missing test for adding modelling group; there's no remove-modelling-group endpoint,
# so unsure how to test this in a pleasant way.
