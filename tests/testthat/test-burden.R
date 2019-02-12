context("burden")


### BURDEN ESTIMATE SETS

test_that("Burden estimate sets", {
  location <- montagu_test_server()
  dat <- montagu_burden_estimate_sets(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", location)
  expect_is(dat, "data.frame")
  expect_true("id" %in% names(dat))
  expect_true("uploaded_on" %in% names(dat))
  
  expect_equal(sum(is.na(match(
    c("id", "uploaded_on", "uploaded_by", "type", "details", "status"),
    names(dat)))), 0)
  
  expect_equal(ncol(dat), 6)
})

test_that("Burden estimate sets info - incorrect group", {
  location <- montagu_test_server()
  
  expect_error(montagu_burden_estimate_sets(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", location),
    paste0("Error running request:\n",
           "\t - forbidden: You do not have sufficient permissions ",
           "to access this resource. Missing these permissions: ",
           "modelling-group:ZZZIC-Garske/estimates.read"))
})

test_that("Burden estimate sets - incorrect touchstone", {
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_sets(
    "IC-Garske", "ZZZ201710gavi-5", "yf-no-vaccination", location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'")
})

test_that("Burden estimate sets - incorrect scenario", {
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_sets(
    "IC-Garske", "201710gavi-5", "ZZZyf-no-vaccination", location),
    "Unknown scenario-description with id 'ZZZyf-no-vaccination")
})

###############################################################################
### INDIVIDUAL BURDEN ESTIMATE SET

test_that("Burden estimate set info - incorrect group", {
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_info(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    paste0("Error running request:\n",
           "\t - forbidden: You do not have sufficient permissions ",
           "to access this resource. Missing these permissions: ",
           "modelling-group:ZZZIC-Garske/estimates.read"))
})

test_that("Burden estimate set info - incorrect touchstone", {
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_info(
    "IC-Garske", "ZZZ201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'")
})

test_that("Burden estimate set info - incorrect scenario", {
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_info(
    "IC-Garske", "201710gavi-5", "ZZZyf-no-vaccination", 10, location),
    "Unknown scenario-description with id 'ZZZyf-no-vaccination")
})

test_that("Burden estimate set info - incorrect estimate set id", {
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_info(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown burden estimate set with id '10'")
})

### BURDEN ESTIMATE SET - GET DATA

test_that("Burden estimate set info - incorrect group", {
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_data(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    paste0("Error running request:\n",
           "\t - forbidden: You do not have sufficient permissions ",
           "to access this resource. Missing these permissions: ",
           "modelling-group:ZZZIC-Garske/estimates.read"))
})

test_that("Burden estimate set info - incorrect touchstone", {
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_data(
    "IC-Garske", "ZZZ201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'")
})

test_that("Burden estimate set info - incorrect scenario", {
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_data(
    "IC-Garske", "201710gavi-5", "ZZZyf-no-vaccination", 10, location),
    "Unknown scenario-description with id 'ZZZyf-no-vaccination")
})

test_that("Burden estimate set info - incorrect estimate set id", {
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_data(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown burden estimate set with id '10'")
})
### BURDEN ESTIMATE SET PROBLEMS

test_that("Burden estimate set problems", {
  location <- montagu_test_server()
  
  dat <- montagu_burden_estimate_set_problems(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", 687, location)
  expect_is(dat, "list")
})

test_that("Burden estimate set problems - incorrect group", {
  location <- montagu_test_server()
  
  expect_error(montagu_burden_estimate_set_problems(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    paste0("Error running request:\n",
           "\t - forbidden: You do not have sufficient permissions ",
           "to access this resource. Missing these permissions: ",
           "modelling-group:ZZZIC-Garske/estimates.read"))
})

test_that("Burden estimate set problems - incorrect touchstone", {
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_problems(
    "IC-Garske", "ZZZ201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'")
})

test_that("Burden estimate set problems - incorrect scenario", {
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_problems(
    "IC-Garske", "201710gavi-5", "ZZZyf-no-vaccination", 10, location),
    "Unknown scenario-description with id 'ZZZyf-no-vaccination")
})


### Create Burden Estimate Set

test_that("Create Burden Estimate - incorrect group", {
  location <- montagu_test_server()
  
  expect_error(montagu_burden_estimate_set_create(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    paste0("Error running request:\n",
           "\t - forbidden: You do not have sufficient permissions ",
           "to access this resource. Missing these permissions: ",
           "modelling-group:ZZZIC-Garske/estimates.write"))
})

### Clear Burden Estimate Set (get)

test_that("Clear non-open set - incorrect group", {
  location <- montagu_test_server()
  
  expect_error(montagu_burden_estimate_set_clear(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    paste0("Error running request:\n",
           "\t - forbidden: You do not have sufficient permissions ",
           "to access this resource. Missing these permissions: ",
          "modelling-group:ZZZIC-Garske/estimates.write"))
})

test_that("Clear non-open set - incorrect touchstone", {
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_clear(
    "IC-Garske", "ZZZ201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'")
})

test_that("Clear non-open set - incorrect id", {
  
  # This test currently fails - see 
  # https://vimc.myjetbrains.com/youtrack/issue/VIMC-2631
  
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_clear(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown burden-estimate-set with id '10'")
})


