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
  
  # This test fails - the API currently returns a valid empty list.
  # See https://vimc.myjetbrains.com/youtrack/issue/VIMC-2628
  
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
  # This test fails - 
  # (1) https://vimc.myjetbrains.com/youtrack/issue/VIMC-2628 is the issue
  #     currently causing the problem
  # (2) https://vimc.myjetbrains.com/youtrack/issue/VIMC-2600 would enable
  #     filtering at the server side - at presept, 
  #     montagu_burden_estimate_set_info is working around i2600, and 
  #     then running into i2628 as a result.
  
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_info(
    "IC-Garske", "201710gavi-5", "ZZZyf-no-vaccination", 687, location),
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
  
  # This test fails, befure it gets to the incorrect group.
  # See https://vimc.myjetbrains.com/youtrack/issue/VIMC-2632
  
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_data(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    paste0("Error running request:\n",
           "\t - forbidden: You do not have sufficient permissions ",
           "to access this resource. Missing these permissions: ",
           "modelling-group:ZZZIC-Garske/estimates.read"))
})

test_that("Burden estimate set info - incorrect touchstone", {
  
  # This test fails, befure it gets to the incorrect touchstone.
  # See https://vimc.myjetbrains.com/youtrack/issue/VIMC-2632
  
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_data(
    "IC-Garske", "ZZZ201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'")
})

test_that("Burden estimate set info - incorrect scenario", {
  # This test fails, befure it gets to the incorrect scenario.
  # See https://vimc.myjetbrains.com/youtrack/issue/VIMC-2632
  
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_data(
    "IC-Garske", "201710gavi-5", "ZZZyf-no-vaccination", 10, location),
    "Unknown scenario-description with id 'ZZZyf-no-vaccination")
})

test_that("Burden estimate set info - incorrect estimate set id", {
  # This test fails, befure it gets to the incorrect id.
  # See https://vimc.myjetbrains.com/youtrack/issue/VIMC-2632
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
  # This test is failing...
  # API reports incorrect burden estimate set id, but not the
  # nonsense scenario.
  # I think https://vimc.myjetbrains.com/youtrack/issue/VIMC-2628
  # will resolve it.
  
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_problems(
    "IC-Garske", "201710gavi-5", "ZZZyf-no-vaccination", 10, location),
    "Unknown scenario-description with id 'ZZZyf-no-vaccination")
})

#############################################################################
### Create Burden Estimate Set

test_that("Create Burden Estimate - incorrect group", {
  location <- montagu_test_server()
  
  expect_error(montagu_burden_estimate_set_create(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", "stochastic",
    10, "Details", location),
    paste0("Error running request:\n",
           "\t - forbidden: You do not have sufficient permissions ",
           "to access this resource. Missing these permissions: ",
           "modelling-group:ZZZIC-Garske/estimates.write"))
})

test_that("Create Burden Estimate - incorrect touchstone", {
  location <- montagu_test_server()
  
  expect_error(montagu_burden_estimate_set_create(
    "IC-Garske", "ZZZ201710gavi-5", "yf-no-vaccination", "stochastic",
    10, "Details", location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'")
})

test_that("Create Burden Estimate - incorrect scenario", {
  location <- montagu_test_server()
  
  expect_error(montagu_burden_estimate_set_create(
    "IC-Garske", "201710gavi-5", "ZZZyf-no-vaccination", "stochastic",
    10, "Details", location),
    "Unknown scenario-description with id 'ZZZyf-no-vaccination'")
})

test_that("Create Burden Estimate - incorrect type", {
  location <- montagu_test_server()
  
  expect_error(montagu_burden_estimate_set_create(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", "plastic",
    10, "Details", location),
    paste0("Invalid type - must be one of central-single-run, stochastic, ",
           "central-averaged, or central-unknown"))
})

test_that("Create Burden Estimate - absurd parameter set id", {
  location <- montagu_test_server()
 
   # Note the typo below...!
  
  expect_error(montagu_burden_estimate_set_create(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", "stochastic",
    -123, "Details", location),
    "Unknown model run paramater set with id '-123'")
})

test_that("Create Burden Estimate - misplaced parameter set id", {
  location <- montagu_test_server()
  
  # This test does not throw an error - unclear whether it should,
  # see https://vimc.myjetbrains.com/youtrack/issue/VIMC-2650
  
  expect_error(montagu_burden_estimate_set_create(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", "stochastic",
    1, "Details", location),
    "Unknown model run paramater set with id '1'")
})

test_that("Create Burden Estimate - General usage", {
  location <- montagu_test_server()
  
  dat <- montagu_burden_estimate_set_create(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", "central-averaged",
    20, "Details", location)
  expect_is(dat, "integer")
  
  
  # Populate it a bit
  
  data <- data.frame(disease = "YF", 
                   year=c(2016,2017,2016,2017), 
                   age=c(50,50,50,50), 
                   country=c("AGO","AGO","ZMB","ZMB"), 
                   country_name=c("Angola","Angola","Zambia","Zambia"), 
                   cohort_size = c(10000,10500,5000,6000), 
                   deaths=c(1000,900,100,1200), 
                   cases=c(2000,2000,NA,NA), 
                   dalys=c(NA,NA,5670,5870))
  
  
  # Test clearing it...
  
  montagu_burden_estimate_set_clear(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", dat, location)
  

})

###########################################################################
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

###########################################################################
### Close Burden Estimate Set

test_that("Close burden estimate set - incorrect group", {
  location <- montagu_test_server()
  
  expect_error(montagu_burden_estimate_set_close(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    paste0("Error running request:\n",
           "\t - forbidden: You do not have sufficient permissions ",
           "to access this resource. Missing these permissions: ",
           "modelling-group:ZZZIC-Garske/estimates.write"))
})

test_that("Clear non-open set - incorrect touchstone", {
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_close(
    "IC-Garske", "ZZZ201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'")
})

test_that("Clear non-open set - incorrect id", {
  
  # This test currently fails - see 
  # https://vimc.myjetbrains.com/youtrack/issue/VIMC-2631
  
  location <- montagu_test_server()
  expect_error(montagu_burden_estimate_set_close(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown burden-estimate-set with id '10'")
})
