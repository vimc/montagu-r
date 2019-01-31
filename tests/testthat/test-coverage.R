context("coverage")

test_that("download coverage info", {
  location <- montagu_test_server()
  dat <- montagu_coverage_info("IC-Garske", "201710gavi-5", "yf-no-vaccination",
                               location)
  expect_is(dat, "data.frame")
  expect_equal(length(dat), 6)
})

test_that("download coverage info, wrong modelling group", {
  location <- montagu_test_server()
  
  # Below is not the best error message - see
  # https://vimc.myjetbrains.com/youtrack/issue/VIMC-2583
  
  expect_error(montagu_coverage_info("ZZZIC-Garske", "201710gavi-5", 
                                     "yf-no-vaccination", location),
    paste0("Error running request:\n\t - forbidden: You do not have sufficient",
           " permissions to access this resource. Missing these permissions:",
           " modelling-group:ZZZIC-Garske/coverage.read"))
})

test_that("download coverage info, wrong touchstone", {
  location <- montagu_test_server()
  expect_error(montagu_coverage_info("IC-Garske", "ZZZ201710gavi-5", 
                                     "yf-no-vaccination", location),
               "Unknown touchstone-version with id 'ZZZ201710gavi-5'")
})

test_that("download coverage info, wrong scenario", {
  location <- montagu_test_server()
  expect_error(montagu_coverage_info("IC-Garske", "201710gavi-5",
                                     "zzzyf-no-vaccination", location),
               "Unknown responsibility with id 'zzzyf-no-vaccination'")
})

test_that("download coverage data - check all-countries flag", {
  location <- montagu_test_server()
  dat_some <- montagu_coverage_data("CDA-Razavi", "201710gavi-5",
                                   "hepb-bd-routine-with", location = location)
  dat_all <- montagu_coverage_data("CDA-Razavi", "201710gavi-5",
             "hepb-bd-routine-with", all_countries = TRUE, location = location)

  expect_is(dat_some, "data.frame")
  expect_is(dat_all, "data.frame")
  expect_equal(length(dat_some), 13)
  expect_equal(length(dat_all), 13)
  
  # Looking for an example where all_countries = TRUE makes a difference
})

test_that("download coverage data, long or wide format", {
  location <- montagu_test_server()
  dat_long <- montagu_coverage_data("IC-Garske", "201710gavi-5",
                      "yf-routine-gavi", format = "long", location = location)
  dat_wide <- montagu_coverage_data("IC-Garske", "201710gavi-5",
                      "yf-routine-gavi", format = "wide", location = location)
  
  expect_is(dat_long, "data.frame")
  expect_is(dat_wide, "data.frame")
  expect_equal(length(dat_long), 13)
  expect_gt(length(dat_wide), length(dat_long))
})

test_that("download coverage info, wrong modelling group", {
  location <- montagu_test_server()
  
  # This fails at the moment, because the API does not throw an error.
  # https://vimc.myjetbrains.com/youtrack/issue/VIMC-2584
  
  dat <- montagu_coverage_data("ZZZIC-Garske", "201710gavi-5", 
                               "yf-no-vaccination", location = location)
  
  expect_error(montagu_coverage_data("ZZZIC-Garske", "201710gavi-5", 
                            "yf-no-vaccination", location = location),
               paste0("Error running request:\n\t - forbidden: You do not have sufficient",
                      " permissions to access this resource. Missing these permissions:",
                      " modelling-group:ZZZIC-Garske/coverage.read"))
})

test_that("download coverage data, wrong touchstone", {
  location <- montagu_test_server()
  expect_error(montagu_coverage_data("IC-Garske", "ZZZ201710gavi-5", 
                          "yf-no-vaccination", location = location),
               "Unknown touchstone-version with id 'ZZZ201710gavi-5'")
})

test_that("download coverage data, wrong scenario", {
  location <- montagu_test_server()
  expect_error(montagu_coverage_data("IC-Garske", "201710gavi-5", 
                        "zzzyf-no-vaccination", location = location),
               "Unknown responsibility with id 'zzzyf-no-vaccination'")
})

