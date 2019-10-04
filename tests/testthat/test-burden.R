context("burden")

### BURDEN ESTIMATE SETS

test_that("Burden estimate sets", {
  location <- montagu_test_server_user()
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

test_that("Burden estimate sets info - no permission", {
  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_sets(
    "PHE-Vynnycky", "201710gavi-5", "rubella-rcv1-gavi", location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:PHE-Vynnycky/",
           "estimates.read"),
    class = "montagu_api_error")
})

test_that("Burden estimate sets info - incorrect group", {
  location <- montagu_test_server_admin()

  expect_error(montagu_burden_estimate_sets(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", location),
    "Unknown modelling-group with id 'ZZZIC-Garske'",
    class = "montagu_api_error")

  location <- montagu_test_server_user()

  expect_error(montagu_burden_estimate_sets(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:ZZZIC-Garske/estimates.read"),
    class = "montagu_api_error")
})

test_that("Burden estimate sets - incorrect touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_sets(
    "IC-Garske", "ZZZ201710gavi-5", "yf-no-vaccination", location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
    class = "montagu_api_error")
})

test_that("Burden estimate sets - incorrect scenario", {
  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_sets(
    "IC-Garske", "201710gavi-5", "ZZZyf-no-vaccination", location),
    "Unknown scenario-description with id 'ZZZyf-no-vaccination",
    class = "montagu_api_error")

})

###############################################################################
### INDIVIDUAL BURDEN ESTIMATE SET

test_that("Burden estimate set info", {
  location <- montagu_test_server_user()
  dat <- montagu_burden_estimate_set_info(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", 687, location)
  expect_is(dat, "list")
  expect_equal(dat$id, 687)
  expect_false(is.null(dat$uploaded_on))
  expect_false(is.null(dat$uploaded_by))
  expect_false(is.null(dat$type))
  expect_false(is.null(dat$status))
})

test_that("Burden estimate set info - incorrect permissions", {
  location <- montagu_test_server_user()

  expect_error(montagu_burden_estimate_set_info(
        "PHE-Vynnycky", "201710gavi-5", "rubella-rcv1-gavi", 10, location),
        paste0("You do not have sufficient permissions to access this resource. ",
               "Missing these permissions: modelling-group:PHE-Vynnycky/estimates.read"),
        class = "montagu_api_error")
})

test_that("Burden estimate set info - incorrect group", {
  location <- montagu_test_server_admin()

  expect_error(montagu_burden_estimate_set_info(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown modelling-group with id 'ZZZIC-Garske'",
    class = "montagu_api_error")

  location <- montagu_test_server_user()

  expect_error(montagu_burden_estimate_set_info(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:ZZZIC-Garske/estimates.read"),
    class = "montagu_api_error")

})

test_that("Burden estimate set info - incorrect touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_info(
    "IC-Garske", "ZZZ201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
    class = "montagu_api_error")
})

test_that("Burden estimate set info - incorrect scenario", {
 location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_info(
    "IC-Garske", "201710gavi-5", "ZZZyf-no-vaccination", 687, location),
    "Unknown scenario-description with id 'ZZZyf-no-vaccination",
    class = "montagu_api_error")
})

test_that("Burden estimate set info - incorrect estimate set id", {
  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_info(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown burden-estimate-set with id '10'",
    class = "montagu_api_error")
})

### BURDEN ESTIMATE SET - GET DATA

test_that("Burden estimate set data - incorrect permissions", {
  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_data(
    "PHE-Vynnycky", "201710gavi-5", "rubella-rcv1-gavi", 10, location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:PHE-Vynnycky/estimates.read"),
    class = "montagu_api_error")
})

test_that("Burden estimate set data - incorrect group", {
  location <- montagu_test_server_admin()
  expect_error(montagu_burden_estimate_set_data(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown modelling-group with id 'ZZZIC-Garske'",
    class = "montagu_api_error")

  location <- montagu_test_server_user()

  expect_error(montagu_burden_estimate_set_data(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:ZZZIC-Garske/estimates.read"),
    class = "montagu_api_error")
})

test_that("Burden estimate set data - incorrect touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_data(
    "IC-Garske", "ZZZ201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
    class = "montagu_api_error")
})

test_that("Burden estimate set data - incorrect scenario", {
  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_data(
    "IC-Garske", "201710gavi-5", "ZZZyf-no-vaccination", 10, location),
    "Unknown scenario-description with id 'ZZZyf-no-vaccination'",
    class = "montagu_api_error")
})

test_that("Burden estimate set info - incorrect estimate set id", {
  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_data(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown burden-estimate-set with id '10'",
    class = "montagu_api_error")
})

### BURDEN ESTIMATE SET - OUTCOME ENDPOINT

test_that("Burden outcome estimate set data - incorrect permissions", {
  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_outcome_data(
    "PHE-Vynnycky", "201710gavi-5", "rubella-rcv1-gavi", 10,
    "cases", group_by = "age", location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:PHE-Vynnycky/estimates.read"),
    class = "montagu_api_error")
})

test_that("Burden outcome estimate set data - incorrect group", {
  location <- montagu_test_server_admin()

  expect_error(montagu::montagu_burden_estimate_set_outcome_data(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", 687,
    "cases", group_by = "age", location),
    "Unknown modelling-group with id 'ZZZIC-Garske'",
    class = "montagu_api_error")

  location <- montagu_test_server_user()
  expect_error(montagu::montagu_burden_estimate_set_outcome_data(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", 687,
    "cases", group_by = "age", location),
    paste0("You do not have sufficient permissions to access this resource. ",
    "Missing these permissions: modelling-group:ZZZIC-Garske/estimates.read"),
    class = "montagu_api_error")
})

test_that("Burden outcome estimate set data - incorrect touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu::montagu_burden_estimate_set_outcome_data(
    "IC-Garske", "ZZZ201710gavi-5", "yf-no-vaccination", 687,
    "cases", group_by = "age", location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
    class = "montagu_api_error")
})

test_that("Burden outcome estimate set data - incorrect scenario", {
  location <- montagu_test_server_user()
  expect_error(montagu::montagu_burden_estimate_set_outcome_data(
    "IC-Garske", "201710gavi-5", "ZZZyf-no-vaccination", 687,
    "cases", group_by = "age", location),
    "Unknown scenario-description with id 'ZZZyf-no-vaccination'",
    class = "montagu_api_error")
})

test_that("Burden outcome estimate set data - incorrect estimate set id", {
  location <- montagu_test_server_user()
  expect_error(montagu::montagu_burden_estimate_set_outcome_data(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", 1,
    "cases", group_by = "age", location),
    "Unknown burden-estimate-set with id '1'",
    class = "montagu_api_error")
})

test_that("Burden outcome estimate set data - incorrect outcome code", {
  # API doesn't throw an error with this; the error below is from R.
  # See i2724
  location <- montagu_test_server_user()
  expect_error(montagu::montagu_burden_estimate_set_outcome_data(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", 687,
    "asparagus", group_by = "age", location),
    "subscript out of bounds",
    class = "simpleError")
})

test_that("Burden outcome estimate set data - incorrect group_by", {
  location <- montagu_test_server_user()
  expect_error(montagu::montagu_burden_estimate_set_outcome_data(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", 687,
    "cases", group_by = "gender", location),
    "group_by must be set to 'age' or 'year'",
    class = "simpleError")
})

test_that("Burden outcome estimate set data - correct test", {
  location <- montagu_test_server_user()
  dat <- montagu::montagu_burden_estimate_set_outcome_data(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", 687,
    "cases", group_by = "age", location)
  expect_is(dat, "data.frame")
  expect_equal(sort(names(dat)), c("age", "x", "y"))

  dat <- montagu::montagu_burden_estimate_set_outcome_data(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", 687,
    "cases", group_by = "year", location)
  expect_is(dat, "data.frame")
  expect_equal(sort(names(dat)), c("x", "y", "year"))
})

### BURDEN ESTIMATE SET PROBLEMS

test_that("Burden estimate set problems", {
  location <- montagu_test_server_user()
  dat <- montagu_burden_estimate_set_problems(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", 687, location)
  expect_is(dat, "list")
})

test_that("Burden estimate set problems - incorrect permissions", {
  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_problems(
    "PHE-Vynnycky", "201710gavi-5", "rubella-rcv2-gavi", 740, location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:PHE-Vynnycky/estimates.read"),
    class = "montagu_api_error")
})

test_that("Burden estimate set problems - incorrect group", {
  location <- montagu_test_server_admin()
  expect_error(montagu_burden_estimate_set_problems(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown modelling-group with id 'ZZZIC-Garske'",
    class = "montagu_api_error")

  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_problems(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:ZZZIC-Garske/estimates.read"),
    class = "montagu_api_error")

})

test_that("Burden estimate set problems - incorrect touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_problems(
    "IC-Garske", "ZZZ201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
    class = "montagu_api_error")
})

test_that("Burden estimate set problems - incorrect scenario", {
  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_problems(
    "IC-Garske", "201710gavi-5", "ZZZyf-no-vaccination", 10, location),
    "Unknown scenario-description with id 'ZZZyf-no-vaccination",
    class = "montagu_api_error")
})

#############################################################################
### Create Burden Estimate Set

test_that("Create Burden Estimate - incorrect group", {

  location <- montagu_test_server_user()

  expect_error(montagu_burden_estimate_set_create(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", "central-averaged",
    "Details", location),
    paste0("You do not have sufficient permissions ",
           "to access this resource. Missing these permissions: ",
           "modelling-group:ZZZIC-Garske/estimates.write"),
    class = "montagu_api_error")
})

test_that("Create Burden Estimate Set - incorrect permissions", {

  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_create(
    "PHE-Vynnycky", "201710gavi-5", "rubella-rcv1-gavi", "central-averaged",
    "Details", location),
    paste0("You do not have sufficient permissions ",
           "to access this resource. Missing these permissions: ",
           "modelling-group:PHE-Vynnycky/estimates.write"),
    class = "montagu_api_error")
})


test_that("Create Burden Estimate - incorrect touchstone", {
  location <- montagu_test_server_user()

  expect_error(montagu_burden_estimate_set_create(
    "IC-Garske", "ZZZ201710gavi-5", "yf-no-vaccination", "central-averaged",
    "Details", location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
    class = "montagu_api_error")
})

test_that("Create Burden Estimate - incorrect scenario", {
  location <- montagu_test_server_user()

  expect_error(montagu_burden_estimate_set_create(
    "IC-Garske", "201710gavi-5", "ZZZyf-no-vaccination", "central-averaged",
    "Details", location),
    "Unknown scenario-description with id 'ZZZyf-no-vaccination'",
    class = "montagu_api_error")
})

test_that("Create Burden Estimate - incorrect type", {
  location <- montagu_test_server_user()

  expect_error(montagu_burden_estimate_set_create(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", "plastic",
    "Details", location),
    "Invalid type - must be one of central-single-run or central-everaged.",
    class = "simpleError")
})

test_that("Create Burden Estimate - General usage - central", {
  location <- montagu_test_server_user()

  bsid <- montagu_burden_estimate_set_create(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", "central-averaged",
    "Details", location)
  expect_is(bsid, "integer")


  # Populate it a bit - data here is all nonsense...

  data <- data_frame(disease = "Hib3",
                   year=c(1986,2017,2016,2017),
                   age=c(50,50,150,150),
                   country=c("AFG","AFG","ZMB","ZMB"),
                   country_name=c("Afghanistan","Afghanistan","Zambia","Zambia"),
                   cohort_size = c(10000,10500,5000,6000),
                   deaths=c(1000,900,100,1200),
                   cases=c(2000,2000,NA,NA),
                   dalys=c(NA,NA,5670,5870))

  expect_error(montagu_burden_estimate_set_upload("IC-Garske", "201710gavi-5",
        "yf-no-vaccination", bsid, data, lines = Inf, location = location),
        "We are not expecting data for country AFG",
        class = "montagu_api_error")

  data$country[data$country == 'AFG'] <- 'AGO'
  data$country_name[data$country_name == 'Afghanistan'] <- "Angola"

  expect_error(montagu_burden_estimate_set_upload("IC-Garske", "201710gavi-5",
        "yf-no-vaccination", bsid, data, lines = Inf, location = location),
        "We are not expecting data for age 50 and year 1986",
        class = "montagu_api_error")


  data$year[data$year==1986] <- 2016

  expect_error(montagu_burden_estimate_set_upload("IC-Garske", "201710gavi-5",
      "yf-no-vaccination", bsid, data, lines = Inf, location = location),
      "Provided estimate lists disease as 'Hib3' but scenario is for disease 'YF'",
      class = "montagu_api_error")

  data$disease <- "YF"

  expect_error(montagu_burden_estimate_set_upload("IC-Garske", "201710gavi-5",
      "yf-no-vaccination", bsid, data, lines = Inf, location = location),
       paste0("We are not expecting data for age 150"),
      class = "montagu_api_error")

  data$age[data$age == 150] <- 50

  montagu_burden_estimate_set_upload("IC-Garske", "201710gavi-5",
        "yf-no-vaccination", bsid, data, lines = Inf, keep_open = TRUE,
        location = location)

  # Test clearing it...

  res <- montagu_burden_estimate_set_clear(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", bsid, location)
  expect_equal(res, "OK")

  # Re-add incomplete - should fail with keep_open = FALSE

  expect_error(montagu_burden_estimate_set_upload("IC-Garske", "201710gavi-5",
      "yf-no-vaccination", bsid, data, lines = Inf, keep_open = FALSE,
      location = location), "Missing rows",
      class = "montagu_api_error")

  # Re-add, duplicate data

  expect_error(montagu_burden_estimate_set_upload("IC-Garske", "201710gavi-5",
      "yf-no-vaccination", bsid, data, lines = Inf, keep_open = FALSE,
      location = location), "An object already exists",
      class = "montagu_api_error")

})

test_that("Create Burden Estimate - with keep_open=FALSE", {

  location <- montagu_test_server_user()

  bsid <- montagu_burden_estimate_set_create(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", "central-averaged",
    "Details", location)
  expect_is(bsid, "integer")

  # Convert this to use coverage.R when all is merged

  path <- "/modelling-groups/IC-Garske/expectations/201710gavi-5/30/?type=central"
  csv <- read.csv(
    text = rawToChar(montagu_api_GET(location, path, accept = "csv")),
    header = TRUE,
    stringsAsFactors = FALSE)
  csv$cohort_size <- sample(nrow(csv))
  csv$deaths <- sample(nrow(csv))
  csv$cases <- sample(nrow(csv))
  csv$dalys <- sample(nrow(csv))

  dat <- montagu_burden_estimate_set_upload("IC-Garske", "201710gavi-5",
        "yf-no-vaccination", bsid, csv, lines = Inf, keep_open = FALSE,
        location = location)

  # Dat seems to be nulll.
  expect_null(dat)

})

test_that("Create Burden Estimate - with keep_open=TRUE and close", {
  location <- montagu_test_server_user()

  bsid <- montagu_burden_estimate_set_create(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", "central-averaged",
    "Details", location)
  expect_is(bsid, "integer")

  # Get the template

  csv <- montagu_central_burden_estimate_template(
    "IC-Garske", "201710gavi-5", 30, location)

  # Fill in with utter nonsense

  csv$cohort_size <- sample(nrow(csv))
  csv$deaths <- sample(nrow(csv))
  csv$cases <- sample(nrow(csv))
  csv$dalys <- sample(nrow(csv))

  dat <- montagu_burden_estimate_set_upload("IC-Garske", "201710gavi-5",
         "yf-no-vaccination", bsid, csv, lines = Inf, keep_open = TRUE,
          location = location)

  # dat appears to be NULL here. Not a totally convincing test, but...
  expect_null(dat)

  dclose <- montagu_burden_estimate_set_close(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", bsid, location)
  expect_is(dclose, "character")
  expect_equal(dclose, "OK")

  # Close again - test for error.

  expect_error(montagu_burden_estimate_set_close(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", bsid, location),
    "This burden estimate set has already been closed.",
    class = "montagu_api_error")

})

###########################################################################
### Clear Burden Estimate Set (get) with errors

test_that("Clear non-open set - incorrect group", {
  location <- montagu_test_server_user()

  expect_error(montagu_burden_estimate_set_clear(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    paste0("You do not have sufficient permissions ",
           "to access this resource. Missing these permissions: ",
          "modelling-group:ZZZIC-Garske/estimates.write"),
    class = "montagu_api_error")
})

test_that("Clear non-open set - incorrect touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_clear(
    "IC-Garske", "ZZZ201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
    class = "montagu_api_error")
})

test_that("Clear non-open set - incorrect id", {
  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_clear(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown burden-estimate-set with id '10'",
    class = "montagu_api_error")
})

###########################################################################
### Close Burden Estimate Set with erroneous params

test_that("Close burden estimate set - incorrect group", {
  location <- montagu_test_server_user()

  expect_error(montagu_burden_estimate_set_close(
    "ZZZIC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    paste0("You do not have sufficient permissions ",
           "to access this resource. Missing these permissions: ",
           "modelling-group:ZZZIC-Garske/estimates.write"),
    class = "montagu_api_error")
})

test_that("Close burden estimate set - incorrect permissions", {

  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_close(
    "PHE-Vynnycky", "201710gavi-5", "rubella-rcv1-gavi", 740, location),
    paste0("You do not have sufficient permissions ",
           "to access this resource. Missing these permissions: ",
           "modelling-group:PHE-Vynnycky/estimates.write"),
    class = "montagu_api_error")
})

test_that("Clear non-open set - incorrect touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_close(
    "IC-Garske", "ZZZ201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
    class = "montagu_api_error")
})

test_that("Clear non-open set - incorrect id", {
  location <- montagu_test_server_user()
  expect_error(montagu_burden_estimate_set_close(
    "IC-Garske", "201710gavi-5", "yf-no-vaccination", 10, location),
    "Unknown burden-estimate-set with id '10'",
    class = "montagu_api_error")
})
