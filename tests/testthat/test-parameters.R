context("parameters")

test_that("download list of model_run_parameter_sets", {
  location <- montagu_test_server()
  dat <- montagu_model_run_parameter_sets("IC-Garske", "201710gavi-5", location)
  expect_is(dat, "data.frame")
  expect_equal(ncol(dat), 5)
  expect_false(anyNA(match(c("id", "model", "uploaded_by", "uploaded_on", "disease"),
                    names(dat))))
})

test_that("download list of model_run_parameter_sets - unknown group", {
  location <- montagu_test_server()
  expect_error(montagu_model_run_parameter_sets("ZZZIC-Garske", "201710gavi-5",
                                                location),
               paste0("You do not have sufficient permissions ",
                      "to access this resource. Missing these permissions: ",
                      "modelling-group:ZZZIC-Garske/estimates.write"))
})

test_that("download list of model_run_parameter_sets - unknown touchstone", {
  location <- montagu_test_server()
  expect_error(montagu_model_run_parameter_sets("IC-Garske", "ZZZ201710gavi-5",
                                                location),
               "Unknown touchstone-version with id 'ZZZ201710gavi-5'")
})

###############################################################################

test_that("download single model_run_parameter_set", {
  location <- montagu_test_server()
  dat <- montagu_model_run_parameter_set_info("IC-Garske", "201710gavi-5",
                                              20, location)
  expect_is(dat, "list")
  expect_equal(length(dat), 5)
  expect_false(anyNA(match(c("id", "model", "uploaded_by", "uploaded_on", "disease"),
                           names(dat))))
})

test_that("download single model_run_parameter_set - unknown group", {
  location <- montagu_test_server()
  expect_error(montagu_model_run_parameter_set_info("ZZZIC-Garske", "201710gavi-5",
                                                20, location),
               paste0("You do not have sufficient permissions ",
                      "to access this resource. Missing these permissions: ",
                      "modelling-group:ZZZIC-Garske/estimates.write"))

})

test_that("download single model_run_parameter_set - unknown touchstone", {
  location <- montagu_test_server()
  expect_error(montagu_model_run_parameter_set_info("IC-Garske", "ZZZ201710gavi-5",
                                                20, location),
               "Unknown touchstone-version with id 'ZZZ201710gavi-5'")
})

test_that("download single model_run_parameter_set - unknown id", {
  location <- montagu_test_server()
  expect_error(montagu_model_run_parameter_set_info("IC-Garske", "201710gavi-5",
                                                    1, location),
               "Unknown model_run_parameter_set_id '1'")
})

###############################################################################

test_that("download single model_run_parameter_set data", {
  location <- montagu_test_server()
  dat <- montagu_model_run_parameter_set_data("IC-Garske", "201710gavi-5",
                                              20, location)
  expect_is(dat, "data.frame")
  expect_true("run_id" %in% names(dat))
})

test_that("download single model_run_parameter_set data - unknown group", {
  location <- montagu_test_server()
  expect_error(montagu_model_run_parameter_set_data("ZZZIC-Garske", "201710gavi-5",
                                                    20, location), 
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: ",
           "modelling-group:ZZZIC-Garske/estimates.write"
    ))
})

test_that("download single model_run_parameter_set data - unknown touchstone", {
  location <- montagu_test_server()
  expect_error(montagu_model_run_parameter_set_data("IC-Garske", "ZZZ201710gavi-5",
                                              20, location),
      "Unknown touchstone-version with id 'ZZZ201710gavi-5'")
})

test_that("download single model_run_parameter_set data - id in other group/touchstone", {
  location <- montagu_test_server()
  expect_error(montagu_model_run_parameter_set_data("IC-Garske", "201710gavi-5",
                                                    1, location),
               "Unknown model run parameter set with id '1'")
})

test_that("download single model_run_parameter_set data - stupid id", {
  location <- montagu_test_server()
  expect_error(montagu_model_run_parameter_set_data("IC-Garske", "201710gavi-5",
                                                    -1234, location),
               "Unknown model run parameter set with id '-1234'")
})

################################################################################

test_that("upload model_run_parameter_set", {
  location <- montagu_test_server()
  params <- data_frame(run_id = 1:5, rnd_1 = sample(5), rnd_2 = sample(5))

  id <- montagu_model_run_parameter_set_upload(
    "IC-Garske", "201710gavi-5", "YF", params, location)

  expect_is(id, "numeric")

  # Fetch it back...
  
  params2 <- montagu_model_run_parameter_set_data(
    "IC-Garske", "201710gavi-5", id, location)
  expect_equal(sum(is.na(match(names(params), names(params2)))), 0)
  
  params <- params[c("run_id", "rnd_1", "rnd_2")]
  params2 <- params2[c("run_id", "rnd_1", "rnd_2")]
  params2 <- params2[order(params2$run_id), ]
  expect_true(all.equal(params, params2))
  
})

test_that("upload model_run_parameter_set - col order tests", {
  location <- montagu_test_server()
  params <- data_frame(rnd_1 = sample(5), run_id = 1:5, rnd_2 = sample(5))
  
  id <- montagu_model_run_parameter_set_upload(
    "IC-Garske", "201710gavi-5", "YF", params, location)
  
  expect_is(id, "numeric")
  
  # Fetch it back...
  
  params2 <- montagu_model_run_parameter_set_data(
    "IC-Garske", "201710gavi-5", id, location)
  
  params <- params[c("run_id", "rnd_1", "rnd_2")]
  params2 <- params2[c("run_id", "rnd_1", "rnd_2")]
  params2 <- params2[order(params2$run_id), ]
  
  expect_true(all.equal(params, params2))
  
})

test_that("upload model_run_parameter_set data - unknown group", {
  location <- montagu_test_server()
  params <- data_frame(run_id = 1:5, rnd_1 = sample(5), rnd_2 = sample(5))
  expect_error(montagu_model_run_parameter_set_upload(
    "ZZZIC-Garske", "201710gavi-5", "YF", params, location),
    paste0("You do not have sufficient permissions to access this resource. ",
            "Missing these permissions: ",
            "modelling-group:ZZZIC-Garske/estimates.write"))
})

test_that("upload model_run_parameter_set data - unknown touchstone", {
  location <- montagu_test_server()
  params <- data_frame(run_id = 1:5, rnd_1 = sample(5), rnd_2 = sample(5))
  expect_error(montagu_model_run_parameter_set_upload(
    "IC-Garske", "ZZZ201710gavi-5", "YF", params, location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'")

})

test_that("upload model_run_parameter_set data - no run_id parameter", {
  location <- montagu_test_server()
  params <- data_frame(rnd_1 = sample(5), rnd_2 = sample(5))
  expect_error(montagu_model_run_parameter_set_upload(
    "IC-Garske", "201710gavi-5", "YF", params, location),
    "run_id not found in data frame")
})

test_that("upload model_run_parameter_set data - run_id but no no parameters", {
  location <- montagu_test_server()
  params <- data_frame(run_id = 1:5)
  expect_error(montagu_model_run_parameter_set_upload(
    "IC-Garske", "201710gavi-5", "YF", params, location),
    "No actual parameters in data frame")
})

test_that("upload model_run_parameter_set data - silly disease_id", {
  location <- montagu_test_server()
  params <- data_frame(run_id = 1:5, rnd_1 = sample(5), rnd_2 = sample(5))
  expect_error(montagu_model_run_parameter_set_upload(
    "IC-Garske", "201710gavi-5", "THE_ELVES_ARE_CURIOUS", params, location),
    paste0("Modelling group IC-Garske does not have any models/model versions ",
           "in the database"))
})

test_that("upload model_run_parameter_set data - wrong disease_id for group", {
  location <- montagu_test_server()
  params <- data_frame(run_id = 1:5, rnd_1 = sample(5), rnd_2 = sample(5))
  expect_error(montagu_model_run_parameter_set_upload(
    "IC-Garske", "201710gavi-5", "MenA", params, location),
    paste0("Modelling group IC-Garske does not have any models/model versions ",
           "in the database"))
})
