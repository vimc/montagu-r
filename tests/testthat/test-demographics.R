context("demographics")

test_that("demographic list with invalid touchstone", {
  location <- montagu_location(montagu_test_server())
  expect_error(montagu_demographics_list("ZZZ201710gavi-5", location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'")
})

test_that("demographic list - working", {
  location <- montagu_location(montagu_test_server())
  demog <- montagu_demographics_list("201710gavi-5", location)
  expect_equal(sort(match(names(demog), c("id", "name", "gendered", "source"))),
               c(1,2,3,4))
})

################################################################################

test_that("download demographic data", {
  location <- montagu_test_server()
  dat <- montagu_demographic_data("cbr", "201804rfp-1", location = location)
  expect_is(dat, "data.frame")
  expect_equal(sort(match(names(dat), c("country_code_numeric", "country_code",
    "country", "age_from", "age_to", "year", "gender", "value"))),
    c(1,2,3,4,5,6,7,8))
})

test_that("download demographic data - something basic wrong", {
  location <- montagu_test_server()
  expect_error(montagu_demographic_data("cbr", "ZZZ201804rfp-1",
                                       location = location),
               "Unknown touchstone-version with id 'ZZZ201804rfp-1'")

  expect_error(montagu_demographic_data("elf", "201804rfp-1",
                                        location = location),
               "Unknown demographic-statistic-type with id 'elf'")

})

test_that("download demographic data - gendered tests", {
  location <- montagu_test_server()
  expect_error(montagu_demographic_data("cbr", "201804rfp-1",
    gender_code = 'elf', location = location),
    "Invalid gender code 'elf' - use male, female or both")

  for (g in c("male", "female")) {
    expect_error(montagu_demographic_data("cbr", "201804rfp-1",
               gender_code = g, location = location),
    sprintf(paste0("The demographic type 'cbr' is not gendered, so ",
           "cannot be filtered by '%s'"), g))
  }

  d1 <- montagu_demographic_data("cbr", "201804rfp-1",
            gender_code = 'both', location = location)
  expect_is(d1, "data.frame")
  expect_equal(nrow(d1), 150)

})

test_that("download demographic data - format tests", {
  location <- montagu_test_server()
  dl <- montagu_demographic_data("as_fert", "201804rfp-1",
      wide = FALSE, location = location)

  dw <- montagu_demographic_data("as_fert", "201804rfp-1",
      wide = TRUE, location = location)

  expect_is(dl, "data.frame")
  expect_is(dw, "data.frame")
  expect_gt(nrow(dl), 0)
  expect_gt(nrow(dw), 0)
  expect_gt(nrow(dl), nrow(dw))
})

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

