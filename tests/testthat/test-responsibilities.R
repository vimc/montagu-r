context("responsibilities")

test_that("download responbilities list", {
  location <- montagu_test_server_user()
  dat <- montagu_touchstones("IC-Garske", location = location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("name", "description", "comment"))
})

test_that("download responbilities list - incorrect modelling_group_id", {
  location <- montagu_test_server_admin()
  expect_error(montagu_touchstones("ZZZ-IC-Garske", location = location),
               "Unknown modelling-group with id 'ZZZ-IC-Garske'",
               class = "montagu_api_error")

  location <- montagu_test_server_user()
  expect_error(montagu_touchstones("ZZZ-IC-Garske", location = location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:ZZZ-IC-Garske/responsibilities.read"),
               class = "montagu_api_error")

  })

test_that("download responbilities list - incorrect permissions", {
  location <- montagu_test_server_user()
  expect_error(montagu_touchstones("PHE-Vynnycky", location = location),
               paste0("You do not have sufficient permissions to access this ",
                      "resource. Missing these permissions: ",
                      "modelling-group:PHE-Vynnycky/responsibilities.read"),
               class = "montagu_api_error")
})

test_that("download all touchstone versions", {
  location <- montagu_test_server_user()
  dat <- montagu_touchstones(NULL, location = location)
  expect_is(dat, "data.frame")
  expect_equal(sum(is.na(match(names(dat),
      c("name", "description", "comment")))), 0)
})

test_that("download touchstone versions", {
  location <- montagu_test_server_user()
  dat <- montagu_touchstone_versions("IC-Garske", "201710gavi", location = location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "name", "version", "description", "status"))
  expect_true(all(dat$name == "201710gavi"))
})

test_that("download touchstone versions - wrong permissions", {
  location <- montagu_test_server_user()
  expect_error(montagu_touchstone_versions("PHE-Vynnycky", "201710gavi", location = location),
      paste0("You do not have sufficient permissions to access this resource. ",
             "Missing these permissions: modelling-group:PHE-Vynnycky/responsibilities.read"),
      class = "montagu_api_error")
})

test_that("download all touchstone versions", {
  location <- montagu_test_server_user()
  dat <- montagu_touchstone_versions(NULL, "201710gavi", location = location)
  dat2 <- montagu_touchstone_versions(NULL, NULL, location = location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "name", "version", "description", "status"))
  expect_true(all(dat$name == "201710gavi"))
  expect_is(dat2, "data.frame")
  expect_equal(names(dat2), c("id", "name", "version", "description", "status"))
  expect_gt(nrow(dat2), nrow(dat))
})

test_that("download touchstone versions - wrong modelling group", {
  location <- montagu_test_server_admin()
  expect_error(montagu_touchstone_versions(
    "ZZZ-IC-Garske", "201710gavi", location = location),
    "Unknown modelling-group with id 'ZZZ-IC-Garske'",
    class = "montagu_api_error")

  location <- montagu_test_server_user()
  expect_error(montagu_touchstone_versions(
    "ZZZ-IC-Garske", "201710gavi", location = location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:ZZZ-IC-Garske/responsibilities.read"),
    class = "montagu_api_error")
})

test_that("download touchstone versions - wrong touchstone id", {
  location <- montagu_test_server_user()
  expect_error(montagu_touchstone_versions(
    "IC-Garske", "ZZZ201710gavi", location = location),
    "Unknown touchstone with id 'ZZZ201710gavi'",
    class = "simpleError")
})

################################################################################

test_that("download scenarios", {
  location <- montagu_test_server_user()
  dat <- montagu_scenarios("IC-Garske", "201710gavi-5", location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("scenario_id" ,"description", "disease"))
})

test_that("download all scenarios", {
  location <- montagu_test_server_user()
  expect_error(montagu_scenarios(NULL, "201710gavi-5", location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: \\*/responsibilities.read"),
    class = "montagu_api_error")

  location <- montagu_test_server_admin()
  dat <- montagu_scenarios(NULL, "201710gavi-5", location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("scenario_id" ,"description", "disease"))
})

test_that("download scenarios - wrong permissions", {
  location <- montagu_test_server_user()
  expect_error(montagu_scenarios("PHE-Vynnycky", "201710gavi-5", location),
     paste0("You do not have sufficient permissions to access this resource. ",
            "Missing these permissions: modelling-group:PHE-Vynnycky/responsibilities.read"),
     class = "montagu_api_error")
})

test_that("download scenarios - wrong group", {
  location <- montagu_test_server_admin()
  expect_error(montagu_scenarios("ZZZIC-Garske", "201710gavi-5", location),
    "Unknown modelling-group with id 'ZZZIC-Garske'",
    class = "montagu_api_error")

  location <- montagu_test_server_user()

  expect_error(montagu_scenarios("ZZZIC-Garske", "201710gavi-5", location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:ZZZIC-Garske/responsibilities.read"),
               class = "montagu_api_error")
})

test_that("download scenarios - wrong touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_scenarios("IC-Garske", "ZZZ201710gavi-5", location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
    class = "montagu_api_error")
})

###############################################################################

test_that("download scenario status", {
  location <- montagu_test_server_user()
  dat <- montagu_scenario_status("IC-Garske", "201710gavi-5",
                                 "yf-no-vaccination", location)
  expect_true(dat %in% c("valid", "invalid", "empty", "complete"))
})

test_that("download scenario status - all scenarios (no modelling group)", {
  location <- montagu_test_server_user()
  expect_error(montagu_scenario_status(NULL, "201710gavi-5",
                                       "yf-no-vaccination", location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: \\*/responsibilities.read"),
    class = "montagu_api_error")

  location <- montagu_test_server_admin()
  dat <- montagu_scenario_status(NULL, "201710gavi-5",
                                 "yf-no-vaccination", location)
  expect_true(dat %in% c("valid", "invalid", "empty", "complete"))
})

test_that("download scenario status - wrong modelling group", {
  location <- montagu_test_server_admin()
  expect_error(montagu_scenario_status("ZZZIC-Garske", "201710gavi-5",
               "yf-no-vaccination", location),
               "Unknown modelling-group with id 'ZZZIC-Garske'",
               class = "montagu_api_error")

  location <- montagu_test_server_user()
  expect_error(montagu_scenario_status("ZZZIC-Garske", "201710gavi-5",
         "yf-no-vaccination", location),
         paste0("You do not have sufficient permissions to access this resource. ",
                "Missing these permissions: modelling-group:ZZZIC-Garske/responsibilities.read"),
         class = "montagu_api_error")
})

test_that("download scenario status - wrong touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_scenario_status("IC-Garske", "ZZZ201710gavi-5",
                                       "yf-no-vaccination", location),
               "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
               class = "montagu_api_error")
})

test_that("download scenario status - wrong scenario", {
  location <- montagu_test_server_user()
  expect_error(montagu_scenario_status("IC-Garske", "201710gavi-5",
                                       "zzzyf-no-vaccination", location),
               "Unknown scenario with id 'zzzyf-no-vaccination'",
               class = "simpleError")
})

###############################################################################

test_that("download scenario problems", {
  location <- montagu_test_server_user()
  dat <- montagu_scenario_problems("IC-Garske", "201710gavi-5",
                                 "yf-no-vaccination", location)
  if (length(dat) == 0) {
    dat <- ""
  }
  expect_is(dat, "character")
})

test_that("download scenario problems - incorrect permission", {
  location <- montagu_test_server_user()
  expect_error(montagu_scenario_problems("PHE-Vynnycky", "201710gavi-5",
                                   "rubella-rcv1-gavi", location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:PHE-Vynnycky/responsibilities.read"),
    class = "montagu_api_error")
})

test_that("download scenario problems - null modelling group", {
  location <- montagu_test_server_user()
  expect_error(montagu_scenario_problems(NULL, "201710gavi-5",
                                         "yf-no-vaccination", location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: \\*/responsibilities.read"),
     class = "montagu_api_error")

  location <- montagu_test_server_admin()
  dat <- montagu_scenario_problems(NULL, "201710gavi-5",
                                   "yf-no-vaccination", location)
  if (length(dat) == 0) {
    dat <- ""
  }
  expect_is(dat, "character")
})

test_that("download scenario problems - wrong modelling group", {
  location <- montagu_test_server_admin()
  expect_error(montagu_scenario_problems("ZZZIC-Garske", "201710gavi-5",
                                       "yf-no-vaccination", location),
               "Unknown modelling-group with id 'ZZZIC-Garske'",
               class = "montagu_api_error")

  location <- montagu_test_server_user()
  expect_error(montagu_scenario_problems("ZZZIC-Garske", "201710gavi-5",
                                         "yf-no-vaccination", location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:ZZZIC-Garske/responsibilities.read"),
   class = "montagu_api_error")

})

test_that("download scenario problems - wrong touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_scenario_problems("IC-Garske", "ZZZ201710gavi-5",
                                       "yf-no-vaccination", location),
               "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
               class = "montagu_api_error")
})

test_that("download scenario problems - wrong scenario", {
  location <- montagu_test_server_user()
  expect_error(montagu_scenario_problems("IC-Garske", "201710gavi-5",
                                       "zzzyf-no-vaccination", location),
               "Unknown scenario with id 'zzzyf-no-vaccination'",
               class = "simpleError")
})

##############################################################################

test_that("download current_estimate_set info", {
  location <- montagu_test_server_user()
  dat <- montagu_current_estimate_set_info("IC-Garske", "201710gavi-5",
                                           "yf-no-vaccination", location)
  expect_is(dat, "list")
  expect_equal(names(dat), c("id", "uploaded_on", "uploaded_by", "type",
                             "details", "status"))
})

test_that("download current_estimate_set info - no permission", {
  location <- montagu_test_server_user()
  expect_error(montagu_current_estimate_set_info("PHE-Vynnycky", "201710gavi-5",
                                           "rubella-rcv1-gavi", location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:PHE-Vynnycky/responsibilities.read"),
    class = "montagu_api_error")
})

test_that("download current_estimate_set info - NULL group", {
  location <- montagu_test_server_user()
  expect_error(montagu_current_estimate_set_info(NULL, "201710gavi-5",
                                                 "yf-no-vaccination", location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: \\*/responsibilities.read"),
    class = "montagu_api_error")

  location <- montagu_test_server_admin()
  dat <- montagu_current_estimate_set_info(NULL, "201710gavi-5",
                                           "yf-no-vaccination", location)
  expect_is(dat, "list")
  expect_equal(names(dat), c("id", "uploaded_on", "uploaded_by", "type",
                             "details", "status"))
})

test_that("download current_estimate_set info - wrong modelling group", {
  location <- montagu_test_server_admin()

  expect_error(montagu_current_estimate_set_info("ZZZIC-Garske", "201710gavi-5",
                                         "yf-no-vaccination", location),
               "Unknown modelling-group with id 'ZZZIC-Garske'",
               class = "montagu_api_error")

  location <- montagu_test_server_user()
    expect_error(montagu_current_estimate_set_info("ZZZIC-Garske", "201710gavi-5",
                                                 "yf-no-vaccination", location),
      paste0("You do not have sufficient permissions to access this resource. ",
             "Missing these permissions: modelling-group:ZZZIC-Garske.responsibilities.read"),
               class = "montagu_api_error")

})

test_that("download current_estimate_set info - wrong touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_current_estimate_set_info("IC-Garske", "ZZZ201710gavi-5",
                                         "yf-no-vaccination", location),
               "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
               class = "montagu_api_error")
})

test_that("download current_estimate_set info - wrong scenario", {
  location <- montagu_test_server_user()
  expect_error(montagu_current_estimate_set_info("IC-Garske", "201710gavi-5",
                                         "zzzyf-no-vaccination", location),
               "Unknown scenario with id 'zzzyf-no-vaccination'",
               class = "simpleError")
})

##############################################################################

test_that("download current_estimate_set_problems", {
  location <- montagu_test_server_user()
  dat <- montagu_current_estimate_set_problems("IC-Garske", "201710gavi-5",
                                               "yf-no-vaccination", location)
  if (length(dat) == 0) {
    dat <- ""
  }
  expect_is(dat, "character")
})

test_that("download current_estimate_set_problems - no permission", {
  location <- montagu_test_server_user()
  expect_error(montagu_current_estimate_set_problems("PHE-Vynnycky", "201710gavi-5",
                                               "rubella-rcv1-gavi", location),
   paste0("You do not have sufficient permissions to access this resource. ",
          "Missing these permissions: modelling-group:PHE-Vynnycky/responsibilities.read"),
   class = "montagu_api_error")
})

test_that("download current_estimate_set_problems - wrong modelling group", {
  location <- montagu_test_server_admin()
  expect_error(montagu_current_estimate_set_problems("ZZZIC-Garske",
               "201710gavi-5", "yf-no-vaccination", location),
               "Unknown modelling-group with id 'ZZZIC-Garske'",
               class = "montagu_api_error")
  location <- montagu_test_server_user()

  expect_error(montagu_current_estimate_set_problems("ZZZIC-Garske",
       "201710gavi-5", "yf-no-vaccination", location),
       paste0("You do not have sufficient permissions to access this resource. ",
              "Missing these permissions: modelling-group:ZZZIC-Garske/responsibilities.read"),
      class = "montagu_api_error")
})

test_that("download current_estimate_set_problems - wrong touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_current_estimate_set_problems("IC-Garske",
               "ZZZ201710gavi-5", "yf-no-vaccination", location),
               "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
               class = "montagu_api_error")
})

test_that("download current_estimate_set_problems - wrong scenario", {
  location <- montagu_test_server_user()
  expect_error(montagu_current_estimate_set_problems("IC-Garske",
               "201710gavi-5", "zzzyf-no-vaccination", location),
               "Unknown scenario with id 'zzzyf-no-vaccination'",
               class = "simpleError")
})

##############################################################################

test_that("download scenarios for touchstone - wrong modelling group", {
  location <- montagu_test_server_admin()

  expect_error(montagu_touchstones_for_scenario("ZZZIC-Garske", "201710gavi-5",
             "yf-no-vaccination", location),
             "Unknown modelling-group with id 'ZZZIC-Garske'",
             class = "montagu_api_error")

  location <- montagu_test_server_user()
  expect_error(montagu_touchstones_for_scenario("ZZZIC-Garske", "201710gavi-5",
    "yf-no-vaccination", location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:ZZZIC-Garske/responsibilities.read"),
               class = "montagu_api_error")
})

test_that("download scenarios for touchstone - wrong modelling permission", {
  location <- montagu_test_server_user()
  expect_error(montagu_touchstones_for_scenario("PHE-Vynnycky", "201710gavi-5",
                                                "rubella-rcv1-gavi", location),
               paste0("You do not have sufficient permissions to access this resource. ",
                      "Missing these permissions: modelling-group:PHE-Vynnycky/responsibilities.read"),
               class = "montagu_api_error")
})

test_that("download scenarios for touchstone - wrong touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_touchstones_for_scenario("IC-Garske", "ZZZ201710gavi-5",
                                          "yf-no-vaccination", location),
               "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
               class = "montagu_api_error")
})

test_that("download scenarios for touchstone - wrong scenario", {
  location <- montagu_test_server_user()
  expect_error(montagu_touchstones_for_scenario("IC-Garske", "201710gavi-5",
                                        "zzzyf-no-vaccination", location),
               "Unknown scenario with id 'zzzyf-no-vaccination'",
               class = "simpleError")
})

test_that("download expectations ", {
  location <- montagu_test_server_user()
  dat <- montagu_expectations("IC-Garske", "201710gavi-5", location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "description", "min_year", "max_year",
               "min_age", "max_age", "min_birth_cohort", "max_birth_cohort",
               "disease"),
               class = "montagu_api_error")
})

test_that("download expectations - wrong modelling group", {
  location <- montagu_test_server_admin()
  expect_error(montagu_expectations("ZZZIC-Garske", "201710gavi-5", location),
               "Unknown modelling-group with id 'ZZZIC-Garske'",
               class = "montagu_api_error")

  location <- montagu_test_server_user()
  expect_error(montagu_expectations("ZZZIC-Garske", "201710gavi-5", location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:ZZZIC-Garske/responsibilities.read"),
   class = "montagu_api_error")
})

test_that("download expectations - wrong permission", {
  location <- montagu_test_server_user()
  expect_error(montagu_expectations("PHE-Vynnycky", "201710gavi-5", location),
     paste0("You do not have sufficient permissions to access this resource. ",
            "Missing these permissions: modelling-group:PHE-Vynnycky/responsibilities.read"),
     class = "montagu_api_error")
})

test_that("download expectations - wrong touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_expectations("IC-Garske", "ZZZ201710gavi-5", location),
               "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
               class = "montagu_api_error")
})

test_that("download expectation countries", {
  location <- montagu_test_server_user()
  dat <- montagu_expectation_countries("IC-Garske", "201710gavi-5",
                                       30, location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "name"))
})

test_that("download single expectation ", {
  location <- montagu_test_server_user()
  dat <- montagu_expectation("IC-Garske", "201710gavi-5", 30, location)
  expect_is(dat, "list")
  expect_equal(names(dat), c("id", "description", "min_year", "max_year",
                             "min_age", "max_age", "min_birth_cohort",
                             "max_birth_cohort", "disease"))
})

test_that("download single expectation - wrong modelling group", {
  location <- montagu_test_server_admin()
  expect_error(montagu_expectation("ZZZIC-Garske", "201710gavi-5",
               30, location),
               "Unknown modelling-group with id 'ZZZIC-Garske'",
               class = "montagu_api_error")

  location <- montagu_test_server_user()
  expect_error(montagu_expectation("ZZZIC-Garske", "201710gavi-5",
      30, location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:ZZZIC-Garske/responsibilities.read"),
   class = "montagu_api_error")
})

test_that("download single expectation - wrong touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_expectation("IC-Garske", "ZZZ201710gavi-5",
               30, location),
               "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
               class = "montagu_api_error")
})

test_that("download single expectation - wrong expectation id", {
  location <- montagu_test_server_user()
  expect_error(montagu_expectation("IC-Garske", "201710gavi-5",
               -5, location),
               "Unknown expectation with id '-5'",
               class = "simpleError")
})
test_that("download expectation countries", {
  location <- montagu_test_server_user()
  dat <- montagu_expectation_countries("IC-Garske", "201710gavi-5",
                                       30, location)
  expect_is(dat, "data.frame")
  expect_equal(names(dat), c("id", "name"))
})

test_that("download expectation countries - wrong permission", {
  location <- montagu_test_server_user()
  expect_error(montagu_expectation_countries("PHE-Vynnycky", "201710gavi-5",
                                             740, location),
      paste0("You do not have sufficient permissions to access this resource. ",
             "Missing these permissions: modelling-group:PHE-Vynnycky/responsibilities.read"),
      class = "montagu_api_error")
})

test_that("download expectation countries - wrong modelling group", {
  location <- montagu_test_server_user()
  expect_error(montagu_expectation_countries("ZZZIC-Garske", "201710gavi-5",
                                       30, location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:ZZZIC-Garske/responsibilities.read"),

               class = "montagu_api_error")
})

test_that("download expectation countries - wrong touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_expectation_countries("IC-Garske", "ZZZ201710gavi-5",
                                             30, location),
               "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
               class = "montagu_api_error")
})

test_that("download expectation countries - wrong expectation", {
  location <- montagu_test_server_user()
  expect_error(montagu_expectation_countries("IC-Garske", "201710gavi-5",
                                             -5, location),
               "Unknown expectation with id '-5'",
               class = "simpleError")
})

###############################################################################

test_that("download expectation outcomes", {
  location <- montagu_test_server_user()
  dat <- montagu_expectation_outcomes("IC-Garske", "201710gavi-5",
                                             30, location)
  expect_is(dat, "character")
})

test_that("download expectation outcomes - wrong permission", {
  location <- montagu_test_server_user()
  expect_error(montagu_expectation_outcomes("PHE-Vynnycky", "201710gavi-5",
                                             30, location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:PHE-Vynnycky/responsibilities.read"),
    class = "montagu_api_error")
})

test_that("download expectation outcomes - wrong modelling group", {
  location <- montagu_test_server_admin()
  expect_error(montagu_expectation_outcomes("ZZZIC-Garske", "201710gavi-5",
                                             30, location),
               "Unknown modelling-group with id 'ZZZIC-Garske'",
               class = "montagu_api_error")

  location <- montagu_test_server_user()
  expect_error(montagu_expectation_outcomes("ZZZIC-Garske", "201710gavi-5",
        30, location),
        paste0("You do not have sufficient permissions to access this resource. ",
               "Missing these permissions: modelling-group:ZZZIC-Garske/responsibilities.read"),
        class = "montagu_api_error")
})

test_that("download expectation outcomes - wrong touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_expectation_outcomes("IC-Garske", "ZZZ201710gavi-5",
                                             30, location),
               "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
               class = "montagu_api_error")
})

test_that("download expectation outcomes - wrong expectation", {
  location <- montagu_test_server_user()
  expect_error(montagu_expectation_outcomes("IC-Garske", "201710gavi-5",
                                             -5, location),
               "Unknown expectation with id '-5'",
               class = "simpleError")
})

##############################################################################

test_that("download expectation applicable scenarios", {
  location <- montagu_test_server_user()
  dat <- montagu_expectation_applicable_scenarios("IC-Garske", "201710gavi-5",
                                      30, location)
  expect_is(dat, "character")
})

test_that("download expectation applicable scenarios - wrong permission", {
  location <- montagu_test_server_user()
  expect_error(montagu_expectation_applicable_scenarios(
    "PHE-Vynnycky", "201710gavi-5", 30, location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:PHE-Vynnycky/responsibilities.read"),
    class = "montagu_api_error")
})

test_that("download expectation applicable scenarios - wrong modelling group", {
  location <- montagu_test_server_user()
  expect_error(montagu_expectation_applicable_scenarios("ZZZIC-Garske",
               "201710gavi-5", 30, location),
    paste0("You do not have sufficient permissions to access this resource. ",
          "Missing these permissions: modelling-group:ZZZIC-Garske/responsibilities.read"),

               class = "montagu_api_error")
})

test_that("download expectation applicable scenarios - wrong touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_expectation_applicable_scenarios("IC-Garske",
               "ZZZ201710gavi-5", 30, location),
               "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
               class = "montagu_api_error")
})

test_that("download expectation applicable scenarios - wrong expectation", {
  location <- montagu_test_server_user()
  expect_error(montagu_expectation_applicable_scenarios("IC-Garske",
               "201710gavi-5", -5, location),
               "Unknown expectation with id '-5'",
               class = "simpleError")
})
###############################################################################

test_that("download central burden_estimate_template", {
  location <- montagu_test_server_user()
  dat <- montagu_central_burden_estimate_template("IC-Garske", "201710gavi-5",
                                          30, location)
  expect_is(dat, "data.frame")
  expect_equal(ncol(dat), 9)
  expect_gt(nrow(dat),1)
  countries <- sort(montagu_expectation_countries("IC-Garske", "201710gavi-5", 30,
                                                  location)$id)

  expect_equal(countries, sort(unique(dat$country)))
})

test_that("download central burden estimate template - wrong permission", {
  location <- montagu_test_server_user()
  expect_error(montagu_central_burden_estimate_template(
                   "PHE-Vynnycky", "201710gavi-5", 30, location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:PHE-Vynnycky/responsibilities.read"),
    class = "montagu_api_error")
})

test_that("download central burden_estimate_template - wrong modelling group", {
  location <- montagu_test_server_user()
  expect_error(montagu_central_burden_estimate_template(
    "ZZZIC-Garske", "201710gavi-5", 30, location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:ZZZIC-Garske/responsibilities.read"),
    class = "montagu_api_error")
})

test_that("download central burden_estimate_template - wrong touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_central_burden_estimate_template(
    "IC-Garske", "ZZZ201710gavi-5", 30, location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
    class = "montagu_api_error")
})

test_that("download central burden_estimate_template - wrong expectation id", {
  location <- montagu_test_server_user()
  expect_error(montagu_central_burden_estimate_template(
    "IC-Garske", "201710gavi-5", -5, location),
    "Unknown burden-estimate-expectation with id '-5'",
    class = "montagu_api_error")
})

test_that("download stochastic burden_estimate_template", {
  location <- montagu_test_server_user()
  dat <- montagu_stochastic_burden_estimate_template("IC-Garske", "201710gavi-5",
                                                  30, location)
  expect_is(dat, "data.frame")
  expect_equal(ncol(dat), 10)
  expect_gt(nrow(dat),1)

  countries <- sort(montagu_expectation_countries("IC-Garske", "201710gavi-5", 30,
                                             location)$id)

  expect_equal(countries, sort(unique(dat$country)))
})

test_that("download stochastic burden_estimate_template  - wrong permission", {
  location <- montagu_test_server_user()
  expect_error(montagu_stochastic_burden_estimate_template(
    "PHE-Vynnycky", "201710gavi-5", 30, location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:PHE-Vynnycky/responsibilities.read"),
    class = "montagu_api_error")
})

test_that("download stochastic burden_estimate_template - wrong group", {
  location <- montagu_test_server_admin()
  expect_error(montagu_stochastic_burden_estimate_template(
    "ZZZIC-Garske", "201710gavi-5", 30, location),
    sprintf("Unknown modelling-group with id '%s'", "ZZZIC-Garske"),

    class = "montagu_api_error")

  location <- montagu_test_server_user()
  expect_error(montagu_stochastic_burden_estimate_template(
    "ZZZIC-Garske", "201710gavi-5", 30, location),
    paste0("You do not have sufficient permissions to access this resource. ",
           "Missing these permissions: modelling-group:ZZZIC-Garske/responsibilities.read"),

    class = "montagu_api_error")
})

test_that("download stochastic burden_estimate_template - wrong touchstone", {
  location <- montagu_test_server_user()
  expect_error(montagu_stochastic_burden_estimate_template(
    "IC-Garske", "ZZZ201710gavi-5", 30, location),
    "Unknown touchstone-version with id 'ZZZ201710gavi-5'",
    class = "montagu_api_error")
})

test_that("download stochastic burden_estimate_template wrong expectation id", {
  location <- montagu_test_server_user()
  expect_error(montagu_stochastic_burden_estimate_template(
    "IC-Garske", "201710gavi-5", -5, location),
    "Unknown burden-estimate-expectation with id '-5'",
    class = "montagu_api_error")
})
