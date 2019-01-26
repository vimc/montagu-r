helper_touchstones <- function(modelling_group_id, location = NULL) {
  assert_scalar_character(modelling_group_id)
  path <- sprintf("/modelling-groups/%s/responsibilities/", modelling_group_id)
  montagu_api_GET(location, path)
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Retrieve touchstones a modelling group is responsible for.
##' @param location The montagu server to connect to.
##' @param modelling_group_id Id of the modelling group.
##' @return Data frame of touchstone name, description and comment.
##' @export
montagu_touchstones <- function(modelling_group_id, location = NULL) {
  res <- helper_touchstones(modelling_group_id, location)
  data_frame(
    name = vcapply(res, "[[", "id"),
    description = vcapply(res, "[[", "description"),
    comment = vcapply(res, "[[", "comment"))
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Retrieve list of all versions of a given touchstone.
##' @param touchstone_name Name of the touchstone (ie, no version suffix)
##' @inheritParams montagu_touchstones
##' @return Data frame of touchstine id, name, version, description and status
##' @export
montagu_touchstone_versions <- function(modelling_group_id,
                                        touchstone_name, location = NULL) {
  assert_scalar_character(touchstone_name)
  res <- helper_touchstones(modelling_group_id, location)

  if (length(res)==0) {
    stop(sprintf("Unknown touchstone with id '%s'", touchstone_name))

  } else if (sum(which(vcapply(res, "[[", "id") == touchstone_name))>0) {
    versions <- res[[which(vcapply(res, "[[", "id") == touchstone_name)]]$versions
    data_frame(
      id = vcapply(versions, "[[", "id"),
      name = vcapply(versions, "[[", "name"),
      version = viapply(versions, "[[", "version"),
      description = vcapply(versions, "[[", "description"),
      status = vcapply(versions, "[[", "status"))

  } else {
    stop(sprintf("Unknown touchstone with id '%s'", touchstone_name))
  }
}

helper_get_touchstone <- function(modelling_group_id, touchstone_id,
                                  location = NULL) {
  assert_scalar_character(modelling_group_id)
  assert_scalar_character(touchstone_id)
  path <- sprintf("/modelling-groups/%s/responsibilities/%s",
                  modelling_group_id, touchstone_id)
  montagu_api_GET(location, path)
}

helper_get_responsibility <- function(modelling_group_id, touchstone_id,
                                      scenario_id, location = NULL) {

  resps <- helper_get_touchstone(modelling_group_id, touchstone_id,
                                 location)$responsibilities

  select <- which(vcapply(resps, function(x) x$scenario$id) == scenario_id)
  if (length(select)==0) {
    stop(sprintf("Unknown scenario with id '%s'", scenario_id))
  }
  resps[[select]]
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Retrieve information about a scenario
##' @param touchstone_id Id of the touchstone (includes version suffix)
##' @inheritParams montagu_touchstone_versions
##' @return Data frame of scenario_id, description and disease.
##' @export
montagu_scenarios <- function(modelling_group_id,
                              touchstone_id, location = NULL) {
  resps <- helper_get_touchstone(modelling_group_id, touchstone_id,
                                 location)$responsibilities
  data_frame(
    scenario_id = vcapply(resps, function(x) x$scenario$id),
    description = vcapply(resps, function(x) x$scenario$description),
    disease = vcapply(resps, function(x) x$scenario$disease))
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Retrieve current status of a groups' scenario.
##' @param scenario_id Id of the scenario
##' @inheritParams montagu_scenarios
##' @return "invalid", "complete", "valid", or "empty"
##' @export
montagu_scenario_status <- function(
  modelling_group_id, touchstone_id, scenario_id, location = NULL) {

  helper_get_responsibility(modelling_group_id, touchstone_id, scenario_id,
                      location)$status
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Retrieve current status of a groups' scenario.
##' @inheritParams montagu_scenario_status
##' @return A list of problems.
##' @export
montagu_scenario_problems <- function(
  modelling_group_id, touchstone_id, scenario_id, location = NULL) {

  helper_get_responsibility(modelling_group_id,
    touchstone_id, scenario_id, location)$problems
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Get information on current estimate set for a scenario.
##' @inheritParams montagu_scenario_status
##' @return A list of fields about the current estimate set
##' @export
montagu_current_estimate_set_info <- function(
  modelling_group_id, touchstone_id, scenario_id, location = NULL) {

  ces <- helper_get_responsibility(modelling_group_id,
    touchstone_id, scenario_id, location)$current_estimate_set

  list(id = ces$id,
       uploaded_on = ces$uploaded_on,
       uploaded_by = ces$uploaded_by,
       type = ces$type$type,
       details = ces$type$details,
       status = ces$status)
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Get information on current estimate set for a scenario.
##' @inheritParams montagu_scenario_status
##' @return A list of problems.
##' @export
montagu_current_estimate_set_problems <- function(
  modelling_group_id, touchstone_id, scenario_id, location = NULL) {

  helper_get_responsibility(modelling_group_id,
     touchstone_id, scenario_id, location)$current_estimate_set$problems
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Get touchstones that include this scenario
##' @inheritParams montagu_scenario_status
##' @return A vector of touchstone ids
##' @export
montagu_touchstones_for_scenario <- function(
  modelling_group_id, touchstone_id, scenario_id, location = NULL) {

  helper_get_responsibility(modelling_group_id,
            touchstone_id, scenario_id, location)$scenario$touchstones
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Get expectations for a modelling group's touchstone
##' @inheritParams montagu_scenario_status
##' @return A data frame of information about the expectation
##' @export
montagu_expectations <- function(
  modelling_group_id, touchstone_id, location = NULL) {

  exps <- helper_get_touchstone(modelling_group_id, touchstone_id,
                                location)$expectations
  data_frame(
    id = viapply(exps, function(x) x$expectation$id),
    description = vcapply(exps, function(x) x$expectation$description),
    min_year = viapply(exps, function(x) x$expectation$years$minimum_inclusive),
    max_year = viapply(exps, function(x) x$expectation$years$maximum_inclusive),
    min_age = viapply(exps, function(x) x$expectation$ages$minimum_inclusive),
    max_age = viapply(exps, function(x) x$expectation$ages$maximum_inclusive),
    min_birth_cohort = viapply(exps,
                        function(x) x$expectation$cohorts$minimum_birth_year),
    max_birth_cohort = viapply(exps,
                        function(x) x$expectation$cohorts$maximum_birth_year),
    disease = vcapply(exps, function(x) x$disease)
  )
}

helper_get_expectation <- function(modelling_group_id, touchstone_id,
                                   expectation_id, location) {
  assert_integer_like(expectation_id)

  exps <- helper_get_touchstone(modelling_group_id, touchstone_id,
                                location)$expectations
  select <- which(viapply(exps, function(x) x$expectation$id) == expectation_id)

  if (length(select)==0) {
    stop(sprintf("Unknown expectation with id '%s'", as.character(expectation_id)))
  } else {
    exps[[select]]
  }
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Get information about an expectation
##' @inheritParams montagu_scenario_status
##' @param expectation_id Integer ID of the expectation
##' @return A list of data about the expectation
##' @export
montagu_expectation <- function(
  modelling_group_id, touchstone_id, expectation_id, location = NULL) {

  expect <- helper_get_expectation(modelling_group_id, touchstone_id,
                                 expectation_id, location)$expectation
  list(
    id = expect$id,
    description = expect$description,
    min_year = expect$years$minimum_inclusive,
    max_year = expect$years$maximum_inclusive,
    min_age = expect$ages$minimum_inclusive,
    max_age = expect$ages$maximum_inclusive,
    min_birth_cohort = expect$cohorts$minimum_birth_year,
    max_birth_cohort = expect$cohorts$maximum_birth_year,
    disease = expect$disease
  )
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Get country list for an expectation
##' @inheritParams montagu_expectation
##' @param expectation_id Integer ID of the expectation
##' @return A data frame of country id and name
##' @export
montagu_expectation_countries <- function(
  modelling_group_id, touchstone_id, expectation_id, location = NULL) {

  countries <- helper_get_expectation(modelling_group_id, touchstone_id,
                expectation_id, location)$expectation$countries

  data_frame(id = vcapply(countries, function(x) x$id),
             name = vcapply(countries, function(x) x$name))
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Get expected outcomes
##' @inheritParams montagu_expectation
##' @return A vector of outcome names
##' @export
montagu_expectation_outcomes <- function(
  modelling_group_id, touchstone_id, expectation_id, location = NULL) {

  helper_get_expectation(modelling_group_id, touchstone_id,
                         expectation_id, location)$expectation$outcomes
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Get applicable scenarios for an expectation
##' @inheritParams montagu_expectation
##' @return A vector of scenario names
##' @export
montagu_expectation_applicable_scenarios <- function(
  modelling_group_id, touchstone_id, expectation_id, location = NULL) {

  helper_get_expectation(modelling_group_id, touchstone_id,
                         expectation_id, location)$applicable_scenarios
}

helper_burden_estimate_template <- function(
  modelling_group_id, touchstone_id, expectation_id, type, location = NULL) {

  assert_scalar_character(modelling_group_id)
  assert_scalar_character(touchstone_id)
  assert_integer_like(expectation_id)
  path <- sprintf("/modelling-groups/%s/expectations/%s/%s/?type=%s",
                  modelling_group_id, touchstone_id, expectation_id, type)

  res <- rawToChar(montagu_api_GET(location, path, accept = "csv"))
  read.csv(text = res, header = TRUE)
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Get central burden estimate template for expectation
##' @inheritParams montagu_expectation
##' @importFrom utils read.csv
##' @return A data frame with columns disease, year, age, country, and
##'         country_name with given values, then cohort_size, deaths,
##'         cases and dalys, all NA.
##' @export
montagu_central_burden_estimate_template <- function(modelling_group_id,
  touchstone_id, expectation_id, location) {
  helper_burden_estimate_template(modelling_group_id, touchstone_id,
                                  expectation_id, "central", location)
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Get central burden estimate template for expectation
##' @inheritParams montagu_expectation
##' @importFrom utils read.csv
##' @return A data frame with columns disease (given) run_id (NA), year, age,
##'         country, and country_name (given), and finally
##'         cohort_size, deaths, cases and dalys, (all NA).
##' @export
montagu_stochastic_burden_estimate_template <- function(modelling_group_id,
                                  touchstone_id, expectation_id, location) {

  helper_burden_estimate_template(modelling_group_id, touchstone_id,
                                  expectation_id, "stochastic", location)
}
