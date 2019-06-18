helper_touchstones <- function(modelling_group_id = NULL, location = NULL) {
  if (is.null(modelling_group_id)) {
    montagu_api_GET(location, "/touchstones/")
  } else {
    assert_scalar_character(modelling_group_id)
    path <- sprintf("/modelling-groups/%s/responsibilities/", modelling_group_id)
    montagu_api_GET(location, path)
  }
}
##' Touchstones are created by the VIMC management. The touchstone id associates
##' a particular call for burden estimates, with the input data (coverage and
##' demography) required to produce those estimates, and with the results that
##' the modelling groups provide. A touchstone has a base name, and a version.
##' @title Retrieve touchstones a modelling group is responsible for.
##' @param location The montagu server to connect to.
##' @param modelling_group_id id of the modelling group. If omitted or null, 
##' then all touchstones are returned.
##' @return Data frame of touchstone name, description and comment.
##' @export
montagu_touchstones <- function(modelling_group_id = NULL, location = NULL) {
  res <- helper_touchstones(modelling_group_id, location)
  data_frame(
    name = vcapply(res, "[[", "id"),
    description = vcapply(res, "[[", "description"),
    comment = vcapply(res, "[[", "comment"))
}

##' Touchstones are versioned; errata in the input or coverage data may be
##' addressed with a new version of the existing touchstone. When interacting
##' with Montagu, a touchstone_id will consist of a basename, and a version.
##' @title Retrieve list of all versions of a given touchstone.
##' @param touchstone_name Optional base name of the touchstone to filter. 
##' (ie, no version suffix)
##' @param require_open If true, include only open touchstones
##' @inheritParams montagu_touchstones
##' @return Data frame of touchstone id, name, version, description and status
##' @export
montagu_touchstone_versions <- function(modelling_group_id = NULL,
                                        touchstone_name = NULL,
                                        require_open = FALSE, location = NULL) {
  
  res <- montagu_touchstones(modelling_group_id, location)
  res2 <- helper_touchstones(modelling_group_id, location)
  
  if (!is.null(touchstone_name)) {
    res <- res[res$name == touchstone_name, ]
  }

  if (nrow(res) == 0) {
    stop(sprintf("Unknown touchstone with id '%s'", touchstone_name))
  }

  collect_data <- NULL

  for (i in seq_len(nrow(res))) {

    versions <- res2[[which(vcapply(res2, "[[", "id") == res$name[i])]]$versions

    collect_data <- rbind(collect_data, data_frame(
        id = vcapply(versions, "[[", "id"),
        name = vcapply(versions, "[[", "name"),
        version = viapply(versions, "[[", "version"),
        description = vcapply(versions, "[[", "description"),
        status = vcapply(versions, "[[", "status")))
  }
  if (require_open) {
    collect_data <- collect_data[collect_data$status == 'open', ]
  }
  collect_data
}

################################################################################

helper_get_touchstone <- function(modelling_group_id, 
                                  touchstone_id,
                                  location = NULL) {
  if (is.null(modelling_group_id)) {
    path <- sprintf("/touchstones/%s/responsibilities/",
                    touchstone_id)
    montagu_api_GET(location, path)
  
  } else {
    assert_scalar_character(modelling_group_id)
    assert_scalar_character(touchstone_id)
    path <- sprintf("/modelling-groups/%s/responsibilities/%s/",
                    modelling_group_id, touchstone_id)
    montagu_api_GET(location, path)
  }
}

helper_get_responsibilities <- function(modelling_group_id, touchstone_id, 
                                        location) {
  resps <- helper_get_touchstone(modelling_group_id, touchstone_id, location)
  
  if (is.null(modelling_group_id)) {
    resps <- lapply(resps, "[[", "responsibilities")[[1]]
    
  } else {
    resps <- resps$responsibilities
  }
  
  resps
}

helper_get_responsibility <- function(modelling_group_id, touchstone_id,
                                      scenario_id, location = NULL) {

  resps <- helper_get_responsibilities(modelling_group_id, touchstone_id,
                                       location)

  select <- which(vcapply(resps, function(x) x$scenario$id) == scenario_id)
  if (length(select) == 0) {
    stop(sprintf("Unknown scenario with id '%s'", scenario_id))
  }
  resps[[select]]
}


##' A scenario describes the vaccination conditions for a particular run of a
##' model. Typical examples include a scenario where there is no vaccination,
##' a scenario where there is routine (background) vaccination, or a scenario
##' in which there are targetted campaigns to vaccinate particularly ages.
##' Depending on disease, modelling groups may be asked to model various
##' scenarios, for a particular touchstone.
##' @title Retrieve information about a scenario
##' @param modelling_group_id id of the modelling group. If omitted, or null, 
##' then all scenarios associated with the touchstone are returned.
##' @param touchstone_id id of the touchstone (including version)
##' @param location The montagu server to connect to.
##' @return Data frame of scenario_id, description and disease.
##' @export
montagu_scenarios <- function(modelling_group_id = NULL, 
                              touchstone_id,
                              location = NULL) {

  resps <- helper_get_responsibilities(modelling_group_id, touchstone_id, 
                                       location)
  data_frame(
    scenario_id = vcapply(resps, function(x) x$scenario$id),
    description = vcapply(resps, function(x) x$scenario$description),
    disease = vcapply(resps, function(x) x$scenario$disease))
}

##' @title Retrieve current status of a groups' scenario.
##' @param scenario_id id of the scenario
##' @inherit montagu_scenarios
##' @return "invalid", "complete", "valid", or "empty"
##' @export
montagu_scenario_status <- function(modelling_group_id, touchstone_id,
                                    scenario_id, location = NULL) {

  helper_get_responsibility(modelling_group_id, touchstone_id, scenario_id,
                            location)$status
}

##' @title Retrieve a list of any problems with a scenario.
##' @inherit montagu_scenario_status
##' @return A list of problem text.
##' @export
montagu_scenario_problems <- function(modelling_group_id, touchstone_id,
                                      scenario_id, location = NULL) {

  helper_get_responsibility(modelling_group_id, touchstone_id,
                            scenario_id, location)$problems
}

##' Groups upload estimate sets to Montagu. The most recent set of results
##' uploaded by a group for a given scenario can be queried for its status,
##' or retrieved.
##' @title Get information on current estimate set for a scenario.
##' @inheritParams montagu_scenario_status
##' @return A list of fields about the current estimate set
##' @export
montagu_current_estimate_set_info <- function(modelling_group_id,
                                              touchstone_id, scenario_id,
                                              location = NULL) {

  ces <- helper_get_responsibility(modelling_group_id,
    touchstone_id, scenario_id, location)$current_estimate_set

  list(id = ces$id,
       uploaded_on = ces$uploaded_on,
       uploaded_by = ces$uploaded_by,
       type = ces$type$type,
       details = ces$type$details,
       status = ces$status)
}

##' Groups upload estimate sets to Montagu. The most recent set of results
##' uploaded by a group for a given scenario can be queried for its status,
##' or retrieved.
##' @title Retrieve list of any problems in current estimate set for a scenario.
##' @inheritParams montagu_scenario_status
##' @return A list of problem text.
##' @export
montagu_current_estimate_set_problems <- function(modelling_group_id,
                                                  touchstone_id, scenario_id,
                                                  location = NULL) {

  helper_get_responsibility(modelling_group_id,
     touchstone_id, scenario_id, location)$current_estimate_set$problems
}

##' Given a particular touchstone and scenario, we can loookup all the other
##' touchstones that also contain this scenario.
##' @title Get touchstones that include this scenario
##' @inheritParams montagu_scenario_status
##' @return vector of touchstone ids
##' @export
montagu_touchstones_for_scenario <- function(modelling_group_id, touchstone_id,
                                             scenario_id, location = NULL) {

  helper_get_responsibility(modelling_group_id,
            touchstone_id, scenario_id, location)$scenario$touchstones
}

##' The expectations, for a modelling group for a particular touchstone, indicate the
##' range of chronological years, ages and countries for which burden estimates
##' are expected. These are also per-disease, so groups that model multiple
##' diseases will see multiple rows of expectations.
##' @title Get expectations for a modelling group and touchstone
##' @inheritParams montagu_scenario_status
##' @return A data frame of information about the expectation
##' @export
montagu_expectations <- function(modelling_group_id, touchstone_id,
                                 location = NULL) {

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

  if (length(select) == 0) {
    stop(sprintf("Unknown expectation with id '%s'",
                 as.character(expectation_id)))
  } else {
    exps[[select]]
  }
}

##' The expectations, for a modelling group for a particular touchstone, indicate the
##' range of chronological years, ages and countries for which burden estimates
##' are expected, and what burden outcomes are required.
##' @title Get information about an expectation
##' @inheritParams montagu_scenario_status
##' @param expectation_id id of the expectation (integer)
##' @return list of information about the expectation, including a description and ranges of age and time required.
##' @export
montagu_expectation <- function(modelling_group_id, touchstone_id,
                                expectation_id, location = NULL) {

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

##' @title Get country list for an expectation
##' @inherit montagu_expectation
##' @param expectation_id Integer id of the expectation
##' @return A data frame of country id and name, for all expected countries.
##' @export
montagu_expectation_countries <- function(modelling_group_id, touchstone_id,
                                          expectation_id, location = NULL) {

  countries <- helper_get_expectation(modelling_group_id, touchstone_id,
                                      expectation_id,
                                      location)$expectation$countries

  data_frame(   id = vcapply(countries, function(x) x$id),
              name = vcapply(countries, function(x) x$name))
}

##' @title Get expected outcomes
##' @inherit montagu_expectation
##' @return A vector of outcome names
##' @export
montagu_expectation_outcomes <- function(modelling_group_id, touchstone_id,
                                         expectation_id, location = NULL) {

  helper_get_expectation(modelling_group_id, touchstone_id,
                         expectation_id, location)$expectation$outcomes
}

##' Different scenarios may have different expectations. For example, for
##' Hepatitis B, different countries are required for different scenarios,
##' since the different HepB vaccination initiatives have been carried out in
##' different sets of countries. For other diseases, the same expectation might
##' be equally valid in different scenarios. Here, we can query which scenarios a
##' particular expectation applies to.
##' @importFrom utils read.csv
##' @title Get applicable scenarios for an expectation
##' @inheritParams montagu_expectation
##' @return A vector of scenario names
##' @export
montagu_expectation_applicable_scenarios <- function(modelling_group_id,
                                                     touchstone_id,
                                                     expectation_id,
                                                     location = NULL) {

  helper_get_expectation(modelling_group_id, touchstone_id,
                         expectation_id, location)$applicable_scenarios
}

helper_burden_estimate_template <- function(modelling_group_id,
                                            touchstone_id,
                                            expectation_id,
                                            type, location = NULL) {

  assert_scalar_character(modelling_group_id)
  assert_scalar_character(touchstone_id)
  assert_integer_like(expectation_id)

  path <- sprintf("/modelling-groups/%s/expectations/%s/%s/",
                  modelling_group_id, touchstone_id, expectation_id)

  res <- rawToChar(montagu_api_GET(location, path,
                                   accept = "csv",
                                   query = list(type = type)))

  read.csv(text = res, header = TRUE, stringsAsFactors = FALSE)
}

##' The burden estimate set is the list of burden estimates for a particular
##' scenario for a touchstone. Montagu provides csv templates including rows for
##' all the countries, years and ages, and columns for all the burden outcomes
##' expected. These templates can then be filled in by the modelling groups,
##' and uploaded to Montagu as their results submission.
##' @title Get central burden estimate template for an expectation
##' @inheritParams montagu_expectation
##' @return A data frame with columns disease, year, age, country, and
##'         country_name with given values, then cohort_size, deaths,
##'         cases and dalys, all NA.
##' @export
montagu_central_burden_estimate_template <- function(modelling_group_id,
                                                    touchstone_id,
                                                    expectation_id,
                                                    location = NULL) {

  helper_burden_estimate_template(modelling_group_id, touchstone_id,
                                  expectation_id, "central", location)
}

##' Montagu provides the stochastic burden estimate set, which along with the
##' columns of the central-estimate set, also includes a run_id column.
##' @title Get stochastic burden estimate template for an expectation
##' @inheritParams montagu_expectation
##' @return A data frame with columns disease (given), run_id (NA), year, age,
##'         country, and country_name (all given), and finally
##'         cohort_size, deaths, cases and dalys, (NA).
##' @export
montagu_stochastic_burden_estimate_template <- function(modelling_group_id,
                                                        touchstone_id,
                                                        expectation_id,
                                                        location = NULL) {

  helper_burden_estimate_template(modelling_group_id, touchstone_id,
                                  expectation_id, "stochastic", location)
}
