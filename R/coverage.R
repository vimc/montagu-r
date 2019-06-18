##' The coverage data describes for each country, over time and age, the
##' efforts to vaccinate people against each disease. Routine activities involve
##' the regular vaccination that is in place, whereas campaigns are specifically
##' targeted towards a certain disease or people group for a certain time period.
##' @title Retrieve metadata for coverage sets for a particularly group, touchstone and scenario.
##' @param modelling_group id of modelling group
##' @param touchstone_id id of touchstone (including version)
##' @param scenario_id id of scenario within touchstone
##' @param location The montagu server to connect to.
##' @return A data frame of metadata about all associated coverage sets
##' @export
montagu_coverage_info <- function(modelling_group, touchstone_id, scenario_id,
                                  location = NULL) {
  assert_character(modelling_group)
  assert_character(touchstone_id)
  assert_character(scenario_id)

  path <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/coverage-sets/",
                  modelling_group, touchstone_id, scenario_id)
  res <- montagu_api_GET(location, path)

  data_frame(
    id = viapply(res$coverage_sets, "[[", "id"),
    touchstone_version = vcapply(res$coverage_sets, "[[", "touchstone_version"),
    name = vcapply(res$coverage_sets, "[[", "name"),
    vaccine = vcapply(res$coverage_sets, "[[", "vaccine"),
    gavi_support = vcapply(res$coverage_sets, "[[", "gavi_support"),
    activity_type = vcapply(res$coverage_sets, "[[", "activity_type")
  )
}

##' The coverage data describes for each country, over time and age, the
##' efforts to vaccinate people against each disease. Routine activities involve
##' the regular vaccination that is in place, whereas campaigns are specifically
##' targeted towards a certain disease or people group for a certain time period.
##' @title Retrieve coverage data for a scenario, touchstone and modelling group.
##' @inheritParams montagu_coverage_info
##' @param modelling_group id of modelling group; if omitted or NULL, then return coverage info for 
##'                        all modelling groups.
##' @param format "wide" or "long" csv format. Long format contains a row per 
##'                year per country; wide format contains a row per country, 
##'                with year-specific columns for target population
##'                and coverage.
##' @param all_countries Include all countries, instead of just those
##'        for which burden estimates are required.
##' @return A data frame of the coverage data.
##' @export
montagu_coverage_data <- function(modelling_group = NULL, touchstone_id,
                                  scenario_id, format = "long",
                                  all_countries = FALSE, location = NULL) {
  if (!is.null(modelling_group)) assert_scalar_character(modelling_group)
  assert_scalar_character(touchstone_id)
  assert_scalar_character(scenario_id)
  assert_scalar_logical(all_countries)
  assert_scalar_character(format)

  if (!format %in% c("long", "wide")) {
    stop(sprintf("Unrecognised format '%s'\n", format))
  }

  if (!is.null(modelling_group)) {
    path <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/coverage/",
                    modelling_group, touchstone_id, scenario_id)
  } else {
    path <- sprintf("/touchstones/%s/%s/coverage/", touchstone_id, scenario_id)
  }

  query <- list()
  query$format <- format
  query$'all-countries' <- all_countries

  res <- rawToChar(montagu_api_GET(location, path, accept = "csv",
                                   query = query))
  
  read.csv(text = res, header = TRUE, stringsAsFactors = FALSE)
}
