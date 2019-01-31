##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Retrieve metadata for coverage sets associated with a responsibility
##' @param modelling_group Id of modelling group
##' @param touchstone_id Id of touchstone
##' @param scenario_id Id of scenario within touchstone
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

##' @title Retrieve coverage data
##' @param modelling_group Id of modelling group
##' @param touchstone_id Id of touchstone
##' @param scenario_id Id of scenario within touchstone
##' @param format "wide" or "long" csv format.
##' @param all_countries Include all countries, instead of just those
##'        for which burden estimates are required.
##' @param location The montagu server to connect to.
##' @return A data frame of metadata about all associated coverage sets 
##' @export
montagu_coverage_data <- function(modelling_group, touchstone_id, 
                                  scenario_id, format = "long",
                                  all_countries = FALSE, location = NULL) {
  assert_character(modelling_group)
  assert_character(touchstone_id)
  assert_character(scenario_id)
  assert_logical(all_countries)
  assert_character(format)
  
  path <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/coverage/",
                  modelling_group, touchstone_id, scenario_id)
  
  if (all_countries) {
    path <- paste0(path,"?all-countries=true")
  }
  
  if (!is.null(format)) {
    if (!all_countries) {
      path <- paste0(path, "?")
    } else {
      path <- paste0(path, "&")
    }
    path <- paste0(path, sprintf("format=%s", format))
  }

  res <- rawToChar(montagu_api_GET(location, path, accept="csv"))
  read.csv(text = res, header = TRUE, stringsAsFactors = FALSE)
}