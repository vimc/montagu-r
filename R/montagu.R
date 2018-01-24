montagu_burden_estimates <- function(modelling_group_id, touchstone_id,
                                     scenario_id) {
  url <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/",
                 modelling_group_id, touchstone_id, scenario_id)
  res <- montagu_GET(url)
}

montagu_burden_estimate_set_create <- function(modelling_group_id,
                                               touchstone_id, scenario_id,
                                               type,
                                               model_run_parameter_set,
                                               details = NULL) {
  url <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/",
                 modelling_group_id, touchstone_id, scenario_id)
  data <- list(
    type = list(type = jsonlite::unbox(type)),
    model_run_parameter_set = jsonlite::unbox(model_run_parameter_set))
  if (!is.null(details)) {
    data$type$details <- jsonlite::unbox(details)
  }
  res <- montagu_POST(url, body = data, encode = "json")
  as.integer(sub("/", "", basename(res)))
}

montagu_burden_estimate_set_upload <- function(modelling_group_id,
                                               touchstone_id,
                                               scenario_id,
                                               burden_estimate_set_id,
                                               filename,
                                               keep_open = NULL) {
  url <- sprintf(
    "/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/",
    modelling_group_id, touchstone_id, scenario_id, burden_estimate_set_id)
  if (isTRUE(keep_open)) {
    query <- list(keepOpen = "true")
  } else {
    query <- NULL
  }
  montagu_POST(url, body = httr::upload_file(filename), query = query)
}

montagu_burden_estimate_set_clear <- function(modelling_group_id,
                                              touchstone_id,
                                              scenario_id,
                                              burden_estimate_set_id) {
  url <- sprintf(
    "/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/actions/clear/",
    modelling_group_id, touchstone_id, scenario_id, burden_estimate_set_id)
  res <- montagu_POST(url)
}
