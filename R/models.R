##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Retrieve list of all model ids, names, citations and groups.
##' @param location The montagu server to connect to.
##' @export
montagu_models_list <- function(location = NULL) {
  res <- montagu_api_GET(location, "/models/")
  data_frame(
    id = vcapply(res, "[[", "id"),
    name = vcapply(res, "[[", "description"),
    citation = vcapply(res, "[[", "citation"),
    modelling_group = vcapply(res, "[[", "modelling_group"))
}

##' @title Retrieve model with given id.
##' @param model_id The disease id.
##' @inheritParams montagu_models_list
##' @export
montagu_model_by_id <- function(model_id, location = NULL) {
  assert_scalar_character(model_id)
  path <- sprintf("/models/%s/", model_id)
  montagu_api_GET(location, path)
}
