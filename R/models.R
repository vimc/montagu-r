##' Models and modelling groups have text ids in Montagu. Here we can look up
##' the relationships between groups and models, with description and citation
##' information.
##' @title Retrieve list of all model ids, names, citations and groups.
##' @param location The montagu server to connect to.
##' @return Data frame containing information about models.
##' @export
montagu_models <- function(location = NULL) {
  res <- montagu_api_GET(location, "/models/")
  data_frame(
    id = vcapply(res, "[[", "id"),
    description = vcapply(res, "[[", "description"),
    citation = vcapply(res, "[[", "citation"),
    modelling_group = vcapply(res, "[[", "modelling_group"))
}

##' Models and modelling groups have text ids in Montagu. Here we can look up
##' the relationships between groups and models, with description and citation
##' information.
##' @title Retrieve model with given id.
##' @param model_id The model id.
##' @inheritParams montagu_models
##' @return List containing information about one model
##' @export
montagu_model <- function(model_id, location = NULL) {
  assert_scalar_character(model_id)
  path <- sprintf("/models/%s/", model_id)
  res <- montagu_api_GET(location, path)
  res[c("id", "description", "citation", "modelling_group")]
}
