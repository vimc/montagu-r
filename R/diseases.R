##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Retrieve list of all disease ids and names.
##' @param location The montagu server to connect to.
##' @export
montagu_disease_list <- function(location = NULL) {
  res <- montagu_api_GET(location, "/diseases/")
  data_frame(
    id = vcapply(res, "[[", "id"),
    name = vcapply(res, "[[", "name"))
}

##' @title Retrieve disease with given id.
##' @param disease_id The disease id.
##' @inheritParams montagu_disease_list
##' @export
montagu_disease_by_id <- function(disease_id, location = NULL) {
  assert_scalar_character(disease_id)
  path <- sprintf("/diseases/%s/", disease_id)
  res <- montagu_api_GET(location, path)
  data_frame(
    id = res$id, 
    name = res$name)
}
