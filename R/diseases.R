##' @title Retrieve list of all disease ids and names.
##' @param location the montagu server to connect to.
##' @export
montagu_diseases <- function(location = NULL) {
  res <- montagu_api_GET(location, "/diseases/")
  data_frame(
    id = vcapply(res, "[[", "id"),
    name = vcapply(res, "[[", "name"))
}

##' @title Retrieve disease with given id.
##' @param disease_id The disease id.
##' @inheritParams montagu_diseases
##' @export
montagu_disease <- function(disease_id, location = NULL) {
  assert_scalar_character(disease_id)
  path <- sprintf("/diseases/%s/", disease_id)
  res <- montagu_api_GET(location, path)
  res[c("id", "name")]
}
