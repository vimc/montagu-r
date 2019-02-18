##' Diseases have an internal id and a name in Montagu, which are generally,
##' but not always the same. Occasionally, it is necessary to provide the
##' disease id to Montagu functions.
##' @title Retrieve list of all disease ids and names.
##' @param location the montagu server to connect to.
##' @return data frame containing ids and names of diseases
##' @export
montagu_diseases <- function(location = NULL) {
  res <- montagu_api_GET(location, "/diseases/")
  data_frame(
    id = vcapply(res, "[[", "id"),
    name = vcapply(res, "[[", "name"))
}

##' Diseases have an internal id and a name in Montagu, which are generally,
##' but not always the same. Occasionally, it is necessary to provide the
##' disease id to Montagu functions.
##' @title Retrieve disease with given id.
##' @param disease_id The disease id.
##' @inheritParams montagu_diseases
##' @return a list of an id and a disease name
##' @export
montagu_disease <- function(disease_id, location = NULL) {
  assert_scalar_character(disease_id)
  path <- sprintf("/diseases/%s/", disease_id)
  res <- montagu_api_GET(location, path)
  as.list(res[c("id", "name")])
}
