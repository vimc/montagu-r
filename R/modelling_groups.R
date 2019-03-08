##' Modelling group ids generally consist of an identifier for the
##' institution where the group is based, a hyphen, and the principal
##' investigor for that group. Groups will be informed of their internal 
##' id when joining the consortium, but for convenience, we provide
##' functions to look up the modelling group id, and associated models.
##' @title Retrieve list of all modelling group ids and names.
##' @param location The montagu server to connect to.
##' @return A dataframe of id and description for each group
##' @export
montagu_modelling_groups <- function(location = NULL) {

  res <- montagu_api_GET(location, "/modelling-groups/")
  data_frame(
    id = vcapply(res, "[[", "id"),
    description = vcapply(res, "[[", "description"))
}

##' Modelling group ids generally consist of an identifier for the
##' institution where the group is based, a hyphen, and the principal
##' investigor for that group. Groups will be informed of their internal 
##' id when joining the consortium, but for convenience, we provide
##' functions to look up the modelling group id, and associated models.
##' @title Retrieve modelling_group with given modelling_group_id.
##' @param modelling_group_id The id of the modelling group
##' @inheritParams montagu_modelling_groups
##' @return A list of id and description for the given group id.
##' @export

montagu_modelling_group <- function(modelling_group_id, 
                                    location = NULL) {
  assert_scalar_character(modelling_group_id)
  path <- sprintf("/modelling-groups/%s/", modelling_group_id)
  res <- montagu_api_GET(location, path)
  res[c("id", "description")]
}

##' Modelling group ids generally consist of an identifier for the
##' institution where the group is based, a hyphen, and the principal
##' investigor for that group. Groups will be informed of their internal 
##' id when joining the consortium, but for convenience, we provide
##' functions to look up the modelling group id, and associated models.
##' @title Retrieve list of models for a given modelling_group_id.
##' @inheritParams montagu_modelling_group
##' @export

montagu_model_list <- function(modelling_group_id, location = NULL) {
  assert_scalar_character(modelling_group_id)
  path <- sprintf("/modelling-groups/%s/", modelling_group_id)
  res <- montagu_api_GET(location, path)
  models <- res$models
  data_frame(
    id = vcapply(models, "[[", "id"),
    description = vcapply(models, "[[", "description"),
    citation = vcapply(models, "[[", "citation"),
    modelling_group = vcapply(models, "[[", "modelling_group"))
}

##' @title Retrieve list of members for a given modelling_group_id.
##' @inheritParams montagu_modelling_group
##' @export
montagu_modelling_group_members <- function(modelling_group_id, 
                                            location = NULL) {
  assert_scalar_character(modelling_group_id)
  path <- sprintf("/modelling-groups/%s/", modelling_group_id)
  res <- montagu_api_GET(location, path)
  res$members
}
