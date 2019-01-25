##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Retrieve list of all modelling group ids and names.
##' @param location The montagu server to connect to.
##' @export
montagu_modelling_group_list <- function(location = NULL) {
  res <- montagu_api_GET(location, "/modelling-groups/")
  data_frame(
    id = vcapply(res, "[[", "id"),
    description = vcapply(res, "[[", "description"))
}

##' @title Retrieve modelling_group with given modelling_group_id.
##' @param modelling_group_id The id of the modelling group
##' @inheritParams montagu_modelling_group_list
##' @export

montagu_modelling_group_by_id <- function(modelling_group_id, 
                                          location = NULL) {
  assert_scalar_character(modelling_group_id)
  path <- sprintf("/modelling-groups/%s/", modelling_group_id)
  res <- montagu_api_GET(location, path)
  res[c("id", "description")]
}

##' @title Retrieve list of models for a given modelling_group_id.
##' @inheritParams montagu_modelling_group_by_id
##' @export

montagu_model_list_by_modelling_group_id <- function(modelling_group_id, 
                                                     location = NULL) {
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
##' @inheritParams montagu_modelling_group_by_id
##' @export
montagu_members_by_modelling_group_id <- function(modelling_group_id, 
                                                  location = NULL) {
  assert_scalar_character(modelling_group_id)
  path <- sprintf("/modelling-groups/%s/", modelling_group_id)
  res <- montagu_api_GET(location, path)
  res$members
}

montagu_edit_user_in_modelling_group <- function(modelling_group_id,
                                                 username,
                                                 action,
                                                 location = NULL) {
  path <- sprintf("/modelling-groups/%s/actions/associate-member/", 
                  modelling_group_id)
  data <- list(
    action = "add",
    username = username)
  
  montagu_api_POST(location, path, body = data, encode = "json")
}

##' @title Add user to a modelling group
##' @inheritParams montagu_modelling_group_by_id
##' @param username User name of the user to add
##' @export
montagu_add_user_to_modelling_group <- function(modelling_group_id,
                                                username,
                                                location = NULL) {
  montagu_edit_user_in_modelling_group(modelling_group_id, 
                                       username, "add", location)
}

##' @title Remove user from a modelling group
##' @inheritParams montagu_modelling_group_by_id
##' @param username User name of the user to remove
##' @export
montagu_remove_user_from_modelling_group <- function(modelling_group_id,
                                                username,
                                                location = NULL) {
  montagu_edit_user_in_modelling_group(modelling_group_id, 
                                       username, "remove", location)
}

##' @title Add a new modelling group
##' @param modelling_group_id Id for new group, typicallly in form Institution-LeadModeller.
##' @param modelling_group_description A text description of the group
##' @param modelling_group_institution Name of institution
##' @param modelling_group_pi The principal investigator for the group.
##' @inheritParams montagu_modelling_group_by_id
##' @export
montagu_add_modelling_group <- function(modelling_group_id,
                                        modelling_group_description,
                                        modelling_group_institution,
                                        modelling_group_pi,
                                        location = NULL) {
  path <- sprintf("/modelling-groups/", 
                  modelling_group_id)
  data <- list(
    id = modelling_group_id,
    description = modelling_group_description,
    institution = modelling_group_institution,
    pi = modelling_group_pi)
  
  montagu_api_POST(location, path, body = data, encode = "json")
}
