##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Retrieve list of model run parameter sets for a group and touchstone
##' @param modelling_group_id The id of the modelling group
##' @param touchstone_id The id of the touchstone
##' @param location The montagu server to connect to.
##' @return a data frame of info about the model run parameter sets.
##' @export
montagu_model_run_parameter_sets <- function(modelling_group_id, touchstone_id, 
                                             location = NULL) {
  path <- sprintf("/modelling-groups/%s/model-run-parameters/%s/",
                  modelling_group_id, touchstone_id)
  res <- montagu_api_GET(location, path)
  data_frame(
    id = viapply(res, "[[", "id"),
    model = vcapply(res, "[[", "model"),
    uploaded_by = vcapply(res, "[[", "uploaded_by"),
    uploaded_on = vcapply(res, "[[", "uploaded_on"),
    disease = vcapply(res, "[[", "disease"))
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Retrieve info about one model run parameter sets for a group and touchstone
##' @inheritParams montagu_model_run_parameter_sets
##' @param model_run_parameter_set_id The id of the model_run_parameter_set.
##' @return a list of info about one  model run parameter set.
##' @export
montagu_model_run_parameter_set_info <- function(modelling_group_id, touchstone_id, 
                                model_run_parameter_set_id, location = NULL) {
  path <- sprintf("/modelling-groups/%s/model-run-parameters/%s/",
                  modelling_group_id, touchstone_id)
  res <- montagu_api_GET(location, path)
  df <- data_frame(
    id = viapply(res, "[[", "id"),
    model = vcapply(res, "[[", "model"),
    uploaded_by = vcapply(res, "[[", "uploaded_by"),
    uploaded_on = vcapply(res, "[[", "uploaded_on"),
    disease = vcapply(res, "[[", "disease"))
  
  if (!model_run_parameter_set_id %in% df$id) {
    stop(sprintf("Unknown model_run_parameter_set_id '%s'",
                 model_run_parameter_set_id))
  }
  
  as.list(df[df$id == model_run_parameter_set_id,])
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Retrieve info about one model run parameter sets for a group and touchstone
##' @inheritParams montagu_model_run_parameter_set_info
##' @return a csv of parameter values for each run_id.
##' @export
montagu_model_run_parameter_set_data <- function(modelling_group_id, 
        touchstone_id, model_run_parameter_set_id, location = NULL) {
  
  path <- sprintf("/modelling-groups/%s/model-run-parameters/%s/%s/",
                  modelling_group_id, touchstone_id, model_run_parameter_set_id)
  res <- rawToChar(montagu_api_GET(location, path, accept="csv"))
  read.csv(text = res, header = TRUE, stringsAsFactors = FALSE)
}

##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title Upload a mode_run_parameter_set to Montagu.
##' @inheritParams montagu_model_run_parameter_set_info
##' @param data a data frame with a column `run_id`, and other columns for each parameter that varies by run.
##' @return the id of the newly created model_run_parameter_set
##' @export
montagu_model_run_parameter_set_upload <- function(modelling_group_id, 
                                    touchstone_id, params, location = NULL) {
  
  if (!"run_id" %in% names(params)) {
    stop("run_id not found in data frame")
  }
  
  if (length(names(params))==1) {
    stop("No actual parameters in data frame")
  }
  
  previous_ids <- montagu_model_run_parameter_sets(
    modelling_group_id, touchstone_id, location)$id
  
  
  # Create text of the csv with commas and \n
  
  csv <- paste(
    paste(colnames(params), collapse=","),
    paste(apply(params, 1, paste, collapse=","), collapse="\n"),
    sep="\n")
  
  path <- sprintf("/modelling-groups/%s/model-run-parameters/%s/",
                  modelling_group_id, touchstone_id)
  
  headers <- httr::add_headers("Content-Type" = "text/csv") 
  
  # Doesn't work yet. Error: Error running request:
  # - bad-request: Trying to extract a part from multipart/form-data but 
  #   this request is of type null

  montagu_api_POST(location, path, body = csv,
                     headers = headers)
  
  after_ids <- montagu_model_run_parameter_sets(
    modelling_group_id, touchstone_id, location)$id
  
  diffs <- after_ids[after_ids != previous_ids]
  
  if (length(diffs)==1) {
    # Should test
    return(diffs[1])
    
  } else {
    # Definitely need to test
    return(diffs[1])    
  }
}
