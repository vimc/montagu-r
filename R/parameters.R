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
##' @param disease_id The id of the disease associated with this model
##' @return the id of the newly created model_run_parameter_set
##' @export
montagu_model_run_parameter_set_upload <- function(modelling_group_id,
                touchstone_id, disease_id, params, location = NULL) {

  if (!"run_id" %in% names(params)) {
    stop("run_id not found in data frame")
  }

  if (length(names(params))==1) {
    stop("No actual parameters in data frame")
  }

  # Move run_id to left-most column, so we can quote it, as per the spec.

  if (names(params)[1] != "run_id") {
    the_names <- c("run_id",names(params)[names(params)!="run_id"])
    params <- params[the_names]
  }

  params$run_id <- as.character(params$run_id)

  tf <- tempfile()
  write.csv(x = params, quote = 1, file = tf, row.names = FALSE)

  # Create text of the csv with commas and \n

  path <- sprintf("/modelling-groups/%s/model-run-parameters/%s/",
                  modelling_group_id, touchstone_id)

  res <- montagu_api_POST(location, path,
    body = list(disease = disease_id, file = httr::upload_file(tf, type="text/csv"),
    encode = c("multipart", "form", "text", "csv")))

  # This appears to work - I am seeing one warning:
  # Warning message:
  # In charToRaw(enc2utf8(val)) :
  #  argument should be a character vector of length 1
  #  all but the first element will be ignored

  # res contains the URL of the new parameter set endpoint, eg.
  # "https://server/api/v1/modelling-groups/groupid/model-run-parameters/id/
  # We want to return the last id.

  bits <- unlist(strsplit(res, "/"))
  bits <- bits[length(bits)]
  as.numeric(bits)
}
