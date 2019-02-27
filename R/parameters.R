##' A model run parameter set is a list of parameters that is required
##' for a set of stochastic model runs. In a stochastic ensemble, modelling
##' groups are requested to perform a number of model runs. Each model run
##' executes with a unique set of parameters, and for a given run, all scenarios
##' are run with the same parameters, so that we can calculate impact between
##' them. The spread of parameters across the different runs should capture
##' the range of sensible behaviour of the model. The model run parameter set then
##' contains as many rows as there are model runs. Each row must contain a run_id,
##' and the value for each parameter that is varied.
##' 
##' Adding, and querying existing model_run_parameter_sets is supported, and
##' when creating a stochastic burden estimate set, the id of the associated
##' model_run_parameter_set is required as a parameter.
##' 
##' @title Retrieve a list of model run parameter sets for a group and touchstone
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

##' @title Retrieve info about one model run parameter sets for a group and touchstone
##' @inherit montagu_model_run_parameter_sets
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

##' @title Retrieve info about one model run parameter sets for a group and touchstone
##' @inherit montagu_model_run_parameter_set_info
##' @return a csv of parameter values for each run_id.
##' @export
montagu_model_run_parameter_set_data <- function(modelling_group_id,
        touchstone_id, model_run_parameter_set_id, location = NULL) {

  path <- sprintf("/modelling-groups/%s/model-run-parameters/%s/%s/",
                  modelling_group_id, touchstone_id, model_run_parameter_set_id)
  res <- rawToChar(montagu_api_GET(location, path, accept="csv"))
  read.csv(text = res, header = TRUE, stringsAsFactors = FALSE)
}

##' @importFrom utils write.csv
##' @title Upload a model_run_parameter_set to Montagu.
##' @inherit montagu_model_run_parameter_set_info
##' @param data a data frame with a column `run_id`, and other columns for each 
##' parameter that will be varied for each run.
##' @param disease_id The id of the disease associated with this model
##' @return the id of the newly created model_run_parameter_set
##' @export
montagu_model_run_parameter_set_upload <- function(modelling_group_id,
                touchstone_id, disease_id, data, location = NULL) {

  if (!"run_id" %in% names(data)) {
    stop("run_id not found in data frame")
  }

  if (length(names(data))==1) {
    stop("No actual parameters in data frame")
  }

  # Move run_id to left-most column, so we can quote it, as per the spec.

  if (names(data)[1] != "run_id") {
    the_names <- c("run_id",names(data)[names(data)!="run_id"])
    data <- params[the_names]
  }

  data$run_id <- as.character(data$run_id)

  tf <- tempfile()
  write.csv(x = data, quote = 1, file = tf, row.names = FALSE)

  # Create text of the csv with commas and \n

  path <- sprintf("/modelling-groups/%s/model-run-parameters/%s/",
                  modelling_group_id, touchstone_id)

  res <- montagu_api_POST(location, path,
    body = list(disease = disease_id, file = httr::upload_file(tf, type="text/csv"),
    encode = c("multipart")))

  bits <- unlist(strsplit(res, "/"))
  bits <- bits[length(bits)]
  as.numeric(bits)
}
