##' Burden estimate sets define a set of results for a modelling group. They
##' are specific to a touchstone and scenario. Usually, they will be the
##' populated version of the burden estimate set template, which a modelling
##' group can download, and defines the columns and rows for all the
##' countries, ages and years that are expected from that group, for that
##' scenario. The modelling group then overwrites the missing values with
##' results from their model, and submits the results to Montagu.
##' @title Burden Estimate Sets
##' @param location A montagu location
##' @name Burden Estimate Sets
NULL

##' @export
##' @title Retrieves list of estimate sets for a group, touchstone and scenario.
##' @param modelling_group_id Modelling group identifier
##' @param touchstone_id Touchstone identifier
##' @param scenario_id Scenario identifier
##' @param location The montagu server to connect to.
##' @return A data frame of information about all relevant estimate sets.
montagu_burden_estimate_sets <- function(modelling_group_id, touchstone_id,
                                     scenario_id, location = NULL) {
  assert_character(modelling_group_id)
  assert_character(touchstone_id)
  assert_character(scenario_id)

  path <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/",
                  modelling_group_id, touchstone_id, scenario_id)
  res <- montagu_api_GET(location, path)
  df <- data_frame(id = viapply(res, "[[", "id"),
             uploaded_on = vcapply(res, "[[", "uploaded_on"),
             uploaded_by = vcapply(res, "[[", "uploaded_by"),
             type = vcapply(res, function(x) x$type$type),
             details = vcapply(res, function(x) {
                 z <- x$type$details
                 if (is.null(z)) z <- ""
                 z
               }),
             status = vcapply(res, "[[", "status"))
  df[order(df$id),]
}

##' @export
##' @title Retrieves information about a specific burden estimate set.
##' @inherit montagu_burden_estimate_sets
##' @return A list of information about a specific estimate set.
montagu_burden_estimate_set_info <- function(modelling_group_id, touchstone_id,
              scenario_id, burden_estimate_set_id, location = NULL) {

  assert_character(modelling_group_id)
  assert_character(touchstone_id)
  assert_character(scenario_id)
  assert_integer_like(burden_estimate_set_id)

  path <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/",
        modelling_group_id, touchstone_id, scenario_id, burden_estimate_set_id)
  res <- montagu_api_GET(location, path)
  typeinfo <- res$type
  c(res[c("id", "uploaded_on", "uploaded_by")],
    typeinfo[c("type", "details")],
    res["status"])

}

##' @export
##' @title Retrieves the data for a specific burden estimate set.
##' @inherit montagu_burden_estimate_sets
##' @return A list of information about a specific estimate set.
montagu_burden_estimate_set_data <- function(modelling_group_id, touchstone_id,
                      scenario_id, burden_estimate_set_id, location = NULL) {

  assert_character(modelling_group_id)
  assert_character(touchstone_id)
  assert_character(scenario_id)
  assert_integer_like(burden_estimate_set_id)

  path <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/estimates/",
        modelling_group_id, touchstone_id, scenario_id, burden_estimate_set_id)
  res <- rawToChar(montagu_api_GET(location, path, accept="csv"))
  read.csv(text = res, header = TRUE, stringsAsFactors = FALSE)

}

##' @export
##' @title Retrieve data for a particular outcoe of a burden estimate set,
##' aggregated across country and disaggregated by either age or year.
##' @inherit montagu_burden_estimate_sets
##' @param outcome_code The name of an outcome, such as 'cases' or 'deaths'.
##' @param group_by Set to 'age' (the default) or 'year', to set the
##' @return A data frame with columns age or year (depending on group_by),
montagu_burden_estimate_set_outcome_data <- function(modelling_group_id,
                                                     touchstone_id,
                                                     scenario_id,
                                                     burden_estimate_set_id,
                                                     outcome_code,
                                                     group_by = 'age',
                                                     location = NULL) {

  assert_character(modelling_group_id)
  assert_character(touchstone_id)
  assert_character(scenario_id)
  assert_integer_like(burden_estimate_set_id)
  assert_character(outcome_code)

  if (!group_by %in% c("age", "year")) {
    stop("group_by must be set to 'age' or 'year'")
  }

  path <- sprintf(
    "/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/estimates/%s/",
    modelling_group_id, touchstone_id, scenario_id, burden_estimate_set_id,
    outcome_code)

  query <- list()
  if (group_by!='age') query <- list(groupBy = group_by)

  res <- montagu_api_GET(location, path, query = query)

  df <- data_frame(index = rep(as.integer(names(res)), each = length(res[[1]])),
        x = unlist(lapply(res, function(x) { viapply(x, function(z) { z$x })})),
        y = unlist(lapply(res, function(x) { vnapply(x, function(z) { z$y })})))
  if (group_by == 'age') {
    names(df)[names(df)=='index'] <- 'age'
  } else {
    names(df)[names(df)=='index'] <- 'year'
  }
  df
}

##' @export
##' @inherit montagu_burden_estimate_sets
##' @param burden_estimate_set_id Burden estimate set identifier
##' @param modelling_group_id Modelling group identifier
##' @param touchstone_id Touchstone identifier
##' @param scenario_id Scenario identifier
##' @param location The montagu server to connect to.
##' @return A list of any problems with this burden estimate set
montagu_burden_estimate_set_problems <- function(modelling_group_id,
    touchstone_id, scenario_id, burden_estimate_set_id, location = NULL) {

  assert_character(modelling_group_id)
  assert_character(touchstone_id)
  assert_character(scenario_id)
  assert_integer_like(burden_estimate_set_id)

  path <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/",
                  modelling_group_id, touchstone_id, scenario_id,
                  burden_estimate_set_id)
  res <- montagu_api_GET(location, path)
  res$problems
}

##' @export
##' @title Create a new burden estimate set
##' @inherit montagu_burden_estimate_sets
##' @param type Can be `central-single-run`, `central-averaged`
##' or `stochastic`
##' @param model_run_parameter_set Identifier for the parameter set
##' @param details Optional details string
##' @return The id of the burden estimate set
montagu_burden_estimate_set_create <- function(modelling_group_id,
                                               touchstone_id, scenario_id,
                                               type,
                                               model_run_parameter_set = NULL,
                                               details = NULL,
                                               location = NULL) {
  assert_character(modelling_group_id)
  assert_character(touchstone_id)
  assert_character(scenario_id)
  assert_character(type)
  if (!is.null(model_run_parameter_set)) {
    assert_integer_like(model_run_parameter_set)
  }

  # I am not sure if I should allow the r-client to upload
  # with type "central-unknown"

  if (!type %in% c("central-single-run", "stochastic",
                   "central-averaged")) {
    stop(paste0("Invalid type - must be one of central-single-run, ",
               "central-averaged, or stochastic"))
  }

  if (type != "stochastic") {
    if (!is.null(model_run_parameter_set)) {
      stop("model_run_parameter_set should only be specified for stochastic runs")
    }
  }

  if (type == "stochastic") {
    if (is.null(model_run_parameter_set)) {
      stop("model_run_parameter_set must be specified for stochastic runs")
    }
  }

  path <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/",
                  modelling_group_id, touchstone_id, scenario_id)
  data <- list(
    type = list(type = jsonlite::unbox(type)),
    model_run_parameter_set = jsonlite::unbox(model_run_parameter_set))
  if (!is.null(details)) {
    data$type$details <- jsonlite::unbox(details)
  }

  res <- montagu_api_POST(location, path, body = data, encode = "json")
  as.integer(sub("/", "", basename(res)))
}

##' @export
##' @title Deletes all uploaded rows from an incomplete burden estimate set
##' @inherit montagu_burden_estimate_set_create
##' @param burden_estimate_set_id Burden estimate set created by
##'   \code{montagu_burden_estimate_set_crete}
montagu_burden_estimate_set_clear <- function(modelling_group_id,
                                              touchstone_id,
                                              scenario_id,
                                              burden_estimate_set_id,
                                              location = NULL) {
  assert_character(modelling_group_id)
  assert_character(touchstone_id)
  assert_character(scenario_id)
  assert_integer_like(burden_estimate_set_id)

  path <- sprintf(
    "/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/actions/clear/",
    modelling_group_id, touchstone_id, scenario_id, burden_estimate_set_id)
  montagu_api_POST(location, path)
}

##' @export
##' @title Closes a burden estimate set, marking it as complete.
##' @inherit montagu_burden_estimate_set_clear
##' @param burden_estimate_set_id Burden estimate set created by
##'   \code{montagu_burden_estimate_set_create}
montagu_burden_estimate_set_close <- function(modelling_group_id,
                                              touchstone_id,
                                              scenario_id,
                                              burden_estimate_set_id,
                                              location = NULL) {
  assert_character(modelling_group_id)
  assert_character(touchstone_id)
  assert_character(scenario_id)
  assert_integer_like(burden_estimate_set_id)

  path <- sprintf(
    "/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/actions/close/",
    modelling_group_id, touchstone_id, scenario_id, burden_estimate_set_id)
  montagu_api_POST(location, path)
}

##' @export
##' @inherit montagu_burden_estimate_set_clear
##' @param data Data frame containing burden estimates.
##' @param lines Number of lines to chunk the files into
##' @param keep_open Keep the burden estimate set open after upload?
montagu_burden_estimate_set_upload <- function(modelling_group_id,
                                               touchstone_id,
                                               scenario_id,
                                               burden_estimate_set_id,
                                               data,
                                               lines = 10000L,
                                               keep_open = FALSE,
                                               location = NULL) {
  for (col in c("disease", "year", "age", "country", "country_name",
                "cohort_size")) {

    if (!col %in% names(data)) {
      stop(sprintf("'%s' column not found in data"))
    }
  }

  tf <- tempfile()
  write.csv(x = data, file = tf, row.names = FALSE)

  path <- sprintf(
    "/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/",
    modelling_group_id, touchstone_id, scenario_id, burden_estimate_set_id)

  is_complete <- function(complete) {
    if (keep_open || !complete) {
      list(keepOpen = "true")
    }
  }

  if (lines == Inf) {
    montagu_api_POST(location, path, body = httr::upload_file(tf),
                     query = is_complete(TRUE))
    return(invisible())
  }

  ## This is not the most efficient way possible but because every
  ## upload needs to have a header row there's not a lot of
  ## alternatives, really - we have to build up a string each time.
  ## And it looks like we can't just send the whole thing up in one go

  con <- file(tf, "r")
  on.exit(close(con))
  header <- readLines(con, n = 1L)
  reader <- read_chunked(con, lines)
  headers <- httr::add_headers("Content-Type" = "text/csv")

  p <- progress::progress_bar$new("[:spin] chunk :current", total = 100000)
  message(sprintf("Uploading in %d line chunks:\n%s",
                  lines, tf))
  t0 <- Sys.time()
  repeat {
    d <- reader()
    body <- paste0(c(header, d$data), "\n", collapse = "")
    p$tick()
    montagu_api_POST(location, path, body = body,
                     query = is_complete(d$complete), headers = headers)
    if (d$complete) {
      break
    }
  }
  message(sprintf("...Done! (in %s)", format(Sys.time() - t0)))
}

