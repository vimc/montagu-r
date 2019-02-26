##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title The Montagu API
##' @param location A montagu location
##' @rdname montagu_api
##' @name montagu_api
NULL

##' @export
##' @rdname montagu_api
##' @title Retrieves list of estimate sets for a group, touchstone and scenario.
##' @param modelling_group_id Modelling group identifier
##' @param touchstone_id Touchstone identifier
##' @param scenario_id Scenario identifier
##' @param location The montagu server to connect to.
##' @return A data frame of information about all relevant estimate sets.
montagu_burden_estimate_sets <- function(modelling_group_id, touchstone_id,
                                     scenario_id, location = NULL) {
  path <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/",
                  modelling_group_id, touchstone_id, scenario_id)
  res <- montagu_api_GET(location, path)
  data_frame(id = viapply(res, "[[", "id"),
             uploaded_on = vcapply(res, "[[", "uploaded_on"),
             uploaded_by = vcapply(res, "[[", "uploaded_by"),
             type = vcapply(res, function(x) x$type$type),
             details = vcapply(res, function(x) x$type$details),
             status = vcapply(res, "[[", "status"))
}

##' @export
##' @rdname montagu_api
##' @title Retrieves information about a specific burden estimate set.
##' @inheritParams montagu_burden_estimate_sets
##' @return A list of information about a specific estimate set.
montagu_burden_estimate_set_info <- function(modelling_group_id, touchstone_id,
              scenario_id, burden_estimate_set_id, location = NULL) {
  
  
  path <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/",
        modelling_group_id, touchstone_id, scenario_id, burden_estimate_set_id)
  res <- montagu_api_GET(location, path)
  typeinfo <- res$type
  c(res[c("id", "uploaded_on", "uploaded_by")],
    typeinfo[c("type", "details")],
    res["status"])
  
}

##' @export
##' @rdname montagu_api
##' @title Retrieves the data for a specific burden estimate set.
##' @inheritParams montagu_burden_estimate_sets
##' @return A list of information about a specific estimate set.
montagu_burden_estimate_set_data <- function(modelling_group_id, touchstone_id,
                      scenario_id, burden_estimate_set_id, location = NULL) {
  
  
  path <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/estimates",
        modelling_group_id, touchstone_id, scenario_id, burden_estimate_set_id)
  res <- rawToChar(montagu_api_GET(location, path, accept="csv"))
  read.csv(text = res, header = TRUE, stringsAsFactors = FALSE)
  
}

##' @export
##' @rdname montagu_api
##' @inheritParams montagu_burden_estimates
##' @param burden_estimate_set_id Burden estimate set identifier
##' @param modelling_group_id Modelling group identifier
##' @param touchstone_id Touchstone identifier
##' @param scenario_id Scenario identifier
##' @param location The montagu server to connect to.
##' @return A list of any problems with this burden estimate set
montagu_burden_estimate_set_problems <- function(modelling_group_id, 
    touchstone_id, scenario_id, burden_estimate_set_id, location = NULL) {
  
  path <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/",
                  modelling_group_id, touchstone_id, scenario_id, 
                  burden_estimate_set_id)
  res <- montagu_api_GET(location, path)
  res$problems
}

##' @export
##' @title Create a new burden estimate set
##' @rdname montagu_api
##' @inheritParams montagu_burden_estimates
##' @param type Can be `central-single-run`, `central-averaged`, `central-unknown` or `stochastic`
##' @param model_run_parameter_set Identifier for the parameter set
##' @param details Optional details string
##' @return The id of the burden estimate set
montagu_burden_estimate_set_create <- function(modelling_group_id,
                                               touchstone_id, scenario_id,
                                               type,
                                               model_run_parameter_set,
                                               details = NULL,
                                               location = NULL) {
  assert_character(modelling_group_id)
  assert_character(touchstone_id)
  assert_character(scenario_id)
  assert_character(type)
  assert_integer_like(model_run_parameter_set)
  
  # I am not sure if e should allow the r-client to upload 
  # with type "central-unknown" - or how far we want to go down the 
  # stochastic route here, since I guess we are not going to allow full
  # unprocessed stochastic uploads through the API?
  
  if (!type %in% c("central-single-run", "stochastic", 
                   "central-averaged", "central-unknown")) {
    stop(paste0("Invalid type - must be one of central-single-run, stochastic,",
               " central-averaged, or central-unknown")) 
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
##' @rdname montagu_api
##' @inheritParams montagu_burden_estimate_set_create
##' @param burden_estimate_set_id Burden estimate set created by
##'   \code{montagu_burden_estimate_set_crete}
montagu_burden_estimate_set_clear <- function(modelling_group_id,
                                              touchstone_id,
                                              scenario_id,
                                              burden_estimate_set_id,
                                              location = NULL) {
  path <- sprintf(
    "/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/actions/clear/",
    modelling_group_id, touchstone_id, scenario_id, burden_estimate_set_id)
  montagu_api_POST(location, path)
}

##' @export
##' @title Closes a burden estimate set, marking it as complete.
##' @rdname montagu_api
##' @inheritParams montagu_burden_estimate_set_clear
##' @param burden_estimate_set_id Burden estimate set created by
##'   \code{montagu_burden_estimate_set_crete}
montagu_burden_estimate_set_close <- function(modelling_group_id,
                                              touchstone_id,
                                              scenario_id,
                                              burden_estimate_set_id,
                                              location = NULL) {
  path <- sprintf(
    "/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/actions/close/",
    modelling_group_id, touchstone_id, scenario_id, burden_estimate_set_id)
  montagu_api_POST(location, path)
}


##' @export
##' @rdname montagu_api
##' @inheritParams montagu_burden_estimate_clear
##' @param data Data frame containing burden estimates.
##' @param lines Number of lines to chunk the files into
##'
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

##' @export
##' @rdname montagu_api
montagu_touchstones_list <- function(location = NULL) {
  res <- montagu_api_GET(location, "/")
  v <- lapply(res, "[[", "versions")
  vv <- unlist(v, FALSE)

  n <- lengths(v)
  i <- rep(seq_along(n), n)

  data_frame(id = vcapply(vv, "[[", "id"),
             name = vcapply(res, "[[", "id")[i],
             version = viapply(vv, "[[", "version"),
             status = vcapply(vv, "[[", "status"))

}
