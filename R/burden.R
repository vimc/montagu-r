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
  
  
  # path <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/",
  #      modelling_group_id, touchstone_id, scenario_id, burden_estimate_set_id)
  # res <- montagu_api_GET(location, path)
  
  # The endpoint for the above is currently not working 
  # see https://vimc.myjetbrains.com/youtrack/issue/VIMC-2600
  # So below code does the selection in R from the full list and mimics a
  # likely error message for invalid ids.
  
  path <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/",
                  modelling_group_id, touchstone_id, scenario_id)
  res <- montagu_api_GET(location, path)
  
  df <- data_frame(id = viapply(res, "[[", "id"),
             uploaded_on = vcapply(res, "[[", "uploaded_on"),
             uploaded_by = vcapply(res, "[[", "uploaded_by"),
             type = vcapply(res, function(x) x$type$type),
             details = vcapply(res, function(x) x$type$details),
             status = vcapply(res, "[[", "status"))
  
  if (!burden_estimate_set_id %in% df$id) {
    stop(sprintf("Unknown burden estimate set with id '%d'",
                  burden_estimate_set_id))
  }
  
  as.list(df[df$id == burden_estimate_set_id, ])
}

##' @export
##' @rdname montagu_api
##' @title Retrieves the data for a specific burden estimate set.
##' @inheritParams montagu_burden_estimate_sets
##' @return A list of information about a specific estimate set.
montagu_burden_estimate_set_data <- function(modelling_group_id, touchstone_id,
                      scenario_id, burden_estimate_set_id, location = NULL) {
  
  
  path <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/",
        modelling_group_id, touchstone_id, scenario_id, burden_estimate_set_id)
  res <- rawToChar(montagu_api_GET(location, path, accept="csv"))
  read.csv(text = res, header = TRUE, stringsAsFactors = FALSE)
  
  # The endpoint for the above is currently not working 
  # see https://vimc.myjetbrains.com/youtrack/issue/VIMC-2632
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
  
  # Could simplify this when 
  # see https://vimc.myjetbrains.com/youtrack/issue/VIMC-2600 is done
  
  path <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/",
                  modelling_group_id, touchstone_id, scenario_id)
  res <- montagu_api_GET(location, path)
  ids <- viapply(res, "[[", "id")
  if (! burden_estimate_set_id %in% ids) {
    stop(sprintf("Unknown burden estimate set with id '%d'",
                  burden_estimate_set_id))
  }
  probs <- lapply(res, "[[", "problems")
  probs[[which(burden_estimate_set_id == ids)]]
}

##' @export
##' @rdname montagu_api
##' @param type Either "stochastic" or "central" (I think)
##' @param model_run_parameter_set Identifier for the parameter set
##' @param details Optional details string
montagu_burden_estimate_set_create <- function(modelling_group_id,
                                               touchstone_id, scenario_id,
                                               type,
                                               model_run_parameter_set,
                                               details = NULL,
                                               location = NULL) {
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
##' @rdname montagu_api
##'
##' @param burden_estimate_set_id Burden estimate set created by
##'   \code{montagu_burden_estimate_set_crete}
##'
##' @param filename Filename to upload
##'
##' @param lines Number of lines to chunk the files into
##'
##' @param keep_open Keep the burden estimate set open after upload?
montagu_burden_estimate_set_upload <- function(modelling_group_id,
                                               touchstone_id,
                                               scenario_id,
                                               burden_estimate_set_id,
                                               filename,
                                               lines = 10000L,
                                               keep_open = FALSE,
                                               location = NULL) {
  path <- sprintf(
    "/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/",
    modelling_group_id, touchstone_id, scenario_id, burden_estimate_set_id)

  is_complete <- function(complete) {
    if (keep_open || !complete) {
      list(keepOpen = "true")
    }
  }

  if (lines == Inf) {
    montagu_api_POST(location, path, body = httr::upload_file(filename),
                     query = is_complete(TRUE))
    return(invisible())
  }

  ## This is not the most efficient way possible but because every
  ## upload needs to have a header row there's not a lot of
  ## alternatives, really - we have to build up a string each time.
  ## And it looks like we can't just send the whole thing up in one go
  con <- file(filename, "r")
  on.exit(close(con))
  header <- readLines(con, n = 1L)
  reader <- read_chunked(con, lines)
  headers <- httr::add_headers("Content-Type" = "text/csv")

  p <- progress::progress_bar$new("[:spin] chunk :current", total = 100000)
  message(sprintf("Uploading in %d line chunks:\n%s",
                  lines, filename))
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
##' @rdname montagu_api
montagu_touchstones_list <- function(location = NULL) {
  res <- montagu_api_GET(location, "/touchstones/")
  v <- lapply(res, "[[", "versions")
  vv <- unlist(v, FALSE)

  n <- lengths(v)
  i <- rep(seq_along(n), n)

  data_frame(id = vcapply(vv, "[[", "id"),
             name = vcapply(res, "[[", "id")[i],
             version = viapply(vv, "[[", "version"),
             status = vcapply(vv, "[[", "status"))

}
