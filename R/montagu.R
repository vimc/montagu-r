##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title The Montagu API
##' @param location A montagu location
##' @rdname montagu_api
##' @name montagu_api
NULL

##' @export
##' @rdname montagu_api
##' @param modelling_group_id Modelling group identifier
##' @param touchstone_id Touchstone identifier
##' @param scenario_id Scenario identifier
montagu_burden_estimates <- function(modelling_group_id, touchstone_id,
                                     scenario_id, location = NULL) {
  path <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/",
                  modelling_group_id, touchstone_id, scenario_id)
  res <- montagu_api_GET(location, path)
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
