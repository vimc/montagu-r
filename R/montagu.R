montagu_burden_estimates <- function(modelling_group_id, touchstone_id,
                                     scenario_id) {
  url <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/",
                 modelling_group_id, touchstone_id, scenario_id)
  res <- montagu_GET(url)
}

montagu_burden_estimate_set_create <- function(modelling_group_id,
                                               touchstone_id, scenario_id,
                                               type,
                                               model_run_parameter_set,
                                               details = NULL) {
  url <- sprintf("/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/",
                 modelling_group_id, touchstone_id, scenario_id)
  data <- list(
    type = list(type = jsonlite::unbox(type)),
    model_run_parameter_set = jsonlite::unbox(model_run_parameter_set))
  if (!is.null(details)) {
    data$type$details <- jsonlite::unbox(details)
  }
  res <- montagu_POST(url, body = data, encode = "json")
  as.integer(sub("/", "", basename(res)))
}

montagu_burden_estimate_set_upload <- function(modelling_group_id,
                                               touchstone_id,
                                               scenario_id,
                                               burden_estimate_set_id,
                                               filename,
                                               lines = 10000L,
                                               keep_open = FALSE) {
  url <- sprintf(
    "/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/",
    modelling_group_id, touchstone_id, scenario_id, burden_estimate_set_id)

  query <- function(complete) {
    if (keep_open || !complete) {
      list(keepOpen = "true")
    }
  }

  if (lines == Inf) {
    montagu_POST(url, body = httr::upload_file(filename),
                 query = query(TRUE))
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
  repeat {
    d <- reader()
    body <- paste0(c(header, d$data), "\n", collapse = "")
    p$tick()
    montagu_POST(url, body = body, query = query(d$complete), headers = headers)
    if (d$complete) {
      break
    }
  }
  message("...Done!")
}

montagu_burden_estimate_set_clear <- function(modelling_group_id,
                                              touchstone_id,
                                              scenario_id,
                                              burden_estimate_set_id) {
  url <- sprintf(
    "/modelling-groups/%s/responsibilities/%s/%s/estimate-sets/%s/actions/clear/",
    modelling_group_id, touchstone_id, scenario_id, burden_estimate_set_id)
  res <- montagu_POST(url)
}
