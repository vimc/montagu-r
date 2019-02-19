##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title The Montagu Reporting API
##' @param location A montagu location
##' @rdname montagu_reports
##' @name montagu_reports
NULL

##' @export
##' @rdname montagu_reports
montagu_reports_list <- function(location = NULL) {
  res <- montagu_reports_GET(location, "/reports/")
  if (length(res) == 0L) {
    name <- latest_version <- display_name <- character(0)
  } else {
    name <- vcapply(res, "[[", "name")
    display_name <- vcapply(res, function(x) x$display_name %||% NA_character_)
    latest_version <- vcapply(res, "[[", "latest_version")
  }
  data.frame(name = name,
             display_name = display_name,
             latest_version = latest_version,
             stringsAsFactors = FALSE)
}


##' @export
##' @rdname montagu_reports
##' @param name A report name
##'
##' @param error_if_missing Throw an error if the report is missing -
##'   this is often not desired.
montagu_reports_report_versions <- function(name, error_if_missing = TRUE,
                                            location = NULL) {
  allow_missing <- !error_if_missing
  res <- tryCatch(
    montagu_reports_GET(location, sprintf("/reports/%s/", name)),
    error = function(e) e)

  if (inherits(res, "character")) {
    return(res)
  } else if (allow_missing &&
             inherits(res, "montagu_api_error") &&
             length(res$errors) == 1L &&
             res$errors[[1]]$code == "unknown-report") {
    return(character(0))
  } else {
    stop(res)
  }
}


##' @export
##' @rdname montagu_reports
##' @param version The report version identifier (YYYYMMDD-HHMMSS-xxxxxxxx)
montagu_reports_report_metadata <- function(name, version, location = NULL) {
  montagu_reports_GET(location,
                      sprintf("/reports/%s/versions/%s/", name, version))
}


##' @export
##' @rdname montagu_reports
##' @param dest Destination to download to
##' @param progress Print a progress bar
montagu_reports_report_download <- function(name, version, dest = tempfile(),
                                            progress = TRUE, location = NULL) {
  ret <- montagu_reports_GET(
    location,
    sprintf("/reports/%s/versions/%s/all/", name, version),
    accept = "zip", dest = dest, progress = progress)
  if (progress) {
    cat("\n") # httr's progress bar is rubbish
  }
  ret
}


##' @export
##' @rdname montagu_reports
montagu_reports_report_artefact_list <- function(name, version,
                                                 location = NULL) {
  res <- montagu_reports_GET(
    location,
    sprintf("/reports/%s/versions/%s/artefacts/", name, version))
  list_to_character(res)
}


##' @export
##' @rdname montagu_reports
##' @param filename Name of artefact to download
montagu_reports_report_artefact_get <- function(name, version, filename,
                                                dest = NULL,
                                                progress = TRUE,
                                                location = NULL) {
  filename_enc <- encode_path(filename)
  path <- sprintf("/reports/%s/versions/%s/artefacts/%s/",
                  name, version, filename_enc)
  montagu_reports_GET(location, path,
                      accept = "binary", dest = dest, progress = progress)
}


##' @export
##' @rdname montagu_reports
montagu_reports_report_resource_list <- function(name, version,
                                                 location = NULL) {
  res <- montagu_reports_GET(
    location,
    sprintf("/reports/%s/versions/%s/resources/", name, version))
  list_to_character(res)
}


##' @export
##' @rdname montagu_reports
montagu_reports_report_resource_get <- function(name, version, filename,
                                                dest = NULL, progress = TRUE,
                                                location = NULL) {
  filename_enc <- encode_path(filename)
  path <- sprintf("/reports/%s/versions/%s/resources/%s/",
                  name, version, filename_enc)
  montagu_reports_GET(location, path,
                      accept = "binary", dest = dest, progress = progress)
}


##' @export
##' @rdname montagu_reports
montagu_reports_report_data_list <- function(name, version, location = NULL) {
  res <- montagu_reports_GET(
    location,
    sprintf("/reports/%s/versions/%s/data/", name, version))
  list_to_character(res)
}


##' @export
##' @rdname montagu_reports
##' @param hash Hash of data to get
##' @param csv Logical, indicating if csv (rather than rds) should be returned
montagu_reports_report_data_get <- function(name, version, hash,
                                            dest = NULL, progress = TRUE,
                                            csv = FALSE, location = NULL) {
  path <- sprintf("/reports/%s/versions/%s/data/%s", name, version, hash)
  if (csv) {
    type <- "csv"
    accept <- "binary"
  } else {
    type <- "rds"
    accept <- "binary"
  }
  montagu_reports_GET(location, path, query = list(type = type),
                      accept = accept, dest = dest, progress = progress)
}


##' @export
##' @rdname montagu_reports
## NOT IMPLEMENTED
montagu_reports_report_sessioninfo <- function(name, version, location = NULL) {
  ## This is not implemented in the api
  dest <- tempfile()
  on.exit(file.remove(dest))
  montagu_reports_GET(
    location,
    sprintf("/reports/%s/versions/%s/sessioninfo", name, version),
    accept = "binary", dest = dest, progress = FALSE)
  readRDS(dest)
}


##' @export
##' @rdname montagu_reports
montagu_reports_data <- function(hash, csv = FALSE, dest = NULL,
                                 progress = TRUE, location = NULL) {
  if (csv) {
    type <- "csv"
    accept <- "csv"
  } else {
    type <- "rds"
    accept <- "binary"
  }
  path <- sprintf("/data/%s/%s", type, hash)
  montagu_reports_GET(location, path,
                      accept = accept, dest = dest, progress = progress)
}


##' @export
##' @rdname montagu_reports
##' @param parameters List of parameters to run report with
##' @param ref Git reference
##' @param update I can't remember?
##'
##' @param timeout Time to give up on running report; this is set on
##'   the \emph{server}.  After \code{timeout} seconds, the server
##'   will kill the running report.
##'
##' @param poll Time to poll for update
##'
##' @param open Open the report in a browser on completion?
##'
##' @param stop_on_error Throw an error if the report fails?
##'
##' @param stop_on_timeout Throw an error if report is not completed in time?
##'
##' @param output Show output from running the report.  This is a work
##'   in progress.  This has an effect only when \code{progress} is
##'   \code{TRUE}.
##'
##' @param wait Integer value indicating time to wait on the client
##'   side for the report to run.  If \code{0} (or less) we do not
##'   wait.  If you provide a timeout you should make wait at least
##'   the timeout value.
montagu_reports_run <- function(name, parameters = NULL, ref = NULL,
                                update = TRUE,
                                timeout = NULL, wait = Inf, poll = 0.5,
                                open = FALSE,
                                stop_on_error = FALSE,
                                stop_on_timeout = TRUE,
                                progress = TRUE,
                                location = NULL, output = TRUE) {
  location <- montagu_location(location)
  if (!is.null(parameters)) {
    stop("parameters not yet supported")
  }

  query <- list()
  if (!is.null(ref)) {
    query$ref <- ref
  }
  if (!update) {
    query$update <- "false"
  }
  if (!is.null(timeout)) {
    assert_scalar_integer(timeout)
    query$timeout <- as.character(timeout)
  }
  if (length(query) == 0L) {
    query <- NULL
  }

  res <- montagu_reports_POST(location, sprintf("/reports/%s/run/", name),
                              query = query)
  res$location <- location
  class(res) <- "montagu_report_instance"

  if (wait > 0) {
    montagu_reports_wait(res, timeout = wait, poll = poll,
                         open = open,
                         stop_on_error = stop_on_error,
                         stop_on_timeout = stop_on_timeout,
                         progress = progress, output = output)
  } else {
    res
  }
}


##' @export
##' @rdname montagu_reports
##'
##' @param obj A \code{montagu_reports_instance} object, as created by
##'   \code{montagu_reports_run}.
montagu_reports_wait <- function(obj, timeout = 3600, poll = 0.5,
                                 open = FALSE, stop_on_error = FALSE,
                                 stop_on_timeout = TRUE,
                                 progress = TRUE,
                                 output = TRUE,
                                 wait = TRUE) {
  assert_is(obj, "montagu_report_instance")
  path <- obj$path
  name <- obj$name
  key <- obj$key
  location <- obj$location

  t_start <- Sys.time()
  t_stop <- t_start + timeout
  message(sprintf("running report '%s' as '%s'", name, key))
  fmt <- sprintf("[:spin] (%s) :elapsed :status", key)
  prev_output <- list(stderr = NULL, stdout = NULL)

  if (progress) {
    p <- progress::progress_bar$new(fmt, ceiling(timeout / poll * 1.1),
                                    show_after = 0)
    tick <- function(state, status, output) {
      new <- function(now, prev) {
        if (length(prev) == 0) {
          now
        } else {
          now[-seq_along(prev)]
        }
      }
      if (state %in% c("running", "error") && !is.null(output)) {
        new_output <- Map(new, output, prev_output)
        if (any(lengths(new_output)) > 0L) {
          clear_progress_bar(p)
          message(format_output(new_output), appendLF = FALSE)
        }
        prev_output <<- output
      }
      p$tick(tokens = list(status = status))
    }
  } else {
    tick <- function(state, status, output) {}
  }

  state <- "submitted"

  poll_query <- list(output = progress && output)
  w <- getOption("width", 80L)

  repeat {
    ans <- montagu_reports_GET(location, path, query = poll_query)

    state <- ans$status
    if (state == "queued" && output) {
      queue <- matrix(unlist(strsplit(ans$output$stdout, ":", fixed = TRUE)),
                      length(ans$output$stdout), byrow = TRUE)
      status <- trim_string(sprintf("queued (%d): %s", nrow(queue),
                                    paste(queue[, 3], collapse = " < ")),
                            w - 12L)
    } else {
      status <- sprintf("%s: %s", state, ans$version %||% "???")
    }
    tick(state, status, ans$output)

    if (state %in% c("queued", "running")) {
      Sys.sleep(if (state == "queued") max(poll, 1) else poll)
    } else {
      break
    }
    if (Sys.time() > t_stop) {
      if (stop_on_timeout) {
        stop("timeout reached")
      } else {
        message("timeout reached")
        return(obj)
      }
    }
  }

  ## Ensure we clear the screen:
  if (progress) {
    message()
  }

  if (stop_on_error && state %in% c("error", "killed")) {
    ## TODO: It would be super nice to get the full stack trace back
    ## here from orderly on error.  That should not be hard to do.
    if (!progress || !output) {
      ans <- montagu_reports_GET(location, path,
                                 query = list(output = TRUE))
      cat(format_output(ans$output))
    }
    if (state == "error") {
      stop("Report has failed: see above for details")
    } else {
      stop(sprintf("job killed by remote server after %d secs",
                   round(as.numeric(Sys.time() - t_start, "secs"))))
    }
  }

  ans <- montagu_reports_GET(location, path, query = list(output = TRUE))
  if (state == "success") {
    url <- sprintf("%s/reports/%s/%s", location$url_www, name, ans$version)
    if (open) {
      message("Opening report in browser (you may need to log in)")
      utils::browseURL(url)
    }
  } else {
    url <- NULL
  }
  list(name = name,
       id = ans$version,
       status = ans$status,
       output = ans$output,
       url = url)
}


##' @export
##' @rdname montagu_reports
##' @param key Key to a running report (adjective_animal)
montagu_reports_status <- function(key, output = FALSE, location = NULL) {
  path <- sprintf("/reports/%s/status/", key)
  query <- if (output) list(output = TRUE) else NULL
  montagu_reports_GET(location, path, query = query)
}


##' @export
##' @rdname montagu_reports
##' @param value Value to publish report as (boolean)
montagu_reports_publish <- function(name, version, value = NULL, location = NULL) {
  if (is.null(value)) {
    query <- NULL
  } else {
    assert_scalar_logical(value)
    query <- list(value = value)
  }
  montagu_reports_POST(location,
                       sprintf("/reports/%s/versions/%s/publish/", name, version),
                       query = query)
}


##' @export
##' @rdname montagu_reports
montagu_reports_rebuild <- function(location = NULL) {
  montagu_reports_POST(location, "/reports/rebuild/")
}


##' @export
##' @rdname montagu_reports
montagu_reports_git_status <- function(location = NULL) {
  montagu_reports_GET(location, "/reports/git/status/")
}


## TODO: these two need a bit of work:
## 1. errors don't come back correctly (i.e., not as an array): VIMC-809
## 2. git is not working in the container because of auth> VIMC-810


##' @export
##' @rdname montagu_reports
montagu_reports_git_pull <- function(location = NULL) {
  montagu_reports_POST(location, "/reports/git/pull/")
}


##' @export
##' @rdname montagu_reports
montagu_reports_git_fetch <- function(location = NULL) {
  montagu_reports_POST(location, "/reports/git/fetch/")
}
