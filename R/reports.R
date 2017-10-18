## This all needs to move into montagu.  To do that we need to split
## out the auth and request processing code to get us to the point
## where we have the GET function working.  For now, this is not a big
## deal.

## /v1/reports/
montagu_reports_list <- function(location = NULL) {
  res <- montagu_GET("/reports/", reports = TRUE, location = location)
  empty_default(res, character(0))
}

## /v1/reports/:name/
montagu_reports_report_versions <- function(name, location = NULL) {
  res <- montagu_GET(sprintf("/reports/%s/", name),
                     reports = TRUE, location = location)
  empty_default(res, character(0))
}

## /v1/reports/:name/:version/
montagu_reports_report_metadata <- function(name, version, location = NULL) {
  montagu_GET(sprintf("/reports/%s/%s/", name, version),
              reports = TRUE, location = location)
}

## /v1/reports/:name/:version/all/
montagu_reports_report_download <- function(name, version, dest = tempfile(),
                                            progress = TRUE, location = NULL) {
  ret <- montagu_GET(sprintf("/reports/%s/%s/all/", name, version),
                     reports = TRUE, location = location,
                     accept = "zip", dest = dest, progress = progress)
  if (progress) {
    cat("\n") # httr's progress bar is rubbish
  }
  ret
}

## /v1/reports/:name/:version/artefacts/
montagu_reports_report_artefact_list <- function(name, version,
                                                 location = NULL) {
  res <- montagu_GET(sprintf("/reports/%s/%s/artefacts/", name, version),
                     reports = TRUE, location = location)
  list_to_character(res)
}

## /v1/reports/:name/:version/artefacts/:artefact/
montagu_reports_report_artefact_get <- function(name, version, filename,
                                                dest = NULL,
                                                progress = TRUE,
                                                location = NULL) {
  filename_enc <- encode_path(filename)
  path <- sprintf("/reports/%s/%s/artefacts/%s/", name, version, filename_enc)
  montagu_GET(path, reports = TRUE, location = location,
              accept = "binary", dest = dest, progress = progress)
}

## /v1/reports/:name/:version/resources/
montagu_reports_report_resource_list <- function(name, version,
                                                 location = NULL) {
  res <- montagu_GET(sprintf("/reports/%s/%s/resources/", name, version),
                     reports = TRUE, location = location)
  list_to_character(res)
}

## /v1/reports/:name/:version/resources/:resource/
montagu_reports_report_resource_get <- function(name, version, filename,
                                                dest = NULL, progress = TRUE,
                                                location = NULL) {
  filename_enc <- encode_path(filename)
  path <- sprintf("/reports/%s/%s/resources/%s/", name, version, filename_enc)
  montagu_GET(path, reports = TRUE, location = location,
              accept = "binary", dest = dest, progress = progress)
}

## /v1/reports/:name/:version/data/
montagu_reports_report_data_list <- function(name, version, location = NULL) {
  res <- montagu_GET(sprintf("/reports/%s/%s/data/", name, version),
                     reports = TRUE, location = location)
  list_to_character(res)
}

## /v1/reports/:name/:version/data/:data/
montagu_reports_report_data_get <- function(name, version, hash,
                                            dest = NULL, progress = TRUE,
                                            csv = FALSE, location = NULL) {
  path <- sprintf("/reports/%s/%s/data/%s", name, version, hash)
  if (csv) {
    type <- "csv"
    accept <- "binary"
  } else {
    type <- "rds"
    accept <- "binary"
  }
  montagu_GET(path, query = list(type = type),
              reports = TRUE, location = location,
              accept = accept, dest = dest, progress = progress)
}

## NOT IMPLEMENTED
montagu_reports_report_sessioninfo <- function(name, version, location = NULL) {
  ## This is not implemented in the api
  dest <- tempfile()
  on.exit(file.remove(dest))
  montagu_GET(sprintf("/reports/%s/%s/sessioninfo", name, version),
              reports = TRUE, location = location,
              accept = "binary", dest = dest, progress = FALSE)
  readRDS(dest)
}

## /v1/data/csv/:id/
## /v1/data/rds/:id/
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
  montagu_GET(path, reports = TRUE, location = location,
              accept = accept, dest = dest, progress = progress)
}

montagu_reports_run <- function(name, parameters = NULL, ref = NULL,
                                update = TRUE,
                                timeout = 3600, poll = 0.5,
                                open = FALSE, stop_on_error = FALSE,
                                progress = TRUE,
                                location = NULL) {
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
  if (length(query) == 0L) {
    query <- NULL
  }
  res <- montagu_POST(sprintf("/reports/%s/run/", name), query = query,
                      reports = TRUE, location = location)
  t_stop <- Sys.time() + timeout
  path <- res$path
  key <- res$key
  fmt <- sprintf("[:spin] (%s) :elapsed :state", res$key)
  if (progress) {
    p <- progress::progress_bar$new(fmt, ceiling(timeout / poll * 1.1),
                                    show_after = 0)
    tick <- function(state) {
      p$tick(tokens = list(state = state))
    }
  } else {
    tick <- function(state) {}
  }

  state <- "submitted"

  repeat {
    ans <- montagu_GET(res$path, reports = TRUE, location = location)

    if (state != ans$status) {
      state <- ans$status
    }
    if (state %in% c("queued", "running")) {
      tick(sprintf("%s: %s", state, ans$version %||% "???"))
      Sys.sleep(poll)
    } else {
      break
    }
    if (Sys.time() > t_stop) {
      stop("timeout reached")
    }
  }

  if (progress) {
    message()
  }

  if (stop_on_error && state == "error") {
    stop("Report failed")
  }

  ans <- montagu_GET(res$path, query = list(output = TRUE),
                     reports = TRUE, location = location)
  url <- sprintf("%s/reports/%s/%s",
                 montagu$hosts[[location]]$url_www, name, ans$version)
  if (open) {
    message("Opening report in browser (you may need to log in)")
    browseURL(url)
  }
  list(name = name,
       id = ans$version,
       status = ans$status,
       output = ans$output,
       url = url)
}

montagu_reports_status <- function(key, location = NULL) {
  montagu_GET(sprintf("/reports/%s/status/", key),
              reports = TRUE, location = location)
}

montagu_reports_publish <- function(name, id, value = NULL, location = NULL) {
  if (is.null(value)) {
    query <- NULL
  } else {
    assert_scalar_logical(value)
    query <- list(value = value)
  }
  montagu_POST(sprintf("/reports/%s/%s/publish/", name, id), query = query,
               reports = TRUE, location = location)
}

montagu_reports_rebuild <- function(location = NULL) {
  montagu_POST("/reports/rebuild/")
}

montagu_reports_git_status <- function(location = NULL) {
  montagu_GET("/reports/git/status/", reports = TRUE, location = location)
}

## TODO: these need a bit of work:
## 1. errors don't come back correctly (i.e., not as an array): VIMC-809
## 2. git is not working in the container because of auth> VIMC-810
montagu_reports_git_pull <- function(location = NULL) {
  montagu_POST("/reports/git/pull/", reports = TRUE, location = location)
}

montagu_reports_git_fetch <- function(location = NULL) {
  montagu_POST("/reports/git/fetch/", reports = TRUE, location = location)
}
