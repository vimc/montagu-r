## An orderly driver
montagu_orderly <- function(name, host, port = 443, basic = FALSE,
                            username = NULL, password = NULL) {
  location <- montagu_server(name, hostname = host, port = port,
                             basic = basic,
                             username = username, password = password)
  R6_montagu_orderly$new(location)
}


R6_montagu_orderly <- R6::R6Class(
  "montagu_orderly",

  public = list(
    location = NULL,

    initialize = function(location) {
      self$location <- location
    },

    ## in orderly we currently use report_names, report_versions
    list = function() {
      montagu_reports_list(self$location)$name
    },

    list_versions = function(name) {
      montagu_reports_report_versions(name, location = self$location)
    },

    pull = function(name, id, root) {
      tmp <- montagu_reports_report_download(name, id, location = self$location)
      on.exit(file.remove(tmp))
      orderly::unzip_archive(tmp, root, name, id)
    },

    push = function(...) {
      stop("'montagu_server' remotes do not support push")
    },

    publish = function(name, id, value = TRUE) {
      montagu_reports_publish(name, id, value, self$location)
    },

    run = function(name, config, parameters = NULL, ref = NULL,
                   timeout = 3600, poll = 1,
                   open = TRUE, stop_on_error = TRUE,
                   progress = TRUE, remote = NULL) {
      montagu_reports_run(name, parameters = parameters, ref = ref,
                          timeout = timeout, poll = poll,
                          open = open, stop_on_error = stop_on_error,
                          progress = progress, location = self$location)
    }
  ))


## This works around a series of failure modes in unpacking an archive
## that tries to minimise the chance that an invalid archive is
## unpacked.
unzip_archive <- function(zip, root, name, id) {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  res <- utils::unzip(zip, exdir = tmp)

  files <- dir(tmp, all.files = TRUE, no.. = TRUE)
  if (length(files) == 0L) {
    stop("Corrupt zip file? No files extracted")
  } else if (length(files) > 1L) {
    stop("Invalid orderly archive", call. = FALSE)
  }
  if (files != id) {
    stop(sprintf("This is archive '%s' but expected '%s'",
                 files, id), call. = FALSE)
  }

  expected <- c("orderly.yml", "orderly_run.yml", "orderly_run.rds")
  msg <- !file.exists(file.path(tmp, id, expected))
  if (any(msg)) {
    stop(sprintf("Invalid orderly archive: missing files %s",
                 paste(expected[msg], collapse = ", ")), call. = FALSE)
  }

  ## R's file.copy is exceedingly rubbish
  dest <- file.path(root, "archive", name)
  dir.create(dest, FALSE, TRUE)
  file_copy(file.path(tmp, id), dest, recursive = TRUE)
}
