##' An \code{orderly} remote driver for montagu
##'
##' @title orderly remote driver
##' @param name Name for the remote
##' @param host Hostname
##' @param port Port
##' @param basic Logical, indicating if this remote uses basic authentication
##' @param username Username
##' @param password Password
##' @export
montagu_orderly_remote <- function(name, host, port = 443, basic = FALSE,
                            username = NULL, password = NULL) {
  location <- montagu_server(name, hostname = host, port = port,
                             basic = basic,
                             username = username, password = password)
  R6_montagu_orderly_remote$new(location)
}


R6_montagu_orderly_remote <- R6::R6Class(
  "montagu_orderly_remote",

  public = list(
    location = NULL,

    initialize = function(location) {
      self$location <- location
    },

    ## in orderly we currently use report_names, report_versions
    list_reports = function() {
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

    run = function(name, parameters = NULL, ref = NULL,
                   timeout = NULL, wait = 1000, poll = 1, progress = TRUE,
                   stop_on_error = TRUE, open = FALSE) {
      montagu_reports_run(name, parameters = parameters, ref = ref,
                          timeout = timeout, poll = poll, progress = progress,
                          stop_on_error = stop_on_error, open = open,
                          location = self$location)
    }
  ))
