##' An \code{orderlyweb} remote driver for montagu
##'
##' @title orderly remote driver
##'
##' @param name Name for the remote, shared with the
##'   \code{\link{montagu_server}} name.
##'
##' @param host Hostname, shared with the
##'   \code{\link{montagu_server}} host.
##'
##' @param port Port, shared with the
##'   \code{\link{montagu_server}} port.
##'
##' @param ... Additional arguments passed through to
##'   \code{\link{montagu_server}}
##' @export
montagu_orderlyweb_remote <- function(name, host, port, ...) {
  montagu <- montagu_server(name, host, port, ...)
  token <- montagu_orderlyweb_token(montagu)
  orderlyweb_remote(host, port, token, name = name, prefix = "reports")
}


montagu_orderlyweb_token <- function(montagu) {
  force(montagu)
  function() {
    montagu$authorise(quiet = TRUE)
    montagu$token$value
  }
}
