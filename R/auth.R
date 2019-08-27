##' Create a handle to a montagu server
##'
##' @title Create a handle to a montagu server
##'
##' @param name The name to call this server.  This appears in a few
##'   places as a "friendly" name.  If \code{global = TRUE} then this
##'   will be available from the package using this name.
##'
##' @param hostname The hostname of the server
##'
##' @param port The port the server is running on (default is 443
##'   which is the standard https port)
##'
##' @param username The username.  If not given, then on
##'   authorisation, the user will be prompted.  Falls back on the R
##'   global options \code{montagu.<name>.username} and
##'   \code{montagu.username} in turn.
##'
##' @param password The password.  If not given, then on
##'   authorisation, the user will be prompted.  Falls back on the R
##'   global options \code{montagu.<name>.password} and
##'   \code{montagu.password} in turn.
##'
##' @param verbose Logical, indicating if verbose http communication
##'   should be used.  This is for debugging only.
##'
##' @param global Logical, indicating if the server should be stored
##'   in the global set.  If true then you can pass \code{location =
##'   name} to most other functions in the package.
##'
##' @param overwrite Logical, and meaningful only if \code{global =
##'   TRUE}, indicating if a global configuration should be replaced
##'   if it exists.
##'
##' @export
##' @return Invisibly, a \code{montagu_server} object.
##' @importFrom R6 R6Class
montagu_server <- function(name, hostname, port = 443,
                           username = NULL, password = NULL,
                           verbose = FALSE, global = TRUE, overwrite = FALSE) {
  if (global && !overwrite && name %in% montagu_server_global_list()) {
    return(global_servers[[name]])
  }
  server <- R6_montagu_server$new(name, hostname, port, username, password,
                                  verbose)
  if (global) {
    global_servers[[name]] <- server
  }
  invisible(server)
}


##' List known montagu servers
##' @title List known montagu servers
##' @export
montagu_server_global_list <- function() {
  names(global_servers)
}


montagu_server_global_clear <- function() {
  rm(list = ls(global_servers, all.names = TRUE),
     envir = global_servers)
}


##' Set a global default montagu server
##'
##' @title Set a global default montagu server
##' @param location A server location
##' @export
montagu_server_global_default_set <- function(location) {
  location <- montagu_location(location)
  global_servers$.default <- location
}


## This is the main *internal* entrypoint
montagu_location <- function(location) {
  if (is.null(location)) {
    location <- global_servers$.default %||%
      stop("No default montagu location has been set")
  } else if (is.character(location)) {
    location <- global_servers[[location]] %||%
      stop(sprintf("Unknown montagu server '%s'", location))
  }
  assert_is(location, "montagu_server")
  location
}


## Then these are the functions to use from code
montagu_api_GET <- function(location, path, ...) {
  montagu_location(location)$request(httr::GET, path, ...)
}


montagu_api_POST <- function(location, path, ...) {
  montagu_location(location)$request(httr::POST, path, ...)
}


R6_montagu_server <- R6::R6Class(
  "montagu_server",
  public = list(
    name = NULL,
    hostname = NULL,
    port = NULL,
    username = NULL,
    password = NULL,
    api_version = 1L,
    opts = NULL,
    url_www = NULL,
    url_api = NULL,
    token = NULL,
    cache = NULL,

    initialize = function(name, hostname, port,
                          username, password, verbose) {
      assert_scalar_character(name)
      assert_scalar_character(hostname)
      assert_scalar_integer(port)
      if (!is.null(username)) {
        assert_scalar_character(username)
      }
      if (!is.null(password)) {
        assert_scalar_character(password)
      }

      self$name <- name
      self$hostname <- hostname
      self$port <- port
      self$username <- username
      self$password <- password

      self$opts <- list(
        verbose = if (verbose) httr::verbose(),
        insecure = if (hostname == "localhost") curl_insecure())
      if (port == 443) {
        self$url_www <- sprintf("https://%s", hostname)
      } else {
        self$url_www <- sprintf("https://%s:%s", hostname, port)
      }

      self$url_api <- sprintf("https://%s:%d/api/v%d",
                              hostname, port, self$api_version)

      self$cache <- montagu_cache(name)
    },

    authorise = function(refresh = FALSE, quiet = FALSE) {
      montagu_server_authorise(self, refresh, quiet)
    },

    is_authorised = function() {
      !is.null(self$token)
    },

    reauthorise = function() {
      self$authorise(TRUE)
    },

    request = function(verb, path, ...) {
      montagu_server_request(self, verb, path, ...)
    },

    reset_cache = function() {
      self$cache <- storr::storr_environment()
    }
  ))


montagu_server_authorise <- function(x, refresh = FALSE, quiet = FALSE) {
  if (!x$is_authorised() || refresh) {
    if (!quiet) {
      message(sprintf("Authorising with server '%s' (%s)", x$name, x$url_www))
    }
    username <- get_credential(x$username, "username", FALSE, x$name)
    password <- get_credential(x$password, "password", TRUE, x$name)

    auth_str <- openssl::base64_encode(sprintf(
      "%s:%s", username, password))
    headers <- httr::add_headers("Authorization" = paste("Basic", auth_str))

    r <- httr::POST(paste0(x$url_api, "/authenticate/"),
                    headers, x$opts$verbose, x$opts$insecure,
                    body = list("grant_type" = "client_credentials"),
                    encode = "form")
    httr::stop_for_status(r)

    token <- response_to_json(r)$access_token
    header <- httr::add_headers("Authorization" = paste("Bearer", token))
    x$token <- list(header = header, value = token)

    ## Retain the username and password in case we reauthorise; only
    ## do this on exit because it's only then we know they're correct
    x$username <- username
    x$password <- password
  }
  invisible(x)
}


get_credential <- function(value, name, secret, location) {
  if (is.null(value)) {
    read <- if (secret) get_pass else read_line
    key <- c(sprintf("montagu.%s.%s", location, name),
             sprintf("montagu.%s", name))
    prompt <- sprintf("Enter montagu %s %s: ", location, name)
    value <- get_option_cascade(key, read(prompt))
  }
  assert_scalar_character(value, name)
  value
}


montagu_server_request <- function(server, verb, path, ...,
                                   accept = "json",
                                   retry_on_auth_error = TRUE,
                                   dest = NULL, progress = TRUE) {
  server$authorise()
  if (!grepl("^/", path)) {
    stop("Expected an absolute path")
  }
  re_version <- "^/v1/"
  if (grepl(re_version, path)) {
    path <- sub(re_version, "/", path)
  }
  url <- paste0(server$url_api, path)

  do_request <- function() {
    verb(url, server$token$header, server$opts$verbose, server$opts$insecure,
         montagu_accept(accept), montagu_dest(dest, accept, progress), ...)
  }
  r <- do_request()

  if (httr::status_code(r) == 401L && retry_on_auth_error) {
    errors <- from_json(httr::content(r, "text", encoding = "UTF-8"))$errors
    if (any(vcapply(errors, function(x) x$code) == "bearer-token-invalid")) {
      server$reauthorise()
      r <- do_request()
    }
  }

  montagu_response(r, accept, dest)
}


montagu_response <- function(r, accept, dest) {
  code <- httr::status_code(r)
  if (code >= 300) {
    if (is_json_response(r)) {
      res <- response_to_json(r)
      if (length(res$errors) == 1L) {
        stop(montagu_api_error(res$errors[[1]]$message, code, res$errors))
      }
    }
    if (code == 404) {
      stop("endpoint or resource not found")
    } else if (code == 403) {
      stop("endpoint or resource not found, or you do not have permission")
    } else {
      stop("montagu returned error code ", code)
    }
  }
  if (accept == "json") {
    txt <- httr::content(r, "text", encoding = "UTF-8")
    ## The error handler here is for responding to nginx gateway
    ## timeouts without checking the headers (because I don't know
    ## what it returns!)
    dat <- withCallingHandlers(
      from_json(txt),
      error = function(e) message("Original response:\n\n", txt))
    dat <- from_json(httr::content(r, "text", encoding = "UTF-8"))
    if (dat$status == "failure") {
      err_code <- vapply(dat$errors, function(x) x$code, character(1))
      err_msg <- vapply(dat$errors, function(x) x$message, character(1))
      err_str <- paste0("Error running request:",
                        sprintf("\n\t - %s: %s", err_code, err_msg))
      stop(montagu_api_error(err_str, code, dat$errors))
    }
    dat$data
  } else if (is.null(dest)) {
    httr::content(r, "raw")
  } else {
    dest
  }
}


montagu_accept <- function(accept) {
  switch(accept,
         json = httr::accept_json(),
         binary = httr::accept("application/octet-stream"),
         zip = httr::accept("application/zip"),
         csv = httr::accept("text/csv"),
         stop("unknown type ", accept))
}


montagu_dest <- function(dest, accept, progress) {
  if (is.null(dest)) {
    NULL
  } else if (accept == "json") {
    stop("'dest' not valid with accept = \"json\"")
  } else {
    c(httr::write_disk(dest), if (progress) httr::progress())
  }
}


montagu_api_error <- function(msg, code, errors) {
  err <- list(message = msg, errors = errors, code = code)
  class(err) <- c("montagu_api_error", "error", "condition")
  err
}
