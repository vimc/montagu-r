## Montagu options:
montagu <- new.env(parent = emptyenv())

montagu_add_location <- function(name, hostname, port,
                                 basic = FALSE,
                                 verbose = FALSE,
                                 overwrite = FALSE) {
  if (name %in% names(montagu$hosts) && !overwrite) {
    return()
  }
  assert_scalar_character(hostname)
  assert_scalar_integer(port)
  assert_scalar_logical(basic)

  api_version <- 1L

  opts <- if (verbose) httr::verbose() else NULL
  if (hostname == "localhost") {
    opts <- c(curl_insecure(), opts)
  }

  if (port == 443) {
    url_www <- sprintf("https://%s", hostname)
  } else {
    url_www <- sprintf("https://%s:%s", hostname, port)
  }

  montagu$hosts[[name]] <- list(
    hostname = hostname,
    port = port,
    basic = basic,
    opts = opts,
    api_version = api_version,
    url = sprintf("https://%s:%d/api/v%d",
                  hostname, port, api_version),
    url_reports = sprintf("https://%s:%d/reports/api/v%d",
                          hostname, port, api_version),
    url_www = url_www)
}

## These might move into the orderly_config.yml for montagu-reports
montagu_add_location_defaults <- function() {
  montagu$hosts <- list()
  montagu_add_location("production", "montagu.vaccineimpact.org", 443L)
  montagu_add_location("science", "support.montagu.dide.ic.ac.uk", 11443L)
  montagu_add_location("uat", "support.montagu.dide.ic.ac.uk", 10443L)
  montagu_set_default_location("science")
}

montagu_credentials <- function(username, password, location) {
  username <- get_input(username, "username", FALSE, location)
  password <- get_input(password, "password", TRUE, location)
  list(username = username, password = password)
}

montagu_authorise <- function(username = NULL, password = NULL,
                              location = NULL, refresh = FALSE) {
  location <- montagu_location(location)
  dat <- montagu$hosts[[location]]
  if (is.null(dat)) {
    stop(sprintf("Unknown location '%s'", location))
  }
  if (is.null(dat$token) || refresh) {
    message(sprintf("Authorising with server '%s' (https://%s:%s)",
                    location, dat$hostname, dat$port))
    auth <- montagu_credentials(username, password, location)

    if (dat$basic) {
      basic_auth <- httr::authenticate(auth$username, auth$password)
      r <- httr::GET(dat$url_reports, basic_auth)
      httr::stop_for_status(r)
      dat$token <- basic_auth
    } else {
      auth_str <- openssl::base64_encode(sprintf(
        "%s:%s", auth$username, auth$password))
      headers <- httr::add_headers("Authorization" = paste("Basic", auth_str))

      r <- httr::POST(paste0(dat$url, "/authenticate/"),
                      headers, dat$opts,
                      body = list("grant_type" = "client_credentials"),
                      encode = "form")
      httr::stop_for_status(r)
      t <- from_json(httr::content(r, "text", encoding = "UTF-8"))
      dat$token <- httr::add_headers(
        "Authorization" = paste("Bearer", t$access_token))
    }
    ## Retain the username and password in case we reauthorise
    dat$username <- username
    dat$password <- password
    montagu$hosts[[location]] <- dat
  }
  invisible(dat)
}

montagu_reauthorise <- function(location) {
  location <- montagu_location(location)
  dat <- montagu$hosts[[location]]
  if (is.null(dat$token)) {
    stop(sprintf("Have not previously authorised with '%s'", location))
  }
  montagu_authorise(dat$username, dat$password, location, TRUE)
}

montagu_set_default_location <- function(location) {
  prev <- montagu$default
  if (!(location %in% names(montagu$hosts))) {
    stop(sprintf("Unknown montagu location '%s' - must be one of %s",
                 location, paste(names(montagu$hosts), collapse = ", ")))
  }
  montagu$default <- location
  invisible(prev)
}

## this is going to change several times potentially.
montagu_location <- function(location = NULL) {
  structure(location %||% montagu$default,
            class = c("montagu_server",
                      "orderly_api_server",
                      "orderly_remote_location"))
}

montagu_GET <- function(...) {
  montagu_request(httr::GET, ...)
}

montagu_POST <- function(...) {
  montagu_request(httr::POST, ...)
}

montagu_request <- function(verb, path, ..., location = NULL,
                            accept = "json", dest = NULL, progress = TRUE,
                            reports = FALSE, montagu = NULL,
                            retry_on_auth_error = TRUE) {
  location <- montagu_location(location)
  dat <- montagu_authorise(location = location)
  base <- if (reports) dat$url_reports else dat$url
  if (!grepl("^/", path)) {
    stop("Expected an absolute path")
  }
  re_version <- "^/v1/"
  if (grepl(re_version, path)) {
    path <- sub(re_version, "/", path)
  }
  url <- paste0(base, path)

  request <- function() {
    verb(url, dat$token, dat$opts, montagu_accept(accept),
         montagu_dest(dest, accept, progress), ...)
  }
  r <- request()

  if (httr::status_code(r) == 401L && retry_on_auth_error) {
    errors <- from_json(httr::content(r, "text", encoding = "UTF-8"))$errors
    if (length(errors) == 1L && errors[[1]]$code == "bearer-token-invalid") {
      dat <- montagu_reauthorise(location = location)
      r <- request()
    }
  }

  montagu_response(r, accept, dest)
}

montagu_response <- function(r, accept, dest) {
  if (httr::status_code(r) == 404) {
    ## Not sure about 403
    stop("endpoint or resource not found")
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
      ## TODO: make this an S3 error
      err_code <- vapply(dat$errors, function(x) x$code, character(1))
      err_msg <- vapply(dat$errors, function(x) x$message, character(1))
      err_str <- sprintf("\n\t - %s: %s", err_code, err_msg)
      stop(sprintf("Error (code %s) in request:%s",
                   httr::status_code(r), paste(err_str, collapse = "")),
           call. = FALSE)
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


get_input <- function(value, name, secret, location) {
  if (is.null(value)) {
    read <- if (secret) get_pass else read_line
    key <- c(sprintf("montagu.%s.%s", location, name),
             sprintf("montagu.%s", name))
    value <- get_option_cascade(key,
                                read(sprintf("Enter montagu %s: ", name)))
  }
  assert_scalar_character(value, name)
  value
}


get_option_cascade <- function(x, default) {
  for (el in x) {
    v <- getOption(el)
    if (!is.null(v)) {
      return(v)
    }
  }
  default
}
