## Montagu options:
montagu <- new.env(parent = emptyenv())

montagu_add_location <- function(name, hostname, port, verbose = FALSE,
                                 overwrite = FALSE) {
  if (name %in% names(montagu$hosts) && !overwrite) {
    return()
  }
  assert_scalar_character(hostname)
  assert_scalar_integer(port)

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
    opts = opts,
    api_version = api_version,
    url = sprintf("https://%s:%d/api/v%d",
                  hostname, port, api_version),
    url_reports = sprintf("https://%s:%d/reports/api/v%d",
                          hostname, port, api_version),
    url_www = url_www)
}

montagu_add_location_defaults <- function() {
  montagu$hosts <- list()
  montagu_add_location("production", "montagu.vaccineimpact.org", 443L)
  montagu_add_location("science", "support.montagu.dide.ic.ac.uk", 11443L)
  montagu_add_location("uat", "support.montagu.dide.ic.ac.uk", 10443L)
  montagu_set_default_location("production")
}

montagu_credentials <- function(username, password) {
  username <- get_input(username, "username", FALSE)
  password <- get_input(password, "password", TRUE)
  openssl::base64_encode(sprintf("%s:%s", username, password))
}

montagu_authorise <- function(username = NULL, password = NULL,
                              location = NULL, refresh = FALSE) {
  location <- montagu_location(location)
  dat <- montagu$hosts[[location]]
  if (is.null(dat)) {
    stop(sprintf("Unknown location '%s'", location))
  }
  if (is.null(dat$token) || refresh) {
    message(sprintf("Authorising with montagu '%s' (https://%s:%s)",
                    location, dat$hostname, dat$port))
    auth <- montagu_credentials(username, password)
    h <- httr::add_headers("Authorization" = paste("Basic", auth))

    r <- httr::POST(paste0(dat$url, "/authenticate/"),
                    h, dat$opts,
                    body = list("grant_type" = "client_credentials"),
                    encode = "form")
    httr::stop_for_status(r)
    t <- jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"))
    dat$token <- httr::add_headers(
      "Authorization" = paste("Bearer", t$access_token))
    montagu$hosts[[location]] <- dat
  }
  invisible(dat)
}

montagu_set_default_location <- function(location) {
  if (!(location %in% names(montagu$hosts))) {
    stop(sprintf("Unknown montagu location '%s' - must be one of %s",
                 location, paste(names(montagu$hosts), collapse = ", ")))
  }
  montagu$default <- location
}

montagu_location <- function(location = NULL) {
  location %||% montagu$default
}

montagu_GET <- function(...) {
  montagu_request(httr::GET, ...)
}

montagu_POST <- function(...) {
  montagu_request(httr::POST, ...)
}

montagu_request <- function(verb, path, ..., location = NULL,
                            accept = "json", dest = NULL, progress = TRUE,
                            reports = FALSE, montagu = NULL) {
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
  r <- verb(url,
            dat$token,
            dat$opts,
            montagu_accept(accept),
            montagu_dest(dest, accept, progress),
            ...)
  montagu_response(r, accept, dest)
}

montagu_response <- function(r, accept, dest) {
  if (httr::status_code(r) == 404) {
    ## Not sure about 403
    stop("endpoint or resource not found")
  }
  if (accept == "json") {
    dat <- jsonlite::fromJSON(httr::content(r, "text"),
                              simplifyDataFrame = FALSE,
                              simplifyMatrix = FALSE)
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

get_input <- function(value, name, secret) {
  if (is.null(value)) {
    read <- if (secret) get_pass else read_line
    value <- getOption(paste0("montagu.", name),
                       read(sprintf("Enter montagu %s: ", name)))
  }
  assert_scalar_character(value, name)
  value
}

get_default <- function(value, name, default) {
  value %||% getOption(paste0("montagu.", name), default)
}
