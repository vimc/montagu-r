## Montagu options:
montagu <- new.env(parent = emptyenv())

## TODO: work out a nice way of triggering this.  But for the vast
## majority of users this will just be run once and forgotten
montagu_server_options <- function(hostname = NULL,
                                   port = NULL,
                                   verbose = FALSE) {
  hostname <- get_default(hostname, "hostname",
                          "montagu.vaccineimpact.org")
  port <- get_default(port, "port",
                      if (hostname == "localhost") 1443L else 443L)

  assert_scalar_character(hostname)
  assert_scalar_integer(port)
  api_version <- 1L

  opts <- if (verbose) httr::verbose() else NULL
  if (hostname == "localhost") {
    opts <- c(curl_insecure(), opts)
  }
  montagu$opts <- opts
  montagu$api_version <- api_version
  montagu$url <- sprintf("https://%s:%d/api/v%d",
                         hostname, port, api_version)
  montagu$url_reports <- sprintf("https://%s:%d/reports/api/v%d",
                                 hostname, port, api_version)
}

montagu_set_credentials <- function(username = NULL, password = NULL) {
  username <- get_input(username, "username", FALSE)
  password <- get_input(password, "password", TRUE)
  auth <- openssl::base64_encode(sprintf("%s:%s", username, password))
  montagu$auth <- auth
}

montagu_authorise <- function(username = NULL, password = NULL) {
  if (is.null(montagu$auth)) {
    montagu_set_credentials(username, password)
  }
  h <- httr::add_headers("Authorization" = paste("Basic", montagu$auth))
  r <- httr::POST(paste0(montagu$url, "/authenticate/"),
                  h, montagu$opts,
                  body = list("grant_type" = "client_credentials"),
                  encode = "form")
  httr::stop_for_status(r)
  t <- jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"))
  montagu$token <- httr::add_headers(
    "Authorization" = paste("Bearer", t$access_token))
}

montagu_GET <- function(...) {
  montagu_request(httr::GET, ...)
}

montagu_POST <- function(...) {
  montagu_request(httr::POST, ...)
}

montagu_request <- function(verb, path, ...,
                            accept = "json", dest = NULL, progress = TRUE,
                            reports = FALSE) {
  if (is.null(montagu$token)) {
    montagu_authorise()
  }
  base <- if (reports) montagu$url_reports else montagu$url
  if (!grepl("^/", path)) {
    stop("Expected an absolute path")
  }
  url <- paste0(base, path)
  r <- verb(url,
            montagu$token,
            montagu$opts,
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
