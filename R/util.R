`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

get_pass <- function(prompt) {
  getPass::getPass(prompt, TRUE) # nocov
}

read_line <- function(prompt) {
  readline(prompt = prompt) # nocov
}

## We'll need this when connecting to localhost.  It should be the
## only place that it is needed though.
curl_insecure <- function() {
  httr::config(ssl_verifypeer = 0, ssl_verifyhost = 0)
}

list_to_character <- function(x) {
  vapply(x, identity, character(1))
}

empty_default <- function(x, default) {
  if (length(x) == 0L) {
    default
  } else {
    x
  }
}

encode_path <- function(x) {
  gsub("[/\\\\]", ":", x)
}

decode_path <- function(x) {
  gsub(":", "/", x, fixed = TRUE)
}
