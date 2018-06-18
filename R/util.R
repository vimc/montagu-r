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
  clean_input_text(readline(prompt = prompt)) # nocov
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

clean_input_text <- function(x) {
  re <- "(^\\s*[\"']?|[\"']?\\s*$)"
  gsub(re, "", x, perl = TRUE)
}

read_chunked <- function(con, n) {
  assert_connection(con)
  next_chunk <- readLines(con, n)
  if (length(next_chunk) == 0L) {
    stop("connection has already been completely read")
  }
  function() {
    data <- next_chunk
    next_chunk <<- readLines(con, n)
    complete <- length(next_chunk) == 0L
    list(data = data, complete = complete)
  }
}

from_json <- function(x) {
  jsonlite::fromJSON(x, simplifyDataFrame = FALSE,  simplifyMatrix = FALSE)
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

## TODO: this can be done more standalone but it would be nice to get
## Gabor to add it to the package I think.
clear_progress_bar <- function(p) {
  private <- environment(p$tick)$private
  if (nchar(private$last_draw) > 0) {
    progress:::clear_line(private$stream, private$width)
  }
  progress:::cursor_to_start(private$stream)
}


format_output <- function(output) {
  paste(sprintf("%s\n", c(output$stderr, output$stdout)), collapse = "")
}


trim_string <- function(s, w, elipsis = " ...") {
  if (nchar(s) > w) {
    s <- paste0(substr(s, 1L, w - nchar(elipsis)), elipsis)
  }
  s
}
