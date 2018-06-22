##' This may get a name change pending splitting apart of various
##' montagu components.
##' @title The Montagu API: demographics
##' @param location A montagu location
##' @rdname montagu_demographics
##' @name montagu_demographics
NULL


##' @export
##' @rdname montagu_demographics
##' @param touchstone_id Touchstone identifier
montagu_demographics_list <- function(touchstone_id, location = NULL) {
  location <- montagu_location(location)
  tryCatch(
    location$cache$get(touchstone_id, "demographics_list"),
    KeyError = function(e) {
      path <- sprintf("/touchstones/%s/demographics/", touchstone_id)
      res <- montagu_api_GET(location, path)
      d <- data_frame(
        id = vcapply(res, "[[", "id"),
        name = vcapply(res, "[[", "name"),
        gendered = vlapply(res, "[[", "gender_is_applicable"),
        source = vcapply(res, "[[", "source"))
      location$cache$set(touchstone_id, d, "demographics_list")
      d
    }
  )
}


## This one will be the cache-free version - it always downloads data.
## If 'dest' is NULL we'll return the data, otherwise we just download
## it.
montagu_demographics_download <- function(touchstone_id, source_code,
                                          type_code, gender_code = NULL,
                                          format = NULL, dest = NULL,
                                          location = NULL) {
  query <- http_query(gender_code = gender_code,
                      format = format)
  path <- sprintf("/touchstones/%s/demographics/%s/%s/",
                  touchstone_id, source_code, type_code)
  res <- montagu_api_GET(location, path, accept = "csv")
  if (is.null(dest)) {
    tmp <- tempfile()
    on.exit(unlink(tmp))
    writeBin(res, tmp)
    read.csv(tmp, stringsAsFactors = FALSE)
  } else {
    writeBin(res, dest)
    invisible(dest)
  }
}
