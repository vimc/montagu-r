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
    utils::read.csv(tmp, stringsAsFactors = FALSE)
  } else {
    writeBin(res, dest)
    invisible(dest)
  }
}


##' @export
##' @rdname montagu_demographics
##'
##' @param type_code The demographic type code (something like
##'   \code{cbr} for \code{Fertility: Crude birth rate (CBR)}.  Get
##'   the possible values from \code{montagu_demographics_list}
##'
##' @param gender_code One of "male", "female" or "both" (if omitted
##'   montagu will return "both").  Not applicable to all statistics.
##'
##' @param wide Logical, if \code{TRUE} returns data in wide format
##'   rather than long.
##'
##' @param source_code Optional source code, may be useful if one
##'   \code{type_code} can come from two sources within a touchstone.
montagu_demographic_data <- function(type_code, touchstone_id,
                                     gender_code = NULL, wide = FALSE,
                                     source_code = NULL, location = NULL) {
  location <- montagu_location(location)
  cache <- montagu_cache(location)

  key <- list(type_code = type_code, touchstone_id = touchstone_id,
              gender_code = gender_code, wide = wide,
              source_code = source_code)
  hash <- cache$hash_object(key)

  if (cache$exists(hash, "demographic_data")) {
    dat <- cache$get(hash, "demographic_data")
  } else {
    if (is.null(source_code)) {
      d <- montagu_demographics_list(touchstone_id, location = location)
      i <- d$id == type_code
      if (sum(i) == 0L) {
        stop("Could not determine source_code")
      } else if (sum(i) > 1L) {
        stop("More than one source_code applicable for this source: ",
             paste(squote(d$source[i]), collapse = ", "))
      }
      source_code <- d$source[i]
    }
    format <- if (wide) "wide" else NULL
    dat <- montagu_demographics_download(touchstone_id, source_code, type_code,
                                         gender_code, format)

    cache$set(hash, key, "demographic_data_meta")
    cache$set(hash, dat, "demographic_data")
  }
  dat
}
