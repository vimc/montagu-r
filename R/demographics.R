##' Montagu provides standardised demographic data for the groups to use in
##' their models. The source of the data is UNWPP, although various procedures
##' are applied for convenience, including work on some smaller countries,
##' people above the age of 80 in certain time periods, and extrapolating
##' both cohorts backwards in time to their origins before 1950. Internal
##' documents in the reporting portal describe the methods and motivations for
##' these extensions.
##'

##' @export
##' @title List the demographic data available for a given touchstone.
##' @param touchstone_id Touchstone identifier
##' @param location The location of the montagu api server.
##' @return A data frame, giving the code and description for each demographic
##' statistic, whether the data are stratified by gender, and a code indicating
##' the source of the demographic data, which will change if there are future
##' updates to any of the demographic data fields.
montagu_demographics_list <- function(touchstone_id, location = NULL) {
  assert_character(touchstone_id)
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

## I am imagining montagu_demographics_download to be private, and only
## called by montagu_demographic_data.
##
## This one will be the cache-free version - it always downloads data.
## If 'dest' is NULL we'll return the data, otherwise we just download
## it.
##
## format is a string at this point, 'wide' or 'long'

montagu_demographics_download <- function(touchstone_id, source_code,
                                          type_code, gender_code,
                                          format, dest = NULL,
                                          location = NULL) {

  assert_scalar_character(touchstone_id)
  assert_scalar_character(source_code)
  assert_scalar_character(type_code)
  assert_scalar_character(gender_code)
  assert_scalar_character(format)
  
  # See issue 2736 - the API could perhaps throw a 404 if source
  # code is invalid.
  
  d <- montagu_demographics_list(touchstone_id, location)
  if (!source_code %in% d$source) {
    stop(sprintf("Unknown demographic source type with id '%s'", source_code))
  }

  query <- http_query(gender = gender_code,
                      format = format)

  path <- sprintf("/touchstones/%s/demographics/%s/%s/",
                  touchstone_id, source_code, type_code)
  res <- montagu_api_GET(location, path, accept = "csv", query = query)

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
##' @inherit montagu_demographics_list
##' @title Download demographic data for a given touchstone.
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

  assert_scalar_character(type_code)
  assert_scalar_character(touchstone_id)
  assert_scalar_logical(wide)
  
  if (!is.null(gender_code)) {
    assert_scalar_character(gender_code)
  }
  
  if (!is.null(source_code)) {
    assert_scalar_character(source_code)
  }
  
  location <- montagu_location(location)
  cache <- location$cache

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
        stop(sprintf("Unknown demographic-statistic-type with id '%s'", type_code))
      } else if (sum(i) > 1L) {
        stop("More than one source_code applicable for this source: ",
             paste(squote(d$source[i]), collapse = ", "))
      }
      source_code <- d$source[i]
    }

    # Check gender_code is valid

    if (!is.null(gender_code)) {
      assert_character(gender_code)
      if (!gender_code %in% c("male", "female", "both")) {
        stop(sprintf("Invalid gender code '%s' - use male, female or both",
                     gender_code))
      }

      if (gender_code %in% c("male", "female")) {
        d <- montagu_demographics_list(touchstone_id, location)
        d <- d[d$id == type_code,]
        if (!d$gendered) {
          stop(sprintf("Type '%s' is non-gendered and cannot be filtered by '%s'",
                       type_code, gender_code))

        }
      }
    }

    format <- if (wide) "wide" else NULL
    dat <- montagu_demographics_download(touchstone_id, source_code, type_code,
                                         gender_code, format, NULL, location)

    cache$set(hash, key, "demographic_data_meta")
    cache$set(hash, dat, "demographic_data")
  }
  dat
}
