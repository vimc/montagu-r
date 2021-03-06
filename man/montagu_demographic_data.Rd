% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demographics.R
\name{montagu_demographic_data}
\alias{montagu_demographic_data}
\title{Download demographic data for a given touchstone.}
\usage{
montagu_demographic_data(type_code, touchstone_id, gender_code = NULL,
  wide = FALSE, source_code = NULL, location = NULL)
}
\arguments{
\item{type_code}{The demographic type code (something like
\code{cbr} for \code{Fertility: Crude birth rate (CBR)}.  Get
the possible values from \code{montagu_demographics_list}}

\item{touchstone_id}{Touchstone identifier}

\item{gender_code}{One of "male", "female" or "both" (if omitted
montagu will return "both").  Not applicable to all statistics.}

\item{wide}{Logical, if \code{TRUE} returns data in wide format
rather than long.}

\item{source_code}{Optional source code, may be useful if one
\code{type_code} can come from two sources within a touchstone.}

\item{location}{The location of the montagu api server.}
}
\value{
A data frame, giving the code and description for each demographic
statistic, whether the data are stratified by gender, and a code indicating
the source of the demographic data, which will change if there are future
updates to any of the demographic data fields.
}
\description{
Montagu provides standardised demographic data for the groups to use in
their models. The source of the data is UNWPP, although various procedures
are applied for convenience, including work on some smaller countries,
people above the age of 80 in certain time periods, and extrapolating
both cohorts backwards in time to their origins before 1950. Internal
documents in the reporting portal describe the methods and motivations for
these extensions.
}
