% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/responsibilities.R
\name{montagu_central_burden_estimate_template}
\alias{montagu_central_burden_estimate_template}
\title{Get central burden estimate template for an expectation}
\usage{
montagu_central_burden_estimate_template(modelling_group_id, touchstone_id,
  expectation_id, location = NULL)
}
\arguments{
\item{modelling_group_id}{id of the modelling group. If omitted, or null, 
then all scenarios associated with the touchstone are returned.}

\item{touchstone_id}{id of the touchstone (including version)}

\item{expectation_id}{id of the expectation (integer)}

\item{location}{The montagu server to connect to.}
}
\value{
A data frame with columns disease, year, age, country, and
        country_name with given values, then cohort_size, deaths,
        cases and dalys, all NA.
}
\description{
The burden estimate set is the list of burden estimates for a particular
scenario for a touchstone. Montagu provides csv templates including rows for
all the countries, years and ages, and columns for all the burden outcomes
expected. These templates can then be filled in by the modelling groups,
and uploaded to Montagu as their results submission.
}
