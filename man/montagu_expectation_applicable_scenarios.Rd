% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/responsibilities.R
\name{montagu_expectation_applicable_scenarios}
\alias{montagu_expectation_applicable_scenarios}
\title{Get applicable scenarios for an expectation}
\usage{
montagu_expectation_applicable_scenarios(modelling_group_id, touchstone_id,
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
A vector of scenario names
}
\description{
Different scenarios may have different expectations. For example, for
Hepatitis B, different countries are required for different scenarios,
since the different HepB vaccination initiatives have been carried out in
different sets of countries. For other diseases, the same expectation might
be equally valid in different scenarios. Here, we can query which scenarios a
particular expectation applies to.
}
