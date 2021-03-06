% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/responsibilities.R
\name{montagu_current_estimate_set_info}
\alias{montagu_current_estimate_set_info}
\title{Get information on current estimate set for a scenario.}
\usage{
montagu_current_estimate_set_info(modelling_group_id, touchstone_id,
  scenario_id, location = NULL)
}
\arguments{
\item{modelling_group_id}{id of the modelling group. If omitted, or null, 
then all scenarios associated with the touchstone are returned.}

\item{touchstone_id}{id of the touchstone (including version)}

\item{scenario_id}{id of the scenario}

\item{location}{The montagu server to connect to.}
}
\value{
A list of fields about the current estimate set
}
\description{
Groups upload estimate sets to Montagu. The most recent set of results
uploaded by a group for a given scenario can be queried for its status,
or retrieved.
}
