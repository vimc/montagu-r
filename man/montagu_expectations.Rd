% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/responsibilities.R
\name{montagu_expectations}
\alias{montagu_expectations}
\title{Get expectations for a modelling group and touchstone}
\usage{
montagu_expectations(modelling_group_id, touchstone_id, location = NULL)
}
\arguments{
\item{modelling_group_id}{id of the modelling group. If omitted, or null, 
then all scenarios associated with the touchstone are returned.}

\item{touchstone_id}{id of the touchstone (including version)}

\item{location}{The montagu server to connect to.}
}
\value{
A data frame of information about the expectation
}
\description{
The expectations, for a modelling group for a particular touchstone, indicate the
range of chronological years, ages and countries for which burden estimates
are expected. These are also per-disease, so groups that model multiple
diseases will see multiple rows of expectations.
}
