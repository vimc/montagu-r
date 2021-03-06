% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelling_groups.R
\name{montagu_modelling_groups}
\alias{montagu_modelling_groups}
\title{Retrieve list of all modelling group ids and names.}
\usage{
montagu_modelling_groups(location = NULL)
}
\arguments{
\item{location}{The montagu server to connect to.}
}
\value{
A dataframe of id and description for each group
}
\description{
Modelling group ids generally consist of an identifier for the
institution where the group is based, a hyphen, and the principal
investigor for that group. Groups will be informed of their internal 
id when joining the consortium, but for convenience, we provide
functions to look up the modelling group id, and associated models.
}
