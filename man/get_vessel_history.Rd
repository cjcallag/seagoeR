% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vessels.R
\name{get_vessel_history}
\alias{get_vessel_history}
\title{Get Vessel History}
\usage{
get_vessel_history(api_key, mmsi, age = NULL)
}
\arguments{
\item{api_key, }{API access token}

\item{mmsi, }{mmsi tied to the vessel to get history trail for}

\item{age, }{how old the vessels that are returned can be in hours(must be between 1 and 720 hours)}
}
\description{
returns history trail for a specific vessel
}
\author{
Christopher Callaghan, \email{cjcallag@nps.edu}
}
