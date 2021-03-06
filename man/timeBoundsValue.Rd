% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadStationData.R
\name{timeBoundsValue}
\alias{timeBoundsValue}
\title{Compute time bounds}
\usage{
timeBoundsValue(timeDates, tz)
}
\arguments{
\item{timeDates}{A POSIXlt vector of dates}

\item{tz}{Time zone}
}
\value{
A list with components start and end, of POSIXct dates
}
\description{
Compute start/end verification time bounds from a vector of dates.
}
\note{
The function is currently exported to be internally used by the VALUE package to load predictions.
}
\keyword{internal}
