% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/revArrayLatDim.R
\name{revArrayLatDim}
\alias{revArrayLatDim}
\title{Reverse latitude coordinates ordering}
\usage{
revArrayLatDim(mdArray)
}
\arguments{
\item{mdArray}{A n-dimensional array of data as returned by \code{makeSubset}}
}
\value{
A n-dimensional array with the ordering of the lat dimension reversed
}
\description{
Reverses the order of the latitudinal coordinates when the dataset is read in
 reverse y order. Sub-routine of \code{makeSubset}.
}
\note{
The code is partially based on an example provided at \url{http://stackoverflow.com/a/14502298}
 by Hadley Wickham
}
\references{
\url{http://adv-r.had.co.nz/Computing-on-the-language.html}
}
\author{
J. Bedia
}
\keyword{internal}
