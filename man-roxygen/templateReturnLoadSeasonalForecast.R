#' @return A list with the following components providing the necessary information for data representation and analysis.
#' \itemize{
#' \item{Variable}{A list with two elements, and some other attributes including units and temporal aggregation details:}
#'      \itemize{ 
#'          \item \code{varName} A character string indicating which is the variable returned. Same as value provided 
#'          for argument \code{var}
#'          \item \code{level} A numeric value indicating the vertical level of the variable (\code{NULL} for 2D variables)
#'      } 
#' \item{Data}{A N-dimensional array. The number of dimensions (N) depends on the type of request given that 
#' dimensions of length one are dropped. Thus, N can take values from 4 (several members for a 
#' rectangular domain with different values for longitude, latitude, ensemble and time dimensions)
#'  to 1 (atomic vector), for single-point and single-member selections, for which only the time dimension is required.
#'   The dimensions are labelled by the \dQuote{dimnames} attribute, 
#'   and are always arranged in canonical order (i.e.: [member, time, level, lat, lon]).}
#' \item{xyCoords}{A list with \code{x} and \code{y} components, as required by many standard mapping
#'  functions in R (see e.g. \code{\link[graphics]{image}}).
#'   In addition, the attribute \code{projection} provides projection information details.}
#' \item{Dates}{A list with two \code{POSIXct} time elements of the same length as the \sQuote{time} 
#' dimension in \code{Data}, defining the time boundaries of the time axis coordinates in the 
#' interval \emph{[start, end)}, or if the loaded field is static, a character string indicating it. See details.}
#' \item{InitializationDates}{A \code{POSIXct} time object corresponding to the initialization
#'  times selected. Only for forecast datasets. NA for static variables (e.g. orography). See details.}
#' \item{Members}{A character vector with the names of the ensemble members returned,
#'  in the same order as arranged in the \code{Data} array. Only for forecast datasets. NA for static variables (e.g. orography). See details.
#' }
#' }
#' Additionally, there are three global attributes with metadata, (\code{"dataset"}, which is always present.
#'  In addition \code{"source"} and \code{"URL"} are added for datasets from the User Data Gateway.
