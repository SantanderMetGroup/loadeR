#' @param members A vector of integers indicating the members to be loaded.
#'  Default to \code{NULL}, which loads all available members 
#'  . For instance, \code{members=1:5} will retrieve the first five members of dataset.
#'   Discontinuous member selection (e.g. \code{members=c(1,5,7)}) is allowed. If the requested dataset is not a forecast
#'    or the requested variable is static (e.g. orography) it will be ignored.
