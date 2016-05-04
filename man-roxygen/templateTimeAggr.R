#' @section Temporal filtering and aggregation:
#' 
#' The argument \code{time} controls the temporal filtering/aggregation options that may apply for a variable.
#'     In case of daily mean data, this can be obtained in two different ways:
#'        \enumerate{
#'        \item For variables that are already stored as daily means in the dataset, both \code{"DD"} and \code{"none"}
#'         return the required daily output
#'       \item In case of subdaily data, if \code{"DD"} is chosen, the function will compute the daily value using the
#'        aggregation function indicated in the argument \code{aggr.d}, printing an information message on screen.
#'         This function is normally the \code{"mean"} providing daily averages, although if the variable is a flux 
#'         (e.g. precipitation or radiation, (\code{var} = \code{"tp"}, \code{"rsds"} or \code{"rlds"} using the standard UDG naming),
#'          the aggregation function may be \code{"sum"} (i.e., it will return the daily accumulated value).
#'          In the same way, if the variable is a daily maximum/minimum (i.e., \code{var = "tasmax"} / \code{var = "tasmin"}), 
#'          the corresponding function (\code{aggr.d = "max"} or \code{aggr.d = "min"}) could be applied to the subdaily outputs
#'           on a daily basis to obtain absolute maximum/minimum daily values.
#'           }
