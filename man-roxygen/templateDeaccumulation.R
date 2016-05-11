#' @section Deaccumulation:
#' 
#' In case of variables that are deaccumulated (e.g. precipitation amount and radiations in System4 models), 
#' it must be noted that the original forecast dates correspond to the start of each verification step.
#'  Thus, the first value is always zero, and then it starts accumulating. The deaccumulation routine computes a 
#'  lagged difference between forecast dates (R function \code{\link{diff}}) to provide the deaccumulated series. 
#'  Therefore, the first value is always lost. To avoid a systematic loss of the first day, when a deaccumulable
#'   variable is requested the function internally loads the previous time step (e.g., \code{season = c(12,1,2)}
#'   for daily precipitation, the forecast date 30-Nov is also loaded, being the first value of the series
#'    -1st December- the difference between 1 december and 30 november in the model). 
#'    As a result, in \code{leadMonth = 0} requests, the first day of the series is lost,
#'     because there is not a previous forecast time in the initialization to start deaccumulating.



