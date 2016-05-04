#' @section Definition of temporal slices:
#' 
#' The function has been implemented to access seasonal slices, as determined by the \code{season} argument.
#'  Seasons can be defined in several ways: A single month (e.g. \code{season=1} for January, a standard season
#'   (e.g. \code{season=c(1,2,3)} for JFM, or \code{season=c(12,1,2)} for DJF), or any period of consecutive months
#'    (e.g. \code{season=c(1:6)}, for the first half of the year). Seasons are returned for a given year period
#'     (defined by the years argument, e.g. \code{years=1981:2000}) with a homogeneous forecast lead time 
#'     (as given by the leadMonth argument; e.g. \code{leadMonth=1} for one-month lead time) with respect to the first
#'    month of the selected season. For example, \code{season=c(1,2,3)} for \code{years=1995:2000} and \code{leadMonth=1}
#'    will return the following series: JFM 1995 from the December 1994 runtime forecast, ..., JFM 2000 from the 
#'    December 1999 runtime forecast.
#'    
#'    \strong{Year-crossing seasons}
#'     
#'    It is possible to work with year-crossing seasons, such as DJF. In this case, \code{season=c(12,1,2)}
#'     for \code{years=1995:2000} and \code{leadMonth=1} will return the following series: DJF 1994/1995 
#'     (from the November 1994 runtime forecast), ..., DJF 1999/2000 (from the November 1999 runtime forecast). 
#'    
#'    \strong{Full initialization length}
#'      
#'     In case the whole year/forecast extent is needed (instead of a particular season), the argument \code{season} can be omitted.
#'     In this case, its default value is \code{NULL}, equivalent to setting \code{season = 1:12}, or \code{season = 1:n}, 
#'     being \emph{n} the remaining number of forecast months since the given lead month in the case of seasonal forecasts
#'     . The same applies to the argument \code{years}, being all the available years returned when omitted.
#'     
#'   \strong{Initialization times}
#'   
#'   The characteristics of the \code{InitializationDates} output vary depending on the dataset. In the case of models that have simultaneous
#'    initializations for different members (e.g. System4), the output is just a vector of initialization times (one per year selected).
#'     Unlike the simultaneous initializations scheme, the lagged runtime configuration of members used by some other models (e.g. CFSv2)
#'    results in different initialization times for the same forecast times of different members. In this case, the \code{InitializationDates}
#'     are included in a list whose elements are named as the corresponding member.
#'     
