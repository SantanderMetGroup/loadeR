#' @title Selection of time slices of forecast datasets
#' @description Calculates parameters for forecast time determination along the runtime axis. 
#' @param grid a java \sQuote{GeoGrid}
#' @param dataset character string of the dataset
#' @param dic dictionary information
#' @param runTimePars A list of elements as returned by \code{getRunTimeDomain*}
#' @param time Verification time defined as a character string (e.g. \dQuote{06} for data
#' verifying at 06:00:00). Only applies for sub-daily datasets.
#' @param aggr.d Aggregation function for subdaily to daily
#' @param aggr.m Aggregation function from daily to monthly
#' @return A list with the following elements:
#' \item{forecastDates}{A list with POSIXlt dates defining the start and end of the 
#' representative verification time. If start and end are identical, the variable is instantaneous
#' and therefore the representative time interval is 0}
#' \item{foreTimeRangesList}{A list of length \emph{i} containing the java ranges defining the
#' forecast times selected along the \emph{i-th} run time axis.}
#' \item{foreTimeShift}{Integer value (java format) giving the shift to start reading in the time axis}
#' \item{foreTimeStride}{Integer value (java format) giving the stride for reading in the time axis}
#' \item{deaccumFromFirst}{NULL if no deaccumulation is performed. TRUE or FALSE if deaccumulation is performed from
#' the first time of the runtime axis or not respectively. If FALSE, an additional runtime is added at the beginning
#' of each element of the runTimeList to avoid losing the first day when performing deaccumulation.}
#' \item{doDailyMean}{Logical. Are the forecast time values going to be used for data aggregation?. This argument is passed
#' to \code{makeSubset} family functions to undertake the pertinent aggregation if TRUE.}
#' @keywords internal
#' @author J.Bedia, S. Herrera

getForecastTimeDomain <- function(grid, dataset, dic, runTimePars, time, aggr.d, aggr.m) {
      gcs <- grid$getCoordinateSystem()
      timeResInSeconds <- gcs$getTimeAxisForRun(runTimePars$runTimeRanges[[1]]$element(0L))$getTimeResolution()$getValueInSeconds()
      if ((aggr.d == "none") & (time == "DD") & ((timeResInSeconds / 3600) < 24)) {
            stop("Data is sub-daily:\nA daily aggregation function must be indicated to perform daily aggregation")
      }
      # Si es MM hay que asegurarse de que se calcula sobre dato diario
      if ((aggr.m != "none") & ((timeResInSeconds / 3600) < 24) & (time == "none")) {
            stop("Data is sub-daily:\nA daily aggregation function must be indicated first to perform monthly aggregation")
      }
      if ((timeResInSeconds / 3600) == 24) {
            time <- "DD"
            if (aggr.d != "none") {
                  aggr.d <- "none"
                  message("NOTE: The original data is daily: argument 'aggr.d' ignored")
            }
      }
      if (aggr.d != "none") message("NOTE: Daily aggregation will be computed from ", timeResInSeconds / 3600, "-hourly data")
      if (aggr.m != "none") message("NOTE: Daily data will be monthly aggregated")
      foreTimesList <- rep(list(bquote()), length(runTimePars$runTimeRanges))
      foreDatesList <- foreTimesList
      for (i in 1:length(runTimePars$runTimeRanges)) {
            auxDates <- javaCalendarDate2rPOSIXlt(gcs$getTimeAxisForRun(runTimePars$runTimeRanges[[i]]$element(0L))$getCalendarDates())
            ind <- which(((auxDates$mon + 1) %in% runTimePars$season) & ((auxDates$year + 1900) %in% runTimePars$years))
            foreTimesList[[i]] <- seq(from = min(ind), to = max(ind), by = 1)
            foreDatesList[[i]] <- auxDates[foreTimesList[[i]]]
            auxDates <- NULL
      }
      if (time == "DD" | time == "none") {
            foreTimeStride <- 1L
            foreTimeShift <- 0L
      } else {
            time <- as.integer(time)
            timeIndList <- lapply(1:length(foreDatesList), function(x) {
                  which(foreDatesList[[x]]$hour == time)
            })
            if (length(timeIndList[[1]]) == 0) {
                  stop("Non-existing verification time selected.\nCheck value of argument 'time'")
            }
            foreDatesList <- lapply(1:length(foreDatesList), function(x) {
                  foreDatesList[[x]][timeIndList[[x]]]
            })
            foreTimeStride <- as.integer(diff(timeIndList[[1]])[1])
            foreTimeShift <- as.integer(-(timeIndList[[1]][1] - 1))
            timeIndList <- NULL
      }
      # Sub-routine for adjusting times in case of deaccumulation
      deaccum <- FALSE
      if (!is.null(dic)) {
            if (dic$deaccum == 1) {
                  deaccum <- TRUE
                  foreTimesList <- lapply(1:length(foreTimesList), function(x) {
                        append(foreTimesList[[x]], tail(foreTimesList[[x]], 1) + 1)
                  })
            }
      }
      foreTimeRangesList <- lapply(1:length(foreTimesList), function(x) {
            .jnew("ucar/ma2/Range",
                  as.integer(foreTimesList[[x]][1] - 1), 
                  as.integer(tail(foreTimesList[[x]], 1L) - 1),
                  foreTimeStride)$shiftOrigin(foreTimeShift)
      })
      return(list("forecastDates" = foreDatesList,
                  "ForeTimeRangesList" = foreTimeRangesList,
                  "deaccum" = deaccum,
                  "aggr.d" = aggr.d,
                  "aggr.m" = aggr.m))
}
# End
