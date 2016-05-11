#' @title  Run time definition according to the time specifications in decadal forecasts
#' @description Define index positions of runtimes in decadal forecasts. 
#' Its output is passed to the loading functions (the \code{makeSubset\\.*} and \code{derive*} interfaces).
#' @param dataset Target hindcast dataset 
#' @param grid A java \dQuote{GeoGrid} containing the target(leading) variable
#' @param members Member selection 
#' @param season Season selection 
#' @param years Years selected 
#' @return A list of parameters:
#' \itemize{
#' \item validMonth An integer in the range [1,12] indicating the month to take the initialization
#' \item years A vector of years selected
#' \item season Season
#' \item year.cross An auxiliary integer value thet indicates the position of year-crossing
#'  within the season vector. NULL if no year-crossing season has been chosen.
#' \item runDates a POSIXlt vector of initialization dates
#' \item runTimeRanges a list of of initialization times of the java class \dQuote{ucar.ma2.ranges}.
#' }
#' @details The function calls to specific subroutines for CFS or System4 requests, given their different
#' runtime configurations. The function also takes care of selecting the appropriate initialization
#' in the case of year-crossing seasons 
#' @author J. Bedia, S. Herrera
#' @keywords internal

getRunTimeDomain.decadal <- function(dataset, grid, members, season, years) {
      message("[", Sys.time(), "] Defining initialization time parameters")
      gcs <- grid$getCoordinateSystem()
      if (is.null(season)) {
            season <- unique(javaCalendarDate2rPOSIXlt(gcs$getTimeAxisForRun(0L)$getCalendarDates())$mon + 1)
      }
      rt.axis <- gcs$getRunTimeAxis()
      runDatesAll <- javaCalendarDate2rPOSIXlt(rt.axis$getCalendarDates())
      startDay <- javaCalendarDate2rPOSIXlt(rt.axis$getCalendarDateRange()$getStart())
      endDay <- javaCalendarDate2rPOSIXlt(rt.axis$getCalendarDateRange()$getEnd())
      startYear <- startDay$year + 1900
      endYear <- endDay$year + 1900
      allYears <- startYear:endYear
      if (is.null(years)) {
        years <- allYears
      } 
      if (years[1] < startYear & years[length(years)] > endYear) {
            warning("Year selection out of dataset range. Only available years will be returned")
            years <- allYears
      }
      if (years[1] < startYear) {
            warning("First year in dataset: ", startYear,". Only available years will be returned")
            years <- startYear:years[length(years)]
      }
      if (years[length(years)] > endYear) {
            warning("Last initialization in the dataset in year: ", endYear,". Only available years will be returned")
            years <- years[which(years <= endYear + 1)]
      }
      # Month to take the initialization 
      validMonth <- 1
      if (!identical(season, sort(season))) {
            year.cross.ind <- which(diff(season) < 0) # indicates the position of year-crossing within season
            if (years[1] == startYear) { 
                  warning(paste0("First forecast date in dataset: ", startDay, ".\nRequested seasonal data for ", startYear," not available"))
                  years <- years[-length(years)]
            } else {
                  years <- years - 1      
            }
      } else {
            year.cross.ind <- NULL
      }
      # runtime parameters depending on model
      runTimesAll <- which(runDatesAll$mon == (validMonth - 1))
      if (length(runTimesAll) == 0) {
        stop(paste("Incompatible 'leadMonth' and 'season' argument values.\nInitializations in", paste(month.name[unique(runDatesAll$mon + 1)], collapse = ", ")))
      }
      runTimeRanges <- lapply(1:length(runTimesAll), function(x) {
        .jnew("ucar/ma2/Range", as.integer(runTimesAll[x] - 1), as.integer(runTimesAll[x] - 1))
      })
      validYears <- array(data = 0, dim = c(length(runTimeRanges),1))
      for (i in 1:length(runTimeRanges)) {
            auxDates <- javaCalendarDate2rPOSIXlt(gcs$getTimeAxisForRun(runTimeRanges[[i]]$element(0L))$getCalendarDates())
            if (any((auxDates$year + 1900) %in% years)) {
                  validYears[i] <- 1
            }
      }
      runTimes <- runTimesAll[which(validYears == 1)]
      runTimesAll <- NULL
      runDates <- runDatesAll[runTimes]
      # java ranges
      runTimeRanges <- lapply(1:length(runTimes), function(x) {
        .jnew("ucar/ma2/Range", as.integer(runTimes[x] - 1), as.integer(runTimes[x] - 1))
      })
      rtPars <- list("runDates" = runDates, "runTimeRanges" = runTimeRanges)
      return(list("validMonth" = validMonth, "years" = years, "season" = season, "year.cross" = year.cross.ind, "runDates" = rtPars$runDates, "runTimeRanges" = rtPars$runTimeRanges))
}
# End
