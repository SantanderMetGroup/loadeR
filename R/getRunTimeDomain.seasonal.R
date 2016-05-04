getRunTimeDomain.seasonal <- function(dataset, grid, members, season, years, leadMonth) {
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
      if (is.null(years)) years <- allYears
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
      validMonth <- season[1] - leadMonth 
      if ((season[1] - leadMonth) < 1) {
            validMonth <- validMonth + 12
            years <- years - 1 
      }
      # Year-crossing seasons - year to take the initialization
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
      # runtime parameters ----------
      runTimesAll <- which(runDatesAll$mon == (validMonth - 1))
      if (length(runTimesAll) == 0) {
            stop(paste("Incompatible 'leadMonth' and 'season' argument values.\nInitializations in", 
                       paste(month.name[unique(runDatesAll$mon + 1)], collapse = ", ")),
                 call. = FALSE)
      }
      runDatesValidMonth <- runDatesAll[runTimesAll]
      runTimes <- runTimesAll[which((runDatesValidMonth$year + 1900) %in% years)]
      runDatesValidMonth <- runTimesAll <- NULL
      runDates <- runDatesAll[runTimes]
      # java ranges
      runTimeRanges <- lapply(1:length(runTimes), function(x) {
            .jnew("ucar/ma2/Range", as.integer(runTimes[x] - 1), as.integer(runTimes[x] - 1))
      })
      return(list("validMonth" = validMonth,
                  "years" = years,
                  "season" = season,
                  "year.cross" = year.cross.ind,
                  "runDates" = runDates,
                  "runTimeRanges" = runTimeRanges))
}
