#' @title Date adjustment
#' @description Adjust dates in forecast data
#' @param foreTimePars A list of elements as returned by \code{getRunTimeDomain.decadal}
#' @author J. Bedia, S. Herrera
#' @keywords internal
#' @export

adjustDates.forecast <- function(foreTimePars) {
      dates <- as.POSIXct(do.call("c", foreTimePars$forecastDates[[1]]))
      interval <- 0
      dif <- difftime(dates[2], dates[1], units = "days")
      accum.dates.fix <- FALSE
      if ("cell_method" %in% names(foreTimePars)) accum.dates.fix <- foreTimePars$deaccum & foreTimePars$cell_method == "sum"
      if (foreTimePars$aggr.m != "none" | (dif > 27 & dif < 32)) {
            mon.len <- sapply(dates, ndays)
            interval <- mon.len * 86400
      } else if (foreTimePars$aggr.d != "none" | accum.dates.fix) {
            dates <- format(as.Date(substr(dates, 1, 10)), format = "%Y-%m-%d %H:%M:%S", usetz = TRUE) 
            interval <- 86400
      }
      formato <- ifelse(dif < 1, "%Y-%m-%d %H:%M:%S", "%Y-%m-%d")
      if (accum.dates.fix) {
            dates.end <- format(as.POSIXct(as.POSIXlt(dates, tz = "GMT"), tz = "GMT"), format = formato, usetz = TRUE)
            dates.start <- format(as.POSIXct(as.POSIXlt(dates, tz = "GMT") - interval, tz = "GMT"), format = formato, usetz = TRUE)
      }else{
            dates.end <- format(as.POSIXct(as.POSIXlt(dates, tz = "GMT") + interval), format = formato, usetz = TRUE)
            dates.start <- format(as.POSIXct(as.POSIXlt(dates, tz = "GMT"), tz = "GMT"), format = formato, usetz = TRUE)
      }
      return(list("start" = dates.start, "end" = dates.end))
}
# End
