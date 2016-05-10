# loadSeasonalForecast.R Load a user-defined spatio-temporal slice from a seasonal forecast dataset
#
#     Copyright (C) 2016 Santander Meteorology Group (http://www.meteo.unican.es)
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.


#' @title Load a seasonal forecast
#' @description  Load a user-defined spatio-temporal slice from a seasonal forecast
#' @template templateParams
#' @template templateParamMembers
#' @param leadMonth Integer value indicating the lead forecast time, 
#' relative to the first month of \code{season}. Note that \code{leadMonth=1} for \code{season=1} (January)
#'  corresponds to the December initialization. Default to 1 (i.e., 1 lead month forecast)..
#' @template templateParamsDicTimeAggr
#' @template templateReturnLoadSeasonalForecast
#' 
#' @template templateDicDetails
#' @template templateTimeAggr
#' @template templateGeolocation
#' @template templateDeaccumulation
#' @template templateTimeSeasonal
#' 
#' @importFrom stats na.exclude
#' 
#' @export
#' @author J. Bedia
#' @import rJava
#' 

loadSeasonalForecast <- function(dataset,
                                 var,
                                 dictionary = FALSE,
                                 members = NULL,
                                 lonLim = NULL,
                                 latLim = NULL,
                                 season = NULL,
                                 years = NULL,
                                 leadMonth = 1,
                                 time = "none",
                                 aggr.d = "none",
                                 aggr.m = "none") {
      time <- match.arg(time, choices = c("none","00","03","06","09","12","15","18","21","DD"))
      aggr.d <- match.arg(aggr.d, choices = c("none", "mean", "min", "max", "sum"))
      if (time != "DD" & aggr.d != "none") {
            aggr.d <- "none"
            message("NOTE: Argument 'aggr.d' ignored as 'time' was set to ", time)
      }
      aggr.m <- match.arg(aggr.m, choices = c("none", "mean", "min", "max", "sum"))
      aux.level <- findVerticalLevel(var)
      var <- aux.level$var
      level <- aux.level$level
      leadMonth <- as.integer(leadMonth)
      # Dictionary lookup
      cd <- check.dictionary(dataset, var, dictionary, time)
      shortName <- cd$shortName
      dic <- cd$dic
      if (!is.null(season) && (min(season) < 1 | max(season) > 12)) {
            stop("Invalid season definition", call. = FALSE)
      }
      if (!is.null(dic)) {
            if (dic$time_step == "MM" && aggr.d != "none") {
                  aggr.m <- "none"
                  message("NOTE: The dataset is already monthly. Argument 'aggr.m' ignored")
            }
      }
      if (is.null(season)) {
            stop("Argument 'season' must be provided")
      }
      if (min(season) < 1 | max(season) > 12) {
            stop("Invalid season definition")
      }
      # Discover dataset ---------------
      gds <- openDataset(dataset)
      grid <- gds$findGridByShortName(shortName)
      if (is.null(grid)) {
            stop("Variable requested not found\nCheck 'dataInventory' output and/or dictionary 'identifier'.", call. = FALSE)
      }
      # Geolocation ---------------
      latLon <- getLatLonDomain(grid, lonLim, latLim)
      proj <- grid$getCoordinateSystem()$getProjection()
      if (!proj$isLatLon()) latLon <- adjustRCMgrid(gds, latLon, lonLim, latLim)
      # Runtime collocation -------
      runTimePars <- getRunTimeDomain.seasonal(dataset, grid, members,season, years, leadMonth)
      #if (grepl("^System4", dataset)) {
      # Member definition ----------
      memberRangeList <- getMemberDomain(grid, dataset, members)
      # Forecast time collocation ---------
      foreTimePars <- getForecastTimeDomain(grid, dataset, dic, runTimePars, time, aggr.d, aggr.m)
      # Vertical level collocation ----------
      verticalPars <- getVerticalLevelPars(grid, level)
      # Subsetting -------------
      cube <- makeSubset.seasonal(grid, latLon, runTimePars, memberRangeList, foreTimePars, verticalPars)
      # Attribute definition ------
      foreTimePars <- NULL
      if (!is.null(dic)) {
            isStandard <- TRUE
            cube$mdArray <- dictionaryTransformForecast(dic, cube$mdArray)
      } else {
            isStandard <- FALSE
      }
      if (isTRUE(latLon$revLat)) {
            cube$mdArray <- revArrayLatDim(cube$mdArray, grid)
      }
      # formatting initialization dates
      runTimePars$runDates <- format(as.POSIXct(runTimePars$runDates, tz = "GMT"),
                                     format = "%Y-%m-%d %H:%M:%S", usetz = TRUE)
      # Static fields
      if (dic$time_step == "static") {
            runTimePars$runDates <- NA
            names(memberRangeList) <- NA
      }
      # Variable attributes -------
      Variable <- list("varName" = var, "level" = level)
      attr(Variable, "use_dictionary") <- isStandard
      attr(Variable, "description") <- grid$getDescription()
      if (isStandard) {
            vocabulary <- UDG.vocabulary()
            attr(Variable, "units") <- as.character(vocabulary[grep(paste0("^", var, "$"), vocabulary$identifier), 3])
            attr(Variable, "longname") <- as.character(vocabulary[grep(paste0("^", var, "$"), vocabulary$identifier), 2])
      } else {
            attr(Variable, "units") <- tryCatch(grid$getUnitsString(),
                                                error = function(err) {
                                                      err <- "undefined"
                                                      return(err)
                                                })
            attr(Variable, "longname") <- tryCatch(grid$getFullName(),
                                                   error = function(err) {
                                                         err <- "undefined"
                                                         return(err)
                                                })
      }
      attr(Variable, "daily_agg_cellfun") <- cube$foreTimePars$aggr.d
      attr(Variable, "monthly_agg_cellfun") <- cube$foreTimePars$aggr.m
      attr(Variable, "verification_time") <- time
      # Output list -------------------
      out <- list("Variable" = Variable,
                  "Data" = cube$mdArray,
                  "xyCoords" = latLon$xyCoords,
                  "Dates" = cube$foreTimePars$forecastDates,
                  "InitializationDates" = runTimePars$runDates,
                  "Members" = names(memberRangeList))
      gds$close()
      # Other attributes -----------------
      attr(out$xyCoords, "projection") <- proj$toString()
      x <- attr(out$Data, "dimensions")
      if (length(x) > 1) {
            tab <- c("member", "time", "level", "lat", "lon")
            b <- na.exclude(match(tab, x))
            dimNames <- attr(out$Data, "dimensions")[b]
            out$Data <- aperm(out$Data, perm = b)
            attr(out$Data, "dimensions") <- dimNames
      }
      attr(out, "dataset") <- dataset
      if (grepl("http://meteo\\.unican\\.es", dataset)) {
            attr(out, "source") <- "User Data Gateway"
            attr(out, "URL") <- "<http://meteo.unican.es/udg-wiki>"
      }
      message("[", Sys.time(), "]", " Done")
      return(out)
}
