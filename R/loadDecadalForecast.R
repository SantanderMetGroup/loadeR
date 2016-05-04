# loadDecadalForecast.R Load a user-defined spatio-temporal slice from decadal forecasts
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

#' @title Load a grid from a decadal forecast
#' @description Load a user-defined spatio-temporal slice from decadal forecasts
#' @import rJava
#' @template templateParams
#' @param dictionary Default to TRUE, indicating that a dictionary is used and the .dic file is stored in the same path than the
#' dataset. If the .dic file is stored elsewhere, then the argument is the full path to the .dic file (including the extension,
#' e.g.: \code{"/path/to/the/dictionary_file.dic"}). This is the case for instance when the dataset is stored in a remote URL,
#' and we have a locally stored dictionary for that particular dataset. If FALSE no variable homogenization takes place,
#' and the raw variable, as originally stored in the dataset, will be returned. See details for dictionary specification.
#' @param members Vector of integers indicating the members to be loaded.
#' @param time A character vector indicating the temporal filtering/aggregation 
#' of the output data. Default to \code{"none"}, which returns the original time 
#' series as stored in the dataset. For sub-daily variables, instantantaneous data at 
#' selected verification times can be filtered using one of the character strings 
#' \code{"00"}, \code{"03"}, \code{"06"}, \code{"09"}, \code{"12"}, \code{"15"},
#'  \code{"18"}, \code{"21"},and \code{"00"} when applicable. If daily aggregated data are 
#' required use \code{"DD"}. If the requested variable is static (e.g. orography) it will be ignored. 
#' See the next arguments for time aggregation options.
#' @param aggr.d Character string. Function of aggregation of sub-daily data for daily data calculation. 
#' Currently accepted values are \code{"none"}, \code{"mean"}, \code{"min"}, \code{"max"} and \code{"sum"}.
#' @param aggr.m Same as \code{aggr.d}, bun indicating the aggregation function to compute monthly from daily data.
#' If \code{aggr.m = "none"} (the default), no monthly aggregation is undertaken.
#' 
#' @template templateReturnGridData
#' @template templateDicDetails  
#' @template templateGeolocation
#' @export
#' @author J. Bedia, S. Herrera, M. Iturbide, J.M. Gutierrez 
#' @family loading.grid
#'
#' @examples \dontrun{
#' latLim <- c(40,50)
#' lonLim <-  c(-5,10)
#' season <- 3:5
#' period <- 1981:1991
#' loginUDG(username = "myuser", password = "mypassword") 
#' tasDECA <- loadDecadalForecast(
#'    dataset = "http://www.meteo.unican.es/tds5/dodsC/specs/gfdl_specs_decadal.ncml", 
#'    latLim = latLim, 
#'    lonLim = lonLim,
#'    var = "tas", 
#'    years = period, 
#'    season = season)
#' }


loadDecadalForecast <- function(dataset,
                                var,
                                dictionary = FALSE, 
                                members = NULL,
                                lonLim = NULL,
                                latLim = NULL,
                                season = NULL,
                                years = NULL,
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
      if (dictionary == FALSE) {
            dic <- NULL
            shortName <- var
      } else {
            if (isTRUE(dictionary)) {
                  dicPath <- gsub("ncml$", "dic", dataset)
            }
            if (is.character(dictionary)) {
                  dicPath <- dictionary
            }
            dic <- dictionaryLookup(dicPath, var, time)
            shortName <- dic$short_name          
      }
      message("[", Sys.time(), "] ", "Opening connection with the UDG...")
      gds <- tryCatch(expr = {
            J("ucar.nc2.dt.grid.GridDataset")$open(dataset)
      }, error = function(e) {
            if (grepl("return status=503", e)) {
                  stop("UDG SERVICE TEMPORARILY UNAVAILABLE\nThe UDG server is temporarily unable to service your request due to maintenance downtime or capacity problems, please try again later.\n
                        If the problem persists after 24 h please drop a ticket (http://meteo.unican.es/trac/wiki/udg/ecoms)")
            } else if (grepl("Unauthorized to open dataset", e)) {
                  stop("UNAUTHORIZED TO OPEN DATASET\nPlease check your login details in loginECOMS_UDG function.\nIf you don\'t have a valid username/password or OpenID please the UDG Administration Panel (http://www.meteo.unican.es/udg-tap/login)")
            }
      })
      if (is.null(gds)) {
            stop("Requested URL not found\nThe problem may be momentary. Try again and if the error persists please drop a ticket (http://meteo.unican.es/trac/wiki/udg/ecoms)")      
      }
      message("[", Sys.time(), "] ", "Connected successfuly")
      grid <- gds$findGridByShortName(shortName)
      if (is.null(grid)) {
            stop("Variable requested not found\nCheck available variables at http://meteo.unican.es/trac/wiki/udg/ecoms/dataserver/catalog")
      }
      # Forecasts
      if (!is.null(members)) {
            members <- sort(members)
      }
      # latLon <- getLatLonDomainForecast(grid, lonLim, latLim)      
      latLon <- getLatLonDomain(grid, lonLim, latLim)      
      runTimePars <- getRunTimeDomain.decadal(dataset, grid, members, season, years)
      memberRangeList <- getMemberDomain(grid, dataset, members)
      foreTimePars <- getForecastTimeDomain(grid, dataset, dic, runTimePars, time, aggr.d, aggr.m)
      verticalPars <- getVerticalLevelPars(grid, level)
      cube <- makeSubset.decadal(grid, latLon, runTimePars, memberRangeList, foreTimePars, verticalPars)
      auxDates <- as.POSIXlt(cube$foreTimePars$forecastDates$start, tz = "GMT")
      indMonthValid <- which(is.element(auxDates$mon + 1,season))
      if (length(indMonthValid) < dim(cube$mdArray)[grep("^time$", attr(cube$mdArray,"dimensions"))]) {
            dimName <- attr(cube$mdArray, "dimensions")
            cube$mdArray <- cube$mdArray[,,indMonthValid,]
            attr(cube$mdArray, "dimensions") <- dimName
            cube$foreTimePars$forecastDates$start <- cube$foreTimePars$forecastDates$start[indMonthValid]
            cube$foreTimePars$forecastDates$end <- cube$foreTimePars$forecastDates$end[indMonthValid]
      }
      foreTimePars <- NULL
      if (!is.null(dic)) {
            isStandard <- TRUE
            cube$mdArray <- dictionaryTransformForecast(dic, cube$mdArray)
            # var <- derInterface$origVar
      } else {
            isStandard <- FALSE
      }
      if (isTRUE(latLon$revLat)) {
            cube$mdArray <- revArrayLatDim(cube$mdArray, grid)
      }
      # formatting initialization dates
      runTimePars$runDates <- format(as.POSIXct(runTimePars$runDates, tz = "GMT"), format = "%Y-%m-%d %H:%M:%S", usetz = TRUE)
      Variable <- list("varName" = var, "level" = level)
      attr(Variable, "is_standard") <- isStandard
      if (isStandard) {
            vocabulary <- UDG.vocabulary()
            attr(Variable, "units") <- as.character(vocabulary[grep(paste0("^", var, "$"), vocabulary$identifier), 3])
            attr(Variable, "longname") <- as.character(vocabulary[grep(paste0("^", var, "$"), vocabulary$identifier), 2])
      } else {
            attr(Variable, "units") <- grid$getUnitsString()
            attr(Variable, "longname") <- grid$getFullName()
      }
      attr(Variable, "daily_agg_cellfun") <- cube$foreTimePars$aggr.d
      attr(Variable, "monthly_agg_cellfun") <- cube$foreTimePars$aggr.m
      attr(Variable, "verification_time") <- time
      out <- list("Variable" = Variable,
                  "Data" = cube$mdArray,
                  "xyCoords" = latLon$xyCoords,
                  "Dates" = cube$foreTimePars$forecastDates,
                  "InitializationDates" = runTimePars$runDates,
                  "Members" = names(memberRangeList))
      
      gds$close()
      message("[", Sys.time(), "]", " Done")
      attr(out$xyCoords, "projection") <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
      # Dimension ordering
      x <- attr(out$Data, "dimensions")
      if (length(x) > 1) {
            tab <- c("member", "time", "level", "lat", "lon")
            b <- na.exclude(match(tab, x))
            dimNames <- attr(out$Data, "dimensions")[b]
            out$Data <- aperm(out$Data, perm = b)    
            attr(out$Data, "dimensions")  <- dimNames
      }
      # Source Dataset and other metadata 
      attr(out, "dataset") <- dataset
      attr(out, "source") <- "ECOMS User Data Gateway" 
      attr(out, "URL") <- "<http://meteo.unican.es/trac/wiki/udg/ecoms>"
      return(out)
}      
# End



