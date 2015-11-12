# loadGridData.R Load a user-defined spatio-temporal slice from a gridded dataset
#
#     Copyright (C) 2015 Santander Meteorology Group (http://www.meteo.unican.es)
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


#' @title Load a field from a dataset
#' 
#' @description Load a user-defined spatio-temporal slice from a gridded dataset
#' 
#' @import rJava
#' 
#' @template templateParams
#' @param dictionary Default to TRUE, indicating that a dictionary is used and the .dic file is stored in the same path than the
#' dataset. If the .dic file is stored elsewhere, then the argument is the full path to the .dic file (including the extension,
#' e.g.: \code{"/path/to/the/dictionary_file.dic"}). This is the case for instance when the dataset is stored in a remote URL,
#' and we have a locally stored dictionary for that particular dataset. If FALSE no variable homogenization takes place,
#' and the raw variable, as originally stored in the dataset, will be returned. See details for dictionary specification.
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
#' @family loading
#' @family loading.grid
#' 
#' @examples \dontrun{
#' #Download dataset
#' dir.create("mydirectory")
#' download.file("http://meteo.unican.es/work/downscaler/data/Iberia_NCEP.tar.gz", 
#' destfile = "mydirectory/Iberia_NCEP.tar.gz")
#' # Extract files from the tar.gz file
#' untar("mydirectory/NCEP_Iberia.tar.gz", exdir = "mydirectory")
#' # First, the path to the ncml file is defined:
#' ncep <- "mydirectory/Iberia_NCEP/Iberia_NCEP.ncml"
#' # Load air temperature at 850 millibar isobaric surface pressure level from the built-in
#' # NCEP dataset, for the Iberian Peninsula in summer (JJA):
#' field <- loadGridData(ncep, var = "ta@@850", dictionary = TRUE, lonLim = c(-10,5),
#'    latLim = c(35.5, 44.5), season = 6:8, years = 1981:2010)
#' str(field)   
#' plotMeanField(field)
#' # Calculation of monthly mean temperature:
#' field.mm <- loadGridData(ncep, var = "ta@@850", dictionary = TRUE, lonLim = c(-10,5),
#'                          latLim = c(35.5, 44.5), season = 6:8,
#'                          years = 1981:2010, aggr.m = "mean")
#' str(field.mm)
#' 
#' # Same but using the original variable (not homogenized via dictionary):
#' di <- dataInventory(ncep)
#' names(di)
#' showVocabulary()
#' # Variable is named 'T', instead of the standard name 'ta' in the vocabulary
#' # Vertical level is indicated using the '@@' symbol:
#' non.standard.field <- loadGridData(ncep, var = "T@@850", dictionary = FALSE, lonLim = c(-10,5),
#'                                   latLim = c(35.5, 44.5), season = 6:8, 
#'                                   years = 1981:2010, aggr.m = "mean")
#' str(non.standard.field$Variable)
#' # Note the units are now in Kelvin, as originally stored
#' plotMeanField(non.standard.field)
#' 
#' ## Example of data load from a remote repository via OPeNDAP (NASA dataserver)
#  Definition of the OPeNDAP URL of the dataset
#' ds <- "http://dataserver3.nccs.nasa.gov/thredds/dodsC/bypass/NEX-GDDP/bcsd/rcp85/r1i1p1/tasmax/MIROC-ESM.ncml"
#' # Monthly mean maximum summer 2m temperature at 12:00 UTC over the Iberian Peninsula:
#' # (CMIP5 MIROC-ESM model, RCP 8.5)
#' tasmax <- loadGridData(dataset = ds,
#'                        var = "tasmax",
#'                        dictionary = FALSE,
#'                        lonLim = c(-10,5),
#'                        latLim = c(35,44),
#'                        season = 6:8,
#'                        years = 2021,
#'                        time = "12",
#'                        aggr.m = "mean")
#' plotMeanField(tasmax)
#' }
#' 

loadGridData <- function(dataset, var, dictionary = TRUE, lonLim = NULL,
                         latLim = NULL, season = NULL, years = NULL, time = "none",
                         aggr.d = "none", aggr.m = "none") {
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
      # Dictionary lookup
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
      if (!is.null(season) && (min(season) < 1 | max(season) > 12)) {
            stop("Invalid season definition")
      }
      if (grepl("^http://", dataset)) {
            message("[", Sys.time(), "] ", "Opening connection with remote server...")
            gds <- tryCatch(expr = J("ucar.nc2.dt.grid.GridDataset")$open(dataset), error = function(e) {
                  if (grepl("return status=503", e)) {
                        stop("Service temporarily unavailable\nThe server is temporarily unable to service your request due to maintenance downtime or capacity problems, please try again later.")
                  } else if (grepl("return status=404", e)) {
                        stop(dataset," is not a valid URL)")
                  }
            })
            if (is.null(gds)) {
                  stop("Requested URL not found\nThe problem may be momentary.")      
            }
            message("[", Sys.time(), "] ", "Connected successfuly")
      } else {
            gds <- J("ucar.nc2.dt.grid.GridDataset")$open(dataset)
      }
      grid <- gds$findGridByShortName(shortName)
      if (is.null(grid)) {
            stop("Variable requested not found\nCheck 'dataInventory' output and/or dictionary 'identifier'.")
      }
      latLon <- getLatLonDomain(grid, lonLim, latLim)
      proj <- grid$getCoordinateSystem()$getProjection()
      if (!proj$isLatLon()) latLon <- adjustRCMgrid(gds, latLon, lonLim, latLim)
      out <- loadGridDataset(var, grid, dic, level, season, years, time, latLon, aggr.d, aggr.m)
      # Definition of projection
      proj <- proj$toString()
      attr(out$xyCoords, which = "projection") <- proj
      # Dimension ordering
      tab <- c("time", "level", "lat", "lon")
      x <- attr(out$Data, "dimensions")
      if (length(x) > 1) {
            b <- na.exclude(match(tab, x))
            dimNames <- attr(out$Data, "dimensions")[b]
            out$Data <- aperm(out$Data, perm = b)    
            attr(out$Data, "dimensions")  <- dimNames
      }
      # Source Dataset and other metadata 
      attr(out, "dataset") <- dataset
      gds$close()
      message("[",Sys.time(),"]", " Done")
      return(out)
}     
# End


#' @title RCM grid adjustment
#' @description Performs operations to adequately handle 2D XY axis (typically from RCMs)
#' @param gds a Java GridDataSet object
#' @param latLon latLon definition (see \code{\link{getLatLonDomain}})
#' @return a new latLon definition
#' @keywords internal
#' @author S. Herrera, M. Iturbide


adjustRCMgrid <- function(gds, latLon, lonLim, latLim) {
      nc <- gds$getNetcdfDataset()
      lonAxis <- nc$findVariable('lon')
      auxLon <- t(matrix(data = lonAxis$getCoordValues(), nrow = lonAxis$getShape()[2], ncol = lonAxis$getShape()[1]))
      latAxis <- nc$findVariable('lat')
      auxLat <- t(matrix(data = latAxis$getCoordValues(), nrow = latAxis$getShape()[2], ncol = latAxis$getShape()[1]))
      if (is.null(lonLim)){
            lonLim <- c(auxLon[arrayInd(which.min(auxLat), dim(auxLat))[1],
                               which.min(auxLon[arrayInd(which.min(auxLat),
                                                         dim(auxLat))[1],])],
                        auxLon[arrayInd(which.max(auxLat),
                                        dim(auxLat))[1],
                               which.max(auxLon[arrayInd(which.max(auxLat),
                                                         dim(auxLat))[1],])])
      }
      if (is.null(latLim)){
            latLim <- c(auxLat[arrayInd(which.min(auxLat), dim(auxLat))[1],
                               which.min(auxLon[arrayInd(which.min(auxLat),
                                                         dim(auxLat))[1],])],
                        auxLat[arrayInd(which.max(auxLat),
                                        dim(auxLat))[1],
                               which.max(auxLon[arrayInd(which.max(auxLat),
                                                         dim(auxLat))[1],])])
      }
      if (length(lonLim) == 1 | length(latLim) == 1) {
            ind.x <- which.min(abs(auxLon - lonLim))
            ind.y <- which.min(abs(auxLat - latLim))
            pointXYindex <- c(ind.y,ind.x)
            latLon$xyCoords$x <- grid$getCoordinateSystem()$getXHorizAxis()$getCoordValues()[ind.x]
            latLon$xyCoords$y <- grid$getCoordinateSystem()$getYHorizAxis()$getCoordValues()[ind.y]
            latLon$xyCoords$lon <- auxLon[ind.y,ind.x]
            latLon$xyCoords$lat <- auxLat[ind.y,ind.x]
      } else {
            auxDis <- sqrt((auxLon - lonLim[1])^2+(auxLat - latLim[1])^2)
            llrowCol <- arrayInd(which.min(auxDis), dim(auxDis))
            auxDis <- sqrt((auxLon - lonLim[2])^2+(auxLat - latLim[2])^2)
            urrowCol <- arrayInd(which.min(auxDis), dim(auxDis))
            auxDis <- sqrt((auxLon - lonLim[1])^2+(auxLat - latLim[2])^2)
            ulrowCol <- arrayInd(which.min(auxDis), dim(auxDis))
            auxDis <- sqrt((auxLon - lonLim[2])^2+(auxLat - latLim[1])^2)
            lrrowCol <- arrayInd(which.min(auxDis), dim(auxDis))
            llrowCol <- c(min(c(llrowCol[1],lrrowCol[1])), min(c(llrowCol[2],ulrowCol[2])))
            urrowCol <- c(max(c(ulrowCol[1],urrowCol[1])),max(c(lrrowCol[2],ulrowCol[2])))
            latLon$xyCoords$x <- grid$getCoordinateSystem()$getXHorizAxis()$getCoordValues()[llrowCol[2]:urrowCol[2]]
            latLon$xyCoords$y <- grid$getCoordinateSystem()$getYHorizAxis()$getCoordValues()[llrowCol[1]:urrowCol[1]]
            latLon$xyCoords$lon <- auxLon[llrowCol[1]:urrowCol[1],llrowCol[2]:urrowCol[2]]
            latLon$xyCoords$lat <- auxLat[llrowCol[1]:urrowCol[1],llrowCol[2]:urrowCol[2]]
      }
      latLon$lonRanges <- .jnew("ucar/ma2/Range", as.integer(llrowCol[2]-1), as.integer(urrowCol[2]-1))
      latLon$latRanges <- .jnew("ucar/ma2/Range", as.integer(llrowCol[1]-1), as.integer(urrowCol[1]-1))
      return(latLon)
}


#' Loads a user-defined subset of a gridded CDM dataset
#' 
#' Loads a user-defined subset from a gridded dataset compliant with the Common
#'  Data Model interface
#' 
#' @author J. Bedia 
#' @export
#' @keywords internal

loadGridDataset <- function(var, grid, dic, level, season, years, time, latLon, aggr.d, aggr.m) {
      timePars <- getTimeDomain(grid, dic, season, years, time, aggr.d, aggr.m)
      levelPars <- getVerticalLevelPars(grid, level)
      cube <- makeSubset(grid, timePars, levelPars, latLon)
      timePars <- NULL
      if (!is.null(dic)) {
            isStandard <- TRUE
            cube$mdArray <- dictionaryTransformGrid(dic, cube$timePars, cube$mdArray)
      } else {
            isStandard <- FALSE
      }
      if (isTRUE(latLon$revLat)) {
            cube$mdArray <- revArrayLatDim(cube$mdArray, grid)
      }
      Variable <- list("varName" = var, "level" = levelPars$level)
      attr(Variable, "use_dictionary") <- isStandard
      attr(Variable, "description") <- grid$getDescription()
      if (isStandard) {
            # data(vocabulary, envir = environment())
            vocabulary <- showVocabulary()
            attr(Variable, "units") <- as.character(vocabulary[grep(paste0("^", var, "$"), vocabulary$identifier,), 3])
            attr(Variable, "longname") <- as.character(vocabulary[grep(paste0("^", var, "$"), vocabulary$identifier,), 2])
      } else {
            attr(Variable, "units") <- grid$getUnitsString()
            attr(Variable, "longname") <- grid$getFullName()
      }
      attr(Variable, "daily_agg_cellfun") <- cube$timePars$aggr.d
      attr(Variable, "monthly_agg_cellfun") <- cube$timePars$aggr.m
      attr(Variable, "verification_time") <- time
      out <- list("Variable" = Variable, "Data" = cube$mdArray, "xyCoords" = latLon$xyCoords, "Dates" = adjustDates(cube$timePars))
      return(out)
      
}
# End


#' Adjust time/start dates of a loaded object
#' @param timePars Object containing the relevant time parameters
#' @return A list with dates (POSIXct) start and end, defining the interval [start, end)
#' @details Sub-daily information is displayed only in case of subdaily data
#' @author J Bedia 
#' @keywords internal

# timePars <- cube$timePars
adjustDates <- function(timePars) {
      interval <- 0
      if (timePars$aggr.m != "none") {
            mon.len <- sapply(timePars$dateSliceList, ndays)
            interval <- mon.len * 86400
      } else if (timePars$aggr.d != "none") {
            timePars$dateSliceList <- format(as.Date(substr(timePars$dateSliceList, 1, 10)), format = "%Y-%m-%d %H:%M:%S", usetz = TRUE) 
            interval <- 86400
      }
      formato <- ifelse(interval[1] == 0, "%Y-%m-%d %H:%M:%S", "%Y-%m-%d")
      dates.end <- format(as.POSIXct(as.POSIXlt(timePars$dateSliceList, tz = "GMT") + interval), format = formato, usetz = TRUE)
      dates.start <- format(as.POSIXct(as.POSIXlt(timePars$dateSliceList, tz = "GMT"), tz = "GMT"), format = formato, usetz = TRUE)
      return(list("start" = dates.start, "end" = dates.end))
}
# End


#' Calculate the number of days of the current month
#' @param d A date (character) in format YYYY-MM-DD...
#' @return The number of days of the current month
#' @references 
#' \url{http://stackoverflow.com/questions/6243088/find-out-the-number-of-days-of-a-month-in-r}
#' @export

ndays <- function(d) {
      as.difftime(tail((28:31)[which(!is.na(as.Date(paste0(substr(d, 1, 8), 28:31), '%Y-%m-%d')))], 1), units = "days")
}
#End


