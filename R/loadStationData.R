# loadStationData.R Load station data in standard ASCII format
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

#' @title Load station data
#' @description Load observations data from station datasets in standard ASCII format.
#'
#' @template templateParams 
#' @param stationID Optional. A character vector indicating the code names of the stations to be loaded.
#' @param tz A time zone specification to be used for the conversion of dates, if one is required
#' (i.e., if the time zone of the dataset does not correspond to the system-specific one; see
#' \code{\link[base]{timezones}} for details). Default to unspecified (i.e. \code{tz = ""}).
#' @param projection Optional. Coordinate projection that is passed to attribute \code{projection} in the 
#' output object, providing geo-referencing information for more advanced spatial operations/conversions, 
#' in the form of a character string following the \href{http://trac.osgeo.org/proj/}{PROJ.4 specifications}. 
#' Default is set to the most common projection used for station data, this is, WGS84 longitude latitude coordinates.
#' 
#' @return a list with the following elements:
#' \itemize{
#' \item \code{Variable}. Name of the variable
#' \item \code{Data}. A 2-D matrix containing the data. Dates are ordered by rows and Stations by columns, 
#' following the order indicated in the \code{Metadata}.
#' \item \code{xyCoords}. A 2-D matrix with longitude and latitudes of the stations
#' \item \code{Dates}. A list with the verification time interval of each record in the time series.
#'  This is represented by a list with two elements: \code{start} and \code{end}, representing the
#'  lower and upper bounds of the verification period
#' \item \code{Metadata}. A list of variable length depending on the available metadata associated
#' to each observation. If no metadata are provided, at least the station codes (compulsory) are displayed.
#' }
#' 
#' @template templateGeolocation
#' 
#' @note Unlike gridded datasets, station data do not use a dictionary for variable homogenization. Thus, users
#' must take care of variable units and eventual conversions when necessary.
#' 
#' @importFrom utils unzip
#' @importFrom stats setNames
#' 
#' @references \url{https://github.com/SantanderMetGroup/downscaleR/wiki/Observation-Data-format} 
#' @export 
#' @author J. Bedia
#' 
#' 
#' @family loading

loadStationData <- function(dataset, 
                            var, 
                            stationID = NULL, 
                            lonLim = NULL, 
                            latLim = NULL, 
                            season = NULL, 
                            years = NULL, 
                            tz = "", 
                            projection = "+proj=longlat +init=epsg:4326 +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") {
      if (grepl("\\.zip$", dataset)) {
            unzcond <- unz
            zipFileContents <- unzip(dataset, list = TRUE)$Name
      } else {
            # unzcond <- function(...){params <- list(...); return(params$filename)}
            unzcond <- function(description, filename){paste0(description, "/", filename)}
            zipFileContents <- list.files(dataset)
      }
      if ((!is.null(lonLim) | !is.null(latLim)) & !is.null(stationID)) { 
            lonLim <- NULL 
            latLim <- NULL
            warning("lonLim/latLim arguments ignored as Station Codes have been specified.", call. = FALSE)
      }
      # Reading stations from zip file
      stations.file <- grep("stations\\.", zipFileContents, ignore.case = TRUE, value = TRUE)
      if (any(grepl("MACOSX", stations.file))) {
            stations.file <- stations.file[-grep("MACOSX", stations.file)]
      }      
      aux <- read.csv(unzcond(description = dataset, filename = stations.file), stringsAsFactors = FALSE, strip.white = TRUE)
      # Station codes
      stids <- read.csv(unzcond(dataset, stations.file), colClasses = "character")[ ,grep("station_id", names(aux), ignore.case = TRUE)]
      if (!is.null(stationID)) {
            stInd <- match(stationID, stids)
            if (any(is.na(stInd))) {
                  stop("'stationID' values not found.\nCheck data inventory", call. = FALSE)
            }
      } else {
            stInd <- 1:length(stids)
      }
      ## Longitude and latitude
      lons <- aux[ , grep("^longitude$", names(aux), ignore.case = TRUE)]
      lats <- aux[ , grep("^latitude$", names(aux), ignore.case = TRUE)]
      if (!is.null(lonLim) | !is.null(latLim)) {
            if (is.null(lonLim)) lonLim <- range(lons)
            if (is.null(latLim)) latLim <- range(lats)
            latLon <- getLatLonDomainStations(lonLim, latLim, lons, lats)
            if (length(latLon$stInd) == 0) {
                  stop("No stations were found in the selected spatial domain", call. = FALSE)
            }
            stInd <- latLon$stInd
            coords <- setNames(data.frame(latLon$stCoords), nm = c("x", "y"))
            latLon <- NULL
      } else {
             coords <- setNames(data.frame(matrix(cbind(lons, lats)[stInd, ], ncol = 2)), nm = c("x", "y"))
      }
      stids <- stids[stInd]
      ##############################################
      #dimnames(coords) <- list(stids, c("longitude", "latitude"))
      attr(coords, "projection") <- projection
      attr(coords, "resX") <- 0
      attr(coords, "resY") <- 0
      ################################################
      ## Time dimension
      # TODO - fix potential MACOSX errors
      fileInd <- grep(paste(var, "\\.txt", sep = ""), zipFileContents)
      if (length(fileInd) == 0) {
            stop("[", Sys.time(),"] Variable requested not found", call. = FALSE)
      }
      timeString <- read.csv(unzcond(dataset, zipFileContents[fileInd]), colClasses = "character")[ ,1]
      timeDates <- string2date(timeString, tz = tz)
      timeString <- NULL
      timePars <- getTimeDomainStations(timeDates, season, years)
      ## missing data code
      vars <- read.csv(unzcond(dataset, zipFileContents[grep("variables", zipFileContents, ignore.case = TRUE)]))
      miss.col <- grep("missing_code", names(vars), ignore.case = TRUE)
      if (length(miss.col) > 0) {
            na.string <- vars[grep(var, vars[ , grep("variable", names(vars), ignore.case = TRUE)]), miss.col]
            vars <- NULL
            miss.col <- NULL
      } else {
            na.string <- NA
      }
      # Data retrieval
      message("[", Sys.time(), "] Loading data ...", sep = "")
      trim <- function(x) gsub("^\\s+|\\s+$", "", x)
      var.stids <- lapply(strsplit(readLines(unzcond(dataset, zipFileContents[fileInd]), 1), split = ", "), FUN = trim)
      var.stids <- tail(unlist(var.stids), -1)
      closeAllConnections() 
      stInd.var <- match(stids, var.stids)
      Data <- unname(as.matrix(read.csv(unzcond(dataset, zipFileContents[fileInd]), na.strings = na.string)[timePars$timeInd, stInd.var + 1]))
      # Metadata
      message("[", Sys.time(), "] Retrieving metadata ...", sep = "")
      # Assumes that at least station ids must exist, and therefore meta.list is never empty
      ind.meta <- c(1:length(names(aux)))[-pmatch(c("longitude", "latitude", "station_id"), names(aux))]
      meta.list <- list()
      meta.list[[1]] <- stids
      for (i in 1:length(ind.meta)) {
            meta.list[[i + 1]] <- aux[stInd, ind.meta[i]]
      }
      names(meta.list) <- c("station_id", names(aux)[ind.meta])
      aux <- NULL  
      out <- list("Variable" = list("varName" = var), "Data" = Data, "xyCoords" = coords, "Dates" = timeBoundsValue(timePars$timeDates, tz), "Metadata" = meta.list)
      attr(out$Data, "dimensions") <- c("time", "loc")
      message(paste("[", Sys.time(), "] Done.", sep = ""))
      return(out)
}
# End      



#' @title POSIXlt conversion from character 
#' @description Converts the date codes of the Value format to \code{"POSIXlt"}
#' @param timeString Date vector as stored in VALUE files, previously coerced to character
#' @param tz Time zone. See \code{\link{loadStationData}}
#' @return A POSIXlt vector of the same length of the input
#' @details Currently the VALUE format is intended for daily data of the form YYYMMDD. However,
#'  the function also considers the possibility of subdaily data if hourly data are introduced in
#'  the form YYYYMMDDHH, eading to a string of 10 characters.
#'  @keywords internal
#'  @author juaco

string2date <- function(timeString, tz = tz) {
      timeString = gsub("^\\s+|\\s+$", "", timeString)
      if (nchar(timeString[1]) == 8) {
            timeDates <- strptime(timeString, "%Y%m%d", tz = tz)  
      }
      if (nchar(timeString[1]) == 10) {
            timeDates <- strptime(timeString, "%Y%m%d%H", tz = tz)
      }
      return(timeDates)
}
# End


#' @title Compute time bounds
#' @description Compute start/end verification time bounds from a vector of dates.
#' @param timeDates A POSIXlt vector of dates
#' @param tz Time zone
#' @keywords internal
#' @return A list with components start and end, of POSIXct dates

timeBoundsValue <- function(timeDates, tz) {
      varTimeStep <- difftime(timeDates[2], timeDates[1])
      dateSliceStart <- as.POSIXct(timeDates)
      dateSliceEnd <- as.POSIXct(as.POSIXlt(timeDates + varTimeStep))
      usetz <- ifelse(identical(tz, ""), FALSE, TRUE)
      dateSliceStart <- format.POSIXct(dateSliceStart, "%Y-%m-%d %H:%M:%S", usetz = usetz, tz = tz)
      dateSliceEnd <- format.POSIXct(dateSliceEnd, "%Y-%m-%d %H:%M:%S", usetz = usetz, tz = tz)
      return(list("start" = dateSliceStart, "end" = dateSliceEnd))
}
# End







