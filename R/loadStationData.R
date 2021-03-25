#     loadStationData.R Load station data in standard ASCII format
#
#     Copyright (C) 2019 Santander Meteorology Group (http://www.meteo.unican.es)
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
#' output object, providing geo-referencing information for more advanced spatial operations/conversions.
#' Preferable, this should be indicated in the form of CRS character string following the
#'  \href{http://trac.osgeo.org/proj/}{PROJ.4 specifications}. Default is set to WGS84 lon-lat coordinates.
#' @param units Optional (but strongly advised when possible). This is a character string indicating the units
#' of the variable. This should be whenever possible compatible with the udunits valid (\dQuote{parseable})
#'  definitions. See \code{\link[climate4R.UDG]{C4R.vocabulary}} for examples of standard unit string definitions.
#' @param level Optional character of the atmospheric level. This information about the variable will be 
#' included in the output grid. Requires knowledge about the dataset.
#' @param spatialTolerance Numeric. The use of this argument is NOT RECOMMENDED. Distance 
#' (in grid coordinate units) out of the lonLim and LatLim ranges that is allowed for data retrieving.
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
#' @references 
#' \url{https://github.com/SantanderMetGroup/loadeR/wiki/Standard-(ASCII)-format-for-station-data} 
#' 
#' @export 
#' @author J. Bedia
#' 
#' 
#' @family loading
#' @examples \dontrun{
#' ## This example is further illustrated in the loadeR's wiki at:
#' ## <https://github.com/SantanderMetGroup/loadeR/wiki/Accessing-and-loading-station-data>
#' 
#' # Download the VALUE-ECA-86 dataset (https://rmets.onlinelibrary.wiley.com/doi/10.1002/joc.5462) as a temp file
#' 
#' value <- tempfile(fileext = ".zip")
#' download.file("www.value-cost.eu/sites/default/files/VALUE_ECA_86_v2.zip", destfile = value)
#' stationInfo(value)
#' example <- loadStationData(dataset = value, 
#'                            var = "tmax", 
#'                            stationID = c("000234", "003946"), 
#'                            season = 6:8,
#'                            years = 1981:2000)
#' }

loadStationData <- function(dataset, 
                            var, 
                            stationID = NULL, 
                            lonLim = NULL, 
                            latLim = NULL, 
                            season = NULL, 
                            years = NULL, 
                            tz = "", 
                            projection = "+proj=longlat +init=epsg:4326 +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                            units = NULL,
                            level = NULL,
                            spatialTolerance = NULL) {
  if (!is.null(spatialTolerance)) {
    warning("Argument spatialTolerance not implemeted yet. Ignored")
  }
  aux <- NULL
  empty.area <- FALSE
  if (grepl("\\.zip$", dataset)) {
    unzcond <- unz
    zipFileContents <- unzip(dataset, list = TRUE)$Name
  } else if (grepl("\\.ncml$|\\.nc$|\\.nc4$", dataset)) {
    gds <- openDataset(dataset)
    nc <- gds$getNetcdfDataset()
    if (grep("timeSeries",nc$getGlobalAttributes()$toString())){
      varId <- nc$findVariable(var)
      units <- varId$getUnitsString()
    }else{
      stop("The dataset does not content timeSeries.\nCheck the global attribute 'featureType'.", call. = FALSE)
    }
  } else {
    unzcond <- function(description, filename) {
      paste0(description, "/", filename)
    }
    zipFileContents <- list.files(dataset)
  }
  if ((!is.null(lonLim) | !is.null(latLim)) & !is.null(stationID)) { 
    lonLim <- NULL 
    latLim <- NULL
    warning("lonLim/latLim arguments ignored as Station Codes have been specified.", call. = FALSE)
  }
  if (grepl("\\.ncml$|\\.nc$|\\.nc4$", dataset)) {
    stids <- nc$findVariable("station_id")
    stids <- stids$read()
    stids <- tryCatch({stids$make1DStringArray()}, error = function(err) {stids})
    stids <- stids$copyToNDJavaArray()
  } else {
    # Reading stations from zip file
    stations.file <- grep("stations\\.", zipFileContents, ignore.case = TRUE, value = TRUE)
    if (any(grepl("MACOSX", stations.file))) {
      stations.file <- stations.file[-grep("MACOSX", stations.file)]
    }      
    aux <- read.csv(unzcond(description = dataset, filename = stations.file), stringsAsFactors = FALSE, strip.white = TRUE)
    # Station codes
    trim <- function(x) gsub("^\\s+|\\s+$", "", x)
    stids <- trim(read.csv(unzcond(dataset, stations.file), colClasses = "character")[ ,grep("station_id", names(aux), ignore.case = TRUE)])
  }
  if (!is.null(stationID)) {
    stInd <- match(stationID, stids)
    if (any(is.na(stInd))) {
      stop("'stationID' values not found.\nCheck data inventory", call. = FALSE)
    }
  } else {
    stInd <- 1:length(stids)
  }
  ## Longitude and latitude
  if (grepl("\\.ncml$|\\.nc$|\\.nc4$", dataset)) {
    lons <- nc$findVariable("lon")
    if (is.null(lons)) lons <- nc$findVariable("Lon")
    if (is.null(lons)) lons <- nc$findVariable("x")
    lons <- lons$read()
    if (lons$getSize() < 2){
      lons <- lons$copyTo1DJavaArray()
    }else{
      lons <- lons$copyToNDJavaArray()
    }
    lats <- nc$findVariable("lat")
    if (is.null(lats)) lats <- nc$findVariable("Lat")
    if (is.null(lats)) lats <- nc$findVariable("y")
    lats <- lats$read()
    if (lats$getSize() < 2){
      lats <- lats$copyTo1DJavaArray()
    }else{
      lats <- lats$copyToNDJavaArray()
    }
  }else{
    lons <- aux[ , grep("^longitude$", names(aux), ignore.case = TRUE)]
    lats <- aux[ , grep("^latitude$", names(aux), ignore.case = TRUE)]
  }
  if (!is.null(lonLim) | !is.null(latLim)) {
    if (is.null(lonLim)) lonLim <- range(lons)
    if (is.null(latLim)) latLim <- range(lats)
    latLon <- getLatLonDomainStations(lonLim, latLim, lons, lats)
    if (length(latLon$stInd) == 0) {
      empty.area <- TRUE
      warning("No stations were found in the selected spatial domain", call. = FALSE)
    } else {
      stInd <- latLon$stInd
      coords <- setNames(data.frame(latLon$stCoords), nm = c("x", "y"))
      latLon <- NULL
    }
  } else {
    coords <- setNames(data.frame(matrix(cbind(lons, lats)[stInd, ], ncol = 2)), nm = c("x", "y"))
  }
  if (empty.area) {
    out <- NULL
  } else {
    stids <- stids[stInd]
    ## Spatial dimension
    attr(coords, "projection") <- projection
    attr(coords, "resX") <- 0
    attr(coords, "resY") <- 0
    ## Time dimension
    if (grepl("\\.ncml$|\\.nc$|\\.nc4$", dataset)) {
      timeId <- nc$findVariable("time")
      timeDates <- timeId$getCoordValues()
      refDate <- timeId$getUnitsString()
      auxDate <- strsplit(refDate,' ')
      timeDates <- strptime(as.Date(paste(auxDate[[1]][3],auxDate[[1]][4])) + timeDates, "%Y-%m-%d", tz = tz) 
    }else{
      fileInd <- grep(paste(var, "\\.txt", sep = ""), zipFileContents)
      if (any(grepl("MACOSX", zipFileContents[fileInd]))) {
        fileInd <- fileInd[-grep("MACOSX", zipFileContents[fileInd])]
      } 
      if (length(fileInd) == 0) {
        stop("[", Sys.time(),"] Variable requested not found", call. = FALSE)
      }
      timeString <- read.csv(unzcond(dataset, zipFileContents[fileInd]), colClasses = "character")[ ,1]
      timeDates <- string2date(timeString, tz = tz)
      timeString <- NULL
    }
    timePars <- getTimeDomainStations(timeDates, season, years)
    if(grepl("\\.ncml$|\\.nc$|\\.nc4$", dataset)) {
      # varId <- varId$read()
      # Data <- varId$toString()
      # Data <- t(matrix(as.double(strsplit(Data,' ')[[1]]), nrow = varId$getShape()[2], ncol = varId$getShape()[1]))
      # Data <- unname(Data[timePars$timeInd, stInd])
      # gds$close()
      aux.dimensions <- strsplit(varId$getDimensions()$toString(), ",")
      timeIndex <- which(grepl("time",aux.dimensions[[1]]))
      locIndex <- which(grepl("station_id",aux.dimensions[[1]]))
      if (timeIndex > locIndex){
        varId <- varId$read(paste0(range(stInd)[1]-1,":",range(stInd)[2]-1,":1,",range(timePars$timeInd)[1]-1,":",range(timePars$timeInd)[2]-1,":1"));
        Data <- varId$toString()
        Data <- t(matrix(as.double(strsplit(Data,' ')[[1]]), nrow = varId$getShape()[1], ncol = varId$getShape()[2]))
      }else{
        varId <- varId$read(paste0(range(timePars$timeInd)[1]-1,":",range(timePars$timeInd)[2]-1,":1,",range(stInd)[1]-1,":",range(stInd)[2]-1,":1"));
        Data <- varId$toString()
        Data <- t(matrix(as.double(strsplit(Data,' ')[[1]]), nrow = varId$getShape()[2], ncol = varId$getShape()[1]))
      }
      # Data <- varId$toString()
      # Data <- t(matrix(as.double(strsplit(Data,' ')[[1]]), nrow = varId$getShape()[2], ncol = varId$getShape()[1]))
      Data <- unname(Data[timePars$timeInd-(range(timePars$timeInd)[1]-1), stInd-(range(stInd)[1]-1)])
      gds$close()
    }else{
      ## missing data code
      varInd <- grep("variables", zipFileContents, ignore.case = TRUE)
      if (any(grepl("MACOSX", zipFileContents[varInd]))) {
        varInd <- varInd[-grep("MACOSX", zipFileContents[varInd])]
      } 
      vars <- read.csv(unzcond(dataset, zipFileContents[varInd]))
      miss.col <- grep("missing_code", names(vars), ignore.case = TRUE)
      unit.col <- grep("^unit", names(vars), ignore.case = TRUE)
      # Missing data value
      if (length(miss.col) > 0) {
        na.string <- vars[grep(var, vars[ , grep("variable", names(vars), ignore.case = TRUE)]), miss.col]
        miss.col <- NULL
      } else {
        na.string <- NA
      }
      # Level (fake slot)
      if (is.null(level)) {
        aux.level <- findVerticalLevel(var)
        level <- aux.level$level
      }
      # Units
      if (length(unit.col) > 0) {
        units.meta <- gsub(" ","", as.character(vars[grep(var, vars[ , grep("variable", names(vars), ignore.case = TRUE)]), unit.col]))
        if (!is.null(units)) {
          if (!identical(units, units.meta)) {
            warning("the dataset units metadata definition, (\'", units.meta,
                    "\') and the \'units\' value provided, (\'", units,
                    "\') differ.\nThe latter will be used.")
          }
        } else {
          units <- units.meta
        }
      } else {
        na.string <- NA
      }
      # Data retrieval
      message("[", Sys.time(), "] Loading data ...", sep = "")
      var.stids <- lapply(strsplit(readLines(unzcond(dataset, zipFileContents[fileInd]), 1), split = ","), FUN = trim)
      var.stids <- tail(unlist(var.stids), -1)
      closeAllConnections() 
      stInd.var <- match(stids, var.stids)
      Data <- unname(as.matrix(read.csv(unzcond(dataset, zipFileContents[fileInd]), na.strings = na.string)[timePars$timeInd, stInd.var + 1]))
    }
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
    # Units
    aux <- NULL  
    out <- list("Variable" = list("varName" = var, "level" = level), "Data" = Data, "xyCoords" = coords, "Dates" = timeBoundsValue(timePars$timeDates, tz), "Metadata" = meta.list)
    if (is.null(units)) message("NOTE: The \'units\' argument is undefined: It is highly recommended to indicate this attribute\nThis can be made afterwards with transformeR's function \'setGridUnits\'")
    datadimnames <- c("time", "loc")
    if (class(out$Data) == "numeric") datadimnames <- "time"
    attr(out$Data, "dimensions") <- datadimnames
    attr(out$Variable, "units") <- units
    attr(out, "dataset") <- dataset
    attr(out, "R_package_desc") <- paste0("loadeR-v", packageVersion("loadeR"))
    attr(out, "R_package_URL") <- "https://github.com/SantanderMetGroup/loadeR"
    attr(out, "R_package_ref") <- "https://doi.org/10.1016/j.envsoft.2018.09.009"
    if (grepl("http://meteo\\.unican\\.es", dataset)) {
      attr(out, "source") <- "User Data Gateway"
      attr(out, "URL") <- "<http://meteo.unican.es/trac/wiki/udg>"
    }
    message(paste("[", Sys.time(), "] Done.", sep = ""))
  }
  return(out)
}
# End      
