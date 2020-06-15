# dataInventory.R Get an overview of the contents of a dataset
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

#' @title Dataset inventory
#' @description Function to provide a quick overview of a climate dataset
#'  (either stations or gridded data)
#' @param dataset A character string poiting to the target. Either a directory containing the data
#'  in the case of station data in standard ASCII format (see \code{\link{loadStationData}}) for details),
#'  or a target file (a NcML) in the case of other types of gridded data (reanalysis, gridded observations ...,
#'  see \code{\link{loadGridData}} for details).
#' @param return.stats Optional logical flag indicating if summary statistics of the dataset
#'  should be returned with the inventory. Only used for station data.
#' @return A list of components describing the variables and other characteristics of the target dataset.
#' @note The variable names returned correspond to the original names of the variables as stored in the dataset,
#' and not to the standard naming convention defined in the vocabulary.
#' @examples \dontrun{
#' dir.create("mydirectory")
#' download.file("http://meteo.unican.es/work/loadeR/data/VALUE_ECA_86_v2.tar.gz", 
#'               destfile = "mydirectory/VALUE_ECA_86_v2.tar.gz")
#' # Extract files from the tar.gz file
#' untar("mydirectory/VALUE_ECA_86_v2.tar.gz", exdir = "mydirectory")
#' # Data inventory
#' value <- "mydirectory/VALUE_ECA_86_v2"
#' di <- dataInventory(value)
#' str(di)
#' # To obtain summary statistics of the variables stored:
#' di.stats <- dataInventory(value, return.stats = TRUE)
#' print(di.stats$Summary.stats)
#' } 
#' @seealso \code{\link{stationInfo}} for a quick overview of available stations in station datasets.
#' @export
#' @author J Bedia 

dataInventory <- function(dataset, return.stats = FALSE) {
      if (dataset %in% suppressMessages(do.call("c", UDG.datasets()))) {
            lf <- list.files(file.path(find.package("climate4R.UDG")), pattern = "datasets.*.txt", full.names = TRUE)
            df <- lapply(lf, function(x) read.csv(x, stringsAsFactors = FALSE))
            df <- do.call("rbind", df)
            datasetind <- which(df[["name"]] == dataset)
            dataset <- df$url[datasetind][1]
      }
      rs <- return.stats
      message(paste("[", Sys.time(), "] Doing inventory ...", sep = ""))
      if (isTRUE(file.info(dataset)$isdir) | grepl("\\.zip$", dataset)) {
            out <- dataInventory.ASCII(dataset, rs)
      } else {
            gds <- tryCatch({openDataset(dataset)}, error = function(err) {NA})
            nc <- tryCatch({gds$getNetcdfDataset()}, error = function(err) {NA})
            grep.result <- tryCatch({grep("timeSeries",nc$getGlobalAttributes()$toString())}, error = function(err) {NULL})
            if(!is.na(gds)) gds$close()
            if (length(grep.result) == 0 | is.null(grep.result)) {
                  out <- dataInventory.NetCDF(dataset)
                  
            } else {
                  out <- dataInventory.NetCDF.ts(dataset)
            }
      }
      message(paste("[", Sys.time(), "] Done.", sep = ""))
      return(out)
}
# End


#' @title Data inventory of standard ASCII station datasets
#' @description Make data inventory from a station dataset in standard ASCII format
#' @param dataset path to the directory containng the dataset
#' @param rs Logical. return stats?
#' @return A data inventory 
#' @author J Bedia 
#' @keywords internal
#' @importFrom utils read.csv
#' @importFrom utils unzip


dataInventory.ASCII <- function(dataset, rs) {
      if (grepl("\\.zip$", dataset)) {
            dirContents <- unzip(dataset, list = TRUE)$Name
            stations.file <- grep("stations\\.", dirContents, ignore.case = TRUE, value = TRUE)
            vars.file <- grep("variables\\.", dirContents, ignore.case = TRUE, value = TRUE)
            if (any(grepl("MACOSX", stations.file))) {
                  stations.file <- stations.file[-grep("MACOSX", stations.file)]
            }
            if (any(grepl("MACOSX", vars.file))) {
                  vars.file <- vars.file[-grep("MACOSX", vars.file)]
            }
            isZip <- TRUE
            on.exit(closeAllConnections())
      } else {
            isZip <- FALSE
            dirContents <- list.files(dataset, full.names = TRUE)
      }
      # lf <- list.files(dataset, full.names = TRUE)
      if (isZip) {
            stations <- read.csv(unz(dataset, stations.file), strip.white = TRUE, stringsAsFactors = FALSE)
            vars <- read.csv(unz(dataset, vars.file), strip.white = TRUE, stringsAsFactors = FALSE)
      } else {
            stations <- read.csv(grep("stations", dirContents, ignore.case = TRUE, value = TRUE), 
                                 strip.white = TRUE, stringsAsFactors = FALSE)
            vars <- read.csv(grep("variables", dirContents, ignore.case = TRUE, value = TRUE),
                             strip.white = TRUE, colClasses = "character")      
      }
      # Var info 
      var.info <- list("variable" = vars[ ,grep("variable", names(vars), ignore.case = TRUE)],
                       "longname" = vars[ ,grep("longname", names(vars), ignore.case = TRUE)],
                       "unit" = vars[ ,grep("unit", names(vars), ignore.case = TRUE)],
                       "missing.code" = vars[ ,grep("missing_code", names(vars), ignore.case = TRUE)])
      var.info <- do.call("cbind.data.frame", var.info)
      # Station info
      timeString <- if (isZip) {
            dirind <- grep(paste0(vars[ ,1][1], "\\.*"), dirContents, value = TRUE)
            if (any(grepl("MACOSX", dirind))) dirind <- dirind[-grep("MACOSX", dirind)]
            read.csv(unz(dataset, dirind), colClasses = "character")[ ,1]
      } else {
            read.csv(grep(paste0(vars[ ,1][1], "\\.*"), dirContents, value = TRUE), colClasses = "character")[ ,1]
      }
      timeDates <- if (nchar(timeString[1]) == 8) {
            strptime(timeString, "%Y%m%d")  
      } else {
            strptime(timeString, "%Y%m%d%H")
      }
      timeString <- NULL
      timeAxis <- list("startDate" = min(timeDates),
                       "endDate" = max(timeDates),
                       "timeStep" = difftime(timeDates[2], timeDates[1], units = "h"))    
      timeDates <- NULL
      # station_id <- as.character(stations[ ,grep("station_id", names(stations), ignore.case = TRUE)])
      
      if (isZip) {
            dirind <- grep("stations", dirContents, ignore.case = TRUE, value = TRUE)
            if (any(grepl("MACOSX", dirind))) dirind <- dirind[-grep("MACOSX", dirind)]
            aux <- read.csv(unz(dataset, dirind),
                            strip.white = TRUE,
                            stringsAsFactors = FALSE, colClasses = "character")
      } else {
            aux <- read.csv(grep("stations", dirContents, ignore.case = TRUE, value = TRUE),
                      strip.white = TRUE,
                      stringsAsFactors = FALSE, colClasses = "character")
      }
      station_id <- aux[ ,grep("station_id", names(aux), ignore.case = TRUE)]
      aux <- NULL
      lon <- stations[ ,grep("^longitude$", names(stations), ignore.case = TRUE)]
      lat <- stations[ ,grep("^latitude$", names(stations), ignore.case = TRUE)]
      LonLatCoords <- cbind(lon, lat)
      rownames(LonLatCoords) <- station_id
      rm(list = c("lon", "lat"))
      other.metadata <- as.list(stations[ ,-pmatch(c("station_id", "longitude", "latitude"), names(stations))])
      station.info <- list("station_id" = station_id, "LonLatCoords" = LonLatCoords, "times" = timeAxis, "other.metadata" = other.metadata)
      station_id  <- timeAxis <- LonLatCoords <- other.metadata <- NULL
      info <- list("Stations" = station.info, "Variables" = var.info, "Summary.stats" = NULL)
      if (isTRUE(rs)) {
            na.perc <- function(x) {round(length(which(is.na(x))) * 100 / length(x), 1)}
            aux.mat <- matrix(ncol = nrow(vars), nrow = length(station.info$station_id), dimnames = list(station.info$station_id, vars[ ,grep("variable", names(vars), ignore.case = TRUE)]))
            stats.list <- list("missing.percent" = aux.mat, "min" = aux.mat, "max" = aux.mat, "mean" = aux.mat)
            lf <- list.files(dataset)
            lff <- list.files(dataset, full.names = TRUE)
            for (i in 1:nrow(var.info)) {
                  varfile <- lff[grep(paste0("^", var.info[i,1], "\\."), lf)]
                  var <- read.csv(varfile, na.strings = var.info$missing.code[i])[ ,-1]
                  stats.list[[1]][ ,i] <- apply(var, 2, na.perc)
                  stats.list[[2]][ ,i] <- apply(var, 2, min, na.rm = TRUE)
                  stats.list[[3]][ ,i] <- apply(var, 2, max, na.rm = TRUE)
                  stats.list[[4]][ ,i] <- apply(var, 2, mean, na.rm = TRUE)
                  var <- NULL
            }
            info <- list("Stations" = station.info, "Variables" = var.info, "Summary.stats" = stats.list)
      }
      return(info)
}
# End


#' @title Inventory of a Time Series dataset
#' @description Returns a list with summary information about the variables stored in a time series dataset.
#' Sub-routine of \code{dataInventory}
#' @param dataset A full path to the file describing the dataset (NcML)
#' @return A (named) list whose length is determined by the number of variables stored in the dataset,
#' its names corresponding to the short names of the variables.
#' For each variable, information on the variable long name, data type, units, data size (in Mb) and
#' characteristics of its dimensions is provided.
#' @author S. Herrera 
#' @keywords internal
#' @importFrom rJava J

dataInventory.NetCDF.ts <- function(dataset) {
      gds <- openDataset(dataset)
      nc <- gds$getNetcdfDataset()
      listString <- strsplit(nc$getVariables()$toString(), ";")
      varNames <-NULL
      for (l in c(1:length(listString[[1]]))){
            if ((length(grep("time",listString[[1]][l])) > 0) & (length(grep("station_id",listString[[1]][l])) > 0)){
                  auxString <- gsub("\\[double |]|\\\\s", "",strsplit(listString[[1]][l],"\\(")[[1]][1])
                  varNames <- c(varNames,auxString)
            }
      }
      if (length(varNames) == 0) {
            stop("No variables found", call. = FALSE)
      } else {
            var.list <- vector(mode = "list", length = length(varNames))   
            for (i in 1:length(varNames)) {
                  message("[", Sys.time(), "] Retrieving info for \'", varNames[i], "\' (", length(varNames) - i, " vars remaining)")
                  datavar <- gds$getDataVariable(varNames[i])
                  description <- datavar$getDescription()
                  varName <- datavar$getShortName()
                  version <- tryCatch({trimws(datavar$findAttribute("version")$getValues()$toString())}, error = function(e){NA})
                  dataType <- datavar$getDataType()$toString()
                  element.size <- datavar$getDataType()$getSize()
                  data.number <- datavar$getSize()
                  dataSize <- element.size * data.number * 1e-06
                  units <- datavar$getUnitsString()
                  shape <- datavar$getShape()
                  grid <- nc$findVariable(varName)
                  gridShape <- grid$getShape()
                  dim.list <- list()
                  
                  length(dim.list) <- length(gridShape) + 2
                  
                  gcs <- nc$getDimensions()
                  dim.Names <- NULL
                  auxList <- strsplit(gsub("\\[|]|\\\\s", "",gcs$toString()),";")
                  for (l in c(1:length(auxList[[1]]))){
                        aux <- strsplit(gsub("\\, |]|\\\\s", "",auxList[[1]][l])," = ")
                        dim.Names <- c(dim.Names,aux[[1]][1])
                  }
                  if (any(dim.Names == "time")) {
                        tDimIndex <- which(dim.Names == "time")
                        axis <- nc$findCoordinateAxis("time")
                        date.range <- gds$getCalendarDateRange()
                        if (!is.null(date.range)){
                              date.range <- gds$getCalendarDateRange()$toString()
                        }
                        tdim.name <- "time"
                        time.agg <- nc$getAggregation()
                        dim.list[[tDimIndex]] <- list("Type" = axis$getAxisType()$toString(),
                                                      "Units" = axis$getUnitsString(),
                                                      "Values" = axis$getCoordValues())
                        names(dim.list)[tDimIndex] <- "time"
                  }
                  if (any(dim.Names == "station_id")) {
                        sDimIndex <- which(dim.Names == "station_id")
                        axis <- nc$findCoordinateAxis("station_id")
                        axis.var <- nc$findVariable("station_id")
                        sdim.name <- "station_id"
                        dim.list[[sDimIndex]] <- list("Type" = "Station Name",
                                                      "Units" = axis$getUnitsString(),
                                                      "Values" = axis.var$read())
                        names(dim.list)[sDimIndex] <- "station_id"
                  }
                  xDimIndex <- length(dim.Names) + 1
                  if (!is.null(nc$findVariable("lon"))){
                        axis.var <- nc$findVariable("lon")
                  } else {
                        axis.var <- nc$findVariable("x")
                  }
                  xdim.name <- axis.var$getName()
                  dim.list[[xDimIndex]] <- list("Type" = axis.var$getDescription(),
                                                "Units" = axis.var$getUnitsString(),
                                                "Values" = axis.var$read()$copyTo1DJavaArray())
                  names(dim.list)[xDimIndex] <- "longitude"
                  yDimIndex <- length(dim.Names) + 2
                  if (!is.null(nc$findVariable("lat"))){
                        axis.var <- nc$findVariable("lat")
                  } else {
                        axis.var <- nc$findVariable("y")
                  }
                  ydim.name <- axis.var$getName()
                  dim.list[[yDimIndex]] <- list("Type" = axis.var$getDescription(),
                                                "Units" = axis.var$getUnitsString(),
                                                "Values" = axis.var$read()$copyTo1DJavaArray())
                  names(dim.list)[yDimIndex] <- "latitude"
                  
                  var.list[[i]] <- list("Description" = description,
                                        "DataType" = dataType,
                                        "Shape" = shape,
                                        "Units" = units,
                                        "DataSizeMb" = dataSize,
                                        "Version" = version,
                                        "Dimensions" = dim.list)
            }
            names(var.list) <- varNames
      }
      gds$close()
      return(var.list)
}
# end


#' @title Retrieve station info
#' @description Get a quick overview of the stations contained in a stations dataset
#' @param dataset A character string indicating the database to be accessed. For station data in standard ASCII format,
#' this is the path to the directory the dataset lives in.
#' @param plot Logical indicating if a map displaying the station locations should be displayed. Default to TRUE.
#' @return A data.frame with the station codes in its first column, followed by longitude and latitude,
#'  and the rest of metadata (if any) in the following columns. If \code{plot = TRUE}, also a map of the station locations.
#' @note For an adequate map display the station coordinates must be in decimal degrees.
#' @seealso \code{\link{dataInventory}} to obtain a more exhaustive report the dataset.
#' @export
#' @author J. Bedia 
#' 
#' @importFrom graphics lines
#' @importFrom graphics text
#' 
#' @examples \donttest{
#' dir.create("mydirectory")
#' download.file("http://meteo.unican.es/work/loadeR/data/VALUE_ECA_86_v2.tar.gz", 
#'               destfile = "mydirectory/VALUE_ECA_86_v2.tar.gz")
#' # Extract files from the tar.gz file
#' untar("mydirectory/VALUE_ECA_86_v2.tar.gz", exdir = "mydirectory")
#' # Data inventory
#' value <- "mydirectory/VALUE_ECA_86_v2"
#' print(stationInfo(value))
#' }
#' 

stationInfo <- function(dataset, plot = TRUE) {
      di <- dataInventory(dataset, return.stats = FALSE)
      ll <- di$Stations$LonLatCoords
      l <- lapply(1:length(di$Stations$other.metadata), function(x) {di$Stations$other.metadata[[x]]})
      df <- do.call("cbind.data.frame", l)
      l <- NULL
      names(df) <- names(di$Stations$other.metadata)
      stids <- di$Stations$station_id
      df <- cbind.data.frame("stationID" = stids,"longitude" = ll[ ,1], "latitude" = ll[ ,2], df)
      rownames(df) <- NULL
      if (isTRUE(plot)) {
            x.off <- diff(range(ll[ ,1]))*.3
            y.off <- diff(range(ll[ ,2]))*.3
            x.ran <- c(min(ll[ ,1]) - x.off, max(ll[ ,1]) + x.off)
            y.ran <- c(min(ll[ ,2]) - y.off, max(ll[ ,2]) + y.off)
            plot(ll, asp = 1, xlim = x.ran, ylim = y.ran, col = "blue", pch = 10)
            load(file.path(find.package("loadeR"), "wrl.Rda"))
            for (i in 1:length(node.list)) {
                  lines(node.list[[i]][,1], node.list[[i]][,2])            
            }
            text(x = ll[,1], y = ll[,2], labels = stids, pos = 3, cex = .7, col = "red")
      }
      return(df)
}
# End



