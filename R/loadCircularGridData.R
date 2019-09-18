#     loadCircualrGridData.R Load a user-defined spatio-temporal slice from a gridded dataset
#
#     Copyright (C) 2018 Santander Meteorology Group (http://www.meteo.unican.es)
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
#     along with this program. If not, see <http://www.gnu.org/licenses/>.


#' @title Load a grid from a circular gridded dataset
#' @description Load a user-defined spatio-temporal slice from a circular gridded dataset
#' @template templateParams
#' @param members A vector of integers indicating the members to be loaded.
#'  Default to \code{NULL}, which loads all available members if the dataset contains members (i.e. in case a Ensemble Axis is defined).
#'  For instance, \code{members=1:5} will retrieve the first five members of dataset.
#'   Note that unlike \code{\link{loadSeasonalForecast}}, discontinuous member selections (e.g. \code{members=c(1,5,7)}) are NOT allowed. 
#'   If the requested dataset has no Ensemble Axis (or it is a static field, e.g. orography) it will be ignored.
#' @param dictionary Default to FALSE, if TRUE a dictionary is used and the .dic file is stored in the same path than the
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
#' @param threshold Optional, only needed if absolute/relative frequencies are required. A float number defining the threshold
#'  used by  \code{condition} (the next argument).
#' @param condition Optional, only needed if absolute/relative frequencies are required. Inequality operator to be applied considering the given threshold.
#'  \code{"GT"} = greater than the value of \code{threshold}, \code{"GE"} = greater or equal,
#'   \code{"LT"} = lower than, \code{"LE"} = lower or equal than
#' @template templateReturnGridData
#' @template templateDicDetails  
#' @template templateTimeAggr
#' @template templateGeolocation
#' @export
#' @author S. Herrera
#' @family loading
#' @family loading.grid
#' 
#' @importFrom stats na.exclude
#' @importFrom utils packageVersion tail
#' @importFrom climate4R.UDG UDG.datasets



loadCircularGridData <- function(dataset,
                         var,
                         dictionary = FALSE,
                         lonLim = NULL,
                         latLim = NULL,
                         season = NULL,
                         years = NULL,
                         members = NULL,
                         time = "none",
                         aggr.d = "none",
                         aggr.m = "none",
                         condition = NULL,
                         threshold = NULL) {
  time <- match.arg(time, choices = c("none","00","03","06","09","12","15","18","21","DD"))
  aggr.d <- match.arg(aggr.d, choices = c("none", "mean", "min", "max", "sum"))
  if (time != "DD" & aggr.d != "none") {
    aggr.d <- "none"
    message("NOTE: Argument 'aggr.d' ignored as 'time' was set to ", time)
  }
  aggr.m <- match.arg(aggr.m, choices = c("none", "mean", "min", "max", "sum"))
  # Count aggregations
  if (!is.null(condition)) {
    condition <- match.arg(condition, choices = c("GT", "GE", "LT", "LE"))
    if (is.null(threshold)) {
      stop("A 'threshold' argument value is required given 'condition', with no default", call. = FALSE)
    }
    if (!is.numeric(threshold)) stop("Invalid non-numeric 'threshold' argument value", call. = FALSE)
    if (aggr.m == "none") stop("Invalid 'aggr.m' argument value given 'threshold' and 'condition'", call. = FALSE)
  }
  aux.level <- findVerticalLevel(var)
  var <- aux.level$var
  level <- aux.level$level
  # UDG public data parameters -----------------
  if (dataset %in% suppressMessages(do.call("c", UDG.datasets()))) {
        lf <- list.files(file.path(find.package("climate4R.UDG")), pattern = "datasets.*.txt", full.names = TRUE)
        df <- lapply(lf, function(x) read.csv(x, stringsAsFactors = FALSE))
        df <- do.call("rbind", df)
        datasetind <- which(df[["name"]] == dataset)
        dataset <- df$url[datasetind]
        dic.filename <- df$dic[datasetind]
        dictionary <- file.path(find.package("climate4R.UDG"), "dictionaries", dic.filename)
        message("NOTE: Accessing harmonized data from a public UDG dataset")
  }
  # Dictionary lookup -------------
  cd <- check.dictionary(dataset, var, dictionary, time)
  shortName <- cd$shortName
  dic <- cd$dic
  if (!is.null(season) && (min(season) < 1 | max(season) > 12)) {
    stop("Invalid season definition", call. = FALSE)
  }
  # Discover dataset -------------
  gds <- openDataset(dataset)
  grid <- gds$findGridByShortName(shortName)
  if (is.null(grid)) {
    stop("Variable requested not found\nCheck 'dataInventory' output and/or dictionary 'identifier'.", call. = FALSE)
  }
  # Spatial collocation -------------
  ## latLon <- getLatLonDomain(grid, lonLim, latLim)
  ###############################################
  ## adjustRCMgrid <- function(gds, latLon, lonLim, latLim) {
  nc <- gds$getNetcdfDataset()
  lonAxis <- nc$findVariable('lon')
  auxLon <- t(matrix(data = lonAxis$getCoordValues(),
                     nrow = lonAxis$getShape()[2],
                     ncol = lonAxis$getShape()[1]))
  latAxis <- nc$findVariable('lat')
  auxLat <- t(matrix(data = latAxis$getCoordValues(),
                     nrow = latAxis$getShape()[2],
                     ncol = latAxis$getShape()[1]))
  if (is.null(lonLim)) {
    lonLim <- c(min(auxLon),max(auxLon))
  }
  if (is.null(latLim)) {
    latLim <- c(min(auxLat),max(auxLat))
  }
  deltaLon <- lonLim[2] - lonLim[1]
  deltaLat <- latLim[2] - latLim[1]
  if (length(lonLim) == 1 | length(latLim) == 1) {
    ind.x <- which.min(abs(auxLon - lonLim))
    ind.y <- which.min(abs(auxLat - latLim))
    pointXYindex <- c(ind.y,ind.x)
  } else {
    pointXYindex <- c(-1L, -1L)
    auxInd <- intersect(which(auxLat >= latLim[1] & auxLat <= latLim[2]), which(auxLon >= lonLim[1] & auxLon <= lonLim[2]))
    auxInd <- arrayInd(auxInd, dim(auxLat))
    llrowCol <- c(min(auxInd[,1]), min(auxInd[,2]))
    urrowCol <- c(max(auxInd[,1]), max(auxInd[,2]))
    ulrowCol <- c(max(auxInd[,1]), min(auxInd[,2]))
    lrrowCol <- c(min(auxInd[,1]), max(auxInd[,2]))
    llrowCol <- c(min(c(llrowCol[1],lrrowCol[1])), min(c(llrowCol[2],ulrowCol[2])))
    urrowCol <- c(max(c(ulrowCol[1],urrowCol[1])),max(c(lrrowCol[2],urrowCol[2])))
    ind.x <- c(llrowCol[2]:urrowCol[2])
    ind.y <- c(llrowCol[1]:urrowCol[1])
  }
  xSlice <- nc$findCoordinateAxis('rlon')$getCoordValues()[ind.x]
  ySlice <- nc$findCoordinateAxis('rlat')$getCoordValues()[ind.y]
  lonSlice <- auxLon[ind.y,ind.x]
  latSlice <- auxLat[ind.y,ind.x]
  gcs <- grid$getCoordinateSystem()
  bboxDataset <- gcs$getLatLonBoundingBox()
  spec <- .jnew("java/lang/String", paste(latLim[1], lonLim[1], deltaLat, deltaLon, sep = ", "))
  bboxRequest <- .jnew("ucar/unidata/geoloc/LatLonRect", spec)
  llbbox <- list()
  llbbox[[1]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec)
  llRanges <- list()
  lonRanges  <- list()
  latRanges  <- list()
  lonRanges[[1]] <- .jnew("ucar/ma2/Range", as.integer(llrowCol[2]-1), as.integer(urrowCol[2]-1))
  latRanges[[1]] <- .jnew("ucar/ma2/Range", as.integer(llrowCol[1]-1), as.integer(urrowCol[1]-1))
  revLat <- FALSE
  latLon <- list("llRanges" = llRanges, "llbbox" = llbbox, "pointXYindex" = pointXYindex, "xyCoords" = list("x" = xSlice, "y" = ySlice, "lon" = lonSlice, "lat" = latSlice), "revLat" = revLat)
  ## aux <- .jnew("java.util.ArrayList")
  ## aux$add(latRanges[[1]])
  ## aux$add(lonRanges[[1]])
  ## llRanges <- aux
  latLon$llRanges <- lapply(1:length(lonRanges), function(x) {
    aux <- .jnew("java.util.ArrayList")
    aux$add(latRanges[[x]])
    aux$add(lonRanges[[x]])
    aux
  })
  
  ###############################################
  proj <- grid$getCoordinateSystem()$getProjection()
  # Read data -------------------
  out <- loadGridDataset(var, grid, dic, level, season, years, members,
                         time, latLon, aggr.d, aggr.m, threshold, condition)
  # Metadata: projection and spatial resolution -------------
  proj <- proj$toString()
  attr(out$xyCoords, which = "projection") <- proj
  attr(out$xyCoords, "resX") <- (tail(out$xyCoords$x, 1) - out$xyCoords$x[1]) / (length(out$xyCoords$x) - 1)
  attr(out$xyCoords, "resY") <- (tail(out$xyCoords$y, 1) - out$xyCoords$y[1]) / (length(out$xyCoords$y) - 1)
  if ("lon" %in% names(out$xyCoords)) {
    attr(out$xyCoords, "resLON") <- NA 
    attr(out$xyCoords, "resLAT") <- NA
  } 
  gds$close()
  # Dimension ordering -------------
  tab <- c("member", "time", "level", "lat", "lon")
  x <- attr(out$Data, "dimensions")
  if (length(x) > 1) {
    b <- na.exclude(match(tab, x))
    dimNames <- x[b]
    out$Data <- aperm(out$Data, perm = b)    
    attr(out$Data, "dimensions")  <- dimNames
  }
  # Member attributes -----------------------------
  if (!is.null(members)) {
    out$Members <- paste0("Member_", members)
    inits <- vector("list", length(members))
    names(inits) <- out$Members
    out$InitializationDates <- inits
  }
  # Source Dataset and other metadata -------------
  attr(out, "dataset") <- dataset
  attr(out, "R_package_desc") <- paste0("loadeR-v", packageVersion("loadeR"))
  attr(out, "R_package_URL") <- "https://github.com/SantanderMetGroup/loadeR"
  attr(out, "R_package_ref") <- "http://dx.doi.org/10.1016/j.cliser.2017.07.001"
  if (grepl("http://meteo\\.unican\\.es", dataset)) {
    attr(out, "source") <- "User Data Gateway"
    attr(out, "URL") <- "<http://meteo.unican.es/trac/wiki/udg>"
  }
  message("[",Sys.time(),"]", " Done")
  return(out)
}     
# End



