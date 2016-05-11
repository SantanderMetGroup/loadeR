#' @title Determine the geo-location parameters of an arbitrary user selection
#'
#' @description The function uses the \sQuote{GeoGrid} object and the parameters \code{lonLim}
#'  and \code{latLim} passed by \code{loadGridDataset} and calculates the corresponding
#'  index positions.
#'
#' @param grid Java class \sQuote{GeoGrid}
#' @param lonLim see \code{\link{loadGridDataset}}
#' @param latLim see \code{\link{loadGridDataset}}
#' 
#' @return A list with the following items
#' \itemize{
#'  \item{\code{llbbox}} A list of length 1 or two depending on whether the selected domain
#'  crosses or not the dateline and longitude units go from 0 to 360. See details.
#'  \item{\code{pointXYindex}} A vector of length two with the index positions of the
#'  selected XY coordinates -in this order- in case of point selections. See details.
#'  \item{\code{lonSlice}} The X coordinates of the domain selected
#'  \item{\code{latSlice}} The Y coordinates of the domain selected
#'  \item{\code{revLat}} Logical. Whether the order of latitudes should be reversed or
#'  not in order to map the data properly in the geographical space
#' }
#' 
#' @details In order to deal with the problem of dateline crossing, the selection is
#' partitioned into two, and the part of the domain with negative eastings is put in first
#' place for consistent spatial mapping.
#' The index position of lon and lat in the corresponding axes is returned
#' by \code{pointXYindex}, and is passed to the \sQuote{readDataSlice} method in
#'  \code{makeSubset}. For single point locations, this is a integer vector of length
#'   two defining these positions, while in the case of rectangular domains its value is
#'    c(-1L,-1L).
#'    
#' @note The function assumes that datasets have degrees-east and degress-north as units
#' of the corresponding X and Y axes.
#' 
#' @references \url{https://www.unidata.ucar.edu/software/thredds/current/netcdf-java/v4.0/javadocAll/ucar/nc2/dt/GridCoordSystem.html#getRangesFromLatLonRect\%28ucar.unidata.geoloc.LatLonRect\%29}
#' 
#' @author J. Bedia, A. Cofino, M. Iturbide, S. Herrera
#' 
#' @keywords internal
#' @export
#' @import rJava

getLatLonDomain <- function(grid, lonLim, latLim) {
      if (any(lonLim > 180) | any(lonLim < -180) | any(latLim > 90) | any(latLim < -90)) {
            stop("Invalid geographical coordinates. Check 'lonLim' and/or 'latLim' argument values")
      }
      message("[", Sys.time(), "] Defining geo-location parameters")
      gcs <- grid$getCoordinateSystem()
      bboxDataset <- gcs$getLatLonBoundingBox()
      if (length(lonLim) == 1 | length(latLim) == 1) {
            pointXYpars <- findPointXYindex(lonLim, latLim, gcs)
            lonLim <- pointXYpars$lonLim
            latLim <- pointXYpars$latLim
            pointXYindex <- pointXYpars$pointXYindex
      } else {
            pointXYindex <- c(-1L, -1L)
      }    
      if (is.null(latLim)) {
            latLim <- c(bboxDataset$getLatMin(), bboxDataset$getLatMax())
            deltaLat <- latLim[2] - latLim[1]
      }
      if (is.null(lonLim)) {
            lonLim <- c(bboxDataset$getLonMin(), bboxDataset$getLonMax())
            deltaLon <- lonLim[2] - lonLim[1]
            spec <- .jnew("java/lang/String", paste(latLim[1], lonLim[1], deltaLat, deltaLon, sep = ", "))
            bboxRequest <- .jnew("ucar/unidata/geoloc/LatLonRect", spec)
            llbbox <- list()
            llRanges <- list()
            if (lonLim[1] > 180) {
                  lonLim <- lonLim - 360
                  lonLim <- sort(lonLim)
                  spec1 <- .jnew("java/lang/String", paste(latLim[1], lonLim[1], deltaLat, deltaLon, sep = ", "))
                  llbbox[[1]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec1)
                  llRanges[[1]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec1))
            } else if (lonLim[2] > 180) {
                  spec1 <- .jnew("java/lang/String", paste(latLim[1], -180, deltaLat, lonLim[2] - 180, sep = ", "))
                  spec2 <- .jnew("java/lang/String", paste(latLim[1], lonLim[1], deltaLat, (lonLim[2] - 180) - lonLim[1], sep = ", "))
                  llbbox[[1]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec1)
                  llbbox[[2]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec2)
                  llRanges[[1]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec1))
                  llRanges[[2]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec2))
            } else {
                  if (bboxRequest$getLonMin() < 0 & bboxRequest$getLonMax() >= 0 & bboxDataset$crossDateline()) {
                        spec1 <- .jnew("java/lang/String", paste(latLim[1], lonLim[1], deltaLat, 0 - lonLim[1], sep = ", "))
                        spec2 <- .jnew("java/lang/String", paste(latLim[1], 0, deltaLat, lonLim[2], sep = ", "))
                        llbbox[[1]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec1)
                        llbbox[[2]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec2)
                        llRanges[[1]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec1))
                        llRanges[[2]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec2))
                  } else {
                        spec1 <- .jnew("java/lang/String", paste(latLim[1], lonLim[1], deltaLat, deltaLon, sep = ", "))
                        llbbox[[1]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec1)
                        llRanges[[1]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec1))
                  }
            }
      } else {
            deltaLat <- latLim[2] - latLim[1]
            deltaLon <- lonLim[2] - lonLim[1]
            spec <- .jnew("java/lang/String", paste(latLim[1], lonLim[1], deltaLat, deltaLon, sep = ", "))
            bboxRequest <- .jnew("ucar/unidata/geoloc/LatLonRect", spec)
            llbbox <- list()
            llRanges <- list()
                  if (bboxRequest$getLonMin() < 0 & bboxRequest$getLonMax() >= 0 & bboxDataset$crossDateline()) {
                        spec1 <- .jnew("java/lang/String", paste(latLim[1], lonLim[1], deltaLat, 0 - lonLim[1], sep = ", "))
                        spec2 <- .jnew("java/lang/String", paste(latLim[1], 0, deltaLat, lonLim[2], sep = ", "))
                        llbbox[[1]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec1)
                        llbbox[[2]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec2)
                        llRanges[[1]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec1))
                        llRanges[[2]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec2))
                  } else {
                        llbbox[[1]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec)
                        llRanges[[1]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec))
                  }
            
      }
      if (pointXYindex[1] >= 0) {
            aux <- grid$makeSubset(.jnull(), .jnull(), .jnull(), 1L, 1L, 1L)
            lonSlice <- aux$getCoordinateSystem()$getLonAxis()$getCoordValue(pointXYindex[1])
      } else {
            lonAux <- list()
            for (k in 1:length(llbbox)) {
                  aux <- grid$makeSubset(.jnull(), .jnull(), llbbox[[k]], 1L, 1L, 1L)
                  lonAxisShape <- aux$getCoordinateSystem()$getXHorizAxis()$getShape()
                  lonAux[[k]] <- aux$getCoordinateSystem()$getXHorizAxis()$getCoordValues()
                  if (length(lonAxisShape) > 1) {
                        lonAux[[k]] <- apply(t(matrix(lonAux[[k]], nrow = lonAxisShape[2], ncol = lonAxisShape[1])), 2, min)
                  }
            }
            lonSlice <- do.call("c", lonAux)
      }
      lonSlice[which(lonSlice > 180)] <- lonSlice[which(lonSlice > 180)] - 360
      lonSlice <- sort(lonSlice)
      aux <- grid$makeSubset(.jnull(), .jnull(), llbbox[[1]], 1L, 1L, 1L)
      revLat <- FALSE
      if (pointXYindex[2] >= 0) {
            latSlice <- aux$getCoordinateSystem()$getYHorizAxis()$getCoordValue(pointXYindex[2])
      } else {
            latSlice <- aux$getCoordinateSystem()$getYHorizAxis()$getCoordValues()
            latAxisShape <- aux$getCoordinateSystem()$getYHorizAxis()$getShape()
            if (length(latAxisShape) > 1) {
                  latSlice <- apply(t(matrix(latSlice, nrow = latAxisShape[2], ncol = latAxisShape[1])), 1, min)
            }
            if (diff(latSlice)[1] < 0) {
                  latSlice <- rev(latSlice)
                  revLat <- TRUE
            }
      }
      return(list("llRanges" = llRanges, "llbbox" = llbbox, "pointXYindex" = pointXYindex, "xyCoords" = list("x" = lonSlice, "y" = latSlice), "revLat" = revLat))
}
# End



#' @title RCM grid adjustment
#' @description Performs operations to adequately handle 2D XY axis (typically from RCMs)
#' @param gds a Java GridDataSet object
#' @param latLon latLon definition (see \code{\link{getLatLonDomain}})
#' @param lonLim lonLim
#' @param latLim latLim
#' @return a new latLon definition
#' @keywords internal
#' @export
#' @importFrom stats na.omit
#' @author S. Herrera, M. Iturbide, J. Bedia


adjustRCMgrid <- function(gds, latLon, lonLim, latLim) {
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
            lonLim <- c(auxLon[arrayInd(which.min(auxLat), dim(auxLat))[1],
                               which.min(auxLon[arrayInd(which.min(auxLat),
                                                         dim(auxLat))[1],])],
                        auxLon[arrayInd(which.max(auxLat),
                                        dim(auxLat))[1],
                               which.max(auxLon[arrayInd(which.max(auxLat),
                                                         dim(auxLat))[1],])])
      }
      if (is.null(latLim)) {
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
            latLon$xyCoords$x <- nc$findCoordinateAxis('rlon')$getCoordValues()[ind.x]
            latLon$xyCoords$y <- nc$findCoordinateAxis('rlat')$getCoordValues()[ind.y]
            latLon$xyCoords$lon <- auxLon[ind.y,ind.x]
            latLon$xyCoords$lat <- auxLat[ind.y,ind.x]
      } else {
            auxDis <- sqrt((auxLon - lonLim[1]) ^ 2 + (auxLat - latLim[1]) ^ 2)
            llrowCol <- arrayInd(which.min(auxDis), dim(auxDis))
            auxDis <- sqrt((auxLon - lonLim[2]) ^ 2 + (auxLat - latLim[2]) ^ 2)
            urrowCol <- arrayInd(which.min(auxDis), dim(auxDis))
            auxDis <- sqrt((auxLon - lonLim[1]) ^ 2 + (auxLat - latLim[2]) ^ 2)
            ulrowCol <- arrayInd(which.min(auxDis), dim(auxDis))
            auxDis <- sqrt((auxLon - lonLim[2]) ^ 2 + (auxLat - latLim[1]) ^ 2)
            lrrowCol <- arrayInd(which.min(auxDis), dim(auxDis))
            llrowCol <- c(min(c(llrowCol[1],lrrowCol[1])), min(c(llrowCol[2],ulrowCol[2])))
            urrowCol <- c(max(c(ulrowCol[1],urrowCol[1])),max(c(lrrowCol[2],urrowCol[2])))
            latLon$xyCoords$x <- nc$findCoordinateAxis('rlon')$getCoordValues()[llrowCol[2]:urrowCol[2]]
            latLon$xyCoords$y <- nc$findCoordinateAxis('rlat')$getCoordValues()[llrowCol[1]:urrowCol[1]]
            latLon$xyCoords$lon <- auxLon[llrowCol[1]:urrowCol[1],llrowCol[2]:urrowCol[2]]
            latLon$xyCoords$lat <- auxLat[llrowCol[1]:urrowCol[1],llrowCol[2]:urrowCol[2]]
      }
      lonRanges  <- list()
      latRanges  <- list()
      if (length(latLon$llRanges) > 1) {
            range1 <- na.omit(as.integer(strsplit(latLon$llRanges[[1]]$toString(), "[^[:digit:]]")[[1]]))
            range2 <- na.omit(as.integer(strsplit(latLon$llRanges[[2]]$toString(), "[^[:digit:]]")[[1]]))
            for (i in 1:length(latLon$llRanges)){
                  lonRanges[[1]] <- .jnew("ucar/ma2/Range", range2[3], range2[4])
                  latRanges[[1]] <- .jnew("ucar/ma2/Range", as.integer(range2[1]), as.integer(range2[2]))
                  lonRanges[[2]] <- .jnew("ucar/ma2/Range", as.integer(range1[3] +1), as.integer(range1[4]))
                  latRanges[[2]] <- .jnew("ucar/ma2/Range", range1[1], range1[2])
            }
      } else {
            lonRanges[[1]] <- .jnew("ucar/ma2/Range", as.integer(llrowCol[2]-1), as.integer(urrowCol[2]-1))
            latRanges[[1]] <- .jnew("ucar/ma2/Range", as.integer(llrowCol[1]-1), as.integer(urrowCol[1]-1))
      }
      latLon$llRanges <- lapply(1:length(latLon$llRanges), function(x) {
            aux <- .jnew("java.util.ArrayList")
            aux$add(latRanges[[x]])
            aux$add(lonRanges[[x]])
            aux
      })
      return(latLon)
}


