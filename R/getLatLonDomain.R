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
#' @references \url{https://www.unidata.ucar.edu/software/thredds/v4.3/netcdf-java/v4.3/javadoc/ucar/nc2/dt/GridCoordSystem.html}
#' 
#' @author J. Bedia, A. Cofino, M. Iturbide, S. Herrera
#' 
#' @keywords internal
#' @export
#' @importFrom rJava .jnew .jnull
#' @importFrom stats median

getLatLonDomain <- function(grid, lonLim, latLim, spatialTolerance = NULL) {
  if (any(lonLim > 180) | any(lonLim < -180) | any(latLim > 90) | any(latLim < -90)) {
    stop("Invalid geographical coordinates. Check 'lonLim' and/or 'latLim' argument values")
  }
  message("[", Sys.time(), "] Defining geo-location parameters")
  gcs <- grid$getCoordinateSystem()
  bboxDataset <- gcs$getLatLonBoundingBox()
  resY <- tryCatch((bboxDataset$getLatMax() - bboxDataset$getLatMin())/(grid$getYDimension()$getLength()-1), error = function(e) NA, finally = NA)
  resX <- tryCatch((bboxDataset$getLonMax() - bboxDataset$getLonMin())/(grid$getXDimension()$getLength()-1), error = function(e) NA, finally = NA)
  if ((!is.null(latLim)) & (length(unique(latLim)) == 1)){
    latLim <- latLim[1]
  }
  if ((!is.null(lonLim)) & (length(unique(lonLim)) == 1)){
    lonLim <- lonLim[1]
  }                   
  if (length(latLim) > 1) {
    deltaLat <- latLim[2] - latLim[1]
    if (abs(resY - deltaLat) <= 1e-5 | is.na(resY)) {
      # latLim <- mean(latLim)
      latLim <- c(mean(latLim)-1e-5-max(c(resY,deltaLat), na.rm = TRUE)*0.5,mean(latLim)+1e-5+max(c(resY,deltaLat), na.rm = TRUE)*0.5)
      warning("Requested latLim range is smaller than the grid resolution. The nearest cell to ", mean(latLim), " will be returned.")
      deltaLat <- latLim[2] - latLim[1]
    } else if (deltaLat == 0 | is.na(resY)) {
      latLim <- c(latLim[1]-1e-5,latLim[1]+1e-5)
      deltaLat <- latLim[2] - latLim[1]
    }
  } else if ((length(latLim) == 1) & (is.na(resY))) {
    latLim <- c(latLim[1]-1e-5,latLim[1]+1e-5)
    deltaLat <- latLim[2] - latLim[1]
  }
  if (length(lonLim) > 1) {
    deltaLon <- lonLim[2] - lonLim[1]
    if (abs(resX - deltaLon) <= 1e-5 | is.na(resX)) {
      lonLim <- c(mean(lonLim)-1e-5-max(c(resX,deltaLon), na.rm = TRUE)*0.5,mean(lonLim)+1e-5+max(c(resX,deltaLon), na.rm = TRUE)*0.5)
      deltaLon <- lonLim[2] - lonLim[1]
      warning("Requested lonLim range is smaller than the grid resolution. The nearest cell to ", mean(lonLim), " will be returned.")
    } else if (deltaLon == 0 | is.na(resX)) {
      lonLim <- c(lonLim[1]-1e-5,lonLim[1]+1e-5)
      deltaLon <- lonLim[2] - lonLim[1]
    }
  } else if ((length(lonLim) == 1) & (is.na(resX))) {
    lonLim <- c(lonLim[1]-1e-5,lonLim[1]+1e-5)
    deltaLon <- lonLim[2] - lonLim[1]
  }
  if (length(lonLim) == 1 | length(latLim) == 1) {
    pointXYpars <- findPointXYindex(lonLim, latLim, gcs, spatialTolerance = spatialTolerance)
    lonLim <- pointXYpars$lonLim
    latLim <- pointXYpars$latLim
    pointXYindex <- pointXYpars$pointXYindex
  } else if ((bboxDataset$getLatMax() == bboxDataset$getLatMin()) & (bboxDataset$getLonMax() == bboxDataset$getLonMin())){
    pointXYpars <- findPointXYindex(bboxDataset$getLonMax(), bboxDataset$getLatMax(), gcs, spatialTolerance = spatialTolerance)
    lonLim <- pointXYpars$lonLim
    latLim <- pointXYpars$latLim
    pointXYindex <- pointXYpars$pointXYindex
    # pointXYindex <- as.integer(c(0,0))
  } else {
    pointXYindex <- c(-1L, -1L)
  }
  if (is.null(latLim)) {
    latLim <- c(bboxDataset$getLatMin(), bboxDataset$getLatMax())
  }else if (!is.null(spatialTolerance)){
    if ((latLim[1] > bboxDataset$getLatMax()) & (latLim[1] - spatialTolerance <= bboxDataset$getLatMax())){
      latLim[1] <- bboxDataset$getLatMax()
    } 
    if ((latLim[2] < bboxDataset$getLatMin()) & (latLim[2] + spatialTolerance >= bboxDataset$getLatMin())){
      latLim[2] <- bboxDataset$getLatMin()
    } 
  }
  deltaLat <- latLim[2] - latLim[1]
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
      if ((pointXYindex[1] >= 0) & (pointXYindex[2] >= 0)){
        spec1 <- .jnew("java/lang/String", paste(latLim[1], -180, deltaLat, lonLim[2] - 180, sep = ", "))
        spec2 <- .jnew("java/lang/String", paste(latLim[1], lonLim[1], deltaLat, (lonLim[2] - 180) - lonLim[1], sep = ", "))
        llbbox[[1]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec1)
        llRanges[[1]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec1))
        if (!is.element(pointXYindex[1],c(llRanges[[1]]$get(1L)$min():llRanges[[1]]$get(1L)$max()))){
          llbbox[[1]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec2)
          llRanges[[1]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec2))
        }
      }else{
        # spec1 <- .jnew("java/lang/String", paste(latLim[1], -180, deltaLat, -180 + (lonLim[2] - 180), sep = ", "))
        # spec2 <- .jnew("java/lang/String", paste(latLim[1], lonLim[1], deltaLat, (lonLim[2] - 180) - lonLim[1], sep = ", "))
        # llbbox[[1]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec1)
        # llbbox[[2]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec2)
        # llRanges[[1]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec1))
        # llRanges[[2]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec2))
        spec1 <- .jnew("java/lang/String", paste(latLim[1], lonLim[1], deltaLat, 180 - lonLim[1], sep = ", "))
        spec2 <- .jnew("java/lang/String", paste(latLim[1], -180 + max(c(resX, .Machine$double.eps), na.rm = TRUE), deltaLat, (lonLim[2] - 360) + 180, sep = ", "))
        llbbox[[1]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec1)
        llbbox[[2]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec2)
        llRanges[[1]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec1))
        llRanges[[2]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec2))
      }
    } else {
      if (bboxRequest$getLonMin() < 0 & bboxRequest$getLonMax() >= 0 & bboxDataset$crossDateline()) {
        spec1 <- .jnew("java/lang/String", paste(latLim[1], lonLim[1], deltaLat, 0 - lonLim[1], sep = ", "))
        spec2 <- .jnew("java/lang/String", paste(latLim[1], 0, deltaLat, lonLim[2], sep = ", "))
        llbbox[[1]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec1)
        llbbox[[2]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec2)
        llRanges[[1]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec1))
        llRanges[[2]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec2))
      } else if ((bboxDataset$getLatMax() == bboxDataset$getLatMin()) & (bboxDataset$getLonMax() == bboxDataset$getLonMin())){
        llbbox[[1]] <- .jnull()
        llRanges[[1]] <- .jnull()
      } else {
        spec1 <- .jnew("java/lang/String", paste(latLim[1], lonLim[1], deltaLat, deltaLon, sep = ", "))
        llbbox[[1]] <- .jnew("ucar/unidata/geoloc/LatLonRect", spec1)
        llRanges[[1]] <- gcs$getRangesFromLatLonRect(.jnew("ucar/unidata/geoloc/LatLonRect", spec1))
      }
    }
  } else {
    if (!is.null(spatialTolerance)){
      if ((lonLim[1] > bboxDataset$getLonMax()) & (lonLim[1] - spatialTolerance <= bboxDataset$getLonMax())){
        lonLim[1] <- bboxDataset$getLonMax()
      } 
      if ((lonLim[2] < bboxDataset$getLonMin()) & (lonLim[2] + spatialTolerance >= bboxDataset$getLonMin())){
        lonLim[2] <- bboxDataset$getLonMin()
      } 
    }
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
    lonSlice <- tryCatch(aux$getCoordinateSystem()$getLonAxis()$getCoordValue(pointXYindex[1]), error = function(e) NULL, finally = NULL)
    if (is.null(lonSlice)){
      lonAxisShape <- aux$getCoordinateSystem()$getXHorizAxis()$getShape()
      lonSlice <- aux$getCoordinateSystem()$getXHorizAxis()$getCoordValues()
      if (length(lonAxisShape) > 1) {
        lonSlice <- t(matrix(lonSlice, nrow = lonAxisShape[2], ncol = lonAxisShape[1]))
        if (pointXYindex[2] >= 0) {
          lonSlice <- lonSlice[pointXYindex[2]+1,pointXYindex[1]+1]
        } else {
          lonSlice <- lonSlice[1,pointXYindex[1]+1]
        }
      } else {
        lonSlice <- lonSlice[pointXYindex[1]+1]
      }
    }
  } else {
    lonAux <- list()
    for (k in 1:length(llbbox)) {
      aux <- grid$makeSubset(.jnull(), .jnull(), llbbox[[k]], 1L, 1L, 1L)
      lonAxisShape <- aux$getCoordinateSystem()$getXHorizAxis()$getShape()
      lonAux[[k]] <- aux$getCoordinateSystem()$getXHorizAxis()$getCoordValues()
      if (length(lonAxisShape) > 1) {
        lonAux[[k]] <- apply(t(matrix(lonAux[[k]], nrow = lonAxisShape[2], ncol = lonAxisShape[1])), 2, median)
      }
    }
    lonSlice <- do.call("c", lonAux)
  }
  lonSlice[which(lonSlice > 180)] <- lonSlice[which(lonSlice > 180)] - 360
  lonSlice <- sort(lonSlice, index.return = TRUE)
  if (!any(lonSlice$ix == sort(lonSlice$ix))){
    ixlonSlice <- lonSlice$ix
  }else{
    ixlonSlice <- NULL
  }
  lonSlice <- lonSlice$x
  aux <- grid$makeSubset(.jnull(), .jnull(), llbbox[[1]], 1L, 1L, 1L)
  revLat <- FALSE
  if (pointXYindex[2] >= 0) {
    latSlice <- tryCatch(aux$getCoordinateSystem()$getYHorizAxis()$getCoordValue(pointXYindex[2]), error = function(e) NULL, finally = NULL)
    if (is.null(latSlice)){
      latSlice <- aux$getCoordinateSystem()$getYHorizAxis()$getCoordValues()
      latAxisShape <- aux$getCoordinateSystem()$getYHorizAxis()$getShape()
      if (length(latAxisShape) > 1) {
        latSlice <- t(matrix(latSlice, nrow = latAxisShape[2], ncol = latAxisShape[1]))
        if (pointXYindex[1] >= 0){
          latSlice <- latSlice[pointXYindex[2]+1,pointXYindex[1]+1]
        }else{
          latSlice <- latSlice[pointXYindex[2]+1,1]
        }
      } else {
        latSlice <- latSlice[pointXYindex[2]+1]
      }
    }
  } else {
    latSlice <- aux$getCoordinateSystem()$getYHorizAxis()$getCoordValues()
    latAxisShape <- aux$getCoordinateSystem()$getYHorizAxis()$getShape()
    if (length(latAxisShape) > 1) {
      latSlice <- apply(t(matrix(latSlice, nrow = latAxisShape[2], ncol = latAxisShape[1])), 1, median)
    }
    if (latAxisShape[1] > 1 & diff(latSlice)[1] < 0) {
      latSlice <- rev(latSlice)
      revLat <- TRUE
    }
  }
  return(list("llRanges" = llRanges, "llbbox" = llbbox, "pointXYindex" = pointXYindex, "xyCoords" = list("x" = lonSlice, "y" = latSlice, "resX" = resX, "resY" = resY), "revLat" = revLat, "ix" = ixlonSlice))
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
#' @importFrom rJava .jnew
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
  if (length(lonLim) == 1 & length(latLim) == 1) {
    if (is.matrix(auxLat)){
      ind.y <- arrayInd(which.min(abs(auxLat - latLim)), dim(auxLat))[1]
    }else{
      ind.y <- which.min(abs(auxLat - latLim))
    }
    if (is.matrix(auxLon)){
      ind.x <- arrayInd(which.min(abs(auxLon - lonLim)), dim(auxLon))[2]
    }else{
      ind.x <- which.min(abs(auxLon - lonLim))
    }
    pointXYindex <- c(ind.y,ind.x)
    
    llrowCol <- c(ind.y,ind.x)
    lrrowCol <- c(ind.y,ind.x)
    ulrowCol <- c(ind.y,ind.x)
    urrowCol <- c(ind.y,ind.x)
    llrowCol <- c(min(c(llrowCol[1],lrrowCol[1])), min(c(llrowCol[2],ulrowCol[2])))
    urrowCol <- c(max(c(ulrowCol[1],urrowCol[1])),max(c(lrrowCol[2],urrowCol[2])))
    
    if (!is.null(nc$findDimension("rlon"))){
      latLon$xyCoords$x <- nc$findCoordinateAxis('rlon')$getCoordValues()[ind.x]
    }else{
      latLon$xyCoords$x <- nc$findCoordinateAxis('x')$getCoordValues()[ind.x]
    }
    if (!is.null(nc$findDimension("rlat"))){
      latLon$xyCoords$y <- nc$findCoordinateAxis('rlat')$getCoordValues()[ind.y]
    }else{
      latLon$xyCoords$y <- nc$findCoordinateAxis('y')$getCoordValues()[ind.x]
    }
    latLon$xyCoords$lon <- auxLon[ind.y,ind.x]
    latLon$xyCoords$lat <- auxLat[ind.y,ind.x]
    if (latLon$pointXYindex[1] >= 0){
      latLon$pointXYindex[1] = as.integer(-1)
    }
    if (latLon$pointXYindex[2] >= 0){
      latLon$pointXYindex[2] = as.integer(-1)
    }
  } else if (length(lonLim) == 1) {
    auxDis <- sqrt((auxLon - lonLim[1]) ^ 2 + (auxLat - latLim[1]) ^ 2)
    llrowCol <- arrayInd(which.min(auxDis), dim(auxDis))
    lrrowCol <- arrayInd(which.min(auxDis), dim(auxDis))
    auxDis <- sqrt((auxLon - lonLim[1]) ^ 2 + (auxLat - latLim[2]) ^ 2)
    ulrowCol <- arrayInd(which.min(auxDis), dim(auxDis))
    urrowCol <- arrayInd(which.min(auxDis), dim(auxDis))
    llrowCol <- c(min(c(llrowCol[1],lrrowCol[1])), min(c(llrowCol[2],ulrowCol[2])))
    urrowCol <- c(max(c(ulrowCol[1],urrowCol[1])),max(c(lrrowCol[2],urrowCol[2])))
    if (!is.null(nc$findDimension("rlon"))){
      latLon$xyCoords$x <- nc$findCoordinateAxis('rlon')$getCoordValues()[llrowCol[2]:urrowCol[2]]
    }else{
      latLon$xyCoords$x <- nc$findCoordinateAxis('x')$getCoordValues()[llrowCol[2]:urrowCol[2]]
    }
    if (!is.null(nc$findDimension("rlat"))){
      latLon$xyCoords$y <- nc$findCoordinateAxis('rlat')$getCoordValues()[llrowCol[1]:urrowCol[1]]
    }else{
      latLon$xyCoords$y <- nc$findCoordinateAxis('y')$getCoordValues()[llrowCol[1]:urrowCol[1]]
    }
    latLon$xyCoords$lon <- auxLon[llrowCol[1]:urrowCol[1],llrowCol[2]:urrowCol[2]]
    latLon$xyCoords$lat <- auxLat[llrowCol[1]:urrowCol[1],llrowCol[2]:urrowCol[2]]
    if (latLon$pointXYindex[1] >= 0){
      latLon$pointXYindex[1] = as.integer(-1)
    }
    if ((latLon$pointXYindex[2] >= 0) & (length(latLon$xyCoords$y) > 1)){
      latLon$pointXYindex[2] = as.integer(-1)
    }
  } else if (length(latLim) == 1) {
    auxDis <- sqrt((auxLon - lonLim[1]) ^ 2 + (auxLat - latLim[1]) ^ 2)
    ulrowCol <- arrayInd(which.min(auxDis), dim(auxDis))
    llrowCol <- arrayInd(which.min(auxDis), dim(auxDis))
    auxDis <- sqrt((auxLon - lonLim[2]) ^ 2 + (auxLat - latLim[1]) ^ 2)
    urrowCol <- arrayInd(which.min(auxDis), dim(auxDis))
    lrrowCol <- arrayInd(which.min(auxDis), dim(auxDis))
    llrowCol <- c(min(c(llrowCol[1],lrrowCol[1])), min(c(llrowCol[2],ulrowCol[2])))
    urrowCol <- c(max(c(ulrowCol[1],urrowCol[1])),max(c(lrrowCol[2],urrowCol[2])))
    if (!is.null(nc$findDimension("rlon"))){
      latLon$xyCoords$x <- nc$findCoordinateAxis('rlon')$getCoordValues()[llrowCol[2]:urrowCol[2]]
    }else{
      latLon$xyCoords$x <- nc$findCoordinateAxis('x')$getCoordValues()[llrowCol[2]:urrowCol[2]]
    }
    if (!is.null(nc$findDimension("rlat"))){
      latLon$xyCoords$y <- nc$findCoordinateAxis('rlat')$getCoordValues()[llrowCol[1]:urrowCol[1]]
    }else{
      latLon$xyCoords$y <- nc$findCoordinateAxis('y')$getCoordValues()[llrowCol[1]:urrowCol[1]]
    }
    latLon$xyCoords$lon <- auxLon[llrowCol[1]:urrowCol[1],llrowCol[2]:urrowCol[2]]
    latLon$xyCoords$lat <- auxLat[llrowCol[1]:urrowCol[1],llrowCol[2]:urrowCol[2]]
    if (latLon$pointXYindex[2] >= 0){
      latLon$pointXYindex[2] = as.integer(-1)
    }
    if ((latLon$pointXYindex[1] >= 0) & (length(latLon$xyCoords$x) > 1)){
      latLon$pointXYindex[1] = as.integer(-1)
    }
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
    if (!is.null(nc$findDimension("rlon"))){
      latLon$xyCoords$x <- nc$findCoordinateAxis('rlon')$getCoordValues()[llrowCol[2]:urrowCol[2]]
    }else{
      latLon$xyCoords$x <- nc$findCoordinateAxis('x')$getCoordValues()[llrowCol[2]:urrowCol[2]]
    }
    if (!is.null(nc$findDimension("rlat"))){
      latLon$xyCoords$y <- nc$findCoordinateAxis('rlat')$getCoordValues()[llrowCol[1]:urrowCol[1]]
    }else{
      latLon$xyCoords$y <- nc$findCoordinateAxis('y')$getCoordValues()[llrowCol[1]:urrowCol[1]]
    }
    latLon$xyCoords$lon <- auxLon[llrowCol[1]:urrowCol[1],llrowCol[2]:urrowCol[2]]
    latLon$xyCoords$lat <- auxLat[llrowCol[1]:urrowCol[1],llrowCol[2]:urrowCol[2]]
    if (latLon$pointXYindex[1] >= 0){
      latLon$pointXYindex[1] = as.integer(-1)
    }
    if (latLon$pointXYindex[2] >= 0){
      latLon$pointXYindex[2] = as.integer(-1)
    }
  }
  lonRanges  <- list()
  latRanges  <- list()
  if (length(latLon$llRanges) > 1) {
    range1 <- na.omit(as.integer(strsplit(latLon$llRanges[[1]]$toString(), "[^[:digit:]]")[[1]]))
    range2 <- na.omit(as.integer(strsplit(latLon$llRanges[[2]]$toString(), "[^[:digit:]]")[[1]]))
    for (i in 1:length(latLon$llRanges)) {
      lonRanges[[1]] <- .jnew("ucar/ma2/Range", range2[3], range2[4])
      latRanges[[1]] <- .jnew("ucar/ma2/Range", as.integer(range2[1]), as.integer(range2[2]))
      lonRanges[[2]] <- .jnew("ucar/ma2/Range", as.integer(range1[3] + 1), as.integer(range1[4]))
      latRanges[[2]] <- .jnew("ucar/ma2/Range", range1[1], range1[2])
    }
  } else {
    lonRanges[[1]] <- .jnew("ucar/ma2/Range", as.integer(llrowCol[2] - 1),
                            as.integer(urrowCol[2] - 1))
    latRanges[[1]] <- .jnew("ucar/ma2/Range", as.integer(llrowCol[1] - 1),
                            as.integer(urrowCol[1] - 1))
  }
  latLon$llRanges <- lapply(1:length(latLon$llRanges), function(x) {
    aux <- .jnew("java.util.ArrayList")
    aux$add(latRanges[[x]])
    aux$add(lonRanges[[x]])
    aux
  })
  return(latLon)
}
