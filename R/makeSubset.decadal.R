#' Makes a logical subset of a System4 GeoGrid
#' 
#' Makes a logical subset of a System4 GeoGrid using the parameters specified by the user,
#' applying the java methods makeSubset and readDataSlice. 
#' 
#' @param grid An input java GeoGrid
#' @param latLon A list of geolocation parameters, as returned by getLatLonDomainForecast
#' @param runTimePars A list of run time definition parameters, as returned by getRunTimeDomain.decadal
#' @param memberRangeList A list of ensemble java ranges as returned by getMemberDomain.S4
#' @param foreTimePars A list of forecast time definition parameters, as returned by getForecastTimeDomain.S4
#' @param verticalPars A list with vertical level definition, as returned by \code{getVerticalLevelPars.ECOMS}. Only the last element (the kava index position) is used
#' @return A list containing a n-dimensional array and the (possibly) modified foreTimePars object with the 
#' corrected dates in case of time aggregations. Dimensions are labelled by the \dQuote{dimnames} attribute.
#' @details Dimensions of length one are dropped and the \dQuote{dimnames} attribute is consequently modified.
#' In the current version the Z dimension is ignored (and dropped), as it is not planned to include multi-level variables
#' in the ECOMS-UDG by the moment.
#' 
#' @keywords internal
#' 
#' @references \url{http://www.unidata.ucar.edu/software/thredds/v4.3/netcdf-java/v4.3/javadocAll/ucar/nc2/dt/grid/GeoGrid.html}
#' @author J Bedia, A. Cofino

makeSubset.decadal <- function(grid, latLon, runTimePars, memberRangeList, foreTimePars, verticalPars) {
      message("[", Sys.time(), "] Retrieving data subset ..." )
      gcs <- grid$getCoordinateSystem()
      dimNames <- rev(names(scanVarDimensions(grid))) # reversed!
      z <- verticalPars$zRange
      aux.foreDatesList <- rep(list(foreTimePars$forecastDates), length(memberRangeList))      
      foreTimePars$forecastDates <- NULL
      aux.list <- rep(list(bquote()), length(memberRangeList))
      #######
      for (i in 1:length(memberRangeList)) {
            ens <- memberRangeList[[i]]
            aux.list1 <- rep(list(bquote()), length(runTimePars$runTimeRanges))
            for (j in 1:length(runTimePars$runTimeRanges)) {
                  rt <- runTimePars$runTimeRanges[[j]]
                  ft <- foreTimePars$ForeTimeRangesList[[j]]
                  aux.list2 <- rep(list(bquote()), length(latLon$llRanges))
                  # dateline crossing
                  for (k in 1:length(latLon$llRanges)) {
                        subSet <- grid$makeSubset(rt, ens, ft, z, latLon$llRanges[[k]]$get(0L), latLon$llRanges[[k]]$get(1L))
                        shapeArray <- rev(subSet$getShape())
                        dimNamesRef <- dimNames              
                        if (latLon$pointXYindex[1] >= 0) {
                              rm.dim <- grep(gcs$getXHorizAxis()$getDimensionsString(), dimNamesRef, fixed = TRUE)
                              shapeArray <- shapeArray[-rm.dim]
                              dimNamesRef <- dimNamesRef[-rm.dim]
                        }
                        if (latLon$pointXYindex[2] >= 0) {
                              rm.dim <- grep(gcs$getYHorizAxis()$getDimensionsString(), dimNamesRef, fixed = TRUE)
                              shapeArray <- shapeArray[-rm.dim]
                              dimNamesRef <- dimNamesRef[-rm.dim]
                        }
                        aux.list2[[k]] <- array(subSet$readDataSlice(-1L, -1L, -1L, -1L, latLon$pointXYindex[2], latLon$pointXYindex[1])$copyTo1DJavaArray(), dim = shapeArray)
                  }
                  aux.list1[[j]] <- do.call("abind", c(aux.list2, along = 1))
                  aux.list2 <- NULL
                  # Deaccumulator
                  if (foreTimePars$deaccum) {
                        mar <- c(1:length(dim(aux.list1[[j]])))[-grep("^time", dimNamesRef)] 
                        aux.list1[[j]] <- apply(aux.list1[[j]], mar, diff)#deaccumulate, foreTimePars$deaccumFromFirst)
                        dimNamesRef <- c("time", dimNamesRef[grep("^time$", dimNamesRef, invert = TRUE)])
                  }      
                  # Daily aggregator
                  if (foreTimePars$aggr.d != "none") {
                        aux.string <- paste((aux.foreDatesList[[i]][[j]])$mon, (aux.foreDatesList[[i]][[j]])$mday, sep = "-")
                        aux.factor <- factor(aux.string, levels = unique(aux.string))
                        mar <- grep("^time", dimNamesRef, invert = TRUE)
                        aux.list1[[j]] <- apply(aux.list1[[j]], mar, function(x) {
                              tapply(x, INDEX = aux.factor, FUN = foreTimePars$aggr.d, na.rm = TRUE)
                        })
                        dimNamesRef <- c("time", dimNamesRef[mar])
                        # Convert dates to daily:
                        nhours <- length(aux.factor) / nlevels(aux.factor)
                        aux.foreDatesList[[i]][[j]] <- aux.foreDatesList[[i]][[j]][seq(1, by = nhours, length.out = nlevels(aux.factor))]
                  }
                  # Monthly aggregator
                  if (foreTimePars$aggr.m != "none") {
                        mes <- (aux.foreDatesList[[i]][[j]])$mon
                        day <- (aux.foreDatesList[[i]][[j]])$mday
                        mar <- grep("^time", dimNamesRef, invert = TRUE)
                        aux.list1[[j]] <- apply(aux.list1[[j]], MARGIN = mar, FUN = function(x) {
                              tapply(x, INDEX = mes, FUN = foreTimePars$aggr.m)
                        })
                        dimNamesRef <- if (length(unique(mes)) > 1) {
                              c("time", dimNamesRef[mar])
                        } else {
                              dimNamesRef[mar]
                        }
                        aux.foreDatesList[[i]][[j]] <- if (aux.foreDatesList[[i]][[j]][1]$mday == 2) { # case of lm0 precip
                              append(aux.foreDatesList[[i]][[j]][1], aux.foreDatesList[[i]][[j]][which(day == 1)])
                        } else {
                              aux.foreDatesList[[i]][[j]][which(day == 1)]      
                        }
                  }
            }
            if (foreTimePars$aggr.m != "none") {
                  if (length(unique(mes)) > 1) {
                        aux.list[[i]] <- do.call("abind", c(aux.list1, along = grep("^time", dimNamesRef)))
                  } else {
                        aux.list[[i]] <- do.call("abind", c(aux.list1, along = -1L))
                        dimNamesRef <- c("time", dimNamesRef)
                  }
            } else {
                  aux.list[[i]] <- do.call("abind", c(aux.list1, along = grep("^time", dimNamesRef)))
            }
            aux.list1 <- NULL
      }
      mdArray <- do.call("abind", c(aux.list, along = grep(gcs$getEnsembleAxis()$getDimensionsString(), dimNamesRef, fixed = TRUE)))
      aux.list <- NULL
      if (any(dim(mdArray) == 1)) {
            dimNames <- dimNamesRef[-which(dim(mdArray) == 1)]
            mdArray <- drop(mdArray)
      } else {
            dimNames <- dimNamesRef
      }
      dimNames <- gsub("^time.*", "time", dimNames)
      mdArray <- unname(mdArray)
      attr(mdArray, "dimensions") <- dimNames
      # Date adjustment
      foreTimePars$forecastDates <- aux.foreDatesList
      foreTimePars$forecastDates <- adjustDates.forecast(foreTimePars)
      return(list("mdArray" = mdArray, "foreTimePars" = foreTimePars))
}
# End
