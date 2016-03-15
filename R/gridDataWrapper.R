
#' @description Wrap an array in a gridded dataset
#! @param data N-dimensional Array. The number of dimensions depends on the type of gridded data: 4 in case of ensemble data, 3 in the other cases.
#! @param copy_from_grid Existing grid object to use for the definition of all the parameters except \code{"data"} 
#! @param dimnames A vector containing the name of the dimensions contained into \code{"data"} in the right order
#! @param varname The name of the variable contained into \code{"data"} 
#! @param lat Vector with latitude values (assuming latLonProjection)
#! @param lon Vector with longitude values (assuming latLonProjection)
#! @param timeVector If specified it contains all the time stamps for the \code{"data"} time dimension  
#! @param timeStart The time stamp of the first value of \code{"data"}
#! @param intervalLength The length (e.g. "1 day") of each time step
#' @template templateReturnGridData
#! @examples \dontrun{
#! # Averaging two grid data fields (g1 and g2)
#! g_average = gridDataWrapper(data = 0.5*g1$Data + 0.5 * g2$Data, copy_from_grid = g1)
#! }
#' @export
#' @author M. De Felice
gridDataWrapper <- function(data, copy_from_grid = NULL, dimNames = NULL, varname, lat, lon, timeVector = NULL, timeStart, intervalLength) {
     
     # Copy_from_grid let the user have the possibility to copy all the parameters from an existing grid object. 
     # This is particularly useful when you are doing computations on a data field, e.g. an average between two fields with the same size
     if (!is.null(copy_from_grid)) {
         if (!is.list(copy_from_grid)) {
             stop('Object in copy_from_grid must be a list')
         }
         message("[", Sys.time(), "] ", "Copying names, grid and time vector from another grid object...")
         # Copy all the parameters from another grid object
         if (!is.null(dimNames)) warning('DimNames will be overwritten due to copy_from_grid...')
         dimNames <- attr(copy_from_grid$Data, "dimensions")
         if (!missing("varname")) warning('Varname will be overwritten due to copy_from_grid...')
         varname  <- copy_from_grid$Variable$varName
         if (!missing("lat")) warning('Lat will be overwritten due to copy_from_grid...')
         lat      <- copy_from_grid$xyCoords$y
         if (!missing("lon")) warning('Lon will be overwritten due to copy_from_grid...')
         lon      <- copy_from_grid$xyCoords$x
         if (!missing("timeStart") || !is.null(timeVector)) warning('Time information will be overwritten due to copy_from_grid...')
         timeVector <- copy_from_grid$Dates$start
     }
     
     if (length(dim(data)) == 2) {
         warning('Data has only two dimensions and it is converted into a 3D array with time dimension of size 1')
         data <- array(data, dim = c(1, dim(data)))
         if (!is.null(dimNames)) {
           dimNames = c('time', dimNames)
         }
     } else if (length(dim(data)) < 2 || length(dim(data)) > 4 ) {
         stop('Number of data dimensions is wrong')
     } 
     
     if (is.null(dimNames)) {
        if (length(dim(data)) == 3) {
          dimNames <- c("time", "lat", "lon")
        } else {
        dimNames <- c("member", "time", "lat", "lon")
        }
      }
     
     # Variable
     Variable <- list("varName" = varname, "level" = NULL,
                      "use_dictionary" = FALSE)
     # Data     
     Data <- data
     attr(Data, "dimensions") <- dimNames
     
     # Coordinates
     lat_dim_index <- match("lat", dimNames)
     lon_dim_index <- match("lon", dimNames)
     if ((dim(data)[lat_dim_index] != length(lat)) || (dim(data)[lon_dim_index] != length(lon))) {
         stop("Lat/lon vectors are not consistent with data dimensions")
     }
     xyCoords <- list(x = lon, y = lat)
     attr(xyCoords, "projection") <- "LatLonProjection"
     
     # Dates
     if (is.null(timeVector)) {
         time_dim_index <- match("time", dimNames)
         if (intervalLength == '1 month') {
             # Monthly intervals should be dealt differently from other intervals (daily, weekly) because the length of the interval
             # is not constant, e.g. 30-01-2016 + 1 month -> 29-02-2016 
            timeVector <- as.Date(timeStart) %m+% months(seq(1,  dim(data)[time_dim_index]))
         } else {
            timeVector <- seq(as.POSIXct(timeStart), length = dim(data)[time_dim_index], by = intervalLength)    
         }  
     }
     # Check consistency with data
     if (length(timeVector) != dim(data)[match("time", dimNames)]) {
         stop('Length of time vector is not consistent with data time-dimension')
     }
     
     Dates <- list("start" = format(as.Date(timeVector), "%Y-%m-%d %H:%M:%S"), 
                   "end" = format(as.Date(timeVector), "%Y-%m-%d %H:%M:%S"))
     
     out <- list("Variable" = Variable, "Data" = Data, "xyCoords" = xyCoords, "Dates" = Dates)
     return(out)
}