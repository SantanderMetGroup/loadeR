#     writeStations.R Load a user-defined spatio-temporal slice from a gridded dataset
#
#     Copyright (C) 2017 Santander Meteorology Group (http://www.meteo.unican.es)
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
#' @title Load a grid from a gridded dataset
#' @description Convert to Value format or to netcdf a station dataset.
#' @param obj Object as returned from \code{\link[loadeR]{loadStationData}}.
#' @param path A string. Specify the path where to save the .netcdf or the .txt.
#' @param na.code Possible values are NA or NaN. Default is NA. Indicates how missing values should be specified.
#' @param type A string c("Value","netcdf"). To what format would you like to convert your data?
#' @export
#' @author J. Bano, M. Iturbide
writeStations <- function(obj, path, na.code = NA, type = c("Value","netcdf")) {
  if (type == "Value") {
    df <- matrix(data = NA, nrow = length(obj$Dates$start), ncol = length(obj$Metadata$station_id)+1)
    colnames(df) <- c("YYYYMMDD",obj$Metadata$station_id)
    df[,1] <- sapply(1:length(obj$Dates$start), FUN = function(z) {
      paste0(substr(obj$Dates$start[z],1,4),substr(obj$Dates$start[z],6,7),substr(obj$Dates$start[z],9,10))
    })
    df[,2:ncol(df)] <- obj$Data
    
    if (anyNA(df)) {
      df[which(is.na(df))] <- na.code
    }
  }
  write.csv(df,file = path,sep = ",", quote = FALSE, row.names = FALSE)
}
