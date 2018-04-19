#     writeStationData.R Write a station object to Value format
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
#' @title Write to Value format
#' @description Write to Value format
#' @param obj Object as returned from \code{\link[loadeR]{loadStationData}}.
#' @param path A string. Specify the path where to save the .txt.
#' @param na.code Possible values are NA or NaN. Default is NA. Indicates how missing values should be specified.
#' @export
#' @author J. Bano
writeStationData <- function(obj, path, na.code = NA) {
    df <- matrix(data = NA, nrow = length(obj$Dates$start), ncol = length(obj$Metadata$station_id)+1)
    colnames(df) <- c("YYYYMMDD",obj$Metadata$station_id)
    df[,1] <- sapply(1:length(obj$Dates$start), FUN = function(z) {
      paste0(substr(obj$Dates$start[z],1,4),substr(obj$Dates$start[z],6,7),substr(obj$Dates$start[z],9,10))
    })
    df[,2:ncol(df)] <- obj$Data
    
    if (anyNA(df)) {
      df[which(is.na(df))] <- na.code
    }
  write.csv(df,file = path,sep = ",", quote = FALSE, row.names = FALSE)
}
