#' @title Inventory of a gridded dataset
#' @description Returns a list with summary information about the variables stored in a gridded dataset.
#' Sub-routine of \code{dataInventory}
#' @param dataset A full path to the file describing the dataset (NcML)
#' @return A (named) list whose length is determined by the number of variables stored in the dataset,
#' its names corresponding to the short names of the variables.
#' For each variable, information on the variable long name, data type, units, data size (in Mb) and
#' characteristics of its dimensions is provided.
#' @author J. Bedia 
#' @keywords internal
#' @importFrom rJava J


dataInventory.NetCDF <- function(dataset) {
    gds <- J("ucar.nc2.dt.grid.GridDataset")$open(dataset)
    varNames <- unlist(strsplit(gsub("\\[|]|\\s", "", gds$getGrids()$toString()), ","))
    rm.ind <- grep("^lon|^lat", varNames)  
    if (length(rm.ind) > 0) {
        varNames <- varNames[-rm.ind]
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
            grid <- gds$findGridByShortName(varName)
            dim.list <- scanVarDimensions(grid)
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
# End