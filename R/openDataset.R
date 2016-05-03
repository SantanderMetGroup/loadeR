#' @title Open a grid dataset
#' @description Open a local or remote grid dataset
#' @param dataset The character string defining the dataset location
#' @return A java object of the class GridDataset
#' @keywords internal
#' @export
#' @author J. Bedia

openDataset <- function(dataset) {
      if (grepl("^http", dataset)) {
            message("[", Sys.time(), "] ", "Opening connection with remote server...")
            gds <- tryCatch(expr = J("ucar.nc2.dt.grid.GridDataset")$open(dataset), error = function(e) {
                  if (grepl("return status=503", e)) {
                        stop("Service temporarily unavailable\nThe server is temporarily unable to service your request due to maintenance downtime or capacity problems, please try again later.", call. = FALSE)
                  } else if (grepl("return status=404", e)) {
                        stop(dataset," is not a valid URL)", call. = FALSE)
                  }
            })
            if (is.null(gds)) {
                  stop("Requested URL not found\nThe problem may be momentary.", call. = FALSE)      
            }
            message("[", Sys.time(), "] ", "Connected successfuly")
      } else {
            gds <- J("ucar.nc2.dt.grid.GridDataset")$open(dataset)
      }
      return(gds)
}
