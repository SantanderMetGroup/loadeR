#' @title Open a grid dataset
#' @description Open a local or remote grid dataset
#' @param dataset The character string defining the dataset location
#' @return A java object of the class GridDataset
#' @keywords internal
#' @importFrom RCurl getURL
#' @importFrom RCurl url.exists
#' @import rJava
#' @export
#' @author J. Bedia, A Cofino, M. Vega

openDataset <- function(dataset) {
      url.check <- paste0(dataset, ".html")
      txt <- url.exists(url.check, .header = TRUE)
      if (!is.logical(txt)) {
            message("[", Sys.time(), "] ", "Opening dataset...")
            htmlheader <- getURL(url.check, ssl.verifypeer = FALSE)
            if (grepl("code = 404", htmlheader)) {
                  stop("The requested dataset URL was not found (HTTP error 404)", call. = FALSE)
            }
            if (grepl("code = 503", htmlheader)) {
                  stop("Service temporarily unavailable (HTTP error 503)\nThe server is temporarily unable to service your request, please try again later.", call. = FALSE)
            }
            gds <- tryCatch(expr = J("ucar.nc2.dt.grid.GridDataset")$open(dataset), error = function(e) {
                  # These return status do not appear in recent versions of the java api. Only the last one is currently acting
                  if (grepl("return status=503", e)) {
                        stop("Service temporarily unavailable\nThe server is temporarily unable to service your request, please try again later.", call. = FALSE)
                  } else if (grepl("return status=404", e)) {
                        stop(dataset," is not a valid URL)", call. = FALSE)
                  } else if (grepl("return status=401", e)) {
                        stop("UNAUTHENTICATED\nCheck your login details", call. = FALSE)
                  } else if (grepl("return status=403", e)) {
                        stop("FORBIDDEN: You don't have the necessary permissions to access the requested dataset", call. = FALSE)
                  } else if (grepl("Unauthorized to open dataset", e)) {
                        stop("UNAUTHORIZED to open the requested dataset.\nCheck your login details and authorized datasets.", call. = FALSE)
                  }
            })
            if (is.null(gds)) {
                  stop("Requested URL not found\nThe problem may be momentary.", call. = FALSE)      
            }
            message("[", Sys.time(), "] ", "The dataset was successfuly opened")
      } else {
            gds <- J("ucar.nc2.dt.grid.GridDataset")$open(dataset)
      }
      return(gds)
}

