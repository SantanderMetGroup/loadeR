#' @title Reverse latitude coordinates ordering
#' 
#' @description Reverses the order of the latitudinal coordinates when the dataset is read in
#'  reverse y order. Sub-routine of \code{makeSubset}.
#' 
#' @param mdArray A n-dimensional array of data as returned by \code{makeSubset}
#' @param grid A java-class \sQuote{GeoGrid}
#' @return A n-dimensional array with the ordering of the lat dimension reversed
#' @references \url{http://adv-r.had.co.nz/Computing-on-the-language.html}
#' @author J. Bedia 
#' @note The code is partially based on an example provided at \url{http://stackoverflow.com/a/14502298}
#'  by Hadley Wickham
#' 
#' @keywords internal
#' @export

revArrayLatDim <- function(mdArray) {
  dimNames <- attr(mdArray, "dimensions")
  lat.dim.index <- grep("^lat$", dimNames)   
  if ((length(lat.dim.index) == 0) & (length(grep("^y$", dimNames)) > 0)) {
    lat.dim.index <- grep("^y$", dimNames) 
  } else if ((length(lat.dim.index) == 0) & (length(grep("^latitude$", dimNames)) > 0)) {
    lat.dim.index <- grep("^latitude$", dimNames) 
  }
  indices <- rep(list(bquote()), length(dim(mdArray)))
  indices[[lat.dim.index]] <- dim(mdArray)[lat.dim.index]:1
  call <- as.call(c(list(as.name("["), quote(mdArray)), indices))
  mdArray <- eval(call)
  attr(mdArray, "dimensions") <- dimNames
  return(mdArray)
}
# End
