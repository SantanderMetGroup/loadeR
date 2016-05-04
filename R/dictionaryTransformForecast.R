#' Performs variable transformation
#' 
#' Performs variable transformation according to dictionary specifications.
#' Sub-routine of the \code{load*Forecast} methods.
#' 
#' @param dic Dictionary line for the variable, as returned by \code{dictionaryLookup.ECOMS}
#' @param mdArray A n-dimensional array, as returned by \code{makeSubset}
#' @return The transformed n dimensional array of data. See details.
#' @keywords internal
#' @export
#' @author J. Bedia 

dictionaryTransformForecast <- function(dic, mdArray) {
      mdArray * dic$scale + dic$offset
}

	 
