#' Define appropriate indices for member selection
#' 
#' Subroutine of \code{\link{loadSeasonalForecast.S4}}
#' 
#' @param grid A java \sQuote{GeoGrid} 
#' @param dataset Character string indicating teh dataset to be accessed.
#' @param members Numeric vector of the members to be selected
#' @return A list of length \emph{n} members, containing a \sQuote{ucar.ma2.Range} object 
#' @note The function assumes that the ensemble members are internally designated by character strings,
#' rather than integers than in the former version. Otherwise, the getCoordValues()
#' method should be used instead of getNames() on the ensemble axis.
#' @author J. Bedia \email{joaquin.bedia@@gmail.com}

getMemberDomain <- function(grid, dataset, members) {
      gcs <- grid$getCoordinateSystem()
      if (is.null(members)) {
            members <- as.integer(javaString2rChar(gcs$getEnsembleAxis()$getNames()$toString()))
      } else {
            members <- as.integer(members - 1)
      }
      memberRangeList <- lapply(1:length(members), function(x) {
            .jnew("ucar/ma2/Range", members[x], members[x])
      })
      names(memberRangeList) <- paste("Member_", members + 1, sep = "")
      return(memberRangeList)
}
# End
