#' Define indices for member selection
#' Define indices for member selection
#' @param grid A java \sQuote{GeoGrid} 
#' @param dataset Character string indicating teh dataset to be accessed.
#' @param members Numeric vector of the members to be selected
#' @return A list of length \emph{n} members, containing a \sQuote{ucar.ma2.Range} object 
#' @note The function assumes that the ensemble members are internally designated by character strings,
#' rather than integers than in the former version. Otherwise, the getCoordValues()
#' method should be used instead of getNames() on the ensemble axis.
#' @author J. Bedia
#' @keywords internal

getMemberDomain <- function(grid, dataset, members) {
      gcs <- grid$getCoordinateSystem()
      members <- if (is.null(members)) {
            as.integer(javaString2rChar(gcs$getEnsembleAxis()$getNames()$toString()))
      } else {
            as.integer(members - 1)
      }
      memberRangeList <- lapply(1:length(members), function(x) {
            .jnew("ucar/ma2/Range", members[x], members[x])
      })
      names(memberRangeList) <- paste("Member_", members + 1, sep = "")
      return(memberRangeList)
}
# End
