#' Define indices for member selection
#' Define indices for member selection
#' @param grid A java \sQuote{GeoGrid} 
#' @param members Numeric vector of the members to be selected
#' @return A list of length \emph{n} members, containing a \sQuote{ucar.ma2.Range} object 
#' @note The function assumes that the ensemble members are internally designated by character strings,
#' rather than integers than in the former version. Otherwise, the getCoordValues()
#' method should be used instead of getNames() on the ensemble axis.
#' @author J. Bedia
#' @keywords internal
#' @importFrom utils tail

getMemberDomain <- function(grid, members, continuous = FALSE) {
      gcs <- grid$getCoordinateSystem()
      members <- if (is.null(members)) {
            as.integer(javaString2rChar(gcs$getEnsembleAxis()$getNames()$toString()))
      } else {
            as.integer(members - 1)
      }
      if (!continuous) {
            memberRangeList <- lapply(1:length(members), function(x) {
                  .jnew("ucar.ma2.Range", members[x], members[x])
            })
            names(memberRangeList) <- paste0("Member_", members + 1)
      } else {
            if (!all(diff(members) == 1)) stop("Non-continuous member selections are not allowed", call. = FALSE)
            memberRangeList <- .jnew("ucar.ma2.Range", members[1], tail(members, 1))    
      }
      return(memberRangeList)
}
# End
