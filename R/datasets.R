#' @title Show UDG datasets
#' @description Access the installed user's vocabulary
#' @return The datasets table, in the form of a data.frame
#' @note The function assumes that the user has read permission to the package installation directory
#' @author M Iturbide
#' @export
#' @importFrom utils read.csv
#' @examples
#' # Default built-in datasets
#' (sets <- UDG.datasets())
#' sets[grep("^ta", sets$name), ]

UDG.datasets <- function() {
      read.csv(file.path(find.package("loadeR"), "datasets.txt"))
}