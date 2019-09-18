#' @title Show standard public datasets
#' @description Show standard public datasets
#' @param pattern Optional. Pattern in the dataset name as passed to function \code{\link{grep}} (case-insensitive).
#' @return The datasets table, in the form of a \code{data.frame}
#' @note The function assumes that the user has read permission to the package installation directory
#' @author M Iturbide
#' @export
#' @importFrom utils read.csv
#' @examples
#' # Default built-in datasets
#' str(C4R.datasets())

C4R.datasets <- function(pattern = "") {
    df <- read.csv(file.path(find.package("loadeR"), "datasets.txt"), stringsAsFactors = FALSE)[ ,1:3]
    df[grep(pattern, df$name, ignore.case = TRUE),]
}
