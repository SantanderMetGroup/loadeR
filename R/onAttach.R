#' @importFrom utils packageDescription

.onAttach <- function(...) {
      pkgname <- "loadeR"
      ver <- packageDescription(pkgname)$Version
      builddate <- packageDescription(pkgname)$Date
      mess <- paste(pkgname, " version ", ver, " (", builddate,") is loaded", sep = "")
      packageStartupMessage(mess)
      url <- paste0("https://raw.githubusercontent.com/SantanderMetGroup/", pkgname, "/master/DESCRIPTION")
      b <- tryCatch(suppressWarnings(readLines(url)), error = function(er) {
            er <- NULL
            return(er)
      })
      if (!is.null(b)) {
            latest.ver <- package_version(gsub("Version: ", "", b[grep("^Version", b)]))
            if (ver < latest.ver) {
                  ver.mess1 <- paste0("WARNING: Your current version of ", pkgname, " (v", ver, ") is not up-to-date")
                  ver.mess <- paste0("Get the latest stable version (", latest.ver,
                                     ") using <devtools::install_github(c('SantanderMetGroup/climate4R.UDG','SantanderMetGroup/", pkgname, "'))>")
                  packageStartupMessage(ver.mess1)
                  packageStartupMessage(ver.mess)
            }
      }
      packageStartupMessage("Please use 'citation(\"", pkgname, "\")' to cite this package.")
}
# End

#' @importFrom utils globalVariables

if (getRversion() >= "3.1.0") utils::globalVariables(c("node.list"))
