#' @title Login to the Santander Met Group Thredds Data Server
#' @description Provides HTTP authentication for accessing the Santander Met Group's THREDDS Data Server
#' @param username A character string with a valid user ID. See details.
#' @param password Character string. Authorized password. See details.
#' @param proxy.host In case of a proxy connection, the name of the proxy host, as a character string
#' @param proxy.port In case of a proxy connection, an integer indicating the proxy port
#' @details The Santander Met Group has deployed a THREDDS Data Server to enable access to different climate databases/data collections,
#' implementing a fine-grained user authorization and using remote data access protocols with data subsetting capabilities.
#' The THREDDS server plus the authentication layer conform the User Data Gateway (\href{http://meteo.unican.es/trac/wiki/udg}{UDG}).
#' Prior to data access, users must set their credentials. Registration can be obtained via the THREDDS Administration Panel (\href{http://www.meteo.unican.es/tap}{TAP}),
#'  indicating the group (Project) you belong to (e.g. CORDEX, SPECS ...), which will grant access to certain databases.
#'  Further details on registration for data access can be viewed in this \href{http://meteo.unican.es/trac/wiki/udg/registration}{example}.
#' @author J Bedia, M. Vega, A. Cofino
#' @export
#' @importFrom RCurl getURL


loginUDG <- function(username, password, proxy.host = NULL, proxy.port = NULL) {
      proxy.port <- as.integer(proxy.port)
      if (!is.character(username) | !is.character(password)) {
            stop("\'username\' and \'password\' must be character strings", call. = FALSE)
      }
      url.check <- paste0("https://meteo.unican.es/udg-tap/rest/v1/signin/verify?username=", username, "&password=", password)
      message("[",Sys.time(), "] Setting credentials...")
      con <- tryCatch(getURL(url.check, ssl.verifypeer = FALSE), error = function(er) {
            er <- NULL
            return(er)
      })
      if (is.null(con)) {
            stop("Could not establish a connection.", call. = FALSE)
      } else {
            b <- readLines(textConnection(con))
            if (grepl("SUCCESS", strsplit(b, split = "\\\""))) {
                  message("[",Sys.time(), "] Success!\nGo to <http://www.meteo.unican.es/udg-tap/home> for details on your authorized groups and datasets")
            } else {
                  stop("User name and password do not match\nPlease check your registration details or visit <http://www.meteo.unican.es/udg-tap/home> if in doubt")
            }
      }
      if (!is.null(proxy.host)) {
            J("ucar.nc2.util.net.HTTPSession")$setGlobalProxy(proxy.host, proxy.port)
      }
      J("ucar.httpservices.MyHTTPFactory")$setCredentials(username, password)
}
# End





