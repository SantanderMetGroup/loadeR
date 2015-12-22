#' @title Subset decadal forecast by initialization years
#' 
#' @description Subset decadal forecast by initialization years
#' 
#' @param data field obtained from the output of function loadDecadalForecast. 
#' @param ly integer indicating the lead year of initialization 
#' @author J. Bedia, S. Herrera, M. Iturbide, J.M. Gutierrez 
#' @export
#' @examples \dontrun{
#' latLim <- c(-20,10)
#' lonLim <-  c(-60,-30)
#' season <- 3:5
#' period <- 1981:1991
#' loginUDG(username = "myuser", password = "mypassword") #type help(loginUDG)
#' tasDECA <- loadDecadalForecast(dataset = "http://www.meteo.unican.es/tds5/dodsC/specs/gfdl_specs_decadal.ncml", 
#'    latLim = latLim, lonLim = lonLim,
#'    var = "tas", dictionary = F, years = 1981:1982, season = season)
#' # data corresponding to the initialitation of the year before
#' subsetDecadalForecast(tasDECA, ly = 1)
#' }

subsetDecadalForecast <- function(data, ly = 1){
      
      init <- data$InitializationDates      
      dates <- data$Dates$start
      y <- getYearsAsINDEX(data)
      yu <- unique(y)
      
      yl <- (length(data$InitializationDates)-length(unique(y))+1) - ly
      
      months <- (which(y[1:12] != y[1])[1]-1)/2
      
      if (is.na(months) & length(yu)>1){
            months <- 12
      }else if (is.na(months) & length(yu)==1){
            months <- length(y)/(length(data$InitializationDates)-length(unique(y))+1)
      }
      
      IND <- list()
      yinit.sub <- list()
      for (i in 1:length(yu)){
            year <- which(y == yu[i])
            
            a <- yl*months
            ind <- lapply(1:length(a), function(x){
                  (a[x]-months+1):a[x]
                  
            })
            
            yinit <- as.numeric(substr(init, 1, 4))
            initdates <- yu[i]-(length(data$InitializationDates)-length(unique(y))+1)+yl
            initdates2 <- numeric()
                  for (n in 1:length(initdates)){
                        initdates2[n] <- which(yinit == initdates[n])
                  }
            yinit.sub[[i]] <- initdates2
                  
            IND[[i]] <- year[unlist(ind)]
            
      }
      
      ind <- sort(unlist(IND))
      ind.init <- sort(unique(unlist(yinit.sub)))
      if (length(ind.init ) == 0) {stop("ly (lead years) outside the extent of the number of initializations")}
      data$InitializationDates <- init[ind.init]
      data$Dates$start <- dates[ind]
      data$Dates$end <- dates[ind]
      
      if(any(attr(data$Data, "dimensions") == "member")){
            dat <- data$Data[,ind,,]  
      }else{
            dat <- data$Data[ind,,]  
      }
      attr(dat, "dimensions") <- attr(data$Data, "dimensions")
      data$Data <- dat
      
      return(data)
}
#End



#' @title Get years as a factor
#' @description Extract the year as a factor (e.g. for computing annual statistics)
#' @param obj Any object extending the station or field classes
#' @return A vector of years of the same length as the time dimension of the object, 
#' seasonally-adjusted in the case of year-crossing seasons (e.g. DJF). See details.
#' @details The function performs a very basic operation, extracting the year element from the 
#' dates previously converted to POSIXlt. The trick lies in the year-crossing seasons. For instance:
#'  by convention, winter 2001 encompasses December 2000 and January, February 2001. Therefore, in order to compute
#' annual statistics for a year-crossing season, it is necessary to modify first the vector of years, 
#' and assign year 2001 to the preceding December. Similarly, the next December 2001 belongs to winter 2002,
#'  and so on... The function is useful for computing and/or plotting annual statistics, seasonal climatologies ... 
#' @section Warning:
#' The function should no be used to extract the actual years vector
#' @author J. Bedia \email{joaquin.bedia@@gmail.com}


getYearsAsINDEX <- function(obj) {
      season <- getSeason(obj)
      dimNames <- attr(obj$Data, "dimensions")
      if (any(grepl("var", dimNames))) {
            aux.dates <- as.POSIXlt(obj$Dates[[1]]$start)
      } else {
            aux.dates <- as.POSIXlt(obj$Dates$start)
      }
      yrs <- aux.dates$year + 1900
      if (!identical(season, sort(season))) {
            yy <- unique(yrs)[-1]
            aux <- match(aux.dates$mon + 1, season)
            brks <- c(1, which(diff(aux) < 0) + 1, length(aux) + 1)
            l <- lapply(1:(length(brks) - 1), function(x) {
                  a <- yrs[brks[x] : (brks[x + 1] - 1)]
                  return(rep(yy[x], length(a)))
            })
            yrs  <- do.call("c", l)
      }
      return(yrs)
}
# End


#' @title Get season from a station or field object
#' @description Retrieves the season encompassed by a station or field object
#' @param obj Any object extending the station or field classes
#' @return An integer vector with the season
#' @author J. Bedia \email{joaquin.bedia@@gmail.com}


getSeason <- function(obj) {
      dimNames <- attr(obj$Data, "dimensions")
      aux <- if (any(grepl("var", dimNames))) {
            as.POSIXlt(obj$Dates[[1]]$start)$mon + 1      
      } else {
            as.POSIXlt(obj$Dates$start)$mon + 1      
      }
      return(unique(aux))
}
# End