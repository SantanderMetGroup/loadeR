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
