#' @title Subset decadal forecast by initialization years
#' 
#' @description Subset decadal forecast by initialization years
#' 
#' @param data field obtained from the output of function loadDecadalForecast. 
#' @param ly lead years. A vector with integers from 1 to 10. 
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
#' subsetDecadalForecast(tasDECA, ly = c(2,3,5))
#' }

subsetDecadalForecast <- function(data, ly = c(1, 2, 3)){
      
      init <- data$InitializationDates      
      
      y <- getYearsAsINDEX(data)
      yu <- unique(y)
      
      
      months <- (which(y[1:12] != y[1])[1]-1)/2
      
      if (is.na(months) & length(yu)>1){
            months <- 12
      }else if (is.na(months) & length(yu)==1){
            months <- length(y)/10
      }
      
      IND <- list()
      yinit.sub <- list()
      for (i in 1:length(yu)){
            year <- which(y == yu[i])
            
            a <- ly*months
            ind <- lapply(1:length(a), function(x){
                  (a[x]-months+1):a[x]
                  
            })
            
            yinit <- as.numeric(substr(init, 1, 4))
            initdates <- yu[i]-10+ly
            initdates2 <- numeric()
                  for (n in 1:length(initdates)){
                        initdates2[n] <- which(yinit == initdates[n])
                  }
            yinit.sub[[i]] <- initdates2
                  
            IND[[i]] <- year[unlist(ind)]
            
      }
      
      ind <- sort(unlist(IND))
      ind.init <- sort(unique(unlist(yinit.sub)))
      
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
