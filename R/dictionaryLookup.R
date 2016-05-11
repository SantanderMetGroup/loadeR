#' @title Searches variable string in the dictionary
#' 
#' @description Searches variable string provided in the dictionary to map it in the vocabulary, in order to get
#' all the necessary information for variable homogenization. It also includes a new column specifying the
#' aggregation function to be applied (if any).
#' 
#' @param dicPath Full path to the dictionary file (a csv file with extension \sQuote{.dic}).
#' @param var Character string with the (standard) name of the variable
#' @param time Time specification.
#' 
#' @return A data.frame of 1 row with the mapping information
#' 
#' @references \url{http://meteo.unican.es/trac/wiki/udg/ecoms/RPackage/homogeneization}
#' 
#' @author J. Bedia
#' 
#' @keywords internal

dictionaryLookup <- function(dicPath, var, time) {
      message("[", Sys.time(), "] Defining homogeneization parameters for variable \"", var, "\"")
      dictionary <- tryCatch({read.csv(dicPath, stringsAsFactors = FALSE)}, error = function(e) stop("Dictionary not found"))
      dicRow <- grep(paste("^", var, "$", sep = ""), dictionary$identifier) 
      if (length(dicRow) == 0) {
            stop("Variable requested does not match any identifier in the dictionary\nType 'help(UDG.vocabulary)' for help on standard variable nomenclature", call. = FALSE)
      }
      if (length(dicRow) > 1) {
            if (time == "DD") {
                  dicRow <- dicRow[dictionary$time_step[dicRow] == "24h"]
                  if (length(dicRow) == 0) {
                        dicRow <- grep(paste("^", var, "$", sep = ""), dictionary$identifier)                  
                        dicRow <- dicRow[dictionary$time_step[dicRow] == "6h" | dictionary$time_step[dicRow] == "3h"]
                  }
            } else {
                  dicRow <- dicRow[dictionary$time_step[dicRow] == "6h" | dictionary$time_step[dicRow] == "3h"]
            }
      } else {
            if (dictionary$time_step[dicRow] == "12h" & time == "DD") {
                  stop("Cannot compute daily mean from 12-h data", call. = FALSE)
            }
            if ((time %in% c("03","06","09","15","18","21")) & dictionary$time_step[dicRow] == "12h") {
                  stop("Requested 'time' value (\"", time, "\") not available for 12-h data", call. = FALSE)
            }
            if ((time != "none" & time != "DD") & (dictionary$time_step[dicRow] == "24h")) {
                  stop("Subdaily data not available for variable \"", var, "\". Check value of argument 'time'", call. = FALSE)
            }
            if (time == "DD" & dictionary$time_step[dicRow] == "24h") {
                  time <- "none"
            }
      }
      return(dictionary[dicRow, ])
}
# End


#' @title Check for dictionary options
#' @description Check dictionary argument and transform variable name and generate dic details accordingly
#' @param dataset dataset
#' @param var var
#' @param dictionary dictionary
#' @param time time
#' @return a list with actual shortName and dic definition
#' @keywords internal
#' @author J. Bedia

check.dictionary <- function(dataset, var, dictionary, time) {
      if (dictionary == FALSE) {
            dic <- NULL
            shortName <- var
      } else {
            dicPath <- if (isTRUE(dictionary)) {
                  gsub("ncml$", "dic", dataset)
            } else {
                  dictionary
            }
            dic <- dictionaryLookup(dicPath, var, time)
            shortName <- dic$short_name          
      }
      return(list("shortName" = shortName, "dic" = dic))
}
