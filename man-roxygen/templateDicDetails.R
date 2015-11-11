#' @section Variable homogenization: The different nature of the various databases, models and variables, 
#' and the idiosyncratic naming and storage conventions often applied by the different 
#' modelling centres, makes necessary a previous homogeneization across datasets in 
#' order to implement a truly user-friendly toolbox for data access. 
#' This package achieves this aim by defining a common \code{vocabulary} to all 
#' climate datasets. The particular variables of each dataset are translated -and transformed if necessary- 
#' to the standard variables by means of a dictionary, provided by the argument \code{dictionary}.
#' In essence, the \file{dictionary} is a csv file particular for each individual dataset, 
#' containing the necessary information for performing the unit conversions to match the standard variable 
#' definitions contained in the vocabulary (see \code{\link{showVocabulary}}). This feature is described in more detail
#'  in the 
#' \href{http://meteo.unican.es/trac/wiki/udg/ecoms/RPackage/homogeneization}{User Data Gateway wiki}
#' of the Santander Meteorology Group. There are also some built-in dictionaries in this package.

