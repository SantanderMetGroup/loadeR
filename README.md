# What is loadeR?

loadeR is an R package for climate data access building on NetCDF Java. It allows loading  local or remote data (from OPeNDAP servers) and is fully integrated with the User Data Gateway ([UDG](http://www.meteo.unican.es/udg-wiki)). This package has been conceived to work in the framework of both seasonal forecasting and climate change studies. Thus, it considers ensemble members as a basic dimension of the two main data structures (`grid` and `station`). Find out more about this package at the [loadeR wiki](https://github.com/SantanderMetGroup/loadeR/wiki). 

This package is part of the [climate4R bundle](http://www.meteo.unican.es/climate4r), formed by `loadeR`, `transformeR`, `downscaleR` and `visualizeR`.

The recommended installation procedure (for loader and the companion loadeR.java packages) is to use the `install_github` command from the devtools R package (see the installation info in the wiki):

```r
devtools::install_github(c("SantanderMetGroup/loadeR.java", "SantanderMetGroup/loadeR"))
```
**IMPORTANT:** The package requires Java version 1.7 or higher. Several _recommendations for known problems with R and Java_ are given in the [wiki installation info](https://github.com/SantanderMetGroup/loadeR/wiki/Installation)). 
 
**NOTE:** loadeR is enhanced by [loadeR.ECOMS](http://meteo.unican.es/udg-wiki/ecoms/RPackage) package which allows to remotely access harmonized data from several state-of-the-art seasonal forecasting databases stored at the ECOMS-UDG. 

---
Reference and further information: 

Iturbide et al. (2018) climate4R: An R-based Framework for Climate Data Access, Post-processing and Bias Correction. Submitted to **Environmental Modeling and Software***, http://www.meteo.unican.es/climate4r_paper 

Cofiño et al. (2018) The ECOMS User Data Gateway: Towards seasonal forecast data provision and research reproducibility in the era of Climate Services. **Climate Services**, http://dx.doi.org/10.1016/j.cliser.2017.07.001.
