# What is loadeR?

loadeR is an R package for climate data access building on NetCDF Java. It allows loading  local or remote data (from OPeNDAP servers) and is fully integrated with the User Data Gateway ([UDG](http://www.meteo.unican.es/udg-wiki)). This package has been conceived to work in the framework of both seasonal forecasting and climate change studies. Thus, it considers ensemble members as a basic dimension of the two main data structures (`grid` and `station`). Find out more about this package at the [loadeR wiki](https://github.com/SantanderMetGroup/loadeR/wiki). 

This package is part of the [climate4R bundle](http://www.meteo.unican.es/climate4r), formed by `loadeR`, `transformeR`, `downscaleR` and `visualizeR`.

The recommended installation procedure (for loader and the companion loadeR.java packages) is to use the `install_github` command from the devtools R package (see the installation info in the wiki):

```r
devtools::install_github(c("SantanderMetGroup/loadeR.java", "SantanderMetGroup/loadeR"))
```
**IMPORTANT:** On OS X, be sure to execute this in R started from the Terminal, not the R App! (This is because the R app doesn’t honor $PATH changes in ~/.bash_profile)

**IMPORTANT:** The package requires Java version 1.7 or higher. Several _recommendations for known problems with R and Java_ are given in the [wiki installation info](https://github.com/SantanderMetGroup/loadeR/wiki/Installation)). 
 
**NOTE:** loadeR is enhanced by [loadeR.ECOMS](http://meteo.unican.es/udg-wiki/ecoms/RPackage) package which allows to remotely access harmonized data from several state-of-the-art seasonal forecasting databases stored at the ECOMS-UDG. 

---
Reference and further information: 

**[General description of the framework]** Iturbide et al. (2019) The R-based climate4R open framework for reproducible climate data access and post-processing. **Environmental Modelling and Software**, 111, 42-54. https://doi.org/10.1016/j.envsoft.2018.09.009
Check out the companion notebooks for the two examples [GitHub](https://github.com/SantanderMetGroup/notebooks).

**[Seasonal forecasting applications]** Cofiño et al. (2018) The ECOMS User Data Gateway: Towards seasonal forecast data provision and research reproducibility in the era of Climate Services. **Climate Services**, 9, 33-43. http://doi.org/10.1016/j.cliser.2017.07.001

**[Example of a sectoral application (fire danger)]** Bedia et al. (2018) Seasonal predictions of Fire Weather Index: Paving the way for their operational applicability in Mediterranean Europe. **Climate Services**, 9, 101-110. http://doi.org/10.1016/j.cliser.2017.04.001
