What is loadeR?
===============

`loadeR` is a set of functions for climate data load and pre-processing in R, including:
 * Reading local and remote (OPeNDAP) climate datasets
 * Creation of catalogs
 * Authentication to remote data servers
 * Interpolation/regridding
 * Visualization

`loadeR` is powered by NetCDF-Java. Thus, a large variety of files can be read, including the most common NetCDF, Grib, HDF... (see a complete [list of supported formats](http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/reference/formats/FileTypes.html)). 

Since November 2015, this package supersedes the data loading capabilities of package [`downscaleR`](https://github.com/SantanderMetGroup/downscaleR).

Find out more about this package in the [loadeR's WIKI](https://github.com/SantanderMetGroup/loadeR/wiki).


# How can I install it?

We recommend using the utilities in package `devtools` to this aim, as follows:

```{r}
devtools::install_github(c("SantanderMetGroup/loadeR.java", "SantanderMetGroup/loadeR"))
```

# See also...

 *[`loadeR.ECOMS`](https://github.com/SantanderMetGroup/loadeR.ECOMS/) extends `loadeR` by providing homogenized access to the [User Data Gateway](http://meteo.unican.es/trac/wiki/udg), the Santander MetGroup's OPeNDAP central data repository, including different reanalysis products and seasonal to decadal forecast datasets.

 *[`loadeR.2nc`](https://github.com/SantanderMetGroup/loadeR.2nc/) provides support for exporting `loadeR` grids to NetCDF.

 *[`downscaleR`](https://github.com/SantanderMetGroup/downscaleR) is an R package for climate data analysis, with a special focus on bias correction and empirical-statistical downscaling of daily data, fully integrated with the `loadeR` bundle. Find more about this package in the [downscaleR's wiki](https://github.com/SantanderMetGroup/downscaleR/wiki).


