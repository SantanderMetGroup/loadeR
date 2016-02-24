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

[`downscaleR`](https://github.com/SantanderMetGroup/downscaleR) is an R package for climate data analysis, with a special focus on empirical-statistical downscaling of daily data. Find more about this package in the [downscaleR's wiki](https://github.com/SantanderMetGroup/downscaleR/wiki).

The [`loadeR.ECOMS`](https://github.com/SantanderMetGroup/loadeR.ECOMS/) package extends the `loadeR` capabilities by providing homogenized access to the User Data Gateway, the Santander MetGroup's central data repository, including different seasonal and decadal forecast datasets.


