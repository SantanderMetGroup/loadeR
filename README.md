What is loadeR?
===============

`loadeR` is a set of functions for climate data load and pre-processing in R, including:
 * Reading local and remote (OPeNDAP) climate datasets
 * Creation of catalogs
 * Authentication to remote data servers
 * Interpolation/regridding
 * Visualization

`loadeR` is powered by NetCDF-Java. Thus, a large variety of files can be read, including the most common NetCDF, Grib, HDF... See a complete (http://www.unidata.ucar.edu/software/thredds/current/netcdf-java/reference/formats/FileTypes.html)[list of supported formats]

Since November 2015, this package supersedes the data loading capabilities of package `downscaleR`.

## How can I install it?

We recommend using the utilities in package `devtools` to this aim, as follows:

```{r}
devtools::install_github(c("SantanderMetGroup/loadeR.java", "SantanderMetGroup/loadeR"))
```


