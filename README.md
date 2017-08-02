What is loadeR?
===============

`loadeR` is an R package for climate data data access and manipulation powered by NetCDF-Java (trough the `rJava` package). It allows:
 * Reading local and remote (OPeNDAP) climate datasets (NetCDF, Grib, HDF, etc.)
 * Creation of catalogs
 * Integration with the User Data Gateway ([UDG](http://www.meteo.unican.es/udg-wiki))
 * Basic data maniputation, homogeneization and spatiotemporal collocation.

Find out more about this package (including [installation information](https://github.com/SantanderMetGroup/loadeR/wiki/Installation)) in the [loadeR's WIKI](https://github.com/SantanderMetGroup/loadeR/wiki).

# `loadeR` within the **climate4R** Bundle

`loadeR` is a central building-block of the **climate4R bundle**, a set of user-oriented R packages fully integrated through a Common Data Model based on the data structure returned by `loadeR`. [See our paper](http://www.meteo.unican.es/en/node/73360) in a special issue of Climate Services for an overview of the **climate4R Bundle** description.

## Other packages of the `climate4R` bundle

 * [`loadeR.ECOMS`](https://github.com/SantanderMetGroup/loadeR.ECOMS/) extends `loadeR` by providing harmonized access to ***seasonal and decadal forecast datasets*** from the [ECOMS](http://www.eu-ecoms.eu) initiative. More information in the [ECOMS-UDG web](https://meteo.unican.es/trac/wiki/udg/ecoms) and in the dedicated [*Climate Services* paper](http://www.meteo.unican.es/en/node/73360) and its [companion vignette](http://meteo.unican.es/work/UDG/climate-services-manuscript.html) with worked examples.

 * [`transformeR`](https://github.com/SantanderMetGroup/transformeR) is an R package for **climate data transformation** integrated with the `loadeR's` data structures. It includes tools for subsetting, aggregation, principal component/EOF analysis, regridding and more...

 * [`downscaleR`](https://github.com/SantanderMetGroup/downscaleR/wiki) is an R package for **empirical-statistical downscaling** of daily data, including bias correction techniques.  
 
 * [`visualizeR`](https://github.com/SantanderMetGroup/visualizeR/wiki) is an R package implementing a set of advanced **visualization tools for forecast verification**.

 * [`loadeR.2nc`](https://github.com/SantanderMetGroup/loadeR.2nc/) provides support for **exporting to NetCDF**.
 
 * [`fireDanger`](https://github.com/SantanderMetGroup/fireDanger), which implements the Canadian Fire Weather Index System for its direct application to `climate4R` grids, also in seasonal forecast applications. See this [*Climate Services* paper](http://www.meteo.unican.es/en/node/73359), and a [vignette with worked examples](http://meteo.unican.es/work/fireDanger/ClimateServices2017.html).

 * [`drought4R`](https://github.com/SantanderMetGroup/drought4R). A package for drought and potential evapotranspiration calculation seamlessly integrated in the climate4R bundle.


