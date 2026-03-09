# Run before any test

if (Sys.which("ncgen") != "") {
  # Create a temporary directory 
  temp_test_dir <- file.path(tempdir(), "testdata")
  dir.create(temp_test_dir, showWarnings = FALSE, recursive = TRUE)

  # Create a temporary subdirectory for test_grid
  temp_test_grid_dir <- file.path(temp_test_dir, "test_grid")
  dir.create(temp_test_grid_dir, showWarnings = FALSE)

  # Store the path as an option so tests can access it
  options(loadeR.tempdir = temp_test_dir)

  # Cleanup 
  withr::defer(unlink(temp_test_dir, recursive = TRUE), teardown_env())

  # Get cdl files
  cdl_files <- list.files(testthat::test_path("testdata"), pattern = "\\.cdl$",
                          recursive = TRUE, full.names = TRUE)

  # Generate nc files
  message("Starting .nc generation from .cdl files...")

  for (cdl in cdl_files) {
    nc_filename <- sub("\\.cdl$", ".nc", basename(cdl))
    
    # If the file is one of the test_grid files, use the test_grid subdirectory
    if (nc_filename %in% c("test_pr.nc", "test_T.nc", "test_T_subdaily.nc", "test_Z.nc")) {
      nc_path <- file.path(temp_test_grid_dir, nc_filename)
    } else {
      nc_path <- file.path(temp_test_dir, nc_filename)
    }

    system2("ncgen", args = c("-7", "-o", nc_path, cdl))
  }

  # Generate ncml files
  message("Starting .ncml generation...")

  writeLines(
    '<?xml version="1.0" encoding="UTF-8"?>
    <netcdf xmlns="http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2">
      <aggregation type="union">
        <netcdf location="test_timestation.nc"/>
      </aggregation>
      <attribute name="timeSeries" value="true"/>
    </netcdf>',
    file.path(temp_test_dir, "test_timestation.ncml")
  )

  writeLines(
    '<?xml version="1.0" encoding="UTF-8"?>
    <netcdf xmlns="http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2">
      <aggregation type="union">
        <netcdf location="test_timestationlatlon.nc"/>
      </aggregation>
      <attribute name="timeSeries" value="true"/>
    </netcdf>',
    file.path(temp_test_dir, "test_timestationlatlon.ncml")
  )

  writeLines(
    '<?xml version="1.0" encoding="UTF-8"?>
        <netcdf xmlns="http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2"
                location="test_multidim.nc">
          <variable name="member" type="string" shape="member">
            <attribute name="standard_name" value="realization"/>
            <attribute name="_CoordinateAxisType" value="Ensemble"/>
            <attribute name="ref" value="http://www.uncertml.org/samples/realisation"/>
            <values> 1 2 3 4 5 </values>
          </variable>
        </netcdf>',
    file.path(temp_test_dir, "test_multidim.ncml")
  )

  writeLines(
    '<?xml version="1.0" encoding="UTF-8"?>
    <netcdf xmlns="http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2">
      <aggregation type="union">
 
        <netcdf>
          <aggregation dimName="time" type="joinExisting" timeUnitsChange="true">
            <variableAgg name="pr"/>
            <netcdf location="test_pr.nc" ncoords="1"/>
          </aggregation>
        </netcdf>
 
        <netcdf>
          <aggregation dimName="time" type="joinExisting" timeUnitsChange="true">
            <variableAgg name="T"/>
            <netcdf location="test_T.nc" ncoords="1096"/>
            <netcdf location="test_T_subdaily.nc" ncoords="8"/>
          </aggregation>
        </netcdf>
 
        <netcdf>
          <aggregation dimName="time" type="joinExisting" timeUnitsChange="true">
            <variableAgg name="Z"/>
            <netcdf location="test_Z.nc" ncoords="2"/>
          </aggregation>
        </netcdf>

      </aggregation>
    </netcdf>',
    file.path(temp_test_grid_dir, "test_grid.ncml")
  )
}

# Generate dic files
message("Starting .dic generation...")

writeLines(
"identifier,short_name,time_step,lower_time_bound,upper_time_bound,aggr_fun,offset,scale,deaccum
tas,T,24h,0,24,mean,-273.15,1,0
pr,pr,24h,0,24,sum,0,1000,0",
    file.path(temp_test_dir, "test.dic"))

writeLines(
"identifier,short_name,time_step,lower_time_bound,upper_time_bound,aggr_fun,offset,scale,deaccum
tas@850,tas,24h,0,24,mean,-273.15,1,0",
    file.path(temp_test_dir, "test_levelxy_subdaily.dic"))

writeLines(
"identifier,short_name,time_step,lower_time_bound,upper_time_bound,aggr_fun,offset,scale,deaccum
T,T,24h,0,24,mean,-273.15,1,0
Z,Z,24h,0,24,mean,0,0.1020408,0
pr,pr,24h,0,24,sum,0,1000,0",
    file.path(temp_test_grid_dir, "test_grid.dic"))
