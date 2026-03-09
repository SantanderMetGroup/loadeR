# ============================
# Test: NetCDF-Java ensemble
# ============================

test_that("ensemble axis extraction works with NetCDF dataset", {
    skip_if(Sys.which("ncgen") == "", "Skipping test 'loadGridData': 'ncgen' is not available on system")
    skip_if_not(identical(Sys.getenv("RUN_NETCDF_TEST"), "true"),
    message = "Set RUN_NETCDF_TEST=true to run NetCDF-Java ensemble test."
    )
    
    # Dataset
    temp_dir <- getOption("loadeR.tempdir")
    ncml_path <- file.path(temp_dir, "test_multidim.ncml") 

    # Call
    out <- loadGridData(
        dataset = ncml_path,
        var = "tas@850", 
        lonLim = c(-10, 5),
        latLim = c(35, 45),
        season = 1:3,
        years = 2000:2000,
        members = 1:2)
    expect_type(out, "list")
    expect_true("Members" %in% names(out))
})