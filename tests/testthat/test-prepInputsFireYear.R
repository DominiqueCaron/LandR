test_that("prepInputs fire year works", {
  skip_on_cran()
  skip_on_ci()

  td <- withr::local_tempdir("dest_")

  badExtent <- terra::ext(-80.7421995, -75.816238, 44.3156278, 46.971853)
  badPoly <- terra::vect(badExtent)
  terra::crs(badPoly) <- "epsg:4269"
  badRTM <- terra::rast(
    terra::ext(badPoly),
    crs = terra::crs(badPoly),
    resolution = c(0.01, 0.01),
    vals = 1
  )
  furl <- "https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip"
  expect_no_error({
    suppressWarnings({
      prepInputsFireYear(
        rasterToMatch = badRTM, maskTo = badPoly,
        destinationPath = td,
        url = furl,
        fireField = "YEAR"
      )
    })
  })
})
