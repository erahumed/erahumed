test_that("setup_hbl(): no error with valid arguments", {
  simulation <- test_sim_small()
  storage_curve_slope_m2 <-
    eval( formals(setup_hydrology)$storage_curve_slope_m2 )
  storage_curve_intercept_m3 <-
    eval( formals(setup_hydrology)$storage_curve_intercept_m3 )
  petp_surface_m2 <-
    eval( formals(setup_hydrology)$petp_surface_m2 )

  expect_no_error(
    setup_hbl(simulation = simulation,
              storage_curve_slope_m2 = storage_curve_slope_m2,
              storage_curve_intercept_m3 = storage_curve_intercept_m3,
              petp_surface_m2 = petp_surface_m2)
    )
})

test_that("setup_hbl(): returns an object of the correct class", {
  simulation <- test_sim_small()
  storage_curve_slope_m2 <-
    eval( formals(setup_hydrology)$storage_curve_slope_m2 )
  storage_curve_intercept_m3 <-
    eval( formals(setup_hydrology)$storage_curve_intercept_m3 )
  petp_surface_m2 <-
    eval( formals(setup_hydrology)$petp_surface_m2 )

  obj <- setup_hbl(simulation = simulation,
                   storage_curve_slope_m2 = storage_curve_slope_m2,
                   storage_curve_intercept_m3 = storage_curve_intercept_m3,
                   petp_surface_m2 = petp_surface_m2)
  expect_s3_class(obj, class(erahumed_simulation()))
})
