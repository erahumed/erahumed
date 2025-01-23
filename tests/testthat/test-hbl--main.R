test_that("setup_hbl(): no error with valid arguments", {
  simulation <- test_sim_small()
  storage_curve <- eval( formals(setup_hydrology)$storage_curve )
  petp_function <- eval( formals(setup_hydrology)$petp_function )

  expect_no_error(
    setup_hbl(simulation = simulation,
              storage_curve = storage_curve,
              petp_function = petp_function)
    )
})

test_that("setup_hbl(): returns an object of the correct class", {
  simulation <- test_sim_small()
  storage_curve <- eval( formals(setup_hydrology)$storage_curve )
  petp_function <- eval( formals(setup_hydrology)$petp_function )

  obj <- setup_hbl(simulation = simulation,
                   storage_curve = storage_curve,
                   petp_function = petp_function)
  expect_s3_class(obj, class(erahumed_simulation()))
})
