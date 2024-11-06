test_that("get_layer_argcheck_fun() gets the right function", {
  expect_identical(get_layer_argcheck_fun("dum"),
                   compute_dum_argcheck)
})

test_that("get_layer_output_fun() gets the right function", {
  expect_identical(get_layer_output_fun("dum"),
                   compute_dum_output)
})

test_that("get_layer_output_validator_fun() gets the right function", {
  expect_identical(get_layer_output_validator_fun("dum"),
                   dum_validate_output)
})

test_that("get_erahumed_fun() returns NULL if function not found", {
  expect_null( get_erahumed_fun("not a function name") )
})

test_that("get_layer_constructor_fun() returns a valid constructor", {
  new_dum <- get_layer_constructor_fun("dum")

  expect_no_error(obj <- new_dum(output = data.frame(),
                                 params = list(numeric_param = 0)
                                 )
                  )
  expect_s3_class(obj, class(new_simulation_layer()))
  expect_s3_class(obj, "erahumed_dum")
})

test_that("get_layer_constructor_fun() returns a valid constructor", {
  new_dum <- get_layer_constructor_fun("dum")

  expect_no_error(obj <- new_dum(output = data.frame(),
                                 params = list(numeric_param = 0)
                                 )
                  )
  expect_s3_class(obj, class(new_simulation_layer()))
  expect_s3_class(obj, "erahumed_dum")
})

test_that("compute_layer() succeeds with valid input", {
  expect_no_error(
    compute_layer(model = test_mod_small(),
                      layer = "dum",
                      numeric_param = 0)
    )
})

test_that("compute_layer() returns an object of the correct class", {
  obj <- compute_layer(model = test_mod_small(),
                           layer = "dum",
                           numeric_param = 0)

  expect_s3_class(obj, class(erahumed_simulation()))
})

test_that("compute_layer() returned object has the added layer", {
  obj <- compute_layer(model = test_mod_small(),
                           layer = "dum",
                           numeric_param = 0)

  expect_s3_class(dum(obj), "erahumed_dum")
})

test_that("compute_layer() throws error when missing upstream deps", {
  expect_error(compute_layer(model = erahumed_simulation(),
                                 layer = "dum",
                                 numeric_param = 0),
               class = "compute_layer_basecheck_error"
               )
})

test_that("compute_layer() erases downstream deps if present", {
  m <- test_mod_small()

  if (is.null(hba(m)))
    stop("Invalid test object") # Ensure the test does not succeed trivially!
  m <- compute_layer(model = m,
                         layer = "inp",
                         outflows_df = albufera_outflows,
                         weather_df = albufera_weather
                         )

  expect_null(hba(m))
})
