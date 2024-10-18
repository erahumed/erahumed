test_that("get_component_argcheck_fun() gets the right function", {
  expect_identical(get_component_argcheck_fun("dum"),
                   compute_dum_argcheck)
})

test_that("get_component_output_fun() gets the right function", {
  expect_identical(get_component_output_fun("dum"),
                   compute_dum_output)
})

test_that("get_component_output_validator_fun() gets the right function", {
  expect_identical(get_component_output_validator_fun("dum"),
                   dum_validate_output)
})

test_that("get_erahumed_fun() returns NULL if function not found", {
  expect_null( get_erahumed_fun("not a function name") )
})

test_that("get_component_constructor_fun() returns a valid constructor", {
  new_dum <- get_component_constructor_fun("dum")

  expect_no_error(obj <- new_dum(output = data.frame(),
                                 params = list(numeric_param = 0)
                                 )
                  )
  expect_s3_class(obj, class(new_model_component()))
  expect_s3_class(obj, "erahumed_dum")
})

test_that("get_component_constructor_fun() returns a valid constructor", {
  new_dum <- get_component_constructor_fun("dum")

  expect_no_error(obj <- new_dum(output = data.frame(),
                                 params = list(numeric_param = 0)
                                 )
                  )
  expect_s3_class(obj, class(new_model_component()))
  expect_s3_class(obj, "erahumed_dum")
})

test_that("compute_component() succeeds with valid input", {
  expect_no_error(
    compute_component(model = test_mod_small(),
                      component = "dum",
                      numeric_param = 0)
    )
})

test_that("compute_component() returns an object of the correct class", {
  obj <- compute_component(model = test_mod_small(),
                           component = "dum",
                           numeric_param = 0)

  expect_s3_class(obj, class(erahumed_model()))
})

test_that("compute_component() returned object has the added component", {
  obj <- compute_component(model = test_mod_small(),
                           component = "dum",
                           numeric_param = 0)

  expect_s3_class(dum(obj), "erahumed_dum")
})

test_that("compute_component() throws error when missing upstream deps", {
  expect_error(compute_component(model = erahumed_model(),
                                 component = "dum",
                                 numeric_param = 0),
               class = "compute_component_basecheck_error"
               )
})

test_that("compute_component() erases downstream deps if present", {
  m <- test_mod_small()

  if (is.null(hba(m)))
    stop("Invalid test object") # Ensure the test does not succeed trivially!
  m <- compute_component(model = m,
                         component = "inp",
                         outflows_df = albufera_outflows,
                         weather_df = albufera_weather
                         )

  expect_null(hba(m))
})
