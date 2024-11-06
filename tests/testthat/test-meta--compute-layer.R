test_that("get_compute_bare_fun() gets the right function", {
  expect_identical(get_compute_bare_fun("hba"), compute_hba_bare)
})

test_that("get_validate_output_fun() gets the right function", {
  expect_identical(get_validate_output_fun("ct"), validate_ct_output)
})

test_that("get_erahumed_fun() returns NULL if function not found", {
  expect_null( get_erahumed_fun("not a function name") )
})


test_that("compute_layer() succeeds with valid input", {
  expect_no_error( compute_layer(test_sim_small(), "inp") )
})

test_that("compute_layer() returns an object of the correct class", {
  obj <- compute_layer(simulation = test_sim_small(), layer = "hba")

  expect_s3_class(obj, class(erahumed_simulation()))
})


test_that("compute_layer() throws error when missing upstream deps", {
  expect_error(compute_layer(erahumed_simulation(), "hba"),
               class = "check_upstream_layers_error"
  )
})

