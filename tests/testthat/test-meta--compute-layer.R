test_that("compute_layer() returns an object of the correct class", {
  obj <- compute_layer(simulation = test_sim_small(), layer = "hba")

  expect_s3_class(obj, class(erahumed_simulation()))
})


test_that("compute_layer() throws error when missing upstream deps", {
  expect_error(compute_layer(erahumed_simulation(), "hba"),
               class = "check_upstream_layers_error"
  )
})

