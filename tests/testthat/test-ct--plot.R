test_that("plot.erahumed_ct does not produce an error with valid inputs", {
  ct_obj <- ct(test_mod_small())
  cluster_id <- component_output(ct_obj)$cluster_id[[1]]

  expect_no_error( plot(ct_obj, cluster_id = cluster_id) )
})

