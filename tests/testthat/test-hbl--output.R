test_that("materialize_output.erahumed_output_hbl() succeeds", {
  raw <- get_raw_output(test_sim_small(), "hbl")

  expect_no_error(materialize_output(raw))
})
