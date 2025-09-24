test_that("materialize_output.erahumed_output_hbc() succeeds", {
  raw <- get_raw_output(test_sim_small(), "hbc")

  expect_no_error(materialize_output(raw))
})
