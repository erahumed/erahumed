test_that("materialize_output.erahumed_output_hbd() succeeds", {
  raw <- get_raw_output(test_sim_small(), "hbd")

  expect_no_error(materialize_output(raw))
})
