test_that("materialize_output.erahumed_output_rl() succeeds", {
  raw <- get_raw_output(test_sim_small(), "rl")

  expect_no_error(materialize_output(raw))
})

test_that("materialize_output.erahumed_output_rd() succeeds", {
  raw <- get_raw_output(test_sim_small(), "rd")

  expect_no_error(materialize_output(raw))
})

test_that("materialize_output.erahumed_output_rc() succeeds", {
  raw <- get_raw_output(test_sim_small(), "rc")

  expect_no_error(materialize_output(raw))
})
