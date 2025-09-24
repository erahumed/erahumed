test_that("materialize_output.erahumed_output_ctl() succeeds", {
  raw <- get_raw_output(test_sim_small(), "ctl")

  expect_no_error(materialize_output(raw))
})

test_that("materialize_output.erahumed_output_ctd() succeeds", {
  raw <- get_raw_output(test_sim_small(), "ctd")

  expect_no_error(materialize_output(raw))
})

test_that("materialize_output.erahumed_output_ctc() succeeds", {
  raw <- get_raw_output(test_sim_small(), "ctc")

  expect_no_error(materialize_output(raw))
})
