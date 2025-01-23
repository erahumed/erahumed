test_that("setup_hbd() does not raise an error with valid inputs", {
  expect_no_error( setup_hbd(test_sim_small(), ditch_level_m = 1) )
})

test_that("setup_hbd() error if invalid ditch_level_m", {
  expect_error(
    setup_hbd(test_sim_small(), ditch_level_m = -1),
    class = "validate_hbd_params_error"
  )

  expect_error(
    setup_hbd(test_sim_small(), ditch_level_m = "one"),
    class = "validate_hbd_params_error"
  )

  expect_error(
    setup_hbd(test_sim_small(), ditch_level_m = NA),
    class = "validate_hbd_params_error"
  )

  expect_error(
    setup_hbd(test_sim_small(), ditch_level_m = Inf),
    class = "validate_hbd_params_error"
  )

  expect_error(
    setup_hbd(test_sim_small(), ditch_level_m = NaN),
    class = "validate_hbd_params_error"
  )

})
