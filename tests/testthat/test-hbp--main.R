test_that("compute_hbp() does not raise an error with valid inputs", {
  expect_no_error( compute_hbp(test_mod_small()) )
})

test_that("compute_hbp() error if invalid ideal_flow_rate_cm", {
  expect_error(
    compute_hbp(test_mod_small(), ideal_flow_rate_cm = -1),
    class = "compute_hbp_argcheck_error"
  )

  expect_error(
    compute_hbp(test_mod_small(), ideal_flow_rate_cm = "one"),
    class = "compute_hbp_argcheck_error"
  )

  expect_error(
    compute_hbp(test_mod_small(), ideal_flow_rate_cm = NA),
    class = "compute_hbp_argcheck_error"
  )

  expect_error(
    compute_hbp(test_mod_small(), ideal_flow_rate_cm = Inf),
    class = "compute_hbp_argcheck_error"
  )

  expect_error(
    compute_hbp(test_mod_small(), ideal_flow_rate_cm = NaN),
    class = "compute_hbp_argcheck_error"
  )


})

test_that("compute_hbp() error if missing upstream layers", {

  expect_error(
    compute_hbp(erahumed_simulation()),
    class = "compute_layer_basecheck_error"
  )

})

test_that("compute_hbp() error if invalid input data-frames", {
  m <- erahumed_simulation() |> compute_inp() |> compute_hba()

  expect_error(
    compute_hbp(m, management_df = erahumed::albufera_management[,-1]),
    class = "compute_hbp_argcheck_error"
  )


  expect_error(
    compute_hbp(m, clusters_df = erahumed::albufera_clusters[,-1]),
    class = "compute_hbp_argcheck_error"
  )

})

test_that("compute_hbp() error if invalid input numeric parameters", {
  m <- erahumed_simulation() |> compute_inp() |> compute_hba()

  expect_error(
    compute_hbp(m, ideal_flow_rate_cm = -1),
    class = "compute_hbp_argcheck_error"
    )

  expect_error(
    compute_hbp(m, ideal_flow_rate_cm = NA_real_),
    class = "compute_hbp_argcheck_error"
  )


  expect_error(
    compute_hbp(m, height_thresh_cm = "two"),
    class = "compute_hbp_argcheck_error"
    )

  expect_error(
    compute_hbp(m, height_thresh_cm = NA_real_),
    class = "compute_hbp_argcheck_error"
  )

})
