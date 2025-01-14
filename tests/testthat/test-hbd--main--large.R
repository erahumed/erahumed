# Property based tests

test_that("Returned dataset has the expected number of rows", {
  test_df <- get_layer_output(test_sim_large(), "hbd")
  n_ditches <- nrow(info_ditches())
  n_days <- length( seq.Date(from = min(test_df$date),
                             to = max(test_df$date),
                             by = "day")
  )

  expect_equal(nrow(test_df), n_ditches * n_days)
})

test_that("Outflow = Inflow", {
  test_df <- get_layer_output(test_sim_large(), "hbd") |>
    dplyr::mutate(
      test = abs(outflow_lake_m3 - inflow_clusters_m3 - inflow_external_m3),
      tol = 1e-6 * mean(outflow_lake_m3)
      ) |>
    dplyr::filter(test > tol)

  expect_equal(nrow(test_df), 0)
})

test_that("External inflow is always positive", {
  test_df <- get_layer_output(test_sim_large(), "hbd") |>
    dplyr::filter(inflow_external_m3 < -1e6 * mean(inflow_external_m3))

  expect_equal(nrow(test_df), 0)
})

test_that("Cluster inflow is always positive", {
  test_df <- get_layer_output(test_sim_large(), "hbd") |>
    dplyr::filter(inflow_clusters_m3 < -1e6 * mean(inflow_clusters_m3))

  expect_equal(nrow(test_df), 0)
})
