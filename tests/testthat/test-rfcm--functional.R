test_that("Can run a valid simulation with an 'empty' cluster map", {
  expect_no_error(
    obj <- erahumed_simulation(date_end = "2020-01-10",
                               cluster_map = new_cluster_map()
                               )
    )

  expect_no_error(assert_erahumed_simulation(obj))
})

test_that("Simulation can run with an 'empty' cluster map", {
  expect_no_error(
    erahumed_simulation(date_end = "2020-01-10",
                        cluster_map = new_cluster_map()) |>
      plot_hbl()
  )
})
