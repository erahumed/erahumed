test_that("Simulation can run with an 'empty' cluster map", {
  expect_no_error(
    erahumed_simulation(date_end = "2020-01-10",
                        cluster_map = new_cluster_map())
    )
})
