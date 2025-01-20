test_that("run_simulation() succeeds with default layer argument", {
  s <- test_sim_small() |> setup_hydrology()  # drop simulation outputs

  expect_no_error(run_simulation(s))
})

test_that("run_simulation() succeeds with valid layer argument", {
  s <- test_sim_small() |> setup_hydrology()  # drop simulation outputs

  expect_no_error(run_simulation(s, layer = "hbl"))
})

test_that("run_simulation(layer) does not run downstream deps of 'layer'", {
  s <- test_sim_small() |>
    setup_hydrology()  |> # drop simulation outputs
    run_simulation(layer = "hbl")

  expect_null(get_layer_output(s, "hbc"))
})

test_that("run_simulation() throws an error for invalid layer argument", {
  s <- erahumed_simulation()
  expect_error(run_simulation(s, layer = "invalid_layer"),
               class = "run_simulation_argcheck_error")
})

test_that("run_simulation() throws an error for invalid simulation object", {
  s <- list()
  expect_error(run_simulation(s), class = "run_simulation_argcheck_error")
})
