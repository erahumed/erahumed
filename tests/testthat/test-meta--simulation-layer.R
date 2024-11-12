test_that("Constructor succeeds with valid input", {
  expect_no_error( new_simulation_layer() )
})

test_that("Constructor throws an error if 'output' is not a data.frame", {
  expect_error(
    new_simulation_layer(output = "Not a data.frame"),
    class = "new_simulation_layer_argcheck_error"
    )
})

test_that("Constructor throws an error if 'params' is not a list", {
  expect_error(
    new_simulation_layer(params = numeric(10)),
    class = "new_simulation_layer_argcheck_error"
  )
})

test_that("print() method succeeds", {
  obj <- erahumed_simulation() |> get_layer("hba")
  expect_no_error(capture.output(print(obj)))
})

test_that("print() method returns invisibly", {
  obj <- erahumed_simulation() |> get_layer("hba")
  print_res <- expect_output(print(obj))
  expect_identical(print_res, obj)
})

test_that("summary() method succeeds", {
  obj <- erahumed_simulation() |> get_layer("hba")
  expect_no_error( capture.output(summary(obj)) )
})
