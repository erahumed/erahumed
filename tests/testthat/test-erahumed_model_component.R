test_that("Constructor succeeds with valid input", {
  expect_no_error( new_model_component() )
})

test_that("Constructor throws an error if 'output' is not a data.frame", {
  expect_error(
    new_model_component(output = "Not a data.frame"),
    class = "new_model_component_argcheck_error"
    )
})

test_that("Constructor throws an error if 'params' is not a list", {
  expect_error(
    new_model_component(params = numeric(10)),
    class = "new_model_component_argcheck_error"
  )
})
