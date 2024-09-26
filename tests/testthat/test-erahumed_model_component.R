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

test_that("get_model_component() succeeds with valid input", {
  model <- test_mod_small()
  expect_no_error( get_model_component(model, "hba") )
})

test_that("get_model_component() throws an error with invalid input", {
  model <- test_mod_small()
  expect_error( get_model_component(model, "unexisting_component") )
})

test_that("get_model_component() returns NULL if component not yet computed", {
  model <- erahumed_model()
  expect_null( get_model_component(model, "hba") )
})
