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
