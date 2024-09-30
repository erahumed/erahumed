test_that("Low-level constructor succeeds", {
  expect_no_error( new_erahumed_model() )
})

test_that("High-level constructor succeeds", {
  expect_no_error( erahumed_model() )
})

test_that("Validator returns TRUE on constructor output", {
  m <- erahumed_model()
  expect_true( is_erahumed_model(m) )
})

test_that("Validator: FALSE if object is not a list", {
  obj <- structure("Not a list", class = class(erahumed_model()))
  expect_false( is_erahumed_model(obj) )
})

test_that("Validator: FALSE if list elements are not of the right S3 class", {
  obj <- erahumed_model()
  obj[["invalid_element"]] <- list()  # Not of 'model_component' class!
  expect_false( is_erahumed_model(obj) )
})

test_that("Validator: FALSE if object is not of the right S3 class", {
  expect_false( is_erahumed_model(list()) )
})
