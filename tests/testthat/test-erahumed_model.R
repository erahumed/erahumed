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

test_that("Validator returns FALSE if the underlying object is not a list", {
  obj <- structure("Not a list", class = class(erahumed_model()))
  expect_false( is_erahumed_model(obj) )
})

test_that("Validator returns FALSE on an ordinary list", {
  expect_false( is_erahumed_model(list()) )
})
