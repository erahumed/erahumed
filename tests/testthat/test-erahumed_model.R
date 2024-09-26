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
