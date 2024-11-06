test_that("Low-level constructor succeeds", {
  expect_no_error( new_erahumed_simulation() )
})

test_that("High-level constructor succeeds", {
  expect_no_error( erahumed_simulation() )
})

test_that("Validator returns TRUE on constructor output", {
  m <- erahumed_simulation()
  expect_true( is_erahumed_simulation(m) )
})

test_that("Validator: FALSE if object is not a list", {
  obj <- structure("Not a list", class = class(erahumed_simulation()))
  expect_false( is_erahumed_simulation(obj) )
})

test_that("Validator: FALSE if list elements are not of the right S3 class", {
  obj <- erahumed_simulation()
  obj[["invalid_element"]] <- list()  # Not of 'simulation_layer' class!
  expect_false( is_erahumed_simulation(obj) )
})

test_that("Validator: FALSE if object is not of the right S3 class", {
  expect_false( is_erahumed_simulation(list()) )
})
