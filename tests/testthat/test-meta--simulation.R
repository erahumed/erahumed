test_that("Constructor succeeds", {
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

test_that("print() method succeeds", {
  obj <- erahumed_simulation()
  expect_no_error(capture.output(print(obj)))
})

test_that("print() method returns invisibly", {
  obj <- erahumed_simulation()
  print_res <- expect_output(print(obj))
  expect_identical(print_res, obj)
})

test_that("summary() method succeeds", {
  expect_no_error( capture.output(summary(erahumed_simulation())) )
})
