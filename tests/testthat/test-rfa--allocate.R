test_that("Allocation of a rfms succeeds", {
  rfa <- new_rfa()
  rfms <- new_rfms()

  expect_no_error(allocate_rfms(rfa = rfa, rfms = rfms, target_fraction = 0.5))
})

test_that("allocate_rfms() returns an object of the correct type", {
  res <-
    allocate_rfms(rfa = new_rfa(), rfms = new_rfms(), target_fraction = 0.5)

  expect_no_error(assert_rfa(res))
})

test_that("allocate_rfms() increases the number of rfmss each time", {
  rfa <- new_rfa()
  expect_length(rfa$rfms, 0)
  rfa <- allocate_rfms(rfa = rfa, rfms = new_rfms(), target_fraction = .1)
  expect_length(rfa$rfms, 1)
  rfa <- allocate_rfms(rfa = rfa, rfms = new_rfms(), target_fraction = .2)
  expect_length(rfa$rfms, 2)
  })
