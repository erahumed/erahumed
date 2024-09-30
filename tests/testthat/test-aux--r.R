test_that("succeeds() works as expected", {
  expect_true(succeeds(identity(1)))
  expect_false(succeeds(stop()))
})
