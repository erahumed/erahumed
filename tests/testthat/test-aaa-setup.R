# This is a dummy test introduced with the sole purpose to make explicit the
# fact that the setup.R script was correctly executed before starting actual
# tests.
# NB: this only gives a nice result when setup.R succeeds. If it fails, no test
# is run and the error thrown by the script is printed instead.

test_that("{testthat} initialization succeeds (setup.R)", {
  expect_true(TRUE)
})
