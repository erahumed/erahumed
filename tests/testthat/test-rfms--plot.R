test_that("Plot succeeds with Clearfield", {
  expect_no_error(plot_rfms(clearfield()))
})

test_that("Plot succeeds with Empty management system", {
  expect_no_error(plot_rfms(new_management_system()))
})
