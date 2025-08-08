test_that("Can run a valid simulation with an 'empty' cluster map", {
  expect_no_error(
    obj <- erahumed_simulation(date_end = "2020-01-10",
                               rfms_map = new_rfms_map(),
                               .progress = \(.) {}
                               )
    )

  expect_no_error(assert_erahumed_simulation(obj))
})

test_that("Can plot hydrology of an 'empty' cluster map", {
  obj <- erahumed_simulation(date_end = "2020-01-10",
                             rfms_map = new_rfms_map(),
                             .progress = \(.) {})

  expect_no_error(plot_hbl(obj))
})
