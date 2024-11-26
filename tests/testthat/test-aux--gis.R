test_that("info_clusters() succeeds", {
  expect_no_error(info_clusters())
})

test_that("info_clusters(include_geometry = TRUE) succeeds", {
  expect_no_error(info_clusters(include_geometry = TRUE))
})

test_that("info_ditches() succeeds", {
  expect_no_error(info_ditches())
})

test_that("info_ditches(include_geometry = TRUE) succeeds", {
  expect_no_error(info_ditches(include_geometry = TRUE))
})




test_that("plot_albufera_clusters(): no error with default args", {
  expect_no_error(plot_albufera_clusters())
})
