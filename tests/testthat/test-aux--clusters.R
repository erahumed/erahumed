test_that("clusters() succeeds", {
  expect_no_error(clusters())
})

test_that("clusters(include_geometry = TRUE) succeeds", {
  expect_no_error(clusters(include_geometry = TRUE))
})

