test_that("clusters() succeeds", {
  expect_no_error(clusters())
})

test_that("clusters(include_geometry = TRUE) succeeds", {
  expect_no_error(clusters(include_geometry = TRUE))
})

test_that("generate_clusters_variety(c(0.8, 0.1, 0.1)) succeeds", {
  expect_no_error( generate_clusters_variety(c(0.8, 0.1, 0.1)) )
})



