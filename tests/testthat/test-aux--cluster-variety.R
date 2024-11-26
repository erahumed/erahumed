test_that("info_clusters() succeeds", {
  expect_no_error(info_clusters())
})

test_that("info_clusters(include_geometry = TRUE) succeeds", {
  expect_no_error(info_clusters(include_geometry = TRUE))
})

test_that("generate_clusters_variety(c(0.8, 0.1, 0.1)) succeeds", {
  expect_no_error( generate_clusters_variety(c(0.8, 0.1, 0.1)) )
})

test_that("generate_clusters_variety(c(1, 100, 1)) succeeds with a warning", {
  expect_warning( generate_clusters_variety(c(1, 100, 1)) )
})

test_that("generate_clusters_variety(c(1, 1, 100)) succeeds with a warning", {
  expect_warning( generate_clusters_variety(c(1, 1, 100)) )
})




