test_that("new_cluster_map() constructor succeeds", {
  expect_no_error(new_cluster_map())
})

test_that("new_cluster_map() creates an object of the correct class", {
  x <- new_cluster_map()
  expect_s3_class(x, "erahumed_cluster_map")
})

test_that("Validator succeeds", {
  x <- new_cluster_map()
  expect_no_error(assert_cluster_map(x))
})

