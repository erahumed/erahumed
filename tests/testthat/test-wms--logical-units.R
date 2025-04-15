test_that("wms_irrigation() succeeds", {
  expect_no_error( wms_irrigation(numeric(10)) )
})

test_that("wms_irrigation() returns a logical vector of the correct size", {
  n <- 840
  res <- wms_irrigation(numeric(n))
  expect_vector(res, ptype = logical(), size = n)
})

test_that("wms_irrigation() returns the expected output in simple case", {
  x <-
    c(4, 4, 2, 0, 0, 0, 1, 2, 1, 0, 0, 1, 2, 2, 1, 0, 0, 0, 2, 4, 4)
  expected <-
    c(T, T, F, F, F, F, T, T, F, F, F, T, T, T, F, F, F, F, T, T, T)
  expect_equal(wms_irrigation(x), expected)
})

test_that("wms_draining() succeeds", {
  expect_no_error( wms_draining(numeric(10)) )
})

test_that("wms_draining() returns a logical vector of the correct size", {
  n <- 840
  res <- wms_draining(numeric(n))
  expect_vector(res, ptype = logical(), size = n)
})

test_that("wms_draining() returns the expected output in simple case", {
  x <-
    c(4, 4, 2, 0, 0, 0, 1, 2, 1, 0, 0, 1, 2, 2, 1, 0, 0, 0, 2, 4, 4)
  expected <-
    c(T, T, T, T, F, F, F, F, T, T, F, F, F, T, T, T, F, F, F, F, T)
  expect_equal(wms_draining(x), expected)
})
