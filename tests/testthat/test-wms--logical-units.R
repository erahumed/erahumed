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

test_that("wms_height_cm() succeeds with valid arguments", {
  args <- formals(water_management_scheme) |> lapply(eval)
  args <- args[ names(args)[names(args) %in% names(formals(wms_height_cm))] ]
  args <- c(args, tancat = TRUE)

  expect_no_error(do.call(wms_height_cm, args))
})

test_that("wms_height_cm() value is zero in emptyings", {
  args <- formals(water_management_scheme) |> lapply(eval)
  args <- args[ names(args)[names(args) %in% names(formals(wms_height_cm))] ]
  args <- c(args, tancat = TRUE)

  res <- do.call(wms_height_cm, args)
  emptyings <- args$emptyings_yday

  expect_true(all(res[emptyings] == 0))
})

test_that("wms_height_cm() maximum value is that expected", {
  args <- formals(water_management_scheme) |> lapply(eval)
  args <- args[ names(args)[names(args) %in% names(formals(wms_height_cm))] ]
  args <- c(args, tancat = TRUE)

  res <- do.call(wms_height_cm, args)
  expected_max <- max(c(args$flow_height_cm, args$perellona_height_cm))

  expect_equal(max(res), expected_max)
})
