test_that("ca_delay_vector(): output has same type and length of input", {
  set.seed(840)

  n <- 20
  inputs <- list(
    double = rnorm(n),
    integer = rpois(n, 100),
    logical = rnorm(n) > .5,
    character = letters[1:n]
    )
  delay <- cumsum(runif(n) > .8)

  for (input in inputs) {
    output <- ca_delay_vector(input, delay)
    expect_vector(output, ptype = input, size = length(input))
  }
})

test_that("ca_delay_vector(): output is the expected in simple case", {
  x <-        1:10
  delay <-    c(0, 0, 0, 1, 1, 2, 3, 3, 3, 0 )
  expected <- c(1, 2, 3, 3, 4, 4, 4, 5, 6, 10)

  expect_equal(ca_delay_vector(x, delay), expected)
})
