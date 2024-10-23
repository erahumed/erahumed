pmin2 <- function(x, thresh) {
  thresh + (x - thresh) * (x < thresh)
}



pmax2 <- function(x, thresh) {
  thresh + (x - thresh) * (x > thresh)
}



moving_average <- function(x, k) {
  assert_numeric_vector(x)
  assert_positive_integer(k)

  if (k %% 2 == 0) {
    warning("'k' should be odd. Incrementing by 1.")
    k <- k + 1
  }

  left_pad <- k %/% 2
  right_pad <- k - left_pad - 1
  n <- length(x)

  x <- c(rep(x[1], left_pad), x, rep(x[n], right_pad))
  y <- stats::filter(x, rep(1 / k, k), sides = 2)

  y[(left_pad + 1):(left_pad + n)]
}



lgl_buffer <- function(x, distance = 0) {
  # Create a "logical buffer" around a (supposedly sparse) vector 'x'. Entries
  # within the given distance from non zero values of 'x' are flagged as TRUE.
  res <- rep(FALSE, length(x))

  for (pos in which(x != 0)) {
    lo <- max(1, pos - distance)
    up <- min(length(x), pos + distance)

    res[lo:up] <- TRUE
  }

  return(res)
}



exp2by2 <- function(a, b, c, d) {
  # Compute matrix exponential
  # https://math.stackexchange.com/questions/1535731/matrix-exponential-of-non-diagonalizable-matrix/1538095#1538095

  tr <- a + d
  det <- a * d - b * c

  u <- tr / 2
  r <- det / u ^ 2
  sqr <- sqrt(1 - r)
  lambdap <- u * (1 + sqr)
  lambdam <- u * (1 - sqr)
  dlambda <- lambdap - lambdam

  Up11 <- a - lambdam
  Up22 <- d - lambdam
  Um11 <- a - lambdap
  Um22 <- d - lambdap

  list(
    E11 = (exp(lambdap) * Up11 - exp(lambdam) * Um11) / dlambda,
    E22 = (exp(lambdap) * Up22 - exp(lambdam) * Um22) / dlambda,
    E12 = b * (exp(lambdap) - exp(lambdam)) / dlambda,
    E21 = c * (exp(lambdap) - exp(lambdam)) / dlambda
  )

}
