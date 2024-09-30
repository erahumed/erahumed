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
