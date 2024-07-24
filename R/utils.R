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

cumsum_thresh <- function(x, init, lower_thresh = 0) {
  res <- numeric(length(x) + 1)
  res[1] <- max(init, lower_thresh)

  for (i in seq_along(x)) {
    res[i + 1] <- max(res[i] + x[i], lower_thresh)
  }

  return(res)
}

s_per_day <- function() {
  24 * 60 * 60
}
