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

get_mm <- function(date) {
  as.POSIXlt(date)$mon + 1
}

get_dd <- function(date) {
  as.POSIXlt(date)$mday
}

s_per_day <- function() {
  24 * 60 * 60
}

cm_day_to_m3_s <- function(x, area_m2) {
  x * area_m2 / s_per_day() / 100
}

m3_s_to_cm_day <- function(x, area_m2) {
  x * 100 * s_per_day() / area_m2
}
