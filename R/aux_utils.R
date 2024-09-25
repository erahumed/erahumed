# Constants and unit conversions
s_per_day <- function() {
  24 * 60 * 60
}

cm_day_to_m3_s <- function(x, area_m2) {
  x * area_m2 / s_per_day() / 100
}

m3_s_to_cm_day <- function(x, area_m2) {
  x * 100 * s_per_day() / area_m2
}



# String manipulation
bold <- function(x) {
  paste0("\033[1m", x, "\033[0m")
}


# Date manipulation
get_mm <- function(date) {
  stopifnot(class(date)[1] == "POSIXlt")
  date$mon + 1
}

get_dd <- function(date) {
  stopifnot(class(date)[1] == "POSIXlt")
  date$mday
}



# Mathematics
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

pmin2 <- function(x, thresh) {
  thresh + (x - thresh) * (x < thresh)
}

pmax2 <- function(x, thresh) {
  thresh + (x - thresh) * (x > thresh)
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



# R language utilities
succeeds <- function(expr) {
  result <- try(expr, silent = TRUE)
  !inherits(result, "try-error")
}

capture_params <- function(
  fun = sys.function(sys.parent()),
  envir = parent.frame()
  )
{
  forms <- formals(fun = fun, envir = envir)
  # Get the names of all formal arguments except 'model'
  arg_names <- names(forms)[names(forms) != "model"]

  # Retrieve their values
  args_list <- lapply(arg_names, get, envir = envir)

  # Set the names of the list
  names(args_list) <- arg_names

  # Return the list of parameters
  return(args_list)
}
