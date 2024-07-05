assert_numeric_vector <- function(x, name = deparse(substitute(x))) {
  if (is.numeric(x))
    return(invisible(TRUE))

  msg <- paste0("'", name, "' must be a numeric vector.")
  stop(msg)
}

assert_no_na <- function(x, name = deparse(substitute(x))) {
  if (all(!is.na(x)))
    return(invisible(TRUE))

  msg <- paste0("'", name, "' cannot be NA.")
  stop(msg)
}

assert_length_one <- function(x, name = deparse(substitute(x))) {
  if (is.vector(x) & (length(x) == 1))
    return(invisible(TRUE))

  msg <- paste0("'", name, "' must be a length one vector.")
  stop(msg)
}

assert_integer_vector <- function(x, name = deparse(substitute(x))) {
  assert_numeric_vector(x, name)

  if (all(x %% 1 == 0))
    return(invisible(TRUE))

  msg <- paste0("'", name, "' must be integer.")
  stop(msg)
}

assert_positive_vector <- function(x, name = deparse(substitute(x))) {
  assert_numeric_vector(x, name)
  assert_no_na(x, name)

  if (all(x >= 0))
    return(invisible(TRUE))

  msg <- paste0("'", name, "' must be positive.")
  stop(msg)
}

assert_positive_number <- function(x, name = deparse(substitute(x))) {
  assert_positive_vector(x)
  assert_length_one(x)
}

assert_positive_integer <- function(x, name = deparse(substitute(x))) {
  assert_positive_number(x, name)
  assert_integer_vector(x, name)
}


