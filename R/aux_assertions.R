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

assert_string <- function(x, name = deparse(substitute(x))) {
  if(is.character(x) && length(x) == 1)
    return(invisible( assert_no_na(x, name) ))

  msg <- paste0("'", name, "' must be a length one character (not NA).")
  stop(msg)
}

assert_list <- function(x, name = deparse(substitute(x))) {
  if(is.list(x))
    return(invisible(TRUE))

  stop(paste0("'", name, "' must be a list."))
}

assert_atomic <- function(x, name = deparse(substitute(x))) {
  if (is.atomic(x))
    return(invisible(TRUE))

  stop(paste0("'", name, "' must be atomic."))
}

assert_function <- function(x, name = deparse(substitute(x)), check = NULL)
{
  if (!is.function(x))
    stop(paste0("'", name, "' must be a function."))

  if ( is.null(check) || succeeds(do.call(x, check)) )
    return(invisible(TRUE))

  stop(paste0("Test call of ", name, "() failed."))
}

assert_data.frame <- function(
    x,
    name = deparse(substitute(x)),
    template = NULL,
    extends = TRUE
    )
{
  if (!("data.frame" %in% class(x)))
    stop(paste0("'", name, "' must be a data.frame."))

  if (is.null(template))
    return(invisible(TRUE))

  assert_data.frame(template)

  exp_cols <- sort(colnames(template))
  cols <- sort(colnames(x))

  if (identical(cols, exp_cols) || (all(exp_cols %in% cols) && extends))
    return(invisible(TRUE))

  msg <- paste0("'", name, "' ",
                "must have ",
                ifelse(extends, "", "exactly "),
                "the following columns: ",
                paste0(exp_cols, collapse = ", ")
                )
  stop(msg)
}
