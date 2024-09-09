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

assert_positive_vector <- function(x, name = deparse(substitute(x)), tol = 0) {
  assert_numeric_vector(x, name)
  assert_no_na(x, name)

  if (all(x >= -tol))
    return(invisible(TRUE))

  msg <- paste0("'", name, "' must be positive.")
  stop(msg)
}

assert_positive_number <- function(
    x,
    name = deparse(substitute(x)),
    check_finite = TRUE,
    tol = 0
    )
{
  assert_positive_vector(x, name, tol = tol)
  assert_length_one(x, name)
  if (check_finite && is.infinite(x))
    stop(paste0("'", name, "' must be finite."))
}

assert_positive_integer <- function(x, name = deparse(substitute(x))) {
  assert_positive_number(x, name)
  assert_integer_vector(x, name)
}

assert_character <- function(x, name = deparse(substitute(x))) {
  if(is.character(x))
    return(invisible(TRUE))

  msg <- paste0("'", name, "' must be a character vector.")
  stop(msg)
}

assert_string <- function(x, name = deparse(substitute(x))) {
  assert_character(x, name)
  assert_no_na(x, name)
  assert_length_one(x, name)
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
    extends = TRUE,
    check_types = TRUE
    )
{
  if (!("data.frame" %in% class(x)))
    stop(paste0("'", name, "' must be a data.frame."))

  if (is.null(template))
    return(invisible(TRUE))

  assert_data.frame(template)

  exp_cols <- sort(colnames(template))
  cols <- sort(colnames(x))

  for (col in exp_cols) {
    if (!col %in% cols)
      stop( paste0("Column ", col, " is missing in data.frame '", name, "'.") )

    if (!check_types)
      next
    type <- typeof(x[[col]])
    exp_type <- typeof(template[[col]])
    type_match <- type == exp_type ||
      all(c(type, exp_type) %in% c("integer", "double"))
    if (!type_match)
      stop( paste0("Columm ", col, " must be of type '", exp_type, "'.") )
    }

  if (extends | all(col %in% exp_cols))
    return(invisible(TRUE))

  msg <- paste0("'", name, "' ",
                "must have exactly the following columns: ",
                paste0(exp_cols, collapse = ", ")
                )
  stop(msg)
}

assert_date <- function(x, name = deparse(substitute(x))) {
  tryCatch(
    x <- as.Date(x),
    error = function(cnd)
      stop(paste0("'", name, "' must be coercible to class 'Date'."))
  )
  return(invisible(TRUE))
}

assert_valid_date_range <- function(x, name = deparse(substitute(x))) {
  assert_date(x, name = name)
  if (length(x) != 2)
    stop(paste0("'", name, "' must have length 2."))
  if (any(is.na(x)))
    stop(paste0("'", name, "' cannot be NA."))
  if (x[1] > x[2])
    stop(paste0("'", name, "' is an empty date range."))

  return(invisible(TRUE))
}

assert_logical <- function(x, name  = deparse(substitute(x))) {
  if (is.logical(x))
    return(invisible(TRUE))

  stop(paste0("'", name, "' must be a logical vector."))
}
