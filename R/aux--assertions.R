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

assert_fraction <- function(x, name = deparse(substitute(x)), tol = 0) {
  assert_positive_number(x, name)
  if (x <= 1 - tol)
    return(invisible(TRUE))

  stop(paste("'", name, "' must be smaller than 1."))
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
  if (!inherits(x, "data.frame"))
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

  if (extends | all(cols %in% exp_cols))
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

assert_erahumed_simulation <- function(x, name  = deparse(substitute(x))) {
  if (is_erahumed_simulation(x))
    return(invisible(TRUE))
  msg <- paste(name, "is not a valid 'erahumed_simulation' object.")
  stop(msg)
}

assert_erahumed_chemical <- function(x, name  = deparse(substitute(x))) {
  if (is_erahumed_chemical(x))
    return(invisible(TRUE))
  msg <- paste(name, "is not a valid 'erahumed_chemical' object.")
  stop(msg)
}

assert_wms <- function(x, name  = deparse(substitute(x))) {
  if (is_wms(x))
    return(invisible(TRUE))
  msg <- paste(name, "is not a valid 'erahumed_wms' object.")
  stop(msg)
}

assert_management_system <- function(x, name  = deparse(substitute(x))) {
  if (is_management_system(x))
    return(invisible(TRUE))
  msg <- paste(name, "is not a valid 'erahumed_management_scheme' object.")
  stop(msg)
}

assert_rfms_map <- function(x, name  = deparse(substitute(x))) {
  if (is_rfms_map(x))
    return(invisible(TRUE))
  msg <- paste(name, "is not a valid 'erahumed_rfms_map' object.")
  stop(msg)
}
