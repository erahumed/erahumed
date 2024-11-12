succeeds <- function(expr) {
  result <- try(expr, silent = TRUE)
  !inherits(result, "try-error")
}
