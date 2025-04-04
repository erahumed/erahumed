succeeds <- function(expr) {
  result <- try(expr, silent = TRUE)
  !inherits(result, "try-error")
}

inform_once <- (function(msg, id = NULL) {
  .message_registry <- environment()

  res <- function(msg, id) {
    if (!exists(id, envir = .message_registry)) {
      message(msg)
      assign(id, TRUE, envir = .message_registry)
    }
  }

  return(res)
})()
