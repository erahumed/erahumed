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
  arg_names <- names(forms)[names(forms) != "simulation"]
  args_list <- lapply(arg_names, get, envir = envir)
  names(args_list) <- arg_names

  return(args_list)
}
